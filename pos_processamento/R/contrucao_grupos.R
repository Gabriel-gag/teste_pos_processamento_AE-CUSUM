
# ============================================================
# 1. CONSTRUIR GRUPOS A PARTIR DA CONTINUIDADE FORTE
# ============================================================

dados <- tabela_consolidada
comp <- resultado_compatibilidade

# Ordenar intervalos cronologicamente.
dados <- dados[order(dados$alarme1), ]
comp <- comp[
  match(dados$alarme2[-nrow(dados)],
        comp$alarme_compartilhado),
]

# Identificar quais ligações foram classificadas como fortes.
ligacao_forte <- comp$compatibilidade == "Continuidade forte"
ligacao_forte[is.na(ligacao_forte)] <- FALSE

# Cada ligação forte une dois intervalos consecutivos.
indices <- which(ligacao_forte)

# Encontrar sequências de ligações consecutivas.
grupos_indices <- list()

if (length(indices) > 0) {
  
  sequencias <- split(
    indices,
    cumsum(c(TRUE, diff(indices) != 1))
  )
  
  for (i in seq_along(sequencias)) {
    
    seq_i <- sequencias[[i]]
    
    primeiro <- min(seq_i)
    ultimo <- max(seq_i) + 1
    
    grupos_indices[[length(grupos_indices) + 1]] <-
      list(
        grupo = paste0("G", length(grupos_indices) + 1),
        primeiro = primeiro,
        ultimo = ultimo,
        alarmes = c(
          dados$alarme1[primeiro],
          dados$alarme2[primeiro:ultimo]
        )
      )
  }
}

# Montar tabela dos grupos.
if (length(grupos_indices) > 0) {
  
  tabela_grupos <- do.call(
    rbind,
    lapply(grupos_indices, function(g) {
      
      data.frame(
        grupo = g$grupo,
        primeiro_alarme = min(g$alarmes),
        ultimo_alarme = max(g$alarmes),
        quantidade_alarmas = length(g$alarmes),
        alarmes = paste(g$alarmes, collapse = ", ")
      )
    })
  )
  
} else {
  
  tabela_grupos <- data.frame(
    grupo = character(),
    primeiro_alarme = numeric(),
    ultimo_alarme = numeric(),
    quantidade_alarmas = integer(),
    alarmes = character()
  )
}

print(tabela_grupos, row.names = FALSE)

write.csv(
  tabela_grupos,
  "resultados/grupos_alarmas.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ============================================================
# 2. VISUALIZAÇÃO DOS GRUPOS DE ALARMES
# ============================================================

todos_alarmas <- sort(unique(c(
  dados$alarme1,
  dados$alarme2
)))

# Preparar gráfico.
n_grupos <- nrow(tabela_grupos)

if (n_grupos > 0) {
  
  posicoes <- seq(n_grupos, 1)
  
  plot(
    NA,
    xlim = range(todos_alarmas),
    ylim = c(-2, n_grupos + 1),
    xlab = "Índice da série temporal",
    ylab = "",
    yaxt = "n",
    main = "Agrupamento dos alarmes por compatibilidade"
  )
  
  axis(
    2,
    at = posicoes,
    labels = tabela_grupos$grupo,
    las = 1
  )
  
  # Desenhar cada grupo.
  for (i in seq_len(n_grupos)) {
    
    inicio <- tabela_grupos$primeiro_alarme[i]
    fim <- tabela_grupos$ultimo_alarme[i]
    
    alarmas <- grupos_indices[[i]]$alarmes
    
    segments(
      inicio, posicoes[i],
      fim, posicoes[i],
      lwd = 9,
      col = "#2878B9"
    )
    
    points(
      alarmas,
      rep(posicoes[i], length(alarmas)),
      pch = 19,
      cex = 1.1
    )
    
    text(
      fim,
      posicoes[i] + 0.25,
      labels = paste0(
        length(alarmas), " alarmes"
      ),
      pos = 4,
      cex = 0.7
    )
  }
  
  # Identificar alarmes que não pertencem a grupos fortes.
  alarmas_agrupados <- unique(
    unlist(lapply(grupos_indices, `[[`, "alarmes"))
  )
  
  alarmas_isolados <- setdiff(
    todos_alarmas,
    alarmas_agrupados
  )
  
  points(
    alarmas_isolados,
    rep(-1, length(alarmas_isolados)),
    pch = 19,
    col = "gray40"
  )
  
  text(
    alarmas_isolados,
    rep(-1.3, length(alarmas_isolados)),
    labels = alarmas_isolados,
    cex = 0.55,
    srt = 45,
    adj = 1
  )
  
  axis(
    2,
    at = -1,
    labels = "Não agrupados",
    las = 1
  )
  
} else {
  
  message("Nenhum grupo com continuidade forte foi identificado.")
}

# Salvar gráfico.
png(
  "resultados/visualizacao_grupos_alarmas.png",
  width = 1600,
  height = 900,
  res = 130
)

# Reexecutar o bloco de visualização acima neste dispositivo.
# Alternativamente, use dev.copy() antes de fechar o gráfico.
dev.off()
