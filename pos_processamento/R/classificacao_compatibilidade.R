
# ============================================================
# CLASSIFICAR COMPATIBILIDADE ENTRE INTERVALOS CONSECUTIVOS
# ============================================================

# Usar a tabela de comparações já calculada.
comp <- comparacoes

# Identificar discordância nas tendências.
tem_discordancia <- function(x) {
  !is.na(x) & grepl("Discordância", x, fixed = TRUE)
}

# Identificar concordância forte.
tem_concordancia_forte <- function(x) {
  !is.na(x) & grepl("Concordância forte", x, fixed = TRUE)
}

# Identificar intervalos com M2 disponível.
tem_m2 <- function(x) {
  !is.na(x) & nzchar(x)
}

# Classificação inicial, com categorias conservadoras.
comp$compatibilidade <- "Inconclusiva"

for (i in seq_len(nrow(comp))) {
  
  d <- comp$diferenca_inclinacao_M2[i]
  
  disc_ant <- tem_discordancia(
    comp$tendencia_anterior[i]
  )
  
  disc_seg <- tem_discordancia(
    comp$tendencia_seguinte[i]
  )
  
  forte_ant <- tem_concordancia_forte(
    comp$tendencia_anterior[i]
  )
  
  forte_seg <- tem_concordancia_forte(
    comp$tendencia_seguinte[i]
  )
  
  m2_disponivel <- !is.na(d)
  
  # Não classificar quando os dados necessários estão ausentes.
  if (!m2_disponivel) next
  
  # Discordância explícita é um alerta de ruptura.
  if (disc_ant || disc_seg) {
    comp$compatibilidade[i] <- "Possível ruptura"
    next
  }
  
  # Diferença muito pequena e concordância forte nos dois lados.
  # Limite provisório para triagem, não conclusão estatística.
  if (d <= 5 && forte_ant && forte_seg) {
    comp$compatibilidade[i] <- "Continuidade forte"
    next
  }
  
  # Diferença pequena, mas sem concordância forte dos dois lados.
  if (d <= 5) {
    comp$compatibilidade[i] <- "Continuidade a investigar"
    next
  }
  
  # Diferença intermediária.
  if (d <= 60) {
    comp$compatibilidade[i] <- "Compatibilidade parcial"
    next
  }
  
  # Diferença elevada.
  comp$compatibilidade[i] <- "Possível ruptura"
}

# Tabela final de diagnóstico.
resultado_compatibilidade <- comp[
  ,
  c(
    "alarme_compartilhado",
    "intervalo_anterior",
    "intervalo_seguinte",
    "diferenca_inclinacao_M2",
    "tendencia_anterior",
    "tendencia_seguinte",
    "compatibilidade"
  )
]

print(resultado_compatibilidade, row.names = FALSE)

# Salvar resultado.
write.csv(
  resultado_compatibilidade,
  "resultados/compatibilidade_entre_intervalos.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Resumo das classificações.
print(table(resultado_compatibilidade$compatibilidade))
