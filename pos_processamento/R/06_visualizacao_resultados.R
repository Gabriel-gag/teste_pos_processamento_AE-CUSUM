
# ============================================================
# 06_visualizacao_resultados.R
# VISUALIZACAO DO PIPELINE AE + CUSUM E POS-PROCESSAMENTO
# ============================================================
#
# Execute depois do pipeline e do pos-processamento.
# Nao executa novamente o Autoencoder nem o CUSUM.
#
# Saidas:
# resultados/visualizacoes/
#
# ============================================================


# ============================================================
# 1. PACOTES
# ============================================================

pacotes <- c("ggplot2", "gridExtra")

faltantes <- pacotes[
  !vapply(
    pacotes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

library(ggplot2)
library(gridExtra)


# ============================================================
# 2. VERIFICAR OBJETOS NECESSARIOS
# ============================================================

objetos_obrigatorios <- c(
  "series",
  "results_mse",
  "results_tau"
)

nao_encontrados <- objetos_obrigatorios[
  !vapply(
    objetos_obrigatorios,
    exists,
    logical(1),
    envir = .GlobalEnv
  )
]

if (length(nao_encontrados) > 0) {
  
  stop(
    "Objetos ausentes no Environment: ",
    paste(nao_encontrados, collapse = ", "),
    ". Execute primeiro o pipeline AE + CUSUM."
  )
  
}


# ============================================================
# 3. FUNCAO PARA RECUPERAR OBJETOS
# ============================================================

get_obj <- function(nome, padrao = NULL) {
  
  if (
    exists(
      nome,
      envir = .GlobalEnv,
      inherits = FALSE
    )
  ) {
    
    return(
      get(nome, envir = .GlobalEnv)
    )
    
  }
  
  return(padrao)
  
}


# ============================================================
# 4. CONFIGURACOES VISUAIS
# ============================================================

dir_saida <- file.path(
  "resultados",
  "visualizacoes"
)

dir.create(
  dir_saida,
  recursive = TRUE,
  showWarnings = FALSE
)


# Parametros do tuning escolhido.
# Os objetos existentes no Environment tem prioridade.

K_FACTOR <- get_obj("K_FACTOR", 0.35)
H_LOW <- get_obj("H_LOW", 3.5)
H_HIGH <- get_obj("H_HIGH", 6)


# Paleta visual

cor_serie <- "#263B53"
cor_alarme <- "#D94841"
cor_corte <- "#7A5195"
cor_warning <- "#E39C37"
cor_tau <- "#008C95"


# Tema comum

tema_relatorio <- theme_minimal(base_size = 12) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    
    plot.subtitle = element_text(
      size = 10,
      color = "grey35"
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom",
    
    legend.title = element_blank(),
    
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.margin = margin(
      10, 14, 10, 10
    )
    
  )


# ============================================================
# 5. ORGANIZACAO DOS DADOS
# ============================================================

n <- length(series)

idx <- seq_len(n)


# Funcao para alinhar vetores

vetor_seguro <- function(
    nome,
    padrao = rep(NA_real_, n)
) {
  
  x <- get_obj(nome, NULL)
  
  if (is.null(x)) {
    return(padrao)
  }
  
  x <- as.numeric(x)
  
  if (length(x) == n) {
    return(x)
  }
  
  if (length(x) < n) {
    
    return(
      c(
        rep(NA_real_, n - length(x)),
        x
      )
    )
    
  }
  
  return(
    x[seq_len(n)]
  )
  
}


# Data frame principal

df <- data.frame(
  
  indice = idx,
  
  serie = as.numeric(series),
  
  mse = vetor_seguro("results_mse"),
  
  tau = vetor_seguro("results_tau"),
  
  z = vetor_seguro("results_z"),
  
  cusum_pos = vetor_seguro(
    "results_pos",
    rep(NA_real_, n)
  ),
  
  cusum_neg = vetor_seguro(
    "results_neg",
    rep(NA_real_, n)
  ),
  
  warning = vetor_seguro(
    "results_warning",
    rep(0, n)
  ),
  
  drift_flag = vetor_seguro(
    "results_drift",
    rep(0, n)
  ),
  
  anomalia = vetor_seguro(
    "results_anomaly",
    rep(0, n)
  )
  
)


# ============================================================
# 6. ALARMES DO CUSUM
# ============================================================

alarmes <- get_obj(
  "drift_indices",
  integer(0)
)

if (length(alarmes) == 0) {
  
  alarmes <- which(
    df$drift_flag == 1
  )
  
}

alarmes <- sort(
  unique(
    as.integer(alarmes)
  )
)

alarmes <- alarmes[
  !is.na(alarmes) &
    alarmes >= 1 &
    alarmes <= n
]


df_alarm <- df[
  df$indice %in% alarmes,
  ,
  drop = FALSE
]


# ============================================================
# 7. EVENTOS REAIS
# ============================================================

labels_obj <- get_obj(
  "labels",
  NULL
)

if (
  !is.null(labels_obj) &&
  length(labels_obj) == n
) {
  
  df_eventos <- df[
    which(as.numeric(labels_obj) == 1),
    ,
    drop = FALSE
  ]
  
} else {
  
  df_eventos <- df[0, ]
  
}


# ============================================================
# 8. CORTES DO POS-PROCESSAMENTO
# ============================================================

extrair_numeros <- function(x) {
  
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    
    return(numeric(0))
    
  }
  
  x <- paste(
    x,
    collapse = ","
  )
  
  achados <- gregexpr(
    "[0-9]+",
    x,
    perl = TRUE
  )
  
  as.numeric(
    regmatches(x, achados)[[1]]
  )
  
}


cortes_m1 <- numeric(0)
cortes_m2 <- numeric(0)
cortes_candidatos <- numeric(0)


# Resultados da segmentacao

obj_resultados <- get_obj(
  "resultados",
  NULL
)

if (is.data.frame(obj_resultados)) {
  
  if ("corte_M1" %in% names(obj_resultados)) {
    
    cortes_m1 <- unlist(
      lapply(
        obj_resultados$corte_M1,
        extrair_numeros
      )
    )
    
  }
  
  if ("cortes_M2" %in% names(obj_resultados)) {
    
    cortes_m2 <- unlist(
      lapply(
        obj_resultados$cortes_M2,
        extrair_numeros
      )
    )
    
  }
  
}


# Tabela de diagnosticos

obj_tabela <- get_obj(
  "tabela_final",
  NULL
)

if (
  is.data.frame(obj_tabela) &&
  "corte_candidato" %in% names(obj_tabela)
) {
  
  cortes_candidatos <- suppressWarnings(
    as.numeric(
      obj_tabela$corte_candidato
    )
  )
  
}


# Limpeza dos indices

limpar_cortes <- function(x) {
  
  x <- unique(
    as.numeric(x)
  )
  
  x <- x[
    is.finite(x) &
      x >= 1 &
      x <= n
  ]
  
  sort(x)
  
}


cortes_m1 <- limpar_cortes(cortes_m1)

cortes_m2 <- limpar_cortes(cortes_m2)

cortes_candidatos <- limpar_cortes(
  cortes_candidatos
)


# Data frame dos cortes

df_cortes <- data.frame(
  
  indice = c(
    cortes_m1,
    cortes_m2,
    cortes_candidatos
  ),
  
  tipo = c(
    
    rep(
      "Corte M1",
      length(cortes_m1)
    ),
    
    rep(
      "Corte M2",
      length(cortes_m2)
    ),
    
    rep(
      "Corte candidato (diagnostico)",
      length(cortes_candidatos)
    )
    
  )
  
)

df_cortes <- unique(df_cortes)


# ============================================================
# 8.1. TABELA DE CLASSIFICACAO DOS CORTES M1/M2
# ============================================================

janela_corte <- 20
min_obs_corte <- 8

# ------------------------------------------------------------
# Identificar intervalo de origem
# ------------------------------------------------------------

identificar_intervalo <- function(corte) {
  
  if (
    !is.data.frame(obj_tabela) ||
    !"intervalo" %in% names(obj_tabela)
  ) {
    return(NA_character_)
  }
  
  for (i in seq_len(nrow(obj_tabela))) {
    
    nums <- extrair_numeros(
      obj_tabela$intervalo[i]
    )
    
    if (
      length(nums) >= 2 &&
      corte >= nums[1] &&
      corte <= nums[2]
    ) {
      return(
        as.character(obj_tabela$intervalo[i])
      )
    }
  }
  
  NA_character_
}


# ------------------------------------------------------------
# Analisar cada corte
# ------------------------------------------------------------

analisar_corte <- function(corte, origem) {
  
  inicio <- max(1, corte - janela_corte)
  fim <- min(n, corte + janela_corte)
  
  antes <- df[
    df$indice >= inicio &
      df$indice < corte,
    ,
    drop = FALSE
  ]
  
  depois <- df[
    df$indice >= corte &
      df$indice <= fim,
    ,
    drop = FALSE
  ]
  
  antes <- antes[
    is.finite(antes$serie),
    ,
    drop = FALSE
  ]
  
  depois <- depois[
    is.finite(depois$serie),
    ,
    drop = FALSE
  ]
  
  intervalo <- identificar_intervalo(corte)
  
  if (
    nrow(antes) < min_obs_corte ||
    nrow(depois) < min_obs_corte
  ) {
    
    return(data.frame(
      intervalo = intervalo,
      corte = corte,
      origem = origem,
      nivel_antes = NA_real_,
      nivel_depois = NA_real_,
      delta_nivel = NA_real_,
      inclinacao_antes = NA_real_,
      inclinacao_depois = NA_real_,
      delta_inclinacao = NA_real_,
      classificacao = "Dados insuficientes",
      evidencia = "Janela local insuficiente"
    ))
  }
  
  # Centralizar índice no corte
  
  antes$x <- antes$indice - corte
  depois$x <- depois$indice - corte
  
  modelo_antes <- lm(
    serie ~ x,
    data = antes
  )
  
  modelo_depois <- lm(
    serie ~ x,
    data = depois
  )
  
  # Níveis estimados na fronteira
  
  nivel_antes <- unname(
    coef(modelo_antes)["(Intercept)"]
  )
  
  nivel_depois <- unname(
    coef(modelo_depois)["(Intercept)"]
  )
  
  # Inclinações locais
  
  inclinacao_antes <- unname(
    coef(modelo_antes)["x"]
  )
  
  inclinacao_depois <- unname(
    coef(modelo_depois)["x"]
  )
  
  # Diferenças entre os lados
  
  delta_nivel <- nivel_depois - nivel_antes
  
  delta_inclinacao <-
    inclinacao_depois - inclinacao_antes
  
  # Escala residual robusta
  
  residuos <- c(
    residuals(modelo_antes),
    residuals(modelo_depois)
  )
  
  escala_residual <- mad(
    residuos,
    center = median(residuos, na.rm = TRUE),
    constant = 1.4826,
    na.rm = TRUE
  )
  
  if (
    !is.finite(escala_residual) ||
    escala_residual < 1e-8
  ) {
    escala_residual <- sd(
      residuos,
      na.rm = TRUE
    )
  }
  
  if (
    !is.finite(escala_residual) ||
    escala_residual < 1e-8
  ) {
    escala_residual <- 1e-8
  }
  
  # Critérios exploratórios
  
  mudou_nivel <-
    abs(delta_nivel) > 2 * escala_residual
  
  efeito_inclinacao <-
    abs(delta_inclinacao * janela_corte)
  
  mudou_inclinacao <-
    efeito_inclinacao > 2 * escala_residual
  
  # Classificação
  
  if (mudou_nivel && mudou_inclinacao) {
    
    classificacao <- "Nivel e inclinacao"
    
  } else if (mudou_nivel) {
    
    classificacao <- "Mudanca de nivel"
    
  } else if (mudou_inclinacao) {
    
    classificacao <- "Mudanca de inclinacao"
    
  } else {
    
    classificacao <- "Sem alteracao local clara"
  }
  
  evidencia <- paste0(
    "Descontinuidade estimada: ",
    round(abs(delta_nivel), 3),
    " | Efeito da inclinacao em ",
    janela_corte,
    " pontos: ",
    round(efeito_inclinacao, 3),
    " | Escala residual: ",
    round(escala_residual, 3)
  )
  
  data.frame(
    intervalo = intervalo,
    corte = corte,
    origem = origem,
    nivel_antes = nivel_antes,
    nivel_depois = nivel_depois,
    delta_nivel = delta_nivel,
    inclinacao_antes = inclinacao_antes,
    inclinacao_depois = inclinacao_depois,
    delta_inclinacao = delta_inclinacao,
    classificacao = classificacao,
    evidencia = evidencia
  )
}


# ------------------------------------------------------------
# Montar tabela
# ------------------------------------------------------------

lista_cortes <- list()

if (length(cortes_m1) > 0) {
  
  lista_cortes[["M1"]] <- lapply(
    cortes_m1,
    analisar_corte,
    origem = "M1"
  )
}

if (length(cortes_m2) > 0) {
  
  lista_cortes[["M2"]] <- lapply(
    cortes_m2,
    analisar_corte,
    origem = "M2"
  )
}

if (length(lista_cortes) > 0) {
  
  tabela_classificacao_cortes <- do.call(
    rbind,
    unlist(lista_cortes, recursive = FALSE)
  )
  
  rownames(tabela_classificacao_cortes) <- NULL
  
} else {
  
  tabela_classificacao_cortes <- data.frame()
}


# ------------------------------------------------------------
# Ordenar e arredondar
# ------------------------------------------------------------

if (nrow(tabela_classificacao_cortes) > 0) {
  
  tabela_classificacao_cortes <-
    tabela_classificacao_cortes[
      order(tabela_classificacao_cortes$corte),
      ,
      drop = FALSE
    ]
  
  colunas_numericas <- c(
    "nivel_antes",
    "nivel_depois",
    "delta_nivel",
    "inclinacao_antes",
    "inclinacao_depois",
    "delta_inclinacao"
  )
  
  tabela_classificacao_cortes[
    colunas_numericas
  ] <- lapply(
    tabela_classificacao_cortes[
      colunas_numericas
    ],
    round,
    digits = 4
  )
}


# ------------------------------------------------------------
# Exibir e salvar
# ------------------------------------------------------------

cat("\nCLASSIFICACAO DOS CORTES M1/M2\n")

print(
  tabela_classificacao_cortes,
  row.names = FALSE
)

write.csv(
  tabela_classificacao_cortes,
  file.path(
    dir_saida,
    "09_classificacao_cortes_M1_M2.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (nrow(tabela_classificacao_cortes) > 0) {
  
  tabela_visual <- tabela_classificacao_cortes
  
  tabela_visual[] <- lapply(
    tabela_visual,
    as.character
  )
  
  tabela_grafica <- gridExtra::tableGrob(
    tabela_visual,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(
      base_size = 8
    )
  )
  
  ggsave(
    filename = file.path(
      dir_saida,
      "09_classificacao_cortes_M1_M2.png"
    ),
    plot = tabela_grafica,
    width = 18,
    height = max(
      5,
      0.35 * nrow(tabela_visual) + 2
    ),
    dpi = 320,
    bg = "white"
  )
}

# ============================================================
# 9. FUNCAO PARA SALVAR GRAFICOS
# ============================================================

salvar_plot <- function(
    plot,
    nome,
    largura = 14,
    altura = 6,
    dpi = 320
) {
  
  caminho <- file.path(
    dir_saida,
    nome
  )
  
  ggsave(
    
    filename = caminho,
    
    plot = plot,
    
    width = largura,
    
    height = altura,
    
    dpi = dpi,
    
    bg = "white"
    
  )
  
  message(
    "Figura salva: ",
    normalizePath(
      caminho,
      winslash = "/",
      mustWork = FALSE
    )
  )
  
  invisible(caminho)
  
}



# ============================================================
# 10. CONFIGURACAO VISUAL DOS CORTES
# ============================================================

# IMPORTANTE:
# Os nomes abaixo devem corresponder aos valores de df_cortes$tipo.

cores_cortes <- c(
  "Corte candidato (diagnostico)" = "#E69F00",
  "Corte M1" = "#0072B2",
  "Corte M2" = "#009E73"
)

tipos_linha_cortes <- c(
  "Corte candidato (diagnostico)" = "solid",
  "Corte M1" = "dashed",
  "Corte M2" = "dotted"
)

# Conferir os tipos existentes
cat("\nTipos de cortes encontrados:\n")
print(unique(as.character(df_cortes$tipo)))

# Padronizar os tipos como fator
df_cortes$tipo <- factor(
  as.character(df_cortes$tipo),
  levels = names(cores_cortes)
)


# ============================================================
# 11. GRAFICO 1 - SERIE COMPLETA
# ============================================================

g_serie <- ggplot(
  df,
  aes(
    x = indice,
    y = serie
  )
) +
  
  geom_line(
    color = cor_serie,
    linewidth = 0.42,
    na.rm = TRUE
  ) +
  
  {
    if (nrow(df_eventos) > 0) {
      
      geom_point(
        data = df_eventos,
        aes(
          x = indice,
          y = serie
        ),
        inherit.aes = FALSE,
        color = "#2A9D8F",
        size = 1.35
      )
      
    }
  } +
  
  {
    if (nrow(df_alarm) > 0) {
      
      geom_point(
        data = df_alarm,
        aes(
          x = indice,
          y = serie
        ),
        inherit.aes = FALSE,
        color = cor_alarme,
        shape = 17,
        size = 2.2
      )
      
    }
  } +
  
  {
    if (nrow(df_cortes) > 0) {
      
      geom_vline(
        data = df_cortes,
        aes(
          xintercept = indice,
          color = tipo,
          linetype = tipo
        ),
        linewidth = 0.75,
        alpha = 0.95,
        inherit.aes = FALSE
      )
      
    }
  } +
  
  scale_color_manual(
    name = "Tipo de corte",
    values = cores_cortes,
    breaks = names(cores_cortes),
    drop = FALSE
  ) +
  
  scale_linetype_manual(
    name = "Tipo de corte",
    values = tipos_linha_cortes,
    breaks = names(tipos_linha_cortes),
    drop = FALSE
  ) +
  
  labs(
    title = "Serie temporal monitorada - AE + CUSUM",
    
    subtitle = paste0(
      length(alarmes),
      " alarmes registrados. Cortes do pos-processamento destacados."
    ),
    
    x = "Indice da observacao",
    y = "Pressao de Cabeca TPT"
  ) +
  
  tema_relatorio +
  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )


print(g_serie)

salvar_plot(
  g_serie,
  "01_serie_completa_alarmes_e_cortes.png",
  15,
  6
)


# ============================================================
# 12. GRAFICO 2 - ERRO DE RECONSTRUCAO MSE
# ============================================================

df_mse <- df[
  is.finite(df$mse) &
    df$mse > 0,
]

g_mse <- ggplot(
  df_mse,
  aes(
    x = indice,
    y = mse
  )
) +
  
  geom_line(
    color = "#35618D",
    linewidth = 0.4
  ) +
  
  scale_y_log10() +
  
  labs(
    title = "Erro de reconstrucao do Autoencoder",
    
    subtitle = "Escala logaritmica para facilitar a leitura dos valores extremos",
    
    x = "Indice da observacao",
    y = "MSE - escala log10"
  ) +
  
  tema_relatorio


print(g_mse)

salvar_plot(
  g_mse,
  "02_erro_reconstrucao_MSE.png",
  14,
  5
)


# ============================================================
# 13. GRAFICO 3 - LIMIAR TAU
# ============================================================

df_tau <- df[
  is.finite(df$tau),
]

g_tau <- ggplot(
  df_tau,
  aes(
    x = indice,
    y = tau
  )
) +
  
  geom_line(
    color = cor_tau,
    linewidth = 0.55
  ) +
  
  labs(
    title = "Limiar adaptativo tau",
    
    subtitle = "Limiar utilizado na comparacao MSE > tau",
    
    x = "Indice da observacao",
    y = "Tau"
  ) +
  
  tema_relatorio


print(g_tau)

salvar_plot(
  g_tau,
  "03_limiar_tau.png",
  14,
  4.5
)


# ============================================================
# 14. GRAFICO 4 - CUSUM POSITIVO
# ============================================================

df_cusum <- df[
  is.finite(df$cusum_pos) |
    is.finite(df$cusum_neg),
]


g_cusum_pos <- ggplot(
  df_cusum,
  aes(
    x = indice,
    y = cusum_pos
  )
) +
  
  geom_line(
    color = "#35618D",
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  
  geom_hline(
    yintercept = H_LOW,
    linetype = "dotted",
    color = cor_warning,
    linewidth = 0.7
  ) +
  
  geom_hline(
    yintercept = H_HIGH,
    linetype = "dashed",
    color = cor_alarme,
    linewidth = 0.7
  ) +
  
  labs(
    title = "CUSUM positivo",
    
    subtitle = paste0(
      "Warning: H_LOW = ",
      H_LOW,
      " | Drift: H_HIGH = ",
      H_HIGH
    ),
    
    x = "Indice da observacao",
    y = "CUSUM positivo"
  ) +
  
  tema_relatorio


print(g_cusum_pos)

salvar_plot(
  g_cusum_pos,
  "04_CUSUM_positivo.png",
  14,
  5
)


# ============================================================
# 15. GRAFICO 5 - CUSUM NEGATIVO
# ============================================================

g_cusum_neg <- ggplot(
  df_cusum,
  aes(
    x = indice,
    y = cusum_neg
  )
) +
  
  geom_line(
    color = "#7A5195",
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  
  geom_hline(
    yintercept = 0,
    color = "grey45",
    linewidth = 0.35
  ) +
  
  labs(
    title = "CUSUM negativo",
    
    subtitle = "Estatistica de acompanhamento exploratorio",
    
    x = "Indice da observacao",
    y = "CUSUM negativo"
  ) +
  
  tema_relatorio


print(g_cusum_neg)

salvar_plot(
  g_cusum_neg,
  "05_CUSUM_negativo.png",
  14,
  4.5
)


# ============================================================
# 16. GRAFICO 6 - Z-SCORE
# ============================================================

df_z <- df[
  is.finite(df$z),
]


g_z <- ggplot(
  df_z,
  aes(
    x = indice,
    y = z
  )
) +
  
  geom_line(
    color = "#35618D",
    linewidth = 0.4
  ) +
  
  geom_hline(
    yintercept = 0,
    color = "grey45",
    linewidth = 0.35
  ) +
  
  labs(
    title = "Z-score do erro de reconstrucao",
    
    subtitle = paste0(
      "Parametro K do CUSUM = ",
      K_FACTOR
    ),
    
    x = "Indice da observacao",
    y = "Z-score"
  ) +
  
  tema_relatorio


print(g_z)

salvar_plot(
  g_z,
  "06_zscore.png",
  14,
  4.5
)


# ============================================================
# 17. PAINEL GERAL DO PIPELINE
# ============================================================

painel_pipeline <- gridExtra::arrangeGrob(
  
  g_serie,
  g_mse,
  g_tau,
  g_cusum_pos,
  g_z,
  
  ncol = 1,
  
  heights = c(
    1.35,
    1,
    0.8,
    1,
    0.8
  )
  
)


grid::grid.newpage()

grid::grid.draw(
  painel_pipeline
)


ggsave(
  
  filename = file.path(
    dir_saida,
    "07_painel_geral_AE_CUSUM.png"
  ),
  
  plot = painel_pipeline,
  
  width = 15,
  height = 20,
  dpi = 320,
  bg = "white"
  
)


# ============================================================
# 18. AMPLIACOES DOS INTERVALOS DE ALARME
# ============================================================

if (length(alarmes) >= 2) {
  
  limites <- data.frame(
    
    inicio = head(
      alarmes,
      -1
    ),
    
    fim = tail(
      alarmes,
      -1
    )
    
  )
  
  limites <- limites[
    limites$fim > limites$inicio,
  ]
  
  
  lista_intervalos <- lapply(
    
    seq_len(nrow(limites)),
    
    function(i) {
      
      a <- limites$inicio[i]
      b <- limites$fim[i]
      
      d <- df[
        df$indice >= a &
          df$indice <= b,
        ,
        drop = FALSE
      ]
      
      d$corte_candidato <- (
        d$indice %in% cortes_candidatos
      )
      
      return(d)
      
    }
    
  )
  
  
  df_intervalos <- do.call(
    rbind,
    lista_intervalos
  )
  
  
  df_intervalos$intervalo <- factor(
    
    paste(
      limites$inicio,
      limites$fim,
      sep = "-"
    )[rep(
      seq_len(nrow(limites)),
      vapply(
        lista_intervalos,
        nrow,
        integer(1)
      )
    )],
    
    levels = paste(
      limites$inicio,
      limites$fim,
      sep = "-"
    )
    
  )
  
  
  g_intervalos <- ggplot(
    df_intervalos,
    aes(
      x = indice,
      y = serie
    )
  ) +
    
    geom_line(
      color = cor_serie,
      linewidth = 0.35
    ) +
    
    geom_point(
      
      data = df_intervalos[
        df_intervalos$corte_candidato,
        ,
        drop = FALSE
      ],
      
      aes(
        x = indice,
        y = serie
      ),
      
      inherit.aes = FALSE,
      
      color = cores_cortes[
        "Corte candidato (diagnostico)"
      ],
      
      size = 1.5
      
    ) +
    
    facet_wrap(
      ~ intervalo,
      scales = "free_x",
      ncol = 4
    ) +
    
    labs(
      
      title = "Intervalos delimitados pelos alarmes",
      
      subtitle = "Pontos laranja representam cortes candidatos do diagnostico",
      
      x = "Indice da observacao",
      y = "Pressao de Cabeca TPT"
      
    ) +
    
    tema_relatorio +
    
    theme(
      
      strip.text = element_text(
        size = 8
      ),
      
      axis.text = element_text(
        size = 7
      ),
      
      axis.title = element_text(
        size = 9
      )
      
    )
  
  
  print(g_intervalos)
  
  salvar_plot(
    g_intervalos,
    "08_intervalos_alarmados.png",
    16,
    14
  )
  
}


# ============================================================
# 19. AMPLIACOES DE INTERVALOS REPRESENTATIVOS
# ============================================================

intervalos_foco <- list(
  
  "996_1099" = c(996, 1099),
  
  "4458_4757" = c(4458, 4757),
  
  "5659_6039" = c(5659, 6039),
  
  "7735_7949" = c(7735, 7949),
  
  "10030_10374" = c(10030, 10374)
  
)


for (nome in names(intervalos_foco)) {
  
  faixa <- intervalos_foco[[nome]]
  
  d <- df[
    df$indice >= faixa[1] &
      df$indice <= faixa[2],
    ,
    drop = FALSE
  ]
  
  if (nrow(d) == 0) {
    next
  }
  
  
  d_corte <- df_cortes[
    df_cortes$indice >= faixa[1] &
      df_cortes$indice <= faixa[2],
    ,
    drop = FALSE
  ]
  
  
  d_alarme <- df_alarm[
    df_alarm$indice >= faixa[1] &
      df_alarm$indice <= faixa[2],
    ,
    drop = FALSE
  ]
  
  
  g_zoom <- ggplot(
    d,
    aes(
      x = indice,
      y = serie
    )
  ) +
    
    geom_line(
      color = cor_serie,
      linewidth = 0.65
    ) +
    
    {
      if (nrow(d_corte) > 0) {
        
        geom_vline(
          
          data = d_corte,
          
          aes(
            xintercept = indice,
            color = tipo,
            linetype = tipo
          ),
          
          linewidth = 1.0,
          alpha = 0.95,
          inherit.aes = FALSE
          
        )
        
      }
    } +
    
    {
      if (nrow(d_alarme) > 0) {
        
        geom_point(
          
          data = d_alarme,
          
          aes(
            x = indice,
            y = serie
          ),
          
          inherit.aes = FALSE,
          
          color = cor_alarme,
          
          shape = 17,
          
          size = 2.5
          
        )
        
      }
    } +
    
    scale_color_manual(
      name = "Tipo de corte",
      values = cores_cortes,
      breaks = names(cores_cortes),
      drop = FALSE
    ) +
    
    scale_linetype_manual(
      name = "Tipo de corte",
      values = tipos_linha_cortes,
      breaks = names(tipos_linha_cortes),
      drop = FALSE
    ) +
    
    labs(
      
      title = paste(
        "Ampliacao do intervalo",
        gsub("_", "-", nome)
      ),
      
      subtitle = "Laranja = candidato | Azul = M1 | Verde = M2 | Triangulos vermelhos = alarmes",
      
      x = "Indice da observacao",
      y = "Pressao de Cabeca TPT"
      
    ) +
    
    tema_relatorio +
    
    theme(
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  
  print(g_zoom)
  
  salvar_plot(
    
    g_zoom,
    
    paste0(
      "zoom_",
      nome,
      ".png"
    ),
    
    13,
    5.5
    
  )
  
}


# ============================================================
# 20. RESUMO DA VISUALIZACAO
# ============================================================

resumo <- data.frame(
  
  indicador = c(
    
    "Numero de observacoes",
    
    "Numero de alarmes CUSUM",
    
    "Numero de warnings",
    
    "Numero de anomalias MSE > tau",
    
    "Numero de cortes M1",
    
    "Numero de cortes M2",
    
    "Numero de cortes candidatos"
    
  ),
  
  quantidade = c(
    
    n,
    
    length(alarmes),
    
    sum(
      df$warning == 1,
      na.rm = TRUE
    ),
    
    sum(
      df$anomalia == 1,
      na.rm = TRUE
    ),
    
    length(cortes_m1),
    
    length(cortes_m2),
    
    length(cortes_candidatos)
    
  )
  
)

print(resumo)

write.csv(
  resumo,
  file.path(
    dir_saida,
    "resumo_visualizacao.csv"
  ),
  row.names = FALSE
)

cat("\nVisualizacoes geradas com sucesso!\n")
cat("Pasta:", dir_saida, "\n")


cat("\n")
cat("============================================\n")
cat("RESUMO DA VISUALIZACAO\n")
cat("============================================\n")

print(
  resumo,
  row.names = FALSE
)


write.csv(
  
  resumo,
  
  file.path(
    dir_saida,
    "resumo_visualizacao.csv"
  ),
  
  row.names = FALSE,
  
  fileEncoding = "UTF-8"
  
)


# ============================================================
# 20. FINALIZACAO
# ============================================================

cat("\n")
cat("============================================\n")
cat("VISUALIZACOES CONCLUIDAS\n")
cat("============================================\n")

cat(
  "Graficos exibidos no painel Plots do RStudio.\n"
)

cat(
  "Arquivos salvos em: ",
  normalizePath(
    dir_saida,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)
