
avaliar_residuos_modelo <- function(y, cortes = integer(),
                                    nome_modelo = "M0") {
  
  n <- length(y)
  
  # Ajustar regressão segmentada
  ajuste <- ajustar_segmentacao(y, cortes)
  
  residuos <- as.numeric(ajuste$residuos)
  
  # Identificar o segmento de cada observação
  grupo <- findInterval(seq_len(n) - 1, cortes) + 1L
  
  # Estatísticas gerais
  rmse <- sqrt(mean(residuos^2))
  mae <- mean(abs(residuos))
  media_residuos <- mean(residuos)
  desvio_residuos <- sd(residuos)
  
  # Autocorrelação de primeira ordem
  acf1 <- if (n >= 2) {
    as.numeric(acf(
      residuos,
      lag.max = 1,
      plot = FALSE
    )$acf[2])
  } else {
    NA_real_
  }
  
  # Teste Ljung-Box
  lag_teste <- min(10, floor(n / 5))
  
  p_ljung <- if (lag_teste >= 2) {
    tryCatch(
      Box.test(
        residuos,
        lag = lag_teste,
        type = "Ljung-Box"
      )$p.value,
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }
  
  # Estatísticas por segmento
  segmentos <- lapply(sort(unique(grupo)), function(g) {
    
    r <- residuos[grupo == g]
    
    data.frame(
      modelo = nome_modelo,
      segmento = g,
      n = length(r),
      media_residuos = mean(r),
      desvio_padrao = sd(r),
      RMSE = sqrt(mean(r^2)),
      MAE = mean(abs(r))
    )
  })
  
  segmentos <- do.call(rbind, segmentos)
  
  # Teste de igualdade de dispersão entre segmentos
  p_fligner <- if (length(unique(grupo)) >= 2) {
    tryCatch(
      fligner.test(
        residuos,
        factor(grupo)
      )$p.value,
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }
  
  geral <- data.frame(
    modelo = nome_modelo,
    n = n,
    RMSE = rmse,
    MAE = mae,
    media_residuos = media_residuos,
    desvio_padrao = desvio_residuos,
    ACF1 = acf1,
    p_ljung_box = p_ljung,
    p_fligner = p_fligner
  )
  
  list(
    geral = geral,
    segmentos = segmentos,
    residuos = residuos,
    grupo = grupo,
    ajuste = ajuste
  )
}


analisar_residuos_intervalo <- function(
    linha, series
) {
  
  inicio <- linha$alarme1
  fim <- linha$alarme2
  
  y <- series[inicio:fim]
  
  # Converter cortes absolutos em posições locais
  converter_cortes_locais <- function(x) {
    
    if (is.na(x) || x == "" || x == "Nenhum") {
      return(integer(0))
    }
    
    cortes <- as.numeric(
      trimws(strsplit(x, "/", fixed = TRUE)[[1]])
    )
    
    cortes <- cortes[is.finite(cortes)]
    
    as.integer(cortes - inicio + 1)
  }
  
  # Cortes dos modelos
  cortes_m1 <- converter_cortes_locais(
    as.character(linha$corte_M1)
  )
  
  cortes_m2 <- converter_cortes_locais(
    as.character(linha$cortes_M2)
  )
  
  # Avaliar modelos
  m0 <- avaliar_residuos_modelo(
    y,
    cortes = integer(0),
    nome_modelo = "M0"
  )
  
  m1 <- avaliar_residuos_modelo(
    y,
    cortes = cortes_m1,
    nome_modelo = "M1"
  )
  
  m2 <- if (length(cortes_m2) == 2) {
    avaliar_residuos_modelo(
      y,
      cortes = cortes_m2,
      nome_modelo = "M2"
    )
  } else {
    NULL
  }
  
  modelos <- list(M0 = m0, M1 = m1, M2 = m2)
  
  # Consolidar estatísticas gerais
  geral <- do.call(
    rbind,
    lapply(names(modelos), function(nome) {
      
      modelo <- modelos[[nome]]
      
      if (is.null(modelo)) return(NULL)
      
      resultado <- modelo$geral
      
      resultado$alarme1 <- inicio
      resultado$alarme2 <- fim
      resultado$BIC <- linha[[paste0("BIC", substring(nome, 2))]]
      
      resultado[, c(
        "alarme1", "alarme2", "modelo",
        "BIC", "n", "RMSE", "MAE",
        "media_residuos", "desvio_padrao",
        "ACF1", "p_ljung_box", "p_fligner"
      )]
    })
  )
  
  # Consolidar estatísticas por segmento
  segmentos <- do.call(
    rbind,
    lapply(names(modelos), function(nome) {
      
      modelo <- modelos[[nome]]
      
      if (is.null(modelo)) return(NULL)
      
      resultado <- modelo$segmentos
      
      resultado$alarme1 <- inicio
      resultado$alarme2 <- fim
      
      resultado[, c(
        "alarme1", "alarme2", "modelo",
        "segmento", "n", "media_residuos",
        "desvio_padrao", "RMSE", "MAE"
      )]
    })
  )
  
  list(
    geral = geral,
    segmentos = segmentos,
    modelos = modelos
  )
}


processar_analise_residuos <- function(resultados, series) {
  
  lista <- lapply(seq_len(nrow(resultados)), function(i) {
    
    cat(
      "Analisando resíduos do intervalo",
      i, "de", nrow(resultados), "\n"
    )
    
    analisar_residuos_intervalo(
      linha = resultados[i, ],
      series = series
    )
  })
  
  geral <- do.call(
    rbind,
    lapply(lista, function(x) x$geral)
  )
  
  segmentos <- do.call(
    rbind,
    lapply(lista, function(x) x$segmentos)
  )
  
  list(
    geral = geral,
    segmentos = segmentos,
    detalhes = lista
  )
}


analisar_acf_segmentos <- function(modelo, max_lag = 10) {
  
  residuos <- modelo$residuos
  grupo <- modelo$grupo
  
  resultados <- lapply(sort(unique(grupo)), function(g) {
    
    r <- residuos[grupo == g]
    n <- length(r)
    
    lag_maximo <- min(max_lag, n - 1)
    
    acf_resultado <- acf(
      r,
      lag.max = lag_maximo,
      plot = FALSE
    )
    
    data.frame(
      segmento = g,
      n = n,
      ACF1 = if (n >= 2) {
        as.numeric(acf_resultado$acf[2])
      } else {
        NA_real_
      },
      ACF2 = if (n >= 3) {
        as.numeric(acf_resultado$acf[3])
      } else {
        NA_real_
      },
      p_ljung_box = if (n >= 5) {
        tryCatch(
          Box.test(
            r,
            lag = min(5, n - 1),
            type = "Ljung-Box"
          )$p.value,
          error = function(e) NA_real_
        )
      } else {
        NA_real_
      }
    )
  })
  
  do.call(rbind, resultados)
}


plotar_diagnostico_residuos <- function(
    analise, indice, modelo = "M2"
) {
  
  detalhe <- analise$detalhes[[indice]]
  resultado <- detalhe$modelos[[modelo]]
  
  residuos <- resultado$residuos
  grupo <- resultado$grupo
  
  n <- length(residuos)
  indice_local <- seq_len(n)
  
  par(mfrow = c(2, 2))
  
  # 1. Resíduos ao longo do tempo
  plot(
    indice_local, residuos,
    pch = 16,
    cex = 0.5,
    xlab = "Índice local",
    ylab = "Resíduo",
    main = paste("Resíduos -", modelo)
  )
  abline(h = 0, lty = 2)
  
  # 2. Resíduos com suavização
  plot(
    indice_local, residuos,
    pch = 16,
    cex = 0.5,
    xlab = "Índice local",
    ylab = "Resíduo",
    main = "Tendência dos resíduos"
  )
  abline(h = 0, lty = 2)
  lines(
    lowess(indice_local, residuos),
    lwd = 2
  )
  
  # 3. Resíduos por segmento
  boxplot(
    residuos ~ factor(grupo),
    xlab = "Segmento",
    ylab = "Resíduo",
    main = "Distribuição dos resíduos"
  )
  abline(h = 0, lty = 2)
  
  # 4. ACF
  acf(
    residuos,
    main = "ACF dos resíduos"
  )
  
  par(mfrow = c(1, 1))
}
verificar_acf_mudanca <- function(
    series,
    corte,
    alarme1,
    alarme2,
    contexto = 40
) {
  
  inicio <- max(alarme1, corte - contexto)
  fim <- min(alarme2, corte + contexto)
  
  diferencas <- diff(series[inicio:fim])
  
  list(
    antes = acf(
      diferencas[1:(corte - inicio)],
      plot = FALSE
    ),
    depois = acf(
      diferencas[(corte - inicio + 1):length(diferencas)],
      plot = FALSE
    )
  )
}