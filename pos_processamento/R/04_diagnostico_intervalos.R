
diagnosticar_intervalos <- function(resultados) {
  
  # Extrai os valores numéricos de uma coluna de inclinações
  extrair_inclinacoes <- function(x) {
    if (is.na(x) || x == "" || x == "NA") {
      return(numeric(0))
    }
    
    as.numeric(trimws(strsplit(x, "/", fixed = TRUE)[[1]]))
  }
  
  # Extrai os cortes absolutos
  extrair_cortes <- function(x) {
    if (is.na(x) || x == "" || x == "Nenhum") {
      return(numeric(0))
    }
    
    as.numeric(trimws(strsplit(x, "/", fixed = TRUE)[[1]]))
  }
  
  diagnosticos <- lapply(seq_len(nrow(resultados)), function(i) {
    
    linha <- resultados[i, ]
    
    # BICs disponíveis
    bics <- c(
      M0 = linha$BIC0,
      M1 = linha$BIC1,
      M2 = linha$BIC2
    )
    
    bics_validos <- sort(bics[is.finite(bics)])
    
    # Diferença entre o melhor e o segundo melhor BIC
    delta <- if (length(bics_validos) >= 2) {
      bics_validos[2] - bics_validos[1]
    } else {
      NA_real_
    }
    
    # Classificação da força da preferência
    forca <- if (is.na(delta)) {
      "Não calculável"
    } else if (delta < 2) {
      "Diferença pequena"
    } else if (delta < 6) {
      "Evidência positiva"
    } else if (delta < 10) {
      "Evidência forte"
    } else {
      "Evidência muito forte"
    }
    
    vencedor <- as.character(linha$modelo_vencedor)
    
    # Inclinações do modelo vencedor
    inclinacoes <- extrair_inclinacoes(
      linha[[paste0("inclinacoes_", vencedor)]]
    )
    
    # Cortes do modelo vencedor
    if (vencedor == "M0") {
      cortes <- numeric(0)
    } else if (vencedor == "M1") {
      cortes <- extrair_cortes(linha$corte_M1)
    } else {
      cortes <- extrair_cortes(linha$cortes_M2)
    }
    
    # Comprimento dos segmentos
    inicio <- linha$alarme1
    fim <- linha$alarme2
    
    comprimentos <- if (length(cortes) == 0) {
      fim - inicio + 1
    } else {
      c(
        cortes[1] - inicio + 1,
        if (length(cortes) > 1) diff(cortes),
        fim - tail(cortes, 1)
      )
    }
    
    # Variação absoluta entre inclinações consecutivas
    diferencas <- if (length(inclinacoes) >= 2) {
      abs(diff(inclinacoes))
    } else {
      numeric(0)
    }
    
    data.frame(
      alarme1 = inicio,
      alarme2 = fim,
      modelo_vencedor = vencedor,
      modelos_comparados = length(bics_validos),
      comparacao = if (length(bics_validos) == 3) {
        "Completa"
      } else {
        "Parcial"
      },
      delta_BIC = delta,
      forca_preferencia = forca,
      numero_cortes = length(cortes),
      inclinacoes = paste(inclinacoes, collapse = " / "),
      maior_variacao_inclinacao = if (length(diferencas) > 0) {
        max(diferencas)
      } else {
        NA_real_
      },
      variacao_total_inclinacao = if (length(inclinacoes) >= 2) {
        abs(tail(inclinacoes, 1) - inclinacoes[1])
      } else {
        NA_real_
      },
      menor_segmento = min(comprimentos)
    )
  })
  
  do.call(rbind, diagnosticos)
}


plotar_intervalo <- function(alarme1, alarme2, resultados, series) {
  
  linha <- resultados[
    resultados$alarme1 == alarme1 &
      resultados$alarme2 == alarme2,
  ]
  
  if (nrow(linha) != 1) {
    stop("Intervalo não encontrado ou duplicado.")
  }
  
  y <- series[alarme1:alarme2]
  vencedor <- as.character(linha$modelo_vencedor)
  
  # Recuperar cortes absolutos do modelo vencedor
  if (vencedor == "M0") {
    cortes_abs <- numeric(0)
    
  } else if (vencedor == "M1") {
    cortes_abs <- as.numeric(linha$corte_M1)
    
  } else {
    cortes_abs <- as.numeric(
      trimws(strsplit(
        as.character(linha$cortes_M2),
        "/",
        fixed = TRUE
      )[[1]])
    )
  }
  
  cortes_abs <- cortes_abs[is.finite(cortes_abs)]
  
  # Converter cortes absolutos para posições locais
  cortes_locais <- cortes_abs - alarme1 + 1
  
  # Ajustar novamente o modelo vencedor
  modelo <- ajustar_segmentacao(y, cortes_locais)
  
  ajustado <- y - modelo$residuos
  indices <- alarme1:alarme2
  
  # Série original
  # Centralizar a série para facilitar a visualização
  y_centralizado <- y - y[1]
  ajustado_centralizado <- ajustado - y[1]
  
  plot(
    indices,
    y_centralizado,
    type = "p",
    pch = 16,
    cex = 0.5,
    col = "gray40",
    main = paste("Intervalo", alarme1, "-", alarme2),
    xlab = "Índice temporal",
    ylab = "Variação em relação ao início"
  )
  
  lines(
    indices,
    ajustado_centralizado,
    col = "red",
    lwd = 2
  )
  
  if (length(cortes_abs) > 0) {
    abline(
      v = cortes_abs,
      col = "blue",
      lty = 2,
      lwd = 2
    )
  }
  
  legend(
    "topright",
    legend = c("Observações", "Ajuste segmentado", "Cortes"),
    col = c("gray40", "red", "blue"),
    pch = c(16, NA, NA),
    lty = c(NA, 1, 2),
    lwd = c(NA, 2, 2),
    cex = 0.7
  )
}