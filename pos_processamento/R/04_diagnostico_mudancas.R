
diagnosticar_mudanca_interna <- function(
    alarme1,
    alarme2,
    series,
    contexto = 40,
    min_obs = 10,
    min_segmento = 20,
    passo = 5
) {
  
  # --------------------------------------------------
  # A. Ajustar os modelos originais do intervalo
  # --------------------------------------------------
  
  y <- series[alarme1:alarme2]
  
  comparacao <- comparar_modelos(
    y,
    min_segmento = min_segmento,
    passo = passo
  )
  
  if (is.null(comparacao)) {
    return(data.frame(
      alarme1 = alarme1,
      alarme2 = alarme2,
      status = "Intervalo insuficiente"
    ))
  }
  
  modelo_base <- comparacao$modelos[[comparacao$vencedor]]
  residuos <- modelo_base$residuos
  
  # ACF de primeira ordem dos resíduos
  acf1 <- if (
    length(residuos) > 2 &&
    sd(residuos[-1]) > 0 &&
    sd(residuos[-length(residuos)]) > 0
  ) {
    cor(
      residuos[-1],
      residuos[-length(residuos)]
    )
  } else {
    NA_real_
  }
  
  
  # --------------------------------------------------
  # B. Procurar mudança nas primeiras diferenças
  # --------------------------------------------------
  
  candidatos <- seq.int(alarme1 + 1, alarme2 - 1)
  
  avaliados <- lapply(candidatos, function(corte) {
    
    # Restringir a janela aos limites do intervalo
    inicio <- max(alarme1, corte - contexto)
    fim <- min(alarme2, corte + contexto)
    
    if (fim <= inicio) return(NULL)
    
    valores <- series[inicio:fim]
    
    if (any(!is.finite(valores))) return(NULL)
    
    diferencas <- diff(valores)
    
    # Índice da observação final de cada diferença
    indices <- (inicio + 1):fim
    
    antes <- diferencas[indices < corte]
    depois <- diferencas[indices >= corte]
    
    if (
      length(antes) < min_obs ||
      length(depois) < min_obs
    ) {
      return(NULL)
    }
    
    rss0 <- sum((diferencas - mean(diferencas))^2)
    
    rss1 <- sum((antes - mean(antes))^2) +
      sum((depois - mean(depois))^2)
    
    n <- length(diferencas)
    
    rss0 <- max(rss0, .Machine$double.eps)
    rss1 <- max(rss1, .Machine$double.eps)
    
    bic0 <- n * log(rss0 / n) + 2 * log(n)
    bic1 <- n * log(rss1 / n) + 3 * log(n)
    
    data.frame(
      corte = corte,
      n_antes = length(antes),
      n_depois = length(depois),
      media_antes = mean(antes),
      media_depois = mean(depois),
      sd_antes = sd(antes),
      sd_depois = sd(depois),
      BIC_sem_mudanca = bic0,
      BIC_com_mudanca = bic1,
      delta_BIC = bic0 - bic1
    )
  })
  
  avaliados <- Filter(Negate(is.null), avaliados)
  
  if (length(avaliados) == 0) {
    return(data.frame(
      alarme1 = alarme1,
      alarme2 = alarme2,
      modelo_base = comparacao$vencedor,
      acf1_residuos = acf1,
      status = "Sem candidatos com observacoes suficientes"
    ))
  }
  
  avaliados <- do.call(rbind, avaliados)
  
  # Melhor posição segundo o BIC com mudança
  melhor <- avaliados[
    which.max(avaliados$delta_BIC),
  ]
  
  delta <- melhor$delta_BIC
  
  # Classificação descritiva da diferença de BIC
  forca <- if (delta < 0) {
    "Sem preferência por mudança"
  } else if (delta < 2) {
    "Diferença pequena"
  } else if (delta < 6) {
    "Evidência positiva"
  } else if (delta < 10) {
    "Evidência forte"
  } else {
    "Evidência muito forte"
  }
  
  
  
  comparar_mudancas_diferencas <- function(
    series,
    alarme1,
    alarme2,
    min_segmento = 20
  ) {
    
    # Primeiras diferenças dentro do intervalo
    y <- diff(series[alarme1:alarme2])
    
    if (any(!is.finite(y))) {
      stop("Existem valores ausentes ou não finitos no intervalo.")
    }
    
    n <- length(y)
    
    if (n < 3 * min_segmento) {
      stop("Intervalo insuficiente para comparar até duas mudanças.")
    }
    
    # RSS de cada segmento usando somas acumuladas
    soma <- c(0, cumsum(y))
    soma2 <- c(0, cumsum(y^2))
    
    calcular_rss <- function(inicio, fim) {
      quantidade <- fim - inicio + 1
      s <- soma[fim + 1] - soma[inicio]
      s2 <- soma2[fim + 1] - soma2[inicio]
      
      max(s2 - s^2 / quantidade, 0)
    }
    
    calcular_bic <- function(rss, parametros) {
      rss <- max(rss, .Machine$double.eps)
      n * log(rss / n) + parametros * log(n)
    }
    
    # M0: nenhuma mudança
    rss0 <- calcular_rss(1, n)
    bic0 <- calcular_bic(rss0, 2)
    
    # M1: uma mudança
    melhor1 <- Inf
    corte1 <- NA_integer_
    
    for (c1 in seq(min_segmento, n - min_segmento)) {
      
      rss <- calcular_rss(1, c1) +
        calcular_rss(c1 + 1, n)
      
      bic <- calcular_bic(rss, 3)
      
      if (bic < melhor1) {
        melhor1 <- bic
        corte1 <- c1
      }
    }
    
    # M2: duas mudanças
    melhor2 <- Inf
    cortes2 <- c(NA_integer_, NA_integer_)
    
    for (c1 in seq(min_segmento, n - 2 * min_segmento)) {
      
      for (c2 in seq(
        c1 + min_segmento,
        n - min_segmento
      )) {
        
        rss <- calcular_rss(1, c1) +
          calcular_rss(c1 + 1, c2) +
          calcular_rss(c2 + 1, n)
        
        bic <- calcular_bic(rss, 4)
        
        if (bic < melhor2) {
          melhor2 <- bic
          cortes2 <- c(c1, c2)
        }
      }
    }
    
    resultados <- data.frame(
      modelo = c("M0", "M1", "M2"),
      BIC = c(bic0, melhor1, melhor2)
    )
    
    resultados$delta_BIC <- min(resultados$BIC) -
      resultados$BIC
    
    vencedor <- resultados$modelo[
      which.min(resultados$BIC)
    ]
    
    list(
      resultados = resultados,
      vencedor = vencedor,
      cortes_M1 = if (!is.na(corte1)) {
        alarme1 + corte1
      } else NA_integer_,
      cortes_M2 = alarme1 + cortes2,
      n = n
    )
  }
  
  resumir_segmentos <- function(series, alarme1, alarme2, cortes) {
    
    y <- diff(series[alarme1:alarme2])
    
    # Converter cortes globais para posições locais
    cortes_locais <- cortes - alarme1
    
    limites <- c(0, cortes_locais, length(y))
    
    resultado <- lapply(seq_len(length(limites) - 1), function(i) {
      
      inicio <- limites[i] + 1
      fim <- limites[i + 1]
      
      segmento <- y[inicio:fim]
      
      data.frame(
        segmento = paste0("S", i),
        inicio = inicio,
        fim = fim,
        n = length(segmento),
        media = mean(segmento),
        desvio_padrao = sd(segmento),
        minimo = min(segmento),
        maximo = max(segmento)
      )
    })
    
    do.call(rbind, resultado)
  }
  
  
  inspecionar_segmento <- function(
    series,
    alarme1,
    alarme2,
    inicio_local,
    fim_local
  ) {
    
    diferencas <- diff(series[alarme1:alarme2])
    
    indices <- inicio_local:fim_local
    
    dados <- data.frame(
      posicao_local = indices,
      indice_global = alarme1 + indices,
      diferenca = diferencas[indices]
    )
    
    cat("\nResumo do segmento:\n")
    print(summary(dados$diferenca))
    
    cat("\nMedidas adicionais:\n")
    print(data.frame(
      media = mean(dados$diferenca),
      mediana = median(dados$diferenca),
      desvio_padrao = sd(dados$diferenca),
      IQR = IQR(dados$diferenca),
      minimo = min(dados$diferenca),
      maximo = max(dados$diferenca)
    ))
    
    cat("\nValores ordenados por magnitude:\n")
    print(
      dados[order(dados$diferenca), ],
      row.names = FALSE
    )
    
    invisible(dados)
  }
  # --------------------------------------------------
  # C. Resultado consolidado
  # --------------------------------------------------
  
  data.frame(
    alarme1 = alarme1,
    alarme2 = alarme2,
    modelo_base = comparacao$vencedor,
    acf1_residuos = acf1,
    corte_candidato = melhor$corte,
    n_antes = melhor$n_antes,
    n_depois = melhor$n_depois,
    media_antes = melhor$media_antes,
    media_depois = melhor$media_depois,
    sd_antes = melhor$sd_antes,
    sd_depois = melhor$sd_depois,
    BIC_sem_mudanca = melhor$BIC_sem_mudanca,
    BIC_com_mudanca = melhor$BIC_com_mudanca,
    delta_BIC = delta,
    forca_evidencia = forca,
    status = "Analisado"
  )
}