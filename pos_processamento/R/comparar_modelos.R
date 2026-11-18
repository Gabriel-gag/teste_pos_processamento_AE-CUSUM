
# ============================================================
# comparar_modelos.R
# Comparacao entre modelos de segmentacao M0, M1 e M2
# ============================================================

comparar_modelos <- function(y,
                             min_segmento = 20,
                             passo = 5) {
  
  n <- length(y)
  
  if (n < 2 * min_segmento + 1) {
    return(NULL)
  }
  
  # M0: sem cortes
  m0 <- ajustar_segmentacao(y)
  
  # Posicoes candidatas
  candidatos <- seq(
    min_segmento,
    n - min_segmento,
    by = passo
  )
  
  if (length(candidatos) == 0) {
    return(NULL)
  }
  
  # M1: um corte
  modelos_m1 <- lapply(candidatos, function(c1) {
    ajustar_segmentacao(y, c1)
  })
  
  melhor_m1 <- modelos_m1[[
    which.min(vapply(modelos_m1, function(m) m$bic, numeric(1)))
  ]]
  
  # M2: dois cortes
  combinacoes <- combn(candidatos, 2)
  
  # Distancia minima entre cortes
  distancias <- combinacoes[2, ] - combinacoes[1, ]
  
  combinacoes <- combinacoes[
    ,
    distancias >= min_segmento,
    drop = FALSE
  ]
  
  modelos_m2 <- if (ncol(combinacoes) > 0) {
    
    lapply(seq_len(ncol(combinacoes)), function(i) {
      ajustar_segmentacao(y, combinacoes[, i])
    })
    
  } else {
    list()
  }
  
  melhor_m2 <- if (length(modelos_m2) > 0) {
    
    modelos_m2[[
      which.min(vapply(modelos_m2, function(m) m$bic, numeric(1)))
    ]]
    
  } else {
    NULL
  }
  
  modelos <- list(
    M0 = m0,
    M1 = melhor_m1,
    M2 = melhor_m2
  )
  
  bics <- vapply(modelos, function(m) {
    if (is.null(m)) Inf else m$bic
  }, numeric(1))
  
  modelo_vencedor <- names(which.min(bics))
  
  list(
    modelos = modelos,
    vencedor = modelo_vencedor
  )
}


extrair_metricas_modelo <- function(resultado) {
  
  modelos <- resultado$modelos
  
  data.frame(
    modelo = names(modelos),
    
    BIC = vapply(modelos, function(m) {
      if (is.null(m)) NA_real_ else m$bic
    }, numeric(1)),
    
    RMSE = vapply(modelos, function(m) {
      if (is.null(m)) NA_real_ else m$rmse
    }, numeric(1))
  )
}