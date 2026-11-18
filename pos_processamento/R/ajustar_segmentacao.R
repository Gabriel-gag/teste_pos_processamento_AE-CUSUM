
# ============================================================
# ajustar_segmentacao.R
# Ajuste de modelos de regressao segmentada
# ============================================================

ajustar_segmentacao <- function(y, cortes = integer()) {
  
  n <- length(y)
  x <- seq_len(n)
  
  if (length(cortes) == 0) {
    
    X <- cbind(1, x)
    
  } else if (length(cortes) == 1) {
    
    X <- cbind(
      1,
      x,
      pmax(0, x - cortes[1])
    )
    
  } else {
    
    X <- cbind(
      1,
      x,
      pmax(0, x - cortes[1]),
      pmax(0, x - cortes[2])
    )
  }
  
  ajuste <- lm.fit(X, y)
  
  residuos <- ajuste$residuals
  rss <- sum(residuos^2)
  
  k <- ncol(X) + 1
  
  bic <- if (rss > 0) {
    n * log(rss / n) + k * log(n)
  } else {
    -Inf
  }
  
  rmse <- sqrt(rss / n)
  
  coeficientes <- ajuste$coefficients
  
  inclinacoes <- if (length(cortes) == 0) {
    
    coeficientes[2]
    
  } else if (length(cortes) == 1) {
    
    c(
      coeficientes[2],
      coeficientes[2] + coeficientes[3]
    )
    
  } else {
    
    c(
      coeficientes[2],
      coeficientes[2] + coeficientes[3],
      coeficientes[2] + coeficientes[3] + coeficientes[4]
    )
  }
  
  list(
    bic = bic,
    rmse = rmse,
    cortes = cortes,
    inclinacoes = inclinacoes,
    residuos = residuos
  )
}