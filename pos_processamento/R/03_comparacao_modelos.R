
# ============================================================
# 03_comparacao_modelos.R
# Analise automatizada de um intervalo entre alarmes
# ============================================================

analisar_intervalo_automatico <- function(
    alarme1,
    alarme2,
    series,
    min_segmento = 20,
    passo = 5
) {
  
  y <- series[alarme1:alarme2]
  
  resultado <- comparar_modelos(
    y,
    min_segmento = min_segmento,
    passo = passo
  )
  
  if (is.null(resultado)) {
    return(NULL)
  }
  
  modelos <- resultado$modelos
  
  m0 <- modelos$M0
  m1 <- modelos$M1
  m2 <- modelos$M2
  
  converter_cortes <- function(modelo) {
    
    if (is.null(modelo)) {
      return(NA_character_)
    }
    
    if (length(modelo$cortes) == 0) {
      return("Nenhum")
    }
    
    paste(
      alarme1 + modelo$cortes - 1,
      collapse = " / "
    )
  }
  
  data.frame(
    alarme1 = alarme1,
    alarme2 = alarme2,
    
    BIC0 = m0$bic,
    BIC1 = m1$bic,
    BIC2 = if (is.null(m2)) NA_real_ else m2$bic,
    
    RMSE0 = m0$rmse,
    RMSE1 = m1$rmse,
    RMSE2 = if (is.null(m2)) NA_real_ else m2$rmse,
    
    corte_M1 = converter_cortes(m1),
    cortes_M2 = converter_cortes(m2),
    
    inclinacoes_M0 = paste(
      round(m0$inclinacoes, 3),
      collapse = " / "
    ),
    
    inclinacoes_M1 = paste(
      round(m1$inclinacoes, 3),
      collapse = " / "
    ),
    
    inclinacoes_M2 = if (is.null(m2)) {
      NA_character_
    } else {
      paste(round(m2$inclinacoes, 3), collapse = " / ")
    },
    
    modelo_vencedor = resultado$vencedor
  )
}