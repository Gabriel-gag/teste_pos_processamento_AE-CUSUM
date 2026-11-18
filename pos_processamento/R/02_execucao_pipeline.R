
# ============================================================
# 02_execucao_pipeline.R
# Processamento automatizado dos intervalos
# ============================================================

processar_intervalos <- function(
    intervalos,
    series,
    min_segmento = 20,
    passo = 5
) {
  
  resultados <- lapply(seq_len(nrow(intervalos)), function(i) {
    
    cat(
      "Processando intervalo",
      i, "de", nrow(intervalos), "\n"
    )
    
    analisar_intervalo_automatico(
      alarme1 = intervalos$alarme1[i],
      alarme2 = intervalos$alarme2[i],
      series = series,
      min_segmento = min_segmento,
      passo = passo
    )
  })
  
  resultados <- Filter(Negate(is.null), resultados)
  
  if (length(resultados) == 0) {
    return(data.frame())
  }
  
  do.call(rbind, resultados)
}