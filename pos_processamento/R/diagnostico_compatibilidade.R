
# ============================================================
# DIAGNÓSTICO DE COMPATIBILIDADE ENTRE INTERVALOS CONSECUTIVOS
# ============================================================

dados <- tabela_consolidada

# Extrair a inclinação M0 como referência do comportamento
# predominante em cada intervalo.
dados$inclinacao_base <- as.numeric(
  gsub(",", ".", trimws(dados$inclinacoes_M0))
)

# Extrair a primeira inclinação do modelo M2.
extrair_primeira <- function(x) {
  if (is.na(x)) return(NA_real_)
  
  valores <- strsplit(as.character(x), "/", fixed = TRUE)[[1]]
  
  as.numeric(trimws(valores[1]))
}

# Extrair a última inclinação do modelo M2.
extrair_ultima <- function(x) {
  if (is.na(x)) return(NA_real_)
  
  valores <- strsplit(as.character(x), "/", fixed = TRUE)[[1]]
  
  as.numeric(trimws(tail(valores, 1)))
}

dados$M2_inicio <- vapply(
  dados$inclinacoes_M2,
  extrair_primeira,
  numeric(1)
)

dados$M2_fim <- vapply(
  dados$inclinacoes_M2,
  extrair_ultima,
  numeric(1)
)

# Comparar intervalos consecutivos.
comparacoes <- data.frame()

for (i in 1:(nrow(dados) - 1)) {
  
  atual <- dados[i, ]
  seguinte <- dados[i + 1, ]
  
  # Verificar se os intervalos realmente são consecutivos.
  consecutivos <- atual$alarme2 == seguinte$alarme1
  
  if (!consecutivos) next
  
  diferenca_base <- abs(
    atual$inclinacao_base - seguinte$inclinacao_base
  )
  
  # Compatibilidade entre a inclinação final do primeiro
  # intervalo e a inicial do intervalo seguinte.
  diferenca_M2 <- abs(
    atual$M2_fim - seguinte$M2_inicio
  )
  
  comparacoes <- rbind(
    comparacoes,
    data.frame(
      alarme_compartilhado = atual$alarme2,
      
      intervalo_anterior = paste(
        atual$alarme1, atual$alarme2, sep = "-"
      ),
      
      intervalo_seguinte = paste(
        seguinte$alarme1, seguinte$alarme2, sep = "-"
      ),
      
      diferenca_inclinacao_base = diferenca_base,
      
      diferenca_inclinacao_M2 = diferenca_M2,
      
      evidencia_anterior = atual$evidencia_M2,
      
      evidencia_seguinte = seguinte$evidencia_M2,
      
      estabilidade_anterior = atual$estabilidade_M2,
      
      estabilidade_seguinte = seguinte$estabilidade_M2,
      
      tendencia_anterior = atual$classificacoes_trend,
      
      tendencia_seguinte = seguinte$classificacoes_trend
    )
  )
}

print(comparacoes, row.names = FALSE)
