# 1. Carregar funções
source("R/ajustar_segmentacao.R")
source("R/comparar_modelos.R")
source("R/03_comparacao_modelos.R")
source("R/02_execucao_pipeline.R")
source("R/04_diagnostico_intervalos.R")
source("R/04_diagnostico_mudancas.R")
source("R/05_analise_residuos.R")
source("R/diagnostico_compatibilidade")
source("R/comparar_modelos")
source("R/contrucao_grupos")
source("R/classificacao_compatibilidade")
source("R/06_visualizacao_resultados")

# 2. Parâmetros
min_segmento <- 20
passo <- 5

# 3. Executar pipeline
resultados <- processar_intervalos(
intervalos = tabela_consolidada,
series = series,
min_segmento = min_segmento,
passo = passo
)

# 4. Calcular diferenças entre os BICs
resultados$BIC_segundo <- apply(
resultados[, c("BIC0", "BIC1", "BIC2")],
1,
function(x) sort(x, na.last = TRUE)[2]
)

resultados$BIC_melhor <- apply(
resultados[, c("BIC0", "BIC1", "BIC2")],
1,
min,
na.rm = TRUE
)

resultados$delta_BIC <-
resultados$BIC_segundo - resultados$BIC_melhor

# 5. Visualizar resultados
print(resultados)


# Diagnóstico dos intervalos
diagnostico <- diagnosticar_intervalos(resultados)

# Visualizar diagnóstico
print(diagnostico)

# Salvar resultados
dir.create("resultados", showWarnings = FALSE)

write.csv(
resultados,
"resultados/eventos_processados.csv",
row.names = FALSE
)

write.csv(
diagnostico,
"resultados/diagnostico_intervalos.csv",
row.names = FALSE
)

# Análise dos resíduos
analise_residuos <- processar_analise_residuos(
resultados = resultados,
series = series
)

# Salvar resultados
write.csv(
analise_residuos$geral,
"resultados/diagnostico_residuos_modelos.csv",
row.names = FALSE
)

write.csv(
analise_residuos$segmentos,
"resultados/diagnostico_residuos_segmentos.csv",
row.names = FALSE
)


cat("\nPipeline finalizado!\n")