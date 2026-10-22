# Ejecutar desde la raíz del proyecto (donde está llamas.Rproj): Rscript 02_CODE/analisis.R
source("02_CODE/R/cargar.R")

dir.create(config$salida, showWarnings = FALSE)
registerDoParallel(cores = config$nucleos)

datos <- preparar_datos(config, correcciones_campo)
llamas <- datos$llamas

resultados <- ESTRATOS |>
  split(seq_len(nrow(ESTRATOS))) |>
  map(\(estrato) analizar_estrato(estrato, llamas, config)) |>
  unir_resultados()

saveRDS(resultados, file.path(config$salida, "resultados.rds"))

orden_estratos <- ESTRATOS$etiqueta
ordenar <- \(tabla) mutate(tabla, estrato = factor(estrato, levels = orden_estratos)) |> arrange(estrato)

exportar_figuras(llamas, resultados, config)

mejores <- resultados$comparacion |>
  filter(mejor) |>
  ordenar()

redondear <- \(tabla) mutate(tabla, across(where(is.numeric), \(x) round(x, 4)))

write_xlsx(
  map(list(
    atipicos = datos$atipicos,
    depuracion = datos$cambios,
    descriptiva_grupos = describir_por_grupo(llamas),
    anova_factorial = anova_factorial(llamas),
    homogeneidad_pendientes = probar_homogeneidad(llamas),
    descriptiva_estratos = ordenar(resultados$descriptiva),
    seleccion_variables = ordenar(resultados$seleccion),
    boruta = ordenar(resultados$boruta),
    metricas = ordenar(resultados$metricas),
    validacion_cruzada = ordenar(resultados$validacion),
    comparacion_modelos = ordenar(resultados$comparacion),
    friedman = ordenar(resultados$friedman),
    mejor_modelo = mejores,
    importancia_permutacion = ordenar(resultados$permutacion),
    hiperparametros = ordenar(resultados$hiperparametros),
    comparacion_medias = ordenar(resultados$medias)
  ), redondear),
  file.path(config$salida, "tablas_resultados.xlsx")
)

stopImplicitCluster()
writeLines(capture.output(sessionInfo()), file.path(config$salida, "session_info.txt"))
