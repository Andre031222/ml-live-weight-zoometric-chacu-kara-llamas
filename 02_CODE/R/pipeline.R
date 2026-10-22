analizar_escenario <- function(escenario, particion, config) {
  message("  Escenario: ", escenario$etiqueta)
  predictores <- definir_predictores(particion$entrenamiento, escenario, config$semilla)
  modelos <- entrenar_modelos(particion$entrenamiento, predictores$variables, config)
  list(
    predictores = predictores,
    modelos = modelos,
    predicciones = predecir_conjuntos(modelos, particion)
  )
}

analizar_estrato <- function(estrato, datos, config) {
  message("Estrato: ", estrato$etiqueta)
  carpeta <- file.path(config$salida, estrato$id)
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  ruta <- \(archivo) file.path(carpeta, archivo)

  subconjunto <- filtrar_estrato(datos, estrato)
  particion <- particionar(subconjunto, config$proporcion_entrenamiento, config$semilla)
  escenarios <- ESCENARIOS |>
    split(seq_len(nrow(ESCENARIOS))) |>
    set_names(ESCENARIOS$id) |>
    map(\(escenario) analizar_escenario(escenario, particion, config))

  principal <- escenarios[[config$escenario_principal]]
  comparacion_principal <- comparar_con_mejor(principal$modelos, config)
  mejor <- principal$modelos[[comparacion_principal$modelo[1]]]

  graficar_correlaciones(particion$entrenamiento, escenarios$filtro_095$predictores$retenidas, ruta("fig_correlaciones.png"))
  graficar_boruta(escenarios$filtro_095$predictores, ruta("fig_boruta.png"))
  graficar_importancia(principal$modelos, ruta("fig_importancia.png"))
  graficar_observado_predicho(principal$predicciones, ruta("fig_observado_predicho.png"))
  graficar_correlacion_predicciones(principal$predicciones, ruta("fig_correlacion_predicciones.png"))
  saveRDS(map(escenarios, "modelos"), ruta("modelos.rds"))

  etiquetar <- \(tabla, escenario = NULL) {
    tabla <- mutate(tabla, estrato = estrato$etiqueta, .before = 1)
    if (is.null(escenario)) tabla else mutate(tabla, escenario = escenario, .after = estrato)
  }
  por_escenario <- \(funcion) {
    imap(escenarios, \(resultado, id) etiquetar(funcion(resultado), ESCENARIOS$etiqueta[ESCENARIOS$id == id])) |>
      bind_rows()
  }

  list(
    descriptiva = etiquetar(describir_estrato(subconjunto)),
    seleccion = por_escenario(\(r) tibble(
      n_entrenamiento = nrow(particion$entrenamiento),
      n_prueba = nrow(particion$prueba),
      eliminadas = paste(r$predictores$eliminadas, collapse = ", "),
      seleccionadas = paste(r$predictores$variables, collapse = ", "),
      numero_predictores = length(r$predictores$variables)
    )),
    boruta = por_escenario(\(r) r$predictores$estadisticos),
    metricas = por_escenario(\(r) resumir_metricas(r$predicciones)),
    validacion = por_escenario(\(r) resumir_validacion(r$modelos)),
    comparacion = por_escenario(\(r) comparar_con_mejor(r$modelos, config)),
    friedman = por_escenario(\(r) probar_friedman(r$modelos)),
    hiperparametros = por_escenario(\(r) extraer_hiperparametros(r$modelos)),
    medias = por_escenario(\(r) comparar_medias(r$predicciones)),
    permutacion = etiquetar(importancia_permutacion(mejor, particion$prueba)) |>
      mutate(modelo = comparacion_principal$modelo[1], .after = estrato),
    predicciones_mejor = principal$predicciones |>
      filter(modelo == comparacion_principal$modelo[1]) |>
      mutate(modelo = as.character(modelo)) |>
      etiquetar()
  )
}

unir_resultados <- function(resultados) {
  nombres <- names(resultados[[1]])
  set_names(map(nombres, \(nombre) bind_rows(map(resultados, nombre))), nombres)
}
