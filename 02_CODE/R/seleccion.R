ESCENARIOS <- tribble(
  ~id,          ~etiqueta,                              ~umbral, ~metodo,
  "filtro_075", "A. Filtro 0,75 + Boruta (referencia)", 0.75,    "caret",
  "filtro_095", "B. Filtro guiado 0,95 + Boruta",       0.95,    "guiado",
  "completo",   "C. Doce medidas",                      NA,      NA
)

filtrar_colinealidad <- function(entrenamiento, umbral, metodo = "guiado") {
  if (metodo == "caret") {
    eliminadas <- findCorrelation(cor(entrenamiento[ZOOMETRICAS], method = "spearman"),
                                  cutoff = umbral, names = TRUE, exact = TRUE)
    return(list(retenidas = setdiff(ZOOMETRICAS, eliminadas), eliminadas = eliminadas))
  }
  correlacion <- abs(cor(entrenamiento[ZOOMETRICAS], method = "spearman"))
  relevancia <- abs(cor(entrenamiento[ZOOMETRICAS], entrenamiento$PEVI, method = "spearman"))[, 1]
  diag(correlacion) <- 0
  retenidas <- ZOOMETRICAS
  repeat {
    actual <- correlacion[retenidas, retenidas]
    if (max(actual) < umbral) break
    par <- retenidas[which(actual == max(actual), arr.ind = TRUE)[1, ]]
    retenidas <- setdiff(retenidas, par[which.min(relevancia[par])])
  }
  list(retenidas = retenidas, eliminadas = setdiff(ZOOMETRICAS, retenidas))
}

seleccionar_boruta <- function(entrenamiento, candidatas, semilla, minimo = 2) {
  if (length(candidatas) <= minimo) {
    return(list(variables = candidatas, estadisticos = tibble(variable = candidatas, decision = "Retenida")))
  }
  set.seed(semilla)
  boruta <- Boruta(
    x = as.data.frame(entrenamiento[candidatas]),
    y = entrenamiento$PEVI,
    maxRuns = 500,
    doTrace = 0
  )
  if (any(boruta$finalDecision == "Tentative")) boruta <- TentativeRoughFix(boruta)
  estadisticos <- attStats(boruta) |>
    rownames_to_column("variable") |>
    as_tibble() |>
    arrange(desc(meanImp))
  confirmadas <- estadisticos$variable[estadisticos$decision == "Confirmed"]
  if (length(confirmadas) < minimo) confirmadas <- head(estadisticos$variable, minimo)
  list(variables = confirmadas, estadisticos = estadisticos, boruta = boruta)
}

definir_predictores <- function(entrenamiento, escenario, semilla) {
  if (is.na(escenario$umbral)) {
    return(list(variables = ZOOMETRICAS, eliminadas = character(), estadisticos = tibble()))
  }
  colinealidad <- filtrar_colinealidad(entrenamiento, escenario$umbral, escenario$metodo)
  seleccion <- seleccionar_boruta(entrenamiento, colinealidad$retenidas, semilla)
  list(
    variables = seleccion$variables,
    retenidas = colinealidad$retenidas,
    eliminadas = colinealidad$eliminadas,
    estadisticos = seleccion$estadisticos,
    boruta = seleccion$boruta
  )
}
