# Ecuaciones alométricas de campo (peso = a · medida^b) por raza y sexo, con error de validación cruzada 10×10.
# Ejecutar desde la raíz del paper: Rscript 02_CODE/ecuaciones_campo.R  -> 03_RESULTS/ecuaciones_campo.csv
source("02_CODE/R/cargar.R")
llamas <- preparar_datos(config, correcciones_campo)$llamas

grupos <- list(
  "K'ara males" = c("K'ara", "Macho"), "K'ara females" = c("K'ara", "Hembra"),
  "Ch'acu males" = c("Ch'acu", "Macho"), "Ch'acu females" = c("Ch'acu", "Hembra")
)
formulas <- list(
  "PETO" = log(PEVI) ~ log(PETO),
  "PETO + ALCRU" = log(PEVI) ~ log(PETO) + log(ALCRU),
  "PETO + ALCRU + PROF" = log(PEVI) ~ log(PETO) + log(ALCRU) + log(PROF)
)

# Ajuste en escala log y retransformación con el estimador de smearing de Duan
ajustar <- function(formula, datos) {
  m <- lm(formula, data = datos)
  list(modelo = m, smearing = mean(exp(residuals(m))))
}
predecir <- function(ajuste, nuevos) exp(predict(ajuste$modelo, nuevos)) * ajuste$smearing

validar <- function(formula, datos, folds = 10, repeticiones = 10) {
  set.seed(config$semilla)
  errores <- map_dfr(seq_len(repeticiones), \(r) {
    pliegue <- sample(rep(seq_len(folds), length.out = nrow(datos)))
    map_dfr(seq_len(folds), \(k) {
      ajuste <- ajustar(formula, datos[pliegue != k, ])
      prueba <- datos[pliegue == k, ]
      e <- predecir(ajuste, prueba) - prueba$PEVI
      tibble(RMSE = sqrt(mean(e^2)), MAPE = 100 * mean(abs(e) / prueba$PEVI))
    })
  })
  summarise(errores, RMSE = mean(RMSE), MAPE = mean(MAPE))
}

tabla <- imap_dfr(grupos, \(g, nombre) {
  datos <- filter(llamas, VARIE == g[1], SEXO == g[2])
  imap_dfr(formulas, \(f, medidas) {
    ajuste <- ajustar(f, datos)
    coef_ <- coef(ajuste$modelo)
    cv <- validar(f, datos)
    tibble(
      grupo = nombre, medidas = medidas, n = nrow(datos),
      a = exp(coef_[[1]]) * ajuste$smearing,
      b_PETO = coef_[["log(PETO)"]],
      b_ALCRU = if ("log(ALCRU)" %in% names(coef_)) coef_[["log(ALCRU)"]] else NA_real_,
      b_PROF = if ("log(PROF)" %in% names(coef_)) coef_[["log(PROF)"]] else NA_real_,
      R2_log = summary(ajuste$modelo)$r.squared,
      RMSE_cv = cv$RMSE, MAPE_cv = cv$MAPE
    )
  })
})
write.csv(tabla, file.path(config$salida, "ecuaciones_campo.csv"), row.names = FALSE)
print(as.data.frame(mutate(tabla, across(where(is.numeric), \(x) signif(x, 4)))))
