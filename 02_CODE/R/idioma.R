TRADUCCIONES_EN <- c(
  "Machos" = "Males",
  "Hembras" = "Females",
  "K'ara machos" = "K'ara males",
  "K'ara hembras" = "K'ara females",
  "Ch'acu machos" = "Ch'acu males",
  "Ch'acu hembras" = "Ch'acu females",
  "General" = "Pooled",
  "A. Filtro 0,75 + Boruta (referencia)" = "A. Filter 0.75 + Boruta (reference)",
  "B. Filtro guiado 0,95 + Boruta" = "B. Response-guided filter 0.95 + Boruta",
  "C. Doce medidas" = "C. All twelve measurements",
  "Estadísticos" = "Statistical models",
  "Aprendizaje automático" = "Machine learning",
  "Alométrico" = "Allometric",
  "Edad (años)" = "Age (years)",
  "Peso vivo (kg)" = "Live weight (kg)",
  "r de Spearman" = "Spearman's r",
  "Reducción\nde A a C" = "Reduction\nfrom A to C",
  "RMSE del mejor modelo en validación cruzada (kg)" = "Cross-validated RMSE of the best model (kg)",
  "RMSE validación cruzada (kg)" = "Cross-validated RMSE (kg)",
  "Aumento del RMSE al permutar la variable (kg)" = "Increase in RMSE after permuting the variable (kg)",
  "Peso vivo observado (kg)" = "Observed live weight (kg)",
  "Peso vivo predicho (kg)" = "Predicted live weight (kg)",
  "Alzadas" = "Heights",
  "Longitudes" = "Lengths",
  "Diámetros" = "Diameters",
  "Perímetros" = "Circumferences",
  "Vista dorsal" = "Dorsal view",
  "Río" = "River",
  "Carretera nacional" = "National road",
  "Ferrocarril Cusco–Puno" = "Cusco–Puno railway",
  "Límite de Melgar" = "Melgar province boundary",
  "Altitud (m)" = "Elevation (m)"
)

idioma_ingles <- function() {
  identical(getOption("llamas.idioma"), "en")
}

traducir <- function(x) {
  if (!idioma_ingles()) return(x)
  if (is.factor(x)) return(factor(traducir(as.character(x)), levels = unique(traducir(levels(x)))))
  traducido <- unname(TRADUCCIONES_EN[as.character(x)])
  ifelse(is.na(traducido), x, traducido)
}

decimal <- function(x, digitos) {
  formatC(x, format = "f", digits = digitos, decimal.mark = if (idioma_ingles()) "." else ",")
}
