describir_estrato <- function(datos) {
  datos |>
    select(all_of(c("PEVI", ZOOMETRICAS))) |>
    pivot_longer(everything(), names_to = "variable") |>
    mutate(variable = factor(variable, levels = c("PEVI", ZOOMETRICAS))) |>
    group_by(variable) |>
    summarise(
      media = mean(value),
      mediana = median(value),
      de = sd(value),
      minimo = min(value),
      maximo = max(value),
      cv = 100 * sd(value) / mean(value)
    )
}

describir_por_grupo <- function(datos) {
  datos |>
    pivot_longer(all_of(c("PEVI", ZOOMETRICAS)), names_to = "variable") |>
    mutate(variable = factor(variable, levels = c("PEVI", ZOOMETRICAS))) |>
    group_by(variable, VARIE, SEXO) |>
    summarise(media = mean(value), de = sd(value), .groups = "drop") |>
    mutate(resumen = sprintf("%.2f ± %.2f", media, de)) |>
    select(-media, -de) |>
    pivot_wider(names_from = c(VARIE, SEXO), values_from = resumen, names_sep = " ")
}

anova_factorial <- function(datos) {
  map(c("PEVI", ZOOMETRICAS), \(variable) {
    ajuste <- aov(reformulate("VARIE * SEXO * EDAD", response = variable), data = datos)
    tabla <- summary(ajuste)[[1]]
    tibble(
      variable = variable,
      efecto = trimws(rownames(tabla)),
      F = tabla$`F value`,
      p_valor = tabla$`Pr(>F)`
    ) |>
      filter(efecto != "Residuals")
  }) |>
    bind_rows() |>
    mutate(efecto = gsub("VARIE", "Raza", gsub("SEXO", "Sexo", gsub("EDAD", "Edad", efecto))))
}

probar_homogeneidad <- function(datos, predictores = c("PETO", "ALCRU")) {
  formula_comun <- reformulate(predictores, response = "PEVI")
  formula_raza <- reformulate(sprintf("(%s) * VARIE", paste(predictores, collapse = " + ")), response = "PEVI")
  formula_completa <- reformulate(sprintf("(%s) * VARIE * SEXO", paste(predictores, collapse = " + ")), response = "PEVI")
  comun <- lm(formula_comun, data = datos)
  raza <- lm(formula_raza, data = datos)
  completa <- lm(formula_completa, data = datos)
  comparacion <- anova(comun, raza, completa)
  tibble(
    modelo = c("Comun", "Por raza", "Por raza y sexo"),
    gl_residual = comparacion$Res.Df,
    sce = comparacion$RSS,
    F = comparacion$F,
    p_valor = comparacion$`Pr(>F)`,
    r2_ajustado = c(summary(comun)$adj.r.squared, summary(raza)$adj.r.squared, summary(completa)$adj.r.squared)
  )
}
