detectar_atipicos <- function(datos, umbral = 5) {
  variables <- c(ZOOMETRICAS, "PEVI")
  map(variables, \(variable) {
    residuo <- residuals(lm(reformulate("VARIE * SEXO * EDAD", response = variable), data = datos))
    datos |>
      transmute(
        NRO, VARIE, SEXO, EDAD,
        variable = variable,
        valor = .data[[variable]],
        esperado = valor - residuo,
        z_robusto = (residuo - median(residuo)) / mad(residuo)
      )
  }) |>
    bind_rows() |>
    filter(abs(z_robusto) > umbral) |>
    arrange(desc(abs(z_robusto)))
}

depurar_datos <- function(datos, atipicos, correcciones, umbral_error = 8) {
  errores <- atipicos |>
    filter(abs(z_robusto) > umbral_error) |>
    select(NRO, variable, valor) |>
    left_join(correcciones, by = c("NRO", "variable")) |>
    mutate(valor_usado = NA_real_)
  depurados <- datos
  for (i in seq_len(nrow(errores))) {
    fila <- depurados$NRO == errores$NRO[i]
    variable <- errores$variable[i]
    reemplazo <- errores$valor_corregido[i]
    if (is.na(reemplazo)) {
      celda <- depurados$VARIE == depurados$VARIE[fila] &
        depurados$SEXO == depurados$SEXO[fila] &
        depurados$EDAD == depurados$EDAD[fila] & !fila
      reemplazo <- median(depurados[[variable]][celda])
    }
    depurados[[variable]][fila] <- reemplazo
    errores$valor_usado[i] <- reemplazo
  }
  list(datos = depurados, cambios = mutate(errores, origen = ifelse(is.na(valor_corregido), "Mediana de la celda", "Registro de campo")))
}
