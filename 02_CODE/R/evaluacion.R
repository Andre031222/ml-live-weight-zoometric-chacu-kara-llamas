calcular_metricas <- function(observado, predicho) {
  tibble(
    MAE = mean(abs(observado - predicho)),
    RMSE = sqrt(mean((observado - predicho)^2)),
    MAPE = 100 * mean(abs((observado - predicho) / observado)),
    R2 = cor(observado, predicho)^2
  )
}

predecir <- function(modelo, datos) {
  variables <- setdiff(names(modelo$trainingData), ".outcome")
  predict(modelo, as.data.frame(datos[variables]))
}

predecir_conjuntos <- function(modelos, particion) {
  imap(modelos, \(modelo, nombre) {
    bind_rows(
      tibble(conjunto = "Entrenamiento", observado = particion$entrenamiento$PEVI,
             predicho = predecir(modelo, particion$entrenamiento)),
      tibble(conjunto = "Prueba", observado = particion$prueba$PEVI,
             predicho = predecir(modelo, particion$prueba))
    ) |>
      mutate(modelo = nombre, .before = 1)
  }) |>
    bind_rows() |>
    mutate(modelo = factor(modelo, levels = names(modelos)))
}

resumir_metricas <- function(predicciones) {
  predicciones |>
    group_by(modelo, conjunto) |>
    reframe(calcular_metricas(observado, predicho))
}

resumir_validacion <- function(modelos) {
  imap(modelos, \(modelo, nombre) {
    modelo$resample |>
      select(RMSE, R2 = Rsquared, MAE) |>
      pivot_longer(everything(), names_to = "criterio") |>
      group_by(criterio) |>
      summarise(
        media = mean(value, na.rm = TRUE),
        de = sd(value, na.rm = TRUE),
        n = sum(!is.na(value))
      ) |>
      mutate(
        ic_inferior = media - qt(0.975, n - 1) * de / sqrt(n),
        ic_superior = media + qt(0.975, n - 1) * de / sqrt(n),
        modelo = nombre,
        .before = 1
      ) |>
      select(-n)
  }) |>
    bind_rows()
}

extraer_hiperparametros <- function(modelos) {
  imap(modelos, \(modelo, nombre) {
    tune <- modelo$bestTune
    tibble(
      modelo = nombre,
      hiperparametros = paste(
        names(tune),
        map_chr(tune, \(valor) if (is.numeric(valor)) format(signif(valor, 3)) else as.character(valor)),
        sep = " = ", collapse = "; "
      )
    )
  }) |>
    bind_rows()
}

comparar_medias <- function(predicciones) {
  prueba <- predicciones |>
    filter(conjunto == "Prueba") |>
    group_by(modelo) |>
    mutate(id = row_number()) |>
    ungroup()
  largo <- bind_rows(
    prueba |> distinct(id, observado) |> transmute(id, fuente = "Observado", valor = observado),
    prueba |> transmute(id, fuente = as.character(modelo), valor = predicho)
  ) |>
    mutate(fuente = factor(fuente, levels = c("Observado", levels(predicciones$modelo))))
  ajuste <- aov(valor ~ fuente, data = largo)
  resumen <- summary(ajuste)[[1]]
  tibble(F = resumen$`F value`[1], p_valor = resumen$`Pr(>F)`[1])
}

extraer_remuestreos <- function(modelos) {
  imap(modelos, \(modelo, nombre) {
    modelo$resample |>
      transmute(modelo = nombre, remuestreo = Resample, RMSE)
  }) |>
    bind_rows()
}

comparar_con_mejor <- function(modelos, config) {
  remuestreos <- extraer_remuestreos(modelos)
  medias <- remuestreos |>
    group_by(modelo) |>
    summarise(RMSE = mean(RMSE))
  mejor <- medias$modelo[which.min(medias$RMSE)]
  referencia <- filter(remuestreos, modelo == mejor) |> select(remuestreo, RMSE_mejor = RMSE)
  correccion <- 1 / (config$folds - 1)
  remuestreos |>
    inner_join(referencia, by = "remuestreo") |>
    group_by(modelo) |>
    summarise(
      RMSE = mean(RMSE),
      diferencia = mean(RMSE - RMSE_mejor),
      varianza = var(RMSE - RMSE_mejor),
      n = n()
    ) |>
    mutate(
      t = ifelse(varianza > 0, diferencia / sqrt((1 / n + correccion) * varianza), NA_real_),
      p_valor = ifelse(is.na(t), NA_real_, pt(t, df = n - 1, lower.tail = FALSE)),
      mejor = modelo == mejor,
      equivalente = mejor | (!is.na(p_valor) & p_valor >= 0.05)
    ) |>
    select(modelo, RMSE, diferencia, t, p_valor, mejor, equivalente) |>
    arrange(RMSE)
}

probar_friedman <- function(modelos) {
  matriz <- extraer_remuestreos(modelos) |>
    pivot_wider(names_from = modelo, values_from = RMSE) |>
    select(-remuestreo) |>
    as.matrix()
  prueba <- friedman.test(matriz)
  tibble(chi_cuadrado = unname(prueba$statistic), gl = unname(prueba$parameter), p_valor = prueba$p.value)
}

importancia_permutacion <- function(modelo, datos, repeticiones = 100, semilla = 2026) {
  variables <- setdiff(names(modelo$trainingData), ".outcome")
  base <- sqrt(mean((datos$PEVI - predecir(modelo, datos))^2))
  set.seed(semilla)
  map(variables, \(variable) {
    incrementos <- replicate(repeticiones, {
      permutados <- datos
      permutados[[variable]] <- sample(permutados[[variable]])
      sqrt(mean((datos$PEVI - predecir(modelo, permutados))^2)) - base
    })
    tibble(variable = variable, incremento_rmse = mean(incrementos), de = sd(incrementos))
  }) |>
    bind_rows() |>
    arrange(desc(incremento_rmse))
}
