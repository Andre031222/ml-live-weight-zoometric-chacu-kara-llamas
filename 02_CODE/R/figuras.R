FAMILIAS_MODELO <- c(
  MLR = "Estadísticos", Alometrico = "Estadísticos", ENet = "Estadísticos", PLS = "Estadísticos",
  KNN = "Aprendizaje automático", CART = "Aprendizaje automático", RF = "Aprendizaje automático",
  XGBoost = "Aprendizaje automático", SVM = "Aprendizaje automático", ANN = "Aprendizaje automático",
  AdaBoost = "Aprendizaje automático"
)

etiquetar_modelo <- function(modelo) {
  niveles <- traducir(recode(names(FAMILIAS_MODELO), Alometrico = "Alométrico"))
  factor(traducir(recode(modelo, Alometrico = "Alométrico")), levels = niveles)
}

figura_distribucion <- function(datos) {
  datos <- mutate(
    datos,
    edad = as.numeric(as.character(EDAD)),
    SEXO = traducir(factor(recode(SEXO, Macho = "Machos", Hembra = "Hembras"), levels = c("Machos", "Hembras")))
  )
  medias <- datos |>
    group_by(SEXO, VARIE, edad) |>
    summarise(
      media = mean(PEVI),
      ic = qt(0.975, n() - 1) * sd(PEVI) / sqrt(n()),
      .groups = "drop"
    )
  finales <- filter(medias, edad == 5)
  diferencias <- finales |>
    select(SEXO, VARIE, media) |>
    pivot_wider(names_from = VARIE, values_from = media) |>
    mutate(
      etiqueta = paste0("+", decimal(`K'ara` - `Ch'acu`, 1), " kg"),
      y = (`K'ara` + `Ch'acu`) / 2,
      ymin = `Ch'acu`,
      ymax = `K'ara`
    )
  ggplot(datos, aes(edad, PEVI, colour = VARIE, fill = VARIE)) +
    geom_point(position = position_jitterdodge(jitter.width = 0.18, dodge.width = 0.35, seed = 2026),
               size = 0.9, alpha = 0.35, shape = 16) +
    geom_ribbon(data = medias, aes(y = media, ymin = media - ic, ymax = media + ic), alpha = 0.18, colour = NA) +
    geom_line(data = medias, aes(y = media), linewidth = 0.9) +
    geom_point(data = medias, aes(y = media), size = 2, shape = 21, colour = "white", stroke = 0.6) +
    geom_text(data = finales, aes(x = 5.18, y = media + ifelse(VARIE == "K'ara", 2.5, -2.5), label = VARIE),
              hjust = 0, size = 2.8, fontface = "bold",
              show.legend = FALSE) +
    geom_segment(data = diferencias, aes(x = 5.75, xend = 5.75, y = ymin, yend = ymax), inherit.aes = FALSE,
                 colour = "grey35", linewidth = 0.35) +
    geom_text(data = diferencias, aes(x = 5.82, y = y, label = etiqueta), inherit.aes = FALSE, hjust = 0,
              size = 2.5, colour = "grey20") +
    facet_wrap(~SEXO) +
    scale_colour_manual(values = COLORES_RAZA, guide = "none") +
    scale_fill_manual(values = COLORES_RAZA, guide = "none") +
    scale_x_continuous(breaks = 1:5, limits = c(0.7, 6.6)) +
    scale_y_continuous(breaks = seq(40, 140, 20)) +
    labs(x = traducir("Edad (años)"), y = traducir("Peso vivo (kg)")) +
    tema_articulo() +
    theme(panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3))
}

figura_correlaciones <- function(datos) {
  variables <- c("PEVI", ZOOMETRICAS)
  matrices <- map(levels(datos$VARIE), \(raza) {
    correlacion <- cor(filter(datos, VARIE == raza)[variables], method = "spearman")
    as.data.frame(as.table(correlacion)) |>
      setNames(c("fila", "columna", "r")) |>
      mutate(
        raza = raza,
        fila = factor(fila, levels = rev(variables)),
        columna = factor(columna, levels = variables)
      ) |>
      filter(as.integer(columna) < match(fila, variables))
  }) |>
    bind_rows() |>
    mutate(raza = factor(raza, levels = levels(datos$VARIE)))
  ggplot(matrices, aes(columna, fila, fill = r)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sub("^0", "", decimal(r, 2)), colour = r > 0.85), size = 1.9, show.legend = FALSE) +
    facet_wrap(~raza) +
    scale_fill_gradient(low = "#f2f2f2", high = "#104281", limits = c(0.5, 1), oob = scales::squish,
                        name = traducir("r de Spearman")) +
    scale_colour_manual(values = c("FALSE" = "grey15", "TRUE" = "white")) +
    labs(x = NULL, y = NULL) +
    tema_articulo() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      axis.line = element_blank(), axis.ticks = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "right", legend.title = element_text(size = 8)
    )
}

figura_escenarios <- function(comparacion, seleccion, orden_estratos) {
  predictores <- select(seleccion, estrato, escenario, numero_predictores)
  mejores <- comparacion |>
    group_by(estrato, escenario) |>
    slice_min(RMSE, n = 1, with_ties = FALSE) |>
    ungroup() |>
    left_join(predictores, by = c("estrato", "escenario")) |>
    mutate(
      estrato = factor(estrato, levels = rev(orden_estratos)),
      escenario = factor(escenario, levels = rev(traducir(ESCENARIOS$etiqueta))),
      etiqueta = sprintf("%s kg · %d var.", decimal(RMSE, 2), numero_predictores)
    )
  reduccion <- mejores |>
    group_by(estrato) |>
    summarise(
      porcentaje = 100 * (1 - RMSE[escenario == traducir(ESCENARIOS$etiqueta[3])] / RMSE[escenario == traducir(ESCENARIOS$etiqueta[1])]),
      maximo = max(RMSE)
    )
  colores <- setNames(c("#b8b8b8", "#eb6834", "#2a78d6"), traducir(ESCENARIOS$etiqueta))
  ggplot(mejores, aes(RMSE, estrato, fill = escenario)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    geom_text(aes(label = etiqueta), position = position_dodge(width = 0.8), hjust = -0.06, size = 2.2,
              colour = "grey20") +
    geom_label(data = reduccion, aes(x = 16.2, y = estrato, label = paste0("−", round(porcentaje), if (idioma_ingles()) "%" else " %")),
               inherit.aes = FALSE, size = 2.5, fontface = "bold", colour = "#1c5cab", fill = "#eef4fb",
               linewidth = 0, label.padding = unit(0.15, "lines")) +
    annotate("text", x = 16.2, y = length(orden_estratos) + 0.62, label = traducir("Reducción\nde A a C"),
             size = 2.4, lineheight = 0.9, colour = "#1c5cab", fontface = "bold") +
    scale_fill_manual(values = colores, breaks = traducir(ESCENARIOS$etiqueta), name = NULL) +
    scale_x_continuous(limits = c(0, 17.2), breaks = seq(0, 12, 2), expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = "off") +
    labs(x = traducir("RMSE del mejor modelo en validación cruzada (kg)"), y = NULL) +
    guides(fill = guide_legend(nrow = 1)) +
    tema_articulo() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

figura_mapa_rmse <- function(comparacion, orden_estratos) {
  datos <- comparacion |>
    mutate(
      estrato = factor(estrato, levels = rev(orden_estratos)),
      familia = factor(traducir(FAMILIAS_MODELO[modelo]), levels = unique(traducir(FAMILIAS_MODELO))),
      modelo = etiquetar_modelo(modelo)
    )
  ggplot(datos, aes(modelo, estrato, fill = RMSE)) +
    geom_tile(colour = "white", linewidth = 0.9) +
    geom_text(aes(label = decimal(RMSE, 2), fontface = ifelse(equivalente, "bold", "plain"),
                  colour = RMSE > 4.5), size = 2.5, show.legend = FALSE) +
    facet_grid(~familia, scales = "free_x", space = "free_x") +
    scale_fill_gradient(low = "#cde2fb", high = "#104281", name = traducir("RMSE validación cruzada (kg)")) +
    scale_colour_manual(values = c("FALSE" = "grey10", "TRUE" = "white")) +
    labs(x = NULL, y = NULL) +
    tema_articulo() +
    theme(
      legend.title = element_text(size = 8), axis.line = element_blank(), axis.ticks = element_blank(),
      panel.grid.major.y = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1)
    )
}

figura_importancia <- function(permutacion, orden_estratos) {
  datos <- permutacion |>
    group_by(estrato) |>
    slice_max(incremento_rmse, n = 6, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      panel = factor(sprintf("%s\n(%s)", estrato, modelo),
                     levels = unique(sprintf("%s\n(%s)", estrato, modelo)[order(match(estrato, orden_estratos))])),
      variable = ordenar_dentro(variable, incremento_rmse, panel)
    )
  ggplot(datos, aes(incremento_rmse, variable)) +
    geom_col(fill = "#2a78d6", width = 0.65) +
    geom_errorbar(aes(xmin = pmax(incremento_rmse - de, 0), xmax = incremento_rmse + de),
                  width = 0.25, colour = "grey35", linewidth = 0.3, orientation = "y") +
    facet_wrap(~panel, scales = "free", ncol = 4) +
    scale_y_discrete(labels = \(x) sub("___.*$", "", x)) +
    labs(x = traducir("Aumento del RMSE al permutar la variable (kg)"), y = NULL) +
    tema_articulo() +
    theme(strip.text = element_text(size = 7.5), panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

figura_observado_predicho <- function(predicciones, orden_estratos) {
  prueba <- predicciones |>
    filter(conjunto == "Prueba") |>
    mutate(panel = sprintf("%s\n(%s)", estrato, modelo))
  orden_paneles <- unique(prueba$panel[order(match(prueba$estrato, orden_estratos))])
  prueba <- mutate(prueba, panel = factor(panel, levels = orden_paneles))
  anotaciones <- prueba |>
    group_by(panel) |>
    summarise(
      etiqueta = sprintf("R² = %s\nRMSE = %s kg", decimal(cor(observado, predicho)^2, 3), decimal(sqrt(mean((observado - predicho)^2)), 2))
    )
  limites <- range(c(prueba$observado, prueba$predicho))
  ggplot(prueba, aes(observado, predicho)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.4) +
    geom_point(colour = "#2a78d6", size = 1.5, alpha = 0.85) +
    geom_text(data = anotaciones, aes(x = limites[1], y = limites[2], label = etiqueta),
              hjust = 0, vjust = 1, size = 2.3, colour = "grey20", lineheight = 0.95) +
    facet_wrap(~panel, ncol = 4) +
    coord_equal(xlim = limites, ylim = limites) +
    labs(x = traducir("Peso vivo observado (kg)"), y = traducir("Peso vivo predicho (kg)")) +
    tema_articulo() +
    theme(strip.text = element_text(size = 7.5))
}

NOMBRES_FIGURAS <- tribble(
  ~numero, ~es,                                      ~en,
  1,       "figura_1_area_estudio",                  "Figure_1",
  2,       "figura_2_zoometrica_llama_fotografica",  "Figure_2",
  3,       "figura_3_peso_vivo",                     "Figure_3",
  4,       "figura_4_correlaciones",                 "Figure_4",
  5,       "figura_5_escenarios",                    "Figure_5",
  6,       "figura_6_rmse_modelos",                  "Figure_6",
  7,       "figura_7_importancia",                   "Figure_8",
  8,       "figura_8_observado_predicho",            "Figure_7"
)

traducir_resultados <- function(resultados) {
  map(resultados, \(tabla) {
    if ("estrato" %in% names(tabla)) tabla$estrato <- traducir(tabla$estrato)
    if ("escenario" %in% names(tabla)) tabla$escenario <- traducir(tabla$escenario)
    tabla
  })
}

exportar_figuras <- function(llamas, resultados, config, idioma = "es", carpeta = "figuras",
                             formatos = "png", dpi = 300, mapa_contexto = TRUE) {
  opciones <- options(llamas.idioma = idioma, OutDec = if (idioma == "en") "." else ",")
  on.exit(options(opciones))
  destino <- file.path(config$salida, carpeta)
  dir.create(destino, showWarnings = FALSE)
  nombres <- NOMBRES_FIGURAS[[idioma]]
  exportar <- \(numero, grafico, ancho, alto) {
    guardar(grafico, file.path(destino, nombres[numero]), ancho, alto, formatos, dpi)
  }
  orden <- traducir(ESTRATOS$etiqueta)
  resultados <- traducir_resultados(resultados)
  principal <- filter(resultados$comparacion,
                      escenario == traducir(ESCENARIOS$etiqueta[ESCENARIOS$id == config$escenario_principal]))

  exportar(1, figura_area_estudio(config, mapa_contexto), if (mapa_contexto) 18 else 14, 12)
  exportar(2, graficar_esquema_zoometrico(), 17, 11)
  exportar(3, figura_distribucion(llamas), 18, 9)
  exportar(4, figura_correlaciones(llamas), 18, 9)
  exportar(5, figura_escenarios(resultados$comparacion, resultados$seleccion, orden), 18, 13)
  exportar(6, figura_mapa_rmse(principal, orden), 18, 9)
  exportar(7, figura_importancia(resultados$permutacion, orden), 18, 11)
  exportar(8, figura_observado_predicho(resultados$predicciones_mejor, orden), 18, 10.5)
}
