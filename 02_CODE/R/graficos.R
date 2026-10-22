COLORES_RAZA <- c("K'ara" = "#2a78d6", "Ch'acu" = "#eb6834")

tema_articulo <- function() {
  theme_classic(base_size = 10) +
    theme(
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      axis.line = element_line(colour = "grey40", linewidth = 0.3),
      axis.ticks = element_line(colour = "grey40", linewidth = 0.3),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "top",
      legend.title = element_blank()
    )
}

guardar <- function(grafico, archivo, ancho = 16, alto = 10, formatos = "png", dpi = 300) {
  base <- sub("\\.(png|pdf)$", "", archivo)
  for (formato in formatos) {
    dispositivo <- if (formato == "pdf") cairo_pdf else "png"
    ggsave(paste0(base, ".", formato), grafico, width = ancho, height = alto, units = "cm", dpi = dpi,
           device = dispositivo)
  }
}

graficar_correlaciones <- function(entrenamiento, retenidas, archivo) {
  completa <- cor(entrenamiento[c(ZOOMETRICAS, "PEVI")], method = "spearman")
  filtrada <- cor(entrenamiento[c(retenidas, "PEVI")], method = "spearman")
  png(archivo, width = 22, height = 11, units = "cm", res = 300)
  par(mfrow = c(1, 2))
  paleta <- colorRampPalette(c("#eb6834", "#f2f2f2", "#2a78d6"))(200)
  corrplot(completa, method = "color", type = "upper", col = paleta, addCoef.col = "grey15",
           number.cex = 0.45, tl.cex = 0.6, tl.col = "black", title = "a) Antes del filtrado", mar = c(0, 0, 2, 0))
  corrplot(filtrada, method = "color", type = "upper", col = paleta, addCoef.col = "grey15",
           number.cex = 0.7, tl.cex = 0.8, tl.col = "black", title = "b) Después del filtrado", mar = c(0, 0, 2, 0))
  dev.off()
}

graficar_boruta <- function(seleccion, archivo) {
  if (is.null(seleccion$boruta)) return(invisible(NULL))
  png(archivo, width = 14, height = 10, units = "cm", res = 300)
  par(mar = c(6, 4, 1, 1))
  plot(seleccion$boruta, las = 2, cex.axis = 0.7, xlab = "", ylab = "Importancia (Z)",
       colCode = c("#1baf7a", "#eda100", "#e34948", "#9c9c9c"))
  dev.off()
}

graficar_importancia <- function(modelos, archivo) {
  importancia <- map(c("RF", "XGBoost"), \(nombre) {
    varImp(modelos[[nombre]], scale = TRUE)$importance |>
      rownames_to_column("variable") |>
      transmute(modelo = nombre, variable, importancia = Overall)
  }) |>
    bind_rows()
  grafico <- ggplot(importancia, aes(importancia, ordenar_dentro(variable, importancia, modelo))) +
    geom_col(fill = "#2a78d6", width = 0.6) +
    facet_wrap(~modelo, scales = "free_y") +
    scale_y_discrete(labels = \(x) sub("___.*$", "", x)) +
    labs(x = "Importancia relativa (%)", y = NULL) +
    tema_articulo()
  guardar(grafico, archivo, 16, 7)
}

ordenar_dentro <- function(variable, valor, grupo) {
  clave <- paste(variable, grupo, sep = "___")
  reorder(clave, valor)
}

graficar_observado_predicho <- function(predicciones, archivo) {
  prueba <- filter(predicciones, conjunto == "Prueba")
  limites <- range(c(prueba$observado, prueba$predicho))
  grafico <- ggplot(prueba, aes(observado, predicho)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.4) +
    geom_point(colour = "#2a78d6", size = 1.6, alpha = 0.85) +
    facet_wrap(~modelo, nrow = 2) +
    coord_equal(xlim = limites, ylim = limites) +
    labs(x = "Peso vivo observado (kg)", y = "Peso vivo predicho (kg)") +
    tema_articulo()
  guardar(grafico, archivo, 18, 10)
}

graficar_correlacion_predicciones <- function(predicciones, archivo) {
  matriz <- predicciones |>
    filter(conjunto == "Prueba") |>
    group_by(modelo) |>
    mutate(id = row_number()) |>
    ungroup() |>
    select(id, observado, modelo, predicho) |>
    pivot_wider(names_from = modelo, values_from = predicho) |>
    rename(Observado = observado) |>
    select(-id)
  png(archivo, width = 13, height = 12, units = "cm", res = 300)
  corrplot(cor(matriz, method = "spearman"), method = "color", type = "upper",
           col = colorRampPalette(c("#eb6834", "#f2f2f2", "#2a78d6"))(200),
           addCoef.col = "grey15", number.cex = 0.6, tl.cex = 0.75, tl.col = "black", mar = c(0, 0, 1, 0))
  dev.off()
}
