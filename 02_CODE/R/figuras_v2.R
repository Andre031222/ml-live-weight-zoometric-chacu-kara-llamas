# Figuras 3 y 5-10 rediseñadas (21/10/2026) a partir de la validación anidada (03_RESULTS/nested_2026-10-20)
# Un solo sistema visual: Helvetica, paneles a/b, raza siempre azul/naranja, edad en escala secuencial.

CONJUNTOS <- c("K'ara", "K'ara males", "K'ara females", "Ch'acu", "Ch'acu males", "Ch'acu females", "Pooled")
MODELOS <- c("MLR", "Allometric", "ENet", "PLS", "KNN", "CART", "RF", "XGBoost", "SVM", "ANN", "AdaBoost")
FAMILIA <- setNames(rep(c("Statistical", "Machine learning"), c(4, 7)), MODELOS)
COLOR_CONJUNTO <- c(setNames(rep(COLORES_RAZA, each = 3), CONJUNTOS[1:6]), Pooled = "grey25")
COLOR_EDAD <- c("1" = "#c7e9b4", "2" = "#7fcdbb", "3" = "#41b6c4", "4" = "#2c7fb8", "5" = "#253494")
COLOR_ESTRATEGIA <- c(A = "#d9d9d9", B = "#8ab6d6", C = "#1f4e79")

tema_v2 <- function(base = 8) {
  theme_classic(base_size = base, base_family = "Helvetica") +
    theme(
      axis.line = element_line(colour = "grey30", linewidth = 0.3),
      axis.ticks = element_line(colour = "grey30", linewidth = 0.3),
      axis.text = element_text(colour = "grey15"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0, size = base),
      plot.tag = element_text(face = "bold", size = base + 3),
      legend.key.size = unit(3, "mm"),
      panel.spacing = unit(4, "mm")
    )
}

leer_anidado <- function(carpeta = "03_RESULTS/nested_2026-10-20") {
  csv <- file.path(carpeta, "csv")
  list(
    resumen = read_csv(file.path(carpeta, "cv_summary_C.csv"), show_col_types = FALSE),
    pliegues = map(set_names(c("A", "B", "C")), \(s) read_csv(file.path(csv, paste0("folds_", s, ".csv")), show_col_types = FALSE)),
    oof = read_csv(file.path(csv, "oof_C.csv"), show_col_types = FALSE),
    importancia = read_csv(file.path(carpeta, "permutation_importance.csv"), show_col_types = FALSE),
    ba = read_csv(file.path(carpeta, "bland_altman_calibration.csv"), show_col_types = FALSE)
  )
}

etiqueta_conjunto <- function(x) factor(x, levels = CONJUNTOS)

# Predicciones fuera de pliegue del mejor modelo de cada conjunto, con raza, sexo y edad
oof_mejor <- function(anidado, llamas) {
  mejores <- filter(anidado$resumen, best) |> select(dataset, model)
  anidado$oof |>
    semi_join(mejores, by = c("dataset", "model")) |>
    left_join(transmute(llamas, NRO, EDAD = factor(EDAD), VARIE, SEXO), by = "NRO") |>
    mutate(panel = factor(sprintf("%s (%s)", dataset, model),
                          levels = sprintf("%s (%s)", CONJUNTOS, mejores$model[match(CONJUNTOS, mejores$dataset)])),
           dif = pred - obs, media = (pred + obs) / 2)
}

# Fig. 3: crecimiento por raza y sexo (a) y diferencia entre razas con IC 95 % por edad (b)
figura3_v2 <- function(llamas) {
  datos <- mutate(llamas, edad = as.numeric(as.character(EDAD)),
                  sexo = factor(ifelse(SEXO == "Macho", "Males", "Females"), levels = c("Males", "Females")))
  medias <- datos |>
    group_by(sexo, VARIE, edad) |>
    summarise(media = mean(PEVI), ic = qt(0.975, n() - 1) * sd(PEVI) / sqrt(n()), .groups = "drop")
  a <- ggplot(datos, aes(edad, PEVI, colour = VARIE)) +
    geom_point(position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.4, seed = 2026),
               size = 0.7, alpha = 0.35, shape = 16) +
    geom_line(data = medias, aes(y = media), linewidth = 0.7, position = position_dodge(0.4)) +
    geom_pointrange(data = medias, aes(y = media, ymin = media - ic, ymax = media + ic),
                    size = 0.25, linewidth = 0.5, position = position_dodge(0.4)) +
    geom_text(data = filter(medias, edad == 5), aes(x = 5.3, y = media, label = VARIE), hjust = 0,
              size = 2.6, fontface = "bold", family = "Helvetica") +
    facet_wrap(~sexo) +
    scale_colour_manual(values = COLORES_RAZA, guide = "none") +
    scale_x_continuous(breaks = 1:5, limits = c(0.7, 6.1)) +
    scale_y_continuous(breaks = seq(40, 140, 20)) +
    labs(x = "Age (years)", y = "Live weight (kg)", tag = "a") +
    tema_v2() +
    theme(panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3))
  dif <- datos |>
    group_by(sexo, edad) |>
    summarise(prueba = list(t.test(PEVI[VARIE == "K'ara"], PEVI[VARIE == "Ch'acu"])), .groups = "drop") |>
    mutate(d = map_dbl(prueba, \(t) unname(diff(rev(t$estimate)))),
           lo = map_dbl(prueba, \(t) t$conf.int[1]), hi = map_dbl(prueba, \(t) t$conf.int[2]),
           p = map_dbl(prueba, "p.value"), sig = hi < 0 | lo > 0)
  b <- ggplot(dif, aes(edad, d, colour = sexo, shape = sexo)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, fill = COLORES_RAZA[["K'ara"]], alpha = 0.06) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, fill = COLORES_RAZA[["Ch'acu"]], alpha = 0.06) +
    annotate("text", x = 0.6, y = 24, label = "K'ara heavier", hjust = 0, size = 2.3, colour = COLORES_RAZA[["K'ara"]], family = "Helvetica") +
    annotate("text", x = 0.6, y = -9, label = "Ch'acu heavier", hjust = 0, size = 2.3, colour = COLORES_RAZA[["Ch'acu"]], family = "Helvetica") +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_pointrange(aes(ymin = lo, ymax = hi, fill = ifelse(sig, as.character(sexo), "ns")),
                    position = position_dodge(0.45), size = 0.35, linewidth = 0.5, stroke = 0.7) +
    scale_colour_manual(values = c(Males = "grey15", Females = "grey55"), name = NULL) +
    scale_fill_manual(values = c(Males = "grey15", Females = "grey55", ns = "white"), guide = "none") +
    scale_shape_manual(values = c(Males = 21, Females = 24), name = NULL) +
    scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +
    labs(x = "Age (years)", y = "K'ara − Ch'acu (kg, 95% CI)", tag = "b") +
    tema_v2() +
    theme(legend.position = "inside", legend.position.inside = c(0.98, 0.98), legend.justification = c(1, 1),
          legend.background = element_blank(), panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3))
  a + b + plot_layout(widths = c(2.3, 1))
}

# Fig. 5: RMSE del mejor modelo por estrategia (media e IC 95 % corregido de Nadeau-Bengio)
figura5_v2 <- function(anidado) {
  datos <- imap(anidado$pliegues, \(p, s) mutate(filter(p, rep < 3), estrategia = s)) |>
    bind_rows() |>
    group_by(estrategia, dataset, model) |>
    summarise(rmse = mean(RMSE), J = n(), v = var(RMSE), .groups = "drop") |>
    group_by(estrategia, dataset) |>
    slice_min(rmse, n = 1) |>
    ungroup() |>
    mutate(se = sqrt((1 / J + 1 / 9) * v), lo = rmse - qt(0.975, J - 1) * se, hi = rmse + qt(0.975, J - 1) * se,
           conjunto = factor(dataset, levels = rev(CONJUNTOS)))
  rango <- datos |>
    group_by(conjunto) |>
    summarise(min = min(rmse), max = max(rmse),
              cambio = 100 * (rmse[estrategia == "C"] / rmse[estrategia == "A"] - 1), .groups = "drop")
  ggplot(datos, aes(rmse, conjunto)) +
    geom_segment(data = rango, aes(x = min, xend = max, yend = conjunto), colour = "grey85", linewidth = 2.2, lineend = "round") +
    geom_point(aes(fill = estrategia), shape = 21, size = 2.8, stroke = 0.4, colour = "grey20") +
    geom_text(data = rango, aes(x = 15, label = sprintf("−%.0f%%", -cambio)), hjust = 1, size = 2.6,
              fontface = "bold", family = "Helvetica", colour = "grey15") +
    annotate("text", x = 15, y = 7.75, label = "A → C", hjust = 1, size = 2.4, colour = "grey40", family = "Helvetica") +
    scale_x_log10(breaks = c(1, 2, 3, 5, 10), limits = c(0.75, 15)) +
    scale_y_discrete(expand = expansion(add = c(0.6, 1))) +
    scale_fill_manual(values = COLOR_ESTRATEGIA, name = NULL,
                      labels = c(A = "A: |r| ≥ 0.75 + Boruta", B = "B: guided |r| ≥ 0.95 + Boruta", C = "C: all 12 measurements")) +
    labs(x = "Nested cross-validated RMSE of the best model (kg, log scale)", y = NULL) +
    tema_v2() +
    theme(legend.position = "top", legend.justification = "left", axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour = rev(COLOR_CONJUNTO[CONJUNTOS]), face = "bold"),
          panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# Fig. 6: RMSE relativo al mejor modelo (a) y rango medio de cada algoritmo en los 7 conjuntos (b)
figura6_v2 <- function(anidado) {
  datos <- anidado$resumen |>
    group_by(dataset) |>
    mutate(relativo = RMSE / min(RMSE), rango = rank(RMSE)) |>
    ungroup() |>
    mutate(conjunto = factor(dataset, levels = rev(CONJUNTOS)), modelo = factor(model, levels = MODELOS),
           familia = factor(FAMILIA[model], levels = c("Statistical", "Machine learning")))
  a <- ggplot(datos, aes(modelo, conjunto, fill = relativo)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_tile(data = filter(datos, best), fill = NA, colour = "black", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.2f", RMSE), fontface = ifelse(equivalent, "bold", "plain"),
                  colour = relativo > 1.9), size = 2.3, family = "Helvetica", show.legend = FALSE) +
    facet_grid(~familia, scales = "free_x", space = "free_x") +
    scale_fill_gradientn(colours = c("#fff7ec", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000"), trans = "log2",
                         breaks = c(1, 1.5, 2, 3), labels = c("best", "1.5×", "2×", "3×"),
                         name = "RMSE relative\nto best model") +
    scale_colour_manual(values = c("FALSE" = "grey10", "TRUE" = "white")) +
    labs(x = NULL, y = NULL, tag = "a") +
    tema_v2() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), strip.text = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 40, hjust = 1),
          axis.text.y = element_text(colour = rev(COLOR_CONJUNTO[CONJUNTOS]), face = "bold"),
          legend.title = element_text(size = 7), legend.key.height = unit(6, "mm"))
  rangos <- datos |>
    group_by(modelo, familia) |>
    summarise(medio = mean(rango), min = min(rango), max = max(rango), n_eq = sum(equivalent), .groups = "drop") |>
    mutate(modelo = reorder(modelo, -medio))
  b <- ggplot(rangos, aes(medio, modelo, colour = familia)) +
    geom_segment(aes(x = min, xend = max, yend = modelo), linewidth = 0.5, alpha = 0.5) +
    geom_point(size = 2.2) +
    geom_text(aes(x = 13.6, label = sprintf("%d/7", n_eq)), hjust = 1, size = 2.3, colour = "grey30", family = "Helvetica") +
    annotate("text", x = 13.6, y = 11.7, label = "best or\nequiv.", hjust = 1, vjust = 0, size = 2.1,
             colour = "grey40", lineheight = 0.9, family = "Helvetica") +
    scale_colour_manual(values = c(Statistical = "grey60", `Machine learning` = "#1f4e79"), guide = "none") +
    scale_x_continuous(breaks = c(1, 3, 6, 9, 11), limits = c(1, 13.6)) +
    scale_y_discrete(expand = expansion(add = c(0.6, 1.8))) +
    labs(x = "Rank (1 = lowest RMSE)", y = NULL, tag = "b") +
    tema_v2() +
    theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
          axis.text.y = element_text(colour = ifelse(FAMILIA[levels(rangos$modelo)] == "Statistical", "grey50", "#1f4e79"),
                                     face = "bold"))
  a + b + plot_layout(widths = c(2.6, 1))
}

# Fig. 7: observado frente a predicho, con el RMSE y el R2 de la validación anidada (iguales a la Tabla 4)
figura7_v2 <- function(oof, resumen) {
  metricas <- oof |>
    group_by(panel, dataset) |>
    summarise(n = n(), lo = min(obs, pred), hi = max(obs, pred), .groups = "drop") |>
    left_join(filter(resumen, best) |> select(dataset, RMSE, R2), by = "dataset") |>
    mutate(etiqueta = sprintf("RMSE = %.2f kg\nR² = %.3f\nn = %d", RMSE, R2, n))
  limites <- bind_rows(transmute(metricas, panel, x = lo, y = lo), transmute(metricas, panel, x = hi, y = hi))
  ggplot(oof, aes(pred, obs)) +
    geom_blank(data = limites, aes(x, y)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey55", linewidth = 0.35, linetype = "22") +
    geom_point(aes(fill = EDAD), shape = 21, colour = "white", stroke = 0.15, size = 1.5) +
    geom_text(data = metricas, aes(x = lo, y = hi, label = etiqueta), hjust = 0, vjust = 1, size = 2.2,
              lineheight = 0.95, colour = "grey20", family = "Helvetica") +
    facet_wrap(~panel, ncol = 4, scales = "free") +
    scale_fill_manual(values = COLOR_EDAD, name = "Age (years)") +
    guides(fill = guide_legend(override.aes = list(size = 2.6), nrow = 1, title.position = "top")) +
    labs(x = "Predicted live weight (kg)", y = "Observed live weight (kg)") +
    tema_v2() +
    theme(aspect.ratio = 1, legend.position = "inside", legend.position.inside = c(0.875, 0.22),
          strip.text = element_text(size = 7.5))
}

# Fig. 8: Bland-Altman con banda de los límites de acuerdo y valores anotados
figura8_v2 <- function(oof) {
  ba <- oof |>
    group_by(panel) |>
    summarise(sesgo = mean(dif), sd = sd(dif), x = max(media), .groups = "drop") |>
    mutate(lo = sesgo - 1.96 * sd, hi = sesgo + 1.96 * sd)
  extremo <- filter(oof, NRO == 139, dataset == "Ch'acu males")
  ggplot(oof, aes(media, dif)) +
    geom_rect(data = ba, aes(xmin = -Inf, xmax = Inf, ymin = lo, ymax = hi), inherit.aes = FALSE, fill = "grey93") +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3, linetype = "22") +
    geom_hline(data = ba, aes(yintercept = sesgo), colour = "grey15", linewidth = 0.45) +
    geom_hline(data = ba, aes(yintercept = lo), colour = "grey45", linewidth = 0.3) +
    geom_hline(data = ba, aes(yintercept = hi), colour = "grey45", linewidth = 0.3) +
    geom_point(aes(fill = EDAD), shape = 21, colour = "white", stroke = 0.15, size = 1.4) +
    geom_text(data = ba, aes(x = Inf, y = hi, label = sprintf("+%.1f", hi)), hjust = 1.1, vjust = -0.4, size = 2.1,
              colour = "grey35", family = "Helvetica") +
    geom_text(data = ba, aes(x = Inf, y = lo, label = sprintf("%.1f", lo)), hjust = 1.1, vjust = 1.4, size = 2.1,
              colour = "grey35", family = "Helvetica") +
    geom_text(data = extremo, aes(label = "#139"), hjust = -0.25, size = 2.1, colour = "grey20", family = "Helvetica") +
    facet_wrap(~panel, ncol = 4, scales = "free") +
    scale_fill_manual(values = COLOR_EDAD, name = "Age (years)") +
    scale_y_continuous(expand = expansion(mult = 0.12)) +
    guides(fill = guide_legend(override.aes = list(size = 2.6), nrow = 1, title.position = "top")) +
    labs(x = "Mean of observed and predicted live weight (kg)", y = "Predicted − observed (kg)") +
    tema_v2() +
    theme(legend.position = "inside", legend.position.inside = c(0.875, 0.22), strip.text = element_text(size = 7.5))
}

# Fig. 9: error dentro de cada edad (RMSE en kg y como % del peso medio de la clase)
figura9_v2 <- function(oof) {
  por_edad <- oof |>
    group_by(panel, dataset, EDAD) |>
    summarise(rmse = sqrt(mean(dif^2)), rel = 100 * rmse / mean(obs), .groups = "drop") |>
    mutate(panel = factor(panel, levels = rev(levels(oof$panel))))
  ggplot(por_edad, aes(EDAD, panel, fill = rel)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.2f", rmse), colour = rel > 7), size = 2.5, fontface = "bold",
              family = "Helvetica", nudge_y = 0.12) +
    geom_text(aes(label = sprintf("%.1f%%", rel), colour = rel > 7), size = 1.9, family = "Helvetica", nudge_y = -0.2) +
    scale_fill_gradientn(colours = c("#f7fcf5", "#c7e9c0", "#fdd49e", "#fc8d59", "#b30000"), limits = c(0, 10),
                         name = "RMSE as % of\nmean weight") +
    scale_colour_manual(values = c("FALSE" = "grey10", "TRUE" = "white"), guide = "none") +
    scale_x_discrete(position = "top") +
    labs(x = "Age class (years)", y = NULL) +
    tema_v2() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(colour = rev(COLOR_CONJUNTO[CONJUNTOS]), face = "bold"),
          legend.title = element_text(size = 7), legend.key.height = unit(6, "mm"))
}

# Fig. 10: importancia por permutación de las 12 medidas en los 7 conjuntos (% de la más importante y orden)
figura10_v2 <- function(anidado) {
  datos <- anidado$importancia |>
    group_by(dataset) |>
    mutate(rel = pmin(100, 100 * pmax(mean, 0) / max(mean)), orden = rank(-mean)) |>
    ungroup()
  orden_var <- datos |> group_by(variable) |> summarise(r = mean(orden)) |> arrange(r) |> pull(variable)
  mejores <- filter(anidado$resumen, best)
  datos <- mutate(datos, variable = factor(variable, levels = rev(orden_var)),
                  conjunto = factor(sprintf("%s\n(%s)", sub(" ", "\n", dataset), mejores$model[match(dataset, mejores$dataset)]),
                                    levels = sprintf("%s\n(%s)", sub(" ", "\n", CONJUNTOS), mejores$model[match(CONJUNTOS, mejores$dataset)])))
  ggplot(datos, aes(conjunto, variable, fill = rel)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = ifelse(orden <= 3, orden, ""), colour = rel > 55), size = 2.5, fontface = "bold", family = "Helvetica") +
    scale_fill_gradientn(colours = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"), limits = c(0, 100),
                         name = "Importance\n(% of top variable)") +
    scale_colour_manual(values = c("FALSE" = "grey10", "TRUE" = "white"), guide = "none") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    tema_v2() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_text(size = 6.8, lineheight = 0.95),
          legend.title = element_text(size = 7), legend.key.height = unit(6, "mm"))
}

exportar_v2 <- function(grafico, nombre, ancho, alto, carpeta = "03_RESULTS/figures_v2") {
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(carpeta, paste0(nombre, ".pdf")), grafico, width = ancho, height = alto, units = "cm", device = cairo_pdf)
  ggsave(file.path(carpeta, paste0(nombre, ".png")), grafico, width = ancho, height = alto, units = "cm", dpi = 600)
}
