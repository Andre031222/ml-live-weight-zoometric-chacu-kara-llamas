# Figuras v3 (21/10/2026): misma información que v2, con ggdist (distribuciones), ggside (marginales),
# ggtext (texto con color), geomtextpath (etiquetas sobre las curvas) y scico (paletas perceptualmente uniformes).
# Reutiliza CONJUNTOS, MODELOS, FAMILIA, leer_anidado() y oof_mejor() de figuras_v2.R.

for (paquete in c("ggdist", "ggtext", "ggside", "scico", "geomtextpath", "distributional")) {
  suppressPackageStartupMessages(library(paquete, character.only = TRUE))
}

AZUL <- COLORES_RAZA[["K'ara"]]
NARANJA <- COLORES_RAZA[["Ch'acu"]]
COLOR_EDAD3 <- setNames(scico(5, palette = "batlow", begin = 0.1, end = 0.85, direction = -1), 1:5)
COLOR_FAMILIA <- c(Statistical = "#9aa5b1", `Machine learning` = "#1f4e79")

# Etiqueta de conjunto en HTML, coloreada por raza
html_conjunto <- function(x) {
  color <- ifelse(grepl("^K'ara", x), AZUL, ifelse(grepl("^Ch'acu", x), NARANJA, "#3a3a3a"))
  sprintf("<span style='color:%s'>**%s**</span>", color, x)
}

# Título de panel en dos líneas: conjunto coloreado y, debajo, el modelo en gris
html_panel <- function(panel) {
  conjunto <- sub(" \\(.*$", "", panel)
  modelo <- sub("^.*\\((.*)\\)$", "\\1", panel)
  sprintf("%s<br><span style='color:grey45;font-size:7pt'>%s</span>", html_conjunto(conjunto), modelo)
}

tema_v3 <- function(base = 8) {
  tema_v2(base) +
    theme(
      plot.title = element_markdown(size = base + 1.5, face = "plain", margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = base, colour = "grey30", lineheight = 1.2),
      plot.title.position = "plot",
      axis.text.y = element_markdown(),
      strip.text = element_markdown(hjust = 0, size = base),
      panel.grid.major.x = element_line(colour = "grey94", linewidth = 0.25),
      panel.grid.major.y = element_line(colour = "grey94", linewidth = 0.25)
    )
}

# Fig. 3: (a) distribución por edad (media-violín por raza) con curvas etiquetadas; (b) distribución de confianza de la diferencia
figura3_v3 <- function(llamas) {
  datos <- mutate(llamas, edad = as.numeric(as.character(EDAD)),
                  sexo = factor(ifelse(SEXO == "Macho", "Males", "Females"), levels = c("Males", "Females")),
                  lado = ifelse(VARIE == "K'ara", "left", "right"))
  medias <- datos |> group_by(sexo, VARIE, edad) |> summarise(media = mean(PEVI), .groups = "drop")
  a <- ggplot(datos, aes(edad, PEVI, colour = VARIE, fill = VARIE)) +
    stat_halfeye(data = filter(datos, VARIE == "K'ara"), aes(group = interaction(edad, VARIE)), side = "left", orientation = "vertical", normalize = "groups",
                 adjust = 1.2, width = 0.42, justification = 1.08, alpha = 0.35, .width = 0, point_colour = NA,
                 interval_colour = NA, slab_colour = NA) +
    stat_halfeye(data = filter(datos, VARIE == "Ch'acu"), aes(group = interaction(edad, VARIE)), side = "right", orientation = "vertical", normalize = "groups",
                 adjust = 1.2, width = 0.42, justification = -0.08, alpha = 0.35, .width = 0, point_colour = NA,
                 interval_colour = NA, slab_colour = NA) +
    geom_line(data = filter(medias, sexo == "Females"), aes(y = media), linewidth = 0.8) +
    geom_textline(data = filter(medias, sexo == "Males"), aes(y = media, label = VARIE), linewidth = 0.8, size = 2.6, fontface = "bold",
                  hjust = 0.82, vjust = -0.5, family = "Helvetica", text_smoothing = 30) +
    stat_pointinterval(aes(group = interaction(edad, VARIE)), .width = 0.95, point_interval = "mean_qi", orientation = "vertical",
                       position = position_dodge(width = 0.18), size = 0.9, linewidth = 0.6, point_fill = "white",
                       shape = 21, stroke = 0.6, interval_alpha = 0.8) +
    facet_wrap(~sexo) +
    scale_colour_manual(values = COLORES_RAZA, guide = "none") +
    scale_fill_manual(values = COLORES_RAZA, guide = "none") +
    scale_x_continuous(breaks = 1:5, expand = expansion(add = 0.5)) +
    scale_y_continuous(breaks = seq(40, 140, 20)) +
    labs(x = "Age (years)", y = "Live weight (kg)", tag = "a",
         title = sprintf("Growth of <span style='color:%s'>**K'ara**</span> and <span style='color:%s'>**Ch'acu**</span> llamas", AZUL, NARANJA),
         subtitle = "Half-violins: distribution of the 12 animals per cell; points: mean and 95% range") +
    tema_v3() +
    theme(panel.grid.major.x = element_blank())
  dif <- datos |>
    group_by(sexo, edad) |>
    summarise(prueba = list(t.test(PEVI[VARIE == "K'ara"], PEVI[VARIE == "Ch'acu"])), .groups = "drop") |>
    mutate(d = map_dbl(prueba, \(t) unname(diff(rev(t$estimate)))), se = map_dbl(prueba, "stderr"),
           gl = map_dbl(prueba, \(t) unname(t$parameter)), sig = map_dbl(prueba, "p.value") < 0.05)
  b <- ggplot(dif, aes(y = factor(edad), colour = sexo, fill = sexo)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    stat_halfeye(aes(xdist = dist_student_t(gl, d, se), slab_alpha = sig), position = position_dodge(width = 0.75),
                 .width = c(0.66, 0.95), point_size = 1.4, interval_size_range = c(0.5, 1.4), height = 0.7,
                 slab_colour = NA, scale = 0.8, normalize = "groups") +
    annotate("richtext", x = 24, y = 5.75, label = sprintf("<span style='color:%s'>K'ara heavier →</span>", AZUL),
             hjust = 1, size = 2.3, fill = NA, label.colour = NA, family = "Helvetica") +
    annotate("richtext", x = -10, y = 5.75, label = sprintf("<span style='color:%s'>← Ch'acu</span>", NARANJA),
             hjust = 0, size = 2.3, fill = NA, label.colour = NA, family = "Helvetica") +
    scale_colour_manual(values = c(Males = "grey15", Females = "grey55"), name = NULL) +
    scale_fill_manual(values = c(Males = "grey35", Females = "grey75"), name = NULL) +
    scale_slab_alpha_discrete(range = c(0.25, 0.8), guide = "none") +
    scale_x_continuous(limits = c(-10, 25)) +
    scale_y_discrete(expand = expansion(add = c(0.3, 1))) +
    labs(x = "K'ara − Ch'acu (kg)", y = "Age (years)", tag = "b",
         title = "Breed difference", subtitle = "Confidence distribution; bars: 66% and 95% CI") +
    tema_v3() +
    theme(legend.position = "inside", legend.position.inside = c(0.98, 0.02), legend.justification = c(1, 0),
          legend.background = element_blank(), panel.grid.major.y = element_blank())
  a + b + plot_layout(widths = c(2.2, 1))
}

# Fig. 5: distribución del RMSE por pliegue del mejor modelo de cada estrategia (rainclouds horizontales)
figura5_v3 <- function(anidado) {
  pliegues <- imap(anidado$pliegues, \(p, s) mutate(filter(p, rep < 3), estrategia = s)) |> bind_rows()
  mejores <- pliegues |> group_by(estrategia, dataset, model) |> summarise(m = mean(RMSE), .groups = "drop") |>
    group_by(estrategia, dataset) |> slice_min(m, n = 1) |> ungroup()
  datos <- semi_join(pliegues, mejores, by = c("estrategia", "dataset", "model")) |>
    mutate(conjunto = factor(html_conjunto(dataset), levels = rev(html_conjunto(CONJUNTOS))))
  cambio <- mejores |> group_by(dataset) |>
    summarise(c = 100 * (m[estrategia == "C"] / m[estrategia == "A"] - 1), .groups = "drop") |>
    mutate(conjunto = factor(html_conjunto(dataset), levels = levels(datos$conjunto)))
  ggplot(datos, aes(RMSE, conjunto, fill = estrategia, colour = estrategia)) +
    stat_slab(position = position_dodgejust(width = 0.75), height = 0.85, alpha = 0.55, normalize = "groups",
              slab_colour = NA, scale = 0.9, adjust = 1.3) +
    stat_pointinterval(position = position_dodgejust(width = 0.75), point_interval = "mean_qi", .width = c(0.5, 0.9),
                       point_size = 1.3, interval_size_range = c(0.4, 1.1), shape = 21, point_fill = "white", stroke = 0.6) +
    geom_richtext(data = cambio, aes(x = 30, y = conjunto, label = sprintf("**−%.0f%%**", -c)), inherit.aes = FALSE,
                  hjust = 1, size = 2.6, fill = NA, label.colour = NA, family = "Helvetica") +
    scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20), labels = c("0.5", "1", "2", "5", "10", "20")) +
    coord_cartesian(xlim = c(0.25, 30)) +
    scale_fill_manual(values = c(A = "#c9c9c9", B = "#79a7cf", C = "#1f4e79"), guide = "none") +
    scale_colour_manual(values = c(A = "grey45", B = "#4f86b8", C = "#1f4e79"), guide = "none") +
    labs(x = "RMSE per outer test fold (kg, log scale)", y = NULL,
         title = "Removing correlated predictors costs accuracy",
         subtitle = paste0("Best model per strategy: <span style='color:grey45'>**A** |r| ≥ 0.75 + Boruta</span> · ",
                           "<span style='color:#4f86b8'>**B** guided |r| ≥ 0.95 + Boruta</span> · ",
                           "<span style='color:#1f4e79'>**C** all 12 measurements</span><br>",
                           "Slabs: 30 outer folds; point: mean; bars: 50% and 90% intervals; right: change A → C")) +
    tema_v3() +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major.y = element_blank())
}

# Fig. 6: (a) mapa de calor con scico; (b) raincloud del RMSE por pliegue relativo al mejor modelo, en los 7 conjuntos
figura6_v3 <- function(anidado) {
  datos <- anidado$resumen |>
    group_by(dataset) |> mutate(relativo = RMSE / min(RMSE)) |> ungroup() |>
    mutate(conjunto = factor(html_conjunto(dataset), levels = rev(html_conjunto(CONJUNTOS))),
           modelo = factor(model, levels = MODELOS), familia = factor(FAMILIA[model], levels = names(COLOR_FAMILIA)))
  a <- ggplot(datos, aes(modelo, conjunto, fill = relativo)) +
    geom_tile(colour = "white", linewidth = 0.9) +
    geom_tile(data = filter(datos, best), fill = NA, colour = "black", linewidth = 0.7) +
    geom_text(aes(label = sprintf("%.2f", RMSE), fontface = ifelse(equivalent, "bold", "plain"),
                  colour = relativo > 1.9), size = 2.3, family = "Helvetica", show.legend = FALSE) +
    facet_grid(~familia, scales = "free_x", space = "free_x") +
    scale_fill_scico(palette = "lajolla", direction = -1, begin = 0.05, end = 0.85, trans = "log2", breaks = c(1, 1.5, 2, 3),
                     labels = c("best", "1.5×", "2×", "3×"), name = "RMSE relative\nto best") +
    scale_colour_manual(values = c("FALSE" = "grey10", "TRUE" = "white")) +
    labs(x = NULL, y = NULL, tag = "a", title = "Nested cross-validated RMSE (kg)",
         subtitle = "Framed: best model · **bold**: statistically equivalent (P ≥ 0.05)") +
    tema_v3() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), strip.text = element_markdown(face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 40, hjust = 1), panel.grid = element_blank(),
          legend.title = element_text(size = 7), legend.key.height = unit(6, "mm"))
  mejor <- group_by(anidado$resumen, dataset) |> summarise(ref = min(RMSE))
  pliegues <- anidado$pliegues$C |>
    left_join(mejor, by = "dataset") |>
    mutate(rel = RMSE / ref, familia = factor(FAMILIA[model], levels = names(COLOR_FAMILIA)))
  orden <- pliegues |> group_by(model) |> summarise(m = median(rel)) |> arrange(desc(m)) |> pull(model)
  pliegues <- mutate(pliegues, modelo = factor(model, levels = orden))
  b <- ggplot(pliegues, aes(rel, modelo, fill = familia, colour = familia)) +
    geom_vline(xintercept = 1, colour = "grey50", linewidth = 0.3, linetype = "22") +
    stat_halfeye(adjust = 1.4, height = 0.8, alpha = 0.5, slab_colour = NA, .width = c(0.5, 0.9),
                 point_interval = "median_qi", point_size = 1.3, interval_size_range = c(0.4, 1.1)) +
    scale_x_log10(breaks = c(0.5, 1, 2, 4), labels = c("0.5×", "1×", "2×", "4×")) +
    scale_y_discrete(labels = \(x) sprintf("<span style='color:%s'>**%s**</span>",
                                           ifelse(FAMILIA[x] == "Statistical", "#7d8894", "#1f4e79"), x)) +
    coord_cartesian(xlim = c(0.3, 6)) +
    scale_fill_manual(values = COLOR_FAMILIA, guide = "none") +
    scale_colour_manual(values = COLOR_FAMILIA, guide = "none") +
    labs(x = "Fold RMSE ÷ best (log)", y = NULL, tag = "b",
         title = sprintf("<span style='color:%s'>**Machine learning**</span> vs <span style='color:%s'>**statistical**</span>",
                         COLOR_FAMILIA[["Machine learning"]], "#7d8894"),
         subtitle = "700 outer folds (7 datasets × 100)") +
    tema_v3() +
    theme(panel.grid.major.y = element_blank())
  a + b + plot_layout(widths = c(2.5, 1))
}

# Fig. 7: observado frente a predicho con densidades marginales del error por edad (ggside)
figura7_v3 <- function(oof, resumen) {
  metricas <- oof |> group_by(panel, dataset) |>
    summarise(n = n(), lo = min(obs, pred), hi = max(obs, pred), .groups = "drop") |>
    left_join(filter(resumen, best) |> select(dataset, RMSE, R2), by = "dataset") |>
    mutate(etiqueta = sprintf("RMSE **%.2f** kg<br>R² **%.3f**", RMSE, R2))
  limites <- bind_rows(transmute(metricas, panel, x = lo, y = lo), transmute(metricas, panel, x = hi, y = hi))
  oof <- mutate(oof, panel_html = factor(html_panel(as.character(panel)), levels = html_panel(levels(panel))))
  metricas <- mutate(metricas, panel_html = factor(html_panel(as.character(panel)), levels = levels(oof$panel_html)))
  limites <- mutate(limites, panel_html = factor(html_panel(as.character(panel)), levels = levels(oof$panel_html)))
  ggplot(oof, aes(pred, obs)) +
    geom_blank(data = limites, aes(x, y)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey55", linewidth = 0.35, linetype = "22") +
    geom_point(aes(fill = EDAD), shape = 21, colour = "white", stroke = 0.15, size = 1.5, alpha = 0.9) +
    geom_richtext(data = metricas, aes(x = lo, y = hi, label = etiqueta), hjust = 0, vjust = 1, size = 2.2,
                  fill = NA, label.colour = NA, lineheight = 1.1, family = "Helvetica", colour = "grey20") +
    facet_wrap(~panel_html, ncol = 4, scales = "free") +
    scale_fill_manual(values = COLOR_EDAD3, name = "Age (years)") +
    guides(fill = guide_legend(override.aes = list(size = 2.6), nrow = 1, title.position = "top")) +
    labs(x = "Predicted live weight (kg)", y = "Observed live weight (kg)",
         title = "Out-of-fold predictions of the best model in each dataset",
         subtitle = "Dashed line: line of equality; RMSE and R\u00b2 from nested cross-validation") +
    tema_v3() +
    theme(legend.position = "inside", legend.position.inside = c(0.875, 0.22), aspect.ratio = 1)
}

# Fig. 8: Bland-Altman con distribución marginal de las diferencias (ggside) y banda de los límites de acuerdo
figura8_v3 <- function(oof) {
  oof <- mutate(oof, panel_html = factor(html_panel(as.character(panel)), levels = html_panel(levels(panel))))
  ba <- oof |> group_by(panel_html) |> summarise(sesgo = mean(dif), sd = sd(dif), .groups = "drop") |>
    mutate(lo = sesgo - 1.96 * sd, hi = sesgo + 1.96 * sd)
  ggplot(oof, aes(media, dif)) +
    geom_rect(data = ba, aes(xmin = -Inf, xmax = Inf, ymin = lo, ymax = hi), inherit.aes = FALSE, fill = "grey94") +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3, linetype = "22") +
    geom_hline(data = ba, aes(yintercept = sesgo), colour = "grey10", linewidth = 0.5) +
    geom_point(aes(fill = EDAD), shape = 21, colour = "white", stroke = 0.15, size = 1.4) +
    geom_ysidedensity(aes(x = after_stat(density)), fill = "grey55", colour = NA, alpha = 0.7) +
    geom_ysidehline(data = ba, aes(yintercept = sesgo), colour = "grey10", linewidth = 0.4) +
    geom_text(data = ba, aes(x = Inf, y = hi, label = sprintf("+%.1f", hi)), hjust = 1.1, vjust = -0.4, size = 2.1,
              colour = "grey35", family = "Helvetica") +
    geom_text(data = ba, aes(x = Inf, y = lo, label = sprintf("%.1f", lo)), hjust = 1.1, vjust = 1.4, size = 2.1,
              colour = "grey35", family = "Helvetica") +
    geom_text(data = filter(oof, NRO == 139, dataset == "Ch'acu males"), aes(label = "#139"), hjust = -0.25,
              size = 2.1, colour = "grey20", family = "Helvetica") +
    facet_wrap(~panel_html, ncol = 4, scales = "free") +
    scale_fill_manual(values = COLOR_EDAD3, name = "Age (years)") +
    scale_y_continuous(expand = expansion(mult = 0.12)) +
    scale_ysidex_continuous(guide = "none") +
    guides(fill = guide_legend(override.aes = list(size = 2.6), nrow = 1, title.position = "top")) +
    labs(x = "Mean of observed and predicted live weight (kg)", y = "Predicted − observed (kg)",
         title = "Agreement between predicted and observed live weight",
         subtitle = "Black line: bias; shaded band: 95% limits of agreement; side panel: distribution of differences") +
    tema_v3() +
    theme(legend.position = "inside", legend.position.inside = c(0.875, 0.22), ggside.panel.scale = 0.2,
          ggside.panel.background = element_blank(), ggside.axis.text = element_blank(), ggside.axis.ticks = element_blank(),
          ggside.axis.line = element_blank(), ggside.panel.grid = element_blank())
}

# Fig. 9: (a) mapa de calor del error por edad (scico); (b) raincloud de los errores absolutos por edad (todos los conjuntos)
figura9_v3 <- function(oof) {
  por_edad <- oof |> group_by(panel, EDAD) |>
    summarise(rmse = sqrt(mean(dif^2)), rel = 100 * rmse / mean(obs), .groups = "drop") |>
    mutate(panel = factor(html_conjunto(as.character(panel)), levels = rev(html_conjunto(levels(oof$panel)))))
  a <- ggplot(por_edad, aes(EDAD, panel, fill = rel)) +
    geom_tile(colour = "white", linewidth = 0.9) +
    geom_text(aes(label = sprintf("%.2f", rmse), colour = rel > 6), size = 2.5, fontface = "bold", family = "Helvetica", nudge_y = 0.13) +
    geom_text(aes(label = sprintf("%.1f%%", rel), colour = rel > 6), size = 1.9, family = "Helvetica", nudge_y = -0.2) +
    scale_fill_gradientn(colours = c("#f6f3ec", "#dcdcd3", "#aab8c6", "#6f88a3", "#3e5671"), limits = c(0, 10), name = "RMSE, % of\nmean weight") +
    scale_colour_manual(values = c("FALSE" = "grey10", "TRUE" = "white"), guide = "none") +
    scale_x_discrete(position = "top") +
    labs(x = "Age class (years)", y = NULL, tag = "a", title = "Within-age RMSE (kg) of the best model") +
    tema_v3() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
          legend.title = element_text(size = 7), legend.key.height = unit(6, "mm"))
  b <- ggplot(filter(oof, dataset != "Pooled"), aes(abs(dif), EDAD, fill = EDAD, colour = EDAD)) +
    stat_slab(height = 0.85, alpha = 0.6, slab_colour = NA, adjust = 1, scale = 0.9) +
    stat_dots(side = "bottom", quantiles = 40, scale = 0.5, alpha = 0.9) +
    stat_pointinterval(point_interval = "median_qi", .width = 0.9, point_size = 1.2, colour = "grey10",
                       position = position_nudge(y = -0.03)) +
    scale_x_continuous(trans = "sqrt", breaks = c(0, 1, 2, 5, 10, 20)) +
    scale_fill_manual(values = COLOR_EDAD3, guide = "none") +
    scale_colour_manual(values = COLOR_EDAD3, guide = "none") +
    labs(x = "|Predicted − observed| (kg, sqrt scale)", y = "Age (years)", tag = "b", title = "Absolute errors",
         subtitle = "Breed\u2013sex datasets; 40 quantile dots") +
    tema_v3() + theme(panel.grid.major.y = element_blank())
  a + b + plot_layout(widths = c(1.7, 1))
}

# Fig. 10: mapa de calor de importancia (scico) con barra marginal de la importancia media
figura10_v3 <- function(anidado) {
  mejores <- filter(anidado$resumen, best)
  datos <- anidado$importancia |> group_by(dataset) |>
    mutate(rel = pmin(100, 100 * pmax(mean, 0) / max(mean)), orden = rank(-mean)) |> ungroup()
  media <- datos |> group_by(variable) |> summarise(m = mean(rel), r = mean(orden)) |> arrange(r)
  datos <- mutate(datos, variable = factor(variable, levels = rev(media$variable)),
                  conjunto = factor(html_conjunto(sprintf("%s<br>(%s)", sub(" ", "<br>", dataset), mejores$model[match(dataset, mejores$dataset)])),
                                    levels = html_conjunto(sprintf("%s<br>(%s)", sub(" ", "<br>", CONJUNTOS), mejores$model[match(CONJUNTOS, mejores$dataset)]))))
  a <- ggplot(datos, aes(conjunto, variable, fill = rel)) +
    geom_tile(colour = "white", linewidth = 0.9) +
    geom_text(data = filter(datos, orden <= 3), aes(label = paste0("#", orden), colour = rel > 55), size = 2.4, fontface = "bold", family = "Helvetica") +
    scale_colour_manual(values = c("FALSE" = "grey10", "TRUE" = "white"), guide = "none") +
    scale_fill_scico(palette = "devon", direction = -1, begin = 0.05, limits = c(0, 100), name = "Importance\n(% of top)") +
    scale_x_discrete(position = "top") +
    scale_y_discrete(labels = \(x) sprintf("**%s**", x)) +
    labs(x = NULL, y = NULL, title = "Permutation importance of the twelve measurements",
         subtitle = "#1–#3: highest-ranked variables in each dataset; right: mean across datasets") +
    tema_v3() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
          axis.text.x.top = element_markdown(size = 6.5, lineheight = 1.05),
          legend.title = element_text(size = 7), legend.key.height = unit(6, "mm"), legend.position = "left")
  b <- ggplot(mutate(media, variable = factor(variable, levels = rev(media$variable))), aes(m, variable)) +
    geom_col(fill = scico(1, palette = "devon", begin = 0.35), width = 0.7) +
    geom_text(aes(label = sprintf("%.0f", m)), hjust = -0.2, size = 2.1, colour = "grey30", family = "Helvetica") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25)), breaks = c(0, 50)) +
    labs(x = "Mean (%)", y = NULL) +
    tema_v3() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
          panel.grid.major.y = element_blank())
  a + b + plot_layout(widths = c(7, 1.2))
}

# Fig. 3, opciones de diseño (21/10/2026) ---------------------------------------------------------

# Panel b común: distribución de confianza de la diferencia entre razas
figura3_diferencia <- function(llamas) {
  datos <- mutate(llamas, edad = as.numeric(as.character(EDAD)),
                  sexo = factor(ifelse(SEXO == "Macho", "Males", "Females"), levels = c("Males", "Females")))
  dif <- datos |>
    group_by(sexo, edad) |>
    summarise(prueba = list(t.test(PEVI[VARIE == "K'ara"], PEVI[VARIE == "Ch'acu"])), .groups = "drop") |>
    mutate(d = map_dbl(prueba, \(t) unname(diff(rev(t$estimate)))), se = map_dbl(prueba, "stderr"),
           gl = map_dbl(prueba, \(t) unname(t$parameter)), sig = map_dbl(prueba, "p.value") < 0.05)
  ggplot(dif, aes(y = factor(edad), colour = sexo, fill = sexo)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    stat_halfeye(aes(xdist = dist_student_t(gl, d, se), slab_alpha = sig), position = position_dodge(width = 0.75),
                 .width = c(0.66, 0.95), point_size = 1.4, interval_size_range = c(0.5, 1.4), height = 0.7,
                 slab_colour = NA, scale = 0.8, normalize = "groups") +
    annotate("richtext", x = 25, y = 5.75, label = sprintf("<span style='color:%s'>K'ara heavier →</span>", AZUL),
             hjust = 1, size = 2.3, fill = NA, label.colour = NA, family = "Helvetica") +
    annotate("richtext", x = -10, y = 5.75, label = sprintf("<span style='color:%s'>← Ch'acu</span>", NARANJA),
             hjust = 0, size = 2.3, fill = NA, label.colour = NA, family = "Helvetica") +
    scale_colour_manual(values = c(Males = "grey15", Females = "grey55"), name = NULL) +
    scale_fill_manual(values = c(Males = "grey35", Females = "grey75"), name = NULL) +
    scale_slab_alpha_discrete(range = c(0.25, 0.8), guide = "none") +
    scale_x_continuous(limits = c(-10, 25)) +
    scale_y_discrete(expand = expansion(add = c(0.3, 1))) +
    labs(x = "K'ara − Ch'acu (kg)", y = "Age (years)", tag = "b",
         title = "Breed difference", subtitle = "Confidence distribution; bars: 66% and 95% CI") +
    tema_v3() +
    theme(legend.position = "inside", legend.position.inside = c(0.98, 0.02), legend.justification = c(1, 0),
          legend.background = element_blank(), panel.grid.major.y = element_blank())
}

# Opción A, "flujo": bandas de cuantiles (50, 80 y 95 %) interpoladas entre edades, mediana como línea
figura3_flujo <- function(llamas) {
  datos <- mutate(llamas, edad = as.numeric(as.character(EDAD)),
                  sexo = factor(ifelse(SEXO == "Macho", "Males", "Females"), levels = c("Males", "Females")))
  niveles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  cuantiles <- datos |>
    group_by(sexo, VARIE, edad) |>
    reframe(q = niveles, valor = quantile(PEVI, niveles))
  malla <- seq(1, 5, by = 0.02)
  suave <- cuantiles |>
    group_by(sexo, VARIE, q) |>
    reframe(x = malla, valor = splinefun(edad, valor, method = "monoH.FC")(malla)) |>
    rename(edad = x) |>
    pivot_wider(names_from = q, values_from = valor, names_prefix = "q")
  bandas <- list(c("q0.025", "q0.975", 0.16), c("q0.1", "q0.9", 0.22), c("q0.25", "q0.75", 0.32))
  p <- ggplot(suave, aes(edad, fill = VARIE, colour = VARIE))
  for (b in bandas) {
    p <- p + geom_ribbon(aes(ymin = .data[[b[1]]], ymax = .data[[b[2]]]), colour = NA, alpha = as.numeric(b[3]))
  }
  etiquetas <- filter(suave, edad == 5)
  p +
    geom_point(data = datos, aes(x = edad + ifelse(VARIE == "K'ara", -0.07, 0.07), y = PEVI), inherit.aes = FALSE,
               colour = "white", size = 0.55, alpha = 0.9) +
    geom_line(aes(y = q0.5), linewidth = 0.9) +
    geom_text(data = etiquetas, aes(x = 5.08, y = q0.5, label = VARIE), hjust = 0, size = 2.7, fontface = "bold",
              family = "Helvetica") +
    facet_wrap(~sexo) +
    scale_fill_manual(values = COLORES_RAZA, guide = "none") +
    scale_colour_manual(values = COLORES_RAZA, guide = "none") +
    scale_x_continuous(breaks = 1:5, limits = c(0.9, 5.8)) +
    scale_y_continuous(breaks = seq(40, 140, 20)) +
    labs(x = "Age (years)", y = "Live weight (kg)", tag = "a",
         title = sprintf("Growth of <span style='color:%s'>**K'ara**</span> and <span style='color:%s'>**Ch'acu**</span> llamas", AZUL, NARANJA),
         subtitle = "Line: median; bands: 50%, 80% and 95% of animals; white dots: individual llamas") +
    tema_v3() +
    theme(panel.grid.major.x = element_blank())
}

# Opción B, "crestas": distribución del peso por edad (ggridges), las dos razas superpuestas
figura3_crestas <- function(llamas) {
  datos <- mutate(llamas, edad = factor(EDAD, levels = 1:5),
                  sexo = factor(ifelse(SEXO == "Macho", "Males", "Females"), levels = c("Males", "Females")))
  ggplot(datos, aes(PEVI, edad, fill = VARIE, colour = VARIE)) +
    ggridges::geom_density_ridges(scale = 1.6, alpha = 0.55, linewidth = 0.35, rel_min_height = 0.01,
                                  jittered_points = TRUE, point_size = 0.5, point_alpha = 0.8,
                                  position = ggridges::position_points_jitter(height = 0.05, yoffset = -0.08),
                                  bandwidth = 2.2, quantile_lines = TRUE, quantiles = 2) +
    facet_wrap(~sexo) +
    scale_fill_manual(values = COLORES_RAZA, guide = "none") +
    scale_colour_manual(values = COLORES_RAZA, guide = "none") +
    scale_x_continuous(breaks = seq(40, 140, 20)) +
    scale_y_discrete(expand = expansion(add = c(0.3, 1.7))) +
    labs(x = "Live weight (kg)", y = "Age (years)", tag = "a",
         title = sprintf("Live weight of <span style='color:%s'>**K'ara**</span> and <span style='color:%s'>**Ch'acu**</span> llamas by age", AZUL, NARANJA),
         subtitle = "Ridges: weight distribution of the 12 animals per cell; vertical line: median") +
    tema_v3() +
    theme(panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.25))
}

# Fig. 5, opción "pendientes": una línea por conjunto que une A, B y C (RMSE medio del mejor modelo), etiquetas directas
separar_etiquetas <- function(y, hueco = 0.05) {
  orden <- order(y); z <- log10(y[orden])
  for (i in seq_along(z)[-1]) z[i] <- max(z[i], z[i - 1] + hueco)
  z <- z - (mean(z) - mean(log10(y[orden])))
  salida <- numeric(length(y)); salida[orden] <- 10^z; salida
}

figura5_pendientes <- function(anidado) {
  pliegues <- imap(anidado$pliegues, \(p, s) mutate(filter(p, rep < 3), estrategia = s)) |> bind_rows()
  datos <- pliegues |>
    group_by(estrategia, dataset, model) |>
    summarise(m = mean(RMSE), .groups = "drop") |>
    group_by(estrategia, dataset) |> slice_min(m, n = 1) |> ungroup() |>
    mutate(x = match(estrategia, c("A", "B", "C")),
           raza = ifelse(grepl("^K'ara", dataset), "K'ara", ifelse(grepl("^Ch'acu", dataset), "Ch'acu", "Pooled")),
           grupo = ifelse(grepl("males|females", dataset), "sex", "breed"))
  colores <- c(COLORES_RAZA, Pooled = "grey25")
  finales <- datos |> group_by(dataset) |>
    summarise(y = m[estrategia == "C"], c = 100 * (m[estrategia == "C"] / m[estrategia == "A"] - 1),
              raza = first(raza), .groups = "drop") |>
    mutate(y_lab = separar_etiquetas(y),
           etiqueta = sprintf("<span style='color:%s'>**%s**</span> <span style='color:grey40'>%.2f kg \u00b7 \u2212%.0f%%</span>",
                              colores[raza], dataset, y, -c))
  iniciales <- filter(datos, estrategia == "A") |> mutate(y_lab = separar_etiquetas(m))
  ggplot(datos, aes(x, m, group = dataset, colour = raza)) +
    geom_segment(data = finales, aes(x = 3.04, xend = 3.28, y = y, yend = y_lab), inherit.aes = FALSE,
                 colour = "grey70", linewidth = 0.25) +
    geom_line(aes(linetype = grupo), linewidth = 0.75) +
    geom_point(aes(shape = grupo), fill = "white", size = 2.3, stroke = 0.9) +
    geom_text(data = iniciales, aes(x = 0.93, y = y_lab, label = sprintf("%.1f", m)), hjust = 1, size = 2.3,
              family = "Helvetica", show.legend = FALSE) +
    geom_richtext(data = finales, aes(x = 3.3, y = y_lab, label = etiqueta), inherit.aes = FALSE,
                  hjust = 0, size = 2.4, fill = NA, label.colour = NA, family = "Helvetica") +
    scale_x_continuous(breaks = 1:3, labels = c("**A**<br>|r| \u2265 0.75 + Boruta", "**B**<br>guided |r| \u2265 0.95<br>+ Boruta",
                                                 "**C**<br>all 12 measurements"),
                       limits = c(0.7, 4.6), expand = expansion(0)) +
    scale_y_log10(breaks = c(1, 2, 3, 5, 10)) +
    scale_colour_manual(values = colores, guide = "none") +
    scale_linetype_manual(values = c(breed = "solid", sex = "22"), labels = c(breed = "Breed or pooled", sex = "Sex within breed"), name = NULL) +
    scale_shape_manual(values = c(breed = 21, sex = 24), labels = c(breed = "Breed or pooled", sex = "Sex within breed"), name = NULL) +
    labs(x = NULL, y = "Nested cross-validated RMSE of the best model (kg, log scale)",
         title = "Keeping all measurements lowers the error",
         subtitle = "Mean RMSE of the best model under each predictor selection strategy; right: RMSE with C and change from A") +
    tema_v3() +
    theme(axis.text.x = element_markdown(lineheight = 1.15, colour = "grey15"), panel.grid.major.x = element_blank(),
          legend.position = "inside", legend.position.inside = c(0.02, 0.02), legend.justification = c(0, 0),
          legend.background = element_blank(), legend.key.width = unit(8, "mm"))
}

# Fig. 6, opción "mapa anotado": arriba, distribución del RMSE relativo por modelo (700 pliegues) alineada con
# las columnas; centro, mapa de calor; derecha, RMSE del mejor modelo de cada conjunto
figura6_anotada <- function(anidado) {
  datos <- anidado$resumen |>
    group_by(dataset) |> mutate(relativo = RMSE / min(RMSE)) |> ungroup() |>
    mutate(conjunto = factor(html_conjunto(dataset), levels = rev(html_conjunto(CONJUNTOS))),
           modelo = factor(model, levels = MODELOS), familia = factor(FAMILIA[model], levels = names(COLOR_FAMILIA)))
  mejor <- group_by(anidado$resumen, dataset) |> summarise(ref = min(RMSE), modelo = model[which.min(RMSE)])
  pliegues <- anidado$pliegues$C |> left_join(mejor, by = "dataset") |>
    mutate(rel = RMSE / ref, modelo = factor(model, levels = MODELOS),
           familia = factor(FAMILIA[model], levels = names(COLOR_FAMILIA)))
  n_eq <- datos |> group_by(modelo, familia) |> summarise(n = sum(equivalent), .groups = "drop")
  sin_eje_x <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(),
                     axis.title.x = element_blank())

  arriba <- ggplot(pliegues, aes(modelo, rel, fill = familia, colour = familia)) +
    geom_hline(yintercept = 1, colour = "grey50", linewidth = 0.3, linetype = "22") +
    stat_halfeye(adjust = 1.4, width = 0.8, alpha = 0.45, slab_colour = NA, .width = c(0.5, 0.9),
                 point_interval = "median_qi", point_size = 1.1, interval_size_range = c(0.4, 1),
                 trim = TRUE) +
    facet_grid(~familia, scales = "free_x", space = "free_x") +
    scale_y_log10(breaks = c(0.5, 1, 2, 4), labels = c("0.5×", "1×", "2×", "4×")) +
    coord_cartesian(ylim = c(0.35, 5.5)) +
    scale_fill_manual(values = COLOR_FAMILIA, guide = "none") +
    scale_colour_manual(values = COLOR_FAMILIA, guide = "none") +
    labs(y = "Fold RMSE\n÷ best",
         title = sprintf("<span style='color:%s'>**Machine learning**</span> outperforms <span style='color:%s'>**statistical**</span> models",
                         COLOR_FAMILIA[["Machine learning"]], "#7d8894"),
         subtitle = "Top: 700 outer folds per model (median, 50% and 90% intervals); x/7 under each model: datasets where it was best or equivalent") +
    tema_v3() + sin_eje_x +
    theme(strip.text = element_markdown(face = "bold", hjust = 0.5), panel.grid.major.x = element_blank(),
          axis.title.y = element_text(size = 7))

  centro <- ggplot(datos, aes(modelo, conjunto, fill = relativo)) +
    geom_tile(colour = "white", linewidth = 0.9) +
    geom_tile(data = filter(datos, best), fill = NA, colour = "grey15", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.2f", RMSE), fontface = ifelse(equivalent, "bold", "plain"),
                  colour = relativo > 2), size = 2.3, family = "Helvetica", show.legend = FALSE) +
    facet_grid(~familia, scales = "free_x", space = "free_x") +
    scale_fill_gradientn(colours = c("#f6f3ec", "#dcdcd3", "#aab8c6", "#6f88a3", "#3e5671"), trans = "log2",
                         breaks = c(1, 1.5, 2, 3), labels = c("best", "1.5×", "2×", "3×"), name = "RMSE relative\nto best") +
    scale_colour_manual(values = c("FALSE" = "grey10", "TRUE" = "white")) +
    scale_x_discrete(labels = \(x) sprintf("**%s**<br><span style='color:grey45'>%d/7</span>", x,
                                           n_eq$n[match(x, n_eq$modelo)])) +
    labs(x = NULL, y = NULL) +
    tema_v3() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
          strip.text = element_blank(), axis.text.x = element_markdown(colour = "grey15", lineheight = 1.2),
          legend.title = element_text(size = 7), legend.key.height = unit(6, "mm"))

  derecha <- ggplot(mutate(mejor, conjunto = factor(html_conjunto(dataset), levels = levels(datos$conjunto))),
                    aes(ref, conjunto)) +
    geom_col(aes(fill = grepl("^K'ara", dataset) + 2 * grepl("^Ch'acu", dataset)), width = 0.62, alpha = 0.75,
             show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.2f", ref)), hjust = -0.15, size = 2.2, colour = "grey20", family = "Helvetica") +
    scale_fill_gradientn(colours = c("grey40", AZUL, NARANJA), limits = c(0, 2)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.35)), breaks = c(0, 2)) +
    labs(x = "Best RMSE\n(kg)", y = NULL) +
    tema_v3() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
          panel.grid.major.y = element_blank(), axis.title.x = element_text(size = 7))

  (arriba + plot_spacer() + centro + derecha) +
    plot_layout(ncol = 2, widths = c(6, 1), heights = c(1.1, 2), guides = "collect") &
    theme(legend.position = "right")
}
