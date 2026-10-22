# Graphical abstract (sin IA): foto real del CIP La Raya + resultados de la validación anidada.
# Ejecutar desde la raíz: Rscript 02_CODE/graphical_abstract.R -> 05_SUBMISSION_SAT/graphical_abstract.png (≥1328×531 px)
source("02_CODE/R/cargar.R")
source("02_CODE/R/figuras_v2.R")
source("02_CODE/R/figuras_v3.R")
suppressPackageStartupMessages(library(readr))
options(llamas.idioma = "en", OutDec = ".")

anidado <- leer_anidado()
foto <- leer_foto("llama_campo_lateral.png")
tema_ga <- theme_void(base_family = "Helvetica") +
  theme(plot.title = element_markdown(size = 10.5, face = "bold", hjust = 0.5, margin = margin(b = 4)),
        plot.subtitle = element_markdown(size = 7.5, colour = "grey30", hjust = 0.5))

campo <- ggplot() +
  annotation_raster(foto$imagen, xmin = 0, xmax = foto$ancho, ymin = 0, ymax = foto$alto, interpolate = TRUE) +
  coord_equal(xlim = c(250, foto$ancho), ylim = c(80, foto$alto - 80), expand = FALSE) +
  labs(title = "1 · Field measurements", subtitle = "240 llamas, 2 breeds, 2 sexes, 1–5 years<br>12 body measurements (stick + tape)") +
  tema_ga

mejor <- filter(anidado$resumen, best) |> select(dataset, mejor = RMSE)
comp <- anidado$resumen |> filter(model == "MLR") |> select(dataset, mlr = RMSE) |> left_join(mejor, by = "dataset") |>
  mutate(conjunto = factor(dataset, levels = rev(CONJUNTOS)),
         color = ifelse(grepl("^K'ara", dataset), AZUL, ifelse(grepl("^Ch'acu", dataset), NARANJA, "grey30")))
modelos <- ggplot(comp, aes(y = conjunto)) +
  geom_segment(aes(x = mejor, xend = mlr, yend = conjunto), colour = "grey80", linewidth = 2.2, lineend = "round") +
  geom_point(aes(x = mlr), colour = "#9aa5b1", size = 2.6) +
  geom_point(aes(x = mejor, colour = color), size = 2.8) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0, 7), breaks = c(0, 2, 4, 6)) +
  annotate("text", x = 6.8, y = 7.75, label = "MLR", colour = "#7d8894", size = 2.7, hjust = 1, family = "Helvetica", fontface = "bold") +
  annotate("text", x = 0.2, y = 7.75, label = "Best ML", colour = "grey20", size = 2.7, hjust = 0, family = "Helvetica", fontface = "bold") +
  scale_y_discrete(expand = expansion(add = c(0.5, 1.2))) +
  labs(x = "Nested cross-validated RMSE (kg)", y = NULL, title = "2 · Nested cross-validation",
       subtitle = "11 models per breed, sex and pooled dataset") +
  theme_minimal(base_size = 8, base_family = "Helvetica") +
  theme(plot.title = element_markdown(size = 10.5, face = "bold", hjust = 0.5, margin = margin(b = 4)),
        plot.subtitle = element_markdown(size = 7.5, colour = "grey30", hjust = 0.5),
        axis.text.y = element_text(colour = rev(c(rep(AZUL, 3), rep(NARANJA, 3), "grey30")), face = "bold"),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())

mensajes <- tibble(
  y = c(4, 3, 2, 1),
  cifra = c("0.9–3.1 kg", "38–64%", "26–31%", "×1.2–4.5"),
  texto = c("best-model error<br>(KNN, SVM, AdaBoost,<br>ANN)", "lower error than<br>linear regression",
            "lower error with<br>breed-specific models", "more error if the |r| ≥ 0.75<br>filter removes predictors")
)
resultados <- ggplot(mensajes) +
  geom_richtext(aes(x = 0, y = y, label = sprintf("<span style='color:#1f4e79'>**%s**</span>", cifra)), hjust = 0,
                size = 4.2, fill = NA, label.colour = NA, family = "Helvetica") +
  geom_richtext(aes(x = 0.5, y = y, label = texto), hjust = 0, size = 2.6, fill = NA, label.colour = NA,
                family = "Helvetica", colour = "grey20", lineheight = 1.1) +
  scale_x_continuous(limits = c(-0.02, 1.15)) + scale_y_continuous(limits = c(0.5, 4.5)) +
  labs(title = "3 · Key results", subtitle = "Keep all 12 measurements and<br>model each breed and sex") +
  tema_ga

flecha <- ggplot() + annotate("text", x = 0, y = 0, label = "➜", size = 9, colour = "grey55") + theme_void()
ga <- (campo | flecha | modelos | flecha | resultados) +
  plot_layout(widths = c(1, 0.12, 1, 0.12, 1.25)) +
  plot_annotation(title = "Machine learning prediction of live weight from body measurements in K'ara and Ch'acu llamas",
                  theme = theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 12, hjust = 0.5),
                                plot.background = element_rect(fill = "white", colour = NA)))
ggsave("05_SUBMISSION_SAT/graphical_abstract.png", ga, width = 20, height = 8, units = "cm", dpi = 300)
ggsave("05_SUBMISSION_SAT/graphical_abstract.pdf", ga, width = 20, height = 8, units = "cm", device = cairo_pdf)
