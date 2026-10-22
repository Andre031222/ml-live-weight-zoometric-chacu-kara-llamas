# Ejecutar desde la raíz del paper: Rscript 02_CODE/figuras_v3.R
# Salida: 03_RESULTS/figures_v3 (con títulos, para presentaciones) y 03_RESULTS/figures_v3_envio (sin títulos, para la revista)
source("02_CODE/R/cargar.R")
source("02_CODE/R/figuras_v2.R")
source("02_CODE/R/figuras_v3.R")
suppressPackageStartupMessages(library(readr))
options(llamas.idioma = "en", OutDec = ".")

llamas <- preparar_datos(config, correcciones_campo)$llamas
anidado <- leer_anidado()
oof <- oof_mejor(anidado, llamas)

sin_titulos <- function(p) {
  quitar <- theme(plot.title = element_blank(), plot.subtitle = element_blank())
  if (inherits(p, "patchwork")) p & quitar else p + quitar
}

figuras <- list(
  Figure_2 = list(graficar_esquema_campo(), 19, 12.5),
  Figure_3 = list(figura3_crestas(llamas) + figura3_diferencia(llamas) + plot_layout(widths = c(2.2, 1)), 19, 10),
  Figure_5 = list(figura5_pendientes(anidado), 16, 11),
  Figure_6 = list(figura6_anotada(anidado), 19, 14),
  Figure_7 = list(figura7_v3(oof, anidado$resumen), 19, 12),
  Figure_8 = list(figura8_v3(oof), 19, 12),
  Figure_9 = list(figura9_v3(oof), 19, 9),
  Figure_10 = list(figura10_v3(anidado), 17, 10)
)
for (nombre in names(figuras)) {
  f <- figuras[[nombre]]
  exportar_v2(f[[1]], nombre, f[[2]], f[[3]], "03_RESULTS/figures_v3")
  exportar_v2(sin_titulos(f[[1]]), nombre, f[[2]], f[[3]] - 1, "03_RESULTS/figures_v3_envio")
}
