# Ejecutar desde la raíz del paper: Rscript 02_CODE/figuras_v2.R
# Requiere los CSV de 02_CODE/python_nested/export_csv.py. Salida en 03_RESULTS/figures_v2 (numeradas por orden de aparición).
source("02_CODE/R/cargar.R")
source("02_CODE/R/figuras_v2.R")
suppressPackageStartupMessages(library(readr))
options(llamas.idioma = "en", OutDec = ".")

llamas <- preparar_datos(config, correcciones_campo)$llamas
anidado <- leer_anidado()
oof <- oof_mejor(anidado, llamas)

exportar_v2(figura3_v2(llamas), "Figure_3", 19, 8.5)
exportar_v2(figura5_v2(anidado), "Figure_5", 14, 8)
exportar_v2(figura6_v2(anidado), "Figure_6", 19, 8.5)
exportar_v2(figura7_v2(oof, anidado$resumen), "Figure_7", 19, 10.5)
exportar_v2(figura8_v2(oof), "Figure_8", 19, 10.5)
exportar_v2(figura9_v2(oof), "Figure_9", 14, 8)
exportar_v2(figura10_v2(anidado), "Figure_10", 14, 9.5)
exportar_v2(figura_area_estudio(config, contexto = TRUE), "Figure_1", 19, 13)
