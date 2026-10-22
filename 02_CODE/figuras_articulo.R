source("02_CODE/R/cargar.R")

llamas <- preparar_datos(config, correcciones_campo)$llamas
resultados <- readRDS(file.path(config$salida, "resultados.rds"))

exportar_figuras(llamas, resultados, config)
exportar_figuras(llamas, resultados, config, idioma = "en", carpeta = "figures_en",
                 formatos = c("pdf", "png"), dpi = 600, mapa_contexto = FALSE)
