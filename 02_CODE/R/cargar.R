for (modulo in c("paquetes", "datos", "calidad", "descriptiva", "seleccion", "modelos", "evaluacion",
                 "idioma", "graficos", "esquema", "mapa", "figuras", "pipeline", "configuracion")) {
  source(file.path("02_CODE", "R", paste0(modulo, ".R")))
}
