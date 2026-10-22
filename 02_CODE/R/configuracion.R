config <- list(
  archivo = "01_DATA/BASE DATOS LLAMAS_CHACU_KARA_1_a_5_year_VRI_2026.xlsx",
  salida = "03_RESULTS",
  semilla = 2026,
  proporcion_entrenamiento = 0.75,
  folds = 10,
  repeticiones = 10,
  escenario_principal = "completo",
  sitio = list(nombre = "CIP La Raya", latitud = -14.4954, longitud = -70.9412, altitud = 4198),
  qgis = "/Applications/QGIS-final-4_2_2.app/Contents/MacOS/qgis_process",
  nucleos = max(1, parallel::detectCores() - 2)
)

correcciones_campo <- tribble(
  ~NRO, ~variable, ~valor_corregido,
  69,   "ALGRU",   NA_real_,
  70,   "ALGRU",   NA_real_,
  84,   "ALAB",    NA_real_
)

preparar_datos <- function(config, correcciones) {
  originales <- leer_llamas(config$archivo)
  atipicos <- detectar_atipicos(originales)
  depuracion <- depurar_datos(originales, atipicos, correcciones)
  list(atipicos = atipicos, cambios = depuracion$cambios, llamas = depuracion$datos)
}
