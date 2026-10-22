ZOOMETRICAS <- c(
  "ALCRU", "ALGRU", "PETO", "PEAB", "LARDO", "LACU",
  "PROF", "AMPLI", "ALAB", "LAGRU", "ANGRU", "PECAN"
)

ESTRATOS <- tribble(
  ~id,             ~etiqueta,          ~raza,    ~sexo,
  "kara",          "K'ara",            "K'ara",  NA,
  "kara_machos",   "K'ara machos",     "K'ara",  "Macho",
  "kara_hembras",  "K'ara hembras",    "K'ara",  "Hembra",
  "chacu",         "Ch'acu",           "Ch'acu", NA,
  "chacu_machos",  "Ch'acu machos",    "Ch'acu", "Macho",
  "chacu_hembras", "Ch'acu hembras",   "Ch'acu", "Hembra",
  "general",       "General",          NA,       NA
)

leer_llamas <- function(ruta) {
  read_excel(ruta, sheet = "viq_todo", range = "A19:Q259") |>
    rename(NRO = Nro) |>
    mutate(
      across(all_of(c(ZOOMETRICAS, "PEVI")), \(x) round(as.numeric(x), 2)),
      VARIE = factor(VARIE, levels = c("KARA", "CHACU"), labels = c("K'ara", "Ch'acu")),
      SEXO = factor(SEXO, levels = c("MACHO", "HEMBRA"), labels = c("Macho", "Hembra")),
      EDAD = factor(EDAD)
    )
}

filtrar_estrato <- function(datos, estrato) {
  datos |>
    filter(is.na(estrato$raza) | VARIE == estrato$raza) |>
    filter(is.na(estrato$sexo) | SEXO == estrato$sexo)
}

particionar <- function(datos, proporcion, semilla) {
  grupos <- droplevels(interaction(datos$VARIE, datos$SEXO, datos$EDAD))
  set.seed(semilla)
  indice <- createDataPartition(grupos, p = proporcion, list = FALSE)
  list(entrenamiento = datos[indice, ], prueba = datos[-indice, ])
}
