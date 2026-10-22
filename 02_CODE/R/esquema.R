COLORES_MEDIDA <- c(
  "Alzadas" = "#2a78d6",
  "Longitudes" = "#eb6834",
  "Diámetros" = "#1baf7a",
  "Perímetros" = "#c98500"
)

leer_foto <- function(archivo) {
  ruta <- file.path("03_RESULTS", "figuras", archivo)
  if (!file.exists(ruta)) stop("No se encontró la fotografía: ", ruta, call. = FALSE)
  foto <- png::readPNG(ruta)
  list(imagen = foto, ancho = dim(foto)[2], alto = dim(foto)[1])
}

agrupar <- function(tabla, grupo) {
  mutate(tabla, grupo = factor(grupo, levels = names(COLORES_MEDIDA)))
}

elipse <- function(x0, y0, a, b, n = 120) {
  angulo <- seq(0, 2 * pi, length.out = n)
  tibble(x = x0 + a * cos(angulo), y = y0 + b * sin(angulo))
}

etiqueta_medida <- function(datos) {
  geom_label(data = datos, aes(x_etiqueta, y_etiqueta, label = clave, colour = grupo), size = 2.5,
             fontface = "bold", linewidth = 0.25, label.padding = unit(0.12, "lines"), fill = "white",
             show.legend = FALSE)
}

graficar_esquema_lateral <- function() {
  foto <- leer_foto("llama_chacu_fotografica.png")
  suelo <- 38
  lineales <- tribble(
    ~clave,  ~grupo,                ~x,   ~y,    ~xend, ~yend, ~x_etiqueta, ~y_etiqueta,
    "ALCRU", "Alzadas",             500,  suelo, 500,   715,   500,         -15,
    "ALAB",  "Alzadas",             760,  suelo, 760,   355,   760,         -15,
    "ALGRU", "Alzadas",             1080, suelo, 1080,  735,   1080,        -15,
    "LARDO", "Longitudes",          500,  790,   1100,  790,   800,         835,
    "LACU",  "Longitudes",          320,  540,   1195,  540,   1300,        540,
    "LAGRU", "Longitudes",          960,  725,   1190,  600,   1290,        690,
    "PROF",  "Diámetros",      560,  715,   560,   365,   650,         470
  ) |>
    mutate(grupo = factor(grupo, levels = names(COLORES_MEDIDA)))
  perimetros <- tribble(
    ~clave,  ~x0, ~y0, ~a, ~b,  ~x_etiqueta, ~y_etiqueta, ~x_guia, ~y_guia,
    "PETO",  540, 540, 32, 178, 250,         420,         508,     470,
    "PEAB",  840, 548, 34, 196, 930,         250,         850,     355,
    "PECAN", 607, 170, 38, 13,  430,         170,         569,     170
  ) |>
    agrupar("Perímetros")
  contornos <- perimetros |>
    mutate(puntos = pmap(list(x0, y0, a, b), elipse)) |>
    select(clave, grupo, puntos) |>
    unnest(puntos)
  flecha <- arrow(length = unit(1.6, "mm"), ends = "both", type = "closed")

  ggplot() +
    annotation_raster(foto$imagen, xmin = 0, xmax = foto$ancho, ymin = 0, ymax = foto$alto, interpolate = TRUE) +
    geom_hline(yintercept = suelo, colour = "grey55", linewidth = 0.3) +
    geom_path(data = contornos, aes(x, y, group = clave, colour = grupo), linewidth = 0.7, linetype = "22") +
    geom_segment(data = lineales, aes(x, y, xend = xend, yend = yend, colour = grupo), arrow = flecha, linewidth = 0.6) +
    geom_segment(data = perimetros, aes(x_etiqueta, y_etiqueta, xend = x_guia, yend = y_guia, colour = grupo), linewidth = 0.3) +
    etiqueta_medida(lineales) +
    etiqueta_medida(perimetros) +
    scale_colour_manual(values = COLORES_MEDIDA, labels = traducir(names(COLORES_MEDIDA)), drop = FALSE, name = NULL) +
    coord_equal(xlim = c(150, foto$ancho), ylim = c(-50, foto$alto), expand = FALSE, clip = "off") +
    theme_void(base_size = 9) +
    theme(legend.position = "bottom", plot.background = element_rect(fill = "white", colour = NA))
}

graficar_esquema_dorsal <- function() {
  foto <- leer_foto("llama_chacu_vista_dorsal_fotografica.png")
  anchos <- tribble(
    ~clave,  ~x,  ~xend, ~y,  ~x_etiqueta, ~y_etiqueta,
    "AMPLI", 335, 690,   836, 512,         920,
    "ANGRU", 340, 705,   276, 522,         360
  ) |>
    agrupar("Diámetros")
  ggplot() +
    annotation_raster(foto$imagen, xmin = 0, xmax = foto$ancho, ymin = 0, ymax = foto$alto, interpolate = TRUE) +
    geom_segment(data = anchos, aes(x, y, xend = xend, yend = y, colour = grupo),
                 arrow = arrow(length = unit(1.4, "mm"), ends = "both", type = "closed"), linewidth = 0.6) +
    etiqueta_medida(anchos) +
    annotate("text", x = foto$ancho / 2, y = -60, label = traducir("Vista dorsal"), size = 2.4, colour = "grey30") +
    scale_colour_manual(values = COLORES_MEDIDA, drop = FALSE, guide = "none") +
    coord_equal(xlim = c(200, 824), ylim = c(-120, foto$alto), expand = FALSE) +
    theme_void() +
    theme(panel.background = element_rect(fill = "white", colour = "grey75", linewidth = 0.3))
}

graficar_esquema_zoometrico <- function() {
  (graficar_esquema_lateral() | graficar_esquema_dorsal()) +
    patchwork::plot_layout(widths = c(3.2, 1), guides = "collect") &
    theme(legend.position = "bottom")
}

# Fig. 2 con fotografía real de dominio público (CC0): "Llama walking on the mountain", Joselodos, Wikimedia Commons
# (01_DATA/foto_cc0/FUENTE.md). Coordenadas en píxeles de la foto recortada (1200 × 846; y desde abajo).
# La llama está en pendiente: las alzadas se trazan hasta una línea de suelo que une los cascos.
graficar_esquema_cc0 <- function() {
  foto <- leer_foto("llama_cc0_lateral.png")
  suelo <- function(x) 72 + (x - 500) * (203 - 72) / (1120 - 500)
  lineales <- tribble(
    ~clave,  ~grupo,       ~x,   ~y,          ~xend, ~yend, ~x_etiqueta, ~y_etiqueta,
    "ALCRU", "Alzadas",    560,  suelo(560),  560,   646,   560,         suelo(560) - 38,
    "ALAB",  "Alzadas",    760,  suelo(760),  760,   368,   760,         suelo(760) - 38,
    "ALGRU", "Alzadas",    1000, suelo(1000), 1000,  736,   1000,        suelo(1000) - 38,
    "LARDO", "Longitudes", 560,  692,         1000,  782,   760,         800,
    "LACU",  "Longitudes", 495,  440,         1062,  548,   1135,        548,
    "LAGRU", "Longitudes", 935,  738,         1055,  600,   1135,        670,
    "PROF",  "Diámetros",  585,  640,         585,   360,   515,         560
  ) |>
    agrupar_v2()
  anchos <- tribble(
    ~clave,  ~x,  ~y,  ~x_etiqueta, ~y_etiqueta,
    "AMPLI", 640, 560, 700,         610,
    "ANGRU", 965, 650, 905,         690
  ) |>
    mutate(grupo = factor("Diámetros", levels = names(COLORES_MEDIDA)))
  perimetros <- tribble(
    ~clave,  ~x0, ~y0, ~a, ~b,  ~x_etiqueta, ~y_etiqueta, ~x_guia, ~y_guia,
    "PETO",  615, 500, 28, 142, 420,         330,         596,     400,
    "PEAB",  800, 538, 32, 166, 860,         300,         808,     376,
    "PECAN", 612, 215, 26, 10,  440,         215,         586,     215
  ) |>
    mutate(grupo = factor("Perímetros", levels = names(COLORES_MEDIDA)))
  contornos <- perimetros |>
    mutate(puntos = pmap(list(x0, y0, a, b), elipse)) |>
    select(clave, grupo, puntos) |>
    unnest(puntos)
  flecha <- arrow(length = unit(1.6, "mm"), ends = "both", type = "closed")

  ggplot() +
    annotation_raster(foto$imagen, xmin = 0, xmax = foto$ancho, ymin = 0, ymax = foto$alto, interpolate = TRUE) +
    annotate("rect", xmin = 0, xmax = foto$ancho, ymin = 0, ymax = foto$alto, fill = "white", alpha = 0.22) +
    annotate("segment", x = 480, xend = 1140, y = suelo(480), yend = suelo(1140), colour = "white", linewidth = 1.2) +
    annotate("segment", x = 480, xend = 1140, y = suelo(480), yend = suelo(1140), colour = "grey25", linewidth = 0.4,
             linetype = "42") +
    geom_path(data = contornos, aes(x, y, group = clave), colour = "white", linewidth = 1.6) +
    geom_path(data = contornos, aes(x, y, group = clave, colour = grupo), linewidth = 0.8, linetype = "22") +
    geom_segment(data = lineales, aes(x, y, xend = xend, yend = yend), colour = "white", linewidth = 1.6) +
    geom_segment(data = lineales, aes(x, y, xend = xend, yend = yend, colour = grupo), arrow = flecha, linewidth = 0.7) +
    geom_segment(data = perimetros, aes(x_etiqueta, y_etiqueta, xend = x_guia, yend = y_guia, colour = grupo), linewidth = 0.35) +
    geom_segment(data = anchos, aes(x_etiqueta, y_etiqueta, xend = x, yend = y, colour = grupo), linewidth = 0.35) +
    geom_point(data = anchos, aes(x, y), shape = 21, size = 4.2, fill = "white", colour = "white", stroke = 1) +
    geom_point(data = anchos, aes(x, y, colour = grupo), shape = 13, size = 3.6, stroke = 0.9) +
    etiqueta_medida(lineales) +
    etiqueta_medida(perimetros) +
    etiqueta_medida(anchos) +
    scale_colour_manual(values = COLORES_MEDIDA, labels = traducir(names(COLORES_MEDIDA)), drop = FALSE, name = NULL) +
    coord_equal(xlim = c(80, foto$ancho), ylim = c(0, foto$alto), expand = FALSE) +
    theme_void(base_size = 9) +
    theme(legend.position = "bottom", plot.background = element_rect(fill = "white", colour = NA))
}

agrupar_v2 <- function(tabla) mutate(tabla, grupo = factor(grupo, levels = names(COLORES_MEDIDA)))

# Fig. 2 con fotografías propias del CIP La Raya (01_DATA/fotos_campo, Excel del Dr. Ibañez; caras difuminadas
# con 02_CODE/python_nested/fotos_campo.py). (a) Medidas sobre una llama del rebaño; (b, c) toma de medidas en campo.
# Coordenadas en píxeles de llama_campo_lateral.png (1104 × 1010; y desde abajo).
graficar_esquema_campo <- function() {
  foto <- leer_foto("llama_campo_lateral.png")
  suelo <- function(x) 125 + (x - 500) * 45 / 320
  lineales <- tribble(
    ~clave,  ~grupo,       ~x,   ~y,          ~xend, ~yend, ~x_etiqueta, ~y_etiqueta,
    "ALCRU", "Alzadas",    455,  suelo(455),  455,   605,   455,         suelo(455) - 45,
    "ALAB",  "Alzadas",    650,  suelo(650),  650,   400,   650,         suelo(650) - 45,
    "ALGRU", "Alzadas",    850,  suelo(850),  850,   605,   850,         suelo(850) - 45,
    "LARDO", "Longitudes", 455,  660,         880,   660,   668,         700,
    "LACU",  "Longitudes", 432,  478,         908,   528,   1010,        480,
    "LAGRU", "Longitudes", 795,  640,         912,   570,   1010,        650,
    "PROF",  "Diámetros",  488,  600,         488,   432,   360,         610
  ) |>
    agrupar_v2()
  anchos <- tribble(
    ~clave,  ~x,  ~y,  ~x_etiqueta, ~y_etiqueta,
    "AMPLI", 540, 545, 600,         580,
    "ANGRU", 810, 535, 1010,        395
  ) |>
    mutate(grupo = factor("Diámetros", levels = names(COLORES_MEDIDA)))
  perimetros <- tribble(
    ~clave,  ~x0, ~y0, ~a, ~b,  ~x_etiqueta, ~y_etiqueta, ~x_guia, ~y_guia,
    "PETO",  515, 515, 20, 86,  380,         380,         497,     470,
    "PEAB",  690, 505, 22, 104, 760,         330,         700,     405,
    "PECAN", 528, 235, 22, 8,   370,         235,         506,     235
  ) |>
    mutate(grupo = factor("Perímetros", levels = names(COLORES_MEDIDA)))
  contornos <- perimetros |>
    mutate(puntos = pmap(list(x0, y0, a, b), elipse)) |>
    select(clave, grupo, puntos) |>
    unnest(puntos)
  flecha <- arrow(length = unit(1.4, "mm"), ends = "both", type = "closed")

  a <- ggplot() +
    annotation_raster(foto$imagen, xmin = 0, xmax = foto$ancho, ymin = 0, ymax = foto$alto, interpolate = TRUE) +
    annotate("segment", x = 400, xend = 960, y = suelo(400), yend = suelo(960), colour = "white", linewidth = 1.2) +
    annotate("segment", x = 400, xend = 960, y = suelo(400), yend = suelo(960), colour = "grey20", linewidth = 0.4,
             linetype = "42") +
    geom_path(data = contornos, aes(x, y, group = clave), colour = "white", linewidth = 1.5) +
    geom_path(data = contornos, aes(x, y, group = clave, colour = grupo), linewidth = 0.75, linetype = "22") +
    geom_segment(data = lineales, aes(x, y, xend = xend, yend = yend), colour = "white", linewidth = 1.5) +
    geom_segment(data = lineales, aes(x, y, xend = xend, yend = yend, colour = grupo), arrow = flecha, linewidth = 0.65) +
    geom_segment(data = perimetros, aes(x_etiqueta, y_etiqueta, xend = x_guia, yend = y_guia, colour = grupo), linewidth = 0.35) +
    geom_segment(data = anchos, aes(x_etiqueta, y_etiqueta, xend = x, yend = y, colour = grupo), linewidth = 0.35) +
    geom_point(data = anchos, aes(x, y), shape = 21, size = 4, fill = "white", colour = "white", stroke = 1) +
    geom_point(data = anchos, aes(x, y, colour = grupo), shape = 13, size = 3.4, stroke = 0.9) +
    etiqueta_medida(lineales) +
    etiqueta_medida(perimetros) +
    etiqueta_medida(anchos) +
    scale_colour_manual(values = COLORES_MEDIDA, labels = traducir(names(COLORES_MEDIDA)), drop = FALSE, name = NULL) +
    coord_equal(xlim = c(0, foto$ancho), ylim = c(0, foto$alto), expand = FALSE) +
    labs(tag = "a") +
    theme_void(base_size = 9) +
    theme(legend.position = "bottom", plot.tag = element_text(face = "bold", size = 11))

  panel_foto <- function(archivo, letra) {
    f <- leer_foto(archivo)
    ggplot() +
      annotation_raster(f$imagen, xmin = 0, xmax = f$ancho, ymin = 0, ymax = f$alto, interpolate = TRUE) +
      coord_equal(xlim = c(0, f$ancho), ylim = c(0, f$alto), expand = FALSE) +
      labs(tag = letra) +
      theme_void() +
      theme(plot.tag = element_text(face = "bold", size = 11))
  }
  (a | (panel_foto("campo_baston.png", "b") / panel_foto("campo_cinta.png", "c"))) +
    patchwork::plot_layout(widths = c(1.35, 1), guides = "collect") &
    theme(legend.position = "bottom")
}
