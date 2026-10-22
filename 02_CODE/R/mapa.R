CRS_UTM <- 32719

ETIQUETAS_LINEA <- c(
  "Río" = "#3b82c4",
  "Carretera nacional" = "#4a4a4a",
  "Ferrocarril Cusco–Puno" = "#111111",
  "Límite de Melgar" = "#1c5cab"
)

cargar_limites <- function(nivel, ruta = "01_DATA/datos_geo") {
  sf::st_as_sf(geodata::gadm("PER", level = nivel, path = ruta))
}

leer_osm <- function(capa, ruta = "01_DATA/datos_geo") {
  elementos <- jsonlite::fromJSON(file.path(ruta, paste0("osm_", capa, ".json")), simplifyVector = FALSE)$elements
  etiqueta <- \(e, clave) e$tags[[clave]] %||% NA_character_
  if (capa == "lugares") {
    return(sf::st_as_sf(
      tibble(
        nombre = map_chr(elementos, etiqueta, "name"),
        tipo = map_chr(elementos, etiqueta, "place"),
        lon = map_dbl(elementos, "lon"),
        lat = map_dbl(elementos, "lat")
      ),
      coords = c("lon", "lat"), crs = 4326
    ))
  }
  lineas <- map(elementos, \(e) sf::st_linestring(do.call(rbind, map(e$geometry, \(p) c(p$lon, p$lat)))))
  sf::st_sf(
    nombre = map_chr(elementos, etiqueta, "name"),
    ref = map_chr(elementos, etiqueta, "ref"),
    geometry = sf::st_sfc(lineas, crs = 4326)
  )
}

preparar_dem <- function(caja, ruta = "01_DATA/datos_geo") {
  archivo <- file.path(ruta, "dem_melgar_utm.tif")
  if (!file.exists(archivo)) {
    teselas <- map(c(-14.5, -15.2), \(lat) geodata::elevation_3s(lon = -70.6, lat = lat, path = ruta))
    dem <- terra::crop(do.call(terra::merge, teselas), terra::ext(caja[c("xmin", "xmax", "ymin", "ymax")]))
    dem <- terra::project(dem, paste0("EPSG:", CRS_UTM), res = 90, method = "bilinear")
    terra::writeRaster(dem, archivo, overwrite = TRUE)
  }
  archivo
}

procesar_relieve_qgis <- function(archivo_dem, qgis, ruta = "01_DATA/datos_geo") {
  sombra <- file.path(ruta, "sombra_melgar_qgis.tif")
  curvas <- file.path(ruta, "curvas_melgar_qgis.gpkg")
  if (!file.exists(sombra) || !file.exists(curvas)) {
    options(qgisprocess.path = qgis)
    qgisprocess::qgis_configure(quiet = TRUE)
    qgisprocess::qgis_run_algorithm("native:hillshade", INPUT = archivo_dem, Z_FACTOR = 1.6,
                                    AZIMUTH = 315, V_ANGLE = 40, OUTPUT = sombra, .quiet = TRUE)
    qgisprocess::qgis_run_algorithm("gdal:contour", INPUT = archivo_dem, BAND = 1, INTERVAL = 200,
                                    FIELD_NAME = "ELEV", OUTPUT = curvas, .quiet = TRUE)
  }
  list(
    elevacion = terra::rast(archivo_dem),
    sombra = terra::rast(sombra),
    curvas = sf::st_read(curvas, quiet = TRUE)
  )
}

formatear_gms <- function(valor, positivo, negativo) {
  hemisferio <- ifelse(valor >= 0, positivo, negativo)
  valor <- abs(valor)
  grados <- floor(valor)
  minutos <- floor((valor - grados) * 60)
  segundos <- round(((valor - grados) * 60 - minutos) * 60)
  sprintf("%d° %02d′ %02d″ %s", grados, minutos, segundos, hemisferio)
}

tema_mapa <- function() {
  theme_void(base_size = 8) +
    theme(
      panel.border = element_rect(fill = NA, colour = "grey40", linewidth = 0.4),
      plot.tag = element_text(face = "bold", size = 10),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

mapa_continente <- function(peru) {
  sudamerica <- rnaturalearth::ne_countries(scale = "medium", continent = "South America", returnclass = "sf")
  ggplot() +
    geom_sf(data = sudamerica, fill = "#f2f2f2", colour = "grey60", linewidth = 0.15) +
    geom_sf(data = peru, fill = "#cde2fb", colour = "#2a78d6", linewidth = 0.3) +
    coord_sf(xlim = c(-82, -34), ylim = c(-56, 13), expand = FALSE) +
    labs(tag = "a") +
    tema_mapa() +
    theme(panel.background = element_rect(fill = "#eef4fb", colour = NA))
}

mapa_pais <- function(departamentos, puno, caja) {
  ggplot() +
    geom_sf(data = departamentos, fill = "#f7f7f5", colour = "grey65", linewidth = 0.15) +
    geom_sf(data = puno, fill = "#eb6834", colour = "#a8441d", linewidth = 0.35, alpha = 0.85) +
    geom_sf(data = sf::st_as_sfc(caja), fill = NA, colour = "grey10", linewidth = 0.45) +
    geom_sf_text(data = puno, aes(label = "Puno"), size = 2.4, fontface = "bold", colour = "grey10",
                 nudge_x = -2.2, nudge_y = -0.6) +
    coord_sf(expand = FALSE) +
    labs(tag = "b") +
    tema_mapa() +
    theme(panel.background = element_rect(fill = "#eef4fb", colour = NA))
}

mapa_detalle <- function(provincias, distritos, caja, sitio, qgis) {
  melgar <- filter(provincias, NAME_1 == "Puno", NAME_2 == "Melgar")
  relieve <- procesar_relieve_qgis(preparar_dem(caja), qgis)
  marco <- sf::st_transform(sf::st_as_sfc(caja), CRS_UTM)
  recortar <- \(capa) suppressWarnings(sf::st_intersection(sf::st_transform(capa, CRS_UTM), marco))

  distritos_utm <- recortar(distritos)
  santa_rosa <- filter(distritos_utm, NAME_2 == "Melgar", NAME_3 == "Santa Rosa")
  lineas <- bind_rows(
    mutate(select(recortar(leer_osm("rios")), nombre), capa = "Río"),
    mutate(select(recortar(filter(leer_osm("vias"), grepl("^PE-", ref))), nombre), capa = "Carretera nacional"),
    mutate(select(recortar(leer_osm("tren")), nombre), capa = "Ferrocarril Cusco–Puno"),
    mutate(transmute(sf::st_cast(recortar(melgar), "MULTILINESTRING"), nombre = NAME_2), capa = "Límite de Melgar")
  ) |>
    mutate(capa = factor(capa, levels = names(ETIQUETAS_LINEA)))
  melgar_utm <- sf::st_transform(melgar, CRS_UTM)
  pueblos <- recortar(leer_osm("lugares")) |>
    filter(tipo == "town" | nombre %in% c("Santa Rosa", "Macarí", "Umachiri", "Llalli", "Cupi")) |>
    filter(!duplicated(nombre), lengths(sf::st_intersects(geometry, melgar_utm)) > 0)
  punto <- sf::st_transform(
    sf::st_as_sf(data.frame(x = sitio$longitud, y = sitio$latitud), coords = c("x", "y"), crs = 4326),
    CRS_UTM
  )
  punto$rotulo <- sprintf("%s\n%s\n%s\n%s m", sitio$nombre, formatear_gms(sitio$latitud, "N", "S"),
                          formatear_gms(sitio$longitud, "E", if (idioma_ingles()) "W" else "O"), sitio$altitud)

  ggplot() +
    tidyterra::geom_spatraster(data = relieve$sombra, maxcell = 2e6, show.legend = FALSE) +
    scale_fill_gradient(low = "grey30", high = "white", na.value = NA, guide = "none") +
    ggnewscale::new_scale_fill() +
    tidyterra::geom_spatraster(data = relieve$elevacion, maxcell = 2e6, alpha = 0.55) +
    tidyterra::scale_fill_hypso_c(palette = "dem_poster", name = traducir("Altitud (m)"), na.value = NA,
                                  breaks = seq(3800, 5600, 400),
                                  guide = guide_colourbar(barheight = unit(24, "mm"), barwidth = unit(3, "mm"), order = 2)) +
    geom_sf(data = relieve$curvas, colour = "grey25", linewidth = 0.12, alpha = 0.35) +
    geom_sf(data = distritos_utm, fill = NA, colour = "grey35", linewidth = 0.2, linetype = "dotted") +
    geom_sf(data = santa_rosa, fill = "#eb6834", colour = "#a8441d", alpha = 0.15, linewidth = 0.45) +
    geom_sf(data = lineas, aes(colour = capa, linetype = capa, linewidth = capa), key_glyph = "path") +
    scale_colour_manual(values = ETIQUETAS_LINEA, labels = traducir(names(ETIQUETAS_LINEA)), name = NULL) +
    scale_linetype_manual(values = c("solid", "solid", "22", "solid"), labels = traducir(names(ETIQUETAS_LINEA)), name = NULL) +
    scale_linewidth_manual(values = c(0.35, 0.5, 0.45, 0.8), labels = traducir(names(ETIQUETAS_LINEA)), name = NULL) +
    geom_sf(data = pueblos, shape = 21, size = 1.6, fill = "white", colour = "grey10", stroke = 0.5) +
    ggrepel::geom_text_repel(data = pueblos, aes(label = nombre, geometry = geometry), stat = "sf_coordinates",
                             size = 2.3, colour = "grey10", fontface = "italic", min.segment.length = 0.2,
                             bg.color = "white", bg.r = 0.12, seed = 2026) +
    geom_sf(data = punto, shape = 24, size = 3.4, fill = "#d22c2c", colour = "white", stroke = 0.8) +
    ggrepel::geom_label_repel(data = punto, aes(label = rotulo, geometry = geometry), stat = "sf_coordinates",
                              size = 2.2, lineheight = 0.9, fill = alpha("white", 0.92), label.size = 0.2,
                              nudge_x = 16000, nudge_y = 14000, min.segment.length = 0, seed = 2026) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.6, line_width = 0.4,
                                bar_cols = c("grey15", "white")) +
    ggspatial::annotation_north_arrow(location = "tr", height = unit(9, "mm"), width = unit(7, "mm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(text_size = 6)) +
    coord_sf(crs = CRS_UTM, datum = 4326, expand = FALSE) +
    labs(tag = "c", x = NULL, y = NULL) +
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 1), linewidth = guide_legend(order = 1)) +
    theme_bw(base_size = 8) +
    theme(
      panel.grid = element_line(colour = "grey70", linewidth = 0.15, linetype = "dotted"),
      plot.tag = element_text(face = "bold", size = 10),
      legend.position = "right",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6.5),
      legend.key.width = unit(6, "mm")
    )
}

figura_area_estudio <- function(config, contexto = TRUE) {
  departamentos <- cargar_limites(1)
  provincias <- cargar_limites(2)
  distritos <- cargar_limites(3)
  peru <- sf::st_union(departamentos)
  puno <- filter(departamentos, NAME_1 == "Puno")
  melgar <- filter(provincias, NAME_1 == "Puno", NAME_2 == "Melgar")
  caja <- sf::st_bbox(sf::st_buffer(melgar, 3000))
  detalle <- mapa_detalle(provincias, distritos, caja, config$sitio, config$qgis)
  if (!contexto) return(detalle + labs(tag = NULL))
  izquierda <- mapa_continente(peru) / mapa_pais(departamentos, puno, caja)
  (izquierda | detalle) +
    patchwork::plot_layout(widths = c(1, 2.6))
}
