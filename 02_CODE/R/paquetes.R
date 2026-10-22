paquetes <- c(
  "readxl", "dplyr", "tidyr", "purrr", "tibble", "ggplot2", "caret", "kernlab",
  "randomForest", "rpart", "nnet", "xgboost", "Boruta", "corrplot",
  "writexl", "doParallel", "glmnet", "pls", "patchwork", "sf", "geodata",
  "ggspatial", "rnaturalearth", "rnaturalearthdata", "terra", "tidyterra", "ggnewscale", "png", "qgisprocess", "ggrepel", "jsonlite"
)

faltantes <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltantes) > 0) install.packages(faltantes, repos = "https://cloud.r-project.org")

for (paquete in c("readxl", "dplyr", "tidyr", "purrr", "tibble", "ggplot2",
                  "caret", "Boruta", "corrplot", "writexl", "doParallel", "patchwork")) {
  suppressPackageStartupMessages(library(paquete, character.only = TRUE))
}
