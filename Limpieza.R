# LECTURA DE PROMEDIOS DGGI 2021-2025
# Leyendo solo la hoja de promedios de Matemática

library(readxl)
library(stringr)
library(tidyverse)
library(fpp3)
library(here)     

# 1. Ruta del archivo usando here()
archivo <- here("data", "aprendizaje_raw")

if (!file.exists(archivo)) {
  stop("No se encontró el archivo. Verifica que esté en 'data/' dentro del repositorio.")
}

# 2. P_mat — Matemática
raw <- read_excel(archivo, sheet = "P_mat", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_mat",
                skip  = inicio - 1,
                n_max = fin - inicio + 1)

dt_tabla <- dt

names(dt_tabla)[names(dt_tabla) == 'Promedio en Matemática'] <- 'P_mat'
names(dt_tabla)[names(dt_tabla) == 'Año'] <- 'Anio'
names(dt_tabla)[names(dt_tabla) == 'Categoría'] <- 'Categoria'

dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

# Limpieza de columnas extra y formateo de año
dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Anio <- as.integer(dt_tabla$Anio)

# Filtramos y limpiamos los textos con tildes
dt_tabla <- dt_tabla %>%
  filter(!is.na(Anio),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_mat)) %>%
  # Corregimos la "Variable"
  mutate(Variable = str_replace_all(Variable, c("RegiÃ³n" = "Region", "Región" = "Region"))) %>%
  # Corregimos la "Categoria"
  mutate(Categoria = str_replace_all(Categoria, c(
    "GalÃ¡pagos" = "Galapagos", 
    "Galápagos" = "Galapagos",
    "AmazonÃa" = "Amazonia",
    "Amazonía" = "Amazonia",
    "Costa - Galapagos" = "Costa - Galapagos"
  )))

# 3. Ahora creamos el tsibble y guardamos
data_mat <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoria), index = Anio)

nrow(data_mat)
head(data_mat)

# Guardamos el archivo limpio
readr::write_csv(data_mat, "data/data.csv")





























