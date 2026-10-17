# LECTURA DE PROMEDIOS DGGI 2021-2025
# Leyendo solo la hoja de promedios de Matemática

library(readxl)
library(stringr) #da funciones para trabajar con texto: buscar patrones, extraer, reemplazar, etc.
library(tidyverse)
library(fpp3)
library(here)     

archivo <- here("data", "DGGI_20260527_Historico_promedios_2021-2025_vf.xlsx")

if (!file.exists(archivo)) {
  stop("No se encontró el archivo. Verifica que esté en 'data/' dentro del repositorio.")
}


# 1. P_mat — Matemática

raw <- read_excel(archivo, sheet = "P_mat", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_mat",
                skip  = inicio - 1,
                n_max = fin - inicio + 1)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Matemática'] <- 'P_mat'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.integer(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_mat))

data_mat <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_mat)
head(data_mat)






























