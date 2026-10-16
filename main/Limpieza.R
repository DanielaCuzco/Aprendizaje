# LECTURA DE PROMEDIOS DGGI 2021-2025
# 10 asignaturas 

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


# 2. P_lyl — Lengua y Literatura

raw <- read_excel(archivo, sheet = "P_lyl", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_lyl",
                skip  = inicio - 1,
                n_max = fin - inicio + 1)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Lengua y Literatura'] <- 'P_lyl'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.integer(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_lyl))

data_lyl <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_lyl)
head(data_lyl)

# 3. P_icn — Ciencias Naturales

raw <- read_excel(archivo, sheet = "P_icn", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_icn",
                skip  = inicio - 1,
                n_max = fin - inicio + 1)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Ciencias Naturales'] <- 'P_icn'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.integer(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_icn))

data_icn <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_icn)
head(data_icn)




# 4. P_ifis — Física

raw <- read_excel(archivo, sheet = "P_ifis", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_ifis",
                skip  = inicio - 1,
                n_max = fin - inicio + 1)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Física'] <- 'P_ifis'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.integer(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_ifis))

data_ifis <- dt_tabla %>%
  as_tsibblee(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_ifis)
head(data_ifis)




# 5. P_iqui — Química

raw <- read_excel(archivo, sheet = "P_iqui", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_iqui",
                skip  = inicio - 1,
                n_max = fin - inicio + 1)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Química'] <- 'P_iqui'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.integer(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_iqui))

data_iqui <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_iqui)
head(data_iqui)




# 6. P_ibio — Biología

raw <- read_excel(archivo, sheet = "P_ibio", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_ibio",
                skip  = inicio - 1,
                n_max = fin - inicio)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Biología'] <- 'P_ibio'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.integer(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_ibio))

data_ibio <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_ibio)
head(data_ibio)



# 7. P_ihis — Historia

raw <- read_excel(archivo, sheet = "P_ihis", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsxx(archivo, sheet = "P_ihis",
                skip  = inicio - 1,
                n_max = fin - inicio + 1)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Historia'] <- 'P_ihis'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.integer(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_ihis))

data_ihis <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_ihis)
head(data_ihis)




# 8. P_ifil — Filosofía

raw <- read_excel(archivo, sheet = "P_ifil", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_ifil",
                skip  = inicio-1,
                n_max = fin - inicio + 1)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Filosofía'] <- 'P_ifil'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.integer(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_ifil))

data_ifil <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_ifil)
head(data_ifil)




# 9. P_ied — Educación para la Ciudadanía

raw <- read_excel(archivo, sheet = "P_ied", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_ied",
                skip  = inicio - 1,
                n_max = fin - inicio + 1)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Educación para la Ciudadanía'] <- 'P_ied'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.numeric(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_ied))

data_ied <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_ied)
head(data_ied)




# 10. P_ies — Estudios Sociales


raw <- read_excel(archivo, sheet = "P_ies", col_names = FALSE)
inicio <- which(raw[[1]] == "Año")[1]
fin    <- which(str_detect(raw[[1]], "^Fuente:"))[1] - 1

dt <- read_xlsx(archivo, sheet = "P_ies",
                skip  = inicio - 1,
                n_max = fin - inicio + 1)

dt_tabla <- dt
names(dt_tabla)[names(dt_tabla) == 'Promedio en Estudios Sociales'] <- 'P_ies'
dt_tabla <- dt_tabla %>%
  rename(Sub_nivel = any_of(c("Subniveles/ Nivel","Subniveles","Nivel")))

dt_tabla <- dt_tabla[, !names(dt_tabla) %in% c("...10", "Índice")]
dt_tabla$Año <- as.integer(dt_tabla$Año)
dt_tabla <- dt_tabla %>%
  filter(!is.na(Año),
         Sub_nivel %in% c("Bachillerato","Elemental","Medio","Superior"),
         !is.na(P_ies))

data_ies <- dt_tabla %>%
  as_tsibble(key = c(Sub_nivel, Variable, Categoría), index = Año)

nrow(data_ies)
head(data_ies)

