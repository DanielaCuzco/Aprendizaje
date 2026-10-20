installed.packages("ggrepel")

library(fpp3)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggrepel) #Esta librería es para visualizar y que no se superpongan los años que está sobre las líneas del segundo gráfico
library(here)


datos <- read_csv(here("data", "data.csv")) |>
  as_tsibble(key = c(Sub_nivel, Variable, Categoria), index = Anio)

# Gráfico 1
#En este caso se hizo este gráfico para mostrar la evolución del promedio en Matemática entre los años 2021 y 2025.
#Apliqué el filtro sobre la variable "Nacional" para visualizar mejor la evolución sin sus subcategorías
#Mi interés es estudiar el promedio de forma Nacional 


datos |>
  filter(Variable == "Nacional") |>
  autoplot(P_mat) +
  geom_line(linewidth = 1) + # Líneas más gruesas
  geom_point(size = 2) +     # Puntos en los años
  theme_gray() +
  labs(
    title = "Promedio en Matemática por Subnivel en años",
    subtitle = "Filtro: Variable Nacional (2021-2025)",
    x = "Año",
    y = "Promedio (P_mat)",
    color = "Subnivel"
  ) +
  scale_x_continuous(breaks = 2021:2025)



# COMENTARIO GRÁFICO 1
# Tendencia: no hay una tendencia común, los datos por año son bastante diferentes.
# Para el subnivel elemental se puede ver que en año 2022 cae fuerte y se recupera algo hasta
# 2024 y vuelve a caer en 2025, lo que nos dice que para 2025 existe una tendencia decreciente.
# Para el subnivel medio cae en 2022 y luego se mantiene casi estable.
# Para bachillerato baja en 2022 y se recupera en 2025, sin tendencia clara.
# Para el subnivel superior sube hasta 2023 y luego baja, formando una U invertida.
# No hay estacionalidad pues los datos son anuales y no hay ciclo dentro del año.
# Para la etacionariedad por cada subnivel podemos ver:
# - Elemental no es estacionaria pues cambia de nivel en 2022 y caída final.
# - Medio baja en 2022, pero desde ahí se ve estable.
# - Bachillerato es el màs cercano a tener estacionariedad pues se mantiene bastante estable.
# - Superior no es estacionaria pues cambia de nivel en 2023 y luego baja.





# Gráfico 2
# Necesitamos filtrar la variable Nacional porque quiero ver la evolución de los
# subniveles de Bachillerato, Elemental, Medio y Superior
datos_nacional <- datos |>
  filter(Variable == "Nacional") |>
  as_tibble()

# Gráfico 2
ggplot(datos_nacional, aes(x = Sub_nivel, y = P_mat, group = Anio, color = factor(Anio))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  # Etiquetas de los años al final del subnivel "Superior"
  geom_text_repel(
    data = datos_nacional |> filter(Sub_nivel == "Superior"),
    aes(label = Anio),
    hjust = 0, 
    nudge_x = 0.3,
    direction = "y",
    size = 4.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  theme_gray() +
  # Expandimos el eje X para que los años quepan a la derecha
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.2))) +
  theme(legend.position = "none") +
  labs(
    title = "Evolución de los promedios por Subnivel",
    subtitle = "Filtro: Variable Nacional",
    x = "Subnivel Educativo",
    y = "Promedio (P_mat)"
  )

# COMENTARIO GRÁFICO 2
# Tendencia, estacionalidad y estacionariedad no se pueden evaluar aquí pues el eje x
# es categórico, no de tiempo. Sin embargo, podemos ver que en 2021 los promedios son parejos 
# en todos los subniveles, desde 2022 la caída se concentra en Elementaly Medio,
# mientras Bachillerato y Superior se mantienen más estables.