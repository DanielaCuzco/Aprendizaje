installed.packages("ggrepel")

library(fpp3)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel) 


datos <- read_csv("C:/Users/DELL/Documents/data/data.csv") |>
  as_tsibble(key = c(Sub_nivel, Variable, Categoria), index = Anio)

# Gráfico 1
#En este caso se hizo este gráfico para mostrar la evolución del promedio en Matemática entre los años 2021 y 2025.
#Dado que la base de datos tene varias desagregaciones de los estudiantes (por Región, Sexo, Área y Sostenimiento). 
#Al graficarla tendría muchas líneas que no me dejaría ver algún patrón 
#Por lo que  apliqué un filtro Variable == "Nacional". 
#Esta variable contiene el consolidado general del país para cada subnivel educativo. 


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





# Necesitamos filtrar la variable Nacional porque quiero ver la evolución de los
# subniveles de Bachillerato, Elemental, Medio y Superior, lo que quiero
# es ver estos en general, sin separar por región, sexo o tipo de institución 
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



