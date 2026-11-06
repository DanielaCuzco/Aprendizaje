#aquí debo trabajar con la base de datos de Dake y hacer la descomposición de la serie temporal 

library(fpp3)
library(slider)
library(lubridate)
library(readr)
library(zoo)

energia <- read_csv(
  "data/data_energia.csv",
  col_types = cols(
    fecha_hora = col_character()
  ),
  show_col_types = FALSE
)

class(energia$fecha_hora)
head(energia$fecha_hora)

head(energia)
class(energia)

# Convertir la variable temporal
energia <- energia |>
  mutate(
    fecha_hora = as.POSIXct(
      fecha_hora,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
  ) |>
  arrange(fecha_hora)


#conversion a tsibble de las columnas fecha hora, demanda real
energia_ts <- energia |>
  select(fecha_hora, demanda_real) |>
  as_tsibble(index = fecha_hora)

head(energia_ts)

#interpolación para completar los datos faltantes de la demanda real
energia_ts <- energia_ts |>
  mutate(
    demanda_real = zoo::na.approx(
      demanda_real,
      x = as.numeric(fecha_hora),
      na.rm = FALSE, 
      rule = 2
    )
  )

#visualizacion de datos
energia_ts |>
  autoplot(demanda_real) +
  labs(
    y = "Demanda real",
    x = "Fecha",
    title = "Demanda real de energía eléctrica (2015-2018)"
  )
#hay DEMASIADOS DATOS, por lo que es mejor filtrar por año 

#base de datos de los dos últimos años 
energia_2018 <- energia_ts |>
  filter(year(fecha_hora) == 2018)

energia_ts_da <- energia_2018 |>
  select(fecha_hora, demanda_real) |>
  as_tsibble(index = fecha_hora)


#visualizacion de nuevos datos 
energia_ts_da |> 
  autoplot(demanda_real) + 
  labs( y = "Demanda real 2018",
        x = "Fecha",
        title = "Demanda real de energía electrica en 2018")

#medias moviles
energia_ma <- energia_ts_da |>
  mutate(
    `24-MA` = slider::slide_dbl(
      demanda_real,
      mean,
      .before = 11,
      .after = 12, #pues son 24 horas por día 
      .complete = TRUE
    ),
    `2x24-MA` = slider::slide_dbl(
      `24-MA`,
      mean,
      .before = 1, #para corregir la asimetría 
      .after = 0,
      .complete = TRUE
    )
  )

#grafica de media movil 
energia_ma |>
  autoplot(demanda_real, colour = "gray") +
  geom_line(
    aes(y = `2x24-MA`),
    colour = "#D55E00",
    linewidth = 0.7
  ) +
  labs(
    y = "Demanda real",
    x = "Fecha",
    title = "Demanda real y media móvil centrada de 24 horas"
  )


#DESCOMPOSICION ADITIVA: Yt = St + Tt + Rt
descomposicion_energia <- energia_ts_da |>
  model(
    classical_decomposition(
      demanda_real ~ season(period = 24),
      type = "additive"
    )
  ) |>
  components()

# Visualización completa 
descomposicion_energia |>
  autoplot() +
  labs(
    title = "Descomposición aditiva de la demanda de energía (2018)",
    y = "Demanda real",
    x = "Fecha"
  )


#COMPONENTES EXPLÍCITOS

#tendencia
energia_ma_tend <- energia_ma |>
  mutate(
    tendencia = `2x24-MA`
  )

#periodo
energia_pt <- energia_ma_tend |>
  mutate(
    pt = demanda_real - tendencia,
    hora = lubridate::hour(fecha_hora)
  )

#promedio 
energia_estacional <- energia_pt |>
  as_tibble() |>
  group_by(hora) |>
  summarise(
    media = mean(pt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    estacional = media - mean(media)
  )

#inclusión de resido
energia_componentes <- energia_pt |>
  as_tibble() |>
  left_join(
    energia_estacional |>
      select(hora, estacional),
    by = "hora",
    relationship = "many-to-one"
  ) |>
  mutate(
    residuo = demanda_real - tendencia - estacional
  ) |>
  as_tsibble(index = fecha_hora)

#Reconstrucción de la serie
energia_all_componentes <- energia_componentes |>
  mutate(
    demanda_reconstruida = tendencia + estacional + residuo
  )

#Graficas de las compnentes 

#grafica de la tendencia
energia_all_componentes |>
  autoplot(tendencia) +
  labs(
    title = "Tendencia-ciclo de la demanda eléctrica",
    x = "Fecha",
    y = "Demanda real"
  )

#grafica de la estacionalidad
energia_all_componentes |>
  autoplot(estacional) +
  labs(
    title = "Estacionalidad de la demanda eléctrica",
    x = "Fecha",
    y = "Demanda real"
  )

#grafica del residuo 
energia_all_componentes |>
  autoplot(residuo) +
  labs(
    title = "Tendencia-ciclo de la demanda eléctrica",
    x = "Fecha",
    y = "Demanda real"
  )
