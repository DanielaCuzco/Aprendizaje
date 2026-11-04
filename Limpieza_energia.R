library(fpp3)
library(tidyverse)
library(here)
library(readr)

here::i_am("Limpieza_energia.R")

energia<- read.csv(here("data", "energy_raw.csv"))


energia <- energia %>%
  mutate(time = ymd_hms(time)) |>
  rename(
    # Tiempo y Demanda
    fecha_hora = time,
    demanda_real = `total.load.actual`,
    demanda_prevista = `total.load.forecast`,
    
    precio_real = `price.actual`,
    precio_previsto = `price.day.ahead`,
    
    # Generación - Renovables
    gen_biomasa = `generation.biomass`,
    gen_geotermica = `generation.geothermal`,
    gen_hidro_fluyente = `generation.hydro.run.of.river.and.poundage`,
    gen_hidro_embalse = `generation.hydro.water.reservoir`,
    gen_marina = `generation.marine`,
    gen_solar = `generation.solar`,
    gen_eolica_terrestre = `generation.wind.onshore`,
    gen_eolica_marina = `generation.wind.offshore`,
    gen_residuos = `generation.waste`,
    gen_otras_renovables = `generation.other.renewable`,
    
    # Generación - No Renovables
    gen_nuclear = `generation.nuclear`,
    gen_gas = `generation.fossil.gas`,
    gen_carbon = `generation.fossil.hard.coal`,
    gen_lignito = `generation.fossil.brown.coal.lignite`,
    gen_petroleo = `generation.fossil.oil`,
    gen_esquisto = `generation.fossil.oil.shale`,
    gen_turba = `generation.fossil.peat`,
    gen_gas_carbon = `generation.fossil.coal.derived.gas`,
    gen_otras = `generation.other`,
    
    # Almacenamiento
    gen_bombeo_agregado = `generation.hydro.pumped.storage.aggregated`,
    consumo_bombeo = `generation.hydro.pumped.storage.consumption`,
    
    # Previsiones de generación
    prev_solar = `forecast.solar.day.ahead`,
    prev_eolica_terrestre = `forecast.wind.onshore.day.ahead`,
    prev_eolica_marina = `forecast.wind.offshore.eday.ahead`
  )

energia
  
  
#Tsibble - indice fecha y hora
energia_ts <- energia %>%
  as_tsibble(index = fecha_hora)

energia_ts

class(energia_ts)


head(energia_ts)

# Guardar la base de datos limpia 
write_csv(energia, here("data", "data_energia.csv"))


