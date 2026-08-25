# Ecologia de poblaciones y comunidades
# Codigo 01a Lectura de datos ordenados
# Profesor Hector Zumbado Ulate

# setup ----------------------------------------------------------

rm(list = ls())

library(gt) #hace cuadros
library(tidyverse) #analizar datos; confictos, hay otros factores que tienen la misma función

#Objetivo: organizar data sets que tengan defectos

# data -------------------------------------------------------------
#phy

read_csv('data/raw/messy_weather.csv') %>% #shr: texto; dbdl: numérico
  view()

read_csv('data/raw/messy_weather.csv') %>%
  lobstr::ref() #nos muestra donde se guarda la info en el disco duro y cómo está organizada

# fixing data -------------------------------------------------

# Read in and assign the data:

messy_weather_long <-
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer( #agragar más fila; pivot_wider: agragar más columnas
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_')



messy_weather_long %>%
  view() #nos permite ver qué valores pose cada variable y así identificar valores

#shift+ctrl+m
messy_weather_long %>%
  distinct(checked) #si es una observación que ya se verificó

unique(messy_weather_long$checked)


messy_weather_long %>%
  distinct(state) #en qué estado se realizó la observación climática

unique(messy_weather_long$state)


messy_weather_long %>%
  distinct(variable) #variables que se tomaron, temperatura min y max deberían estar separadp

unique(messy_weather_long$variable)

messy_weather_long %>%
  distinct(Station) #código de cada estación

unique(messy_weather_long$Station)

messy_weather_long %>%
  distinct(Name) #nombre de la estación
unique(messy_weather_long$Name)

# fix names ---------------------------------------------------------------

messy_weather_long %>%
  select( #nos permite cambiar el nombre y reacomodar para ver mejor los datos
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value)

weather_names_fix <-
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer(
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_') %>%
  select(
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value)

weather_names_fix %>%
  view()

# fix variables -----------------------------------------------------------

weather_names_fix %>%
  pivot_wider(
    names_from = 'variable',
    values_from = 'value')

weather_variable_fix <-
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer(
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_') %>%
  select(
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value) %>%
  pivot_wider(
    names_from = 'variable',
    values_from = 'value') %>%
  rename(precipitation = precip) #La precipitación y nieve indica que es un caracter, pasar a dbl

weather_variable_fix %>%
  view()

# create date -------------------------------------------------------------

weather_variable_fix %>%
  unite(
    col = 'date', #new column; unir el año y el mes en una sola columna
    year:day,
    sep = '-') %>%
  mutate(date = as_date(date)) #indicar que el una fecha y no un chr, trabaj solo si los datos está acomodados ano-mes-día; con un paquete llamado libridate se pueden hacer gráfico extrayendo fechas

weather_date_fix <-
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer(
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_') %>%
  select(
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value) %>%
  pivot_wider(
    names_from = variable) %>%
  rename(precipitation = precip) %>%
  unite( #para unir coulumnas
    col = 'date',
    year:day,
    sep = '-') %>%
  mutate(date = as_date(date))

weather_date_fix

# fix temp ----------------------------------------------------------------
names(weather_date_fix)

weather_date_fix %>%
  select(temperature_min_max) #las temperatura están divididas por dos puntos

weather_date_fix %>%
  select(temperature_min_max) %>%
  separate( #separa la columna
    temperature_min_max, #columna a separar
    into = c('temperature_min', 'temperature_max'),
    sep = ':')

weather_fix_temp <- #agragar a la base que ya tenemos la corrección de temperatura
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer(
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_') %>%
  select(
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value) %>%
  pivot_wider(
    names_from = variable) %>%
  rename(precipitation = precip) %>%
  unite(
    col = 'date',
    year:day,
    sep = '-') %>%
  mutate(date = as_date(date)) %>%
  separate(
    temperature_min_max,
    into = c('temperature_min', 'temperature_max'),
    sep = ':')

weather_fix_temp

# fix values --------------------------------------------------------------

weather_fix_temp %>%
  pivot_longer( #hacer mpás final
    cols = precipitation:temperature_max, #a partir de estas columnas, juntar los datos en una nueva clumna llamado value
    names_to = 'variable',
    values_to = 'value') %>%
  filter(is.na(value)) #con esto observamos los valores de NA que nos pueden afectar en nuestros cálculos, a veces se hace una columna con estos valores

weather_fix_temp %>%
  mutate(
    precipitation = as.numeric(precipitation),
    snow = as.numeric(snow),
    temperature_min = as.numeric(temperature_min),
    temperature_max = as.numeric(temperature_max))

# much better to use across

weather_fix_temp %>%
  mutate(
    across( # en lugar de poner en mutate 4 veces lo mismo, le dice que haga 4 veces lo mismo para transformar de chr a dbl desde precipitación a tem_max de forma más rápida
      precipitation:temperature_max,
      ~ as.numeric(.x)))

weather_value_temp <-
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer(
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_') %>%
  select(
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value) %>%
  pivot_wider(
    names_from = variable) %>%
  rename(precipitation = precip) %>%
  unite(
    col = 'date',
    year:day,
    sep = '-') %>%
  mutate(date = as_date(date)) %>%
  separate(
    temperature_min_max,
    into = c('temperature_min', 'temperature_max'),
    sep = ':') %>%
  mutate(
    across(
      precipitation:temperature_max,
      ~ as.numeric(.x)))

weather_value_temp

# output/results ----------------------------------------------------------

# grafico para compararar valores de Temperatura

weather_value_temp %>%
  group_by(name) %>% #permite hacer una operación combinada, de porma tal que, en este caso se calculen los valores promedios para cadas grupo, en este caso cada grupo es cada estación
  summarise(
    temperature_min =
      mean(temperature_min, na.rm = TRUE),
    temperature_max =
      mean(temperature_max, na.rm = TRUE)) %>%
  ggplot(
    aes(
      x = reorder(name, temperature_max), #ordena de la max a la mínima, parque el que tenga temps más altas salta de primero en el gráfico
      ymin = temperature_min,
      ymax = temperature_max
    )) +
  geom_linerange(linewidth = 1) +
  geom_point(
    aes(y = temperature_min),
    size = 3,
    shape = 21,
    fill = 'white') +
  geom_point(
    aes(y = temperature_max),
    size = 3) +
  coord_flip() +
  labs(
    x = NULL,
    y = 'Mean temperature (°C)') +
  theme_classic()

ggsave(
  'output/figures/temperature.tiff',
  width = 180,
  height = 120,
  units = 'mm',
  dpi = 300,
  compression = 'lzw',
  bg = 'white') #resolución mínima t dimesiones adecuadas para un revista

## cuadro para comparar valores de variables

weather_value_temp %>%
  group_by(name, state, elevation) %>%
  summarise(
    precipitation =
      mean(precipitation, na.rm = TRUE),
    snow =
      mean(snow, na.rm = TRUE),
    temperature_min =
      mean(temperature_min, na.rm = TRUE),
    temperature_max =
      mean(temperature_max, na.rm = TRUE)) %>%
  ungroup() %>%
  gt() %>%
  tab_header(
    title = md(
      '**Mean climatic conditions recorded at five weather stations during March (2010–2020).**' #en negrita ** text**
    )) %>%
  cols_label(
    name = 'Station',
    state = 'State',
    elevation = 'Elevation (m)',
    precipitation = 'Precipitation (mm)',
    snow = 'Snow (mm)',
    temperature_min = 'Min. temp. (°C)',
    temperature_max = 'Max. temp. (°C)') %>%
  fmt_number(
    columns = elevation:temperature_max,
    decimals = 1) %>%
  opt_row_striping()

# individual tables -------------------------------------------------------

# there are two different dataframes in this dataset
# station data
# weather data

unique(weather_value_temp$station)

weather_value_temp %>%
  distinct(
    station,
    x,
    y,
    elevation,
    state,
    name) #hay una repetición de datos, los cuales están ligados a la estación y no a las variablea ambientales, por lo que se pueden hacer dos data sets

weather_value_temp %>%
  select(station:name) %>%
  distinct()

# Generate a station-level data frame and assign to the name `stations`:

stations <- #infirmación de la estación
  weather_value_temp %>%
  select(station:name) %>%
  distinct()

stations

# Generate a observation-level data frame and assign to the name
# `observations`:

observations <- #observaciones metereológicas
  weather_value_temp %>%
  select(!x:name) #todo lo que había antes menor X a name

observations

stations %>%
  inner_join(observations) #aquí se puede volver a unir el dataset, siempre y cuando se tenga una misma columana para los datos

# create a list to save ---------------------------------------------------

list(stations, observations) #crear una lista con dos data sets

list(
  stations =
    weather_value_temp %>%
    select(station:name) %>%
    distinct(),

  observations =
    weather_value_temp %>%
    select(!x:name)) %>%

  # Write to file:

  write_rds('data/processed/weather_tidy.rds')
