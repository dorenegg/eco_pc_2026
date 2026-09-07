# Herpetología
# Taller 4. Distribución geográfica
# descarga de datos iNaturalist
# Profesor Héctor Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

library(rinat)
library(tidyverse)

# data --------------------------------------------------------------------

query <-
  'Nasua narica'

my_species <-
  'nasua_narica'


#total of observations

inat_metadata <-
  get_inat_obs(
    query = query, # nombre de la especie sin guion
    meta = TRUE) %>%
  pluck('meta')

inat_metadata

# download data

data <-
  get_inat_obs(
    query = query,
    quality = 'research',
    geo = TRUE,
    maxresults = 10000,
    meta = FALSE) %>%
  as_tibble()

inat <- #Revisar que el nombre científico sea igual que el query para no jalar el nombre de otra sp
  data %>%
  filter(
    captive_cultivated == 'false',
    scientific_name == query) %>%
  mutate(
    year = lubridate::year(observed_on)) %>%
  select(
    id,
    species = scientific_name,
    x = longitude,
    y = latitude,
    year,
    uncertainty = positional_accuracy)

names(inat)

# save data ---------------------------------------------------------------

inat %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_inat_raw.rds'))
