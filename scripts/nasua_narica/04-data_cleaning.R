# Herpetología
# Taller 4. Distribución geográfica
# limpieza de datos y combinacion de datasets
# Profesor Héctor Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(CoordinateCleaner) #identifica coordenadas de dudosa procedencia
library(tidyverse)

# species -----------------------------------------------------------------

my_species <-
  'nasua_narica'
# shapefiles --------------------------------------------------------------

costa_rica <-
  read_sf('shapefiles/costa_rica_wgs84.gpkg')

# data --------------------------------------------------------------------

gbif <-
  read_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.rds'))

inat <-
  read_rds(
    paste0(
      'data/raw/',
      my_species,
      '_inat_raw.rds'))

# GBIF

gbif <- #limpiar lo datos que no tengan x, y o año
  gbif %>%
  filter(
    !is.na(x),
    !is.na(y),
    !is.na(year))

# iNaturalist

inat <-
  inat %>%
  filter(
    !is.na(x),
    !is.na(y),
    !is.na(year))

occ <- #ambos docs tienen las mismas columnas con los mismos nombre así que ya se pueden pegar y unir
  bind_rows(
    gbif %>%
      mutate(source = 'GBIF'), #se agrega una columna llamanda fuente
    inat %>%
      mutate(source = 'iNaturalist')) %>%
  filter(year >= 1960) %>% #el profe usa 1960 porqué aquí aparece el museo de la UCR y se empieza a revisar mejor
  distinct(
    species,
    x,
    y,
    .keep_all = TRUE)

cc_clean <-
  clean_coordinates(
    x = occ,
    lon = 'x',
    lat = 'y',
    species = 'species',
    tests = c( #teste que si es flaso o verdadero, buscamos que sea verdadero)
      'zeros',
      'equal',
      'centroids', #ligado al centroide de una cuadrícula, por lo general es un dato erroneo
      'institutions'),
    value = 'spatialvalid')
#se marcaron 0 todos estaban bien

cc_clean %>%
  count(.summary)

occ_clean <-
  cc_clean %>%
  filter(.summary)

# conservar registros dentro de Costa Rica -------------------------------
#
occ_sf <-
  occ_clean %>%
  st_as_sf(
    coords = c('x', 'y'),
    crs = 4326,
    remove = FALSE) %>%
  st_filter(
    costa_rica,
    .predicate = st_within) #eliminar los datos que estén fuera de Costa Rica)
#Se perdieron 10 datos por estar fuera de CR

# save clean data ---------------------------------------------------------

dir.create(
  'data/processed',
  recursive = TRUE)

occ_sf %>%
  write_sf(
    paste0(
      'data/processed/',
      my_species,
      '_occ_clean.gpkg'))
