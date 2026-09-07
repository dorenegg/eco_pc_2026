# Herpetología
# Taller 4. Distribución geográfica
# descarga de datos gbif
# Profesor Héctor Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

library(rgbif)
library(janitor)
library(tidyverse)

# data --------------------------------------------------------------------

species <-
  'Nasua narica'

my_species <-
  'nasua_narica'


key <- #cada especies en GBIF astá asociado a un código llamado key
  name_backbone(species) %>%
  pull(usageKey)

key


gbif_download <-
  occ_download(
    pred_in("taxonKey", key),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    format = "SIMPLE_CSV",
    user = 'dorenegg',
    pwd = 'Chifrijo32',
    email = 'dorenegonza@gmail.com') # important to use pred_in

gbif_download #importante guarda esta información para efecto de publicación

# save citation -----------------------------------------------------------

# para salvar metadata

dir.create(
  'output/other',
  recursive = TRUE)

gbif_download %>% #manejo de datos paste0, pegar sin espacios,permite trabajar con más fácil en R ya que los espacios alteran el código
  write_rds(
    paste0(
      'output/other/',
      my_species,
      '_key.rds'))

# leer metadata

# gbif_download <-
#   read_rds(
#   paste0(
#     'output/other/',
#     my_species,
#     '_key.rds'))

# check download processing -----------------------------------------------

# ver avance de la descarga en datasets grandes

occ_download_wait(gbif_download)

# crear objeto de descarga

data <- #genera un archivo zip descarga enn la computadora con toda la información anterior
  occ_download_get(
    gbif_download,
    path = 'output/other',
    overwrite = TRUE) %>%
  occ_download_import()

# data <-
#   occ_download_get(
#     '0011429-260226173443078',
#     path = 'data/raw',
#     overwrite = TRUE)

gbif <- #filtrado de la información que se necesita
  data %>%
  filter(
    occurrenceStatus == "PRESENT",
    basisOfRecord %in% c(
      "HUMAN_OBSERVATION",
      "MACHINE_OBSERVATION",
      "PRESERVED_SPECIMEN")) %>%
  select(
    id = gbifID,
    species,
    x = decimalLongitude,
    y = decimalLatitude,
    year,
    uncertainty = coordinateUncertaintyInMeters)

# save data ---------------------------------------------------------------

# csv

dir.create(
  'data/raw',
  recursive = TRUE)

gbif %>% #salvando el archivo dentro de paste0 llamado especie_x_gbif-zip
  clean_names() %>%
  write_csv(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.csv'))

# chequear archivo guardado

read_csv(
  paste0(
    'data/raw/',
    my_species,
    '_gbif_raw.csv'))

# rds

gbif %>%
  clean_names() %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.rds'))

# chequear archivo guardado

read_rds(
  paste0(
    'data/raw/',
    my_species,
    '_gbif_raw.rds'))
