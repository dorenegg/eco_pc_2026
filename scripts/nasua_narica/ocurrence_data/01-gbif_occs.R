# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# descarga de datos gbif. Una especie

# setup -------------------------------------------------------------------

rm(list = ls())

library(rgbif)
library(tidyverse)

# data --------------------------------------------------------------------

species <- 'Nasua narica'
my_species <- 'nasua_narica'
# taxon key ---------------------------------------------------------------

key <-
  name_backbone(
    name = species) %>%
  pull(usageKey)

key #"2433531"

# download request --------------------------------------------------------

gbif_download <-
  occ_download(
    pred(
      'taxonKey',
      key),
    format = 'SIMPLE_CSV')

gbif_download

# save download information ----------------------------------------------

gbif_download %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_download.rds'))

# read download information ----------------------------------------------

gbif_download <-
  read_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_download.rds'))


# <<gbif download>>
# Your download is being processed by GBIF:
#   https://www.gbif.org/occurrence/download/0011420-260928105237408
# Most downloads finish within 15 min.
# Check status with
# occ_download_wait('0011420-260928105237408')
# After it finishes, use
# d <- occ_download_get('0011420-260928105237408') %>%
#   occ_download_import()
# to retrieve your download.
# Download Info:
#   Username: dorenegg
# E-mail: dorenegonza@gmail.com
# Format: SIMPLE_CSV
# Download key: 0011420-260928105237408
# Created: 2026-10-05T16:16:49.174+00:00
# Citation Info:
#   Please always cite the download DOI when using this data.
# https://www.gbif.org/citation-guidelines
# DOI: 10.15468/dl.svjhn5
# Citation:
#   GBIF Occurrence Download https://doi.org/10.15468/dl.svjhn5 Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2026-10-05
# >

# check download processing -----------------------------------------------

occ_download_wait(
  gbif_download)

# import data -------------------------------------------------------------

data <-
  occ_download_get(
    gbif_download,
    path = 'data/raw',
    overwrite = TRUE) %>%
  occ_download_import() %>%
  as_tibble()

# check -------------------------------------------------------------------

nrow(data) #26537

data %>%
  distinct(species)

# save data ---------------------------------------------------------------

# rds

data %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.rds'))

# check saved files -------------------------------------------------------

print(read_rds(
  paste0(
    'data/raw/',
    my_species,
    '_gbif_raw.rds')))
