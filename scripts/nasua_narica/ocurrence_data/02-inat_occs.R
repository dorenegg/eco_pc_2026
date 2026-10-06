# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# descarga de datos iNaturalist. Una especie

# setup -------------------------------------------------------------------

rm(list = ls())

library(rinat)
library(tidyverse)

# species -----------------------------------------------------------------

my_species <- 'nasua_narica'
query <- 'Nasua narica'


# metadata ----------------------------------------------------------------

inat_metadata <-
  get_inat_obs(
    query = query,
    meta = TRUE) %>%
  pluck('meta')

inat_metadata
# $found
# [1] 29933
#
# $returned
# [1] 100

# download data -----------------------------------------------------------

inat_data <-
  get_inat_obs(
    query = query,
    quality = 'research',
    geo = TRUE,
    maxresults = 10000,
    meta = FALSE) %>%
  as_tibble()

# check -------------------------------------------------------------------

nrow(inat_data) #10000

inat_data %>%
  distinct(scientific_name)

inat_data %>%
  count(
    scientific_name,
    sort = TRUE)

inat_occurrences <-
  inat_data %>%
  filter(
    scientific_name == query)

inat_occurrences %>%
  distinct(scientific_name)

# save data ---------------------------------------------------------------

inat_occurrences %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_inat_raw.rds'))

# check saved files -------------------------------------------------------

read_rds(
  paste0(
    'data/raw/',
    my_species,
    '_inat_raw.rds'))
