# Ecologia de poblaciones y comunidades
# Lectura de datos ordenados
# Profesor Hector Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

library(gt)
library(tidyverse)

# data ------------------------------------------

angola_ungulates <-
  read_csv('data/raw/angola_ungulates.csv')

angola_ungulates %>%
  distinct(taxonomy) #Vienen juntos Clase-Orden-Famili, La clase es la misma para todos, dato repetitivo por lo que se puede quitar y solo poner en el título que son de la misma clase

# 1- Arregle la fecha creando una columna 'date' con las funciones unite() y as_date(). La fecha debe estar ordenada como year, month, day. Asigne al objeto resultante el nombre angola_ungulates_date_fix

angola_ungulates_date_fix <-
  angola_ungulates %>%
  unite( #para unir coulumnas
    col = 'date',
    year:month:day,
    sep = '-') %>%
  mutate(date = as_date(date))


# 2- Haga una nueva columna con el nombre cientifico (sci_name) con la informacion de las columnas llamadas 'genus' y 'species' utilizando la funcion unite. Como argumento separador use sep = ' ' de manera que quede un espacio entre el genero y la especie. Luego utilice la funcion rename para cambiar la columna sci_name a 'species'. Asigne al objeto resultante el nombre angola_ungulates_spp_fix.

angola_ungulates_spp_fix <-
  angola_ungulates_date_fix %>%
  unite(
    col="sci_name",
    genus:species,
    sep = " ") %>%
      rename(species = sci_name)

angola_ungulates_spp_fix

# 3- Separe la columna taxonomia en tres columnas llamadas 'class', 'order', 'family utilizando la funcion separate. Como argumento separador use sep = '-' de manera que R entienda que cada guion separa un nombre correspondiente a cada columna.

angola_ungulates_taxonomy_fix <-
  angola_ungulates_spp_fix %>%  separate(
    taxonomy,
    into = c('class', 'order', 'family'),
    sep = '-') %>%

# 4- haga una lista con 2 elementos llamados 'taxonomy' y 'observations'. Para el objeto taxonomy utilice las columnas species, order, family, common_name. Para el objeto observations seleccione date, user_login, species).  Como el dato Class es el mismo para ambos ordenes (Mammalia) es innecesario ponerlo.

taxonomy <-
  angola_ungulates_taxonomy_fix %>%
  select(order:common_name) %>%
  distinct()

taxonomy

observations <-
  angola_ungulates_taxonomy_fix %>%
  select(date,user_login, species) %>%
  distinct()

observations

list(
  taxonomy = taxonomy,
  observations = observations) %>%
  write_rds('data/processed/ungulates_list.rds')

# figure -------------------------------------------------------------------

# número de observaciones por especie

observations %>%
  count(species) %>%
  ggplot(
    aes(
      x = n,
      y = reorder(species, n))) +
  geom_col() +
  labs(
    x = 'Number of observations',
    y = NULL) +
  theme_classic() +
  theme(
    axis.text.y = element_text(face = 'italic'))

ggsave(
  'output/figures/ungulate_observations.tiff',
  width = 180,
  height = 120,
  units = 'mm',
  dpi = 300,
  compression = 'lzw',
  bg = 'white')


# cuadro ------------------------------------------------------------------

# taxonomia


taxonomy %>%
  mutate(
    species = paste0('*', species, '*')) %>%
  gt() %>%
  tab_header(
    title = md(
      '**Ungulate species recorded in Angola and their taxonomic classification.**')) %>%
  cols_label(
    species = 'Species',
    order = 'Order',
    family = 'Family',
    common_name = 'Common name') %>%
  fmt_markdown(
    columns = species) %>%
  opt_row_striping()
