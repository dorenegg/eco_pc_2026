# Herpetología
# Taller 4. Distribución geográfica
# UICN. Transformar shapefiles
# Profesor Héctor Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(tmap)
library(tidyverse)

# data --------------------------------------------------------------------
# shif+ctrl+c
#
my_species <-
'nasua_narica'

countries <-
  read_sf('shapefiles/countries.gpkg')

iucn_shp <- #hay dos polígono uno que tiene presencia 1 y otro 5, es importante filtra ya que puede ser presencia desconocida o predecida, igual sucede con el origen
  read_sf(
    paste0(
      'shapefiles/',
      my_species,
      '/data_0.shp'))

iucn <- #clasificar lo que significa presencia
  iucn_shp %>%
  mutate(
    presence_class =
      case_when(
        PRESENCE == 1 ~ "Presencia actual",
        PRESENCE == 2 ~ "Probablemente presente",
        PRESENCE == 3 ~ "Posiblemente presente",
        PRESENCE == 4 ~ "Posiblemente extinta",
        PRESENCE == 5 ~ "Extinta",
        PRESENCE == 6 ~ "Presencia incierta",
        PRESENCE == 7 ~ "Rango adicional esperado",
        TRUE ~ "Sin información"),

    origin_class =
      case_when(
        ORIGIN == 1 ~ "Nativa",
        ORIGIN == 2 ~ "Reintroducida",
        ORIGIN == 3 ~ "Introducida",
        ORIGIN == 4 ~ "Errante",
        ORIGIN == 5 ~ "Origen incierto",
        ORIGIN == 6 ~ "Colonización asistida",
        TRUE ~ "Sin información")) %>%
  janitor::clean_names() %>%
  select(
    assessment,
    id = id_no,
    species = sci_name,
    presence_class,
    origin_class,
    seasonal:geometry)

iucn %>%
  filter(
    presence_class %in% c(
      "Presencia actual",
      "Extinta"),
    origin_class == "Nativa") %>%
  tm_shape() +
  tm_polygons(
    fill = 'presence_class',
    fill.scale =
      tm_scale_categorical(
        values = c('red', 'cornflowerblue')),
    fill.legend =
      tm_legend(
        title = 'Status',
        reverse = TRUE)) +
  tm_shape(countries) +
  tm_borders()

# calcular area -------------------------------------------------------

iucn_area <-
  iucn %>%
  st_transform(crs = 3395) %>%
  st_area() %>%  # metros cuadrados
  units::set_units(km^2)

iucn_area

names(iucn_area) <- #si posee dos poligonos para cargar el área este funcionará si solo es una funciona el código de abajo "presente"
  c('presente', 'extinta')

names(iucn_area) <-
  'presente'

# como cargar ------------------------------------------------------------------
#shift+ctrl+m Tools-Globa-Code-use native para que no salga |

iucn %>%
  write_sf(
    paste0(
      'shapefiles/',
      my_species,
      '.gpkg'))

iucn <-
  read_sf(
    paste0(
      'shapefiles/',
    my_species,
    '.gpkg'))
