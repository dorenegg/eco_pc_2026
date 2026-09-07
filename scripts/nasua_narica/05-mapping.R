# Herpetología
# Taller 4. Distribución geográfica
# Mapa de distribucion
# Profesor Héctor Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(terra)
library(tmap)
library(tidyverse)

# species -----------------------------------------------------------------

my_species <-
  'nasua_narica'

# shapefiles --------------------------------------------------------------

iucn <-
  read_sf(
    paste0(
      'shapefiles/',
      my_species,
      '.gpkg'),
    quiet = TRUE)

countries <-
  read_sf(
    'shapefiles/countries.gpkg',
    quiet = TRUE)

costa_rica <-
  read_sf(
    'shapefiles/costa_rica_wgs84.gpkg',
    quiet = TRUE)

occs <-
  read_sf(
    paste0(
      'data/processed/',
      my_species,
      '_occ_clean.gpkg'),
    quiet = TRUE)

# raster ------------------------------------------------------------------

elevation <-
  rast(
    'rasters/elev_30s.tif')

# revisar CRS -------------------------------------------------------------

crs(elevation) #verificar que todos los DATUM sean iguales, en este caso 4326

st_crs(iucn)
st_crs(countries)
st_crs(costa_rica)
st_crs(occs)

# transformar capas al CRS del raster -------------------------------------
#ya desde el inicio estaban en el mismo CRS pero por precaución se correo
iucn <-
  iucn %>%
  st_transform(
    crs(elevation))

countries <-
  countries %>%
  st_transform(
    crs(elevation))

costa_rica <-
  costa_rica %>%
  st_transform(
    crs(elevation))

occs <-
  occs %>%
  st_transform(
    crs(elevation))

# area para el mapa -------------------------------------------------------

# La extension incluye siempre Costa Rica completa
# y se amplia automaticamente si el rango IUCN
# se extiende fuera del pais

map_area <-
  c(
    vect(costa_rica),
    vect(iucn))

map_extent <-
  ext(
    map_area)

# agregar un pequeno margen alrededor

map_extent <-
  extend(
    map_extent,
    0.5)

# recortar paises ---------------------------------------------------------

countries_crop <- #para cortar la región de interés
  crop(
    vect(countries),
    map_extent) %>%
  st_as_sf()

# recortar elevacion ------------------------------------------------------

elevation_crop <-
  crop(
    elevation,
    map_extent)

# eliminar elevacion del oceano -------------------------------------------

land <- #primero se unnern y luego se enmascaran
  countries_crop %>%
  st_union()

elevation_crop <-
  mask(
    elevation_crop,
    vect(land))

# hillshade ---------------------------------------------------------------

slope <-
  terrain(
    elevation_crop,
    v = 'slope',
    unit = 'radians')

aspect <-
  terrain(
    elevation_crop,
    v = 'aspect',
    unit = 'radians')

hillshade <-
  shade(
    slope,
    aspect,
    angle = 40,
    direction = 315)

# map ---------------------------------------------------------------------

tmap_mode('plot')

map <-
  tm_shape(
    hillshade) +
  tm_raster(
    col = names(hillshade)[1],
    col.scale =
      tm_scale_continuous(
        values = c('grey25', 'white')),
    col.legend =
      tm_legend_hide()) +

  tm_shape(
    elevation_crop) +
  tm_raster(
    col = names(elevation_crop)[1],
    col.scale =
      tm_scale_continuous(
        values = 'terrain'),
    col_alpha = 0.72,
    col.legend =
      tm_legend(
        title = 'Elevación (m)',
        z = 2)) +

  tm_shape(
    countries_crop) +
  tm_borders(
    col = 'grey35',
    lwd = 0.8) +

  tm_shape(
    iucn) +
  tm_polygons(
    fill = 'presence_class',
    fill.scale =
      tm_scale_categorical(
        values = c(
          'Presencia actual' = 'cornflowerblue',
          'Extinta' = 'tomato')),
    fill_alpha = 0.28,
    col = 'presence_class',
    col.scale =
      tm_scale_categorical(
        values = c(
          'Presencia actual' = 'dodgerblue3',
          'Extinta' = 'firebrick3')),
    lwd = 1.2,
    fill.legend =
      tm_legend(
        title = 'Distribución IUCN',
        reverse = TRUE,
        z = 1),
    col.legend =
      tm_legend_hide()) +

  tm_shape(
    occs) +
  tm_dots(
    fill = 'red',
    col = 'white',
    size = 0.16,
    lwd = 0.8) +

  tm_scalebar(
    position =
      tm_pos_in(
        'left',
        'bottom')) +

  tm_compass(
    type = 'arrow',
    position =
      tm_pos_in(
        'right',
        'top'),
    size = 1.2) +

  tm_layout(
    bg = TRUE,
    bg.color = '#DCEEF5',
    frame = TRUE,
    frame.color = 'grey30',
    legend.outside = TRUE,
    legend.outside.position = 'right')

map

# save map ----------------------------------------------------------------

dir.create(
  'output/figures',
  recursive = TRUE,
  showWarnings = FALSE)

tmap_save(
  map,
  paste0(
    'output/figures/',
    my_species,
    '_distribution.png'),
  width = 8,
  height = 7,
  units = 'in',
  dpi = 600)
