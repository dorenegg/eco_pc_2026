# Herpetología
# Taller 4. Distribución geográfica
# Instalación de paquetes
# Profesor Héctor Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

ipak <-
  function(pkg){
    new.pkg <-
      pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)}

packages <-
  c(
    "janitor",
    "rinat",
    "tidyverse",
    "rgbif",
    "CoordinateCleaner",
    "tmap",
    "sf",
    "terra",
    "cols4all")

ipak(packages)
