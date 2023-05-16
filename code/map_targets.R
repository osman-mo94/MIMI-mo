################################################################################
#################### SCRIPT FOR MAPPING OF TARGET VARIABLES ####################
################################################################################

rq_packages <- c("tidyverse", "sf", "tmap")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Import shapefiles
nigeria_0 <- st_read("map_data/geoBoundaries-NGA-ADM0-all")
plot(nigeria_0$geometry)

# Admin level 1: States
nigeria_1 <- st_read("map_data/geoBoundaries-NGA-ADM1-all")
plot(nigeria_1$geometry)

# Admin level 2: LGA
nigeria_2 <- st_read("map_data/geoBoundaries-NGA-ADM2-all")
plot(nigeria_2$geometry)