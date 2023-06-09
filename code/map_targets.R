################################################################################
#################### SCRIPT FOR MAPPING OF TARGET VARIABLES ####################
################################################################################

# This script creates maps for for reach and coverage target variables. I also 
# perform some mapping of the adequacy of key micronutrients (Vitamin A, Folate,
# Zinc, Iron and Vitamin B12)

rq_packages <- c("tidyverse", "sf", "tmap", "readr", "rmapshaper", "raster",
                 "ggplot2", "ggspatial", "cowplot", "tmaptools", "terra", 
                 "gridExtra")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Import shapefiles
nigeria_0 <- st_read("map_data/geoBoundaries-NGA-ADM0-all")
# Admin level 1: States
nigeria_1 <- st_read("map_data/geoBoundaries-NGA-ADM1-all")
# Admin level 2: LGA
nigeria_2 <- st_read("map_data/geoBoundaries-NGA-ADM2-all")

# Simplify the polygons to improve processing speed: 
nigeria_0 <- ms_simplify(nigeria_0, keep = 0.1, 
                         keep_shapes = T, snap = T)
nigeria_1 <- ms_simplify(nigeria_1, keep = 0.1,
                         keep_shapes = T, snap = T)
nigeria_2 <- ms_simplify(nigeria_2, keep = 0.1, 
                         keep_shapes = T, snap = T)

# Plot the polygons:
plot(nigeria_0$geometry)
plot(nigeria_1$geometry)
plot(nigeria_2$geometry)

# Transform location names to lowercase ready for data-linkage:
nigeria_1$shapeName <- tolower(nigeria_1$shapeName)
nigeria_2$shapeName <- tolower(nigeria_2$shapeName)

# Rename "abuja federal capital territory" to match how it is recorded in NLSS:
nigeria_1$shapeName <- ifelse(nigeria_1$shapeName == "abuja federal capital territory", 
                              "fct", nigeria_1$shapeName)

# Rename "shapeName" column as "state":
names(nigeria_1)[names(nigeria_1) == "shapeName"] <- "state"

# In nigeria_2 df, rename the "shapeName" column to lga: 
names(nigeria_2)[names(nigeria_2) == "shapeName"] <- "lga"

#-------------------------------------------------------------------------------

# Extract location data for each household:

cover <- read_csv("NLSS_data/Household/secta_cover.csv")
household_locations <- cover %>% dplyr::select("hhid", "zone", "state", "lga", "sector")

# Read in data dictionaries for zone, state and lga: 
zone_dictionary <- read_csv("NLSS_data/data_dictionary/zone.csv")
state_dictionary <- read_csv("NLSS_data/data_dictionary/state.csv")
lga_dictionary <- read_csv("NLSS_data/data_dictionary/lga.csv")

# Select only the relevant columns:
zone_dictionary <- zone_dictionary %>% dplyr::select("Value", "Category")
state_dictionary <- state_dictionary %>% dplyr::select("Value", "Category")
lga_dictionary <- lga_dictionary %>% dplyr::select("Value", "Category")

# Remove the numbering from the category columns: 
zone_dictionary$Category <- gsub("^\\d+\\.\\s+", "", zone_dictionary$Category)
state_dictionary$Category <- gsub("^\\d+\\.\\s+", "", state_dictionary$Category)
lga_dictionary$Category <- gsub("^\\d+\\.\\s+", "", lga_dictionary$Category)

# Rename items in the location df so that it can be linked to the shapefiles: 

# SECTOR:
household_locations <- household_locations %>% mutate(sector = dplyr::case_when(
  sector == 1 ~ "urban",
  sector == 2 ~ "rural", 
  TRUE ~ NA_character_
))

# ZONE: 
# Assign zone to each household according to data-dictionary
household_locations <- merge(household_locations, zone_dictionary, 
                             by.x = "zone", by.y = "Value", all.x = T)
# Rename columns:
household_locations <- household_locations %>% dplyr::select(-zone)
names(household_locations)[names(household_locations) == "Category"] <- "zone"

# STATE: 
# Assign state to each household according to data-dictionary
household_locations <- merge(household_locations, state_dictionary, 
                             by.x = "state", by.y = "Value", all.x = T)
# Rename columns:
household_locations <- household_locations %>% dplyr::select(-state)
names(household_locations)[names(household_locations) == "Category"] <- "state"

# LGA: 
# Assign lga to each household according to data-dictionary
household_locations <- merge(household_locations, lga_dictionary, 
                             by.x = "lga", by.y = "Value", all.x = T)
# Rename columns:
household_locations <- household_locations %>% dplyr::select(-lga)
names(household_locations)[names(household_locations) == "Category"] <- "lga"

# Remove dictionary dataframes as these are no longer required: 
rm(list = c("zone_dictionary", "state_dictionary", "lga_dictionary"))

# Transform the zone, state and lga columns into all lowercase to make linkage easier: 
household_locations$zone <- tolower(household_locations$zone)
household_locations$state <- tolower(household_locations$state)
household_locations$lga <- tolower(household_locations$lga)

# There appear to be trailing spaces in these columns, remove these to avoid issues when linking: 
household_locations$zone <- str_squish(household_locations$zone)
household_locations$state <- str_squish(household_locations$state)
household_locations$lga <- str_squish(household_locations$lga)

# Apply the same function to the columns in the shapefiles: 
nigeria_1$state <- str_squish(nigeria_1$state)
nigeria_2$lga <- str_squish(nigeria_2$lga)

# Replace hyphens with spaces for state and LGA names, as there are some inconsistencies: 
nigeria_1$state <- gsub("-", " ", nigeria_1$state)
nigeria_2$lga <- gsub("-", " ", nigeria_2$lga)

household_locations$state <- gsub("-", " ", household_locations$state)
household_locations$lga <- gsub("-", " ", household_locations$lga)

#-------------------------------------------------------------------------------

# Source the target variables script:
source("code/target_variables.R")

# Merge the df with locations to the df with targets
locations_targets <- household_locations %>% 
  left_join(target_variables,
            by = "hhid")

#-------------------------------------------------------------------------------

# REACH

# Calculate reach aggregated at the state level for each grain:

reach_state <- locations_targets %>% 
  group_by(state) %>% 
  summarise(reach_rice_local = sum(rice_local == "Yes") / n(),
            reach_rice_imported = sum(rice_imported == "Yes") / n(),
            reach_rice_combined = sum(rice_combined == "Yes") / n(),
            reach_wheatf = sum(wheat_flour == "Yes") / n(),
            reach_maizef = sum(maize_flour == "Yes") / n())

# Now calculate reach aggregated at LGA for each grain: 

reach_lga <- locations_targets %>% 
  group_by(lga) %>% 
  summarise(reach_rice_local = sum(rice_local == "Yes") / n(),
            reach_rice_imported = sum(rice_imported == "Yes") / n(),
            reach_rice_combined = sum(rice_combined == "Yes") / n(),
            reach_wheatf = sum(wheat_flour == "Yes") / n(),
            reach_maizef = sum(maize_flour == "Yes") / n())

n_distinct(household_locations$lga) 
# Note that not all LGA's are represented in the survey: Only 741/774

# See lga's in reach_lga that do not match those in nigeria_2
setdiff(reach_lga$lga, nigeria_2$lga) # There are 92 lga's that are named inconsistently

# Rename lga's in nigeria_2 to match those in reach_lga:
nigeria_2$lga[nigeria_2$lga == "abaji"] <- "abaji area council"
nigeria_2$lga[nigeria_2$lga == "abua/odual"] <- "abua odua"
nigeria_2$lga[nigeria_2$lga == "ado"] <- "ador"
nigeria_2$lga[nigeria_2$lga == "akamkpa"] <- "akamkpa buyo"
nigeria_2$lga[nigeria_2$lga == "aninri"] <- "aniniri"
nigeria_2$lga[nigeria_2$lga == "arewa dandi"] <- "arewa"
nigeria_2$lga[nigeria_2$lga == "askira uba"] <- "asikira/uba"
nigeria_2$lga[nigeria_2$lga == "atakunmosa east"] <- "atakumosa east"
nigeria_2$lga[nigeria_2$lga == "atakunmosa west"] <- "atakumosa west"
nigeria_2$lga[nigeria_2$lga == "atisbo"] <- "atigbo"
nigeria_2$lga[nigeria_2$lga == "bagudu"] <- "bagudo"
nigeria_2$lga[nigeria_2$lga == "birnin magaji kiyaw"] <- "birnin magaji"
nigeria_2$lga[nigeria_2$lga == "biriniwa"] <- "birniwa"
nigeria_2$lga[nigeria_2$lga == "boluwaduro"] <- "bolowaduro"
nigeria_2$lga[nigeria_2$lga == "bwari"] <- "bwari area council"
nigeria_2$lga[nigeria_2$lga == "damboa"] <- "damaboa"
nigeria_2$lga[nigeria_2$lga == "dambam"] <- "damban"
nigeria_2$lga[nigeria_2$lga == "dambatta"] <- "danbatta"
nigeria_2$lga[nigeria_2$lga == "wasagu danko"] <- "danko wasagu"
nigeria_2$lga[nigeria_2$lga == "edati"] <- "edati idati"
nigeria_2$lga[nigeria_2$lga == "yewa north"] <- "egbado north/yewa"
nigeria_2$lga[nigeria_2$lga == "yewa south"] <- "egbado south/"
nigeria_2$lga[nigeria_2$lga == "ekiti south west"] <- "ekiti south"
nigeria_2$lga[nigeria_2$lga == "emuoha"] <- "emohu"
nigeria_2$lga[nigeria_2$lga == "ezinihitte"] <- "ezinihitte mbaise"
nigeria_2$lga[nigeria_2$lga == "fufore"] <- "fufore/gurin"
nigeria_2$lga[nigeria_2$lga == "garun malam"] <- "garum mallam"
nigeria_2$lga[nigeria_2$lga == "gwagwalada"] <- "gwagwalada area council"
nigeria_2$lga[nigeria_2$lga == "ido osi"] <- "ido/osi"
nigeria_2$lga[nigeria_2$lga == "ifako/ijaye"] <- "ifako ijaye"
nigeria_2$lga[nigeria_2$lga == "iguegben"] <- "igugben"
nigeria_2$lga[nigeria_2$lga == "ihitte/uboma"] <- "ihitte uboma"
nigeria_2$lga[nigeria_2$lga == "ikorodu"] <- "ikorordu"
nigeria_2$lga[nigeria_2$lga == "ikpoba okha"] <- "ikpooba okha"
nigeria_2$lga[nigeria_2$lga == "ile oluji/okeigbo"] <- "ileoluji/okeigbo"
nigeria_2$lga[nigeria_2$lga == "ilesha east"] <- "ilesa east"
nigeria_2$lga[nigeria_2$lga == "ilesha west"] <- "ilesa west"
nigeria_2$lga[nigeria_2$lga == "imeko afon"] <- "imeko/afon"
nigeria_2$lga[nigeria_2$lga == "isuikwuato"] <- "isuikwato"
nigeria_2$lga[nigeria_2$lga == "itas/gadau"] <- "itas gadau"
nigeria_2$lga[nigeria_2$lga == "kibiya"] <- "kabiya"
nigeria_2$lga[nigeria_2$lga == "kiri kasama"] <- "kirika kasamma"
nigeria_2$lga[nigeria_2$lga == "kogi"] <- "kogi(k.k)"
nigeria_2$lga[nigeria_2$lga == "kuje"] <- "kuje area council"
nigeria_2$lga[nigeria_2$lga == "kwali"] <- "kwali area council"
nigeria_2$lga[nigeria_2$lga == "langtang north"] <- "lantang north"
nigeria_2$lga[nigeria_2$lga == "langtang south"] <- "lantang south"
nigeria_2$lga[nigeria_2$lga == "maiduguri"] <- "maiduguri metropolitan"
nigeria_2$lga[nigeria_2$lga == "maigatari"] <- "maigatar"
nigeria_2$lga[nigeria_2$lga == "malumfashi"] <- "malunfashi"
nigeria_2$lga[nigeria_2$lga == "mbatoli"] <- "mbaitoli"
nigeria_2$lga[nigeria_2$lga == "mopa muro"] <- "mopamuro"
nigeria_2$lga[nigeria_2$lga == "munya"] <- "muya"
nigeria_2$lga[nigeria_2$lga == "nasarawa egon"] <- "nasarawa eggon"
nigeria_2$lga[nigeria_2$lga == "nkwerre"] <- "nkwere"
nigeria_2$lga[nigeria_2$lga == "esit eket"] <- "nsit eket"
nigeria_2$lga[nigeria_2$lga == "obi nwga"] <- "obingwa"
nigeria_2$lga[nigeria_2$lga == "obio/akpor"] <- "obio akpor"
nigeria_2$lga[nigeria_2$lga == "ogbomosho north"] <- "ogbomoso north"
nigeria_2$lga[nigeria_2$lga == "ogbomosho south"] <- "ogbomoso south"
nigeria_2$lga[nigeria_2$lga == "ohaji/egbema"] <- "ohaji egbema"
nigeria_2$lga[nigeria_2$lga == "okrika"] <- "okirika"
nigeria_2$lga[nigeria_2$lga == "onuimo"] <- "ono imo"
nigeria_2$lga[nigeria_2$lga == "osisioma ngwa"] <- "osisioma north"
nigeria_2$lga[nigeria_2$lga == "oturkpo"] <- "otukpo"
nigeria_2$lga[nigeria_2$lga == "pategi"] <- "patigi"
nigeria_2$lga[nigeria_2$lga == "sabon birni"] <- "sabon birnin"
nigeria_2$lga[nigeria_2$lga == "shagamu"] <- "sagamu"
nigeria_2$lga[nigeria_2$lga == "shomgom"] <- "shongom"
nigeria_2$lga[nigeria_2$lga == "sule tankarkar"] <- "sule tankar kar"
nigeria_2$lga[nigeria_2$lga == "tambuwal"] <- "tambuwai"
nigeria_2$lga[nigeria_2$lga == "abi"] <- "ugep south abi"
nigeria_2$lga[nigeria_2$lga == "uhunmwonde"] <- "uhunmuonde"
nigeria_2$lga[nigeria_2$lga == "ukwuani"] <- "ukwani"
nigeria_2$lga[nigeria_2$lga == "umu nneochi"] <- "umunneochi"
nigeria_2$lga[nigeria_2$lga == "yabo"] <- "yabo bodingo"
nigeria_2$lga[nigeria_2$lga == "yakurr"] <- "yakurr ugep north"
nigeria_2$lga[nigeria_2$lga == "yala"] <- "yalla"
nigeria_2$lga[nigeria_2$lga == "yamaltu/deba"] <- "yamaltu deba"
nigeria_2$lga[nigeria_2$lga == "yankwashi"] <- "yan kwashi"
nigeria_2$lga[nigeria_2$lga == "yenegoa"] <- "yenagoa"

# There are 2 lga's with the name "bassa" in nigeria_2, identify which is which:
plot(nigeria_2$geometry)
#Highlight geometry for row 418 on the map:
plot(nigeria_2$geometry[418], add = T, col = "red")
# The map above shows that the lga in row 418 is in Kogi state

# Rename the bassa lga's accordingly: 
nigeria_2$lga[418] <- "bassa (kogi)"
nigeria_2$lga[688] <- "bassa (plateau)"

# There are 2 lga's with the name "ifelodun" in nigeria_2, identify which is which:
plot(nigeria_2$geometry)
plot(nigeria_2$geometry[94], add = T, col = "red")
plot(nigeria_2$geometry[579], add = T, col = "#0000ff")

# Rename accordingly: 
nigeria_2$lga[94] <- "ifelodun (kwara)"

# There are 2 lga's with the name "irepodun" in nigeria_2, identify which is which:
plot(nigeria_2$geometry)
plot(nigeria_2$geometry[397], add = T, col = "red")
plot(nigeria_2$geometry[608], add = T, col = "#0000ff")

# Rename accordingly:
nigeria_2$lga[397] <- "irepodun (kwara)"
nigeria_2$lga[608] <- "irepodun (osun)"

# There are 2 lga's with the name "nasarawa" in nigeria_2, identify which is which:
plot(nigeria_2$geometry)
plot(nigeria_2$geometry[471], add = T, col = "red")
plot(nigeria_2$geometry[215], add = T, col = "#0000ff")

# Rename accordingly:
nigeria_2$lga[471] <- "nasarawa (nasarawa)"
nigeria_2$lga[215] <- "nasarawa (kano)"

# There are 2 lga's with the name "obi" in nigeria_2, identify which is which:
plot(nigeria_2$geometry)
plot(nigeria_2$geometry[596], add = T, col = "red")
plot(nigeria_2$geometry[723], add = T, col = "#0000ff")

# Rename accordingly:
nigeria_2$lga[596] <- "obi (benue)"
nigeria_2$lga[723] <- "obi (nasarawa)"

# There are 2 lga's with the name "surulere" in nigeria_2, identify which is which:
plot(nigeria_2$geometry)
plot(nigeria_2$geometry[81], add = T, col = "red")
plot(nigeria_2$geometry[411], add = T, col = "#0000ff")

# Rename accordingly:
nigeria_2$lga[81] <- "surulere (oyo)"
nigeria_2$lga[411] <- "surulere (lagos)"

# See which elements are in reach_lga but not in nigeria_2$lga:
setdiff(reach_lga$lga, nigeria_2$lga) 
# There are no outstanding differences remaining

# Merge reach to the shapefiles:
nigeria1_reach <- dplyr::left_join(nigeria_1, reach_state, by = "state")
nigeria2_reach <- dplyr::left_join(nigeria_2, reach_lga, by = "lga")

# Save as new shapefiles: 
# st_write(nigeria1_reach, "map_data/outputs/nigeria1_reach.shp")
# st_write(nigeria2_reach, "map_data/outputs/nigeria2_reach.shp")

# Create maps: 

# ADM1

breaks <- c(0, 0.000001, 0.3, 0.6, 0.8, 1)

# Rice
rice_ADM1 <- tm_shape(nigeria1_reach) + 
  tm_fill(col = "reach_rice_combined", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Rice", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

rice_ADM1

# Wheat flour
wheat_ADM1 <- tm_shape(nigeria1_reach) + 
  tm_fill(col = "reach_wheatf", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Wheat flour", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

wheat_ADM1

# Maize flour: 
maize_ADM1 <- tm_shape(nigeria1_reach) + 
  tm_fill(col = "reach_maizef", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Maize flour", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

maize_ADM1

# legend: 
reach_legend1 <- tm_shape(nigeria1_reach) + 
  tm_fill(col = "reach_maizef", breaks = breaks, palette = "Blues",
          title = "Reach (% of households with access 
to the fortification vehicle)",
          labels = c("0", "0-30%", "30-60%", "60-80%", "80-100%"),
          textNA = "Missing Data",
          colorNA = "gray35") +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1) +
  tm_compass(north = 0, type = "arrow")

reach_legend1


# Integrate these mapsand legend into a single figure: 

reach_ADM1 <- list(rice_ADM1, wheat_ADM1, maize_ADM1, reach_legend1)

reach_ADM1 <- tmap_arrange(reach_ADM1, ncol = 2, sync = TRUE)

reach_ADM1

tmap_save(reach_ADM1, "figures/reach_ADM1.png", height = 5, width = 5)


# ADM2

# Rice
rice_ADM2 <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_rice_combined", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

rice_ADM2

# Wheat flour
wheat_ADM2 <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_wheatf", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

wheat_ADM2

# Maize flour: 
maize_ADM2 <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_maizef", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

maize_ADM2

# legend: 
reach_legend2 <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_maizef", breaks = breaks, palette = "Blues",
          title = "Reach (% of households with access 
to the fortification vehicle)",
          labels = c("0", "0-30%", "30-60%", "60-80%", "80-100%"),
          textNA = "Missing Data",
          colorNA = "gray35") +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1) +
  tm_compass(north = 0, type = "arrow")

reach_legend2


# Integrate these mapsand legend into a single figure: 

reach_ADM2 <- list(rice_ADM2, wheat_ADM2, maize_ADM2, reach_legend2)

reach_ADM2 <- tmap_arrange(reach_ADM2, ncol = 2, sync = TRUE)

reach_ADM2

tmap_save(reach_ADM2, "figures/reach_ADM2.png", height = 5, width = 5)

#-------------------------------------------------------------------------------

# COVERAGE

# Calculate coverage aggregated at the state level for each staple grain: 

coverage_state <- locations_targets %>% 
  filter(risk_MND == "Yes") %>% group_by(state) %>% 
  summarise(coverage_rice_local = sum(rice_local == "Yes") / n(),
            coverage_rice_imported = sum(rice_imported == "Yes") / n(),
            coverage_rice_combined = sum(rice_combined == "Yes") / n(),
            coverage_wheatf = sum(wheat_flour == "Yes") / n(),
            coverage_maizef = sum(maize_flour == "Yes") / n())

# Calculate coverage aggregated at the lga level for each staple grain: 

coverage_lga <- locations_targets %>% 
  filter(risk_MND == "Yes") %>% group_by(lga) %>% 
  summarise(coverage_rice_local = sum(rice_local == "Yes") / n(),
            coverage_rice_imported = sum(rice_imported == "Yes") / n(),
            coverage_rice_combined = sum(rice_combined == "Yes") / n(),
            coverage_wheatf = sum(wheat_flour == "Yes") / n(),
            coverage_maizef = sum(maize_flour == "Yes") / n())

# Merge coverage to the shapefiles:
nigeria1_coverage <- dplyr::left_join(nigeria_1, coverage_state, by = "state")
nigeria2_coverage <- dplyr::left_join(nigeria_2, coverage_lga, by = "lga")

# Save as new shapefiles: 
# st_write(nigeria1_coverage, "map_data/outputs/nigeria1_coverage.shp")
# st_write(nigeria2_coverage, "map_data/outputs/nigeria2_coverage.shp")

# Create maps: 

# ADM1

breaks <- c(0, 0.000001, 0.3, 0.6, 0.8, 1)

# Rice
rice_ADM1cov <- tm_shape(nigeria1_coverage) + 
  tm_fill(col = "coverage_rice_combined", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Rice", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

rice_ADM1cov

# Wheat flour
wheat_ADM1cov <- tm_shape(nigeria1_coverage) + 
  tm_fill(col = "coverage_wheatf", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Wheat flour", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

wheat_ADM1cov

# Maize flour: 
maize_ADM1cov <- tm_shape(nigeria1_coverage) + 
  tm_fill(col = "coverage_maizef", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Maize flour", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

maize_ADM1cov

# legend: 
coverage_legend1 <- tm_shape(nigeria1_coverage) + 
  tm_fill(col = "coverage_maizef", breaks = breaks, palette = "Blues",
          title = "Coverage (% of households with apparently inadequate
diet with access to the fortification vehicle)",
          labels = c("0", "0-30%", "30-60%", "60-80%", "80-100%"),
          textNA = "Missing Data",
          colorNA = "gray35") +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1) +
  tm_compass(north = 0, type = "arrow")

coverage_legend1


# Integrate these mapsand legend into a single figure: 

coverage_ADM1 <- list(rice_ADM1cov, wheat_ADM1cov, maize_ADM1cov, coverage_legend1)

coverage_ADM1 <- tmap_arrange(coverage_ADM1, ncol = 2, sync = TRUE)

coverage_ADM1

tmap_save(coverage_ADM1, "figures/coverage_ADM1.png", height = 5, width = 5)


# ADM2

# Rice
rice_ADM2cov <- tm_shape(nigeria2_coverage) + 
  tm_fill(col = "coverage_rice_combined", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

rice_ADM2cov

# Wheat flour
wheat_ADM2cov <- tm_shape(nigeria2_coverage) + 
  tm_fill(col = "coverage_wheatf", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

wheat_ADM2cov

# Maize flour: 
maize_ADM2cov <- tm_shape(nigeria2_coverage) + 
  tm_fill(col = "coverage_maizef", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

maize_ADM2cov

# legend: 
coverage_legend2 <- tm_shape(nigeria2_coverage) + 
  tm_fill(col = "coverage_maizef", breaks = breaks, palette = "Blues",
          title = "Coverage (% of households with apparently inadequate
diet with access to the fortification vehicle)",
          labels = c("0", "0-30%", "30-60%", "60-80%", "80-100%"),
          textNA = "Missing Data",
          colorNA = "gray35") +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1) +
  tm_compass(north = 0, type = "arrow")

coverage_legend2


# Integrate these mapsand legend into a single figure: 

coverage_ADM2 <- list(rice_ADM2cov, wheat_ADM2cov, maize_ADM2cov, coverage_legend2)

coverage_ADM2 <- tmap_arrange(coverage_ADM2, ncol = 2, sync = TRUE)

coverage_ADM2

tmap_save(coverage_ADM2, "figures/coverage_ADM2.png", height = 5, width = 5)


#-------------------------------------------------------------------------------

# TESTING ALTERNATIVE COVERAGE THRESHOLDS

# Calculate coverage using alternative thresholds for "inadequate diet", 
# only at the ADM2 level for now: 

coverage_lga_alt1 <- locations_targets %>% 
  filter(risk_MND1 == "Yes") %>% group_by(lga) %>% 
  summarise(coverage_rice_local = sum(rice_local == "Yes") / n(),
            coverage_rice_imported = sum(rice_imported == "Yes") / n(),
            coverage_rice_combined = sum(rice_combined == "Yes") / n(),
            coverage_wheatf = sum(wheat_flour == "Yes") / n(),
            coverage_maizef = sum(maize_flour == "Yes") / n())

coverage_lga_alt3 <- locations_targets %>% 
  filter(risk_MND3 == "Yes") %>% group_by(lga) %>% 
  summarise(coverage_rice_local = sum(rice_local == "Yes") / n(),
            coverage_rice_imported = sum(rice_imported == "Yes") / n(),
            coverage_rice_combined = sum(rice_combined == "Yes") / n(),
            coverage_wheatf = sum(wheat_flour == "Yes") / n(),
            coverage_maizef = sum(maize_flour == "Yes") / n())

# Merge to shapefiles: 
nigeria2_coverage_alt1 <- dplyr::left_join(nigeria_2, coverage_lga_alt1, by = "lga")
nigeria2_coverage_alt3 <- dplyr::left_join(nigeria_2, coverage_lga_alt3, by = "lga")

# Save as new shapefiles:
# st_write(nigeria2_coverage_alt1, "map_data/outputs/nigeria2_coverage_alt1.shp")
# st_write(nigeria2_coverage_alt3, "map_data/outputs/nigeria2_coverage_alt3.shp")

# No need to map these currently

#-------------------------------------------------------------------------------

# MICRONUTRIENT ADEQUACY:

# Aggregate micronutrient adequacy at the ADM1 and ADM2 level:  

micronutrients_ADM1 <- locations_targets %>% group_by(state) %>% 
  summarise(vitamina_adequacy = sum(vitamina_adequate == "Adequate", na.rm = T) / n(),
            folate_adequacy = sum(folate_adequate == "Adequate", na.rm = T) / n(),
            zinc_adequacy = sum(zn_adequate == "Adequate", na.rm = T) / n(),
            iron_adequacy = sum(fe_adequate == "Adequate", na.rm = T)/ n(),
            b12_adequacy = sum(vitaminb12_adequate == "Adequate", na.rm = T)/ n())

# Merge to shapefiles: 
micronutrients_ADM1 <- dplyr::left_join(nigeria_1, micronutrients_ADM1, by = "state")

# Map adequacy of MN's at ADM1 level

breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

# Vitamin A:
vitamina_map <- tm_shape(micronutrients_ADM1) + 
  tm_fill(col = "vitamina_adequacy", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Vitamin A", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

vitamina_map

# Folate
folate_map <- tm_shape(micronutrients_ADM1) + 
  tm_fill(col = "folate_adequacy", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Folate", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

folate_map

# Zinc: 
zinc_map <- tm_shape(micronutrients_ADM1) + 
  tm_fill(col = "zinc_adequacy", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Zinc", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

zinc_map

# Iron: 
iron_map <- tm_shape(micronutrients_ADM1) + 
  tm_fill(col = "iron_adequacy", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Iron", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

iron_map

# B12: 
b12_map <- tm_shape(micronutrients_ADM1) + 
  tm_fill(col = "b12_adequacy", breaks = breaks, palette = "Blues") +
  tm_layout(main.title = "Vitamin B12", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) + 
  tm_legend(show = F)

b12_map

# legend: 
map_legend <- tm_shape(micronutrients_ADM1) + 
  tm_fill(col = "b12_adequacy", breaks = breaks, palette = "Blues",
          title = "Micronutrient Adequacy",
          labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
          textNA = "Missing Data",
          colorNA = "gray35") +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1) +
  tm_compass(north = 0, type = "arrow")

map_legend


# Integrate these 5 maps and legend into a single figure: 

maps_list <- list(vitamina_map, folate_map, zinc_map, iron_map, b12_map, map_legend)

mn_ADM1 <- tmap_arrange(maps_list, ncol = 3, sync = TRUE)

mn_ADM1

tmap_save(mn_ADM1, "figures/mn_ADM1.png", height = 4.5, width = 7)

#-------------------------------------------------------------------------------

# Repeat for ADM2 level:

micronutrients_ADM2 <- locations_targets %>% group_by(lga) %>% 
  summarise(vitamina_adequacy = sum(vitamina_adequate == "Adequate", na.rm = T) / n(),
            folate_adequacy = sum(folate_adequate == "Adequate", na.rm = T) / n(),
            zinc_adequacy = sum(zn_adequate == "Adequate", na.rm = T) / n(),
            iron_adequacy = sum(fe_adequate == "Adequate", na.rm = T)/ n(),
            b12_adequacy = sum(vitaminb12_adequate == "Adequate", na.rm = T)/ n())

# Merge to shapefiles: 
micronutrients_ADM2 <- dplyr::left_join(nigeria_2, micronutrients_ADM2, by = "lga")

# Create maps

# Vitamin A:
vitamina_ADM2 <- tm_shape(micronutrients_ADM2) + 
  tm_fill(col = "vitamina_adequacy", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Vitamin A", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_legend(show = F)

vitamina_ADM2

# Folate
folate_ADM2 <- tm_shape(micronutrients_ADM2) + 
  tm_fill(col = "folate_adequacy", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Folate", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_legend(show = F)

folate_ADM2

# Zinc: 
zinc_ADM2 <- tm_shape(micronutrients_ADM2) + 
  tm_fill(col = "zinc_adequacy", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Zinc", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_legend(show = F)

zinc_ADM2

# Iron: 
iron_ADM2 <- tm_shape(micronutrients_ADM2) + 
  tm_fill(col = "iron_adequacy", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Iron", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_legend(show = F)

iron_ADM2

# B12: 
b12_ADM2 <- tm_shape(micronutrients_ADM2) + 
  tm_fill(col = "b12_adequacy", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Vitamin B12", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.5) + 
  tm_legend(show = F)

b12_ADM2

# legend: 
ADM2_legend <- tm_shape(micronutrients_ADM2) + 
  tm_fill(col = "b12_adequacy", breaks = breaks, palette = "Blues",
          title = "Micronutrient Adequacy",
          labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
          textNA = "Missing Data",
          colorNA = "gray35") +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1) +
  tm_compass(north = 0, type = "arrow")

ADM2_legend


# Integrate these 5 maps and legend into a single figure: 

maps_list <- list(vitamina_ADM2, folate_ADM2, zinc_ADM2, iron_ADM2, b12_ADM2, ADM2_legend)

mn_ADM2 <- tmap_arrange(maps_list, ncol = 3, sync = TRUE)

mn_ADM2

tmap_save(mn_ADM2, "figures/mn_ADM2.png", height = 4.5, width = 7)

#-------------------------------------------------------------------------------
