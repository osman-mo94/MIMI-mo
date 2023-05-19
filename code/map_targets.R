################################################################################
#################### SCRIPT FOR MAPPING OF TARGET VARIABLES ####################
################################################################################

rq_packages <- c("tidyverse", "sf", "tmap", "readr", "rmapshaper", "raster")

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

#-------------------------------------------------------------------------------

# Source the target variables script:
source("code/target_variables.R")

# Merge the df with locations to the df with targets
locations_targets <- household_locations %>% 
  left_join(target_variables,
            by = "hhid")

#-------------------------------------------------------------------------------

# Calculate reach aggregated at the state level for each grain:

reach_state <- locations_targets %>% 
  group_by(state) %>% 
  summarise(reach_rice_local = sum(rice_local == "Yes") / n(),
            reach_rice_imported = sum(rice_imported == "Yes") / n(),
            reach_wheatf = sum(wheat_flour == "Yes") / n(),
            reach_maizef = sum(maize_flour == "Yes") / n())

#-------------------------------------------------------------------------------

# Merge reach to the shapefile:
nigeria1_targets <- sp::merge(nigeria_1, reach_state, by = "state")

# Save as a new shapefile: 
# st_write(nigeria1_targets, "map_data/outputs/nigeria1_targets.shp")
