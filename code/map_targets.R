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

# Calculate reach aggregated at the state level for each grain:

reach_state <- locations_targets %>% 
  group_by(state) %>% 
  summarise(reach_rice_local = sum(rice_local == "Yes") / n(),
            reach_rice_imported = sum(rice_imported == "Yes") / n(),
            reach_wheatf = sum(wheat_flour == "Yes") / n(),
            reach_maizef = sum(maize_flour == "Yes") / n())

# Now calculate reach aggregated at LGA for each grain: 

reach_lga <- locations_targets %>% 
  group_by(lga) %>% 
  summarise(reach_rice_local = sum(rice_local == "Yes") / n(),
            reach_rice_imported = sum(rice_imported == "Yes") / n(),
            reach_wheatf = sum(wheat_flour == "Yes") / n(),
            reach_maizef = sum(maize_flour == "Yes") / n())

n_distinct(household_locations$lga) 
# Note that not all LGA's are represented in the survey: Only 741/774

# See elements in reach_lga that are not in nigeria_2
setdiff(reach_lga$lga, nigeria_2$lga) # There are 92 of these elements

# Rename elements in nigeria_2 to match those in reach_lga:
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

#-------------------------------------------------------------------------------

# Merge reach to the shapefiles:
nigeria1_targets <- sp::merge(nigeria_1, reach_state, by = "state")

nigeria2_targets <- sp::merge(nigeria_2, reach_lga, by = "lga")


# Save as a new shapefile: 
# st_write(nigeria1_targets, "map_data/outputs/nigeria1_targets.shp")
