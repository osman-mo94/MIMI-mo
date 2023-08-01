################################################################################
##################### SCRIPT FOR MAPPING OF ML PREDICTIONS #####################
################################################################################

# LOAD REQUIRED PACKAGES:

rq_packages <- c("tidyverse", "sf", "tmap", "readr", "rmapshaper", "raster",
                 "ggplot2", "ggspatial", "cowplot", "tmaptools", "terra", 
                 "gridExtra", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# IMPORT SHAPEFILES AND LOCATIONS: 

# Geoboundaries: 
nigeria_1 <- st_read("map_data/shapefiles_locations/nigeria_1.shp")
nigeria_2 <- st_read("map_data/shapefiles_locations/nigeria_2.shp")

# Household locations: 
household_locations <- read_csv("map_data/shapefiles_locations/household_locations.csv")

#-------------------------------------------------------------------------------

# READ IN MACHINE LEARNING PREDICTIONS: 
xgb_rice_predictions <- read_csv("map_data/ML_predictions/xgb_rice_predictions.csv")

#-------------------------------------------------------------------------------

# JOIN PREDICTIONS AND LOCATIONS: 
xgb_rice_predictions <- xgb_rice_predictions %>% 
  left_join(household_locations, by = "hhid")

#-------------------------------------------------------------------------------

# ADD SURVEY WEIGHTS: 

# Get survey weights and enumeration areas: 
cover <- read_csv("NLSS_data/Household/secta_cover.csv")

survey_weights <- cover %>% dplyr::select(hhid, wt_final, ea)

# Add survey weights to xgb_rice_predictions by enumartion area: 
xgb_rice_predictions <- xgb_rice_predictions %>% 
  left_join(survey_weights, by = "hhid")

rm(list = c("cover", "survey_weights"))

# Note that some of the survey weights are missing, find the index of these entries: 
which(is.na(xgb_rice_predictions$wt_final))

# Manually add these survey weights according to the enumeration area (according
# to the survey documentation, all households in an EA have the same weight): 
xgb_rice_predictions$wt_final[1322] <- 1093.0268
xgb_rice_predictions$wt_final[2138] <- 1636.4668
xgb_rice_predictions$wt_final[3404] <- 1332.726

xgb_rice_predictions$xgb_predictions <- as.factor(xgb_rice_predictions$xgb_predictions)

# Create tbl_svy object using xgb_rice_predictions dataframe: 
svy_xgb_rice <- xgb_rice_predictions %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final,
                          strata = ea)
#-------------------------------------------------------------------------------

# PREDICTED REACH:

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# Calculate predicted reach at state level for rice: 
rice_reachADM1 <- svy_xgb_rice %>% 
  group_by(state) %>% 
  summarise(rice_reach = survey_mean(xgb_predictions == "Yes", 
                                     vartype = NULL))

# Calculate predicted reach at LGA level for rice: 
rice_reachADM2 <- svy_xgb_rice %>% 
  group_by(lga) %>% 
  summarise(rice_reach = survey_mean(xgb_predictions == "Yes", 
                                     vartype = NULL))

#-------------------------------------------------------------------------------

# MERGE PREDICTED REACH TO SHAPEFILES: 

rice_reachADM1 <- nigeria_1 %>% left_join(rice_reachADM1, by = "state")
rice_reachADM2 <- nigeria_2 %>% left_join(rice_reachADM2, by = "lga")

#-------------------------------------------------------------------------------

# CREATE MAPS FOR RICE

breaks <- c(0, 0.000001, 0.3, 0.6, 0.8, 1)

# Reach of rice at ADM2 level:
rice_ADM2 <- tm_shape(rice_reachADM2) + 
  tm_fill(col = "rice_reach", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice (predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

rice_ADM2


# Read in ground truth for reach, this will be used for comparison: 

ADM2_true.reach <- st_read("map_data/outputs/nigeria2_reach.shp")

true.rice_ADM2 <- tm_shape(ADM2_true.reach) + 
  tm_fill(col = "rch_rc_c", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

true.rice_ADM2

# Create legend: 

reach_legend2 <- tm_shape(rice_reachADM2) + 
  tm_fill(col = "rice_reach", breaks = breaks, palette = "Blues",
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

# Arrange maps and legend in a single figure
rice_reach <- tmap_arrange(true.rice_ADM2, rice_ADM2, reach_legend2, 
                           ncol = 3, 
                           sync = TRUE)

rice_reach

# Save as output
tmap_save(rice_reach, "figures/ML_outputs/rice_maps.jpeg", 
          height = 2.1, 
          width = 6,
          dpi = 600)

#-------------------------------------------------------------------------------
