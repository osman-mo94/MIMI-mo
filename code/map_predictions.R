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
reach_wf_predictions <- read_csv("map_data/ML_predictions/xgb_reach_wheat.csv")
reach_mf_predictions <- read_csv("map_data/ML_predictions/xgb_reach_maize.csv")
cov_rice_predictions <- read_csv("map_data/ML_predictions/xgb_rice_coverage.csv")
cov_wf_predictions <- read_csv("map_data/ML_predictions/xgb_cov_wheat.csv")
cov_mf_predictions <- read_csv("map_data/ML_predictions/xgb_cov_maize.csv")

#-------------------------------------------------------------------------------

# JOIN PREDICTIONS AND LOCATIONS: 
xgb_rice_predictions <- xgb_rice_predictions %>% 
  left_join(household_locations, by = "hhid")

reach_wf_predictions <- reach_wf_predictions %>% 
  left_join(household_locations, by = "hhid")

reach_mf_predictions <- reach_mf_predictions %>% 
  left_join(household_locations, by = "hhid")

cov_rice_predictions <- cov_rice_predictions %>% 
  left_join(household_locations, by = "hhid")

cov_wf_predictions <- cov_wf_predictions %>% 
  left_join(household_locations, by = "hhid")

cov_mf_predictions <- cov_mf_predictions %>% 
  left_join(household_locations, by = "hhid")

#-------------------------------------------------------------------------------

# ADD SURVEY WEIGHTS: 

# Get survey weights and enumeration areas: 
cover <- read_csv("NLSS_data/Household/secta_cover.csv")

survey_weights <- cover %>% dplyr::select(hhid, wt_final, ea)

# Add survey weights to xgb_rice_predictions by enumartion area: 
xgb_rice_predictions <- xgb_rice_predictions %>% 
  left_join(survey_weights, by = "hhid")

reach_wf_predictions <- reach_wf_predictions %>% 
  left_join(survey_weights, by = "hhid")

reach_mf_predictions <- reach_mf_predictions %>% 
  left_join(survey_weights, by = "hhid")

cov_rice_predictions <- cov_rice_predictions %>% 
  left_join(survey_weights, by = "hhid")

cov_wf_predictions <- cov_wf_predictions %>% 
  left_join(survey_weights, by = "hhid")

cov_mf_predictions <- cov_mf_predictions %>% 
  left_join(survey_weights, by = "hhid")

rm(list = c("cover", "survey_weights"))

# Find and replace any missing survey weights, also ensure that predictions 
# are factor variables: 
which(is.na(xgb_rice_predictions$wt_final))
xgb_rice_predictions$xgb_predictions <- as.factor(xgb_rice_predictions$xgb_predictions)

which(is.na(reach_wf_predictions$wt_final))
reach_wf_predictions$wf.xgb_predictions <- as.factor(reach_wf_predictions$wf.xgb_predictions)

which(is.na(reach_mf_predictions$wt_final))
reach_mf_predictions$mf.xgb_predictions <- as.factor(reach_mf_predictions$mf.xgb_predictions)

which(is.na(cov_rice_predictions$wt_final))
cov_rice_predictions$ricov.xgb_predictions <- as.factor(cov_rice_predictions$ricov.xgb_predictions)

which(is.na(cov_wf_predictions$wt_final))
cov_wf_predictions$wfcov.xgb_predictions <- as.factor(cov_wf_predictions$wfcov.xgb_predictions)

which(is.na(cov_mf_predictions$wt_final))
cov_mf_predictions$mfcov.xgb_predictions <- as.factor(cov_mf_predictions$mfcov.xgb_predictions)

# Create tbl_svy object using xgb_rice_predictions dataframe: 
svy_xgb_rice <- xgb_rice_predictions %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final,
                          strata = ea)

svy_reach_wf <- reach_wf_predictions %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final, 
                          strata = ea)
  
svy_reach_mf <- reach_mf_predictions %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final, 
                          strata = ea)

svy_cov_rice <- cov_rice_predictions %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final, 
                          strata = ea)

svy_cov_wf <- cov_wf_predictions %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final, 
                          strata = ea)

svy_cov_mf <- cov_mf_predictions %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final, 
                          strata = ea)
  
#-------------------------------------------------------------------------------

# PREDICTED REACH:

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# RICE: 

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

# WHEAT FLOUR: 

# State level: 
wf_reachADM1 <- svy_reach_wf %>% 
  group_by(state) %>% 
  summarise(wf_reach = survey_mean(wf.xgb_predictions == "Yes", 
                                   vartype = NULL))

# LGA level: 
wf_reachADM2 <- svy_reach_wf %>% 
  group_by(lga) %>% 
  summarise(wf_reach = survey_mean(wf.xgb_predictions == "Yes", 
                                   vartype = NULL))

# MAIZE FLOUR: 
# State level: 
mf_reachADM1 <- svy_reach_mf %>% 
  group_by(state) %>% 
  summarise(mf_reach = survey_mean(mf.xgb_predictions == "Yes", 
                                   vartype = NULL))

# LGA level: 
mf_reachADM2 <- svy_reach_mf %>% 
  group_by(lga) %>% 
  summarise(mf_reach = survey_mean(mf.xgb_predictions == "Yes", 
                                   vartype = NULL))

# PREDICTED COVERAGE: 
# RICE: 

# Calculate predicted reach at state level for rice: 
rice_covADM1 <- svy_cov_rice %>% 
  group_by(state) %>% 
  summarise(rice_coverage = survey_mean(ricov.xgb_predictions == "Yes", 
                                     vartype = NULL))

# Calculate predicted reach at LGA level for rice: 
rice_covADM2 <- svy_cov_rice %>% 
  group_by(lga) %>% 
  summarise(rice_coverage = survey_mean(ricov.xgb_predictions == "Yes", 
                                     vartype = NULL))

# WHEAT FLOUR: 

# State level: 
wf_covADM1 <- svy_cov_wf %>% 
  group_by(state) %>% 
  summarise(wf_coverage = survey_mean(wfcov.xgb_predictions == "Yes", 
                                   vartype = NULL))

# LGA level: 
wf_covADM2 <- svy_cov_wf %>% 
  group_by(lga) %>% 
  summarise(wf_coverage = survey_mean(wfcov.xgb_predictions == "Yes", 
                                   vartype = NULL))

# MAIZE FLOUR: 
# State level: 
mf_covADM1 <- svy_cov_mf %>% 
  group_by(state) %>% 
  summarise(mf_coverage = survey_mean(mfcov.xgb_predictions == "Yes", 
                                   vartype = NULL))

# LGA level: 
mf_covADM2 <- svy_cov_mf %>% 
  group_by(lga) %>% 
  summarise(mf_coverage = survey_mean(mfcov.xgb_predictions == "Yes", 
                                   vartype = NULL))

#-------------------------------------------------------------------------------

# MERGE PREDICTED REACH AND COVERAGE TO SHAPEFILES: 

# Reach
rice_reachADM1 <- nigeria_1 %>% left_join(rice_reachADM1, by = "state")
rice_reachADM2 <- nigeria_2 %>% left_join(rice_reachADM2, by = "lga")

wf_reachADM1 <- nigeria_1 %>% left_join(wf_reachADM1, by = "state")
wf_reachADM2 <- nigeria_2 %>% left_join(wf_reachADM2, by = "lga")

mf_reachADM1 <- nigeria_1 %>% left_join(mf_reachADM1, by = "state")
mf_reachADM2 <- nigeria_2 %>% left_join(mf_reachADM2, by = "lga")

# Coverage
rice_covADM1 <- nigeria_1 %>% left_join(rice_covADM1, by = "state")
rice_covADM2 <- nigeria_2 %>% left_join(rice_covADM2, by = "lga")

wf_covADM1 <- nigeria_1 %>% left_join(wf_covADM1, by = "state")
wf_covADM2 <- nigeria_2 %>% left_join(wf_covADM2, by = "lga")

mf_covADM1 <- nigeria_1 %>% left_join(mf_covADM1, by = "state")
mf_covADM2 <- nigeria_2 %>% left_join(mf_covADM2, by = "lga")

# Tidy environment: 
rm(list = c("svy_cov_mf", "svy_cov_rice", "svy_cov_wf", "svy_reach_mf",
            "svy_reach_wf", "cov_mf_predictions", "cov_rice_predictions",
            "cov_wf_predictions", "household_locations", "nigeria_1", 
            "nigeria_2", "reach_mf_predictions", "reach_wf_predictions"))

#-------------------------------------------------------------------------------
############################ PART 1: MAPS FOR REACH ############################
#-------------------------------------------------------------------------------

# CREATE MAPS FOR RICE

breaks <- c(0, 0.25, 0.5, 0.75, 1)

# Reach of rice at ADM1 level: 
rice_ADM1 <- tm_shape(rice_reachADM1) + 
  tm_fill(col = "rice_reach", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice (predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

rice_ADM1

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

ADM1_true.reach <- st_read("map_data/outputs/nigeria1_reach.shp")
ADM2_true.reach <- st_read("map_data/outputs/nigeria2_reach.shp")

# Ground truth ADM1: 
true.rice_ADM1 <- tm_shape(ADM1_true.reach) + 
  tm_fill(col = "rch_rc_", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

true.rice_ADM1

# Ground truth ADM2: 
true.rice_ADM2 <- tm_shape(ADM2_true.reach) + 
  tm_fill(col = "rch_rc_", breaks = breaks, palette = "Blues",
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
          labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
          textNA = "Missing Data",
          colorNA = "gray35") +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1) +
  tm_compass(north = 0, type = "arrow")

reach_legend2

# Arrange maps and legend in a single figure
rice_reach <- tmap_arrange(true.rice_ADM1, rice_ADM1, reach_legend2, 
                           true.rice_ADM2, rice_ADM2,
                           ncol = 3, 
                           sync = TRUE)

rice_reach

# Save as output
tmap_save(rice_reach, "figures/ML_outputs/rice_maps.jpeg", 
          height = 5, 
          width = 6,
          dpi = 600)

#-------------------------------------------------------------------------------

# Tidy up environment:

rm(list = c("rice_ADM1", "rice_ADM2", "rice_reach", 
            "rice_reachADM1", "rice_reachADM2", "svy_xgb_rice", 
            "true.rice_ADM1", "true.rice_ADM2", "xgb_rice_predictions"))

#-------------------------------------------------------------------------------

# REACH OF WHEAT FLOUR: 

# CREATE MAPS FOR WHEAT FLOUR

# Reach of wheat at ADM1 level: 
wf_ADM1 <- tm_shape(wf_reachADM1) + 
  tm_fill(col = "wf_reach", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour 
(predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

wf_ADM1

# Reach of wheat at ADM2 level:
wf_ADM2 <- tm_shape(wf_reachADM2) + 
  tm_fill(col = "wf_reach", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour 
(predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

wf_ADM2


# Ground truth ADM1: 
true.wf_ADM1 <- tm_shape(ADM1_true.reach) + 
  tm_fill(col = "rch_wht", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

true.wf_ADM1

# Ground truth ADM2: 
true.wf_ADM2 <- tm_shape(ADM2_true.reach) + 
  tm_fill(col = "rch_wht", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

true.wf_ADM2

# Arrange maps and legend in a single figure
wf_reach <- tmap_arrange(true.wf_ADM1, wf_ADM1, reach_legend2, 
                           true.wf_ADM2, wf_ADM2,
                           ncol = 3, 
                           sync = TRUE)

wf_reach

# Save as output
tmap_save(wf_reach, "figures/ML_outputs/wf_maps.jpeg", 
          height = 5, 
          width = 6,
          dpi = 600)

#-------------------------------------------------------------------------------

# REACH OF MAIZE FLOUR: 

# CREATE MAPS 

# Reach of maize at ADM1 level: 
mf_ADM1 <- tm_shape(mf_reachADM1) + 
  tm_fill(col = "mf_reach", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour 
(predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

mf_ADM1

# Reach of maize flour at ADM2 level:
mf_ADM2 <- tm_shape(mf_reachADM2) + 
  tm_fill(col = "mf_reach", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour 
(predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

mf_ADM2


# Ground truth ADM1: 
true.mf_ADM1 <- tm_shape(ADM1_true.reach) + 
  tm_fill(col = "rch_mzf", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

true.mf_ADM1

# Ground truth ADM2: 
true.mf_ADM2 <- tm_shape(ADM2_true.reach) + 
  tm_fill(col = "rch_mzf", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

true.mf_ADM2

# Arrange maps and legend in a single figure
mf_reach <- tmap_arrange(true.mf_ADM1,  mf_ADM1, reach_legend2, 
                         true.mf_ADM2, mf_ADM2,
                         ncol = 3, 
                         sync = TRUE)

mf_reach

# Save as output
tmap_save(mf_reach, "figures/ML_outputs/mf_maps.jpeg", 
          height = 5, 
          width = 6,
          dpi = 600)

# Tidy up environment: 
rm(list = c("mf_reach", "true.mf_ADM1", "true.mf_ADM2", "true.wf_ADM1", 
            "true.wf_ADM2", "wf_reach", "ADM1_true.reach", "ADM2_true.reach"))

#-------------------------------------------------------------------------------
########################## PART 2: MAPS FOR COVERAGE ###########################
#-------------------------------------------------------------------------------

# CREATE MAPS FOR RICE

breaks <- c(0, 0.25, 0.5, 0.75, 1)

# Coverage of rice at ADM1 level: 
cov.rice_ADM1 <- tm_shape(rice_covADM1) + 
  tm_fill(col = "rice_coverage", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice (predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

cov.rice_ADM1

# Coverage of rice at ADM2 level:
cov.rice_ADM2 <- tm_shape(rice_covADM2) + 
  tm_fill(col = "rice_coverage", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice (predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

cov.rice_ADM2


# Read in ground truth for coverage, this will be used for comparison: 

ADM1_true.coverage <- st_read("map_data/outputs/nigeria1_coverage.shp")
ADM2_true.coverage <- st_read("map_data/outputs/nigeria2_coverage.shp")

# Ground truth ADM1: 
truecov.rice_ADM1 <- tm_shape(ADM1_true.coverage) + 
  tm_fill(col = "cvrg_r_", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

truecov.rice_ADM1

# Ground truth ADM2: 
truecov.rice_ADM2 <- tm_shape(ADM2_true.coverage) + 
  tm_fill(col = "cvrg_r_", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Rice (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

truecov.rice_ADM2

# Create legend: 

cov_legend2 <- tm_shape(ADM2_true.coverage) + 
  tm_fill(col = "cvrg_r_", breaks = breaks, palette = "Blues",
          title = "Coverage (% of households with apparently inadequate
diet with access to the fortification vehicle)",
          labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
          textNA = "Missing Data",
          colorNA = "gray35") +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1) +
  tm_compass(north = 0, type = "arrow")

cov_legend2

# Arrange maps and legend in a single figure
rice_coverage <- tmap_arrange(truecov.rice_ADM1, cov.rice_ADM1, cov_legend2, 
                           truecov.rice_ADM2, cov.rice_ADM2,
                           ncol = 3, 
                           sync = TRUE)

rice_coverage

# Save as output
tmap_save(rice_coverage, "figures/ML_outputs/ricecoverage_maps.jpeg", 
          height = 5, 
          width = 6,
          dpi = 600)

# Tidy environment: 
rm(list = c("truecov.rice_ADM1", "cov.rice_ADM1", "truecov.rice_ADM2", 
            "cov.rice_ADM2", "rice_covADM1", "rice_covADM2", "rice_coverage", 
            "rice_reach"))

#-------------------------------------------------------------------------------

# WHEAT FLOUR:

# Coverage of wheat flour at ADM1 level: 
cov.wf_ADM1 <- tm_shape(wf_covADM1) + 
  tm_fill(col = "wf_coverage", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour 
(predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

cov.wf_ADM1

# Coverage of wheat flour at ADM2 level:
cov.wf_ADM2 <- tm_shape(wf_covADM2) + 
  tm_fill(col = "wf_coverage", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour 
(predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

cov.wf_ADM2

# Ground truth ADM1: 
truecov.wf_ADM1 <- tm_shape(ADM1_true.coverage) + 
  tm_fill(col = "cvrg_wh", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

truecov.wf_ADM1

# Ground truth ADM2: 
truecov.wf_ADM2 <- tm_shape(ADM2_true.coverage) + 
  tm_fill(col = "cvrg_wh", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Wheat flour (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

truecov.wf_ADM2

# Arrange maps and legend in a single figure
wf_coverage <- tmap_arrange(truecov.wf_ADM1, cov.wf_ADM1, cov_legend2, 
                              truecov.wf_ADM2, cov.wf_ADM2,
                              ncol = 3, 
                              sync = TRUE)

wf_coverage

# Save as output
tmap_save(wf_coverage, "figures/ML_outputs/wheatf_coverage_maps.jpeg", 
          height = 5, 
          width = 6,
          dpi = 600)

#-------------------------------------------------------------------------------

# MAIZE FLOUR: 

# Coverage of maize flour at ADM1 level: 
cov.mf_ADM1 <- tm_shape(mf_covADM1) + 
  tm_fill(col = "mf_coverage", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour 
(predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

cov.mf_ADM1

# Coverage of wheat flour at ADM2 level:
cov.mf_ADM2 <- tm_shape(mf_covADM2) + 
  tm_fill(col = "mf_coverage", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour 
(predicted by XGboost)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

cov.mf_ADM2

# Ground truth ADM1: 
truecov.mf_ADM1 <- tm_shape(ADM1_true.coverage) + 
  tm_fill(col = "cvrg_mz", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

truecov.mf_ADM1

# Ground truth ADM2: 
truecov.mf_ADM2 <- tm_shape(ADM2_true.coverage) + 
  tm_fill(col = "cvrg_mz", breaks = breaks, palette = "Blues",
          colorNA = "gray35") +
  tm_layout(main.title = "Maize flour (Ground truth)", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

truecov.mf_ADM2

# Arrange maps and legend in a single figure
mf_coverage <- tmap_arrange(truecov.mf_ADM1, cov.mf_ADM1, cov_legend2, 
                            truecov.mf_ADM2, cov.mf_ADM2,
                            ncol = 3, 
                            sync = TRUE)

mf_coverage

# Save as output
tmap_save(mf_coverage, "figures/ML_outputs/maizef_coverage_maps.jpeg", 
          height = 5, 
          width = 6,
          dpi = 600)

################################################################################
############################## END OF SCRIPT ###################################
################################################################################
