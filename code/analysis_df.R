################################################################################
################### SCRIPT FOR CREATING ANALYSIS DATA-FRAME ####################
################################################################################

# Install and load required packages:
rq_packages <- c("readr", "tidyverse", "recipes")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Source scripts for target variables and predictive inputs:

source("code/create_targets.R")
source("code/predictive_inputs.R")

#-------------------------------------------------------------------------------

# JOIN TARGETS AND PREDICTIVE INPUTS: 

# Create analysis data-frame with desired targets (access to fortification vehicles, 
# and risk of inadequate diet): 

analysis_df <- target_variables %>% 
  dplyr::select(hhid, rice_combined, wheat_flour, maize_flour, risk_MND)

# Join desired predictive inputs: 
analysis_df <- analysis_df %>% 
  left_join(predictive_inputs, 
            by = "hhid")

# Remove objects no longer needed: 
rm(list = c("predictive_inputs", "target_variables"))

#-------------------------------------------------------------------------------

# Add-ADM2 location: 

cover <- read_csv("NLSS_data/Household/secta_cover.csv")
household_locations <- cover %>% dplyr::select("hhid", "lga")

analysis_df <- analysis_df %>% left_join(household_locations, by = "hhid")

#-------------------------------------------------------------------------------

# IMPUTE MISSING DATA: 

# View which columns have missing data: 
vapply(analysis_df, function(x) mean(is.na(x)), c(num = 0))

# Use imputation via bagged trees to deal with missing data:
impute_recipe <- recipe(rice_combined ~ ., data = analysis_df) %>% 
  # Firstly specify which variables require imputation:
  step_impute_bag(risk_MND, material_floor, water_source, toilet_facility, radio,
                  tv, smart_phones, reg_mobile_phone, fridge, cars_vehicles, 
                  proportion_primary, proportion_secondary, proportion_higher, 
                  proportion_wage_salary, proportion_own_agriculture, 
                  proportion_own_NFE, proportion_trainee_apprentice,
                  total_consumption, consumption_quintile,
                  trained = FALSE, 
                  impute_with = imp_vars(all_predictors()), # using all vars
                  trees = 25, # 25 bagged trees
                  seed_val = 1, # setting the seed
                  skip = FALSE)

imp_model <- prep(impute_recipe, training = analysis_df)

imputed_data <- bake(imp_model, new_data = analysis_df, everything())

# I will now use this data-frame for fitting the ML models going forwards: 
analysis_df <- imputed_data

#-------------------------------------------------------------------------------

# TIDY: 

# Remove unwanted objects: 
rm(list = c("imp_model", "impute_recipe", "imputed_data", "cover",
            "household_locations"))

#-------------------------------------------------------------------------------

# WRITE CSV: 
write_csv(analysis_df, file = "NLSS_data/analysis_df.csv")

################################################################################
############################## END OF SCRIPT ###################################
################################################################################