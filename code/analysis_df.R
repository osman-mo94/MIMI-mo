################################################################################
################### SCRIPT FOR CREATING ANALYSIS DATA-FRAME ####################
################################################################################

# Install and load required packages:
rq_packages <- c("readr", "tidyverse")

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

################################################################################
############################## END OF SCRIPT ###################################
################################################################################