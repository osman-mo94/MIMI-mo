################################################################################
######################### CREATING RESULTS TABLES ##############################
################################################################################

# Install and load required packages:
rq_packages <- c("tidyverse", "knitr", "table1", "kableExtra")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Rice reach table: 

# Specify column names
columns <- c("precision", "recall", "accuracy", "f1", "specificity", "geography",
             "SEP", "overall_population", "dummy_model")
# Create entries 
dummy_model <- c(0.6948, 0.4950, 0.5781, 0.4955, 0.4966, NA, NA, NA, 1)
XGB_overall <- c(0.8574, 0.6449, 0.7361, 0.6706, 0.7347, NA, NA, 1, NA)
XGB_urban <- c(0.8954, 0.9218, 0.9084, 0.8411, 0.3663, "urban", NA, NA, NA)
XGB_rural <- c(0.8232, 0.4710, 0.5992, 0.4710, 0.8263, "rural", NA, NA, NA)
XGB_sep1 <- c(0.4636, 0, 0, 0, 0, NA, "1 (Lowest)", NA, NA)
XGB_sep2 <- c(0.4636, 0, 0, 0, 0, NA, "2", NA, NA)
XGB_sep3 <- c(0.4636, 0, 0, 0, 0, NA, "3", NA, NA)
XGB_sep4 <- c(0.4636, 0, 0, 0, 0, NA, "4", NA, NA)
XGB_sep5 <- c(0.4636, 0, 0, 0, 0, NA, "5 (Highest)", NA, NA)

# Create a dataframe: 

rice_reach <- as.data.frame(columns, dummy_model, XGB_overall, XGB_urban, 
                            XGB_rural, XGB_sep1, XGB_sep2, XGB_sep3, XGB_sep4,
                            XGB_sep5)
