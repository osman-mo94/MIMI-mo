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

# REACH RICE: 

# Dummy model: 

metrics <- c("Precision (PPV)", "Recall (sensitivity)", "F1 score", "Accuracy", 
             "Specificity")

dm.reach_rice <- c(0.6948, 0.4950, 0.5781, 0.4955, 0.4966)

dm.reach_rice <- data.frame(metrics, dm.reach_rice)

knitr::kable(dm.reach_rice, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")

# Logistic regression: 

LR.reach_rice <- c(0.8457, 0.6509, 0.7356, 0.6721, 0.7218)

LR.reach_rice <- data.frame(metrics, LR.reach_rice)

knitr::kable(LR.reach_rice, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")

# Random forest: 

RF.reach_rice <- c(0.8581, 0.6418, 0.7344, 0.6746, 0.7513)

RF.reach_rice <- data.frame(metrics, RF.reach_rice)

knitr::kable(RF.reach_rice, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")

# XGBoost: 

XG.reach_rice <- c(0.8530, 0.6474, 0.7361, 0.6715, 0.7299)

XG.reach_rice <- data.frame(metrics, XG.reach_rice)

knitr::kable(XG.reach_rice, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")


