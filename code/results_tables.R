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

# MODEL COMPARISON: 

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

XG.reach_rice <- c(0.8541, 0.6545, 0.7411, 0.6745, 0.7239)

XG.reach_rice <- data.frame(metrics, XG.reach_rice)

knitr::kable(XG.reach_rice, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")

# Tidy up environment: 
rm(list = c("dm.reach_rice", "LR.reach_rice", "RF.reach_rice", "XG.reach_rice"))

#-------------------------------------------------------------------------------

# REACH OF RICE: 

# Stratified by geography: 
urban.reach_rice <- c(0.9092, 0.9282, 0.9186, 0.8564, 0.3621)

rural.reach_rice <- c(0.8089, 0.4918, 0.6117, 0.6003, 0.7933)

geog.reach_rice <- data.frame(metrics, urban.reach_rice, rural.reach_rice)

knitr::kable(geog.reach_rice, col.names = c("", "Urban", "Rural")) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by SEP: 
quintiles <- c("1 (lowest)", "2", "3", "4", "5 (highest)")

f1.reach_rice <- c(0.3684, 0.6074, 0.7966, 0.8532, 0.8362)

sep.reach_rice <- data.frame(quintiles, f1.reach_rice)

knitr::kable(sep.reach_rice, col.names = c("Socio-economic quintile", 
                                           "F1 score")) %>% 
  kable_classic(html_font = "helvetica")

# Tidy up environment: 
rm(list = c("geog.reach_rice", "sep.reach_rice", "f1.reach_rice", 
            "rural.reach_rice", "urban.reach_rice"))

#-------------------------------------------------------------------------------

# REACH OF WHEAT FLOUR: 

# Overall: 
XG.reach_wheat <- c(0.8645, 0.7006, 0.7740, 0.7066, 0.7216)

XG.reach_wheat <- data.frame(metrics, XG.reach_wheat)

knitr::kable(XG.reach_wheat, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by geography: 
urban.reach_wheat <- c(0.8954, 0.9114, 0.9033, 0.8339, 0.3902)

rural.reach_wheat <- c(0.8377, 0.5767, 0.6831, 0.6488, 0.7866)

geog.reach_wheat <- data.frame(metrics, urban.reach_wheat, rural.reach_wheat)

knitr::kable(geog.reach_wheat, col.names = c("", "Urban", "Rural")) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by SEP: 
f1.reach_wheat <- c(0.0921, 0.5297, 0.8025, 0.8926, 0.9328)

sep.reach_wheat <- data.frame(quintiles, f1.reach_wheat)

knitr::kable(sep.reach_wheat, col.names = c("Socio-economic quintile", 
                                           "F1 score")) %>% 
  kable_classic(html_font = "helvetica")

# Tidy up environment: 
rm(list = c("geog.reach_wheat", "sep.reach_wheat", "XG.reach_wheat", 
            "f1.reach_wheat", "rural.reach_wheat", "urban.reach_wheat"))

#-------------------------------------------------------------------------------

# REACH OF MAIZE FLOUR: 

# Overall: 
XG.reach_maize <- c(0.2408, 0.6355, 0.3493, 0.6738, 0.6799)

XG.reach_maize <- data.frame(metrics, XG.reach_maize)

knitr::kable(XG.reach_maize, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by geography: 
urban.reach_maize <- c(0.2794, 0.9252, 0.4292, 0.4271, 0.2760)

rural.reach_maize <- c(0.1654, 0.3125, 0.2164, 0.3125, 0.8351)

geog.reach_maize <- data.frame(metrics, urban.reach_maize, rural.reach_maize)

knitr::kable(geog.reach_maize, col.names = c("", "Urban", "Rural")) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by SEP: 
f1.reach_maize <- c(0.3030, 0.3324, 0.3369, 0.3533, 0.3836)

sep.reach_maize <- data.frame(quintiles, f1.reach_maize)

knitr::kable(sep.reach_maize, col.names = c("Socio-economic quintile", 
                                            "F1 score")) %>% 
  kable_classic(html_font = "helvetica")

rm(list = c("geog.reach_maize", "sep.reach_maize", "XG.reach_maize",
            "f1.reach_maize", "rural.reach_maize", "urban.reach_maize"))

#-------------------------------------------------------------------------------

# COVERAGE OF RICE: 

# Overall: 
XG.coverage_rice <- c(0.8390, 0.6710, 0.7457, 0.6868, 0.7209)

XG.coverage_rice <- data.frame(metrics, XG.coverage_rice)

knitr::kable(XG.coverage_rice, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by geography: 
urban.coverage_rice <- c(0.8969, 0.9097, 0.9033, 0.8331, 0.3750)

rural.coverage_rice <- c(0.7808, 0.5152, 0.6208, 0.6194, 0.7787)

geog.coverage_rice <- data.frame(metrics, urban.coverage_rice, rural.coverage_rice)

knitr::kable(geog.coverage_rice, col.names = c("", "Urban", "Rural")) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by SEP: 
f1.coverage_rice <- c(0.4174, 0.6293, 0.8026, 0.8649, 0.8391)

sep.coverage_rice <- data.frame(quintiles, f1.coverage_rice)

knitr::kable(sep.coverage_rice, col.names = c("Socio-economic quintile", 
                                            "F1 score")) %>% 
  kable_classic(html_font = "helvetica")

#-------------------------------------------------------------------------------

# COVERAGE OF WHEAT FLOUR: 

# Overall: 
XG.coverage_wheatf <- c(0.8675, 0.7077, 0.7795, 0.7192, 0.7461)

XG.coverage_wheatf <- data.frame(metrics, XG.coverage_wheatf)

knitr::kable(XG.coverage_wheatf, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by geography: 
urban.coverage_wheatf <- c(0.8972, 0.9066, 0.9018, 0.8314, 0.3911)

rural.coverage_wheatf <- c(0.8406, 0.5836, 0.6889, 0.6674, 0.8108)

geog.coverage_wheatf <- data.frame(metrics, urban.coverage_wheatf, rural.coverage_wheatf)

knitr::kable(geog.coverage_wheatf, col.names = c("", "Urban", "Rural")) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by SEP: 
f1.coverage_wheatf <- c(0.0804, 0.6415, 0.8398, 0.8955, 0.9258)

sep.coverage_wheatf <- data.frame(quintiles, f1.coverage_wheatf)

knitr::kable(sep.coverage_wheatf, col.names = c("Socio-economic quintile", 
                                              "F1 score")) %>% 
  kable_classic(html_font = "helvetica")

rm(list = c("geog.coverage_rice", "geog.coverage_wheatf", "sep.coverage_rice",
            "sep.coverage_wheatf", "XG.coverage_rice", "XG.coverage_wheatf", 
            "f1.coverage_rice", "f1.coverage_wheatf", "rural.coverage_rice",
            "rural.coverage_wheatf", "urban.coverage_rice", "urban.coverage_wheatf"))

#-------------------------------------------------------------------------------

# COVERAGE OF MAIZE FLOUR: 

# Overall: 
XG.coverage_maizef <- c(0.2435, 0.6168, 0.3492, 0.6763, 0.6860)

XG.coverage_maizef <- data.frame(metrics, XG.coverage_maizef)

knitr::kable(XG.coverage_maizef, col.names = NULL) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by geography: 
urban.coverage_maizef <- c(0.2510, 0.9124, 0.3937, 0.3730, 0.2180)

rural.coverage_maizef <- c(0.2245, 0.3212, 0.2643, 0.8161, 0.8728)

geog.coverage_maizef <- data.frame(metrics, urban.coverage_maizef, 
                                   rural.coverage_maizef)

knitr::kable(geog.coverage_maizef, col.names = c("", "Urban", "Rural")) %>% 
  kable_classic(html_font = "helvetica")

# Stratified by SEP: 
f1.coverage_maizef <- c(0.2775, 0.3397, 0.3632, 0.3891, 0.3297)

sep.coverage_maizef <- data.frame(quintiles, f1.coverage_maizef)

knitr::kable(sep.coverage_maizef, col.names = c("Socio-economic quintile", 
                                                "F1 score")) %>% 
  kable_classic(html_font = "helvetica")

