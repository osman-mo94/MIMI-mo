################################################################################
########################## FITTING XGBoost MODELS ##############################
################################################################################

# Install and load required packages:
rq_packages <- c("tidyverse", "caret", "xgboost", "doMC", "vip")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN DATA:

# Read in analysis dataframe: 
analysis_df <- read_csv("NLSS_data/analysis_df.csv")

#-------------------------------------------------------------------------------

# Convert target variable to factor: 
# Target variable needs to be converted to factor: 
analysis_df$rice_combined <- factor(analysis_df$rice_combined, 
                                    labels = c("No", "Yes"), 
                                    levels = c("No", "Yes"))

analysis_df$wheat_flour <- factor(analysis_df$wheat_flour, 
                                  levels = c("No", "Yes"),
                                  labels = c("No", "Yes"))

analysis_df$maize_flour <- factor(analysis_df$maize_flour, 
                                  levels = c("No", "Yes"),
                                  labels = c("No", "Yes"))

analysis_df$risk_MND <- factor(analysis_df$risk_MND, 
                               levels = c("No", "Yes"),
                               labels = c("No", "Yes"))

analysis_df$geography <- factor(analysis_df$geography, 
                                levels = c("urban", "rural"),
                                labels = c("urban", "rural"))

# Re-organise position of columns: 
analysis_df <- analysis_df %>% relocate(rice_combined, .after = hhid)

#-------------------------------------------------------------------------------

# TRAIN-TEST SPLIT: 

# Stratify split by LGA to ensure that we have enough predictions for mapping:
index <- partition(y = analysis_df$lga, p = c(train = 0.8, test = 0.2), 
                   split_into_list = FALSE)

# Create training and test sets:
train <- analysis_df[index == "train", ]
test <- analysis_df[index == "test", ]

#-------------------------------------------------------------------------------

# IMBALANCED DATA: 

# Use downsampling
set.seed(123)
rice_train <- downSample(x = train[, -ncol(train)],
                         y = train$rice_combined)

# View distribution of classes in balanced dataset
table(rice_train$rice_combined)

#-------------------------------------------------------------------------------

# Specify 5-fold cross validation for train control settings: 
XGb_ctrl <- trainControl(method = "cv", # K-fold cross validation
                         number = 5, # Using 5 folds
                         savePredictions = "all", # For confusion matrix
                         classProbs = TRUE, # Keep class probabilities
                         summaryFunction = twoClassSummary) # For ROC, sens, spec

# Fit an XGboost model using defualt parameter settings:
 
default_XGb <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                       water_source + toilet_facility + n_per_room + agricultural_land +
                       radio + tv + smart_phones + reg_mobile_phone + fridge + 
                       cars_vehicles + proportion_male + proportion_christian + 
                       proportion_muslim + proportion_traditional + proportion_primary + 
                       proportion_secondary + proportion_higher + 
                       proportion_wage_salary + proportion_own_agriculture + 
                       proportion_own_NFE + proportion_trainee_apprentice + geography,
                     data = train_data, 
                     method = "xgbTree",
                     trControl = XGb_ctrl)

default_XGb

confusionMatrix(default_XGb, positive = "Yes")

#-------------------------------------------------------------------------------

# HYPERPARAMETER TUNING: 

# Specify hyperparameters to try in tuning grid: 
tuning_grid <- expand.grid(nrounds = c(100, 500, 1000),
                           max_depth = c(2, 4, 6), 
                           eta = c(0.05, 0.1, 0.3),
                           gamma = c(0, 10),
                           colsample_bytree = 1,
                           min_child_weight = 1,
                           subsample = 1)

# Respecify trainControl with random search this time:
XGb_ctrl <- trainControl(method = "cv", 
                         number = 5, 
                         savePredictions = "all", 
                         classProbs = TRUE, 
                         summaryFunction = twoClassSummary,
                         search = "grid", 
                         verboseIter = TRUE) 

# Run models in parallel using parallel computing:
registerDoMC(cores = 4)

XGb_tuning <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                      water_source + toilet_facility + n_per_room + agricultural_land +
                      radio + tv + smart_phones + reg_mobile_phone + fridge + 
                      cars_vehicles + proportion_male + proportion_christian + 
                      proportion_muslim + proportion_traditional + proportion_primary + 
                      proportion_secondary + proportion_higher + 
                      proportion_wage_salary + proportion_own_agriculture + 
                      proportion_own_NFE + proportion_trainee_apprentice + geography,
                    data = rice_train, 
                    method = "xgbTree",
                    trControl = XGb_ctrl,
                    tuneGrid = tuning_grid, 
                    verbose = TRUE)

confusionMatrix(XGb_tuning)

# Final values for model
# nrounds = 100
# max_depth = 4
# eta = 0.1
# gamma = 0,
# colsample_bytree = 1
# min_child_weight =1
# subsample = 1 

  
#-------------------------------------------------------------------------------

# FINAL XGB model: 

# Fit with the optimal hyperparameters:
tuning_grid <- expand.grid(nrounds = c(400),
                           max_depth = c(4),
                           eta = c(0.05),
                           gamma = c(0),
                           colsample_bytree = c(0.3),
                           min_child_weight = c(1),
                           subsample = c(0.3))


# Run models in parallel using parallel computing:
registerDoMC(cores = 4)

XGb_final <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                      water_source + toilet_facility + n_per_room + agricultural_land +
                      radio + tv + smart_phones + reg_mobile_phone + fridge + 
                      cars_vehicles + proportion_male + proportion_christian + 
                      proportion_muslim + proportion_traditional + proportion_primary + 
                      proportion_secondary + proportion_higher + 
                      proportion_wage_salary + proportion_own_agriculture + 
                      proportion_own_NFE + proportion_trainee_apprentice + geography,
                    data = rice_train, 
                    method = "xgbTree",
                    trControl = XGb_ctrl,
                    tuneGrid = tuning_grid, 
                    verbose = TRUE)

xgb_predictions <- predict(XGb_final, test)

xgb.prec_recall <- confusionMatrix(xgb_predictions, test$rice_combined, 
                                  positive = "Yes", 
                                  mode = "prec_recall")

xgb.prec_recall

xgb.sens_spec <- confusionMatrix(xgb_predictions, test$rice_combined, 
                                positive = "Yes", 
                                mode = "sens_spec")

xgb.sens_spec

# Create visualisation for confusion matrix:
xgb_matrix <- as.data.frame(xgb.prec_recall$table)
xgb_matrix$Prediction <- factor(xgb_matrix$Prediction, 
                               levels=rev(levels(xgb_matrix$Prediction)))

ggplot(xgb_matrix, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to rice","No access")) +
  scale_y_discrete(labels=c("No access","Access to rice"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/xgb_CM.jpeg", 
       width = 6,
       height = 4, 
       dpi = 600)
#-------------------------------------------------------------------------------

# VARIABLE IMPORTANCE:

# Create variable importance plot for all features in the model:
vip(XGb_final, num_features = 25)

ggsave("figures/ML_outputs/vipXGB.jpeg",
       width = 6, 
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# SAVE PREDICTIONS AND ASSOCIATED HHID's AS DATAFRAME: 

xgb_predictions <- as.data.frame(xgb_predictions)
xgb_predictions$hhid <- test$hhid

write_csv(xgb_predictions, "map_data/ML_predictions/xgb_rice_predictions.csv")

################################################################################
############################## END OF SCRIPT ###################################
################################################################################


