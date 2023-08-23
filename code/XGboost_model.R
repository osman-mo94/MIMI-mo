################################################################################
########################## FITTING XGBoost MODELS ##############################
################################################################################

# Install and load required packages:
rq_packages <- c("tidyverse", "caret", "xgboost", "doMC", "vip", "splitTools",
                 "precrec", "pROC")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# PART 1: REACH (USING FULL ANALYSIS DATAFRAME)

# Read in analysis dataframe: 
analysis_df <- read_csv("NLSS_data/analysis_df.csv")

#-------------------------------------------------------------------------------

# Ensure that target variables are factor: 
analysis_df$rice_combined <- factor(analysis_df$rice_combined, 
                                    levels = c("No", "Yes"),
                                    labels = c("No", "Yes"))

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
analysis_df <- analysis_df %>% relocate(wheat_flour, .after = rice_combined)
analysis_df <- analysis_df %>% relocate(maize_flour, .after = wheat_flour)
analysis_df <- analysis_df %>% relocate(risk_MND, .after = maize_flour)

#-------------------------------------------------------------------------------

# TRAIN-TEST SPLIT: 

# Stratify split by LGA to ensure that we have enough predictions for mapping:
set.seed(100)

index <- partition(y = analysis_df$lga, p = c(train = 0.8, test = 0.2), 
                   split_into_list = FALSE)

# Create training and test sets:
train <- analysis_df[index == "train", ]
test <- analysis_df[index == "test", ]

#-------------------------------------------------------------------------------

# IMBALANCED DATA: 

# Use downsampling to balance training data for each fortification vehicle:

# For rice: 
set.seed(123)
rice_train <- downSample(x = train[, -ncol(train)],
                         y = train$rice_combined)

# View distribution of classes in balanced dataset
table(rice_train$rice_combined)

rice_train <- rice_train %>% dplyr::select(-Class)

# For wheat flour: 
set.seed(123)
wheatf_train <- downSample(x = train[, -ncol(train)],
                           y = train$wheat_flour)

table(wheatf_train$wheat_flour)

wheatf_train <- wheatf_train %>% dplyr::select(-Class)

# For maize flour: 
set.seed(123)
maizef_train <- downSample(x = train[, - ncol(train)],
                           y = train$maize_flour)

table(maizef_train$maize_flour)

maizef_train <- maizef_train %>% dplyr::select(-Class)


#-------------------------------------------------------------------------------

# RICE

# Specify 5-fold cross validation for train control settings: 
XGb_ctrl <- trainControl(method = "cv", # K-fold cross validation
                         number = 5, # Using 5 folds
                         savePredictions = "all", # For confusion matrix
                         search = "random", # Grid search for parameter tuning
                         classProbs = TRUE, # Keep class probabilities
                         summaryFunction = twoClassSummary) # For ROC, sens, spec

# Fit an XGboost model using defualt parameter settings:

set.seed(23) 

# Train the model including hyperparameter tuning: 
tuning_XGb <- train(rice_combined ~ geography + total_consumption + radio + tv +
                      fridge + cars_vehicles + mobile_phone + dwelling_free +
                      dwelling_rented + dwelling_owned + material_floor + 
                      electricity + water_source + open_defecaetion + 
                      toilet_unimproved + toilet_improved + n_per_room +
                      agricultural_land + proportion_male + proportion_primary +
                      proportion_secondary + proportion_higher + 
                      proportion_wage_salary + proportion_own_agriculture + 
                      proportion_own_NFE,
                     data = rice_train, 
                     method = "xgbTree",
                     trControl = XGb_ctrl)

tuning_XGb

confusionMatrix(tuning_XGb, positive = "Yes")

# Store the optimal hyper-parameters
optimal_nrounds <- tuning_XGb$bestTune$nrounds
optimal_max_depth <- as.numeric(tuning_XGb$bestTune$max_depth)
optimal_eta <- tuning_XGb$bestTune$eta
optimal_gamma <- tuning_XGb$bestTune$gamma
optimal_colsample_bytree <- tuning_XGb$bestTune$colsample_bytree
optimal_min_child_weight <- as.numeric(tuning_XGb$bestTune$min_child_weight)
optimal_subsample <- tuning_XGb$bestTune$subsample

# nrounds = 456, max_depth = 2, eta = 0.23, gamma = 1.39, colsample_bytree = 0.68,
# min_child_weight = 9, subsample = 0.76 (2 dp for)

#-------------------------------------------------------------------------------

# FINAL XGB model: 

# Fit with the optimal hyperparameters:
tuning_grid <- expand.grid(nrounds = c(optimal_nrounds),
                           max_depth = c(optimal_max_depth),
                           eta = c(optimal_eta),
                           gamma = c(optimal_gamma),
                           colsample_bytree = c(optimal_colsample_bytree),
                           min_child_weight = c(optimal_min_child_weight),
                           subsample = c(optimal_subsample))


# Run models in parallel using parallel computing:
registerDoMC(cores = 4)

set.seed(50)

XGb_final <- train(rice_combined ~ geography + total_consumption + radio + tv +
                     fridge + cars_vehicles + mobile_phone + dwelling_free +
                     dwelling_rented + dwelling_owned + material_floor + 
                     electricity + water_source + open_defecaetion + 
                     toilet_unimproved + toilet_improved + n_per_room +
                     agricultural_land + proportion_male + proportion_primary +
                     proportion_secondary + proportion_higher + 
                     proportion_wage_salary + proportion_own_agriculture + 
                     proportion_own_NFE,
                    data = rice_train, 
                    method = "xgbTree",
                    trControl = XGb_ctrl,
                    tuneGrid = tuning_grid, 
                    verbose = TRUE)

xgb_predictions <- predict(tuning_XGb, test)

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
ggsave("figures/ML_outputs/rice/xgb_CM.jpeg", 
       width = 6,
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# VARIABLE IMPORTANCE:

# Create variable importance plot for all features in the model:
vip(XGb_final, num_features = 26)

ggsave("figures/ML_outputs/rice/vipXGB.jpeg",
       width = 6, 
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

rice_ROC <- roc(response = as.numeric(test$rice_combined),
                predictor = as.numeric(xgb_predictions),
                smoothed = TRUE, 
                plot = TRUE, 
                auc.polygon = TRUE, 
                max.auc.polygon = TRUE,
                print.auc = TRUE, 
                show.thres = TRUE)

title("XGBoost", line = 2.5)

#-------------------------------------------------------------------------------

# SAVE PREDICTIONS AND ASSOCIATED HHID's AS DATAFRAME: 

xgb_predictions <- as.data.frame(xgb_predictions)
xgb_predictions$hhid <- test$hhid

write_csv(xgb_predictions, "map_data/ML_predictions/xgb_rice_predictions.csv")

#-------------------------------------------------------------------------------

# Stratify performance metrics for urban vs. rural populations: 

# Urban subsample of data: 
urban_test <- test %>% filter(geography == "urban")

# Urban predictions: 
urban.xgb_predictions <- predict(XGb_final, urban_test)

# Precision and recall for urban populations: 
confusionMatrix(urban.xgb_predictions, urban_test$rice_combined, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(urban.xgb_predictions, urban_test$rice_combined, 
                mode = "sens_spec", 
                positive = "Yes")

# Rural subsample of data: 
rural_test <- test %>% filter(geography == "rural")

# Urban predictions: 
rural.xgb_predictions <- predict(XGb_final, rural_test)

# Precision and recall for urban populations: 
confusionMatrix(rural.xgb_predictions, rural_test$rice_combined, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(rural.xgb_predictions, rural_test$rice_combined, 
                mode = "sens_spec", 
                positive = "Yes")

#-------------------------------------------------------------------------------

# Stratify performance metrics by consumption quintile: 

quintiles <- c(1,2,3,4,5)


for (i in quintiles) {
  # Create test data-set for each consumption quintile:
  quintile_test <- test %>% filter(consumption_quintile == i)
  
  # Make predictions on quintiles:
  quintile.xgb_prediction <- predict(XGb_final, quintile_test)
  
  # Create confusion matrix:
  quintile_CM <- confusionMatrix(quintile.xgb_prediction, quintile_test$rice_combined, 
                                 mode = "prec_recall",
                                 positive = "Yes")
  
  # Print F1 scores for each quntile
  print(round(quintile_CM$byClass["F1"], digits = 4))
}

#-------------------------------------------------------------------------------

# WHEAT FLOUR: 

# Fit an XGboost model using defualt parameter settings:

set.seed(123)

wf.tuning_XGb <- train(wheat_flour ~ geography + total_consumption + radio + tv +
                         fridge + cars_vehicles + mobile_phone + dwelling_free +
                         dwelling_rented + dwelling_owned + material_floor + 
                         electricity + water_source + open_defecaetion + 
                         toilet_unimproved + toilet_improved + n_per_room +
                         agricultural_land + proportion_male + proportion_primary +
                         proportion_secondary + proportion_higher + 
                         proportion_wage_salary + proportion_own_agriculture + 
                         proportion_own_NFE,
                     data = wheatf_train, 
                     method = "xgbTree",
                     trControl = XGb_ctrl)

wf.tuning_XGb

confusionMatrix(wf.tuning_XGb,
                mode = "prec_recall",
                positive = "Yes")

# Store the optimal hyper-parameters
wf.nrounds <- wf.tuning_XGb$bestTune$nrounds # 526
wf.max_depth <- as.numeric(wf.tuning_XGb$bestTune$max_depth) # 2
wf.eta <- wf.tuning_XGb$bestTune$eta # 0.27
wf.gamma <- wf.tuning_XGb$bestTune$gamma # 6.78
wf.colsample_bytree <- wf.tuning_XGb$bestTune$colsample_bytree # 0.66
wf.min_child_weight <- as.numeric(wf.tuning_XGb$bestTune$min_child_weight) # 18
wf.subsample <- wf.tuning_XGb$bestTune$subsample # 0.78

#-------------------------------------------------------------------------------

# WHEAT - FINAL XGB MODEL: 

# Fit with the optimal hyperparameters:
wf.tuning_grid <- expand.grid(nrounds = c(wf.nrounds),
                           max_depth = c(wf.max_depth),
                           eta = c(wf.eta),
                           gamma = c(wf.gamma),
                           colsample_bytree = c(wf.colsample_bytree),
                           min_child_weight = c(wf.min_child_weight),
                           subsample = c(wf.subsample))


# Run models in parallel using parallel computing:
registerDoMC(cores = 4)

set.seed(123)

wf.XGb_final <- train(wheat_flour ~ geography + total_consumption + radio + tv +
                        fridge + cars_vehicles + mobile_phone + dwelling_free +
                        dwelling_rented + dwelling_owned + material_floor + 
                        electricity + water_source + open_defecaetion + 
                        toilet_unimproved + toilet_improved + n_per_room +
                        agricultural_land + proportion_male + proportion_primary +
                        proportion_secondary + proportion_higher + 
                        proportion_wage_salary + proportion_own_agriculture + 
                        proportion_own_NFE,
                   data = wheatf_train, 
                   method = "xgbTree",
                   trControl = XGb_ctrl,
                   tuneGrid = wf.tuning_grid, 
                   verbose = TRUE)

wf.xgb_predictions <- predict(wf.XGb_final, test)

wf.xgb.prec_recall <- confusionMatrix(wf.xgb_predictions, test$wheat_flour, 
                                   positive = "Yes", 
                                   mode = "prec_recall")

wf.xgb.prec_recall

wf.xgb.sens_spec <- confusionMatrix(wf.xgb_predictions, test$wheat_flour, 
                                 positive = "Yes", 
                                 mode = "sens_spec")

wf.xgb.sens_spec

# Create visualisation for confusion matrix:
wf.xgb_matrix <- as.data.frame(wf.xgb.prec_recall$table)
wf.xgb_matrix$Prediction <- factor(wf.xgb_matrix$Prediction, 
                                levels=rev(levels(wf.xgb_matrix$Prediction)))

ggplot(wf.xgb_matrix, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to wheat flour","No access")) +
  scale_y_discrete(labels=c("No access","Access to wheat flour"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/wheat_flour/xgb_CM.jpeg", 
       width = 6,
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# VARIABLE IMPORTANCE:

# Create variable importance plot for all features in the model:
vip(wf.XGb_final, num_features = 26)

ggsave("figures/ML_outputs/wheat_flour/vipXGB.jpeg",
       width = 6, 
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# SAVE PREDICTIONS AND ASSOCIATED HHID's AS DATAFRAME: 

wf.xgb_predictions <- as.data.frame(wf.xgb_predictions)
wf.xgb_predictions$hhid <- test$hhid

write_csv(wf.xgb_predictions, "map_data/ML_predictions/xgb_reach_wheat.csv")

#-------------------------------------------------------------------------------

# Stratify performance metrics for urban vs. rural populations: 

# Urban subsample of data: 
urban_test <- test %>% filter(geography == "urban")

# Urban predictions: 
wf.urban.xgb_predictions <- predict(wf.XGb_final, urban_test)

# Precision and recall for urban populations: 
confusionMatrix(wf.urban.xgb_predictions, urban_test$wheat_flour, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(wf.urban.xgb_predictions, urban_test$wheat_flour, 
                mode = "sens_spec", 
                positive = "Yes")

# Rural subsample of data: 
rural_test <- test %>% filter(geography == "rural")

# Urban predictions: 
wf.rural.xgb_predictions <- predict(wf.XGb_final, rural_test)

# Precision and recall for urban populations: 
confusionMatrix(wf.rural.xgb_predictions, rural_test$wheat_flour, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(wf.rural.xgb_predictions, rural_test$wheat_flour, 
                mode = "sens_spec", 
                positive = "Yes")

#-------------------------------------------------------------------------------

# Stratify performance metrics by consumption quintile: 

quintiles <- c(1,2,3,4,5)

for (i in quintiles) {
  # Create test data-set for each consumption quintile:
  quintile_test <- test %>% filter(consumption_quintile == i)
  
  # Make predictions on quintiles:
  quintile.xgb_prediction <- predict(wf.XGb_final, quintile_test)
  
  # Create confusion matrix:
  quintile_CM <- confusionMatrix(quintile.xgb_prediction, quintile_test$wheat_flour, 
                                 mode = "prec_recall",
                                 positive = "Yes")
  
  # Print F1 scores for each quntile
  print(round(quintile_CM$byClass["F1"], digits = 4))
}

# Remove objects not required further: 
rm(list = c("wheatf_train", "quintile_test", "quintile_CM", "rural_test", "urban_test",
            "wf.tuning_grid", "wf.tuning_XGb", "wf.XGb_final", "wf.xgb_matrix",
            "wf.xgb_predictions", "wf.xgb.prec_recall", "wf.xgb.sens_spec",
            "i", "index", "quintile.xgb_prediction", "quintiles", "wf.colsample_bytree", 
            "wf.eta", "wf.gamma", "wf.max_depth", "wf.min_child_weight", "wf.nrounds",
            "wf.rural.xgb_predictions", "wf.subsample", "wf.urban.xgb_predictions"))

#-------------------------------------------------------------------------------

# MAIZE FLOUR

# Fit an XGboost model using defualt parameter settings:

mf.tuning_XGb <- train(maize_flour ~ geography + total_consumption + radio + tv +
                         fridge + cars_vehicles + mobile_phone + dwelling_free +
                         dwelling_rented + dwelling_owned + material_floor + 
                         electricity + water_source + open_defecaetion + 
                         toilet_unimproved + toilet_improved + n_per_room +
                         agricultural_land + proportion_male + proportion_primary +
                         proportion_secondary + proportion_higher + 
                         proportion_wage_salary + proportion_own_agriculture + 
                         proportion_own_NFE,
                        data = maizef_train, 
                        method = "xgbTree",
                        trControl = XGb_ctrl)

mf.tuning_XGb

confusionMatrix(mf.tuning_XGb,
                mode = "prec_recall",
                positive = "Yes")

# Store tuning parameters: 
mf.nrounds <- mf.tuning_XGb$bestTune$nrounds # 573
mf.max_depth <- as.numeric(mf.tuning_XGb$bestTune$max_depth) # 2
mf.eta <- mf.tuning_XGb$bestTune$eta # 0.23
mf.gamma <- mf.tuning_XGb$bestTune$gamma # 6.88
mf.colsample_bytree <- mf.tuning_XGb$bestTune$colsample_bytree # 0.54
mf.min_child_weight <- as.numeric(mf.tuning_XGb$bestTune$min_child_weight) # 12
mf.subsample <- mf.tuning_XGb$bestTune$subsample # 0.48

#-------------------------------------------------------------------------------

# MAIZE FLOUR - FINAL XGB MODEL: 

# Fit with the optimal hyperparameters:
mf.tuning_grid <- expand.grid(nrounds = c(mf.nrounds),
                              max_depth = c(mf.max_depth),
                              eta = c(mf.eta),
                              gamma = c(mf.gamma),
                              colsample_bytree = c(mf.colsample_bytree),
                              min_child_weight = c(mf.min_child_weight),
                              subsample = c(mf.subsample))


# Run models in parallel using parallel computing:
registerDoMC(cores = 4)

set.seed(123)

mf.XGb_final <- train(maize_flour ~ geography + total_consumption + radio + tv +
                        fridge + cars_vehicles + mobile_phone + dwelling_free +
                        dwelling_rented + dwelling_owned + material_floor + 
                        electricity + water_source + open_defecaetion + 
                        toilet_unimproved + toilet_improved + n_per_room +
                        agricultural_land + proportion_male + proportion_primary +
                        proportion_secondary + proportion_higher + 
                        proportion_wage_salary + proportion_own_agriculture + 
                        proportion_own_NFE,
                      data = maizef_train, 
                      method = "xgbTree",
                      trControl = XGb_ctrl,
                      tuneGrid = mf.tuning_grid, 
                      verbose = TRUE)

mf.xgb_predictions <- predict(mf.XGb_final, test)

mf.xgb.prec_recall <- confusionMatrix(mf.xgb_predictions, test$maize_flour, 
                                      positive = "Yes", 
                                      mode = "prec_recall")

mf.xgb.prec_recall

mf.xgb.sens_spec <- confusionMatrix(mf.xgb_predictions, test$maize_flour, 
                                    positive = "Yes", 
                                    mode = "sens_spec")

mf.xgb.sens_spec

# Create visualisation for confusion matrix:
mf.xgb_matrix <- as.data.frame(mf.xgb.prec_recall$table)
mf.xgb_matrix$Prediction <- factor(mf.xgb_matrix$Prediction, 
                                   levels=rev(levels(mf.xgb_matrix$Prediction)))

ggplot(mf.xgb_matrix, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to maize flour","No access")) +
  scale_y_discrete(labels=c("No access","Access to maize flour"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/maize_flour/xgb_CM.jpeg", 
       width = 6,
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# VARIABLE IMPORTANCE:

# Create variable importance plot for all features in the model:
vip(mf.XGb_final, num_features = 26)

ggsave("figures/ML_outputs/maize_flour/vipXGB.jpeg",
       width = 6, 
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# SAVE PREDICTIONS AND ASSOCIATED HHID's AS DATAFRAME: 

mf.xgb_predictions <- as.data.frame(mf.xgb_predictions)
mf.xgb_predictions$hhid <- test$hhid

write_csv(mf.xgb_predictions, "map_data/ML_predictions/xgb_reach_maize.csv")

#-------------------------------------------------------------------------------

# Stratify performance metrics for urban vs. rural populations: 

# Urban subsample of data: 
urban_test <- test %>% filter(geography == "urban")

# Urban predictions: 
mf.urban.xgb_predictions <- predict(mf.XGb_final, urban_test)

# Precision and recall for urban populations: 
confusionMatrix(mf.urban.xgb_predictions, urban_test$maize_flour, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(mf.urban.xgb_predictions, urban_test$maize_flour, 
                mode = "sens_spec", 
                positive = "Yes")

# Rural subsample of data: 
rural_test <- test %>% filter(geography == "rural")

# rural predictions: 
mf.rural.xgb_predictions <- predict(mf.XGb_final, rural_test)

# Precision and recall for urban populations: 
confusionMatrix(mf.rural.xgb_predictions, rural_test$maize_flour, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(mf.rural.xgb_predictions, rural_test$maize_flour, 
                mode = "sens_spec", 
                positive = "Yes")

#-------------------------------------------------------------------------------

# Stratify performance metrics by consumption quintile: 

quintiles <- c(1,2,3,4,5)

for (i in quintiles) {
  # Create test data-set for each consumption quintile:
  quintile_test <- test %>% filter(consumption_quintile == i)
  
  # Make predictions on quintiles:
  quintile.xgb_prediction <- predict(mf.XGb_final, quintile_test)
  
  # Create confusion matrix:
  quintile_CM <- confusionMatrix(quintile.xgb_prediction, quintile_test$maize_flour, 
                                 mode = "prec_recall",
                                 positive = "Yes")
  
  # Print F1 scores for each quntile
  print(round(quintile_CM$byClass["F1"], digits = 4))
}

#-------------------------------------------------------------------------------

rm(list = c("maizef_train", "mf.tuning_grid", "mf.tuning_XGb", "mf.XGb_final", 
            "mf.xgb_matrix", "mf.xgb.prec_recall", "mf.xgb.sens_spec", "quintile_CM", 
            "quintile_test", "rice_train", "rural_test", "test", "train", "urban_test",
            "i", "mf.colsample_bytree", "mf.eta", "mf.gamma", "mf.max_depth", 
            "mf.min_child_weight", "mf.nrounds", "mf.rural.xgb_predictions", 
            "mf.subsample", "mf.urban.xgb_predictions", "quintile.xgb_prediction", 
            "quintiles", "mf.xgb_predictions"))

#-------------------------------------------------------------------------------

# PART 2: COVERAGE (USING ONLY DATA FROM HOUSEHOLDS WITH APPARENTLY INADEQUATE
# INTAKE)

analysis_df <- analysis_df %>% filter(risk_MND == "Yes")

#-------------------------------------------------------------------------------

# TRAIN-TEST SPLIT: 

# Stratify split by LGA to ensure that we have enough predictions for mapping:
set.seed(100)

index <- partition(y = analysis_df$lga, p = c(train = 0.8, test = 0.2), 
                   split_into_list = FALSE)

# Create training and test sets:
train <- analysis_df[index == "train", ]
test <- analysis_df[index == "test", ]

#-------------------------------------------------------------------------------

# IMBALANCED DATA: 

# Use downsampling to balance training data for each fortification vehicle:

# For rice: 
set.seed(123)
rice_train <- downSample(x = train[, -ncol(train)],
                         y = train$rice_combined)

# View distribution of classes in balanced dataset
table(rice_train$rice_combined)

rice_train <- rice_train %>% dplyr::select(-Class)

# For wheat flour: 
set.seed(123)
wheatf_train <- downSample(x = train[, -ncol(train)],
                           y = train$wheat_flour)

table(wheatf_train$wheat_flour)

wheatf_train <- wheatf_train %>% dplyr::select(-Class)

# For maize flour: 
set.seed(123)
maizef_train <- downSample(x = train[, - ncol(train)],
                           y = train$maize_flour)

table(maizef_train$maize_flour)

maizef_train <- maizef_train %>% dplyr::select(-Class)

#-------------------------------------------------------------------------------

# RICE

# Fit an XGboost model using defualt parameter settings:

set.seed(111) 

# Train the model including hyperparameter tuning: 
ricov.tuning_XGb <- train(rice_combined ~ geography + total_consumption + radio + tv +
                            fridge + cars_vehicles + mobile_phone + dwelling_free +
                            dwelling_rented + dwelling_owned + material_floor + 
                            electricity + water_source + open_defecaetion + 
                            toilet_unimproved + toilet_improved + n_per_room +
                            agricultural_land + proportion_male + proportion_primary +
                            proportion_secondary + proportion_higher + 
                            proportion_wage_salary + proportion_own_agriculture + 
                            proportion_own_NFE,
                    data = rice_train, 
                    method = "xgbTree",
                    trControl = XGb_ctrl)

ricov.tuning_XGb

confusionMatrix(ricov.tuning_XGb, positive = "Yes")

# Store the optimal hyper-parameters
optimal_nrounds <- ricov.tuning_XGb$bestTune$nrounds # 175
optimal_max_depth <- as.numeric(ricov.tuning_XGb$bestTune$max_depth) # 3
optimal_eta <- ricov.tuning_XGb$bestTune$eta # 0.35
optimal_gamma <- ricov.tuning_XGb$bestTune$gamma # 1.56
optimal_colsample_bytree <- ricov.tuning_XGb$bestTune$colsample_bytree # 0.69
optimal_min_child_weight <- as.numeric(ricov.tuning_XGb$bestTune$min_child_weight) # 15
optimal_subsample <- ricov.tuning_XGb$bestTune$subsample # 0.46

#-------------------------------------------------------------------------------

# FINAL XGB model: 

# Fit with the optimal hyperparameters:
tuning_grid <- expand.grid(nrounds = c(optimal_nrounds),
                           max_depth = c(optimal_max_depth),
                           eta = c(optimal_eta),
                           gamma = c(optimal_gamma),
                           colsample_bytree = c(optimal_colsample_bytree),
                           min_child_weight = c(optimal_min_child_weight),
                           subsample = c(optimal_subsample))


# Run models in parallel using parallel computing:
registerDoMC(cores = 4)

set.seed(650)

ricov.XGb_final <- train(rice_combined ~ geography + total_consumption + radio + tv +
                           fridge + cars_vehicles + mobile_phone + dwelling_free +
                           dwelling_rented + dwelling_owned + material_floor + 
                           electricity + water_source + open_defecaetion + 
                           toilet_unimproved + toilet_improved + n_per_room +
                           agricultural_land + proportion_male + proportion_primary +
                           proportion_secondary + proportion_higher + 
                           proportion_wage_salary + proportion_own_agriculture + 
                           proportion_own_NFE,
                   data = rice_train, 
                   method = "xgbTree",
                   trControl = XGb_ctrl,
                   tuneGrid = tuning_grid, 
                   verbose = TRUE)

ricov.xgb_predictions <- predict(ricov.XGb_final, test)

ricov.xgb.prec_recall <- confusionMatrix(ricov.xgb_predictions, test$rice_combined, 
                                   positive = "Yes", 
                                   mode = "prec_recall")

ricov.xgb.prec_recall

ricov.xgb.sens_spec <- confusionMatrix(ricov.xgb_predictions, test$rice_combined, 
                                 positive = "Yes", 
                                 mode = "sens_spec")

ricov.xgb.sens_spec

# Create visualisation for confusion matrix:
xgb_matrix <- as.data.frame(ricov.xgb.prec_recall$table)
xgb_matrix$Prediction <- factor(xgb_matrix$Prediction, 
                                levels=rev(levels(xgb_matrix$Prediction)))

ggplot(xgb_matrix, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to rice","No access")) +
  scale_y_discrete(labels=c("No access","Access to rice"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/rice/coverage/xgb_CM.jpeg", 
       width = 6,
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# VARIABLE IMPORTANCE:

# Create variable importance plot for all features in the model:
vip(ricov.XGb_final, num_features = 26)

ggsave("figures/ML_outputs/rice/coverage/vipXGB.jpeg",
       width = 6, 
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# SAVE PREDICTIONS AND ASSOCIATED HHID's AS DATAFRAME: 

ricov.xgb_predictions <- as.data.frame(ricov.xgb_predictions)
ricov.xgb_predictions$hhid <- test$hhid

write_csv(ricov.xgb_predictions, "map_data/ML_predictions/xgb_rice_coverage.csv")

#-------------------------------------------------------------------------------

# Stratify performance metrics for urban vs. rural populations: 

# Urban subsample of data: 
urban_test <- test %>% filter(geography == "urban")

# Urban predictions: 
ricov.urban.xgb_predictions <- predict(ricov.XGb_final, urban_test)

# Precision and recall for urban populations: 
confusionMatrix(ricov.urban.xgb_predictions, urban_test$rice_combined, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(ricov.urban.xgb_predictions, urban_test$rice_combined, 
                mode = "sens_spec", 
                positive = "Yes")

# Rural subsample of data: 
rural_test <- test %>% filter(geography == "rural")

# Urban predictions: 
ricov.rural.xgb_predictions <- predict(ricov.XGb_final, rural_test)

# Precision and recall for urban populations: 
confusionMatrix(ricov.rural.xgb_predictions, rural_test$rice_combined, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(ricov.rural.xgb_predictions, rural_test$rice_combined, 
                mode = "sens_spec", 
                positive = "Yes")

#-------------------------------------------------------------------------------

# Stratify performance metrics by consumption quintile: 

quintiles <- c(1,2,3,4,5)

for (i in quintiles) {
  # Create test data-set for each consumption quintile:
  quintile_test <- test %>% filter(consumption_quintile == i)
  
  # Make predictions on quintiles:
  quintile.xgb_prediction <- predict(ricov.XGb_final, quintile_test)
  
  # Create confusion matrix:
  quintile_CM <- confusionMatrix(quintile.xgb_prediction, quintile_test$rice_combined, 
                                 mode = "prec_recall",
                                 positive = "Yes")
  
  # Print F1 scores for each quntile
  print(round(quintile_CM$byClass["F1"], digits = 4))
}

#-------------------------------------------------------------------------------

# Remove objects no longer required: 
rm(list = c("rice_train", "ricov.tuning_XGb", "ricov.XGb_final", 
            "ricov.xgb_predictions", "ricov.xgb.prec_recall", "ricov.xgb.sens_spec",
            "rural_test", "tuning_grid", "xgb_matrix", "i", "index", 
            "optimal_colsample_bytree", "optimal_eta", "optimal_gamma", 
            "optimal_max_depth", "optimal_min_child_weight", "optimal_nrounds",
            "optimal_subsample", "quintile.xgb_prediction", "quintiles", 
            "ricov.rural.xgb_predictions", "ricov.urban.xgb_predictions", 
            "urban.xgb_predictions", "urban_test", "quintile_CM", "quintile_test"))

#-------------------------------------------------------------------------------

# WHEAT FLOUR: 

# Fit an XGboost model using defualt parameter settings:

set.seed(123)

wfcov.tuning_XGb <- train(wheat_flour ~ geography + total_consumption + radio + tv +
                            fridge + cars_vehicles + mobile_phone + dwelling_free +
                            dwelling_rented + dwelling_owned + material_floor + 
                            electricity + water_source + open_defecaetion + 
                            toilet_unimproved + toilet_improved + n_per_room +
                            agricultural_land + proportion_male + proportion_primary +
                            proportion_secondary + proportion_higher + 
                            proportion_wage_salary + proportion_own_agriculture + 
                            proportion_own_NFE,
                       data = wheatf_train, 
                       method = "xgbTree",
                       trControl = XGb_ctrl)

wfcov.tuning_XGb

confusionMatrix(wfcov.tuning_XGb,
                mode = "prec_recall",
                positive = "Yes")

# Store the optimal hyper-parameters
wf.nrounds <- wfcov.tuning_XGb$bestTune$nrounds # 526
wf.max_depth <- as.numeric(wfcov.tuning_XGb$bestTune$max_depth) # 2
wf.eta <- wfcov.tuning_XGb$bestTune$eta # 0.27
wf.gamma <- wfcov.tuning_XGb$bestTune$gamma # 6.78
wf.colsample_bytree <- wfcov.tuning_XGb$bestTune$colsample_bytree # 0.66
wf.min_child_weight <- as.numeric(wfcov.tuning_XGb$bestTune$min_child_weight) # 18
wf.subsample <- wfcov.tuning_XGb$bestTune$subsample # 0.78

#-------------------------------------------------------------------------------

# WHEAT - FINAL XGB MODEL: 

# Fit with the optimal hyperparameters:
wfcov.tuning_grid <- expand.grid(nrounds = c(wf.nrounds),
                              max_depth = c(wf.max_depth),
                              eta = c(wf.eta),
                              gamma = c(wf.gamma),
                              colsample_bytree = c(wf.colsample_bytree),
                              min_child_weight = c(wf.min_child_weight),
                              subsample = c(wf.subsample))


# Run models in parallel using parallel computing:
registerDoMC(cores = 4)

set.seed(123)

wfcov.XGb_final <- train(wheat_flour ~ geography + total_consumption + radio + tv +
                           fridge + cars_vehicles + mobile_phone + dwelling_free +
                           dwelling_rented + dwelling_owned + material_floor + 
                           electricity + water_source + open_defecaetion + 
                           toilet_unimproved + toilet_improved + n_per_room +
                           agricultural_land + proportion_male + proportion_primary +
                           proportion_secondary + proportion_higher + 
                           proportion_wage_salary + proportion_own_agriculture + 
                           proportion_own_NFE,
                      data = wheatf_train, 
                      method = "xgbTree",
                      trControl = XGb_ctrl,
                      tuneGrid = wfcov.tuning_grid, 
                      verbose = TRUE)

wfcov.xgb_predictions <- predict(wfcov.XGb_final, test)

wfcov.xgb.prec_recall <- confusionMatrix(wfcov.xgb_predictions, test$wheat_flour, 
                                      positive = "Yes", 
                                      mode = "prec_recall")

wfcov.xgb.prec_recall

wfcov.xgb.sens_spec <- confusionMatrix(wfcov.xgb_predictions, test$wheat_flour, 
                                    positive = "Yes", 
                                    mode = "sens_spec")

wfcov.xgb.sens_spec

# Create visualisation for confusion matrix:
wfcov.xgb_matrix <- as.data.frame(wfcov.xgb.prec_recall$table)
wfcov.xgb_matrix$Prediction <- factor(wfcov.xgb_matrix$Prediction, 
                                   levels=rev(levels(wfcov.xgb_matrix$Prediction)))

ggplot(wfcov.xgb_matrix, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to wheat flour","No access")) +
  scale_y_discrete(labels=c("No access","Access to wheat flour"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/wheat_flour/coverage/xgb_CM.jpeg", 
       width = 6,
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# VARIABLE IMPORTANCE:

# Create variable importance plot for all features in the model:
vip(wfcov.XGb_final, num_features = 26)

ggsave("figures/ML_outputs/wheat_flour/coverage/vipXGB.jpeg",
       width = 6, 
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# SAVE PREDICTIONS AND ASSOCIATED HHID's AS DATAFRAME: 

wfcov.xgb_predictions <- as.data.frame(wfcov.xgb_predictions)
wfcov.xgb_predictions$hhid <- test$hhid

write_csv(wfcov.xgb_predictions, "map_data/ML_predictions/xgb_cov_wheat.csv")

#-------------------------------------------------------------------------------

# Stratify performance metrics for urban vs. rural populations: 

# Urban subsample of data: 
urban_test <- test %>% filter(geography == "urban")

# Urban predictions: 
wfcov.urban.xgb_predictions <- predict(wfcov.XGb_final, urban_test)

# Precision and recall for urban populations: 
confusionMatrix(wfcov.urban.xgb_predictions, urban_test$wheat_flour, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(wfcov.urban.xgb_predictions, urban_test$wheat_flour, 
                mode = "sens_spec", 
                positive = "Yes")

# Rural subsample of data: 
rural_test <- test %>% filter(geography == "rural")

# Urban predictions: 
wfcov.rural.xgb_predictions <- predict(wfcov.XGb_final, rural_test)

# Precision and recall for urban populations: 
confusionMatrix(wfcov.rural.xgb_predictions, rural_test$wheat_flour, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(wfcov.rural.xgb_predictions, rural_test$wheat_flour, 
                mode = "sens_spec", 
                positive = "Yes")

#-------------------------------------------------------------------------------

# Stratify performance metrics by consumption quintile: 

quintiles <- c(1,2,3,4,5)

for (i in quintiles) {
  # Create test data-set for each consumption quintile:
  quintile_test <- test %>% filter(consumption_quintile == i)
  
  # Make predictions on quintiles:
  quintile.xgb_prediction <- predict(wfcov.XGb_final, quintile_test)
  
  # Create confusion matrix:
  quintile_CM <- confusionMatrix(quintile.xgb_prediction, quintile_test$wheat_flour, 
                                 mode = "prec_recall",
                                 positive = "Yes")
  
  # Print F1 scores for each quntile
  print(round(quintile_CM$byClass["F1"], digits = 4))
}

# Remove objects not required further: 
rm(list = c("wheatf_train", "quintile_test", "quintile_CM", "rural_test", "urban_test",
            "wf.tuning_grid", "wf.tuning_XGb", "wf.XGb_final", "wf.xgb_matrix",
            "wf.xgb_predictions", "wf.xgb.prec_recall", "wf.xgb.sens_spec",
            "i", "index", "quintile.xgb_prediction", "quintiles", "wf.colsample_bytree", 
            "wf.eta", "wf.gamma", "wf.max_depth", "wf.min_child_weight", "wf.nrounds",
            "wf.rural.xgb_predictions", "wf.subsample", "wf.urban.xgb_predictions"))

#-------------------------------------------------------------------------------

# Remove objects not required further: 
rm(list = c("quintile_CM", "quintile_test", "wf.covtuning_grid", "wf.XGb_final",
            "wfcov.tuning_grid", "wfcov.tuning_XGb", "wfcov.XGb_final", 
            "wfcov.xgb_matrix", "wfcov.xgb_predictions", "wfcov.xgb.prec_recall", 
            "wfcov.xgb.sens_spec", "wheatf_train", "i", "quintile.xgb_prediction", 
            "quintiles", "wf.colsample_bytree", "wf.eta", "wf.gamma", "wf.max_depth",
            "wf.min_child_weight", "wf.nrounds", "wf.subsample", "urban_test", 
            "wfcov.rural.xgb_predictions", "wfcov.urban.xgb_predictions", 
            "rural_test"))

#-------------------------------------------------------------------------------

# MAIZE FLOUR: 

# Fit an XGboost model using defualt parameter settings:

mfcov.tuning_XGb <- train(maize_flour ~ geography + total_consumption + radio + tv +
                            fridge + cars_vehicles + mobile_phone + dwelling_free +
                            dwelling_rented + dwelling_owned + material_floor + 
                            electricity + water_source + open_defecaetion + 
                            toilet_unimproved + toilet_improved + n_per_room +
                            agricultural_land + proportion_male + proportion_primary +
                            proportion_secondary + proportion_higher + 
                            proportion_wage_salary + proportion_own_agriculture + 
                            proportion_own_NFE,
                       data = maizef_train, 
                       method = "xgbTree",
                       trControl = XGb_ctrl)

mfcov.tuning_XGb

confusionMatrix(mfcov.tuning_XGb,
                mode = "prec_recall",
                positive = "Yes")

# Store tuning parameters: 
mf.nrounds <- mfcov.tuning_XGb$bestTune$nrounds # 160
mf.max_depth <- as.numeric(mfcov.tuning_XGb$bestTune$max_depth) # 5
mf.eta <- mfcov.tuning_XGb$bestTune$eta # 0.02
mf.gamma <- mfcov.tuning_XGb$bestTune$gamma # 2.95
mf.colsample_bytree <- as.numeric(mfcov.tuning_XGb$bestTune$colsample_bytree) # 0.63
mf.min_child_weight <- as.numeric(mfcov.tuning_XGb$bestTune$min_child_weight) # 17
mf.subsample <- mfcov.tuning_XGb$bestTune$subsample # 0.30

#-------------------------------------------------------------------------------

# MAIZE FLOUR - FINAL XGB MODEL: 

# Fit with the optimal hyperparameters:
mfcov.tuning_grid <- expand.grid(nrounds = c(mf.nrounds),
                              max_depth = c(mf.max_depth),
                              eta = c(mf.eta),
                              gamma = c(mf.gamma),
                              colsample_bytree = c(mf.colsample_bytree),
                              min_child_weight = c(mf.min_child_weight),
                              subsample = c(mf.subsample))


# Run models in parallel using parallel computing:
registerDoMC(cores = 4)

set.seed(123)

mfcov.XGb_final <- train(maize_flour ~ geography + total_consumption + radio + tv +
                           fridge + cars_vehicles + mobile_phone + dwelling_free +
                           dwelling_rented + dwelling_owned + material_floor + 
                           electricity + water_source + open_defecaetion + 
                           toilet_unimproved + toilet_improved + n_per_room +
                           agricultural_land + proportion_male + proportion_primary +
                           proportion_secondary + proportion_higher + 
                           proportion_wage_salary + proportion_own_agriculture + 
                           proportion_own_NFE,
                      data = maizef_train, 
                      method = "xgbTree",
                      trControl = XGb_ctrl,
                      tuneGrid = mfcov.tuning_grid, 
                      verbose = TRUE)

mfcov.xgb_predictions <- predict(mfcov.XGb_final, test)

mfcov.xgb.prec_recall <- confusionMatrix(mfcov.xgb_predictions, test$maize_flour, 
                                      positive = "Yes", 
                                      mode = "prec_recall")

mfcov.xgb.prec_recall

mfcov.xgb.sens_spec <- confusionMatrix(mfcov.xgb_predictions, test$maize_flour, 
                                    positive = "Yes", 
                                    mode = "sens_spec")

mfcov.xgb.sens_spec

# Create visualisation for confusion matrix:
mfcov.xgb_matrix <- as.data.frame(mfcov.xgb.prec_recall$table)
mfcov.xgb_matrix$Prediction <- factor(mfcov.xgb_matrix$Prediction, 
                                   levels=rev(levels(mfcov.xgb_matrix$Prediction)))

ggplot(mfcov.xgb_matrix, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to maize flour","No access")) +
  scale_y_discrete(labels=c("No access","Access to maize flour"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/maize_flour/coverage/xgb_CM.jpeg", 
       width = 6,
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# VARIABLE IMPORTANCE:

# Create variable importance plot for all features in the model:
vip(mfcov.XGb_final, num_features = 26)

ggsave("figures/ML_outputs/maize_flour/coverage/vipXGB.jpeg",
       width = 6, 
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

# SAVE PREDICTIONS AND ASSOCIATED HHID's AS DATAFRAME: 

mfcov.xgb_predictions <- as.data.frame(mfcov.xgb_predictions)
mfcov.xgb_predictions$hhid <- test$hhid

write_csv(mfcov.xgb_predictions, "map_data/ML_predictions/xgb_cov_maize.csv")

#-------------------------------------------------------------------------------

# Stratify performance metrics for urban vs. rural populations: 

# Urban subsample of data: 
urban_test <- test %>% filter(geography == "urban")

# Urban predictions: 
mfcov.urban.xgb_predictions <- predict(mfcov.XGb_final, urban_test)

# Precision and recall for urban populations: 
confusionMatrix(mfcov.urban.xgb_predictions, urban_test$maize_flour, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(mfcov.urban.xgb_predictions, urban_test$maize_flour, 
                mode = "sens_spec", 
                positive = "Yes")

# Rural subsample of data: 
rural_test <- test %>% filter(geography == "rural")

# rural predictions: 
mfcov.rural.xgb_predictions <- predict(mfcov.XGb_final, rural_test)

# Precision and recall for urban populations: 
confusionMatrix(mfcov.rural.xgb_predictions, rural_test$maize_flour, 
                mode = "prec_recall", 
                positive = "Yes")

# Sensitvity and specificity for urban populations:
confusionMatrix(mfcov.rural.xgb_predictions, rural_test$maize_flour, 
                mode = "sens_spec", 
                positive = "Yes")

#-------------------------------------------------------------------------------

# Stratify performance metrics by consumption quintile: 

quintiles <- c(1,2,3,4,5)

for (i in quintiles) {
  # Create test data-set for each consumption quintile:
  quintile_test <- test %>% filter(consumption_quintile == i)
  
  # Make predictions on quintiles:
  quintile.xgb_prediction <- predict(mfcov.XGb_final, quintile_test)
  
  # Create confusion matrix:
  quintile_CM <- confusionMatrix(quintile.xgb_prediction, quintile_test$maize_flour, 
                                 mode = "prec_recall",
                                 positive = "Yes")
  
  # Print F1 scores for each quntile
  print(round(quintile_CM$byClass["F1"], digits = 4))
}

################################################################################
############################## END OF SCRIPT ###################################
################################################################################



