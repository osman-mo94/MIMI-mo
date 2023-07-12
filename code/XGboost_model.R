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

# Read in analysis data-frame: 
source("code/analysis_df.R")

#-------------------------------------------------------------------------------

# Convert target variable to factor: 
# Target variable needs to be converted to factor: 
analysis_df$rice_combined <- factor(analysis_df$rice_combined, 
                                    labels = c("No", "Yes"), 
                                    levels = c("No", "Yes"))

#-------------------------------------------------------------------------------

# MISSING DATA:

# Filter data-frame to include only complete cases: 
analysis_df <- analysis_df %>% filter(complete.cases(analysis_df))

# Note that I will revisit this, to explore methods of imputing missing data.

#-------------------------------------------------------------------------------

# TRAIN/TEST SPLIT:
# Split the data into training and testing sets: 
set.seed(450)

# Using 80% of data for training and 20% for testing:
index <- sample(1:nrow(analysis_df), nrow(analysis_df) * 0.8)

train_data <- analysis_df[index, ]
test_data <- analysis_df[-index, ]

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
                    data = train_data, 
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
                           eta = c(0.1),
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
                    data = train_data, 
                    method = "xgbTree",
                    trControl = XGb_ctrl,
                    tuneGrid = tuning_grid, 
                    verbose = TRUE)

xgb_predictions <- predict(XGb_final, test_data)

xgb.prec_recall <- confusionMatrix(xgb_predictions, test_data$rice_combined, 
                                  positive = "Yes", 
                                  mode = "prec_recall")

xgb.prec_recall

xgb.sens_spec <- confusionMatrix(xgb_predictions, test_data$rice_combined, 
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
