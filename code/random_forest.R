################################################################################
####################### FITTING RANDOM FOREST MODELS ###########################
################################################################################

# Install and load required packages:
rq_packages <- c("tidyverse", "ranger", "randomForest", "e1071", "caret", 
                 "doMC", "vip", "kernelshap", "shapviz", "shapley")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Read in analysis data: 
source("code/analysis_df.R")

#-------------------------------------------------------------------------------

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

# DEFAULT RANDOM FOREST MODEL: 

# Run models in parallel using parallel computing:
registerDoMC(cores = 6)

# Fit default model: 
RF_default <- randomForest(rice_combined ~ dwelling_tenure + material_floor + electricity +
                             water_source + toilet_facility + n_per_room + agricultural_land +
                             radio + tv + smart_phones + reg_mobile_phone + fridge + 
                             cars_vehicles + proportion_male + proportion_christian + 
                             proportion_muslim + proportion_traditional + proportion_primary + 
                             proportion_secondary + proportion_higher + 
                             proportion_wage_salary + proportion_own_agriculture + 
                             proportion_own_NFE + proportion_trainee_apprentice + geography,
                           data = train_data)


# Use this default model to explore the impact of ntrees: 

plot(RF_default) # Error rate appears to stablise after 150 trees


#-------------------------------------------------------------------------------


# EXPLORE IMPACT OF MTRY

# Specify control parameters for tuning a random-forest model using 5-fold 
# cross-validation: 

RFctrl <- trainControl(method = "cv", # K-fold cross validation
                       number = 5, # Using 5 folds
                       savePredictions = "all", # For confusion matrix
                       search = "random", # Random grid search
                       classProbs = TRUE, # Keep class probabilities
                       summaryFunction = twoClassSummary) # For ROC, sens, spec

# Tuning with random search: 

registerDoMC(cores = 6) # Parallel cores

set.seed(450) # Set seed

rf_random <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                     water_source + toilet_facility + n_per_room + agricultural_land +
                     radio + tv + smart_phones + reg_mobile_phone + fridge + 
                     cars_vehicles + proportion_male + proportion_christian + 
                     proportion_muslim + proportion_traditional + proportion_primary + 
                     proportion_secondary + proportion_higher + 
                     proportion_wage_salary + proportion_own_agriculture + 
                     proportion_own_NFE + proportion_trainee_apprentice + geography,
                   data = train_data,
                   method = "rf", 
                   metric = "ROC",
                   tuneLength = 15, 
                   trControl = RFctrl)

rf_random

plot(rf_random, xlab = "Number of Randomly selected predictors",
     ylab = "Area under ROC curve",
     main = "mtry")

confusionMatrix(rf_random$pred$pred, rf_random$pred$obs, 
                positive = "Yes")

#-------------------------------------------------------------------------------

# Create custom random forest model that allows tuning of both mtry and ntree
# in the caret package: 

customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree=param$ntree)
}

#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

#-------------------------------------------------------------------------------

# TUNE MTRY AND NTREES

# Set values for mtry and ntree to try:
tunegrid <- expand.grid(mtry = c(1:10), 
                        ntree = c(100, 150, 200, 250))

set.seed(123)

registerDoMC(cores = 6) # Run using 6 parallel cores

# Train models:
RF_tuning <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                     water_source + toilet_facility + n_per_room + agricultural_land +
                     radio + tv + smart_phones + reg_mobile_phone + fridge + 
                     cars_vehicles + proportion_male + proportion_christian + 
                     proportion_muslim + proportion_traditional + proportion_primary + 
                     proportion_secondary + proportion_higher + 
                     proportion_wage_salary + proportion_own_agriculture + 
                     proportion_own_NFE + proportion_trainee_apprentice + geography,
                   data = train_data,
                   method = customRF, 
                   metric = "ROC",
                   tuneGrid = tunegrid, 
                   trControl = RFctrl,
                   na.action = na.omit)

# View AUROC for different values of mtry and ntree:
plot(RF_tuning, ylab = "Area under ROC curve")

# Get confusion matrix
confusionMatrix(RF_tuning$pred$pred, RF_tuning$pred$obs, 
                positive = "Yes",
                mode = "prec_recall") # This mode gives, precision, recall and F1

# Store values for optimal ntrees and mtry: 
optimal_ntree <- RF_tuning$finalModel$ntree
optimal_mtry <- RF_tuning$finalModel$mtry

#-------------------------------------------------------------------------------

# FIT A FINAL TUNED MODEL: 

RF_tuned <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                                 water_source + toilet_facility + n_per_room + agricultural_land +
                                 radio + tv + smart_phones + reg_mobile_phone + fridge + 
                                 cars_vehicles + proportion_male + proportion_christian + 
                                 proportion_muslim + proportion_traditional + proportion_primary + 
                                 proportion_secondary + proportion_higher + 
                                 proportion_wage_salary + proportion_own_agriculture + 
                                 proportion_own_NFE + proportion_trainee_apprentice + geography,
                               data = train_data,
                               method = "rf", 
                               metric = "ROC",
                               num.trees = optimal_ntree,
                               tuneGrid = expand.grid(mtry = c(optimal_mtry)),
                               trControl = RFctrl,
                               na.action = na.omit)

# Use final model to make predictions on test data:
RF_predictions <- predict(RF_tuned, test_data)

RF.prec_recall <- confusionMatrix(RF_predictions, test_data$rice_combined, 
                                  positive = "Yes", 
                                  mode = "prec_recall")

RF.prec_recall

RF.sens_spec <- confusionMatrix(RF_predictions, test_data$rice_combined, 
                                positive = "Yes", 
                                mode = "sens_spec")

RF.sens_spec

# Create visualisation for confusion matrix:
RF_matrix <- as.data.frame(RF.prec_recall$table)
RF_matrix$Prediction <- factor(RF_matrix$Prediction, 
                                levels=rev(levels(RF_matrix$Prediction)))

ggplot(RF_matrix, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to rice","No access")) +
  scale_y_discrete(labels=c("No access","Access to rice"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/RF_CM.jpeg", 
       width = 6,
       height = 4, 
       dpi = 600)


#-------------------------------------------------------------------------------

# VARIABLE IMPORTANCE:

# Create variable importance plot for all features in the model:
vip(RF_tuned, num_features = 25)

ggsave("figures/ML_outputs/RF_vip.jpeg",
       width = 6, 
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------

View(RF_tuned$pred)


