################################################################################
####################### FITTING RANDOM FOREST MODELS ###########################
################################################################################

# Install and load required packages:
rq_packages <- c("tidyverse", "ranger", "randomForest", "e1071", "caret", 
                 "doMC", "vip", "splitTools", "DMwR")

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
class(rice_train$rice_combined)

#-------------------------------------------------------------------------------

# DEFAULT RANDOM FOREST MODEL: 

# Run models in parallel using parallel computing:
registerDoMC(cores = 4)

# Fit default model: 
RF_default <- randomForest(rice_combined ~ dwelling_tenure + material_floor + electricity +
                             water_source + toilet_facility + n_per_room + agricultural_land +
                             radio + tv + smart_phones + reg_mobile_phone + fridge + 
                             cars_vehicles + proportion_male + proportion_christian + 
                             proportion_muslim + proportion_traditional + proportion_primary + 
                             proportion_secondary + proportion_higher + 
                             proportion_wage_salary + proportion_own_agriculture + 
                             proportion_own_NFE + proportion_trainee_apprentice,
                           data = rice_train)


# Use this default model to explore the impact of ntrees: 

plot(RF_default) # Error rate appears to stablise after 250 trees


#-------------------------------------------------------------------------------


# EXPLORE IMPACT OF MTRY

# Specify control parameters for tuning a random-forest model using 5-fold 
# cross-validation: 

RFctrl <- trainControl(method = "cv", # K-fold cross validation
                       number = 5, # Using 5 folds
                       savePredictions = "all", # For confusion matrix
                       search = "random", # Random grid search
                       classProbs = TRUE, # Keep class probabilities
                       summaryFunction = twoClassSummary, # For ROC, sens, spec
                       verboseIter = TRUE) # Print a training log

# Tuning with random search: 

registerDoMC(cores = 4) # Parallel cores

set.seed(450) # Set seed

rf_random <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                     water_source + toilet_facility + n_per_room + agricultural_land +
                     radio + tv + smart_phones + reg_mobile_phone + fridge + 
                     cars_vehicles + proportion_male + proportion_christian + 
                     proportion_muslim + proportion_traditional + proportion_primary + 
                     proportion_secondary + proportion_higher + 
                     proportion_wage_salary + proportion_own_agriculture + 
                     proportion_own_NFE + proportion_trainee_apprentice,
                   data = rice_train,
                   method = "rf", 
                   metric = "ROC",
                   tuneLength = 15, 
                   trControl = RFctrl)

rf_random

plot(rf_random, xlab = "Number of Randomly selected predictors",
     ylab = "Area under ROC curve",
     main = "mtry")

confusionMatrix(rf_random$pred$pred, rf_random$pred$obs, 
                positive = "Yes", 
                mode = "prec_recall")

confusionMatrix(rf_random$pred$pred, rf_random$pred$obs, 
                positive = "Yes", 
                mode = "sens_spec")

#-------------------------------------------------------------------------------

# CREATE CUSTOM MODEL:

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

# Set values for mtry and ntree (based on previous exploration):
tunegrid <- expand.grid(mtry = c(1:10), 
                        ntree = c(200, 300, 400))

set.seed(123)

registerDoMC(cores = 4) # Run using 4 parallel cores

# Train models:
RF_tuning <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                     water_source + toilet_facility + n_per_room + agricultural_land +
                     radio + tv + smart_phones + reg_mobile_phone + fridge + 
                     cars_vehicles + proportion_male + proportion_christian + 
                     proportion_muslim + proportion_traditional + proportion_primary + 
                     proportion_secondary + proportion_higher + 
                     proportion_wage_salary + proportion_own_agriculture + 
                     proportion_own_NFE + proportion_trainee_apprentice,
                   data = rice_train,
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
                                 proportion_own_NFE + proportion_trainee_apprentice,
                               data = rice_train,
                               method = "rf", 
                               metric = "ROC",
                               num.trees = optimal_ntree,
                               tuneGrid = expand.grid(mtry = c(optimal_mtry)),
                               trControl = RFctrl,
                               na.action = na.omit)

final_predictions <- predict(RF_tuned, test)

RF.prec_recall <- confusionMatrix(final_predictions, test$rice_combined, 
                                  positive = "Yes", 
                                  mode = "prec_recall")

RF.prec_recall

RF.sens_spec <- confusionMatrix(final_predictions, test$rice_combined, 
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

# Create variable importance plot for all features in the model:
vip(RF_tuned, num_features = 24)

ggsave("figures/ML_outputs/RF_vip.jpeg",
       width = 6, 
       height = 4, 
       dpi = 600)

################################################################################
############################## END OF SCRIPT ###################################
################################################################################


