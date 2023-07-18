################################################################################
#################### FITTING LOGISTIC REGRESSION MODELS ########################
################################################################################

# Install and load required packages:
rq_packages <- c("readr", "tidyverse", "caret", "broom", "vip")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

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

# See distribution of targets: 
table(train$rice_combined) # Rice class is imbalanced

# Use downsampling
set.seed(123)
rice_train <- downSample(x = train[, -ncol(train)],
                         y = train$rice_combined)

# View distribution of targets after balancing data: 
table(rice_train$rice_combined)

#-------------------------------------------------------------------------------

# FIT LR MODELS:

# Fit logistic regression model using training data:
LR_rice <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                   water_source + toilet_facility + n_per_room + agricultural_land +
                   radio + tv + smart_phones + reg_mobile_phone + fridge +
                   cars_vehicles + proportion_male + proportion_christian +
                   proportion_muslim + proportion_traditional + proportion_primary +
                   proportion_secondary + proportion_higher +
                   proportion_wage_salary + proportion_own_agriculture +
                   proportion_own_NFE + proportion_trainee_apprentice,
                 data = rice_train,
                 method = "glm",
                 family = binomial)

summary(LR_rice)

# Make predictions on the test data using the Logistic regression model:
LR_predictions <- predict(LR_rice, test)

# Get precision-recall and confusion matrix
LR.prec_recall <- confusionMatrix(LR_predictions, test$rice_combined,
                                  mode = "prec_recall",
                                  positive = "Yes")

LR.prec_recall

# Get sensitivity/specificity:
LR.sens_spec <- confusionMatrix(LR_predictions, test$rice_combined,
                                mode = "sens_spec",
                                positive = "Yes")

LR.sens_spec

# Create visualisation for confusion matrix:
LR_matrix <- as.data.frame(LR.prec_recall$table)
LR_matrix$Prediction <- factor(LR_matrix$Prediction,
                               levels=rev(levels(LR_matrix$Prediction)))

ggplot(LR_matrix, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to rice","No access")) +
  scale_y_discrete(labels=c("No access","Access to rice"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/LR_CM.jpeg",
       width = 6,
       height = 4,
       dpi = 600)

vip(LR_rice, num_features = 25)

ggsave("figures/ML_outputs/LR_vip.jpeg",
       width = 6,
       height = 4,
       dpi = 600)

#-------------------------------------------------------------------------------

# 
# # Specify control parameters for training Logistic Regression Model using 
# # 5-fold cross validation: 
# 
# LRctrl <- trainControl(method = "cv", 
#                        number = 5, 
#                        savePredictions = "all")
# 
# # Set seed for random assignment to "folds": 
# set.seed(202)
# 
# # PREDICT ACCESS TO RICE:
# 
# # Fit logistic regression model using training data: 
# LR_rice <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
#                    water_source + toilet_facility + n_per_room + agricultural_land +
#                    radio + tv + smart_phones + reg_mobile_phone + fridge + 
#                    cars_vehicles + proportion_male + proportion_christian + 
#                    proportion_muslim + proportion_traditional + proportion_primary + 
#                    proportion_secondary + proportion_higher + 
#                    proportion_wage_salary + proportion_own_agriculture + 
#                    proportion_own_NFE + proportion_trainee_apprentice,
#                  data = analysis_df,
#                  method = "glm", 
#                  family = binomial,
#                  trControl = LRctrl)
# 
# # Get model accuracy and cohen's kappa: 
# print(LR_rice)
# 
# # View most important variables ranked: 
# plot(varImp(LR_rice))
# 
# # Get confusion matrix for predictions: 
# confusionMatrix(LR_rice$pred$pred, LR_rice$pred$obs,
#                 # Specify which is the positive class:
#                 positive = "Yes")
# 
# LR.prec_recall <- confusionMatrix(LR_rice$pred$pred, LR_rice$pred$obs,
#                                    # Specify which is the positive class:
#                                    positive = "Yes", 
#                                    mode = "prec_recall")
# 
# LR.prec_recall
# 
# # Join predictions from LR model to analysis dataframe: 
# analysis_df <- merge(analysis_df, LR_rice$pred, 
#                      # Use row indices from analysis_df and "rowIndex" 
#                      # from LR_rice$pred:
#                      by.x = 0, by.y = "rowIndex") %>% 
#   # Remove columns that are not required: 
#   select(-c("Row.names", "obs", "parameter", "Resample")) %>% 
#   rename(LRrice_pred = pred)
# 
# # Create visualisation for confusion matrix:
# LR_matrix <- as.data.frame(LR.prec_recall$table)
# LR_matrix$Prediction <- factor(LR_matrix$Prediction, 
#                                levels=rev(levels(LR_matrix$Prediction)))
# 
# ggplot(LR_matrix, aes(Prediction,Reference, fill= Freq)) +
#   geom_tile() + geom_text(aes(label=Freq)) +
#   scale_fill_gradient(low="white", high="#006db6") +
#   labs(x = "Ground truth",y = "Prediction") +
#   scale_x_discrete(labels=c("Access to rice","No access")) +
#   scale_y_discrete(labels=c("No access","Access to rice"))
# 
# # Save the confusion matrix:
# ggsave("figures/ML_outputs/LR_CM.jpeg", 
#        width = 6,
#        height = 4, 
#        dpi = 600)
# 
# vip(LR_rice, num_features = 25)
# 
# ggsave("figures/ML_outputs/LR_vip.jpeg",
#        width = 6,
#        height = 4, 
#        dpi = 600)


################################################################################
############################## END OF SCRIPT ###################################
################################################################################

