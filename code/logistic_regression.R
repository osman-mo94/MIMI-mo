################################################################################
#################### FITTING LOGISTIC REGRESSION MODELS ########################
################################################################################

# Install and load required packages:
rq_packages <- c("tidyverse", "caret", "broom")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Read in analysis dataframe: 
source("code/analysis_df.R")

#-------------------------------------------------------------------------------

# Convert target variable to factor: 
# Target variable needs to be converted to factor: 
analysis_df$rice_combined <- factor(analysis_df$rice_combined, 
                                    labels = c("No", "Yes"), 
                                    levels = c("No", "Yes"))

#-------------------------------------------------------------------------------

# Filter data-frame to include only complete cases: 
analysis_df <- analysis_df %>% filter(complete.cases(analysis_df))

# Note that I will revisit this, to explore methods of imputing missing data.
#-------------------------------------------------------------------------------

# Specify control parameters for training Logistic Regression Model using 
# 10-fold cross validation: 

LRctrl <- trainControl(method = "cv", 
                       number = 10, 
                       savePredictions = "all")

# Set seed for random assignment to "folds": 
set.seed(202)

# PREDICT ACCESS TO RICE:

# Fit logistic regression model using training data: 
LR_rice <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                   water_source + toilet_facility + n_per_room + agricultural_land +
                   radio + tv + smart_phones + reg_mobile_phone + fridge + 
                   cars_vehicles + proportion_male + proportion_christian + 
                   proportion_muslim + proportion_traditional + proportion_primary + 
                   proportion_secondary + proportion_higher + 
                   proportion_wage_salary + proportion_own_agriculture + 
                   proportion_own_NFE + proportion_trainee_apprentice + geography,
                 data = analysis_df,
                 method = "glm", 
                 family = binomial,
                 trControl = LRctrl)

# Get model accuracy and cohen's kappa: 
print(LR_rice)

# View most important variables ranked: 
plot(varImp(LR_rice))

# Get confusion matrix for predictions: 
confusionMatrix(LR_rice$pred$pred, LR_rice$pred$obs,
                # Specify which is the positive class:
                positive = "Yes")

# Join predictions from LR model to analysis dataframe: 
analysis_df <- merge(analysis_df, LR_rice$pred, 
                     # Use row indices from analysis_df and "rowIndex" 
                     # from LR_rice$pred:
                     by.x = 0, by.y = "rowIndex") %>% 
  # Remove columns that are not required: 
  select(-c("Row.names", "obs", "parameter", "Resample")) %>% 
  rename(LRrice_pred = pred)

#-------------------------------------------------------------------------------

# Also train model and validate using train/test split:

# Split the data into training and testing sets: 
set.seed(450)

# Using 80% of data for training and 20% for testing:
index <- sample(1:nrow(analysis_df), nrow(analysis_df) * 0.8)

train_data <- analysis_df[index, ]
test_data <- analysis_df[-index, ]

# Fit logistic regression model using training data: 
LR_rice2 <- train(rice_combined ~ dwelling_tenure + material_floor + electricity +
                   water_source + toilet_facility + n_per_room + agricultural_land +
                   radio + tv + smart_phones + reg_mobile_phone + fridge + 
                   cars_vehicles + proportion_male + proportion_christian + 
                   proportion_muslim + proportion_traditional + proportion_primary + 
                   proportion_secondary + proportion_higher + 
                   proportion_wage_salary + proportion_own_agriculture + 
                   proportion_own_NFE + proportion_trainee_apprentice + geography,
                 data = train_data,
                 method = "glm", 
                 family = binomial,
                 trControl = LRctrl)

summary(LR_rice2)

plot_model()

# Make predictions on the test data using the Logistic regression model: 
LR_predictions <- predict(LR_rice2, test_data)

# Get precision-recall and confusion matrix 
LR.prec_recall <- confusionMatrix(LR_predictions, test_data$rice_combined,
                                  mode = "prec_recall",
                                  positive = "Yes")

LR.prec_recall

# Get sensitivity/specificity:
LR.sens_spec <- confusionMatrix(LR_predictions, test_data$rice_combined,
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

vip(LR_rice2, num_features = 25)

ggsave("figures/ML_outputs/LR_vip.jpeg",
       width = 6,
       height = 4, 
       dpi = 600)

#-------------------------------------------------------------------------------
