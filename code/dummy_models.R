################################################################################
########################### FITTING DUMMY MODELS ###############################
################################################################################

# Install and load required packages:
rq_packages <- c("tidyverse", "caret", "basemodels", "ggplot2", "mice",
                 "splitTools", "ROSE")

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

rice_train <- rice_train %>% select(-Class)

#-------------------------------------------------------------------------------

# Fit dummy model:
dummy_model <- train(rice_combined ~ .,
                     data = rice_train,
                     method = dummyClassifier,
                     strategy = "stratified")

# Make predictions on the the test data using the dummy classifier:
dummy_predictions <- predict(dummy_model, test)

# Get confusion matrix for dummy model:
prec_recall_matrix <- confusionMatrix(dummy_predictions, test$rice_combined,
                                mode = "prec_recall",
                                positive = "Yes")

prec_recall_matrix

sens_spec_matrix <- confusionMatrix(dummy_predictions, test$rice_combined,
                                    mode = "sens_spec",
                                    positive = "Yes")

sens_spec_matrix

# Create visualisation for confusion matrix:
dmatrix_df <- as.data.frame(prec_recall_matrix$table)
dmatrix_df$Prediction <- factor(dmatrix_df$Prediction,
                                levels=rev(levels(dmatrix_df$Prediction)))

ggplot(dmatrix_df, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to rice","No access")) +
  scale_y_discrete(labels=c("No access","Access to rice"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/dummy_CM.jpeg",
       width = 6,
       height = 4,
       dpi = 600)

################################################################################
############################## END OF SCRIPT ###################################
################################################################################


