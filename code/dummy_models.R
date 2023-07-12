################################################################################
########################### FITTING DUMMY MODELS ###############################
################################################################################

# Install and load required packages:
rq_packages <- c("tidyverse", "caret", "basemodels", "ggplot2")

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

# Split the data into training and testing sets: 
set.seed(450)

# Using 80% of data for training and 20% for testing:
index <- sample(1:nrow(analysis_df), nrow(analysis_df) * 0.8)

train_data <- analysis_df[index, ]
test_data <- analysis_df[-index, ]

# Fit dummy model: 
dummy_model <- train(rice_combined ~ .,
                     data = train_data, 
                     method = dummyClassifier, 
                     strategy = "stratified")

# Make predictions on the the test data using the dummy classifier: 
dummy_predictions <- predict(dummy_model, test_data)

# Get confusion matrix for dummy model: 
prec_recall_matrix <- confusionMatrix(dummy_predictions, test_data$rice_combined,
                                mode = "prec_recall", 
                                positive = "Yes")

sens_spec_matrix <- confusionMatrix(dummy_predictions, test_data$rice_combined,
                                    mode = "sens_spec", 
                                    positive = "Yes")

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

#-------------------------------------------------------------------------------


