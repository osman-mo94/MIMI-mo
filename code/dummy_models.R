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
index <- partition(y = analysis_df$lga, p = c(train = 0.8, test = 0.2), 
                   split_into_list = FALSE)

# Create training and test sets:
train <- analysis_df[index == "train", ]
test <- analysis_df[index == "test", ]

rm(index)

#-------------------------------------------------------------------------------

# IMBALANCED DATA: 

# Use downsampling

# For rice: 
set.seed(123)
rice_train <- downSample(x = train[, -ncol(train)],
                         y = train$rice_combined)

# View distribution of classes in balanced dataset
table(rice_train$rice_combined)

rice_train <- rice_train %>% select(-Class)

# For wheat flour: 
set.seed(123)
wheatf_train <- downSample(x = train[, -ncol(train)],
                           y = train$wheat_flour)

table(wheatf_train$wheat_flour)

wheatf_train <- wheatf_train %>% select(-Class)

# For maize flour: 
set.seed(123)
maizef_train <- downSample(x = train[, - ncol(train)],
                           y = train$maize_flour)

table(maizef_train$maize_flour)

maizef_train <- maizef_train %>% select(-Class)


#-------------------------------------------------------------------------------

# RICE

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
ggsave("figures/ML_outputs/rice/dummy_CM.jpeg",
       width = 6,
       height = 4,
       dpi = 600)

rm(list = c("dmatrix_df", "dummy_model", "prec_recall_matrix", "sens_spec_matrix",
            "dummy_predictions", "rice_train"))

#-------------------------------------------------------------------------------

# WHEAT FLOUR: 

# Fit dummy model:
wf.dummy_model <- train(wheat_flour ~ .,
                     data = wheatf_train,
                     method = dummyClassifier,
                     strategy = "stratified")

# Make predictions on the the test data using the dummy classifier:
wf.dummy_predictions <- predict(wf.dummy_model, test)

# Get confusion matrix for dummy model:
wf.prec_recall_matrix <- confusionMatrix(wf.dummy_predictions, test$wheat_flour,
                                      mode = "prec_recall",
                                      positive = "Yes")

wf.prec_recall_matrix

wf.sens_spec_matrix <- confusionMatrix(wf.dummy_predictions, test$wheat_flour,
                                    mode = "sens_spec",
                                    positive = "Yes")

wf.sens_spec_matrix

# Create visualisation for confusion matrix:
wf.dmatrix_df <- as.data.frame(wf.prec_recall_matrix$table)
wf.dmatrix_df$Prediction <- factor(wf.dmatrix_df$Prediction,
                                levels=rev(levels(wf.dmatrix_df$Prediction)))

ggplot(wf.dmatrix_df, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to wheat flour","No access")) +
  scale_y_discrete(labels=c("No access","Access to wheat flour"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/wheat_flour/dummy_CM.jpeg",
       width = 6,
       height = 4,
       dpi = 600)

rm(list = c("wf.dmatrix_df", "wf.dummy_model", "wf.prec_recall_matrix",
            "wf.sens_spec_matrix", "wheatf_train", "wf.dummy_predictions"))

#-------------------------------------------------------------------------------

# MAIZE FLOUR: 

# Fit dummy model:
mf.dummy_model <- train(maize_flour ~ .,
                     data = maizef_train,
                     method = dummyClassifier,
                     strategy = "stratified")

# Make predictions on the the test data using the dummy classifier:
mf.dummy_predictions <- predict(mf.dummy_model, test)

# Get confusion matrix for dummy model:
mf.prec_recall_matrix <- confusionMatrix(mf.dummy_predictions, test$maize_flour,
                                      mode = "prec_recall",
                                      positive = "Yes")

mf.prec_recall_matrix

mf.sens_spec_matrix <- confusionMatrix(mf.dummy_predictions, test$maize_flour,
                                    mode = "sens_spec",
                                    positive = "Yes")

mf.sens_spec_matrix

# Create visualisation for confusion matrix:
mf.dmatrix_df <- as.data.frame(mf.prec_recall_matrix$table)
mf.dmatrix_df$Prediction <- factor(mf.dmatrix_df$Prediction,
                                levels=rev(levels(mf.dmatrix_df$Prediction)))

ggplot(mf.dmatrix_df, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to maize flour","No access")) +
  scale_y_discrete(labels=c("No access","Access to maize flour"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/maize_flour/dummy_CM.jpeg",
       width = 6,
       height = 4,
       dpi = 600)

rm(list = c("mf.dmatrix_df", "mf.dummy_model", "mf.prec_recall_matrix", 
            "maizef_train", "mf.sens_spec_matrix", "mf.dummy_predictions"))

#-------------------------------------------------------------------------------

# PART 2: COVERAGE (USING ONLY DATA FROM HOUSEHOLDS WITH APPARENTLY INADEQUATE
# INTAKE)

# Filter analysis data-frame for household with an apparently inadequate intake: 
analysis_df <- analysis_df %>% filter(risk_MND == "Yes")

#-------------------------------------------------------------------------------

# TRAIN-TEST SPLIT: 

# Stratify split by LGA to ensure that we have enough predictions for mapping:
index <- partition(y = analysis_df$lga, p = c(train = 0.8, test = 0.2), 
                   split_into_list = FALSE)

# Create training and test sets:
train <- analysis_df[index == "train", ]
test <- analysis_df[index == "test", ]

rm(index)

#-------------------------------------------------------------------------------

# IMBALANCED DATA: 

# Use downsampling

# For rice: 
set.seed(123)
rice_train <- downSample(x = train[, -ncol(train)],
                         y = train$rice_combined)

# View distribution of classes in balanced dataset
table(rice_train$rice_combined)

rice_train <- rice_train %>% select(-Class)

# For wheat flour: 
set.seed(123)
wheatf_train <- downSample(x = train[, -ncol(train)],
                           y = train$wheat_flour)

table(wheatf_train$wheat_flour)

wheatf_train <- wheatf_train %>% select(-Class)

# For maize flour: 
set.seed(123)
maizef_train <- downSample(x = train[, - ncol(train)],
                           y = train$maize_flour)

table(maizef_train$maize_flour)

maizef_train <- maizef_train %>% select(-Class)

# For risk of inadequate diet: 
set.seed(123)
MNrisk_train <- downSample(x = train[, -ncol(train)],
                           y = train$risk_MND)

# See distribution of classes in balanced dataset: 
table(MNrisk_train$risk_MND)

MNrisk_train <- MNrisk_train %>% select(-Class)

#-------------------------------------------------------------------------------

# RICE

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
ggsave("figures/ML_outputs/rice/coverage/dummy_CM.jpeg",
       width = 6,
       height = 4,
       dpi = 600)

rm(list = c("dmatrix_df", "dummy_model", "prec_recall_matrix", "sens_spec_matrix",
            "dummy_predictions", "rice_train"))

#-------------------------------------------------------------------------------

# WHEAT FLOUR: 

# Fit dummy model:
wf.dummy_model <- train(wheat_flour ~ .,
                        data = wheatf_train,
                        method = dummyClassifier,
                        strategy = "stratified")

# Make predictions on the the test data using the dummy classifier:
wf.dummy_predictions <- predict(wf.dummy_model, test)

# Get confusion matrix for dummy model:
wf.prec_recall_matrix <- confusionMatrix(wf.dummy_predictions, test$wheat_flour,
                                         mode = "prec_recall",
                                         positive = "Yes")

wf.prec_recall_matrix

wf.sens_spec_matrix <- confusionMatrix(wf.dummy_predictions, test$wheat_flour,
                                       mode = "sens_spec",
                                       positive = "Yes")

wf.sens_spec_matrix

# Create visualisation for confusion matrix:
wf.dmatrix_df <- as.data.frame(wf.prec_recall_matrix$table)
wf.dmatrix_df$Prediction <- factor(wf.dmatrix_df$Prediction,
                                   levels=rev(levels(wf.dmatrix_df$Prediction)))

ggplot(wf.dmatrix_df, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to wheat flour","No access")) +
  scale_y_discrete(labels=c("No access","Access to wheat flour"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/wheat_flour/coverage/dummy_CM.jpeg",
       width = 6,
       height = 4,
       dpi = 600)

rm(list = c("wf.dmatrix_df", "wf.dummy_model", "wf.prec_recall_matrix",
            "wf.sens_spec_matrix", "wheatf_train", "wf.dummy_predictions"))

#-------------------------------------------------------------------------------

# MAIZE FLOUR: 

# Fit dummy model:
mf.dummy_model <- train(maize_flour ~ .,
                        data = maizef_train,
                        method = dummyClassifier,
                        strategy = "stratified")

# Make predictions on the the test data using the dummy classifier:
mf.dummy_predictions <- predict(mf.dummy_model, test)

# Get confusion matrix for dummy model:
mf.prec_recall_matrix <- confusionMatrix(mf.dummy_predictions, test$maize_flour,
                                         mode = "prec_recall",
                                         positive = "Yes")

mf.prec_recall_matrix

mf.sens_spec_matrix <- confusionMatrix(mf.dummy_predictions, test$maize_flour,
                                       mode = "sens_spec",
                                       positive = "Yes")

mf.sens_spec_matrix

# Create visualisation for confusion matrix:
mf.dmatrix_df <- as.data.frame(mf.prec_recall_matrix$table)
mf.dmatrix_df$Prediction <- factor(mf.dmatrix_df$Prediction,
                                   levels=rev(levels(mf.dmatrix_df$Prediction)))

ggplot(mf.dmatrix_df, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#006db6") +
  labs(x = "Ground truth",y = "Prediction") +
  scale_x_discrete(labels=c("Access to maize flour","No access")) +
  scale_y_discrete(labels=c("No access","Access to maize flour"))

# Save the confusion matrix:
ggsave("figures/ML_outputs/maize_flour/coverage/dummy_CM.jpeg",
       width = 6,
       height = 4,
       dpi = 600)

rm(list = c("mf.dmatrix_df", "mf.dummy_model", "mf.prec_recall_matrix", 
            "maizef_train", "mf.sens_spec_matrix", "mf.dummy_predictions"))

################################################################################
############################## END OF SCRIPT ###################################
################################################################################


