
if (!require(caret)) install.packages("caret", dependencies = TRUE)
if (!require(glmnet)) install.packages("glmnet")
if (!require(dplyr)) install.packages("dplyr")
if (!require(e1071)) install.packages("e1071") 
if (!require(rpart)) install.packages("rpart") 
if (!require(FSelectorRcpp)) install.packages("FSelectorRcpp")

library(caret)
library(glmnet)
library(dplyr)
library(e1071)
library(rpart)
library(FSelectorRcpp)


health_data <- read.csv('health_struct.csv', header = TRUE)

health_data$Gender <- as.factor(health_data$Gender)
health_data$SmokingStatus <- as.factor(health_data$SmokingStatus)
health_data$AlcoholConsumption <- as.factor(health_data$AlcoholConsumption)
health_data$ExerciseFrequency <- as.factor(health_data$ExerciseFrequency)
health_data$Diabetes <- as.factor(health_data$Diabetes)
health_data$HeartDisease <- as.factor(health_data$HeartDisease)
health_data$PhysicalActivityLevel <- as.factor(health_data$PhysicalActivityLevel)
health_data$DietQuality <- as.factor(health_data$DietQuality)
health_data$MedicationAdherence <- as.factor(health_data$MedicationAdherence)

health_data$Systolic <- factor(health_data$Systolic, levels = c("Normal", "Elevated", "Hypertension_sg1", "Hypertension_sg2"))
health_data$Diastolic <- factor(health_data$Diastolic, levels = c("Normal", "Hypertension_sg1", "Hypertension_sg2"))
health_data$Cholesterol <- factor(health_data$Cholesterol, levels = c("Healthy", "At-risk", "High"))
health_data$Bmi <- factor(health_data$Bmi, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))

health_data$AlcoholConsumption <- factor(health_data$AlcoholConsumption, levels = c("Never", "Occasionally", "Regularly"))
health_data$ExerciseFrequency <- factor(health_data$ExerciseFrequency, levels = c("Never", "Rarely", "Sometimes", "Often"))
health_data$PhysicalActivityLevel <- factor(health_data$PhysicalActivityLevel, levels = c("Low", "Medium", "High"))
health_data$DietQuality <- factor(health_data$DietQuality, levels = c("Poor", "Average", "Good"))
health_data$MedicationAdherence <- factor(health_data$MedicationAdherence, levels = c("Low", "Medium", "High"))


health_data$Outcome <- factor(health_data$Outcome,
                              levels = c("At Risk", "Critical", "Healthy"),
                              labels = c("At_Risk", "Critical", "Healthy"))


healthy <- health_data %>% filter(Outcome == "Healthy")
at_risk <- health_data %>% filter(Outcome == "At_Risk")
critical <- health_data %>% filter(Outcome == "Critical")

# Set seed for reproducibility
set.seed(6203)

# Generate random samples to match the size of the smallest class (at_risk)
samples_critical <- critical %>% sample_n(size = nrow(at_risk)/4)
samples_healthy <- healthy %>% sample_n(size = nrow(at_risk)/4)

# Bind into one balanced dataframe and shuffle rows
health_data_balanced <- bind_rows(samples_healthy, at_risk, samples_critical)
health_data_balanced <- health_data_balanced[sample(nrow(health_data_balanced)), ]

# Feature Selection using Mutual Information (Top 10 Features)
weights <- information_gain(Outcome ~ ., data = health_data_balanced)
top_10_features <- weights %>% arrange(desc(importance)) %>% slice(1:10) %>% pull(1)

# Split Data into Training and Testing Sets
set.seed(123)
trainIndex <- createDataPartition(health_data_balanced$Outcome, p = .7, list = FALSE, times = 1)
train_data <- health_data_balanced[trainIndex, ]
test_data <- health_data_balanced[-trainIndex, ]

# Prepare features for glmnet (as.matrix and factor conversion)
X_train <- model.matrix(Outcome ~ . - 1, data = train_data)
y_train <- train_data$Outcome
X_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_test <- test_data$Outcome

# Train Multinomial Logistic Regression Model
set.seed(123)
tuned_model <- cv.glmnet(X_train, y_train, family = "multinomial", type.measure = "class", alpha = 1)

# Make predictions with logistic regression
predictions_log_reg <- predict(tuned_model, newx = X_test, s = "lambda.min", type = "class")

# Evaluate logistic regression model
conf_matrix_log_reg <- confusionMatrix(factor(predictions_log_reg, levels = levels(y_test)), y_test)
print("Multinomial Logistic Regression Accuracy:")
print(conf_matrix_log_reg$overall['Accuracy'])
print(conf_matrix_log_reg$table)

print("Recall")
print(conf_matrix_log_reg$byClass[,'Recall'])
print("Precision")
print(conf_matrix_log_reg$byClass[,'Precision'])
print("F1 ")
print(conf_matrix_log_reg$byClass[,'F1'])
print(" ")
print(conf_matrix_log_reg$byClass)


# Train Decision Tree Models for Each Top Feature
trees <- list()
for (feature in top_10_features) {
  formula <- as.formula(paste("Outcome ~", feature))
  tree_model <- rpart(formula, data = train_data, method = "class")
  trees[[feature]] <- tree_model
}

# Function to Classify and Display Specific Category with Values for Each Feature
classify_patient_areas <- function(patient_data) {
  classification <- list()
  for (feature in top_10_features) {
    feature_value <- as.character(patient_data[[feature]])
    tree_model <- trees[[feature]]
    predicted_class <- predict(tree_model, data.frame(patient_data[feature]), type = "class")
    classification[[feature]] <- paste(predicted_class, feature_value, sep = ", ")
  }
  return(classification)
}


print_patient_report <- function(patient_id, outcome_label, classifications) {
  cat(paste0("Patient ", patient_id, " (", outcome_label, "):\n"))
  for (feature in names(classifications)) {
    cat("  - ", feature, ": ", classifications[[feature]], "\n", sep = "")
  }
  cat("\n")
}

rowData <- nrow(test_data) %/% 330

# Step 8: Loop Through All Patients in Test Data and Generate Report
for (i in 1:rowData) {
  patient_data <- test_data[i, ]
  patient_classifications <- classify_patient_areas(patient_data)

  # Get the logistic regression outcome label for the patient
  outcome_label <- predictions_log_reg[i]

  # Print report 
  print_patient_report(i, outcome_label, patient_classifications)
}