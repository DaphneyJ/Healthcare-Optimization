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

dist <- prop.table(table(train_data$Outcome)) * 100
distTest <- prop.table(table(test_data$Outcome)) * 100
distData <- prop.table(table(health_data_balanced$Outcome)) * 100

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

# Label action plans
train_data$ActionPlan <- ifelse(train_data$Outcome == "Healthy", "Maintain healthy lifestyle",
                                ifelse(train_data$Outcome == "At Risk", "Preventive Care", "Urgent medical attention"))
train_data$ActionPlan <- as.factor(train_data$ActionPlan)

test_data$ActionPlan <- ifelse(test_data$Outcome == "Healthy", "Maintain healthy lifestyle",
                                ifelse(test_data$Outcome == "At Risk", "Preventive Care", "Urgent medical attention"))
test_data$ActionPlan <- as.factor(test_data$ActionPlan)


# Train Decision Tree Models for Each Top Feature
trees <- list()
for (feature in top_10_features) {
  formula <- as.formula(paste("ActionPlan ~", feature))
  tree_model <- rpart(formula, data = train_data, method = "class")
  trees[[feature]] <- tree_model
}

# Evaluate Each Decision Tree Model
tree_metrics <- data.frame(
  Feature = character(),
  Accuracy = numeric(),
  Recall = numeric(),
  Precision = numeric(),
  F1 = numeric(),
  stringsAsFactors = FALSE
)

for (feature in top_10_features) {
  tree_model <- trees[[feature]]
  predictions_tree <- predict(tree_model, newdata = test_data, type = "class")
  
  # Ensure predictions are factors with correct levels
  predictions_tree <- factor(predictions_tree, levels = levels(test_data$ActionPlan))
  
  # Compute confusion matrix
  conf_matrix_tree <- confusionMatrix(predictions_tree, test_data$ActionPlan)
  
  # Extract metrics with error handling
  tryCatch({
    accuracy <- conf_matrix_tree$overall['Accuracy']
    # Handle byClass being a vector or matrix
    if (is.null(dim(conf_matrix_tree$byClass))) {
      # Single-class predictions, replicate values
      recall <- conf_matrix_tree$byClass['Recall']
      precision <- conf_matrix_tree$byClass['Precision']
      f1 <- conf_matrix_tree$byClass['F1']
    } else {
      # Multi-class predictions
      recall <- mean(conf_matrix_tree$byClass[, 'Recall'], na.rm = TRUE)
      precision <- mean(conf_matrix_tree$byClass[, 'Precision'], na.rm = TRUE)
      f1 <- mean(conf_matrix_tree$byClass[, 'F1'], na.rm = TRUE)
    }
    
    # Append metrics to dataframe
    tree_metrics <- rbind(tree_metrics, data.frame(
      Feature = feature,
      Accuracy = accuracy,
      Recall = recall,
      Precision = precision,
      F1 = f1
    ))
  }, error = function(e) {
    message("Error in calculating metrics for feature: ", feature)
    print(e)
  })
}

# Print metrics for all decision trees
print(tree_metrics)
write.csv(tree_metrics, "decision_tree_metrics.csv", row.names = FALSE)
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

rowData <- nrow(test_data) %/% 350

# Loop Through All Patients in Test Data and Generate Report
for (i in 1:rowData) {
  patient_data <- test_data[i, ]
  patient_classifications <- classify_patient_areas(patient_data)
  outcome_label <- predictions_log_reg[i]
  print_patient_report(i, outcome_label, patient_classifications)
}