# Load necessary libraries
if (!require(caret)) install.packages("caret", dependencies = TRUE)
if (!require(glmnet)) install.packages("glmnet")
if (!require(dplyr)) install.packages("dplyr")
if (!require(e1071)) install.packages("e1071") # for confusion matrix
if (!require(rpart)) install.packages("rpart") # for decision tree
if (!require(FSelectorRcpp)) install.packages("FSelectorRcpp") # for mutual information without Java

library(caret)
library(glmnet)
library(dplyr)
library(e1071)
library(rpart)
library(FSelectorRcpp)

# Step 1: Read and Preprocess Data
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


# Rename levels of Outcome to valid variable names
health_data$Outcome <- factor(health_data$Outcome,
                              levels = c("At Risk", "Critical", "Healthy"),
                              labels = c("At_Risk", "Critical", "Healthy"))

# Step 2: Balance the Dataset by Undersampling
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

# Step 3: Feature Selection using Mutual Information (Top 10 Features)
weights <- information_gain(Outcome ~ ., data = health_data_balanced)
# weights <- information_gain(Outcome ~ ., data = health_data)

# Print the structure of weights to confirm column names
print(weights)

# Assuming the first column in weights contains the feature names, use pull(1) to extract the top 10 features
top_10_features <- weights %>%
  arrange(desc(importance)) %>%
  slice(1:10) %>%
  pull(1)  # Use column index 1 to select the feature names

print(top_10_features)
# Filter dataset to only include the top 10 features and the target variable
selected_data <- health_data_balanced %>% select(all_of(top_10_features), Outcome)
# selected_data <- health_data %>% select(all_of(top_10_features), Outcome)

# Step 4: Split Data into Training and Testing Sets
# set.seed(123)
trainIndex <- createDataPartition(selected_data$Outcome, p = .7, list = FALSE, times = 1)
train_data <- selected_data[ trainIndex,]
test_data  <- selected_data[-trainIndex,]

# Prepare features for glmnet (as.matrix and factor conversion)
X_train <- model.matrix(Outcome ~ . - 1, data = train_data)
y_train <- train_data$Outcome
X_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_test <- test_data$Outcome

# Step 5a: Train Multinomial Logistic Regression Model
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


# Step 5b: Train Decision Tree Models for Each Top Feature
trees <- list()
for (feature in top_10_features) {
  formula <- as.formula(paste("Outcome ~", feature))
  tree_model <- rpart(formula, data = train_data, method = "class")
  trees[[feature]] <- tree_model
}

# Step 6: Function to Classify Each Patient's Health Areas with Tree Models
classify_patient_areas <- function(patient_data) {
  area_classifications <- list()
  
  # Loop through each tree model and predict the specific category for each top feature
  for (feature in top_10_features) {
    tree_model <- trees[[feature]]
    
    # Create a data frame with only the relevant feature for prediction
    feature_data <- data.frame(patient_data[feature])
    colnames(feature_data) <- feature
    
    # Predict category using the feature-specific tree model
    predicted_class <- predict(tree_model, feature_data, type = "class")
    
    # Store the specific classification result for the feature
    area_classifications[[feature]] <- as.character(predicted_class)
  }
  
  return(area_classifications)
}

# Step 7a: Filter Patients by Predicted Outcome Categories
# Using the logistic regression predictions, select 2 patients from each outcome category
at_risk_patients <- test_data %>%
  filter(predictions_log_reg == "At_Risk") %>%
  slice(1:2)

critical_patients <- test_data %>%
  filter(predictions_log_reg == "Critical") %>%
  slice(1:2)

healthy_patients <- test_data %>%
  filter(predictions_log_reg == "Healthy") %>%
  slice(1:2)

# Combine selected patients into one dataframe
selected_patients <- bind_rows(at_risk_patients, critical_patients, healthy_patients)

# Step 8: Run Area Classification by Tree Models for Selected Patients
patient_areas_report <- list()

for (i in 1:nrow(selected_patients)) {
  patient_data <- selected_patients[i, ]
  patient_report <- classify_patient_areas(patient_data)
  
  # Add classification based on initial logistic regression prediction for context
  outcome_label <- predictions_log_reg[rownames(test_data) == rownames(selected_patients[i,])]
  patient_areas_report[[paste("Patient", i, "(", outcome_label, ")")]] <- patient_report
}

# Display the report for each selected patient
print("Detailed Area Classification Report for Selected Patients:")
print(patient_areas_report)