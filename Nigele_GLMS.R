

# Clear the environment
rm(list = ls())

# libraries 
if (!require(caret)) install.packages("caret", dependencies = TRUE)
library(caret)
library(dplyr)
# ****************************************************************************
# Read data 
healthData = read.csv('health_struct.csv',header = TRUE)

healthData$Gender <- as.factor(healthData$Gender)
healthData$SmokingStatus <- as.factor(healthData$SmokingStatus)
healthData$AlcoholConsumption <- as.factor(healthData$AlcoholConsumption)
healthData$ExerciseFrequency <- as.factor(healthData$ExerciseFrequency)
healthData$Diabetes <- as.factor(healthData$Diabetes)
healthData$HeartDisease <- as.factor(healthData$HeartDisease)
healthData$PhysicalActivityLevel <- as.factor(healthData$PhysicalActivityLevel)
healthData$DietQuality <- as.factor(healthData$DietQuality)
healthData$MedicationAdherence <- as.factor(healthData$MedicationAdherence)

healthData$Systolic <- factor(healthData$Systolic, levels = c("Normal", "Elevated", "Hypertension_sg1", "Hypertension_sg2"))
healthData$Diastolic <- factor(healthData$Diastolic, levels = c("Normal", "Hypertension_sg1", "Hypertension_sg2"))
healthData$Cholesterol <- factor(healthData$Cholesterol, levels = c("Healthy", "At-risk", "High"))
healthData$Bmi <- factor(healthData$Bmi, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))

healthData$AlcoholConsumption <- factor(healthData$AlcoholConsumption, levels = c("Never", "Occasionally", "Regularly"))
healthData$ExerciseFrequency <- factor(healthData$ExerciseFrequency, levels = c("Never", "Rarely", "Sometimes", "Often"))
healthData$PhysicalActivityLevel <- factor(healthData$PhysicalActivityLevel, levels = c("Low", "Medium", "High"))
healthData$DietQuality <- factor(healthData$DietQuality, levels = c("Poor", "Average", "Good"))
healthData$MedicationAdherence <- factor(healthData$MedicationAdherence, levels = c("Low", "Medium", "High"))

healthData2 <- healthData

# ****************************************************************************
# healthy_at_risk <- healthData[healthData$Outcome %in% c("Healthy", "At Risk"), ]
healthData$Outcome[healthData$Outcome %in% c("At Risk", "Critical")] <- "Unhealthy"
healthy_nothealthy <- healthData[healthData$Outcome %in% c("Healthy", "Unhealthy"), ]
# write new data set healthy and at risk
write.csv(healthy_nothealthy,'healthy_nothealthy.csv')

healthy_nothealthy <- healthy_nothealthy %>%
  mutate(Outcome = ifelse(Outcome == "Healthy", 0, 1))
# ************************************************************************************
train_indices <- createDataPartition(healthy_nothealthy$Outcome, p = 0.7, list = FALSE)
# Split the data into training and testing sets
train_data <- healthy_nothealthy[train_indices, ]
test_data <- healthy_nothealthy[-train_indices, ]
# Check the class distribution in both sets
cat("Healthy not Healthy Training Set Distribution:\n")
print(prop.table(table(train_data$Outcome)))
# Check the class distribution in both sets
cat("Healthy not Healthy Testing Set Distribution:\n")
print(prop.table(table(test_data$Outcome)))

healthyLog <- glm(Outcome ~ .,data = train_data,family = binomial)
summary(healthyLog)

# Predict probabilities for the test set
predicted_prob <- predict(healthyLog , test_data, type = "response")

# Convert probabilities to class labels (using 0.5 as threshold)
predicted_class <- ifelse(predicted_prob > 0.5, "Unhealthy", "Healthy")

# Unhealthy <- predicted_class[predicted_class %in% c("Unhealthy")]
# Unhealthy
Unhealthy_indices <- which(predicted_class == "Unhealthy")

# Convert to factor for comparison
predicted_class <- factor(predicted_class, levels = c("Healthy", "Unhealthy"))

# Check the confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$Outcome)
cat("Confusion Matrix:\n")
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy:", round(accuracy * 100, 2), "%\n")

print("\n\n\n")

# ************************************************************************************
at_risk_critical <- healthData2[healthData2$Outcome %in% c("At Risk", "Critical"), ]
# write new data set healthy and at risk
write.csv(at_risk_critical,'at_risk_critical.csv')

at_risk_critical <- at_risk_critical %>%
  mutate(Outcome = ifelse(Outcome == "At Risk", 0, 1))


train_indices <- createDataPartition(at_risk_critical$Outcome, p = 0.7, list = FALSE)
# Split the data into training and testing sets
train_data <- at_risk_critical[train_indices, ]
test_data <- at_risk_critical[-train_indices, ]
# Check the class distribution in both sets
cat("At Risk Training Set Distribution:\n")
print(prop.table(table(train_data$Outcome)))
# Check the class distribution in both sets
cat("AT Risk Testing Set Distribution:\n")
print(prop.table(table(test_data$Outcome)))

atRiskLog <-glm(Outcome ~.,data = train_data,family = binomial)
summary(atRiskLog)

# Predict probabilities for the test set
predicted_prob <- predict(atRiskLog , test_data, type = "response")

# Convert probabilities to class labels (using 0.5 as threshold)
predicted_class <- ifelse(predicted_prob > 0.7, "At Risk", "Critical")

# Convert to factor for comparison
predicted_class <- factor(predicted_class, levels = c("At Risk", "Critical"))

# Check the confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$Outcome)
cat("Confusion Matrix:\n")
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy:", round(accuracy * 100, 2), "%\n")
# ************************************************************************************


# Predict all the Unhealthy as At risk or Critical
Unhealthy_test_data <- at_risk_critical[Unhealthy_indices, ]
predicted_prob <- predict(atRiskLog , Unhealthy_test_data, type = "response")

# Convert probabilities to class labels (using 0.5 as threshold)
predicted_class <- ifelse(predicted_prob > 0.5, "At Risk", "Critical")

# Convert to factor for comparison
predicted_class <- factor(predicted_class, levels = c("At Risk", "Critical"))

# Check the confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = Unhealthy_test_data$Outcome)
cat("Confusion Matrix Testing At Risk Model on Unhealthy:\n")
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy:", round(accuracy * 100, 2), "%\n")
