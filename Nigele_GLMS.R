

# Clear the environment
rm(list = ls())

# libraries 
if (!require(caret)) install.packages("caret", dependencies = TRUE)
library(caret)
library(dplyr)
# ****************************************************************************
# Read data 
healthData = read.csv('health_struct.csv',header = TRUE)
healthData2 <- healthData

# ****************************************************************************
# healthy_at_risk <- healthData[healthData$Outcome %in% c("Healthy", "At Risk"), ]
healthData$Outcome[healthData$Outcome %in% c("At Risk", "Critical")] <- "Unhealthy"
healthy_nothealthy <- healthData[healthData$Outcome %in% c("Healthy", "Unhealthy"), ]
# write new data set healthy and at risk
write.csv(healthy_nothealthy,'healthy_nothealthy.csv')

healthy_nothealthy$Gender <- as.factor(healthy_nothealthy$Gender)
healthy_nothealthy$SmokingStatus <- as.factor(healthy_nothealthy$SmokingStatus)
healthy_nothealthy$AlcoholConsumption <- as.factor(healthy_nothealthy$AlcoholConsumption)
healthy_nothealthy$ExerciseFrequency <- as.factor(healthy_nothealthy$ExerciseFrequency)
healthy_nothealthy$Diabetes <- as.factor(healthy_nothealthy$Diabetes)
healthy_nothealthy$HeartDisease <- as.factor(healthy_nothealthy$HeartDisease)
healthy_nothealthy$PhysicalActivityLevel <- as.factor(healthy_nothealthy$PhysicalActivityLevel)
healthy_nothealthy$DietQuality <- as.factor(healthy_nothealthy$DietQuality)
healthy_nothealthy$MedicationAdherence <- as.factor(healthy_nothealthy$MedicationAdherence)

# healthy_nothealthy$Outcome <- factor(healthy_nothealthy$Outcome, levels = c("Healthy", "Unhealthy"))
healthy_nothealthy$Systolic <- factor(healthy_nothealthy$Systolic, levels = c("Normal", "Elevated", "Hypertension_sg1", "Hypertension_sg2"))
healthy_nothealthy$Diastolic <- factor(healthy_nothealthy$Diastolic, levels = c("Normal", "Hypertension_sg1", "Hypertension_sg2"))
healthy_nothealthy$Cholesterol <- factor(healthy_nothealthy$Cholesterol, levels = c("Healthy", "At-risk", "High"))
healthy_nothealthy$Bmi <- factor(healthy_nothealthy$Bmi, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))

healthy_nothealthy$AlcoholConsumption <- factor(healthy_nothealthy$AlcoholConsumption, levels = c("Never", "Occasionally", "Regularly"))
healthy_nothealthy$ExerciseFrequency <- factor(healthy_nothealthy$ExerciseFrequency, levels = c("Never", "Rarely", "Sometimes", "Often"))
healthy_nothealthy$PhysicalActivityLevel <- factor(healthy_nothealthy$PhysicalActivityLevel, levels = c("Low", "Medium", "High"))
healthy_nothealthy$DietQuality <- factor(healthy_nothealthy$DietQuality, levels = c("Poor", "Average", "Good"))
healthy_nothealthy$MedicationAdherence <- factor(healthy_nothealthy$MedicationAdherence, levels = c("Low", "Medium", "High"))


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

#Convert categorical variables (Nominal variables)
at_risk_critical$Gender <- as.factor(at_risk_critical$Gender)
at_risk_critical$SmokingStatus <- as.factor(at_risk_critical$SmokingStatus)
at_risk_critical$AlcoholConsumption <- as.factor(at_risk_critical$AlcoholConsumption)
at_risk_critical$ExerciseFrequency <- as.factor(at_risk_critical$ExerciseFrequency)
at_risk_critical$Diabetes <- as.factor(at_risk_critical$Diabetes)
at_risk_critical$HeartDisease <- as.factor(at_risk_critical$HeartDisease)
at_risk_critical$PhysicalActivityLevel <- as.factor(at_risk_critical$PhysicalActivityLevel)
at_risk_critical$DietQuality <- as.factor(at_risk_critical$DietQuality)
at_risk_critical$MedicationAdherence <- as.factor(at_risk_critical$MedicationAdherence)

#Specify order of levels for Ordinal Variables
# at_risk_critical$Outcome <- factor(at_risk_critical$Outcome, levels = c( "At Risk", "Critical"))
at_risk_critical$Systolic <- factor(at_risk_critical$Systolic, levels = c("Normal", "Elevated", "Hypertension_sg1", "Hypertension_sg2"))
at_risk_critical$Diastolic <- factor(at_risk_critical$Diastolic, levels = c("Normal", "Hypertension_sg1", "Hypertension_sg2"))
at_risk_critical$Cholesterol <- factor(at_risk_critical$Cholesterol, levels = c("Healthy", "At-risk", "High"))
at_risk_critical$Bmi <- factor(at_risk_critical$Bmi, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))

at_risk_critical$AlcoholConsumption <- factor(at_risk_critical$AlcoholConsumption, levels = c("Never", "Occasionally", "Regularly"))
at_risk_critical$ExerciseFrequency <- factor(at_risk_critical$ExerciseFrequency, levels = c("Never", "Rarely", "Sometimes", "Often"))
at_risk_critical$PhysicalActivityLevel <- factor(at_risk_critical$PhysicalActivityLevel, levels = c("Low", "Medium", "High"))
at_risk_critical$DietQuality <- factor(at_risk_critical$DietQuality, levels = c("Poor", "Average", "Good"))
at_risk_critical$MedicationAdherence <- factor(at_risk_critical$MedicationAdherence, levels = c("Low", "Medium", "High"))

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
