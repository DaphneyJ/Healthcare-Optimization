
#Load data
rm(list=ls())
health_data_original <- read.csv("health_data.csv", header = TRUE)
health_data <- health_data_original

#Load libraries
library(car)
library(caret)
library(caTools)
library(dplyr)
library(foreign)
library(ggcorrplot)
library(ggplot2)
library(glmnet)
library(nnet)
library(MASS)
library(tidyr)
library(VGAM)


################### DATA PRE-PROCESSING ######################
#Check data structure
str(health_data)

#Check for missing data
any(is.na(health_data))
sum(is.na(health_data))

#Split Blood Pressure column into Systolic & Diastolic
health_data <- separate(health_data, BloodPressure, into = c("SystolicBP", "DiastolicBP"), sep = "/")

#Convert from character to numerical
health_data$Systolic <- as.numeric(health_data$SystolicBP)
health_data$Diastolic <- as.numeric(health_data$DiastolicBP)

#Mean Arterial Pressure Calculation 
health_data$MAP = (1/3) * health_data$Systolic + (2/3) * health_data$Diastolic

#Create Categories:
#Categorize Systolic Blood Pressure
health_data$Systolic <- ifelse(health_data$SystolicBP < 120, "Normal",
                                       ifelse(health_data$SystolicBP >= 120 & health_data$SystolicBP <= 129, "Elevated",
                                              ifelse(health_data$SystolicBP >= 130 & health_data$SystolicBP <= 139, "Hypertension_sg1", "Hypertension_sg2")))
#Categorize Diastolic Blood Pressure
health_data$Diastolic <- ifelse(health_data$DiastolicBP < 80, "Normal",
                                        ifelse(health_data$DiastolicBP >= 80 & health_data$DiastolicBP <= 89, "Hypertension_sg1", "Hypertension_sg2"))

#Categorize Cholesterol
health_data$Cholesterol <- ifelse(health_data$CholesterolLevel < 200, "Healthy",
                                          ifelse(health_data$CholesterolLevel >= 200 & health_data$CholesterolLevel <= 239, "At-risk", "High"))
#Categorize BMI
health_data$Bmi <- ifelse(health_data$BMI < 18.5, "Underweight",
                                  ifelse(health_data$BMI >= 18.5 & health_data$BMI <= 24.9, "Normal weight",
                                         ifelse(health_data$BMI >= 25 & health_data$BMI <= 29.9, "Overweight", "Obese")))


#Remove unnecessary columns 
health_data <- health_data[-c(1,4,8,9,10)] #id, original bp, bmi, & cholesterol columns

#Convert categorical variables (Nominal variables)
health_data$Gender <- as.factor(health_data$Gender)
health_data$SmokingStatus <- as.factor(health_data$SmokingStatus)
health_data$AlcoholConsumption <- as.factor(health_data$AlcoholConsumption)
health_data$ExerciseFrequency <- as.factor(health_data$ExerciseFrequency)
health_data$Diabetes <- as.factor(health_data$Diabetes)
health_data$HeartDisease <- as.factor(health_data$HeartDisease)
health_data$PhysicalActivityLevel <- as.factor(health_data$PhysicalActivityLevel)
health_data$DietQuality <- as.factor(health_data$DietQuality)
health_data$MedicationAdherence <- as.factor(health_data$MedicationAdherence)
#health_data$Systolic <- as.factor(health_data$Systolic)
#health_data$Diastolic <- as.factor(health_data$Diastolic)
#health_data$Cholesterol <- as.factor(health_data$Cholesterol)
#health_data$Bmi <- as.factor(health_data$Bmi)
#health_data$Outcome <- as.factor(health_data$Outcome)

#Specify order of levels for Ordinal Variables
health_data$Outcome <- factor(health_data$Outcome, levels = c("Healthy", "At Risk", "Critical"))
health_data$Systolic <- factor(health_data$Systolic, levels = c("Normal", "Elevated", "Hypertension_sg1", "Hypertension_sg2"))
health_data$Diastolic <- factor(health_data$Diastolic, levels = c("Normal", "Hypertension_sg1", "Hypertension_sg2"))
health_data$Cholesterol <- factor(health_data$Cholesterol, levels = c("Healthy", "At-risk", "High"))
health_data$Bmi <- factor(health_data$Bmi, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))

health_data$AlcoholConsumption <- factor(health_data$AlcoholConsumption, levels = c("Never", "Occasionally", "Regularly"))
health_data$ExerciseFrequency <- factor(health_data$ExerciseFrequency, levels = c("Never", "Rarely", "Sometimes", "Often"))
health_data$PhysicalActivityLevel <- factor(health_data$PhysicalActivityLevel, levels = c("Low", "Medium", "High"))
health_data$DietQuality <- factor(health_data$DietQuality, levels = c("Poor", "Average", "Good"))
health_data$MedicationAdherence <- factor(health_data$MedicationAdherence, levels = c("Low", "Medium", "High"))

#Verify changes
str(health_data)
colSums(is.na(health_data))
any(is.na(health_data))


################### EXPLORATORY ANALYSIS ######################

# Check structure and summary of the data-set
summary(health_data)
str(health_data)

#View the Distribution of the target variable
table(health_data$Outcome)/nrow(health_data) * 100  # % of each class
outcome_dist <- prop.table(table(health_data$Outcome)) * 100
outcome_dist

ggplot(health_data, aes(x = Outcome)) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = 'count', aes(label = scales::percent(..count../sum(..count..))), vjust = -0.5) +
  labs(title = "Distribution of Outcome", x = "outcome", y = "count")


#EDA sample 
sample1 <- sample_n(health_data, 1000)
head(sample1)
table(sample1$Outcome)/nrow(sample1) *100

ggplot(data = sample1, aes(x = Outcome, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Age vs Outcome", x = "Outcome", y = "Age")

ggplot(data = sample1, aes(x = Outcome, y = HealthcareCost)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "HealthcareCost vs Outcome", x = "Outcome", y = "HealthcareCost")

ggplot(data = sample1, aes(x = Outcome, y = GeneticRisk)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "GeneticRisk vs Outcome", x = "Outcome", y = "GeneticRisk")


#View Distribution of IV variables
ggplot(health_data, aes(x = Outcome, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender vs Outcome", x = "outcome", y = "count")

ggplot(health_data, aes(x = MAP)) + 
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) + 
  labs(title = "Distribution of Mean Arterial Pressure (MAP)", x = "MAP", y = "Frequency")

ggplot(health_data, aes(x = Outcome, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Age Distribution by Outcome", x = "Outcome", y = "Age")

ggplot(health_data, aes(x = Gender, y = Age)) +
  geom_boxplot() +
  labs(title = "Age by Gender", x = "Gender", y = "Age")

boxplot(health_data$Gender, health_data$Outcome)
boxplot(health_data$Gender ~ health_data$Outcome)

ggplot(health_data, aes(x = Outcome, y = Age)) +
  geom_boxplot() +
  facet_wrap(~ Gender) +
  labs(title = "Age vs Outcome by gender", x = "outcome", y = "age")



#Correlations of numerical variables
numerical_variables <- health_data[c(1,11,12,13,17)]
cor_matrix = cor(numerical_variables)
ggcorrplot(cor_matrix, lab=TRUE) 



########################## BALANCING ######################
#creating dfs based off outcomes 
healthy <- health_data %>%
  filter(Outcome == "Healthy")
at_risk <- health_data %>%
  filter(Outcome == "At Risk")
critical <- health_data %>%
  filter(Outcome == "Critical")

set.seed(6203)
#Under sample healthy
healthy_sampled <- healthy %>% sample_n(size = nrow(at_risk)/2)
#Over sample  critical 
critical_sampled <- critical %>% sample_n(size = nrow(at_risk)/4, replace = TRUE)
balanced_data <- bind_rows(healthy_sampled, at_risk, critical_sampled)

#verify distribution
table(balanced_data$Outcome) / nrow(balanced_data) * 100


############################# MODEL BUILDING ##########################
#Split data
set.seed(6203)
train_indices <- createDataPartition(balanced_data$Outcome, p = 0.7, list = FALSE)
train_data <- balanced_data[train_indices, ]
test_data <- balanced_data[-train_indices, ]

# Fit logistic regression models without LASSO
multinom_model <- multinom(Outcome ~ Age + Diabetes + HeartDisease, data = train_data)
multinom_model_full <- multinom(Outcome ~ ., data = train_data)
ordinal_model <- polr(Outcome ~ Age + Diabetes, data = train_data, Hess=TRUE)

#LASSO
x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  # -1 to exclude the intercept
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- as.numeric(train_data$Outcome) - 1  # Convert to 0-based index for glmnet
y_test <- as.numeric(test_data$Outcome) - 1

set.seed(6203)
lasso_model <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1)  # alpha=1 for LASSO

coefficients <- coef(lasso_model, s = "lambda.min") 



############################## PREDICTIONS ############################# 
#Lasso Predictions
pred_probs <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")
pred_probs <- drop(pred_probs)  
pred_lasso <- max.col(pred_probs) - 1  

pred_lasso <- factor(pred_lasso, levels = 0:2, labels = levels(train_data$Outcome))
y_test_factor <- factor(y_test, levels = 0:2, labels = levels(train_data$Outcome))

#Models without Lasso 
pred_multinom <- predict(multinom_model, newdata = test_data)
pred_multinom_full <- predict(multinom_model_full, newdata = test_data)
pred_ordinal <- predict(ordinal_model, newdata = test_data)


######################## MODEL VALIDATION ###################### 
#Confusion Matrices
lasso_matrix <- confusionMatrix(pred_lasso, y_test_factor)
multinom_matrix <- confusionMatrix(pred_multinom, test_data$Outcome)
full_multinom_matrix <- confusionMatrix(pred_multinom_full, test_data$Outcome)
ordinal_matrix <- confusionMatrix(pred_ordinal, test_data$Outcome)

#lasso confusion matrix
tn <- lasso_matrix$table[1, 1]  # True Negative
fn <- lasso_matrix$table[2, 1]  # False Negative
fp <- lasso_matrix$table[1, 2]  # False Positive
tp <- lasso_matrix$table[2, 2]  # True Positive
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1_score <- 2 * (precision * recall) / (precision + recall)

#full model confusion matrix
tn <- full_multinom_matrix$table[1, 1]  # True Negative
fn <- full_multinom_matrix$table[2, 1]  # False Negative
fp <- full_multinom_matrix$table[1, 2]  # False Positive
tp <- full_multinom_matrix$table[2, 2]  # True Positive
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1_score <- 2 * (precision * recall) / (precision + recall)


#Accuracy
confusionMatrix(pred_ordinal, test_data$Outcome)$overall['Accuracy']
confusionMatrix(pred_multinom, test_data$Outcome)$overall['Accuracy']
confusionMatrix(pred_multinom_full, test_data$Outcome)$overall['Accuracy']
confusionMatrix(pred_lasso, test_data$Outcome)$overall['Accuracy']


########################## Decision Trees ##########################
library(rpart)
library(rpart.plot)

#label action plans
train_data$ActionPlan <- ifelse(train_data$Outcome == "Healthy", "Maintain healthy lifestyle",
                         ifelse(train_data$Outcome == "At Risk", "Preventive Care", "Urgent medical attention"))

train_data$ActionPlan <- as.factor(train_data$ActionPlan)

#Split data into training and test set
set.seed(123)  
split <- sample.split(train_data$ActionPlan, SplitRatio = 0.7)
train_set <- subset(train_data, split == TRUE)
validation_set <- subset(train_data, split == FALSE)


#Fit the Decision Tree
set.seed(90)
table(train_data$ActionPlan)

decision_tree_model <- rpart(ActionPlan ~ Age + Bmi + GeneticRisk + ExerciseFrequency + MedicationAdherence, data = train_data, method = "class")
rpart.plot(decision_tree_model, type = 3, extra = 101) #genetic risk, BMI

decision_tree_model2 <- rpart(ActionPlan ~ . - Outcome, data = train_data, method = "class")
decision_tree_model2 <- rpart(ActionPlan ~ Age+AnnualCheckups+Bmi+Cholesterol+Diastolic+GeneticRisk+HealthcareCost+MAP, data = train_data, method = "class")
rpart.plot(decision_tree_model2, type = 3, extra = 101) #genetic risk, healthcare cost

decision_tree_model3 <- rpart(ActionPlan ~ . - Outcome - HealthcareCost, data = train_data, method = "class")
rpart.plot(decision_tree_model3, type = 3, extra = 101) #genetic risk, cholesterol


#Important variables
importance <- as.data.frame(varImp(decision_tree_model, scale = FALSE))
print(importance)
importance2 <- as.data.frame(varImp(decision_tree_model2, scale = FALSE))
print(importance2)
importance3 <- as.data.frame(varImp(decision_tree_model3, scale = FALSE))
print(importance3) 


#MODEL VALIDATION 
# Predictions
predicted <- predict(decision_tree_model, newdata = validation_set, type = "class")
predicted2 <- predict(decision_tree_model2, newdata = validation_set, type = "class")
predicted3<- predict(decision_tree_model3, newdata = validation_set, type = "class")


#confusion matrices
confusion_matrix <- confusionMatrix(predicted, validation_set$ActionPlan)
confusion_matrix2 <- confusionMatrix(predicted2, validation_set$ActionPlan)
confusion_matrix3 <- confusionMatrix(predicted3, validation_set$ActionPlan)

print(confusion_matrix)


#Tree Accuracy
confusion_matrix$overall['Accuracy'] 
confusion_matrix2$overall['Accuracy']
confusion_matrix3$overall['Accuracy']

######################### METRICS DF for Dashboard ############################# 

#Export Model Performance
model_performance <- data.frame(
  Model = c("Multinomial Lasso (Balanced)", "Multinomial Lasso (At-Risk Majority)"),
  Accuracy = c(0.6424, 0.717),
  F1_Score = c(0.6462, 0.6062),  
  Precision = c(0.6529, 0.7502), 
  Recall = c(0.6423, 0.5682))     
write.csv(model_performance, "model_performance.csv", row.names = FALSE)

#Export confusion matrix
confusion_data <- as.data.frame(as.table(lasso_matrix$table))
write.csv(confusion_data, "confusion_matrix.csv", row.names = FALSE)


#Export distribution
class_distribution <- data.frame(
  Class = c("Healthy", "At-Risk", "Critical"),
  Before_Balancing = c(70, 10, 20),
  After_Balancing_model1 = c(33.3333333, 33.3333333, 33.3333333),
  After_Balancing_model2 = c(29, 57, 14)    )
write.csv(class_distribution, "class_distribution.csv", row.names = FALSE)

#Export LASSO coefficients
#with labels
class_labels <- levels(train_data$Outcome)
coefficients_list <- lapply(seq_along(coefficients), function(class_index) {
  coef_matrix <- as.matrix(coefficients[[class_index]])
  coef_df <- data.frame(Feature = rownames(coef_matrix), Coefficient = coef_matrix[, 1])
  coef_df$Class <- class_labels[class_index]  # Assign class name based on verified labels
  return(coef_df)
})
coefficients_df <- do.call(rbind, coefficients_list)
write.csv(coefficients_df, "lasso_coefficients.csv", row.names = FALSE)


#Action plan counts
action_plan_summary <- table(train_data$ActionPlan, train_data$Outcome)
write.csv(as.data.frame(action_plan_summary), "action_plans.csv", row.names = FALSE)


#Dataset
write.csv(test_data, "patient_insights.csv", row.names = FALSE)

test_data$ActionPlan <- ifelse(test_data$Outcome == "Healthy", "Maintain healthy lifestyle",
                               ifelse(test_data$Outcome == "At Risk", "Preventive Care",
                                      "Urgent Medical Attention"))
write.csv(test_data, "test_data_with_action_plans.csv", row.names = FALSE)

