#------------------------- Working Script --------------------------

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


################### DATA PRE-PROCESSING #########################################
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


########################## BALANCING ####################################
#creating dfs based off outcomes 

#health_data <- health_data[-c(15,16)]
healthy <- health_data %>%
  filter(Outcome == "Healthy")
at_risk <- health_data %>%
  filter(Outcome == "At Risk")
critical <- health_data %>%
  filter(Outcome == "Critical")

#Under sample healthy
healthy_sampled <- healthy %>% sample_n(size = nrow(at_risk))
#Over sample  critical 
critical_sampled <- critical %>% sample_n(size = nrow(at_risk), replace = TRUE)
balanced_data <- bind_rows(healthy_sampled, at_risk, critical_sampled)

#verify distribution
table(balanced_data$Outcome) / nrow(balanced_data) * 100




################### MODEL BUILDING #####################################

#Split data into Training and Testing sets (70% train, 30% test)
set.seed(6203)
train_indices <- createDataPartition(balanced_data$Outcome, p = 0.7, list = FALSE)
train_data <- balanced_data[train_indices, ]
test_data <- balanced_data[-train_indices, ]

#Verify Distribution
table(train_data$Outcome) / nrow(balanced_data) * 100
table(test_data$Outcome) / nrow(balanced_data) * 100

#Verify Split % 
nrow(train_data)/nrow(balanced_data) *100
nrow(test_data)/nrow(balanced_data) *100


# Fit Multinomial & Ordinal Logistic models without LASSO
multinom_model_full <- multinom(Outcome ~ ., data = train_data)
multinom_model <- multinom(Outcome ~ Age + Diabetes + HeartDisease, data = train_data)
ordinal_model <- polr(Outcome ~ Age + Diabetes, data = train_data, Hess=TRUE)

multinomial_full_summary <- summary(multinom_model_full)
multinomial_summary <- summary(multinom_model)
ordinal_summary <- summary(ordinal_model)

#Cross-Validated multinomial no LASSO
#train_control <- trainControl(method = "cv", number = 10)
#set.seed(6203)
#multinom_cv <- train(Outcome ~ ., data = train_data, method = "multinom", trControl = train_control)
#cv_multinomial_summary <-summary(multinom_cv)
#print(multinom_cv)

#LASSO
x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- as.numeric(train_data$Outcome) - 1  #zero-based indexing 
y_test <- as.numeric(test_data$Outcome) - 1   #zero-based indexing 

set.seed(6203)
lasso_model <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1)  # alpha=1 for LASSO, scaling & cv 10f default

#View LASSO selected variables
coefficients <- coef(lasso_model, s = "lambda.min") #Health: -systolic_stg1, +BMI_overweight




########################## PREDICTIONS & VALIDATION ############################

# LASSO model Predictions on the test data ~ numeric outcome
pred_probs <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")
pred_probs <- drop(pred_probs)  #remove extra dim

pred_lasso <- max.col(pred_probs) - 1  

pred_lasso <- factor(pred_lasso, levels = 0:2, labels = levels(train_data$Outcome))
y_test_factor <- factor(y_test, levels = 0:2, labels = levels(train_data$Outcome))


#Models without Lasso Predictions
pred_multinom_full <- predict(multinom_model_full, newdata = test_data)
pred_multinom <- predict(multinom_model, newdata = test_data)
pred_ordinal <- predict(ordinal_model, newdata = test_data)
#pred_multinom_cv <- predict(multinom_cv, newdata = test_data)


#VALIDATION:
#Confusion Matrices
confusionMatrix(pred_lasso, y_test_factor)
confusionMatrix(pred_multinom_full, test_data$Outcome)
confusionMatrix(pred_multinom, test_data$Outcome)
confusionMatrix(pred_ordinal, test_data$Outcome)

#Accuracy
confusionMatrix(pred_lasso, test_data$Outcome)$overall['Accuracy']
confusionMatrix(pred_multinom_full, test_data$Outcome)$overall['Accuracy']
confusionMatrix(pred_multinom, test_data$Outcome)$overall['Accuracy']
confusionMatrix(pred_ordinal, test_data$Outcome)$overall['Accuracy']
#confusionMatrix(pred_multinom_cv, test_data$Outcome)$overall['Accuracy']




########################### FEATURE ENGINEERING #######################
health_data2 <- health_data

#Interactions
health_data2$Age_MAP <- health_data2$Age * health_data2$MAP
health_data2$Exercise_PhysicalActivity <- as.numeric(health_data2$ExerciseFrequency) * as.numeric(health_data2$PhysicalActivityLevel)
health_data2$Smoking_Alcohol <- as.numeric(health_data2$SmokingStatus) * as.numeric(health_data2$AlcoholConsumption)
health_data2$Age_BMI <- health_data2$Age * as.numeric(health_data2$Bmi)
health_data2$Cholesterol_MAP <- as.numeric(health_data2$Cholesterol) * health_data2$MAP
health_data2$Age_HeartDisease <- health_data2$Age * as.numeric(health_data2$HeartDisease)
health_data2$Diabetes_HeartDisease <- as.numeric(health_data2$Diabetes) * as.numeric(health_data2$HeartDisease)
health_data2$DietQuality_HeartDisease <- as.numeric(health_data2$DietQuality) * as.numeric(health_data2$HeartDisease)
health_data2$DietQuality_Diabetes <- as.numeric(health_data2$DietQuality) * as.numeric(health_data2$Diabetes)
health_data2$DietQuality_BMI <- as.numeric(health_data2$DietQuality) * as.numeric(health_data2$Bmi)
health_data2$HealthcareCost_Diabetes <- health_data2$HealthcareCost * as.numeric(health_data2$Diabetes)
health_data2$Gender_MAP <- as.numeric(health_data2$Gender) * health_data2$MAP
health_data2$Gender_Age <- as.numeric(health_data2$Gender) * health_data2$Age
health_data2$Smoking_HeartDisease <- as.numeric(health_data2$SmokingStatus) * as.numeric(health_data2$HeartDisease)
health_data2$Smoking_Diabetes <- as.numeric(health_data2$SmokingStatus) * as.numeric(health_data2$Diabetes)
health_data2$Exercise_Bmi <- as.numeric(health_data2$ExerciseFrequency) * as.numeric(health_data2$Bmi)
health_data2$Bmi_PhysicalActivity <- as.numeric(health_data2$Bmi) * as.numeric(health_data2$PhysicalActivityLevel)


#skip Scaling 
#health_data2$ScaledAge <- scale(health_data2$Age)
#health_data2$ScaledMAP <- scale(health_data2$MAP)
#health_data2$ScaledHealthcareCost <- scale(health_data2$HealthcareCost)
#health_data2$ScaledGeneticRisk <- scale(health_data2$GeneticRisk)
#Skip log
#health_data2$LogHealthcareCost <- log1p(health_data2$HealthcareCost)
#health_data2$LogGeneticRisk <- log1p(health_data2$GeneticRisk)

cor_matrix <- cor(health_data2 %>% select_if(is.numeric))
high_cor <- cor_matrix[abs(cor_matrix) > 0.8 & cor_matrix != 1]
print(high_cor)
ggcorrplot(cor_matrix, lab=TRUE) 



###Balance
#health_data <- health_data[-c(15,16)]
healthy <- health_data2 %>%
  filter(Outcome == "Healthy")
at_risk <- health_data2 %>%
  filter(Outcome == "At Risk")
critical <- health_data2 %>%
  filter(Outcome == "Critical")

healthy_sampled <- healthy %>% sample_n(size = nrow(at_risk))
critical_sampled <- critical %>% sample_n(size = nrow(at_risk), replace = TRUE)
balanced_data_2 <- bind_rows(healthy_sampled, at_risk, critical_sampled)

table(balanced_data_2$Outcome) / nrow(balanced_data_2) * 100

#balanced_data_2 = health_data2 

#Split Data
set.seed(234)
train_indices <- createDataPartition(balanced_data_2$Outcome, p = 0.7, list = FALSE)
train_data <- balanced_data_2[train_indices, ]
test_data <- balanced_data_2[-train_indices, ]

#Stepwise
model_base <- multinom(Outcome ~ Age + Bmi + MAP + PhysicalActivityLevel + ExerciseFrequency, data = train_data)
model_with_interactions <- multinom(Outcome ~ Age + Bmi + MAP + PhysicalActivityLevel+ ExerciseFrequency +Age_MAP + Age_BMI + Exercise_Bmi + Bmi_PhysicalActivity +Age_HeartDisease +Cholesterol +Age_HeartDisease+ Diabetes_HeartDisease, data = train_data)
AIC(model_base, model_with_interactions) # Compare models with AIC
BIC(model_base, model_with_interactions)
vif(model_with_interactions)  # Check VIF scores for multicollinearity > 5
vif(model_base)  

#LASSO
x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- as.numeric(train_data$Outcome) - 1  #zero-based indexing 
y_test <- as.numeric(test_data$Outcome) - 1   #zero-based indexing 

set.seed(234)
lasso_model <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1)  # alpha=1 for LASSO, scaling & cv 10f default

coef(lasso_model, s = "lambda.min") #Health: -systolic_stg1, +BMI_overweight


#Predictions
pred_multinom_interactions <- predict(model_with_interactions, newdata = test_data)

# LASSO model Predictions on the test data ~ numeric outcome
pred_probs <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")
pred_probs <- drop(pred_probs)  #remove extra dim
pred_lasso <- max.col(pred_probs) - 1  
pred_lasso <- factor(pred_lasso, levels = 0:2, labels = levels(train_data$Outcome))
y_test_factor <- factor(y_test, levels = 0:2, labels = levels(train_data$Outcome))

#Confusion Matrices
confusionMatrix(pred_lasso, y_test_factor)
confusionMatrix(pred_multinom_interactions, test_data$Outcome)

#Accuracy
confusionMatrix(pred_lasso, test_data$Outcome)$overall['Accuracy']
confusionMatrix(pred_multinom_interactions, test_data$Outcome)$overall['Accuracy']





