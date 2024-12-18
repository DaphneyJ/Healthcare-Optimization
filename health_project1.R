
#Load data
rm(list=ls())
health_data_original <- read.csv("health_data.csv", header = TRUE)
health_data <- health_data_original

#Load libraries
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(MASS)
library(dplyr)


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

write.csv(health_data,"health_struct.csv",row.names = FALSE)
################### EXPLORATORY ANALYSIS ######################

# Check structure and summary of the dataset
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


#View Distribution of IV variables (scatter & boxplots)
ggplot(health_data, aes(x = Outcome, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender vs Outcome", x = "outcome", y = "count")

ggplot(health_data, aes(x = Outcome, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Age Distribution by Outcome", x = "Outcome", y = "Age")

ggplot(health_data, aes(x = Gender, y = Age)) +
  geom_boxplot() +
  labs(title = "Age by Gender", x = "Gender", y = "Age")

#ggplot(health_data, aes(x = Age, y = AnnualCheckups, color = Outcome)) +
#  geom_point() +
#  labs(title = "Scatterplot of Age vs Annual Checkups", x = "Age", y = "Annual Checkups")

boxplot(health_data$Gender, health_data$Outcome)
boxplot(health_data$Gender ~ health_data$Outcome)
# Facet plot: Age vs Outcome, faceted by Gender
ggplot(health_data, aes(x = Outcome, y = Age)) +
  geom_boxplot() +
  facet_wrap(~ Gender) +
  labs(title = "Age vs Outcome by gender", x = "outcome", y = "age")
#...

#scatterplot numerical vs dv
#....


#Correlations of numerical variables
numerical_variables <- health_data[c(1,11,12,13)]
cor_matrix = cor(numerical_variables)
ggcorrplot(cor_matrix, lab=TRUE) #heatmap


#Logistic Regression EDA
library(nnet)
library(foreign)
library(VGAM)
library(MASS)






################### MODEL BUILDING ######################

library(caTools)
library(glmnet)


# Split data into Training and Testing sets (70% train, 30% test)
set.seed(123)
split <- sample.split(health_data$Outcome, SplitRatio = 0.7)
train_data <- subset(health_data, split == TRUE)
test_data <- subset(health_data, split == FALSE)

#Verify Split % 
nrow(train_data)/nrow(health_data) *100
nrow(test_data)/nrow(health_data) *100

#Verify Distribution in sets
table(train_data$Outcome)/nrow(train_data) *100
table(test_data$Outcome)/nrow(test_data) *100


# Multinomial Logistic models
multinom_model <- multinom(Outcome ~ Age + Diabetes + HeartDisease, data = train_data)
multinom_model_full <- multinom(Outcome ~ ., data = train_data)
summary(multinom_model)

#Ordinal Logistic model
ordinal_model <- polr(Outcome ~ Age + Diabetes, data = train_data, Hess=TRUE)
summary(ordinal_model)

#LASSO
x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  # -1 removes intercept to prevent duplication
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- train_data$Outcome
y_test <- test_data$Outcome

# LASSO Multinomial Models
lasso_model <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1) #standardized by default

#View Lasso selected variables
coefficients <- coef(lasso_model, s = "lambda.min") #Health: -systolic_stg1, +BMI_overweight


########################## Predictions ###################

library(caret)

# Make predictions on the test data
#LASSO
# Check the dimensions to make sure x_train and x_test match the respective y_train and y_test sizes
dim(x_train)  # Should match length of y_train
dim(x_test)   # Should match length of y_test
length(y_train)
length(y_test)

predictions <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")
predictions <- drop(predictions)  # removes the unnecessary third dimension
dim(predictions)
pred_class_indices  <- max.col(predictions, ties.method = "first")
pred_class <- factor(pred_class_indices, levels = 1:3, labels = levels(y_test))

#y_test <- factor(y_test, levels = levels(y_test))
length(pred_class)  
length(y_test)   


#Multinomial & Ordinal Model no lasso
pred_multinom <- predict(multinom_model, newdata = test_data)
pred_multinom_full <- predict(multinom_model_full, newdata = test_data)
pred_ordinal <- predict(ordinal_model, newdata = test_data)



######## Model Validation ##########

#lasso multinomial model
confusion_matrix <- confusionMatrix(pred_class, y_test)
confusion_matrix
accuracy <- confusion_matrix$overall[1]
print('The multinomial logistic model with lasso variable selection, is only predicting the healthy class, and no instances of at-risk or critical. This is a clear sign of bias or class imbalance in the model.')
cat(accuracy, 'The model accuracy is misleading since the model only correctly predicts the majority class')

tn <- confusion_matrix$table[1, 1]  # True Negative
fn <- confusion_matrix$table[2, 1]  # False Negative
fp <- confusion_matrix$table[1, 2]  # False Positive
tp <- confusion_matrix$table[2, 2]  # True Positive
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision: ", precision, "\n", '--> biased model')
cat("Recall: ", recall, "\n")
cat("F1-Score: ", f1_score, "\n")


#No lasso models
confusionMatrix(pred_multinom, y_test)$overall[1]
confusionMatrix(pred_ordinal, y_test)$overall[1]
confusionMatrix(pred_multinom_full, y_test)$overall[1]







