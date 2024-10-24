
#Load data
rm(list=ls())
health_data_original <- read.csv("health_data.csv", header = TRUE)
health_data <- health_data_original

#Load libraries
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(MASS)


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

# Multinomial Logistic model
multinom_model <- multinom(Outcome ~ Age + Diabetes + HeartDisease, data = health_data)
multinom_model2 <- vglm(Outcome ~ ., data = health_data, family = multinomial)
#Ordinal Logistic model
ordinal_model <- polr(Outcome ~ Age + Diabetes, data = health_data, Hess=TRUE)
summary(multinom_model)
summary(ordinal_model)


#Linear Regression EDA
temp = health_data
temp$Outcome <- as.numeric(factor(health_data$Outcome, levels = c("Healthy", "At Risk", "Critical")))
linear_model <- lm(Outcome ~ ., data = temp)
summary(linear_model)


#Split Data


################### MODEL BUILDING ######################












