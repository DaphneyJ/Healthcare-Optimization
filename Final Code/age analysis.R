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
library(gridExtra)
library(randomForest)
library(gbm)
library(caTools)
library(glmnet)
library(caret)

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
health_data$MAP = (1/3) * (health_data$Systolic + (2 * health_data$Diastolic))

#Create Categories:
#Categorize Systolic Blood Pressure
health_data$Systolic <- ifelse(health_data$SystolicBP < 120, "Normal",
                                       ifelse(health_data$SystolicBP >= 120 & health_data$SystolicBP <= 129, "Elevated",
                                              ifelse(health_data$SystolicBP >= 130 & health_data$SystolicBP <= 139, "Hypertension_sg1", "Hypertension_sg2")))
#Categorize Diastolic Blood Pressure
health_data$Diastolic <- ifelse(health_data$DiastolicBP < 80, "Normal",
                                        ifelse(health_data$DiastolicBP >= 80 & health_data$DiastolicBP <= 89, "Hypertension_sg1", "Hypertension_sg2"))

#Categorize MAP
health_data$MAP <- ifelse(health_data$MAP < 90, "Normal",
                                ifelse(health_data$MAP >= 90 & health_data$MAP <= 92, "Elevated",
                                       ifelse(health_data$MAP > 92 & health_data$MAP <= 96, "Hypertension_sg1", "Hypertension_sg2")))

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
health_data$MAP <- factor(health_data$MAP, levels = c("Normal", "Elevated", "Hypertension_sg1", "Hypertension_sg2"))
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


##################### BALANCING DATASET #######################

## balancing dataset via random undersampling

#creating dfs based off outcomes 
healthy <- health_data %>%
  filter(Outcome == "Healthy")
at_risk <- health_data %>%
  filter(Outcome == "At Risk")
critical <- health_data %>%
  filter(Outcome == "Critical")

#generating random samples, each category having 27269 points
set.seed(6203)
samples_critical <- critical %>%
  sample_n(size = nrow(at_risk))
samples_healthy <- healthy %>%
  sample_n(size = nrow(at_risk))

#binding into one df and reshuffling
health_data_balanced <- bind_rows(samples_healthy, at_risk, samples_critical)
health_data_balanced <- health_data_balanced[sample(nrow(health_data_balanced)),]

#comparing to original

build_plots <- function(health_data, health_data_balanced, predictor, x_lab) {
  original <- ggplot(health_data, aes_string(x = predictor, fill = predictor)) +
    geom_bar() +
    labs(title = paste(predictor, "Original"), x = x_lab, y = "Count")
  
  balanced <- ggplot(health_data_balanced, aes_string(x = predictor, fill = predictor)) +
    geom_bar() +
    labs(title = paste(predictor, "Balanced"), x = x_lab, y = "Count")
  
  list(original, balanced)
}
plots <- c(
  build_plots(health_data, health_data_balanced, "Gender", "Gender"),
  build_plots(health_data, health_data_balanced, "SmokingStatus", "Smoking Status"),
  build_plots(health_data, health_data_balanced, "AlcoholConsumption", "Alcohol Consumption"),
  build_plots(health_data, health_data_balanced, "ExerciseFrequency", "Exercise Frequency"),
  build_plots(health_data, health_data_balanced, "Diabetes", "Diabetes"),
  build_plots(health_data, health_data_balanced, "HeartDisease", "Heart Disease"),
  build_plots(health_data, health_data_balanced, "PhysicalActivityLevel", "Physical Activity Level"),
  build_plots(health_data, health_data_balanced, "DietQuality", "Diet Quality"),
  build_plots(health_data, health_data_balanced, "MedicationAdherence", "Medication Adherence"),
  build_plots(health_data, health_data_balanced, "Systolic", "Systolic BP"),
  build_plots(health_data, health_data_balanced, "Cholesterol", "Cholesterol"),
  build_plots(health_data, health_data_balanced, "Bmi", "BMI")
)

grid.arrange(grobs = plots[1:4], ncol = 2, nrow = 2)
grid.arrange(grobs = plots[5:8], ncol = 2, nrow = 2)
grid.arrange(grobs = plots[9:12], ncol = 2, nrow = 2)
grid.arrange(grobs = plots[13:16], ncol = 2, nrow = 2)
grid.arrange(grobs = plots[17:20], ncol = 2, nrow = 2)
grid.arrange(grobs = plots[21:24], ncol = 2, nrow = 2)

orig_checkups <- mean(health_data$AnnualCheckups)
orig_risk <- mean(health_data$GeneticRisk)
orig_cost <- mean(health_data$HealthcareCost)
orig_age <- mean(health_data$Age)

balanced_checkups <- mean(health_data_balanced$AnnualCheckups)
balanced_risk <- mean(health_data_balanced$GeneticRisk)
balanced_cost <- mean(health_data_balanced$HealthcareCost)
balanced_age <- mean(health_data_balanced$Age)

mean_comparison <- data.frame(Original = c(orig_checkups, orig_risk, orig_cost, orig_age),
                              Balanced = c(balanced_checkups, balanced_risk, balanced_cost,
                                           balanced_age),row.names=c("Annual Checkups", "Genetic Risk",
                                                                     "Healthcare Cost", "Age"))
mean_comparison
################### EXPLORATORY ANALYSIS W/BALANCED SET ####################

# Check structure and summary of the dataset
summary(health_data_balanced) 
str(health_data_balanced)

#View the Distribution of the target variable
table(health_data_balanced$Outcome)/nrow(health_data_balanced) * 100  # % of each class
outcome_dist <- prop.table(table(health_data_balanced$Outcome)) * 100
outcome_dist

ggplot(health_data_balanced, aes(x = Outcome)) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = 'count', aes(label = scales::percent(..count../sum(..count..))), vjust = -0.5) +
  labs(title = "Distribution of Outcome", x = "outcome", y = "count")

#EDA sample
sample1 <- sample_n(health_data_balanced, 1000)
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
ggplot(health_data_balanced, aes(x = Outcome, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender vs Outcome", x = "outcome", y = "count")

ggplot(health_data_balanced, aes(x = Outcome, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Age Distribution by Outcome", x = "Outcome", y = "Age")

ggplot(health_data_balanced, aes(x = Gender, y = Age)) +
  geom_boxplot() +
  labs(title = "Age by Gender", x = "Gender", y = "Age")

#ggplot(health_data, aes(x = Age, y = AnnualCheckups, color = Outcome)) +
#  geom_point() +
#  labs(title = "Scatterplot of Age vs Annual Checkups", x = "Age", y = "Annual Checkups")

boxplot(health_data_balanced$Gender, health_data_balanced$Outcome)
boxplot(health_data_balanced$Gender ~ health_data_balanced$Outcome)
# Facet plot: Age vs Outcome, faceted by Gender
ggplot(health_data_balanced, aes(x = Outcome, y = Age)) +
  geom_boxplot() +
  facet_wrap(~ Gender) +
  labs(title = "Age vs Outcome by gender", x = "outcome", y = "age")
#...

#scatterplot numerical vs dv
#....


#Correlations of numerical variables
numerical_variables <- health_data_balanced[c(1,11,12,13)]
cor_matrix = cor(numerical_variables)
ggcorrplot(cor_matrix, lab=TRUE) #heatmap

health_data_balanced <- health_data_balanced %>%
  select(-Systolic,-Diastolic)


########################### Age Analysis ###############################

########## Age 20-29 ##########

#Logistic w LASSO
df_20s <- subset(health_data_balanced, Age >= 20 & Age <= 29)

#check distribution
table(df_20s$Outcome)/nrow(df_20s) * 100  # % of each class
outcome_dist <- prop.table(table(df_20s$Outcome)) * 100
outcome_dist

#train test
split <- sample.split(df_20s, SplitRatio = 0.7)
train_data <- subset(df_20s, split == TRUE)
test_data <- subset(df_20s, split == FALSE)

x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  # -1 removes intercept to prevent duplication
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- train_data$Outcome
y_test <- test_data$Outcome

# LASSO Multinomial Models
lasso_model_20s <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1) #standardized by default

best_lambda_20s <- lasso_model_20s$lambda.min
lasso_model_20s$lambda.min

#View Lasso selected variables
coefficients_20s <- coef(lasso_model_20s, s = "lambda.min")
coefficients_20s

# lasso_model_20s_prediction <- predict(lasso_model_20s, x_test)
lasso_model_20s_prediction <- predict(lasso_model_20s, newx = x_test, s = "lambda.min", type = "response")
lasso_model_20s_prediction <- drop(lasso_model_20s_prediction)  # removes the unnecessary third dimension
pred_class_indices_20s  <- max.col(lasso_model_20s_prediction, ties.method = "first")
pred_class_20s <- factor(pred_class_indices_20s, levels = 1:3, labels = levels(y_test))
confusion_20s <- confusionMatrix(as.factor(pred_class_20s), as.factor(y_test))



#Age 30-39

df_30s <- subset(health_data_balanced, Age >= 30 & Age <= 39)

#check distribution
table(df_30s$Outcome)/nrow(df_30s) * 100  # % of each class
outcome_dist <- prop.table(table(df_30s$Outcome)) * 100
outcome_dist

#train test
split <- sample.split(df_30s, SplitRatio = 0.7)
train_data <- subset(df_30s, split == TRUE)
test_data <- subset(df_30s, split == FALSE)

x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  # -1 removes intercept to prevent duplication
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- train_data$Outcome
y_test <- test_data$Outcome

# LASSO Multinomial Models
lasso_model_30s <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1) #standardized by default

best_lambda_30s <- lasso_model_30s$lambda.min
lasso_model_30s$lambda.min

#View Lasso selected variables
coefficients_30s <- coef(lasso_model_30s, s = "lambda.min")
coefficients_30s

lasso_model_30s_prediction <- predict(lasso_model_30s, newx = x_test, s = "lambda.min", type = "response")
lasso_model_30s_prediction <- drop(lasso_model_30s_prediction)  # removes the unnecessary third dimension
pred_class_indices_30s  <- max.col(lasso_model_30s_prediction, ties.method = "first")
pred_class_30s <- factor(pred_class_indices_30s, levels = 1:3, labels = levels(y_test))
confusion_30s <- confusionMatrix(as.factor(pred_class_30s), as.factor(y_test))

#Age 40-49

df_40s <- subset(health_data_balanced, Age >= 40 & Age <= 49)

#check distribution
table(df_40s$Outcome)/nrow(df_40s) * 100  # % of each class
outcome_dist <- prop.table(table(df_40s$Outcome)) * 100
outcome_dist

#train test
split <- sample.split(df_40s, SplitRatio = 0.7)
train_data <- subset(df_40s, split == TRUE)
test_data <- subset(df_40s, split == FALSE)

x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  # -1 removes intercept to prevent duplication
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- train_data$Outcome
y_test <- test_data$Outcome

# LASSO Multinomial Models
lasso_model_40s <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1) #standardized by default

best_lambda_40s <- lasso_model_40s$lambda.min
lasso_model_40s$lambda.min

#View Lasso selected variables
coefficients_40s <- coef(lasso_model_40s, s = "lambda.min")
coefficients_40s

lasso_model_40s_prediction <- predict(lasso_model_40s, newx = x_test, s = "lambda.min", type = "response")
lasso_model_40s_prediction <- drop(lasso_model_40s_prediction)  # removes the unnecessary third dimension
pred_class_indices_40s  <- max.col(lasso_model_40s_prediction, ties.method = "first")
pred_class_40s <- factor(pred_class_indices_40s, levels = 1:3, labels = levels(y_test))
confusion_40s <- confusionMatrix(as.factor(pred_class_40s), as.factor(y_test))

#Age 50-59

df_50s <- subset(health_data_balanced, Age >= 50 & Age <= 59)

#check distribution
table(df_50s$Outcome)/nrow(df_50s) * 100  # % of each class
outcome_dist <- prop.table(table(df_50s$Outcome)) * 100
outcome_dist

#train test
split <- sample.split(df_50s, SplitRatio = 0.7)
train_data <- subset(df_50s, split == TRUE)
test_data <- subset(df_50s, split == FALSE)

x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  # -1 removes intercept to prevent duplication
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- train_data$Outcome
y_test <- test_data$Outcome

# LASSO Multinomial Models
lasso_model_50s <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1) #standardized by default

best_lambda_50s <- lasso_model_50s$lambda.min
lasso_model_50s$lambda.min

#View Lasso selected variables
coefficients_50s <- coef(lasso_model_50s, s = "lambda.min")
coefficients_50s

lasso_model_50s_prediction <- predict(lasso_model_50s, newx = x_test, s = "lambda.min", type = "response")
lasso_model_50s_prediction <- drop(lasso_model_50s_prediction)  # removes the unnecessary third dimension
pred_class_indices_50s  <- max.col(lasso_model_50s_prediction, ties.method = "first")
pred_class_50s <- factor(pred_class_indices_50s, levels = 1:3, labels = levels(y_test))
confusion_50s <- confusionMatrix(as.factor(pred_class_50s), as.factor(y_test))

#Age 60-69

df_60s <- subset(health_data_balanced, Age >= 60 & Age <= 69)

#check distribution
table(df_60s$Outcome)/nrow(df_60s) * 100  # % of each class
outcome_dist <- prop.table(table(df_60s$Outcome)) * 100
outcome_dist

#train test
split <- sample.split(df_60s, SplitRatio = 0.7)
train_data <- subset(df_60s, split == TRUE)
test_data <- subset(df_60s, split == FALSE)

x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  # -1 removes intercept to prevent duplication
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- train_data$Outcome
y_test <- test_data$Outcome

# LASSO Multinomial Models
lasso_model_60s <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1) #standardized by default

best_lambda_60s <- lasso_model_60s$lambda.min
lasso_model_60s$lambda.min

#View Lasso selected variables
coefficients_60s <- coef(lasso_model_60s, s = "lambda.min")
coefficients_60s

lasso_model_60s_prediction <- predict(lasso_model_60s, newx = x_test, s = "lambda.min", type = "response")
lasso_model_60s_prediction <- drop(lasso_model_60s_prediction)  # removes the unnecessary third dimension
pred_class_indices_60s  <- max.col(lasso_model_60s_prediction, ties.method = "first")
pred_class_60s <- factor(pred_class_indices_60s, levels = 1:3, labels = levels(y_test))
confusion_60s <- confusionMatrix(as.factor(pred_class_60s), as.factor(y_test))

#Age 70-79

df_70s <- subset(health_data_balanced, Age >= 70 & Age <= 79)

#check distribution
table(df_70s$Outcome)/nrow(df_70s) * 100  # % of each class
outcome_dist <- prop.table(table(df_70s$Outcome)) * 100
outcome_dist

#train test
split <- sample.split(df_70s, SplitRatio = 0.7)
train_data <- subset(df_70s, split == TRUE)
test_data <- subset(df_70s, split == FALSE)

x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  # -1 removes intercept to prevent duplication
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- train_data$Outcome
y_test <- test_data$Outcome

# LASSO Multinomial Models
lasso_model_70s <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1) #standardized by default


best_lambda_70s <- lasso_model_70s$lambda.min
lasso_model_70s$lambda.min

#View Lasso selected variables
coefficients_70s <- coef(lasso_model_70s, s = "lambda.min")
coefficients_70s

lasso_model_70s_prediction <- predict(lasso_model_70s, newx = x_test, s = "lambda.min", type = "response")
lasso_model_70s_prediction <- drop(lasso_model_70s_prediction)  # removes the unnecessary third dimension
pred_class_indices_70s  <- max.col(lasso_model_70s_prediction, ties.method = "first")
pred_class_70s <- factor(pred_class_indices_70s, levels = 1:3, labels = levels(y_test))
confusion_70s <- confusionMatrix(as.factor(pred_class_70s), as.factor(y_test))

#Age 80-89

df_80s <- subset(health_data_balanced, Age >= 80 & Age <= 89)

#check distribution
table(df_80s$Outcome)/nrow(df_80s) * 100  # % of each class
outcome_dist <- prop.table(table(df_80s$Outcome)) * 100
outcome_dist

#train test
split <- sample.split(df_80s, SplitRatio = 0.7)
train_data <- subset(df_80s, split == TRUE)
test_data <- subset(df_80s, split == FALSE)

x_train <- model.matrix(Outcome ~ . - 1, data = train_data)  # -1 removes intercept to prevent duplication
x_test <- model.matrix(Outcome ~ . - 1, data = test_data)
y_train <- train_data$Outcome
y_test <- test_data$Outcome

# LASSO Multinomial Models
lasso_model_80s <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1) #standardized by default

best_lambda_80s <- lasso_model_80s$lambda.min
lasso_model_80s$lambda.min

#View Lasso selected variables
coefficients_80s <- coef(lasso_model_80s, s = "lambda.min")
coefficients_80s

lasso_model_80s_prediction <- predict(lasso_model_80s, newx = x_test, s = "lambda.min", type = "response")
lasso_model_80s_prediction <- drop(lasso_model_80s_prediction)  # removes the unnecessary third dimension
pred_class_indices_80s  <- max.col(lasso_model_80s_prediction, ties.method = "first")
pred_class_80s <- factor(pred_class_indices_80s, levels = 1:3, labels = levels(y_test))
confusion_80s <- confusionMatrix(as.factor(pred_class_80s), as.factor(y_test))


