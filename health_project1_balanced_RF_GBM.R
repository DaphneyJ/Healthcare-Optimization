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

health_data <- health_data_balanced

########################################################################

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
multinom_model_full <- vglm(Outcome ~ ., data = train_data, family = multinomial)
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
lasso_model_scaled <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1, standardize = TRUE) #explicit scaling 
lasso_model_no_scaling <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1, standardize = FALSE) #NO scaling

#plot lasso models optimaL lambda
par(mfrow=c(2,2))
plot(lasso_model)
plot(lasso_model_scaled)
plot(lasso_model_no_scaling)

best_lambda <- lasso_model$lambda.min
lasso_model$lambda.min
lasso_model_scaled$lambda.min
lasso_model_no_scaling$lambda.min  #too large; strong regularization needed 

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
#confusionMatrix(pred_multinom_full, y_test)$overall[1]



######################## Random Forest ###########################
rf_model <- randomForest(Outcome~., data = train_data, 
                         importance = TRUE)

#View feature importance
importance(rf_model)  
importance(rf_model, type=2)
varImpPlot(rf_model)

#Initial RF
rf_pred <- predict(rf_model, test_data, type = 'class')
table(rf_pred, y_test)
rf_confusion <- confusionMatrix(rf_pred, y_test)
rf_confusion

#Parameter tuning trying to find optimal ntree
rf_model2 <- randomForest(Outcome~., data = train_data, 
                          ntree = 700, importance = TRUE)
rf2_pred <- predict(rf_model2, test_data, type = 'class')
rf2_confusion <- confusionMatrix(rf2_pred, y_test)
rf2_confusion

#Parameter tuning trying to find optimal mtry
oob_errs <- vector(length = 10)
for(i in 1:10) {
  i_model <- randomForest(Outcome ~., data = train_data,
                          mtry = i,ntree=500)
  oob_errs[i] <- i_model$err.rate[nrow(i_model$err.rate),1]
}

oob_errs
# 0.3919740 0.3627934 0.3659891 0.3665654 0.3679100 0.3697611 0.3715074 0.3723631 0.3719440 0.3736204

#mtry 2 is the optimal mtry value

#optimal RF model
rf_model_3 <- randomForest(Outcome~., data = train_data, 
                           mtry = 2, ntree = 500, importance = TRUE)
rf3_pred <- predict(rf_model_3, test_data, type = 'class')
rf3_confusion <- confusionMatrix(rf3_pred, y_test)
rf3_confusion
tn_rf <- confusion_boosting$table[1, 1]  # True Negative
fn_rf <- confusion_boosting$table[2, 1]  # False Negative
fp_rf <- confusion_boosting$table[1, 2]  # False Positive
tp_rf <- confusion_boosting$table[2, 2]  # True Positive
precision_rf <- tp / (tp + fp)
recall_rf <- tp / (tp + fn)
f1_score_rf <- 2 * (precision * recall) / (precision + recall)
############################# Boosting ####################################

boost_model <- gbm(Outcome ~., data = train_data, 
                   distribution = "multinomial", n.trees = 1000, shrinkage = .01, 
                   interaction.depth = 3, cv.folds = 10)
test_boost_model <- ifelse(predict(boost_model, newdata = test_data, n.trees = 1000, 
                                   type = "response") < 0.5, 0, 1)
test_boost_model <- drop(test_boost_model)
y_matrix = model.matrix(~ y_test + 0) #converting test outcomes to useable format
y_array = as.array(y_matrix)
test_error_boost_model <- mean(test_boost_model != y_array)
test_error_boost_model

#parameter tuning for optimal trees
optimal_trees <- gbm.perf(boost_model, method = 'cv')
optimal_trees
#optimal number of trees is 994

summary(boost_model)

test_model_optimal_trees <- ifelse(predict(boost_model, newdata = test_data, n.trees = optimal_trees, 
                                           type = "response") < 0.5, 0, 1)
test_model_optimal_trees <- drop(test_model_optimal_trees)
test_error_optimal_trees <- mean(test_model_optimal_trees != y_array)
test_error_optimal_trees

boost_accuracy <- 1- test_error_optimal_trees
boost_accuracy

confusion_boosting <- confusionMatrix(factor(test_model_optimal_trees), factor(y_array))
tn_gbm <- confusion_boosting$table[1, 1]  # True Negative
fn_gbm <- confusion_boosting$table[2, 1]  # False Negative
fp_gbm <- confusion_boosting$table[1, 2]  # False Positive
tp_gbm <- confusion_boosting$table[2, 2]  # True Positive
precision_gbm <- tp / (tp + fp)
recall_gbm <- tp / (tp + fn)
f1_score_gbm <- 2 * (precision * recall) / (precision + recall)

