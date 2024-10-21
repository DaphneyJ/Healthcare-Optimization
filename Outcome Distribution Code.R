#Checking balance of dataset

health_data <- read.csv("health_data.csv")

outcome_breakdown <- table(health_data$Outcome)
print(outcome_breakdown)

outcome_brakdown_percentage <- prop.table(outcome_breakdown) * 100
print(outcome_breakdown_percentage)

#example code to ensure even outcome distribution in train & test data
#https://www.rdocumentation.org/packages/caTools/versions/1.17.1/topics/sample.split

library(caTools)
set.seed(6203)

train_data <- sample.split(health_data$Outcome, SplitRatio = 0.7)

health_train <- subset(health_data, split == TRUE)
health_test <- subset(health_data, split == FALSE)