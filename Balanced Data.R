library(dplyr)

#Checking balance of dataset
health_data <- read.csv("health_data.csv")
outcome_breakdown <- table(health_data$Outcome)
print(outcome_breakdown)
outcome_breakdown_percentage <- prop.table(outcome_breakdown) * 100
print(outcome_breakdown_percentage)

#Converting data to factors 
health_data <- health_data %>%
  mutate(
    Gender = as.factor(Gender),
    SmokingStatus = as.factor(SmokingStatus),
    AlcoholConsumption = as.factor(AlcoholConsumption),
    ExerciseFrequency = as.factor(ExerciseFrequency),
    HeartDisease = as.factor(HeartDisease),
    Diabetes = as.factor(Diabetes),
    PhysicalActivityLevel = as.factor(PhysicalActivityLevel),
    DietQuality = as.factor(DietQuality),
    Outcome = as.factor(Outcome)
  )

## balancing dataset via random undersampling
#creating dfs based off outcomes 

healthy <- health_data %>%
  filter(Outcome == "Healthy")
at_risk <- health_data %>%
  filter(Outcome == "At Risk")
critical <- health_data %>%
  filter(Outcome == "Critical")

set.seed(6203)
samples_critical <- critical %>%
  sample_n(size = nrow(at_risk))
samples_healthy <- healthy %>%
  sample_n(size = nrow(at_risk))

health_data_balanced <- bind_rows(samples_healthy, at_risk, samples_critical)
health_data_balanced <- health_data_balanced[sample(nrow(health_data_balanced)),]

summary(health_data)
summary(health_data_balanced)
