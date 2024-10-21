# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)  # For VIF

# Load the dataset
health_data <- read.csv("health_data.csv")

# Check the structure of the data
str(health_data)

# Convert categorical variables to factors
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

# Split BloodPressure column into Systolic and Diastolic as integers
blood_pressure_split <- strsplit(as.character(health_data$BloodPressure), "/")
health_data$Systolic <- as.integer(sapply(blood_pressure_split, `[`, 1))
health_data$Diastolic <- as.integer(sapply(blood_pressure_split, `[`, 2))

# Categorize Blood Pressure Levels
health_data <- health_data %>%
  mutate(
    BloodPressureCategory = case_when(
      Systolic <= 120 & Diastolic <= 80 ~ "Normal",
      Systolic >= 120 & Systolic <= 129 & Diastolic < 80 ~ "Elevated",
      (Systolic >= 130 & Systolic <= 139) | (Diastolic >= 80 & Diastolic <= 89) ~ "Hypertension Stage 1",
      Systolic >= 140 | Diastolic >= 90 ~ "Hypertension Stage 2",
      Systolic > 180 | Diastolic > 120 ~ "Hypertensive Crisis",
      TRUE ~ "Unknown"
    ),
    BloodPressureCategory = as.factor(BloodPressureCategory)  # Convert to factor
  )

# Categorize Total Cholesterol Levels
health_data <- health_data %>%
  mutate(
    CholesterolCategory = case_when(
      CholesterolLevel < 200 ~ "Heart-Healthy",
      CholesterolLevel >= 200 & CholesterolLevel <= 239 ~ "At-Risk",
      CholesterolLevel >= 240 ~ "Dangerous",
      TRUE ~ "Unknown"
    ),
    CholesterolCategory = as.factor(CholesterolCategory)  # Convert to factor
  )

# Check the modified data structure
str(health_data)

# Write the modified data to a new CSV file
write.csv(health_data, "modified_Healthdata.csv", row.names = FALSE)

# Multiple Regression Model 1: Predict HealthcareCost using demographic and health-related variables
model1 <- lm(HealthcareCost ~ Age + BMI + CholesterolLevel + Systolic + Diastolic + 
               Gender + SmokingStatus + AlcoholConsumption + ExerciseFrequency + 
               Diabetes + HeartDisease + PhysicalActivityLevel + BloodPressureCategory + 
               CholesterolCategory, data = health_data)
summary(model1)

# Check for multicollinearity using VIF
vif(model1)

# Analyze Outcome using demographic and health-related variables
model2 <- lm(as.numeric(Outcome) ~ Age + BMI + CholesterolLevel + HealthcareCost + 
               Systolic + Diastolic + Gender + SmokingStatus + AlcoholConsumption + 
               ExerciseFrequency + Diabetes + HeartDisease + PhysicalActivityLevel + 
               BloodPressureCategory + CholesterolCategory, data = health_data)
summary(model2)

# Compare models using AIC
AIC(model1, model2)

# Visualize relationships between variables
ggplot(health_data, aes(x = Age, y = HealthcareCost, color = Outcome)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Healthcare Cost vs Age", x = "Age", y = "Healthcare Cost")

ggplot(health_data, aes(x = BMI, y = HealthcareCost, color = Outcome)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Healthcare Cost vs BMI", x = "BMI", y = "Healthcare Cost")

ggplot(health_data, aes(x = CholesterolLevel, y = HealthcareCost, color = Outcome)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Healthcare Cost vs Cholesterol Level", x = "Cholesterol Level", y = "Healthcare Cost")

# Save all models and visualizations to a PDF file
pdf("health_data_analysis_output.pdf")

# Print model summaries to the PDF
print(summary(model1))
print(summary(model2))

# Plot visualizations to the PDF
ggplot(health_data, aes(x = Age, y = HealthcareCost, color = Outcome)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Healthcare Cost vs Age", x = "Age", y = "Healthcare Cost")

ggplot(health_data, aes(x = BMI, y = HealthcareCost, color = Outcome)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Healthcare Cost vs BMI", x = "BMI", y = "Healthcare Cost")

ggplot(health_data, aes(x = CholesterolLevel, y = HealthcareCost, color = Outcome)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Healthcare Cost vs Cholesterol Level", x = "Cholesterol Level", y = "Healthcare Cost")

# Close the PDF device
dev.off()