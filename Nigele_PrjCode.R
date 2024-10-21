# Clear the environment
rm(list = ls())

# Load necessary libraries
library(tree)
library(dplyr)

# Load the dataset
health_data <- read.csv("health_data.csv")

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
    BloodPressureCategory = as.factor(BloodPressureCategory)
  )

# Categorize Cholesterol Levels
health_data <- health_data %>%
  mutate(
    CholesterolCategory = case_when(
      CholesterolLevel < 200 ~ "Heart-Healthy",
      CholesterolLevel >= 200 & CholesterolLevel <= 239 ~ "At-Risk",
      CholesterolLevel >= 240 ~ "Dangerous",
      TRUE ~ "Unknown"
    ),
    CholesterolCategory = as.factor(CholesterolCategory)
  )

# Save the modified dataset
write.csv(health_data, "modified_Healthdata.csv", row.names = FALSE)

# ----- FUNCTION TO BUILD AND PLOT TREES USING A LOOP -----
# Open a PDF to save the tree plots
pdf("looped_decision_trees.pdf")

# Loop through each column in the dataset and create a tree
for (col_name in colnames(health_data)) {
  # Skip non-relevant columns
  if (col_name %in% c("PatientID", "BloodPressure")) next
  
  # Create the formula dynamically (e.g., Outcome ~ .)
  formula <- as.formula(paste(col_name, "~ ."))
  
  # Try building the tree and catch any errors
  try({
    # Fit the decision tree model
    tree.data <- tree(formula, data = health_data)
    
    # Check if the tree has more than one node
    if (nrow(tree.data$frame) > 1) {
      # Plot the tree
      plot(tree.data, main = paste("Tree for", col_name))
      text(tree.data, pretty = 0, cex = 0.4)  # Adjust text size
    } else {
      cat("\nThe tree for", col_name, "did not split.\n")
    }
  }, silent = TRUE)  # Continue loop even if there is an error
}

# Close the PDF device
dev.off()