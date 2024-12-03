
setwd('~/Desktop/DataLab/R4DataScience/data/')

n <- 100

set.seed(42) # For reproducibility

# Create the diabetes dataset
diabetes <- data.frame(
  ID = paste0("ID_", sprintf("%03d", 1:n)), # Longer IDs
  Fasting_Blood_Sugar = sample(100:160, n, replace = TRUE),
  Post_Meal_Blood_Sugar = sample(150:250, n, replace = TRUE),
  HbA1c = round(runif(n, 5.5, 8.0), 1),
  Age = sample(30:70, n, replace = TRUE),
  Sex = sample(c("Male", "Female"), n, replace = TRUE),
  BMI = round(runif(n, 18, 35), 1), # New column: Body Mass Index
  Blood_Pressure = sample(110:140, n, replace = TRUE) # New column: Blood Pressure
)

# Randomize the order of rows
diabetes <- diabetes[sample(1:nrow(diabetes)), ]

# View the dataset
head(diabetes)

# Create the diabetes_meta dataset
diabetes_meta <- data.frame(
  ID = paste0("ID_", sprintf("%03d", 1:n)), # Match the longer IDs
  Diagnosis_Year = sample(2000:2020, n, replace = TRUE),
  Treatment_Type = sample(c("Insulin", "Oral Medication", "Diet Control"), n, replace = TRUE),
  City = sample(c("Copenhagen", "Aarhus", "Odense", "Aalborg"), n, replace = TRUE), # New column: City
  Follow_Up_Visits = sample(2:12, n, replace = TRUE) # New column: Number of follow-up visits
)

# Randomize the order of rows
diabetes_meta <- diabetes_meta[sample(1:nrow(diabetes_meta)), ]

# View the dataset
head(diabetes_meta)

# Make the data messy 
# remotes::install_github("nrennie/messy")
library(messy)

diabetes_messy <- change_case(diabetes, messiness = 0.01, cols = 'Sex')
diabetes_meta_messy<- messy(diabetes_meta, messiness = 0.005)

# diabetes_messy_setset <- messy(diabetes[, c('ID', 'Sex') ], messiness = 0.01)
# diabetes_meta_messy_setset <- messy(diabetes_meta[, c('ID', 'Treatment_Type', 'City') ], messiness = 0.01)
# 
# diabetes_messy <- diabetes
# diabetes_messy$ID <- diabetes_messy_setset$ID
# diabetes_messy$Sex <- diabetes_messy_setset$Sex
# 
# diabetes_meta_messy <- diabetes_meta
# diabetes_meta_messy$ID <- diabetes_meta_messy_setset$ID
# diabetes_meta_messy$Treatment_Type <- diabetes_meta_messy_setset$Treatment_Type
# diabetes_meta_messy$City <- diabetes_meta_messy_setset$City

diabetes_messy <- diabetes_messy %>% 
  mutate(Fasting_Blood_Sugar = as.numeric(Fasting_Blood_Sugar),
         Post_Meal_Blood_Sugar = as.numeric(Post_Meal_Blood_Sugar),
         HbA1c = as.numeric(HbA1c),
         Age = as.numeric(Age),
         BMI = as.numeric(BMI),
         Blood_Pressure = as.numeric(Blood_Pressure)
         )

diabetes_meta_messy <- diabetes_meta_messy %>% 
  mutate(Diagnosis_Year = as.numeric(Diagnosis_Year),
         Follow_Up_Visits = as.numeric(Follow_Up_Visits)
  )


# Export datasets
writexl::write_xlsx(diabetes, 'diabetes_toy.xlsx')
writexl::write_xlsx(diabetes_messy, 'diabetes_toy_messy.xlsx')
writexl::write_xlsx(diabetes_meta, 'diabetes_meta_toy.xlsx')
writexl::write_xlsx(diabetes_meta_messy, 'diabetes_meta_toy_messy.xlsx')
write_csv(diabetes_meta_messy, 'diabetes_meta_toy_messy.csv')


