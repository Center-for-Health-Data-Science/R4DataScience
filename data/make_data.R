
setwd('~/Desktop/DataLab/R4DataScience/data/')

diabetes <- read_csv('diabetes.csv')

library(tidyverse)

diabetes_clinical <- diabetes %>% select(ID, Sex, Age, BloodPressure, GeneticRisk, BMI, PhysicalActivity, Smoker, Diabetes)
diabetes_meta <- diabetes %>% select(ID, Married, Work)

rm(diabetes)

# Randomize the order of rows
diabetes_meta <- diabetes_meta[sample(1:nrow(diabetes_meta)), ]

# Make the data messy 
# remotes::install_github("nrennie/messy")
library(messy)
set.seed(101)
diabetes_clinical_messy <- change_case(diabetes_clinical, messiness = 0.01, cols = 'Sex')
diabetes_meta_messy <- messy(diabetes_meta, messiness = 0.005)

# diabetes_messy <- diabetes_messy %>% 
#   mutate(Fasting_Blood_Sugar = as.numeric(Fasting_Blood_Sugar),
#          Post_Meal_Blood_Sugar = as.numeric(Post_Meal_Blood_Sugar),
#          HbA1c = as.numeric(HbA1c),
#          Age = as.numeric(Age),
#          BMI = as.numeric(BMI),
#          Blood_Pressure = as.numeric(Blood_Pressure)
#          )
# 
# diabetes_meta_messy <- diabetes_meta_messy %>% 
#   mutate(Diagnosis_Year = as.numeric(Diagnosis_Year),
#          Follow_Up_Visits = as.numeric(Follow_Up_Visits)
#   )


# Export datasets
writexl::write_xlsx(diabetes_clinical, './data/diabetes_clinical_toy.xlsx')
writexl::write_xlsx(diabetes_clinical_messy, './data/diabetes_clinical_toy_messy.xlsx')
writexl::write_xlsx(diabetes_meta, './data/diabetes_meta_toy.xlsx')
writexl::write_xlsx(diabetes_meta_messy, './data/diabetes_meta_toy_messy.xlsx')


