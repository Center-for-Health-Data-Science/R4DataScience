
setwd('~/Desktop/DataLab/R4DataScience/data/')

library(messy)
############ SALES ############ 

df_sales <- data.frame(
  ID = 1:10,
  Name = c("Alice", "Bob", "Charlie", "Sophie", "Eve", "Frank", "Grace", "Hannah", "Ian", "Jack"),
  Age = c(25, 30, 22, 35, 28, NA, 40, 29, 21, 33),
  Sex = c("Female", "Male", "Male", "Female", "Female", "Male", "Female", "Female", "Male", "Male"),
  sales_2020 = c(100, 200, 150, 300, 250, NA, 400, 500, 450, 300),
  sales_2021 = c(110, 210, 160, 320, 240, 260, 420, 510, 460, 310),
  sales_2022 = c(120, 220, 170, 340, 250, 270, 430, NA, 470, 320),
  sales_2023 = c(100, 230, 200, 250, 270, 280, 450, 500, 480, 290)
)

writexl::write_xlsx(df_sales, './df_sales_1.xlsx')

df_sales_messy <- add_whitespace(df_sales, cols = "Sex", messiness = 0.5)

write_csv(df_sales_messy, './df_sales_messy.csv')

############ DIABETES ############ 
library(tidyverse)

diabetes <- read_csv('diabetes.csv')

diabetes_clinical <- diabetes %>% select(ID, Sex, Age, BloodPressure, GeneticRisk, BMI, PhysicalActivity, Smoker, Diabetes)
diabetes_meta <- diabetes %>% select(ID, Married, Work)

# Randomize the order of rows
diabetes_meta <- diabetes_meta[sample(1:nrow(diabetes_meta)), ]

# Make the data messy 
# remotes::install_github("nrennie/messy")

set.seed(101)
diabetes_clinical_messy <- change_case(diabetes_clinical, messiness = 0.01, cols = 'Sex')
diabetes_meta_messy <- add_whitespace(diabetes_meta, messiness = 0.01, cols = 'Married')

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

############ DIABETES GLUCOSE ############ 
ids <- diabetes$ID

# Set the number of samples
num_diabetic <- 265
num_non_diabetic <- 267

# Define ranges for glucose levels
# Non-diabetic: [Glucose] 0' < 6.0 mmol/L, [Glucose] 120' < 7.8 mmol/L
# Diabetic: [Glucose] 0' > 7.0 mmol/L, [Glucose] 120' > 11.1 mmol/L
# Impaired Glucose Tolerance: 6.1 - 6.9 mmol/L (0'), 7.9 - 11.0 mmol/L (120')

# Function to generate random glucose levels within specified range
generate_glucose <- function(n, min_0, max_0, min_120, max_120) {
  Glucose_0 <- runif(n, min = min_0, max = max_0)
  Glucose_120 <- runif(n, min = min_120, max = max_120)
  
  # Ensure Glucose_120 >= Glucose_0 (biological constraint)
  Glucose_120 <- pmax(Glucose_0, Glucose_120)
  
  Glucose_60 <- runif(n, min = Glucose_0 + (Glucose_120 - Glucose_0) * 0.4, max = Glucose_0 + (Glucose_120 - Glucose_0) * 0.7)
  data.frame(Glucose_0, Glucose_60, Glucose_120)
}

# Generate impaired glucose tolerance measurements
impared_glucose <- generate_glucose(50, 6, 7, 7.5, 11.5)

# Generate non-diabetic measurements
# non_diabetic <- generate_glucose(num_non_diabetic-25, 3.9, 5.9, 3.9, 7.7)
# non_diabetic <- rbind(non_diabetic, impared_glucose[1:25, ])
# non_diabetic$ID <- diabetes %>% filter(Diabetes == 0) %>% pull(ID)

non_diabetic_2 <- generate_glucose(num_non_diabetic, 3.9, 7, 3.9, 11.5)
non_diabetic_2$ID <- diabetes %>% filter(Diabetes == 0) %>% pull(ID)

# Generate diabetic measurements
# diabetic <- generate_glucose(num_diabetic-25, 7.1, 15, 11.2, 20)
# diabetic <- rbind(diabetic, impared_glucose[26:50, ])
# diabetic$ID <- diabetes %>% filter(Diabetes == 1) %>% pull(ID)

diabetic_2 <- generate_glucose(num_diabetic, 6, 15, 7.5, 20)
diabetic_2$ID <- diabetes %>% filter(Diabetes == 1) %>% pull(ID)

# Combine into one dataset
# df_glucose <- rbind(non_diabetic, diabetic)
df_glucose_2 <- rbind(non_diabetic_2, diabetic_2)

# Export datasets
# writexl::write_xlsx(diabetes_clinical, './diabetes_clinical_toy.xlsx')
writexl::write_xlsx(diabetes_clinical_messy, './diabetes_clinical_toy_messy.xlsx')
# writexl::write_xlsx(diabetes_meta, './diabetes_meta_toy.xlsx')
# writexl::write_xlsx(diabetes_meta_messy, './diabetes_meta_toy_messy.xlsx')
write_csv(diabetes_meta_messy, './diabetes_meta_toy_messy.csv')
writexl::write_xlsx(df_glucose_2, './df_glucose.xlsx')

############ Boston housing data (linear regression on house prices) ############ 

#HZ path
setwd('C:/Users/pnv719/Documents/HeaDS/Courses/R4DS/R4DataScience/data/')

df <- as_tibble(read.csv('boston.csv'))
df

hist(df$indus, breaks = 30)
hist(df$dis, breaks = 30)
hist(df$medv, breaks = 30)

#####adding a categorical variable

# Set seed to ensure reproducibility
set.seed(123)

#it would make sense for houses in urban areas to be expensive
assign_neighborhood_probabilistic <- function(medv) {
  if (medv > 35) {
    sample(c("Urban", "Suburban", "Rural"), size = 1, prob = c(0.8, 0.1, 0.1))
  } else if (medv > 20) {
    sample(c("Urban", "Suburban", "Rural"), size = 1, prob = c(0.3, 0.4, 0.3))
  } else {
    sample(c("Urban", "Suburban", "Rural"), size = 1, prob = c(0.1, 0.4, 0.5))
  }
}

# Apply the probabilistic neighborhood assignment to the dataset
df <- df %>%
  mutate(neighborhood = map_chr(medv, assign_neighborhood_probabilistic))

#check distributions
ggplot(df, aes(x=medv,fill=neighborhood)) +
  geom_histogram(position = 'dodge')

#omit dis var
df <- df %>%
  select(-dis, -zn, -ID)
  #select(-ID)


#distance to city center and nr rooms are pretty good predictors of price
ggplot(df, aes(x=dis,y=medv,color=rm)) +
  geom_point()

ggplot(df, aes(x=rm,y=medv,color=dis)) +
  geom_point()

#crime doesn't matter much because the influence is pretty much 
ggplot(df, aes(x=crim,y=medv,color=dis)) +
  geom_point()

write_csv(df, './boston.csv')

