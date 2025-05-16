
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

# Randomize the order of rows and remove 18 samples
diabetes_meta <- diabetes_meta[sample(18:nrow(diabetes_meta)), ]

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


write_csv(df, './boston.csv')

############ Add serum calcium levels to Diabetes data ############ 

#HZ path
setwd('C:/Users/pnv719/Documents/HeaDS/Courses/R4DS/R4DataScience/data/')

diabetes_clinical <- read_excel('../data/diabetes_clinical_toy_messy.xlsx')
head(diabetes_clinical)

#https://www.uclahealth.org/medical-services/surgery/endocrine-surgery/patient-resources/patient-education/normal-calcium-levels
# High calcium levels in the blood may result from overactive parathyroid glands (hyperparathyroidism), certain cancers, or excessive vitamin D intake. 
# Low calcium levels can arise from conditions such as vitamin D deficiency, kidney disease, or insufficient parathyroid hormone production.
#normal range: 8.6 - 10.3 mg/dL 
#extrapolated low range: 7.3 - 8.5 mg/dL 
#extrapolated 

diabetes_clinical <- diabetes_clinical %>%
  mutate(Serum_ca2 = round(rnorm(nrow(diabetes_clinical), mean = 9.45, sd = 0.25),1))

writexl::write_xlsx(diabetes_clinical, './diabetes_clinical_toy_messy.xlsx')

###################### Make data for script exercise #####################

#Background: https://www.verywellhealth.com/high-cholesterol-high-blood-pressure-5204455
#High blood pressure and high cholesterol both damage the inner lining of blood 
#vessels, known as the endothelium. Over time, endothelial damage contributes 
#to the buildup of cholesterol plaques and inflammatory cells in the blood 
#vessels throughout the body, known as atherosclerosis.

#high bp and high chol together may be bad/high risk for cardiovascular events

n_pat <- 50

#run set.seed every time before running rnorm or you wil lget different results
set.seed(123)
patient_data <- tibble(
  patient_id = sample(1:9999, n_pat),
  age = sample(30:80, n_pat, replace = TRUE),
  sex = sample(c("M", "F"), n_pat, replace = TRUE),
  hdl = round(rnorm(n_pat, mean = 56, sd = 12),0),
  ldl = pmax(round(rnorm(n_pat, mean = 125, sd = 35),0),60),
  tg = round(rnorm(n_pat, mean = 150, sd = 45),0) #triglycerides
  )

#check total cholesterol
patient_data$total_chl <- round(patient_data$hdl + patient_data$ldl + (patient_data$tg/5),0)

#for females hdl should not be < 50
patient_data %>%
  count(sex == 'F' & hdl < 50)

#for males hdl should not be < 40
patient_data %>%
  count(sex == 'M' & hdl < 40)

#fix a bit
patient_data[patient_data$patient_id == 7989, 'tg'] <- 185
patient_data[patient_data$patient_id == 3995, 'hdl'] <- 62
patient_data[patient_data$patient_id == 6746, 'hdl'] <- 55
patient_data[patient_data$patient_id == 6672, 'hdl'] <- 57
patient_data[patient_data$patient_id == 4761, 'ldl'] <- 62
patient_data[patient_data$patient_id == 2504, 'hdl'] <- 52
patient_data[patient_data$patient_id == 9209, 'tg'] <- 220
patient_data[patient_data$patient_id == 9506, 'tg'] <- 140
patient_data[patient_data$patient_id == 7391, 'tg'] <- 167


#add specific mess:
patient_data[patient_data$patient_id == 8469, 'sex'] <- 'f'
patient_data[patient_data$patient_id == 2980, 'sex'] <- 'f'
patient_data[patient_data$patient_id == 2504, 'sex'] <- 'm'
patient_data[patient_data$patient_id == 1614, 'sex'] <- 'f'
patient_data[patient_data$patient_id == 3937, 'sex'] <- 'm'


#drop total chol column since this will be calculated in the analysis script
patient_data <- patient_data %>%
  select(-total_chl)

#introduce some NA values
#choose 3 random indices
make_na <- c(2,12,28)
patient_data$ldl[make_na] <- NA
make_na <- c(45,33)
patient_data$tg[make_na] <- NA


#generate blood pressure data

#create a vector with bp status
set.seed(123)
bp_status <- sample(c("Normal", "Elevated", "Hypertension"), 
                    size = n_pat, replace = TRUE, prob = c(0.6, 0.29, 0.11))

table(bp_status)

# Generate Systolic Blood Pressure (SBP) based on category
set.seed(123)
sbp <- ifelse(bp_status == "Normal", 
              round(rnorm(n_pat, mean = 110, sd = 5),0),  # Normal BP
              ifelse(bp_status == "Elevated",
                     round(rnorm(n_pat, mean = 130, sd = 4),0),  # Elevated BP
                     round(rnorm(n_pat, mean = 150, sd = 6),0)))  # Hypertension


#expand to five measurements per person

# Generate 4 additional SBP measurements per original value
sbp_expanded <- sapply(sbp, function(x) round(rnorm(4, mean = x, sd = 2),0))

sum(colMeans(sbp_expanded) > 140)
sum(colMeans(sbp_expanded) < 120) #normal
sum(colMeans(sbp_expanded) > 120 & sbp < 140) #elevated

#wrangle into tibble in long format
sbp_total <- rbind(sbp, sbp_expanded)
rownames(sbp_total) <- paste0('bp',1:nrow(sbp_total))  
sbp_total <- as_tibble(t(sbp_total))
sbp_total$patient_id <- patient_data$patient_id
sbp_total <- sbp_total %>% 
  pivot_longer(cols = starts_with('bp'),
               names_to = 'time_point',
               values_to = 'blood_pressure')

#join with patient data
patient_data <- patient_data %>%
  left_join(sbp_total, by = 'patient_id')

#nest
patient_data <- patient_data %>%
  group_by(patient_id) %>%
  nest(bp_readings = c(time_point, blood_pressure)) %>%
  ungroup()

#HZ path
setwd('C:/Users/pnv719/Documents/HeaDS/Courses/R4DS/R4DataScience/data/')
write_rds(patient_data, 'patient_data.rds')




############ OBSTETRICS DATASET ############ 

#install.packages("medicaldata")
library(tidyverse)
library(medicaldata)
library(mice)

## Show available dataset
#data(package = "medicaldata")

# Pull periodontal disease / obstetrics data
opt <- medicaldata::opt %>% 
  as_tibble()

# Cutoff for number of missing values 30% 
NAcutoff <- round(nrow(opt)*0.30)


# Trim trailing whitespace
# Sub empty fields with NA
# Missing values denoted by . are sub. with NA
# Combined factor for race

opt <- opt %>%
  mutate(across(where(is.factor), ~ str_trim(.)),
         PID = as.character(PID),
         Hisp=ifelse(Hisp=="", "No", as.character(Hisp)),
         Hypertension = ifelse(Hypertension=='Y', "Yes", "No"),
         N.prev.preg=ifelse(Prev.preg=='No', 0, N.prev.preg),
         Birthweight= as.numeric(Birthweight), 
         BMI=as.numeric(BMI)) %>% 
  mutate(across(everything(), ~ ifelse(.=="", NA, .)),
         combin = paste0(Black, White, Nat.Am, Asian, Hisp)) %>%
  mutate(across(everything(), ~ ifelse(.==".", NA, .)))


# Remove any NA in outcomes before imputation 
# Remove too unbalanced factor vars before imputation
# Remove columns with > 30% missing

opt <- opt %>% 
  filter(!if_any(all_of(c("Apgar1", "Apgar5", "Birthweight", "Any.SAE.", 
                          "Fetal.congenital.anomaly", "Preg.ended...37.wk", "GA.at.outcome")), is.na)) %>%
  select(-c(Prev.preg, Birth.outcome, Drug.Add, Polyhyd, Mom.HIV.status,
            X..Vis.Elig, X1st.Miss.Vis, BL.Cortico, O1B1, O1B5, O61, O65, 
            O81, O85, OTNF1, Oligo)) %>%
  select(where(~sum(is.na(.)) < NAcutoff))


# Make combined race/ethnicity variable (short integer form)
# Vary rare race/ethnicity are removed (e.g. not enough data points)
Race <- opt %>% 
  group_by(combin) %>% 
  summarise(each = n()) %>%
  filter(each >= 20) %>% # The cutoff of 7 was picked based on results of summary output
  select(-each) %>%
  mutate(Race = as.character(1:nrow(.)))


# Joining new short race/ethnicity variable with full dataset, remove redundant columns.
opt <- left_join(opt, Race) %>%
  filter(!is.na(Race)) %>%
  select(-c(combin, Black:Hisp)) %>%
  relocate(Race, .after = Age) 


# Systematically mutate serum and medication treatment columns
Serum <- c(names(opt[,grep("^O", names(opt))]), "ETXU_CAT5")
Treat <- names(opt[,grep("Anti|Cortico|Bac", names(opt))])


opt <- opt %>%
  mutate(across(all_of(Serum), ~ as.numeric(.))) %>%
  mutate(across(all_of(Treat), ~ as.factor(.))) %>%
  mutate(across(.cols = where(is.character) & !any_of(c("PID")), .fns = as.factor)) %>%
  mutate(N.PAL.sites = as.factor(ifelse(N.PAL.sites >= 2 , "3-33", as.character(N.PAL.sites)))) 




# Remove ID (should not be used for imputation)
#Outcomes <- c(
#  "PID", 
#  "Apgar1", 
#  "Apgar5",
#  "Any.SAE.",
#  "Birthweight", 
#  "Fetal.congenital.anomaly", 
#  "Preg.ended...37.wk", 
#  "GA...1st.SAE",
#  "GA.at.outcome")

#optOut <- opt %>% 
#  select(all_of(Outcomes))

#opt <- opt %>% 
#  select(-all_of(Outcomes))

PID <- opt %>% 
  select(PID)


# Pattern of missingness
md.pattern(opt[,-1], rotate.names = TRUE)

# Check the methods used for imputing each variable
init <-  mice(opt[,-1], maxit=0) 
meth <-  init$method
meth

# Impute missing values
optImp <- mice(opt[,-1], maxit=10, method = meth, seed = 1234) 



# Check imputed
stripplot(optImp, Use.Tob, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, N.prev.preg, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, Any.stillbirth, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, Any.live.ptb.sb.sp.ab.in.ab, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, Bact.vag, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, Gest.diab, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, UTI, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, Pre.eclamp, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, V5.Antibio, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, EDC.necessary., col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, V5.CAL.avg, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, V5.PD.avg, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, V5..BOP, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, V5.Bac.vag, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, V3.Anti.inf, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, OFIBRIN1, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(optImp, OCRP5, col=c("grey",mdc(2)),pch=c(1,20))


#optImp <- bind_cols(optOut, complete(optImp, 1))


# Bind PID back to dataset
optImp <- bind_cols(PID, complete(optImp, 1))

# Full clean version to have
write_csv(optImp, file = 'Obstetrics_Periodontal_Therapy.csv')





# Check balance of factor variables for ML
factor_counts <- optImp  %>%
  dplyr::select(where(is.factor)) %>%
  map(~ as.data.frame(table(.))) %>%
  imap(~ setNames(.x, c("Level", "Count")) %>% mutate(Variable = .y)) %>%
  bind_rows() %>%
  relocate(Variable, .before = Level)


factor_counts

# Smaller more balanced version for LASSO and R
optML <- optImp %>% 
  dplyr::select(-c(Diabetes, 
                   Use.Alc,
                   Fetal.congenital.anomaly,  
                   Any.stillbirth, 
                   Hypertension,
                   Traumatic.Inj,
                   BL.Bac.vag,
                   ETXU_CAT1)) 


save(optML, file = 'Obt_Perio_ML.Rdata')


