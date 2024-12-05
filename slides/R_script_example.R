#########################################
# R for Data Science - How to R script #
# Author: DataLab HeaDS                #
# Date: 8 Novmeber 2024                #
########################################

###################################################
################## Load Packages ##################
###################################################
library(tidyverse)
library(readxl)

###################################################
#################### Load Data #################### 
###################################################
diabetes <- read_excel('~/Desktop/DataLab/R4DataScience/data/diabetes_toy.xlsx')

###################################################
################## Inspect Data ################### 
###################################################

# Check dimensions of data
dim(diabetes)

# Check structure of data
str(diabetes)

# Check for NA's in each column 
colSums(is.na(diabetes))

###################################################
############ Exploratory Data Analysis ############ 
###################################################

# Plot distribution of BMI
diabetes %>% 
  ggplot(aes(x = BMI)) + 
  geom_histogram(bins = 10)





