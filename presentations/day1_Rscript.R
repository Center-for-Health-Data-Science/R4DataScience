##########################################################
# Project: R for Data Science - Day 1 - How to R script #
# Author: Helene Wegener                                #
# Date: 8 Novmeber 2024                                 #
##########################################################

###################################################
############## Set working directory ##############
###################################################

getwd()
setwd('~/Desktop/DataLab/R2/')
getwd()

###################################################
################## Load packages ################## 
###################################################

library(tidyverse)
# library(readxl)

###################################################
#################### Load data #################### 
###################################################
diabetes <- read_csv('data/diabetes.csv')

###################################################
################## Inspect data ################### 
###################################################

dim(diabetes)

str(diabetes)

is.na(diabetes)

colSums(is.na(diabetes))

###################################################
################### Clean data #################### 
###################################################

# Remove NAs 
# Any rows with NA in any column 
diabetes %>% drop_na() # tidyverse
na.omit(diabetes) # base R

# Any rows with NA in specific column
diabetes %>% filter(!is.na(GeneticRisk)) # tidyverse
df[!is.na(df$your_column), ] # base R

# Remove ID column 



# Exercise: make clean version of diabetes data 


