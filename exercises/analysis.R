# ====================================================
#   Biomedical Data Analysis 
# ====================================================

# You receive this script from your supervisor. It is an analysis a previous PhD
# student has done. Unfortunately they didn't put any comments. 
# Can you find out what happens in this script? Discuss with your neighbors.
# Can you see issues? Is there something you would change in this script?

library(tidyverse)
library(ggplot2)

df <- read_rds('patient_data.rds')
head(df)
str(df)

colSums(is.na(df))
df <- na.omit(df)
colSums(is.na(df))

df$total_chol <- df$hdl + df$ldl + (df$tg/5) 

high_chol_patients <- df %>%
  filter(total_chol > 240) %>%
  select(patient_id, age, total_chol, sex)

table(high_chol_patients$sex)

high_chol_patients <- high_chol_patients %>%
  mutate(sex_fixed = ifelse(sex == 'f', 'F', sex))

bp1 <- df %>%
  unnest(bp_readings) %>%
  select(patient_id, time_point, blood_pressure)

bp2 <- bp1 %>% pivot_wider(names_from = time_point, values_from = blood_pressure)
mean_bp <- rowMeans(bp2[,2:6])
bp2$mean <- mean_bp

for (i in 1:nrow(bp2)) {
  if (bp2$mean[i] > 140) {
    bp2$bp_category[i] <- "Hypertension"
  } else if (bp2$mean[i] < 120) {
    bp2$bp_category[i] <- "elevated BP"
  } else {
    bp2$bp_category[i] <- "Normal BP"
  }
}

merge_df <- merge(df, bp2, by = 'patient_id')

men <- merge_df %>%
  filter(sex == 'M')
women <- merge_df %>%
  filter(sex == 'F')

ggplot(men, aes(x = age, fill = bp_category)) +
  geom_histogram(position = 'dodge', binwidth = 10)
ggplot(women, aes(x = age, fill = bp_category)) +
  geom_histogram(position = 'dodge', binwidth = 10)

