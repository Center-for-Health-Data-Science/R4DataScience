diab <- read_xlsx('../data/diabetes_clinical_toy_messy.xlsx')
diabMeta <- read_csv('../data/diabetes_meta_toy_messy.csv')
glu <- read_xlsx('../data/df_glucose.xlsx')


diab_clean <- diab %>% 
  filter(!if_any(everything(), is.na) & Age != 0 & BloodPressure !=0 & BMI!=0) %>%
  mutate(Diabetes = as.integer(Diabetes), Age = as.integer(Age), Sex = ifelse(Sex == "male", "Male", 
                      ifelse(Sex == "FEMALE", "Female", as.character(Sex)))) 


diab_clean_meta <- left_join(diab_clean, diabMeta)

diab_glu <- left_join(diab_clean_meta, glu)

diabnum <- diab_glu %>% 
  select(-ID) %>% 
  select_if(is.numeric)
  


pca_res <- prcomp(diabnum, scale. = TRUE)
autoplot(pca_res, data = diab_clean_meta, colour='Diabetes')


pclist <- list()

for (i in 1:length(pca_res$sdev)) {
  varpc <- pca_res$sdev[i]^2/sum(pca_res$sdev^2)*100
  npc <- paste0('PC', i)
  
  pclist[[i]] <- tibble(PC= npc, Variance = varpc)
  
}

PCdf <- map_df(pclist, tibble::as_tibble)





# Test for outliers and normality:

# Cooks distance
model1 <- lm(Diabetes ~ ., data = diab_glu)
plot(model1, 4)

# Zscore
z_scores <- scale(diab_glu$PhysicalActivity)
which(abs(z_scores) > 3.29)
