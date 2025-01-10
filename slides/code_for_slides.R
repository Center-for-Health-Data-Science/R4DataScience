
# Test data
tree_wide <- tibble("Type species" = c('Acer rubrum', 'Quercus alba', 'P'),
                        "Site A" = c(12, 4, 35),
                        "Site B" = c(1, 4, 5),
                        "Site C" = c(2, 6, 5),
                        "Site D" = c(6, 33, 1)
                        )

# Pivot 

tree_long <- tree_wide %>% 
  pivot_longer(cols = starts_with("Site"),
               names_to = "Site",
               values_to = "Average diameter (cm)")

tree_wide <- tree_long %>% 
  pivot_wider(names_from = Site, 
              values_from = `Average diameter (cm)`)

# Nesting

tree_long_nested <- tree_long %>% 
  group_by(`Type species`) %>% 
  nest(Data = c(Site, `Average diameter (cm)`)) %>% 
  ungroup()

tree_long_nested %>% 
  filter(`Type species` == 'Quercus alba') %>% 
  pull(Data) 


# Remove variables 
rm(tree_wide, 
   tree_long, 
   tree_long_nested
   )


######## PCA ######## 

library(ggfortify)
library(glue)

iris_colnames <- colnames(iris)[1:4]

for (colname1 in iris_colnames){
  for (colname2 in iris_colnames){
    if (!(colname1 == colname2)){
      
      iris %>% 
        ggplot(aes_string(x = colname1, y = colname2, color = 'Species')) + 
        geom_point()
      
      ggsave(glue('~/Desktop/DataLab/R4DataScience/figures/iris_{colname1}_{colname2}.png'), width = 6.5, height = 4.36)
      
    }
  }
}


df <- iris[1:4]
pca_res <- prcomp(df, scale. = TRUE)
autoplot(pca_res, data = iris, color = "Species")
ggsave(glue('~/Desktop/DataLab/R4DataScience/figures/iris_PCA.png'), width = 8.5, height = 5.5)

autoplot(pca_res, data = iris, color = "Species", loadings = TRUE, loadings.label = TRUE)
ggsave(glue('~/Desktop/DataLab/R4DataScience/figures/iris_PCA_loadings.png'), width = 8.5, height = 5.5)
