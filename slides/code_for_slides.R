
# Test data
tree_wide <- tibble("Type species" = c('A', 'Q', 'P'),
                        "Site A" = c(12, 4, 35),
                        "Site B" = c(1, 4, 5),
                        "Site C" = c(2, 6, 5),
                        "Site D" = c(6, 33, 1)
                        
                        )



tree_wide <- tree_long %>% 
  pivot_wider(names_from = "Site", 
              values_from = "Average diameter (cm)")

tree_long <- tree_wide %>% 
  pivot_longer(cols = starts_with("Site"),
               names_to = "Site",
               values_to = "Average diameter (cm)")







