# 2. Function that take dataframe and a numerical value and returns a boxplot. 
# Required packages: ggplot2
boxplot <- function(df, var_num){
  
  p <- df %>% 
    ggplot(aes_string(y = var_num)) + 
    geom_boxplot(fill = "#03579A") +
    labs(title = paste("Boxplot of", var_num)) + 
    theme_bw() + 
    theme(text = element_text(family="Avenir"))
  
  return(p)
  
}

# 8. Function that take dataframe and a numerical value and returns a density plot. 
# Required packages: ggplot2
bar_plot <- function(df, var_cat){
  
  p <- df %>% 
    ggplot(aes_string(x = var_cat)) + 
    geom_bar(fill = "#03579A") + 
    labs(title = paste("Barplot of", var_cat)) + 
    theme_bw() + 
    theme(text = element_text(family="Avenir"))
  
  return(p)
  
}

# 11. Like #2 boxplot, with error handling.
# Required packages: ggplot2
boxplot_2 <- function(df, var_num){
  
  # Check if the numerical variable is numerical 
  if (!is.numeric(df[[var_num]])){
    stop('The numerical variable (var_num) must be numcerial.')
  }
  
  p <- df %>% 
    ggplot(aes_string(y = var_num)) + 
    geom_boxplot(fill = "#03579A") +
    labs(title = paste("Boxplot of", var_num)) + 
    theme_bw() + 
    theme(text = element_text(family="Avenir"))
  
  return(p)
  
}

# 14. Like #8 density_plot, with error handling.
# Required packages: ggplot2
bar_plot_2 <- function(df, var_cat){

  # Check if the categorical variable is either character or factor
  if (!(is.character(df[[var_cat]]) | is.factor(df[[var_cat]]))){
    stop('The categorical variable (var_cat) must be a character or factor.')
  } 
  
  p <- df %>% 
    ggplot(aes_string(x = var_cat)) + 
    geom_bar(fill = "#03579A") + 
    labs(title = paste("Barplot of", var_cat)) + 
    theme_bw() + 
    theme(text = element_text(family="Avenir"))
  
  return(p)
  
}

# 17. Function that takes an ID and outputs a graph of the glucose measurement over time.
# Required packages: dplyr, ggplot2
glucose_measurement_id_plot <- function(df, id){
  
  # Check if id is character
  if (!is.character(id)){
    stop('The ID must be a character.')
  } 
  
  p <- df %>% 
    filter(ID == id) %>% 
    ggplot(aes_string(x = "Measurement", 
                      y = "`Glucose (mmol/L)`")) +
    geom_point(color = "#03579A") + 
    geom_line(color = "#03579A") + 
    labs(title = paste("Oral Glucose Tolerance Test of ID:", id)) + 
    theme_bw() +
    theme(text = element_text(family="Avenir")) 
  
  return(p)
  
}

# 19. This function takes a categorical variable and a dataframe as input, calculates the mean glucose content for each category and measurement time, and outputs a line graph showing the glucose measurements over time.
# Required packages: dplyr, ggplot2
glucose_measurement_mean_plot <- function(df, var_cat){

  # Check if the categorical variable is either character or factor
  if (!(is.character(df[[var_cat]]) | is.factor(df[[var_cat]]))){
    stop('The categorical variable (var_cat) must be a character or factor.')
  } 
  
  # Calculate the mean glucose values for each category and measurement time
  glucose_group_mean <- df %>% 
    group_by(Measurement, .data[[var_cat]]) %>%
    summarize(glucose_mean = mean(`Glucose (mmol/L)`), .groups = "drop") %>%
    ungroup()
  
  # Dynamically select colors based on the number of unique categories
  colors <- c("#03579A", "#3986C7", "#F9DA8C", "#404857", "#A41E23")
  n_colors <- df[[var_cat]] %>% unique() %>% length()
  colors <- colors[1:n_colors]
  
  # Create a ggplot object to visualize the mean glucose measurements
  p <- glucose_group_mean %>%
    ggplot(aes_string(x = "Measurement",
                      y = "glucose_mean", 
                      color = var_cat)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = colors) +
    labs(title = paste("Mean Oral Glucose Tolerance Test Across", var_cat), 
         y = "Mean Glucose (mmol/L)") +
    theme_bw() +
    theme(text = element_text(family="Avenir"))

  return(p)
  
}
