# Exercise 4B - Solutions: Scripting in R - Functions

make_boxplot <- function(df, plot_column){
  
  if (!is.numeric(df[[plot_column]])){
    stop('The column to plot must be numcerial.')
  }
  
  p <- ggplot(df, aes(y = !!sym(plot_column))) +
    geom_boxplot(fill = "#03579A") +
    labs(title = paste("Boxplot of", plot_column)) + 
    theme_bw()
  
  return(p)
  
}
