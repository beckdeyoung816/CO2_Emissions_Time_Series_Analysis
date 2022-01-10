#-------------------------------------------------------------
#                    Visualization_Helpers.R
#-------------------------------------------------------------

# This script contains helper functions for Visualization.R

png_save <- function(plt, name, h = 800, w = 800, show = F, corr = F){
  # Save a plot as a png
  png(height=h, width=w, file= paste0("Figures/R_",name,".png"))
  plt
  invisible(dev.off())
  
  if (show) plot(plt)
}

ts_plot <- function(var, varName){
  # Make a time series plot
  # @var: variable to plot
  # @varName: Tidy name to label plot
  full %>% ggplot(aes(x = Year, color = `Country Name`)) + 
    geom_line(aes_string(y = var)) + 
    theme(axis.title.x= element_text(size = 16),
          axis.title.y= element_text(size = 16),
          axis.text.x = element_text(size = 12),
          axis.text.y= element_text(size = 12)) +
    labs(y = varName)
  
}

arrange_ts_plot <- function(ts_plots){
  # Make a nice grid image of multiple time series plots
  # @ts_plots: List of plots 
  grid.arrange(ts_plots[[1]], ts_plots[[2]], 
               ts_plots[[3]], ts_plots[[4]], 
               ts_plots[[5]], ts_plots[[6]])
}