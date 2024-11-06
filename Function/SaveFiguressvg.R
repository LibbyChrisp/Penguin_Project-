save_flipper_plot_svg <- function(boxplot,
                                  filename, size, scaling){
                                  size_inches = size/2.54 
                                  svglite(filename, width = size_inches,height = size_inches, scaling = scaling)
                                  print(boxplot)
                                  dev.off() 
                                  }


