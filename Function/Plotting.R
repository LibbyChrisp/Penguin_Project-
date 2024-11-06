plot_boxplot <- function(data, x_column, y_column, x_label, y_label, colour_mapping) {
  data <- data %>% drop_na({{y_column}})
  boxplot <- ggplot(data = data, aes(x = !!sym(x_column), y = !!sym(y_column), color = !!sym(x_column))) +
    geom_boxplot(width = 0.5, show.legend = FALSE) +
    geom_jitter(size = 1, alpha = 0.3, show.legend = FALSE, position = position_jitter(width = 0.2, seed = 0)) +
    labs(x = x_label, y = y_label) +
    scale_color_manual(values = species_colours) +
    theme_bw()
  
  
}

species_colours <- c("Adelie" = "pink", "Chinstrap" = "purple",
                      "Gentoo" = "blue")

