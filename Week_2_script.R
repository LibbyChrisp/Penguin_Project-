
#Script 

library(tidyverse)
library(palmerpenguins)
library(here)
library(janitor)

# This means we can use our own functions from cleaning.R file. We used this last week and I have saved the clean data. 

source(here("Function", "cleaning.R"))

#Make sure the clean data has been named 

Penguins_clean <- read_csv(here("data", "penguin_clean.csv"))

#Create the boxplot 

flipper_boxplot <- ggplot(data = penguins_clean, aes(x=species, y=flipper_length_mm)) + geom_boxplot()

flipper_boxplot

#Errors: Removed 2 rows containing non-finite outside the scale range (`stat_boxplot()`). 
# There is missing data 
# You should only remove missing values from collumns interested in (flippers and species)

# Select the collumns: species and flipper and remove NAs

penguins_flippers <- penguins_clean %>% select(species, flipper_length_mm) %>%  drop_na

head(penguins_flippers)

#Now try the boxplot again. 

species_colours <- c("Adelie" = "pink", "Chinstrap" = "purple",
                     "Gentoo" = "blue")

Flipper_boxplot <- ggplot(data = penguins_flippers, aes(x=species, y=flipper_length_mm)) + 
  geom_boxplot(aes(color = species), width = 0.5, show.legend = FALSE) + 
  geom_jitter(aes(color = species), alpha = 0.3, show.legend = FALSE, position = position_jitter(width =0.2, seed = 0)) +
  labs(x = "Penguin Species", y = "Flipper length (mm)") +
  scale_color_manual(values= species_colours) +
  theme_bw()
Flipper_boxplot


#ADDING DIFFERENT COMPONENTS TO THE BOX PLOT
#But this is a very ugly figure 
# Change the color -> (aes(color = species))
# Remove the legend -> show.legend = FALSE)
# Add raw data on top -> geom_jitter() -> use jitter as gives a scatter rather than points being in a line 
# Add color to jitter, remove legend and reduce widthof the jitter. 

# But due to jitter being random, this data is not reproducible. We need to use a random seed so the computer gives each dot the same X value each time. 
# Change the width of boxplot 
# Change the transparacy with alpha 
# Add labels to the axis 
# Changing default colours 

#Creating a function for this plot 
# This means that I can make similar plots with different data. -> Stops us having to copy and paste code. 
# Go to files and click on the function file. Create script and call it Plotting.R.
# Paste my function into this file. 

#Creating the function to look at flipper length for different species. 

plot_boxplot <- function(data, x_column, y_column, x_lable, y_lable, colour_mapping) {
  data <- data %>% drop_na({{y_column}})
  
  
  
  boxplot <- ggplot(data = data, aes(x={{x_column}}, y={{y_column}})), color = {{x_column}} + 
    geom_boxplot(width = 0.5, show.legend = FALSE)) + 
  geom_jitter(size = 1, alpha = 0.3, show.legend = FALSE, position = position_jitter(width =0.2, seed = 0)) +
  labs(x = x_label, y = y_label) +
  scale_color_manual(values= species_colours) +
  theme_bw()
}

#The above code didn't run so I put it into chat gpt and it said this. It claimed that I couldn't use curly brackets inside the aes function. 

plot_boxplot <- function(data, x_column, y_column, x_lable, y_lable, colour_mapping) {
  data <- data %>% drop_na({{y_column}})
  Flipper_boxplot <- ggplot(data = data, aes(x = !!sym(x_column), y = !!sym(y_column), color = !!sym(x_column))) +
    geom_boxplot(width = 0.5, show.legend = FALSE) +
    geom_jitter(size = 1, alpha = 0.3, show.legend = FALSE, position = position_jitter(width = 0.2, seed = 0)) +
    labs(x = x_label, y = y_label) +
    scale_color_manual(values = species_colours) +
    theme_bw()
}


species_colours <- c("Adelie" = "pink", "Chinstrap" = "purple",
                     "Gentoo" = "blue")



#I put this code into the Plotting.R file to create a function that can be used in any script within this project. 
#I can call this function by using 

#I retrieve this code using the function below

source(here("Function", "Plotting.R"))

#I can now use this function to make plots

boxplot <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm") 

  
print(boxplot)


#This function can be used to make many sub plots. 

# 2 by 2 grid subplots

options(
  repr.plot.width = 8,   # Width of the plot in inches
  repr.plot.height = 8,  # Height of the plot in inches
  repr.plot.res = 96     # Resolution of the plot in dots per inch (DPI), which affects plot clarity
)

library(patchwork)

# Create individual subplots with different y-axis colum

plot1 <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm")
plot2 <- plot_boxplot(penguins_clean, "species", "culmen_length_mm", "Penguin Species", "Culmen Lenghth mm")
plot3 <- plot_boxplot(penguins_clean, "species", "culmen_depth_mm", "Penguin Species", "Culmen Depth mm")
plot4 <- plot_boxplot(penguins_clean, "species", "body_mass_g","Penguin Species", "Body Mass g")
print(plot1)
# Combine plots in a 2x2 grid

combined_plot <- (plot1 | plot2) / (plot3 | plot4) 
print(combined_plot)

#I need another couple of libraries -> add to top of script 

library(ragg)
library(svglite)

#Changing the scale and saving this into my figures file. 

boxplot <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm") 

agg_png("figures/penguin_boxplot_default.png",
        width = 20, height = 20, units = "cm", res = 50, scaling = 1.5)

print(boxplot)
dev.off()

#Reducing the resolution 

boxplot <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm") 

agg_png("figures/penguin_boxplot_lowres.png",
        width = 20, height = 20, units = "cm", res = 50, scaling = 1.5)

print(boxplot)
dev.off()
        
#Icrease the size for a poster

boxplot <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm") 

agg_png("figures/penguin_boxplot_poster.png",
        width = 40, height = 40, units = "cm", res = 50, scaling = 1.5)

print(boxplot)
dev.off()


#We may want a powerpoint figure -> change the scaling 

boxplot <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm") 

agg_png("figures/penguin_boxplot_PP.png",
        width = 20, height = 20, units = "cm", res = 300, scaling = 3)

print(boxplot)
dev.off()

#Should try to save as an svg as has better resolution 

boxplot <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm") 
inches_conversion = 2.54
svglite("figures/penguin_boxplot_PP.svg",
        width = 20/ inches_conversion, height = 20/ inches_conversion)

print(boxplot)
dev.off()

#PNG function - made the function in new R script

source(here("Function", "SaveFigures.R"))

#Example of using this function to resave a plot 

#We may want a powerpoint figure -> change the scaling 

boxplot <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm") 

save_flipper_plot_png(boxplot, here("Figures", "penguin_boxplot_PPP"), size = 20, res = 300, scaling = 2)

#SVG Function 

source(here("Function", "SaveFiguressvg.R"))

boxplot <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm") 

#Example of using this function to resave an already saved plot 

#Increase the size for a poster

boxplot <- plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Lenghth mm") 

save_flipper_plot_svg(boxplot,
                      here("Figures", "flipper_boxplot_Poster.svg"), size =20, scaling = 2)



