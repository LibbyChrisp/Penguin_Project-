#Good practices in R 
#First create penguin project 
#Then create a new folder for the data 

#INstall libraries 

library(tidyverse)
library(palmerpenguins)
library(here)
library(janitor)

#When we run R code we want to read and write to the files already loaded in our project (loaded as I downloaded a package containing the data). We use 'here' to load that data already within the project. 

#Loading the data 
here()

#I want to keep my raw data safe so that when I change it I can always go back to the origonal if I make a mistake 
#This is saying: Create a csv to put penguins_raw into the file Data within my working direcotry (here) and call it "penguins_raw.csv 
write.csv(penguins_raw, here("Data", "penguins_raw.csv"))

#Checking the column names 
#These are very messy and I want to create a function to clean these collumn names 

colnames(penguins_raw)

#Removing certain collumns. 

#Method 1: BAD 
#This is saying: within the penguins raw data, remove the collumn called comments
# This is bad as I am not changing the name of the new data set 
#Do not run this code 

penguins_raw <- select(penguins_raw, -Comments)
penguins_raw <- select(penguins_raw, -start_with("Delta"))

#Adding a - before the collumn will remove it 

#Method 2: Using a pipeline 
#A pipeline adds commands together 

penguins_clean <- penguins_raw %>% select(-Comments) %>% select(-starts_with("Delta")) %>% clean_names()

colnames(penguins_clean)

#Save this new data set 

write.csv(penguins_clean, here("Data", "penguins_clean.csv"))

#Creating a function to clean the data 
#This pipeline can be turned into a function so that I can easily clean my data and also clean other data

cleaning_penguin_columns <- function(raw_data){
  print("remove empty collumns and rows, cleaned column names, removed comments, and delta columns")
  raw_data %>%
    clean_names()%>%
    remove_empty(c("rows", "cols"))%>%
    select(-comments)%>%
    select(-starts_with("delta"))
  }

#I now need to load the raw data and apply the function to it. I must re load the data before I clean it. 

penguins_raw <- read.csv(here("Data","penguins_raw.csv"))

#Applying the function that I have created 
penguins_clean <- cleaning_penguin_columns(penguins_raw)

#View the clean data 

colnames(penguins_clean)

#I am now going to save a function that someone else has made and put it into a Function folder. This is called cleaning.R and it contains tools to clean the data 
#I load this function into my script using source 

source(here("Function","cleaning.R"))

#

#I know want to create a new function using this function to clean the data 
#The cleaning.R file lets us use short hand in our function 

cleaning_penguin_columns <- function(raw_data){
  print("remove empty collumns and rows, cleaned column names, removed comments, and delta columns, shorten species name")
  raw_data %>%
    clean_names()%>%
    shorten_species() %>% 
    remove_empty(c("rows", "cols"))%>%
    select(-comments)%>%
    select(-starts_with("delta"))
}

#I know want to apply this function (contains extra commands) 

penguins_clean <- cleaning_penguin_columns(penguins_raw)

colnames(penguins_clean)

#We can also subset the data using the select() function 

penguins_clean_subset <- penguins_clean %>% 
  select(species, body_mass_g)

print(penguins_clean_subset)

#Removing missing data values 
#na.omit() can be used to remove mising data values 

penguins_clean_subset <- penguins_clean %>% 
  select(species, body_mass_g) %>% 
  na.omit()
print(penguins_clean_subset)

#Filter by species 
#Can use the filter() function to filter by species. 

penguins_clean_subset <- penguins_clean %>% 
  filter(species == "Adelie") %>%  # 
  select(body_mass_g) %>%          
  na.omit() 
print(penguins_clean_subset)

#Installing libraries the reproducible way 
# Using renv can mean that when someone runs this code they automatically upload the libraries 

#Example: load file and I know want to create a snapshot of all the libraries that have been used.
library(boot)

#Creating a renv snapshot. This will create a file called rev.lock that contains all the libraries that I have used.
#All the renv code should be in the console. 

renv::snapshot()

#When I press this file it will show me all the libraries that I have used. 

#Checking which packages I used. 

renv::diagnostics()

#If it says out of sink, do renv :: snapshot again 






