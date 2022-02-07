### Shiny App : Input and UI

# create a directory for the Shiny application
#dir.create(here::here("app.R"))

library(shiny)
library(tidyverse)

# import data for project
breed_traits_raw      <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
breed_rank_all_raw    <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

### Clean and Wrangle 

# dogs rank clean
dogs_rank_long <- 
  breed_rank_all_raw %>%
  pivot_longer(cols = c(`2013 Rank`:`2020 Rank`), names_to = "year", values_to = "rank") %>%
  mutate(year = as.numeric(str_remove(year, " Rank"))) %>%
  select(Breed, year, rank, everything()) %>%
  janitor::clean_names() %>%
  mutate(breed = str_squish(breed))

#### UI

# Define UI for Good Boys app
ui <- 
  
  fluidPage(
    
    # Sidebar panel for inputs
    sidebarLayout(
      
      # Input: Selector for variable (breed) to plot 
      sidebarPanel(width = 1,
                   selectInput("breed", "Breed:", choices = dogs_rank_long %>% arrange(breed) %>% distinct(breed)),
                   tags$style(".well {background-color:white; border: none; box-shadow: none; width: 300px}")),
      
      # Main panel for displaying outputs
      mainPanel(width = 11, plotOutput("top_dogs", height = "100%"))
      
    )
    
  )