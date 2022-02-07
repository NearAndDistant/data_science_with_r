#### Shiny App : Server

library(shiny)
library(tidyverse)
library(cowplot)
library(showtext); showtext_auto()

font_add_google("Loved by the King", "king")
font_add_google("Amatic SC", "amatic")
font_add_google("Just Me Again Down Here", "me_again")

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

# dog traits clean
dogs_trait_long <-
  breed_traits_raw %>%
  select(-`Coat Type`, -`Coat Length`) %>%
  pivot_longer(cols = c(`Affectionate With Family` : `Mental Stimulation Needs`), names_to = "attribute", values_to = "value") %>%
  janitor::clean_names() %>%
  mutate(breed = str_squish(breed))

# transform
top_dogs <-
  dogs_rank_long %>%
  left_join(dogs_trait_long) %>%
  filter(year == 2020) %>%
  mutate(breed = as_factor(breed)) %>%
  group_by(attribute) %>%
  mutate(attribute = str_remove(attribute, " Level"),
         attribute = case_when(attribute == "Affectionate With Family"   ~ "Affectionate",
                               attribute == "Good With Young Children"   ~ "Child-Friendly",
                               attribute == "Good With Other Dogs"       ~ "Combativeness",
                               attribute == "Openness To Strangers"      ~ "Openness",
                               attribute == "Watchdog/Protective Nature" ~ "Protective",
                               attribute == "Coat Grooming Frequency"    ~ "Grooming",
                               attribute == "Mental Stimulation Needs"   ~ "Stimulation",
                               TRUE ~ attribute)) %>%
  mutate(attribute = factor(attribute)) %>%
  ungroup() %>%
  group_by(breed) %>%
  arrange(desc(value)) %>%
  mutate(id = row_number()) %>%
  ungroup() %>% #2 Pissaro #1 Signac
  mutate(fill = case_when(attribute == "Affectionate"   ~  "#fbe183",
                          attribute == "Child-Friendly" ~  "#2b9b81",
                          attribute == "Combativeness"  ~  "#d8443c",
                          attribute == "Openness"       ~  "#e6a2a6",
                          attribute == "Playfulness"    ~  "#9f5691",
                          attribute == "Adaptability"   ~  "#f4c40f",
                          attribute == "Trainability"   ~  "#aa7aa1",
                          attribute == "Energy"         ~  "#fe9b00",
                          attribute == "Protective"     ~  "#e87b89",
                          attribute == "Stimulation"    ~  "#de597c",
                          attribute == "Barking"        ~  "#9b3441",
                          attribute == "Grooming"       ~  "#92c051",
                          attribute == "Shedding"       ~  "#633372",
                          attribute == "Drooling"       ~  "#1f6e9c"))

# Define server logic to plot various breeds

## Define server logic to plot 
server <- function(input, output) {
    
          filtered_input <- reactive({top_dogs %>% filter(breed == input$breed)})
          
          # Generate a plot of the requested breed
          output$top_dogs <- 
            renderPlot({
              ggdraw(
                filtered_input() %>%
                  ggplot() +
                  geom_segment(data = data.frame(y=seq(0,5,1)), aes(x = -0.5, xend = 15, y=y, yend=y), linetype = "ff", color = "grey90") +
                  geom_text(data = data.frame(y=seq(0,5,1)), aes(x = -0.15 , y = y + 0.5, label = y), family = "king", size = 6, fontface = "bold") +
                  geom_col(aes(id, value, fill = fill), show.legend = FALSE) +
                  ggimage::geom_image(aes(x = -0.5, y = -5.5, image = image), size = 0.24) +
                  geom_text(aes(x = id, y = 7, label = attribute), size = 12, fontface = 'bold', family = "king") +
                  geom_text(aes(label = str_wrap(breed,20)), x = -0.5, y = -1.7, size = 8, fontface = 'bold', family = "amatic") +
                  scale_fill_identity() +
                  scale_y_continuous(limits = c(-5.5, 7), breaks = seq(0,5,1)) +
                  scale_x_continuous(limits = c(-0.5, max(top_dogs$id)+1)) +
                  coord_polar(clip = "off") +
                  theme_void() +
                  theme(plot.margin = margin(1.5,0,0,0, unit = "cm"))
              ) +
                draw_text("Dogs of the Tidyverse",                x = 0.5 , y = 0.95, size = 50, family = "amatic", fontface = "bold") + 
                draw_text("Each a Good Boy, Each a Character",    x = 0.5 , y = 0.90, size = 30, family = "amatic") +
                draw_text("Traits: Strong (5) to Weak (0)",       x = 0.5 , y = 0.86, size = 25, family = "amatic") +
                draw_text("Data: AKC | Graphic: @NearAndDistant", x = 0.41 , y = 0.35, size = 16, family = "me_again", hjust = 0, color = "grey70")
            }, height = 800, width = 800)
          }