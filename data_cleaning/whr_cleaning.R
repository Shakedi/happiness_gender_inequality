library(tidyverse)
library(janitor)
library(readxl)

# world happiness report data

whr <- read_xls("raw_data/DataPanelWHR2021C2.xls")

whr_clean <- whr %>% 
  clean_names() %>%
  
  # score is the average happiness score from 1 to 10 by country 
  
  rename(score = "life_ladder") %>% 
  write_csv("shiny_app/datasets/whr_clean.csv")



countries <- whr %>% 
  filter(year == 2018) %>% 
  select(country_name) %>% 
  write_csv("shiny_app/datasets/whr_countries.csv")
