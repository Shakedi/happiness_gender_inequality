library(tidyverse)
library(janitor)
library(readxl)

# saving the whole dataset of the world value survey wave 7 invert scale as an
# object

wvs <- read_rds("raw_data/World_Values_Survey_Wave_7_Inverted_R_v1_6") %>% 
  clean_names()

# variable code from wvs 

wvs_var <- read_xlsx("raw_data/WVS-7_Variables_Report_Annex copy.xlsx") %>% 
  clean_names() %>% 
  
  # I checked the name of this variable in the explanatory appendix pdf, I want
  # to match country code with the name
  
  rename("b_country" = "iso_3166_1_numeric_code")

# matching country code with name:

wvs_countries <- wvs_var %>%
  select(country_territory, b_country)

# combining the countries the data sets

wvs_clean <- wvs %>% 
  inner_join(wvs_countries, by = "b_country") %>%  

# from this dataset I am examining the following questions:
# agree or disagree:
# Q28 - When a mother works for pay, the children suffer
# Q29 - On the whole, men make better political leaders than women do
# Q30 - A university education is more important for a boy than for a girl
# Q31 - On the whole, men make better business executives than women do
# Q32 - Being a housewife is just as fulfilling as working for pay
# Q33 - When jobs are scarce, men should have more right to a job than women
# Q35 - If a woman earns more money than her husband, it's almost certain to
# cause problems
# Q46 - How happy are you? (scale 1 to 4)
# Q48 - feeling of control and free choice in life (scale 1 to 10)
# Q49 - life satisfaction (scale 1 to 10 )
# Q260 - Gender: 1 = Male, 2 = Female

select(country_territory, q260, q28p, q29p, q30p, q31p, q32p, q33p, q35p, q46p,
       q48, q49) %>%
  
  # creating a new gender column
  
  mutate(Gender = ifelse(q260 == "1", "Male", "Female")) %>% 
  write_csv("shiny_app/datasets/wvs_clean.csv")

