library(tidyverse)
library(ggthemes)
library(rstanarm)
library(broom.mixed)

wvs <- read_csv("shiny_app/datasets/wvs_clean.csv")

# this table checkes the inequality value answered by individual 

inequality_2 <- wvs %>% 
  select(q28p, q29p, q30p, q32p, q33p, q35p, Gender, country_territory, q48) %>%
  drop_na() %>% 
  rowwise() %>% 
  mutate(Inequality_2 = (q28p + q29p + q30p + q32p + q33p + q35p)/6) %>%
  rename(control_freedom = "q48") %>% 
  
  # average all is the average agreement value for all the questions I chose
  # regarding gender equality. Higher value is more agreement meaning less
  # equality.
  
  select(country_territory, Gender, Inequality_2, control_freedom)

model_individual <- stan_glm(data = inequality_2,
                             formula = control_freedom ~ Inequality_2*Gender,
                             refresh = 0)

# tidy the model, creating a table

tidy_individual <- model_individual %>% 
  tidy() %>% 
  select(term, estimate)

intercept_F <- tidy_individual$estimate[1]
intercept_M <- tidy_individual$estimate[1] + tidy_individual$estimate[3]
slop_F <- tidy_individual$estimate[2]
slop_M <- tidy_individual$estimate[2] + tidy_individual$estimate[4]

inequality_2 %>% 
  ggplot(aes(x = Inequality_2, y = control_freedom, color = Gender)) +
  geom_point() +
  geom_abline(intercept = intercept_F,
              slope = slop_F, 
              color = "#F8766D", 
              size = 1) +
  geom_abline(intercept = intercept_M,
              slope = slop_M,
              color = "#00BFC4", 
              size = 1)
