library(tidyverse)
library(ggthemes)
library(rstanarm)
library(ggdist)
library(gt)
library(janitor)
library(broom.mixed)
library(gtsummary)

# answering the question- does richer countries have more gender equality views? 

# agreement with inequality statements by country

# 49 countries

agree <- read.csv("shiny_app/datasets/total_agreement_fix.csv") %>% 
  rename(country_name = country_territory)

#142 countries

gdp <- read.csv("shiny_app/datasets/whr_clean.csv") %>% 
  filter(year == "2018") %>% 
  select(country_name, log_gdp_per_capita, score)

agree_gdp <- inner_join(agree, gdp, by = "country_name")

agree_gdp %>% 
  ggplot(aes(x = log_gdp_per_capita, y = percent_agree)) +
  geom_point()

# fitted model 

fit_gdp <- stan_glm(formula = score ~ percent_agree + log_gdp_per_capita,
         data = agree_gdp,
         refresh = 0)

# looking at the regression table

regression_gdp <- tbl_regression(fit_gdp, 
                                 intercept = TRUE, 
                                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>% 
  as_gt() %>% 
  tab_header(title = md("**Linear Regression Model**")) 

regression_gdp


