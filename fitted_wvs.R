library(tidyverse)
library(ggthemes)
library(rstanarm)
library(ggdist)
library(gt)
library(janitor)
library(broom.mixed)
library(gtsummary)

wvs <- read_csv("shiny_app/datasets/wvs_clean.csv")

#looking at agreement percent in questions: q28p, q29p, q30p, q32p, q33p, q35p

agreement_perc <- function(var){
  
  tbl <- wvs %>% 
    select(country_territory, Gender, {{var}}) %>%
    drop_na() %>% 
    group_by(Gender, {{var}} , country_territory) %>%
    drop_na() %>% 
    
    # total agreement is the number of people who answered 1\2\3\4 for
    # the question by gender and country
    
    summarise(total_agreement = n(), .groups = "drop") %>%
    
    # 1 = strongly disagree, 2 = disagree, 3 = agree, 4 = strongly
    # agree. agreement simplifies the model and summarizes 1/2=
    # Disagree, 3/4 = Agree
    
    mutate(agreement = ifelse({{var}} == 1 | {{var}} == 2,
                              "Disagree", "Agree")) %>% 
    group_by(agreement, Gender, country_territory) %>% 
    
    # This new column counts the number of people who Agree/Disagree 
    
    mutate(agreement_gender_country = sum(total_agreement)) %>% 
    ungroup() %>% 
    group_by(country_territory) %>% 
    
    # Because there are different amount of people who answered this
    # question in each country I calculated the percent agreement/
    # disagreement by country by gender.
    
    mutate(perc = agreement_gender_country/sum(agreement_gender_country)) %>% 
    ungroup() %>% 
    filter(agreement == "Agree") %>% 
    select(Gender, country_territory, perc)
  
  {return(tbl)}
}

q28 <- agreement_perc(q28p) %>% 
  rename(perc_28 = perc)
q29 <- agreement_perc(q29p)%>% 
  rename(perc_29 = perc)
q30 <- agreement_perc(q30p)%>% 
  rename(perc_30 = perc)
q32 <- agreement_perc(q32p)%>% 
  rename(perc_32 = perc)
q33 <- agreement_perc(q33p)%>% 
  rename(perc_33 = perc)
q35 <- agreement_perc(q35p)%>% 
  rename(perc_35 = perc)

x <- full_join(q28, q29) %>%  
  full_join(q30) %>% 
  full_join(q32) %>% 
  full_join(q33) %>% 
  full_join(q35) %>% 
  rowwise() %>% 
  mutate(perc_agree = (perc_28 + perc_29 + perc_30 + perc_32 + perc_33 + perc_35)/6) %>% 
  select(Gender, country_territory, perc_agree) %>% 
  distinct()

control <- wvs %>% 
  select(country_territory, Gender, q48) %>%
  drop_na() %>% 
  group_by(country_territory, Gender) %>% 
  
  # life satisfaction and control is from 1 to 10 scale
  
  summarise(Control = mean(q48),
            .groups = "drop")

# final dataset that I use for my fit model. The agreement percent is the
# percent of people who agreed with the statements, meaning higher percent
# agreement higher inequality.

y <- inner_join(control, x, by = c("Gender", "country_territory")) %>% 
  write_csv("shiny_app/datasets/agreement_wvs.csv")

fit_y_2 <- stan_glm(data = y,
                  formula = Control ~ perc_agree*Gender + perc_agree + Gender,
                  refresh = 0)

# plotting tidy_y_2 model 

tidy_y_2 <- fit_y_2 %>% 
  tidy() %>% 
  select(term, estimate) %>% 
  write_csv("shiny_app/datasets/fitted_wvs.csv")

# table explaining the variable I used. The perc_agree has a wide distribution
# which indicates that the relation plotted might not be accurate, meaning the
# slop is not percise. However we see that it is less likely that the slope will
# be positive and we can assume that more agreement is related to less control. 

regression_wvs <- tbl_regression(fit_y_2, 
               intercept = TRUE, 
               estimate_fun = function(x) style_sigfig(x, digits = 3)) %>% 
  as_gt() %>% 
  tab_header(title = md("**Linear Regression Model**")) 
  
regression_wvs %>% 
  gtsave("regression_wvs.html", inline_css = TRUE)
  
  # not sure how to change beta to parameter
  
  tab_source_note("Source: World Value Survey (Wave 7)")

Intercept <- tidy_y_2$estimate[1]
Perc_agree <- tidy_y_2$estimate[2]
Sex_male <- tidy_y_2$estimate[3]
Interaction_term <- tidy_y_2$estimate[4]

Female_intercept <- Intercept
Female_slope <- Perc_agree
Male_intercept <- Intercept + Sex_male
Male_slope <- Perc_agree + Interaction_term

ggplot(y, aes(x = perc_agree, y = Control, color = Gender)) +
  
  # Use geom_point to show the datapoints. 
  
  geom_point() +
  
  # Create a geom_abline object for the female intercept and slope. Set the
  # intercept qual to our previously created female_intercept, while setting
  # slope equal to our previously created female_slope. The color call is for
  # coral, to match the colors used by tidyverse for geom_point().
  
  geom_abline(intercept = Female_intercept,
              slope = Female_slope, 
              color = "#F8766D", 
              size = 1) +
  geom_abline(intercept = Male_intercept,
              slope = Male_slope,
              color = "#00BFC4",
              size = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     n.breaks = 20) +
  labs(title = "Linear Regression:\nThe Relationship Between Agreement and Freedom",
       subtitle = "Percieved freedom decreases as agreement increases",
       x = "Percent of people who agreed",
       y = "Average Control and Freedom",
       caption = "Source: World Value Survey (Wave 7)") +
  theme_clean() +
  theme(legend.position="bottom") 

# each dot is the average agreement in a country

