library(tidyverse)
library(ggthemes)
library(rstanarm)
library(broom.mixed)

wvs <- read_csv("shiny_app/datasets/wvs_clean.csv")

# matching the question number to the question itself

questions <- tibble(q28p = "When a mother works for pay, the children suffer", 
                    q29p = "On the whole, men make better political leaders than women do", 
                    q30p = "A university education is more important for a boy than for a girl", 
                    q31p = "On the whole, men make better business executives than women do", 
                    q32p = "Being a housewife is just as fulfilling as working for pay", 
                    q33p = "When jobs are scarce, men should have more right to a job than women", 
                    q35p = "If a woman earns more money than her husband, it's almost certain to cause problems",  
                    q46p = "How happy are you? (scale 1 to 4)",
                    q48 = "feeling of control and free choice in life (scale 1 to 10)", 
                    q49 = "life satisfaction (scale 1 to 10 )") %>% 
  pivot_longer(values_to = "Questions",
               names_to = "Numbers",
               cols = everything())  
 

# building a function to generate plots
# the function takes three arguments:
# data = must be wvs
# var = a variable which is a question from wvs
# title = the title taken from question tibble

q_plot <- function(var, title){
  
  tbl <- wvs %>% 
    select(country_territory, Gender, {{var}}) %>%
    drop_na() %>% 
    group_by(Gender, {{var}} , country_territory) %>%
    drop_na() %>% 
    
    # total agreement is the number of people who answered 1\2\3\4 for the
    # question by gender and country
    
    summarise(total_agreement = n(), .groups = "drop") %>%
    # 1 = strongly disagree, 2 = disagree, 3 = agree, 4 = strongly agree. 
    # agreement simplifies the model and summarizes 1/2= Disagree, 3/4 = Agree
    
    mutate(agreement = ifelse({{var}} == 1 | {{var}} == 2, "Disagree", "Agree")) %>% 
    group_by(agreement, Gender, country_territory) %>% 
    
    # This new column counts the number of people who Agree/Disagree 
    
    mutate(agreement_gender_country = sum(total_agreement)) %>% 
    ungroup() %>% 
    group_by(country_territory) %>% 
    
    # Because there are different amount of people who answered this question in
    # each country I calculated the percent agreement/ disagreement by country by
    # gender.
    
    mutate(perc = agreement_gender_country/sum(agreement_gender_country)) %>%  
    ungroup() %>% 
    
    #for ordering the data for visualization I made the column perc_neg with
    #disagreement percent as negative
    
    mutate(perc_neg = ifelse(agreement == "Disagree", perc*(-1), perc)) %>% 
    ggplot(aes(y = fct_reorder(country_territory, perc_neg), x = perc, fill = Gender)) +
    geom_col(position = "dodge") + 
    facet_wrap(~ agreement) +
    labs(title = "Agree or Disagree:", 
         subtitle = paste(title),
         fill = "Gender",
         x = "Percent of People Who Agree/Disagree",
         y = NULL) + 
    scale_fill_manual(values = c("#FF9999", "#56B4E9")) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_dark()
  {
    return(tbl)
    }
}

q_plot(q29p, questions$q29p)


# questions I still need to examine:

Q <- c("q46p" = "How happy are you? (scale 1 to 4)",
       "q48" = "feeling of control and free choice in life (scale 1 to 10)", 
       "q49" = "life satisfaction (scale 1 to 10 )")


# calculation the average score for the gender equality questions by country and
# gender to build a linear regression model.

equality <- wvs %>% 
  select(q28p, q29p, q30p, q32p, q33p, q35p, Gender, country_territory) %>%
  drop_na() %>% 
  group_by(country_territory, Gender) %>% 
  summarise(avg_28 = mean(q28p),
            avg_29 = mean(q29p),
            avg_30 = mean(q30p),
            avg_32 = mean(q32p),
            avg_35 = mean(q35p),
            avg_33 = mean(q33p),
            .groups = "drop") %>% 
  rowwise() %>% 
  mutate(Inequality = (avg_28 + avg_29 + avg_30 + avg_32 + avg_33 + avg_35)/6) %>%
  
  # average all is the average agreement value for all the questions I chose
  # regarding gender equality. Higher value is more agreement meaning less
  # equality.
  
  select(country_territory, Gender, Inequality)

control_satisfaction <- wvs %>% 
  select(country_territory, Gender, q48, q49) %>%
  drop_na() %>% 
  group_by(country_territory, Gender) %>% 
  
  # life satisfaction and control is from 1 to 10 scale
  
  summarise(Control = mean(q48),
            Satisfaction = mean(q49),
            .groups = "drop")

obj_1 <- equality %>% 
  inner_join(control_satisfaction, by = c("country_territory", "Gender"))

model_1 <- stan_glm(data = obj_1,
                    formula = Satisfaction ~ Gender + Control + Inequality,
                    refresh = 0)

print(model_1)

model_2 <- stan_glm(data = obj_1,
                    formula = Satisfaction ~ Gender + Inequality,
                    refresh = 0) 
print(model_2)

loo_1 <- loo(model_1)

loo_2 <- loo(model_2)

# model 1 is better than model 2! yay!

loo_compare(loo_1, loo_2)

tidy <- model_1 %>% 
  tidy() %>% 
  select(term, estimate)

# for model 1:
# satisfaction_i = beta0 + beta1Gender + beta2Control + beta3Inequality + epsilon_i
# for male the intercept is beta0 + beta1

intercept_female <- tidy$estimate[1]
intercept_male <- tidy$estimate[2] + intercept_female
coef_control <- tidy$estimate[3]
coef_inequality <- tidy$estimate[4]

# this model examines how inequality and gender effects control to make lofe
# choices.

model_3 <- stan_glm(data = obj_1,
                               formula = Control ~ Gender*Inequality,
                               refresh = 0) 
tidy_3 <- model_3 %>% 
  tidy() %>% 
  select(term, estimate)

control_inter_female <- tidy_3$estimate[1]
control_inter_male <- tidy_3$estimate[2] + tidy_3$estimate[1]
control_slop_female <- tidy_3$estimate[3]
control_slop_male <- tidy_3$estimate[4] + tidy_3$estimate[3]

# plotting model 3 for male and female: how the Inequality value- higher average
# score on questions regarding gender equality affects the feeling of control
# and free choice in life.

obj_1 %>% 
  ggplot(aes(x = Inequality, y = Control, color = Gender)) +
  geom_point() +
  geom_abline(intercept = control_inter_female,
              slope = control_slop_female, 
              color = "#F8766D", 
              size = 1) +
  geom_abline(intercept = control_inter_male,
              slope = control_slop_male,
              color = "#00BFC4", 
              size = 1) +
  labs(title = "Linear regression",
       subtitle = "The relationship between freedom to make life choices\nand gender inequality",
       x = "Gender Inequality",
       y = "Freedom to make life choices") +
  theme_fivethirtyeight()
  
