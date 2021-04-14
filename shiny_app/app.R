

library(shiny)
library(tidyverse)
library(shinythemes)
library(ggthemes)
library(ggdist)
library(gt)
library(broom.mixed)
library(gtsummary)


# Those are the datasets that I will use in this project, after I cleaned them
# in another document. For example I had to combine country code to country name
# as well as changing names of columns to be more meaningful and simple to
# understand.

# World Value Survey

wvs <- read_csv("datasets/wvs_clean.csv",
                col_types = cols(
                    country_territory = col_character(),
                    q260 = col_double(),
                    q28p = col_double(),
                    q29p = col_double(),
                    q30p = col_double(),
                    q31p = col_double(),
                    q32p = col_double(),
                    q33p = col_double(),
                    q35p = col_double(),
                    q46p = col_double(),
                    q48 = col_double(),
                    q49 = col_double(),
                    Gender = col_character()
                ))

# World Happiness Report

whr <- read_csv("datasets/whr_clean.csv",
                col_types = cols(
                    country_name = col_character(),
                    year = col_double(),
                    score = col_double(),
                    log_gdp_per_capita = col_double(),
                    social_support = col_double(),
                    healthy_life_expectancy_at_birth = col_double(),
                    freedom_to_make_life_choices = col_double(),
                    generosity = col_double(),
                    perceptions_of_corruption = col_double(),
                    positive_affect = col_double(),
                    negative_affect = col_double()))

countries <- whr %>% 
    filter(year == 2018) %>% 
    select(country_name) %>% 
    pull()

# a tibble with the questions from WVS

questions<- tibble("q28p" =
                        "When a mother works for pay, the children suffer.", 
                    "q29p" = 
                        "On the whole, men make better political leaders than women do.", 
                    "q30p" = 
                        "A university education is more important for a boy than for a girl.", 
                    "q31p" = 
                        "On the whole, men make better business executives than women do.", 
                    "q32p" = 
                        "Being a housewife is just as fulfilling as working for pay.", 
                    "q33p" = 
                        "When jobs are scarce, men should have more right to a job than women.", 
                    "q35p" = 
                        "If a woman earns more money than her husband, it's almost certain to cause problems.") %>%
    pivot_longer(values_to = "Questions",
                 names_to = "Numbers",
                 cols = everything())
# data for fitted model wvs

agreement <- read_csv("datasets/agreement_wvs.csv")

# fitted model of world value survey

fitted_wvs <- read_csv("datasets/fitted_wvs.csv")

# the variables for the regression plot:

Female_intercept <- fitted_wvs$estimate[1]
Female_slope <- fitted_wvs$estimate[2]
Male_intercept <- fitted_wvs$estimate[1] + fitted_wvs$estimate[3]
Male_slope <- fitted_wvs$estimate[2] + fitted_wvs$estimate[4]


# Define UI for application 

ui <- fluidPage(theme = shinytheme("sandstone"),

    navbarPage("Freedom and Gender Equality",
               
               # panel 1
               # examines questions asked in the world value survey
               
               tabPanel("World Value Survey",
                        h2(strong("World Value Survey (Wave 7: 2017- 2020)")),
                        br(),
                        br(),
                        br(),
                        sidebarPanel(h3("About the data"),
                        h5(strong("Questions from the WVS examining Gender
                           equality views:")),
                        p("In the following questions participants were asked to
                          rate their level of agreement with each statement. 
                          The statements focuse on",
                          strong("gender equality and women rights")),
                        p("The scale as following:"),
                        p("1- strongly disagree"),
                        p("2- disagree"),
                        p("3 - agree"),
                        p("4 - strongly agree"),
                        p("To compare level of agreement across countries and
                          gender I combined responds of strongly agree and agree
                          to one caterory as well as strongly disagree and disagree.
                          Then I calculated the percent of people who agreed by 
                          country and gender.")),
                        br(),
                        
                        # Main Panel
                        mainPanel(
                            selectInput(inputId = "selected_question",
                                       label = "Choose a statement:",
                                       choices = questions$Questions,
                                       selected = "When a mother works for pay,
                                       the children suffer."),
                            plotOutput("question_plot"),
                            br(),
                            br(),
                            br(),
                            br()
                        ),
                        
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        h3(strong("Linear Regression Model - 
                                  Relationship between Gender Inequality and Freedom")),
                        br(),
                        p("In the following model I examine the relationship
                          between precentage of agreement with the statements
                          above and perceived feelings of control and freedom
                          to make life choices. Participants in the World Value 
                          Survey were asked to rate", strong("feelings of
                          control and free choice in life from 1 to 10."),
                          "I calculated the average response to this question by
                          gender and country and used the results in the
                          regression model. Each point on the graph represents 
                          the average result of a country by gender.", 
                          strong("People in countries where the percent of agreement 
                                 with the statements is higher tend feel less control
                                 and freedom to make life choices."), "This trend
                          is observed in both genders."),
                        p("The Equation for the Regression Model:"),
                        withMathJax('$$ freedom_i = \\beta_0 + \\beta_1agreement_i + 
                        \\beta_2Male_i + \\beta_3agreement_i*Male_1 +
                           \\epsilon_i $$'),
                        p("In the regression table you can find the value of the
                          coefficients under the Beta column as well as the 95%
                          confidence interval. Although the data is spread, as 
                          expected given the number of datapoints, by looking at 
                          the 95% interval for the agreement coefficient we can 
                          say that it is far more likely that higher agreement 
                          percentage is negatively correlated with higher
                          perceived freedom than the opposite."),
                        br(),
                        br(),
                        splitLayout(cellWidths = c("40%", "60%"),
                        htmlOutput("regression_gt"),
                        plotOutput("regression_wvs")),
                        ),
               
               # panel 2
               
               tabPanel("World Happiness Report",
                        h3("World Happiness Report Data"),
                        p("The happiness score is the average value for:
                          how would you rate your happiness from 1 to 10?"),
                        br(),
                        mainPanel(
                            selectInput(inputId = "selected_country",
                                        label = "Choose Countries:",
                                        choices = countries,
                                        selected = c("Sweden", "Egypt",
                                                     "United States"),
                                        multiple = TRUE),
                            plotOutput("happiness_plot")
                        )
                        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    wvs <- read_csv("datasets/wvs_clean.csv")

   # The following function generates a plot according to the selected question
   # from WVS. The function take the following arguments: var = a variable which
   # is a question from wvs title = the title taken from question tibble -
   # deleted this
    
    q_plot <- function(var){
        
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
            
            #for ordering the data for visualization I made the column perc_neg
            #with disagreement percent as negative
            
            mutate(perc_neg = ifelse(agreement == "Disagree", perc*(-1), perc)) %>% 
            ggplot(aes(y = fct_reorder(country_territory, perc_neg), x = perc,
                       fill = Gender)) +
            geom_col(position = position_dodge(width = 0.7)) + 
            facet_wrap(~ agreement) +
            labs(title = "Agree or Disagree:", 
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
    
    
   output$question_plot <- renderPlot({
       if(input$selected_question == 
          "When a mother works for pay, the children suffer."){
           q_plot(q28p)
       }
       else if(input$selected_question == 
               "On the whole, men make better political leaders than women do."){
           q_plot(q29p)
       }
       else if(input$selected_question == 
               "A university education is more important for a boy than for a girl."){
           q_plot(q30p)
       }
       else if(input$selected_question == 
               "On the whole, men make better business executives than women do."){
           q_plot(q31p)
       }
       else if(input$selected_question == 
               "Being a housewife is just as fulfilling as working for pay."){
           q_plot(q32p)
       }
       else if(input$selected_question == 
               "When jobs are scarce, men should have more right to a job than women."){
           q_plot(q33p)
       }
       else if(input$selected_question == 
               "If a woman earns more money than her husband, it's almost certain to cause problems."){
           q_plot(q35p)
       }
       
   })
   output$happiness_plot <- renderPlot(
       whr %>% 
           filter(country_name %in% input$selected_country) %>%  
           ggplot(aes(x = year, y = score, color = country_name)) +
           geom_point() +
           geom_line() +
           labs(title = "Happiness Score",
                subtitle = "Values range from 1 to 10",
                x = NULL,
                y = "Happiness",
                color = "Counrty",
                caption = "Source: World Happiness Report") +
           theme_igray()
           
   )
   output$regression_wvs <- renderPlot(
       ggplot(agreement, aes(x = agreement, y = Control, color = Gender)) +
           
           # Use geom_point to show the datapoints. 
           
           geom_point() +
           scale_color_manual(values = c("#FF9999", "#56B4E9")) +
           
           # Create a geom_abline object for the female intercept and slope. Set the
           # intercept qual to our previously created female_intercept, while setting
           # slope equal to our previously created female_slope. The color call is for
           # coral, to match the colors used by tidyverse for geom_point().
           
           geom_abline(intercept = Female_intercept,
                       slope = Female_slope, 
                       color = "#FF9999", 
                       size = 1) +
           geom_abline(intercept = Male_intercept,
                       slope = Male_slope,
                       color = "#56B4E9",
                       size = 1) +
           scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                              n.breaks = 20) +
           labs(title = "Linear Regression:\nThe Relationship Between Agreement and Freedom",
                subtitle = "Percieved freedom decreases as agreement increases",
                x = "Percent of people who agreed",
                y = "Average Control and Freedom",
                caption = "Source: World Value Survey (Wave 7)") +
           theme_get() +
           theme(legend.position="bottom")
   )
   getPage<-function() {
       return(includeHTML("datasets/regression_wvs.html"))
   }
   output$regression_gt<-renderUI({getPage()})
}

# Run the application 
shinyApp(ui = ui, server = server)
