
library(shiny)
library(tidyverse)
library(rsconnect)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))

ui <- fluidPage(sliderInput(inputId = "date", #input id is how this element will be called in the server later
                            label = "Choose the range of dates:", #how it looks to the user; gives instructions
                            min = as.Date("2020-01-21","%Y-%m-%d"), #min date
                            max = as.Date("2022-03-30","%Y-%m-%d"), #max date
                            value = c(as.Date("2020-01-21"), as.Date("2022-03-30")),
                            timeFormat = "%Y-%m-%d",
                            step = 1),
                selectInput(inputId = "stateOne",
                            label = "Choose the first state:",
                            choices = c(Alabama = "alabama", Alaska = "alaska", Arizona = "arizona",
                                        Arkansas = "arkansas", California = "california", Colorado = "colorado",
                                        Connecticut = "connecticut", Delaware = "delaware", 
                                        'District of Columbia' = "district of columbia", Florida = "florida",
                                        Georgia = "georgia", Hawaii = "hawaii", Idaho = "idaho", Illinois = "illinois",
                                        Indiana = "indiana", Iowa = "iowa", Kansas = "kansas",
                                        Kentucky = "kentucky", Louisiana = "louisiana", Maine = "maine",
                                        Maryland = "maryland", Massachusetts = "massachusetts",
                                        Michigan = "michigan", Minnesota = "minnesota", 
                                        Mississippi = "mississippi", Missouri = "missouri",
                                        Montana = "montana", Nebraska = "nebraska", Nevada = "nevada",
                                        'New Hampshire' = "new hampshire", 'New Jersey' = "new jersey",
                                        'New Mexico' = "new mexico", 'New York' = "new york",
                                        'North Carolina' = "north carolina", 'North Dakota' = "north dakota",
                                        Ohio = "ohio", Oklahoma = "oklahoma", Oregon = "oregon",
                                        Pennsylvania = "pennsylvania", 'Rhode Island' = "Rhode Island",
                                        'South Carolina' = "south carolina", 'South Dakota' = "South Dakota",
                                        Tennessee = "tennessee", Texas = "texas", Utah = "utah",
                                        Vermont = "vermont", Virginia = "virginia", Washington = "washington",
                                        'West Virginia' = "west virginia", Wisconsin = "wisconsin",
                                        Wyoming = "wyoming")), #drop-down options, shows the word 'Female' but then chooses 'F' in dataset
                selectInput(inputId = "stateTwo",
                            label = "Choose the second state:",
                            choices = c(Alabama = "alabama", Alaska = "alaska", Arizona = "arizona",
                                        Arkansas = "arkansas", California = "california", Colorado = "colorado",
                                        Connecticut = "connecticut", Delaware = "delaware", 
                                        'District of Columbia' = "district of columbia", Florida = "florida",
                                        Georgia = "georgia", Hawaii = "hawaii", Idaho = "idaho", Illinois = "illinois",
                                        Indiana = "indiana", Iowa = "iowa", Kansas = "kansas",
                                        Kentucky = "kentucky", Louisiana = "louisiana", Maine = "maine",
                                        Maryland = "maryland", Massachusetts = "massachusetts",
                                        Michigan = "michigan", Minnesota = "minnesota", 
                                        Mississippi = "mississippi", Missouri = "missouri",
                                        Montana = "montana", Nebraska = "nebraska", Nevada = "nevada",
                                        'New Hampshire' = "new hampshire", 'New Jersey' = "new jersey",
                                        'New Mexico' = "new mexico", 'New York' = "new york",
                                        'North Carolina' = "north carolina", 'North Dakota' = "north dakota",
                                        Ohio = "ohio", Oklahoma = "oklahoma", Oregon = "oregon",
                                        Pennsylvania = "pennsylvania", 'Rhode Island' = "Rhode Island",
                                        'South Carolina' = "south carolina", 'South Dakota' = "South Dakota",
                                        Tennessee = "tennessee", Texas = "texas", Utah = "utah",
                                        Vermont = "vermont", Virginia = "virginia", Washington = "washington",
                                        'West Virginia' = "west virginia", Wisconsin = "wisconsin",
                                        Wyoming = "wyoming")),
                submitButton(text = "Create my plot!"), 
                plotOutput(outputId = "covid_plot")
)

server <- function(input, output) {
  output$covid_plot <- renderPlot(
    covid19 %>%
      mutate(state = tolower(state)) %>% 
      left_join(census_pop_est_2018, by = "state") %>% 
      group_by(state) %>% 
      mutate(cases_per_10000 = (`cases`/est_pop_2018)*10000) %>% 
      filter(state == input$stateOne | state == input$stateTwo) %>%
      ggplot(aes(x = date,
                 y = cases_per_10000)) +
      scale_x_continuous(limits = input$date) + 
      geom_line(aes(color = state))
  )
}

shinyApp(ui = ui, server = server)
