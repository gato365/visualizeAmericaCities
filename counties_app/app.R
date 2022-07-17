#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidycensus)
library(ggplot2)
library(terra)
library(r2r)
library(stringr)
library(tidyverse)

<<<<<<< HEAD
#setwd("/cloud/project")
setwd("D:/Old Desktop/Desktop/Cal Poly/Frost SURP/visualizeAmericaCities")
=======
setwd("/cloud/project/visualizeAmericaCities")
#setwd("D:/Old Desktop/Desktop/Cal Poly/Frost SURP/visualizeAmericaCities")
>>>>>>> 88564e848d06998a18ecb6b4abbd403a29f7068b

# Hash maps for all the information
citiesMap_df = read_xlsx('county_state_data.xlsx', col_names=FALSE, sheet = 'citiesMap')
citiesMap_df = citiesMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(citiesMap_df$V1)
vec2 = as.numeric(citiesMap_df$V2)
citiesMap = hashmap()
citiesMap[vec1] = vec2

statesMap_df = read_xlsx('county_state_data.xlsx', col_names=FALSE, sheet = 'statesMap')
statesMap_df = statesMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(statesMap_df$V1)
vec2 = as.numeric(statesMap_df$V2)
statesMap = hashmap()
statesMap[vec1] = vec2

countyNameMap_df = read_xlsx('county_state_data.xlsx', col_names=FALSE, sheet = 'countyNameMap')
countyNameMap_df = countyNameMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(countyNameMap_df$V1)
vec2 = countyNameMap_df$V2
countyNameMap = hashmap()
countyNameMap[vec1] = vec2

stateNameMap_df = read_xlsx('county_state_data.xlsx', col_names=FALSE, sheet = 'stateNameMap')
stateNameMap_df = stateNameMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(stateNameMap_df$V1)
vec2 = stateNameMap_df$V2
stateNameMap = hashmap()
stateNameMap[vec1] = vec2

#plot(p)

# Steps to make drop down
# 1. paste together county + state into a vector
dropDownVector = c()
for (i in 1:length(countyNameMap)) {
  # Format County
  county_name = countyNameMap[as.numeric(i)] # extract county name
  state_name = stateNameMap[as.numeric(i)] # extract state name
  split_county = strsplit(as.character(county_name), "_") # split by underscore
  temp_vector = c()
  for (i in 1:length(split_county[[1]])) {
    temp_vector = append(temp_vector, split_county[[1]][i]) # append each word
  }
  split_county = paste(temp_vector, collapse=" ") # combine to a string
  split_county = str_to_title(split_county) # uppercase first letter of each word for formatting
  
  # Format State
  state_name = state_name[[1]][1]
  county_state_name = paste(split_county, state_name, sep=", ")
  
  # Append to main vector
  dropDownVector = append(dropDownVector, county_state_name)
} 
# 2. use vector as selectInput
# DONE

# 3. Loading RDA
load(file="counties_dataframes.rda")

# 4. Format county strings to match loaded rdata format
rda_strings = c()
for (i in 1:length(dropDownVector)) {
  temp_string = dropDownVector[i]
  split_string = str_trim(unlist(strsplit(temp_string,",")))
  split_string[1] = tolower(split_string[1])
  split_string[1] = str_replace(split_string[1], " ", "_")
  split_string = append(split_string, "df")
  joined_string = paste(split_string, collapse="_")
  rda_strings = append(rda_strings, joined_string)
}

formatted_counties = hashmap()
formatted_counties[dropDownVector] = rda_strings

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("County Map"),
  selectInput("county", "County:",
              dropDownVector),
<<<<<<< HEAD
=======
  radioButtons("density", "Population per dot: ",
               c(100, 200, 400, 800, 1600, 3200)),
>>>>>>> 88564e848d06998a18ecb6b4abbd403a29f7068b
  plotOutput("distPlot")
  # Application title
  # titlePanel("Old Faithful Geyser Data"),
  # 
  # # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #     sidebarPanel(
  #         sliderInput("bins",
  #                     "Number of bins:",
  #                     min = 1,
  #                     max = 50,
  #                     value = 30)
  #     ),
  # 
  #     # Show a plot of the generated distribution
  #     mainPanel(
  #        plotOutput("distPlot")
  #     )
  # )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ####### PLOTTING CODE
  # test_reactive <- eventReactive(countyNum, {
  #   city_race <- get_decennial(
  #     geography = "tract",
  #     variables = race_vars,
  #     state = 36,
  #     county = 61,
  #     # state = reactive({statesMap[as.numeric(countyNum())][[1]][1]}),
  #     # county = reactive({citiesMap[as.numeric(countyNum())][[1]][1]}),
  #     geometry = TRUE,
  #     year = 2020
  #  )
  #   return(city_race)
  # })
  load(file="../counties_dataframes.rda")
<<<<<<< HEAD
  observeEvent(input$county, {
=======
  
  toListen <- reactive({
    list(input$county,input$density)
  })
  
  observeEvent(toListen(), {
>>>>>>> 88564e848d06998a18ecb6b4abbd403a29f7068b
    race_vars <- c(
      Hispanic = "P2_002N",
      White = "P2_005N",
      Black = "P2_006N",
      Asian = "P2_008N"
    )
<<<<<<< HEAD

=======
>>>>>>> 88564e848d06998a18ecb6b4abbd403a29f7068b
    city_race <- get(formatted_counties[input$county][[1]][1])

    #print(city_race2)
    #view(city_race2)
    # Convert data to dots
    city_dots <- as_dot_density(
      city_race,
      value = "value",
      # Best values for each city:
<<<<<<< HEAD
      values_per_dot = 400, # 100 -> 800
=======
      values_per_dot = as.numeric(input$density), # 100 -> 800
>>>>>>> 88564e848d06998a18ecb6b4abbd403a29f7068b
      group = "variable"
    )
    # Use one set of polygon geometries as a base layer
    city_base <- city_race[city_race$variable == "Hispanic", ]
    # Map with ggplot2
    print("Reached ggplot2")
    p <- ggplot() +
      geom_sf(data = city_base,
              fill = "white",
              color = "grey") +
      geom_sf(data = city_dots,
              aes(color = variable), # variable -> "red"
              size = 0.3) + # 0.01 -> 0.3
      theme_void() +
      scale_color_manual(values = c("Black" = "blue",
                                    "Asian" = "red",
                                    "White" = "green",
                                    "Hispanic" = "orange"))
    
    output$distPlot <- renderPlot(
      p
    )
  })
  
  
  #####

  # output$distPlot <- renderPlot({
  #     # generate bins based on input$bins from ui.R
  #     x    <- faithful[, 2]
  #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #     # draw the histogram with the specified number of bins
  #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
