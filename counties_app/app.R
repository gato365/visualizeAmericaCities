#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Installing packages
library(bslib)
library(shiny)
library(readxl)
library(tidycensus)
library(ggplot2)
library(terra)
library(r2r)
library(stringr)
library(tidyverse)

## Set working directory
#setwd("D:/Old Desktop/Desktop/Cal Poly/Frost SURP/visualizeAmericaCities")
setwd("/cloud/project/visualizeAmericaCities")

## Hash map mapping number to county ANSI code
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

## Hash map mapping number to state FISP code
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

## Hash map mapping number to county name
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

## Hash map mapping number to state USPS code
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

## Steps to make drop down
## 1. paste together county + state into a vector
dropDownVector = c()
for (i in 1:length(countyNameMap)) {
  ## Format County
  county_name = countyNameMap[as.numeric(i)] # extract county name
  state_name = stateNameMap[as.numeric(i)] # extract state name
  split_county = strsplit(as.character(county_name), "_") # split by underscore
  temp_vector = c()
  for (i in 1:length(split_county[[1]])) {
    temp_vector = append(temp_vector, split_county[[1]][i]) # append each word
  }
  split_county = paste(temp_vector, collapse=" ") # combine to a string
  split_county = str_to_title(split_county) # uppercase first letter of each word for formatting
  
  ## Format State
  state_name = state_name[[1]][1]
  county_state_name = paste(split_county, state_name, sep=", ")
  
  ## Append to main vector
  dropDownVector = append(dropDownVector, county_state_name)
} 
## 2. use vector as selectInput
# DONE

## 3. Format county strings to match loaded rdata format
## Ex: "New York, NY" -> "new_york_NY_df"
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

## Hash map mapping old string to new formatted string
formatted_counties = hashmap()
formatted_counties[dropDownVector] = rda_strings

boroughVector = c()

## Define UI for application
ui <- fluidPage(
  ## bslib theme
  theme = bs_theme(version = 5),
  tabsetPanel(
    tabPanel(
      "County Map",
      "An interactive county map",
      ## Main title
      titlePanel("County Map"),
      ## Drop down list for choosing a county
      selectInput("county", "County:",
                  dropDownVector),
      selectInput("borough", "Borough:", boroughVector), 
      ## Radio buttons for choosing population density per dot
      radioButtons("density", "Population per dot: ",
                   c(400, 800, 1600, 3200)),
      ## Graphing plotly
      plotlyOutput("distPlot")
    ),
    tabPanel(
      "Table",
      "TODO after map is finished"
    )
  )
)

## Define server logic
server <- function(input, output, session) {
  ## Load RDA
  load(file="../counties_dataframes.rda")
  ## Create reactive object
  toListen <- reactive({
    list(input$county,input$density)
  })
  
  ## Watching for changes in the reactive object
  observeEvent(toListen(), {
    ## Identify variables for mapping
    race_vars <- c(
      Hispanic = "P2_002N",
      White = "P2_005N",
      Black = "P2_006N",
      Asian = "P2_008N"
    )
    city_race <- get(formatted_counties[input$county][[1]][1])
    
    ## Create new columns for more information
    grouped_df = city_race
    grouped_df = grouped_df %>% 
      group_by(NAME) %>% 
      mutate(total_population = sum(value)) %>% 
      mutate(race_percent = (value/total_population) * 100)
    
    ## Convert data to dots
    city_dots <- as_dot_density(
      grouped_df,
      value = "value",
      # Best values for each city:
      values_per_dot = as.numeric(input$density),
      group = "variable"
    )
    ## Use one set of polygon geometries as a base layer
    grouped_df_base <- grouped_df[grouped_df$variable == "Hispanic", ]
    
    ## Map with ggplot2
    print("Reached ggplot2")
    p2 <- ggplot(data=grouped_df) +
      geom_sf(data = grouped_df_base,
              aes(text=NAME, color=NAME)
      ) +
      geom_sf(data = city_dots,  
              aes(color = variable),
              size = 0.3) + # 0.01 -> 0.3
      theme_void() +
      scale_color_manual(values = c("Black" = "blue",
                                    "Asian" = "red",
                                    "White" = "green",
                                    "Hispanic" = "orange"))
    ## Create ggplotly
    gg_2 <- ggplotly(p2)
    ## This code below allows user to hover over tract area and get information
    gg_3 <- gg_2 %>% 
      style(
        hoveron = "fills",
        # override the color mapping
        # line.color = toRGB("gray40"),
        # don't apply these style rules to the first trace, which is the background graticule/grid
        traces = seq.int(3, length(gg_2$x$data))
      ) %>%
      hide_legend()
    output$distPlot <- renderPlotly(
      gg_3
    )
  })
  ## Exit gracefully
  session$onSessionEnded(function() {
    stopApp()
  })
}

## Run the application 
shinyApp(ui = ui, server = server)
