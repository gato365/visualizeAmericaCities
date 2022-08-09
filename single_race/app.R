# Installing packages
library(plotly)
library(bslib)
library(shiny)
library(readxl)
library(tidycensus)
library(ggplot2)
library(terra)
library(r2r)
library(stringr) 
library(tidyverse)
library(kableExtra)

## Set working directory
#setwd("D:/Old Desktop/Desktop/Cal Poly/Frost SURP/visualizeAmericaCities")
setwd("/cloud/project/visualizeAmericaCities")

###############################################
## Main County Mapping
###############################################
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

###############################################
## Borough Mapping
###############################################
# Hash map mapping number to county ANSI code
boroughMap_df = read_xlsx('borough_state_data.xlsx', col_names=FALSE, sheet = 'boroughMap')
boroughMap_df = boroughMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(boroughMap_df$V1)
vec2 = as.numeric(boroughMap_df$V2)
boroughMap = hashmap()
boroughMap[vec1] = vec2

# Hash map mapping number to state FISP code
bStatesMap_df = read_xlsx('borough_state_data.xlsx', col_names=FALSE, sheet = 'bStatesMap')
bStatesMap_df = bStatesMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(bStatesMap_df$V1)
vec2 = as.numeric(bStatesMap_df$V2)
bStatesMap = hashmap()
bStatesMap[vec1] = vec2

# Hash map mapping number to county name
boroughNameMap_df = read_xlsx('borough_state_data.xlsx', col_names=FALSE, sheet = 'boroughNameMap')
boroughNameMap_df = boroughNameMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(boroughNameMap_df$V1)
vec2 = boroughNameMap_df$V2
boroughNameMap = hashmap()
boroughNameMap[vec1] = vec2

# Hash map mapping number to state USPS code
bStateNameMap_df = read_xlsx('borough_state_data.xlsx', col_names=FALSE, sheet = 'bStateNameMap')
bStateNameMap_df = bStateNameMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(bStateNameMap_df$V1)
vec2 = bStateNameMap_df$V2
bStateNameMap = hashmap()
bStateNameMap[vec1] = vec2

#plot(p)

## Steps to make drop down
## 1. paste together county + state into a vector
## COUNTIES
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
## BOROUGHS
dropDownVector2 = c()
for (i in 1:length(boroughNameMap)) {
  ## Format Borough
  borough_name = boroughNameMap[as.numeric(i)] # extract county name
  state_name = bStateNameMap[as.numeric(i)] # extract state name
  split_borough = strsplit(as.character(borough_name), "_") # split by underscore
  temp_vector = c()
  for (i in 1:length(split_borough[[1]])) {
    temp_vector = append(temp_vector, split_borough[[1]][i]) # append each word
  }
  split_borough = paste(temp_vector, collapse=" ") # combine to a string
  split_borough = str_to_title(split_borough) # uppercase first letter of each word for formatting
  
  ## Format State
  state_name = state_name[[1]][1]
  county_state_name = paste(split_borough, state_name, sep=", ")
  
  ## Append to main vector
  dropDownVector2 = append(dropDownVector2, county_state_name)
}
## 2. use vector as selectInput
# DONE

## 3. Format county strings to match loaded rdata format
## Ex: "New York, NY" -> "new_york_NY_df"
## COUNTIES
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

## BOROUGHS
borough_rda_strings = c()
for (i in 1:length(dropDownVector2)) {
  temp_string = dropDownVector2[i]
  split_string = str_trim(unlist(strsplit(temp_string,",")))
  split_string[1] = tolower(split_string[1])
  split_string[1] = str_replace(split_string[1], " ", "_")
  split_string = append(split_string, "df")
  joined_string = paste(split_string, collapse="_")
  borough_rda_strings = append(borough_rda_strings, joined_string)
}

## Hash map mapping old string to new formatted string
formatted_boroughs = hashmap()
formatted_boroughs[dropDownVector2] = borough_rda_strings

## Join all the strings
all_rda_strings = c(rda_strings, borough_rda_strings)
## Remove vectors that have boroughs
all_rda_strings = all_rda_strings[! all_rda_strings %in% c('new_york_NY_df')]

## UI
ui <- fluidPage(
  theme = bs_theme(version = 5),
  # Application title
  titlePanel("Choose a Race to Display"),
  radioButtons("race", "Race: ",
               c("Hispanic", "White", "Black", "Asian")),
  selectInput("county", "County:",
              dropDownVector),
  conditionalPanel("input.county == 'New York, NY'",
                   selectInput('NYC_B','Select Borough:',
                               choices = dropDownVector2,
                               selected = "Manhattan, NY"),
  ), 
  ## Radio buttons for choosing population density per dot
  radioButtons("density", "Population per dot: ",
               c(400, 800, 1600, 3200)),
  ## Graphing plot
  plotOutput("distPlot")
)

# Define server logic
server <- function(input, output, session) {
  ## Load RDA
  load(file="../counties_dataframes.rda")
  load(file="../boroughs_dataframes.rda")
  ## Create reactive object
  toListen <- reactive({
    list(input$county,input$density,input$NYC_B,input$race)
  })
  
  observeEvent(toListen(), {
    print(input$county)
    print(input$race)
    #if(input$county)

    if(input$county == "New York, NY") {
      city_race <- get(formatted_boroughs[input$NYC_B][[1]][1])
      #city_race <- get(formatted_counties[input$county][[1]][1])
    } else {
      city_race <- get(formatted_counties[input$county][[1]][1])
    }
    
    city_race = city_race %>% 
      filter(variable %in% c(input$race))

    ## Convert data to dots
    city_dots <- as_dot_density(
      city_race,
      value = "value",
      # Best values for each city:
      values_per_dot = as.numeric(input$density),
      group = "variable"
    )
    
    # Use one set of polygon geometries as a base layer
    city_base <- city_race[city_race$variable == input$race, ]
    
    # Map with ggplot2
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
}

# Run the application 
shinyApp(ui = ui, server = server)
