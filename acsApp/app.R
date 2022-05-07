#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidycensus)
library(tidyverse)
library(tigris)
library(mapview)
library(sf)
library(bslib)
library(showtext)
library(thematic)



# Setup the bslib theme object
my_theme <- bs_theme(bootswatch = "slate",
                     base_font = font_google("Righteous"))

# Let thematic know to update the fonts, too
thematic_shiny(font = "auto") 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #theme 
  theme = my_theme, 
  # Application title
  titlePanel("Demographic Across Different Counties and States"), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('race',h1('Select Race'),list('Asian' = 'B02001_005E',
                                                    'African American' = 'B02001_003E',
                                                    "White" = 'B02001_002E', 
                                                    'American Indian and Alaska Native' = 'B02001_004E', 
                                                    'Native Hawaiian and Other Pacific Islander' = 'B02001_002E'
      )),
      selectInput('state_county','Choose State County Pair', choices = list('California - Santa Clara' = 'CA - Santa Clara County',
                                                                            'Maryland - Baltimore County' = 'MD - Baltimore County', 
                                                                            "Georgia - Fulton County" = "GA - Fulton County", 
                                                                            "Massachusetts - Suffolk County" = "MA - Suffolk County", 
                                                                            "Illinois - Cook County" = "IL - Cook County", 
                                                                            "Michigan - Wayne County" = "MI - Wayne County", 
                                                                            "Texas - Harris County" = "TX - Harris County", 
                                                                            "California - Los Angeles County" = "CA - Los Angeles County", 
                                                                            "Florida - Miami-Dade County" = "FL - Miami-Dade County",
                                                                            "New York - New York County" = "NY - New York County", 
                                                                            "Philadelphia - Philadelphia County" = "PHI - Philadelphia County",
                                                                            "Arizona - Maricopa County" = "AZ - Maricopa County", 
                                                                            "California - San Francisco County" = "CA - San Francisco County",
                                                                            "DC - District of Columbia County" = "DC - District of Columbia County")),
      
      selectInput("year", "Choose Selected Year", choices = list("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")),
      
      actionButton("button","Map")
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      #add button to calculate 
      
      plotOutput("distPlot"),
      verbatimTextOutput('state_county')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  inform <- eventReactive(input$button,{
    select_sc = input$state_county
    select_sc_clean = str_trim(str_split(select_sc,'-')[[1]])
    selected_state = select_sc_clean[1]
    selected_county = select_sc_clean[2]
    tmp_state = selected_state
    tmp_county = selected_county
    tmp_race = input$race
    tmp_year = input$year
    
    tmp_sf_file <- get_acs(geography = "tract", 
                           state = tmp_state,
                           county = tmp_county,
                           variable = tmp_race,
                           geometry = TRUE, 
                           year = as.numeric(tmp_year),
                           cache_table = TRUE)
    
    return(list(tmp_state = tmp_state,tmp_county = tmp_county,
                tmp_race = tmp_race, tmp_year = tmp_year, tmp_sf_file = tmp_sf_file))
    
    
    
  })
  
  output$state_county<- renderText({ 
    inform <- inform()
    race <- c(
      
      'Asian' = 'B02001_005E',
      'African American' = 'B02001_003E',
      "White" = 'B02001_002E', 
      'American Indian and Alaska Native' = 'B02001_004E', 
      'Native Hawaiian and Other Pacific Islander' = 'B02001_002E')
    
    
    state = inform$tmp_state
    county = inform$tmp_county
    year = inform$tmp_year
    race = names(race[race == inform$tmp_race])
    
    sf_file = inform$tmp_sf_file
    
    
    total = sum(sf_file$estimate)
    
    paste0('State: ', state, ', County: ', county, ', Year: ', year, ', Race: ', race, ', Total: ', total)
    
    
  })
  
  output$distPlot <- renderPlot({
    
    button <- inform()
    # state = button$tmp_state
    # county = button$tmp_county
    # race = button$tmp_race
    # year = button$tmp_year
    
    sf_file = button$tmp_sf_file
    
    
    
    # sf_file <- get_acs(geography = "tract", state = state, 
    #                    county = county,
    #                    variable = race, 
    #                    geometry = TRUE, year = as.numeric(year),
    #                    cache_table = TRUE)
    
    
    lst = list(sf_file)
    plot(lst[[1]]['estimate'])
    
    
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
