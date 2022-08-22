#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
library(sf)
library(kableExtra)
library(shinyWidgets)

## Set working directory
setwd("D:/Old Desktop/Desktop/Cal Poly/Frost SURP/visualizeAmericaCities")
#setwd("/cloud/project/visualizeAmericaCities")

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
## EX: "san_francisco", "CA", -> "San Francisco, CA"
## COUNTIES

## Start runtime testing
start_time = Sys.time()
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

## End runtime testing
end_time = Sys.time()
print(paste("FOR LOOP TIME: ", (end_time-start_time), sep = ""))

## Hash map mapping old string to new formatted string
formatted_boroughs = hashmap()
formatted_boroughs[dropDownVector2] = borough_rda_strings

## Join all the strings
all_rda_strings = c(rda_strings, borough_rda_strings)
## Remove vectors that have boroughs
all_rda_strings = all_rda_strings[! all_rda_strings %in% c('new_york_NY_df')]

empty_vector = c()
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
      conditionalPanel("input.county == 'New York, NY'",
                       selectInput('NYC_B','Select Borough:',
                                   choices = dropDownVector2,
                                   selected = "Manhattan, NY"),
      ), 
      ## Radio buttons for choosing population density per dot
      radioButtons("density", "Population per dot: ",
                   c(400, 800, 1600, 3200)),
      ## Graphing plotly
      plotlyOutput("distPlot")
      # column(4,progressBar(id = "pb4", value = 0, display_pct = T))
    ),
    ## Table Panel UI
    tabPanel(
      "Table",
      titlePanel("Distribution Table"),
      sidebarLayout(
        sidebarPanel(
          h6("Input as a decimal (0.25 = 25%)"),
          h6("The below inputs must sum to 1."),
          #sliderInput("hispanic_slider", "% Hispanic: ", min=0, max=100, value=0, step=1)
          textInput("hispanic_percent", "% Hispanic"),
          textInput("white_percent", "% White"),
          textInput("black_percent", "% Black"),
          textInput("asian_percent", "% Asian"),
          htmlOutput('warningText'),
          actionButton("update", "Update")
        ),
        mainPanel(
          ## Drop down list for choosing a county
          selectInput("county_table", "County:",
                      dropDownVector),
          conditionalPanel("input.county_table == 'New York, NY'",
                           selectInput('NYC_B_table','Select Borough:',
                                       choices = dropDownVector2,
                                       selected = "Manhattan, NY"),
          )
        )
      ),
      tableOutput("top_10_kable")
    )
  )
)

## Define server logic
server <- function(input, output, session) {
  ## Load RDA
  load(file="../counties_dataframes.rda")
  load(file="../boroughs_dataframes.rda")
  ## Create reactive object
  toListen <- reactive({
    list(input$county,input$density,input$NYC_B)
  })
  toListenTable <- reactive({
    list(input$county_table, input$NYC_B_table, input$update)
  })
  

  observeEvent(toListen(), {
    print(input$county)
    ## Create Progress Object
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making plot", value = 0)
    progress$inc(1/4, detail = "Generating density dots")
    #if(input$county)
    ## Identify variables for mapping
    race_vars <- c(
      Hispanic = "P2_002N",
      White = "P2_005N",
      Black = "P2_006N",
      Asian = "P2_008N"
    )
    if(input$county == "New York, NY") {
      city_race <- get(formatted_boroughs[input$NYC_B][[1]][1])
      #city_race <- get(formatted_counties[input$county][[1]][1])
    } else {
      city_race <- get(formatted_counties[input$county][[1]][1])
    }
    
    ## Start runtime testing
    start_time = Sys.time()
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
    ## End runtime testing
    end_time = Sys.time()
    print(paste("city_dots time: ", (end_time-start_time), sep=""))
    
    progress$inc(1/4, detail = "Creating ggplot")
    ## Use one set of polygon geometries as a base layer
    grouped_df_base <- grouped_df[grouped_df$variable == "Hispanic", ]
    
    ## Start runtime testing
    start_time = Sys.time()
    
    ## Map with ggplot2
    print("Reached ggplot2")
    p2 <- ggplot(data=grouped_df) +
      geom_sf(data = grouped_df_base,
              aes(text=
                    paste(
                      paste(
                        "Total Population: ", total_pop, sep=""
                      ),
                      paste(
                        "Hispanic: ", round(hispanic_pct, digits=2), "%", sep=""
                      ),
                      paste(
                        "White: ", round(white_pct, digits=2), "%", sep=""
                      ),
                      paste(
                        "Black: ", round(black_pct, digits=2), "%", sep=""
                      ),
                      paste(
                        "Asian: ", round(asian_pct, digits=2), "%", sep=""
                      ),
                      sep="\n"
                    ),
                  color=NAME
              )
              #mapping = aes(fill = AREA),
              #fill = "white",
              #color = "grey"
      ) +
      geom_sf(data = city_dots,  
              aes(color = variable),
              size = 0.3) + # 0.01 -> 0.3
      theme_void() +
      scale_color_manual(values = c("Black" = "blue",
                                    "Asian" = "red",
                                    "White" = "green",
                                    "Hispanic" = "orange"))
    ## End runtime testing
    end_time = Sys.time()
    print(paste("ggplot time: ", (end_time-start_time), sep=""))
    
    progress$inc(1/4, detail = "Creating ggplotly")
    ## Create ggplotly
    print("reached ggplotly")
    
    ## Start runtime testing
    start_time = Sys.time()
    
    gg_2 <- ggplotly(p2)
    ## This code below allows user to hover over tract area and get information
    print("reached gg3")
    gg_3 <- gg_2 %>% 
      style(
        hoveron = "fills",
        # override the color mapping
        # line.color = toRGB("gray40"),
        # don't apply these style rules to the first trace, which is the background graticule/grid
        traces = seq.int(3, length(gg_2$x$data))
      ) %>%
      hide_legend()
    print("reached render plotly")
    output$distPlot <- renderPlotly(
      gg_3
    )
    ## End runtime testing
    end_time = Sys.time()
    print(paste("ggplotly time: ", (end_time-start_time), sep=""))
    
    progress$inc(1/4, detail = "Rendering map")
  })
  
  ## Table Tab
  ## Default values
  r <- reactiveValues(hispanic_percent = 0.25, white_percent = 0.25, black_percent = 0.25, 
                      asian_percent = 0.25)
  ## Update values if actionButton
  observeEvent(input$update, {
    r$hispanic_percent = input$hispanic_percent
    r$white_percent = input$white_percent
    r$black_percent = input$black_percent
    r$asian_percent = input$asian_percent
  })
  
  #test_df = get("san_jose_CA_df")
  observeEvent(toListenTable(), {
    test_df = get("san_jose_CA_df")
    
    dist_percent = c(as.numeric(r$hispanic_percent), as.numeric(r$white_percent), 
                     as.numeric(r$black_percent), as.numeric(r$asian_percent))
    if(sum(dist_percent) != 1) {
      warning_text = 'Distribution does not add up to 1. Check your inputs.'
    }
    else {
      warning_text = ''
      top_10_df = data.frame()
      if(input$county_table == "New York, NY") {
        city_race <- get(formatted_boroughs[input$NYC_B_table][[1]][1])
        #city_race <- get(formatted_counties[input$county_table][[1]][1])
      } else {
        city_race <- get(formatted_counties[input$county_table][[1]][1])
      }
      ## Generate necessary columns
      chisq_df = city_race %>% 
        mutate(hispanic_count = round((total_pop * (hispanic_pct/100)), digits=0),
               white_count = round((total_pop * (white_pct/100)), digits=0),
               black_count = round((total_pop * (black_pct/100)), digits=0),
               asian_count = round((total_pop * (asian_pct/100)), digits=0))
      chisq_df = chisq_df %>% 
        filter(hispanic_count > 5,
               white_count > 5,
               black_count > 5,
               asian_count > 5)
      ## Drop NAs
      chisq_df = na.omit(chisq_df)
      ## Perform chi-squared testing
      chisq_df = chisq_df %>% 
        rowwise() %>% 
        mutate(
          test_stat = chisq.test(c(hispanic_count, white_count, black_count, asian_count), p=dist_percent)$statistic,
          p_val = chisq.test(c(hispanic_count, white_count, black_count, asian_count), p=dist_percent)$p.value
        )
      ## Sort by test_stat  
      ordered_race = chisq_df %>% 
        group_by(NAME) %>% 
        arrange(test_stat)
      #print(ordered_race)
      ## Only keep top 10 test_stat values, and subset dataframe
      ordered_race = head(ordered_race[!duplicated(ordered_race$NAME),], 10)
      ordered_race = ordered_race[c("NAME", "total_pop", "hispanic_count", "white_count",
                                    "black_count", "asian_count", "p_val", "test_stat")]
      ## Keep top 10
      top_10_df = rbind(top_10_df, ordered_race)
      top_10_df = top_10_df[order(top_10_df$test_stat),]
      top_10_df = head(top_10_df, 10)
      
      ## Drop geometry column
      top_10_df = st_drop_geometry(top_10_df)
      
      output$top_10_kable <- function() {
        top_10_df  %>% 
          kable("html") %>% 
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                        font_size = 15)
      }
    }
    output$warningText = renderText(warning_text)
  })
  
  
  ## OUTPUTS
  output$hispanic_out <- renderText(input$hispanic_percent)
  output$white_out <- renderText(input$white_percent)
  output$black_out <- renderText(input$black_percent)
  output$asian_out <- renderText(input$asian_percent)
  
  ## Exit gracefully
  session$onSessionEnded(function() {
    stopApp()
  })
}

## Run the application 
shinyApp(ui = ui, server = server)
