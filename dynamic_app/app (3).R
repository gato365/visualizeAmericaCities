#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Cities"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('cities','Select City:',choices = c('DC','Philly', 'SF','NYC'),selected = 'SF'),
      conditionalPanel("input.cities == 'NYC'",
                       selectInput('NYC_B','Select Boroughs:',
                                   choices = c('Bronx','Manhattan', 'Queens','Staten Island','Brooklyn'),
                                   selected = 'Manhattan'),
      )
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  output$text <- renderText({ 
    
    if(input$cities == 'NYC'){
      paste0('city: ', input$cities, ' borough: ', input$NYC_B)
    } else {
      paste0('city: ', input$cities)
    }
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
