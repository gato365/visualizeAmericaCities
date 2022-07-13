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
ui = fluidPage(
  selectInput("state", "Choose a state:",
              list(`East Coast` = list("NY", "NJ", "CT"),
                   `West Coast` = list("WA", "OR", "CA"),
                   `Midwest` = list("MN", "WI", "IA"))
  ),
  textOutput("result")
)

# Define server logic required to draw a histogram
server = function(input, output) {
  output$result <- renderText({
    paste("You chose", input$state)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
