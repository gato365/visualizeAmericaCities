library(shiny)
suppressPackageStartupMessages(library(tidyverse))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Iris Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput('x_var',label = "X-Axis", 
                  choices = list("Sepal.Length"= "Sepal.Length", 
                                 "Sepal.Width" = "Sepal.Width", 
                                 "Petal.Length"= "Petal.Length", 
                                 "Petal.Width" = "Petal.Width")),
      selectInput('y_var',label = "Y-Axis", 
                  choices = list("Sepal.Length"= "Sepal.Length", 
                                 "Sepal.Width" = "Sepal.Width", 
                                 "Petal.Length"= "Petal.Length", 
                                 "Petal.Width" = "Petal.Width"))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    iris %>% 
      ggplot(aes_string(x = input$x_var, y = input$y_var))  +
      geom_point() +
      theme_bw()
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
