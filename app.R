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
   titlePanel("Distancias"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel( 
         numericInput("lonstart",
                      "Longitud de salida:",
                      min = -180,
                      max = 180,
                      value = -70),
         numericInput("latstart",
                      "Latitud de salida:",
                      min = -90,
                      max = 90,
                      value = -33),
         numericInput("lonend",
                      "Longitud de llegada:",
                      min = -180,
                      max = 180,
                      value = 151),
         numericInput("latend",
                      "Latitud de llegada:",
                      min = -90,
                      max = 90,
                      value = -34)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot", height = "600px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      source("planes.R", local = TRUE)
      geomap(c(input$lonstart, input$latstart), c(input$lonend, input$latend))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

