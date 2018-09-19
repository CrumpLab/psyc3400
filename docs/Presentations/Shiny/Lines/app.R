#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Lines"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("yintercept",
                     "Y intercept:",
                     min = -5,
                     max = 5,
                     value = 0),
         sliderInput("slope",
                     "Slope:",
                     min = -1,
                     max = 1,
                     step= .1,
                     value = 0)
      ),
      
      # Show a plot 
      mainPanel(
         plotOutput("linePlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$linePlot <-renderPlot({
    
    ggplot()+
      theme_classic(base_size = 20)+
      xlim(-10,10)+
      ylim(-10,10)+
      geom_vline(xintercept = 0, color="grey")+
      geom_hline(yintercept=0, color="grey")+
      geom_abline(slope=input$slope, intercept=input$yintercept)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

