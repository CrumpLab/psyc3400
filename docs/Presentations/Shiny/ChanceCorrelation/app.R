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
   titlePanel("Chance Correlation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("samplen",
                     "Sample N:",
                     min = 5,
                     max = 1000,
                     value = 10),
         sliderInput("samplerange",
                     "Range:",
                     min = -100,
                     max = 100,
                     step= 1,
                     value = c(0,10)),
         actionButton("do", "resample")
      ),
      
      # Show a plot 
      mainPanel(
         plotOutput("linePlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #observeEvent(input$do, {
  #  x<-runif(input$samplen,input$samplerange[1],input$samplerange[2])
  #  y<-runif(input$samplen,input$samplerange[1],input$samplerange[2])
  #  df<-data.frame(x,y)
  #})
  
  randomVals <- eventReactive({input$do
    input$samplen
    input$samplerange}, {
    x<-runif(input$samplen,input$samplerange[1],input$samplerange[2])
    y<-runif(input$samplen,input$samplerange[1],input$samplerange[2])
    df<-data.frame(x,y)
    return(df)
  })
   
  output$linePlot <-renderPlot({
    
    #x<-runif(input$samplen,input$samplerange[1],input$samplerange[2])
    #y<-runif(input$samplen,input$samplerange[1],input$samplerange[2])
    #df<-data.frame(x,y)
    df<-randomVals()
    
    ggplot(df,aes(x=x,y=y))+
      geom_point()+
      geom_smooth(method="lm",se=FALSE)+
      theme_classic(base_size = 20)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

