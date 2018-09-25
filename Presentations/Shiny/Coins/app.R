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
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Coin Flipping"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         actionButton("resetGraph", "reset"),
         actionButton("flip1", "flip 1 coin"),
         actionButton("flip10", "flip 10 coins")
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
  
  values <- reactiveValues(flip = 1, outcome = 1)
  
 eventReactive(input$resetGraph, {
    values$flip <- 1
    values$outcome <- 1
    values$mean_heads<-1
    values$df<- data.frame(flip=1,outcome=1,mean_heads=1)
  })
  
  randomVals <- eventReactive(input$flip1, {
    
    flip_outcome<-sample(c(0,1),1)
    values$flip<-values$flip+1
    mean_heads<-mean(c(values$df$outcome,flip_outcome))
    t_df<-data.frame(flip=values$flip, outcome=flip_outcome, mean_heads)
    values$df <-rbind(values$df,t_df)
    return(values$df)
  })
   
  output$linePlot <-renderPlot({
    
    #x<-runif(input$samplen,input$samplerange[1],input$samplerange[2])
    #y<-runif(input$samplen,input$samplerange[1],input$samplerange[2])
    #df<-data.frame(x,y)
    df<-randomVals()
    
    ggplot(df,aes(x=flip,y=mean_heads))+
      geom_point()+
      geom_line()+
      theme_classic(base_size = 20)+
      ylim(0,1)
    
    print(df)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

