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
   titlePanel("Simulated Null"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("samplen",
                     "Sample N:",
                     min = 5,
                     max = 100,
                     step= 1,
                     value = 10),
         sliderInput("normalmean",
                     "Normal Mean:",
                     min = -100,
                     max = 100,
                     step= 1,
                     value = 0),
         sliderInput("normalsd",
                     "Normal SD:",
                     min = 1,
                     max = 100,
                     step= 1,
                     value = 10),
         sliderInput("numsim",
                     "# of simulations:",
                     min = 2,
                     max = 1000,
                     step= 1,
                     value = 10),
         actionButton("do", "run")
      ),
      
      # Show a plot 
      mainPanel(
         plotOutput("diffPlot",width = "100%", height = "250px"),
         plotOutput("tPlot",width = "100%", height = "250px"),
         h3(textOutput("criticalt"))
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
    input$normalmean
    input$normalsd
    input$numsim}, {
    
    sample_A<-t(replicate(input$numsim,rnorm(input$samplen,
                                              input$normalmean,
                                              input$normalsd)))
    sample_B<-t(replicate(input$numsim,rnorm(input$samplen,
                                           input$normalmean,
                                           input$normalsd)))
    sample_data<-sample_A-sample_B
    dims<-dim(sample_data)
    save_mean<-c()
    save_t<-c()
    for(i in 1:dims[1]){
      save_mean[i]<-mean(sample_data[i,])
      save_t[i]<-t.test(sample_data[i,])$statistic
    }

    df<-data.frame(save_mean,save_t)
    return(df)
  })
   
  output$diffPlot <-renderPlot({
    
    dfa<-randomVals()
    
    ggplot(dfa,aes(x=save_mean))+
      geom_histogram(color="white", bins=40)+
      theme_classic(base_size = 20)+
      xlab("mean difference")+
      ggtitle("Histogram of mean differences")
  })
  
  output$tPlot <-renderPlot({
    
    dfb<-randomVals()
    
    ggplot(dfb,aes(x=save_t))+
      geom_histogram(color="white", bins=40)+
      geom_vline(xintercept=qt(.95,(input$samplen-1)), 
                 color="red",
                 size=2)+
      theme_classic(base_size = 20)+
      xlab("t-value")+
      ggtitle("Histogram of t-values")+
      geom_label(data = data.frame(x = qt(.95,(input$samplen-1)), 
                                   y = 0, 
                                   label = "Critical t"), 
                 aes(x = x, y = y, label = label))
  })
  
  output$criticalt <- renderText({ 
    paste(c("Critical t (one-tailed) = "),round(qt(.95,(input$samplen-1)),digits=3))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

