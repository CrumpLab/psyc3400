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
   titlePanel("Independent t-test"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
      column(2,
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
                     value = c(0,0)),
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
                     value = 500),
         actionButton("do", "run")
      ),
      
      # Show a plot 
      column(5,
            plotOutput("normPlot",width = "100%", height = "250px"),
            plotOutput("samplePlot",width = "100%", height = "250px"),
            h4(textOutput("sampleStat"))
          ),
      column(5,
            plotOutput("diffPlot",width = "100%", height = "250px"),
            plotOutput("tPlot",width = "100%", height = "250px"),
            h4(textOutput("criticalt"))
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
  
  exampleSample <-eventReactive({input$do
    input$samplen
    input$normalmean
    input$normalsd
    input$numsim}, {
      
      s<-input$normalsd
      m1<-input$normalmean[1]
      m2<-input$normalmean[2]
      sn<-input$samplen
      
      s_A<-rnorm(sn,m1,s)
      s_B<-rnorm(sn,m2,s)
      
      p_df<-data.frame(sample=rep(c("A","B"),each=sn),
                       values=c(s_A,s_B))
      return(p_df)
      
    })
  
  
  randomVals <- eventReactive({input$do
    input$samplen
    input$normalmean
    input$normalsd
    input$numsim}, {
    
    sample_A<-t(replicate(input$numsim,rnorm(input$samplen,
                                              input$normalmean[1],
                                              input$normalsd)))
    sample_B<-t(replicate(input$numsim,rnorm(input$samplen,
                                           input$normalmean[2],
                                           input$normalsd)))
    #sample_data<-sample_B-sample_A
    #dims<-dim(sample_data)
    save_mean<-c()
    save_t<-c()
    for(i in 1:input$numsim){
      save_mean[i]<-mean(sample_B[i,])-mean(sample_A[i,])
      save_t[i]<-t.test(sample_B[i,],sample_A[i,],var.equal=TRUE)$statistic
    }

    df<-data.frame(save_mean,save_t)
    return(df)
  })
   
  output$normPlot <-renderPlot({
    
    s<-input$normalsd
    m1<-input$normalmean[1]
    m2<-input$normalmean[2]
    
    ggplot()+
      geom_line(aes(x=seq((m1-(s*4)),(m1+(s*4)),.1),
                    y=dnorm(seq((m1-(s*4)),(m1+(s*4)),.1),m1,s)),
                color="#F8766D")+
      geom_line(aes(x=seq((m2-(s*4)),(m2+(s*4)),.1),
                    y=dnorm(seq((m2-(s*4)),(m2+(s*4)),.1),m2,s)),
                color="#00BFC4")+
      theme_classic(base_size = 20)+
      xlab("values")+
      ylab("density")+
      ggtitle("Normal populations")
  })
  
  output$samplePlot <-renderPlot({

  
    p_df<-exampleSample()
    
    ggplot(p_df, aes(x=values,group=sample,fill=sample))+
      geom_histogram(alpha=.4,color="white", position="identity")+
      geom_vline(xintercept=mean(p_df[p_df$sample=="A",]$values), 
                 color="#F8766D",
                 size=2,
                 linetype=2)+
      geom_vline(xintercept=mean(p_df[p_df$sample=="B",]$values),
                 color="#00BFC4",
                 size=2,
                 linetype=2)+
      theme_classic(base_size = 20)+
      xlab("values")+
      ggtitle("Two samples")
  })
  
  
  
    output$diffPlot <-renderPlot({
    
    dfa<-randomVals()
    
    ggplot(dfa,aes(x=save_mean))+
      geom_histogram(color="white", bins=40)+
      theme_classic(base_size = 20)+
      xlab("mean difference")+
      ggtitle("Mean differences")
  })
  
  output$tPlot <-renderPlot({
    
    dfb<-randomVals()
    dfnullt<-rt(input$numsim,(2*input$samplen)-2)
    typeB<-as.factor(rep(c("null","alt"),each=input$numsim))
    dfplot2<-data.frame(typeB,
                       values=c(dfnullt,dfb$save_t))
    
    ggplot(dfplot2,aes(x=values,group=typeB,fill=typeB))+
      geom_histogram(bins=40, alpha=.5, position="identity")+
      geom_vline(xintercept=qt(.95,(input$samplen-2)), 
                 color="red",
                 size=2)+
      theme_classic(base_size = 20)+
      xlab("t-value")+
      ggtitle("t-values")
    #  geom_label(data = data.frame(x = qt(.95,((2*input$samplen)-2)), 
    #                               y = 0, 
    #                               label = "Critical t"), 
     #            aes(x = x, y = y, label = label))
  })
  
  output$criticalt <- renderText({ 
    paste(c("Critical t (one-tailed) = "),round(qt(.95,((2*input$samplen)-2)),digits=3))
  })
  
  output$sampleStat <- renderText({ 
    p_df<-exampleSample()
    t_out<-t.test(p_df[p_df$sample=="B",]$values,p_df[p_df$sample=="A",]$values, var.equal=TRUE, alternative="greater")
    print(t_out)
    paste(c("Mean difference = "),
                round(as.numeric(t_out$estimate[1]),digits=2)-
            round(as.numeric(t_out$estimate[2]),digits=2),
          "\n \n",
          " t(",(2*input$samplen)-2,") = ",
          round(t_out$statistic,digits=2), "\n\n",
          " p = ",
          round(t_out$p.value,digits=3)
          )
    
    #paste(c("Mean difference = "),
    #      round(t_out$estimate,digits=2), "\n\n",
    #      " t(",(2*input$samplen)-2,") = ",
    #      round(t_out$statistic,digits=2), "\n\n",
    #      " p = ",
    #      round(t_out$p.value,digits=3))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

