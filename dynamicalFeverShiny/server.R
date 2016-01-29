library(shiny)
#rm(list=ls())                   # Clear all variables and functions from the workspace
#load('dynamicalFeverData.Rdata') # Load model functions and output

# Define server logic required to draw a histogram
shinyServer({
  
  function(input, output) {
    output$targetPlot <- renderPlot({
      target.2016 <- run.example(input$VaxPct.Dogs,input$VaxPct.Humans)
      par(mar=c(5,5,5,1),mfrow=c(1,2)) # Set up plot
      plot.cases(target.2016)
    })
    output$distPlot <- renderPlot({
      target.runs <- replicate(1000,total.cases(run.example(input$VaxPct.Dogs,input$VaxPct.Humans)))
      par(mar=c(5,5,5,1),mfrow=c(1,2)) # Set up plot
      hist(target.runs['Dogs',], col='dark grey',
           main='Dogs',
           xlab='Number of canine cases',
           ylab='Number of runs')
      
      hist(target.runs['People',], col='dark grey',
           main='People',
           xlab='Number of human cases',
           ylab='Number of runs')
      #     plot(target.runs['Dogs',],target.runs['People',],
      #          main='For each of 1000 runs',
      #          xlab='Number of canine cases',
      #          ylab='Number of human cases')
    })
    
  }})
