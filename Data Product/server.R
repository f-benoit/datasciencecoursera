library(shiny)
library(ggplot2)

function(input, output) {
#
##  n<-reactive({ input$sampleSize  })
#size<-reactive({input$sampleSize})
#  #size<-100
#x <- reactive({sort(runif(size(), -2, 2))})
#y <- reactive({3*x()^3 + 5*x()^2 + 0.5*x() + 20}) # a 3 polynomial model
#err <- reactive({irnorm(size(), sd=3)})
#ye <- reactive({y + err})
#dataset <-  reactive({data.frame(x, ye) })

newData <- reactive({
  

 # isolate({
    
    size<-input$sampleSize
    #size<-100
    x <- sort(runif(size, -2, 2))
    y <- 3*x^3 + 5*x^2 + 0.5*x + 20 # a 3 polynomial model
    err <- rnorm(size, sd=3)
    ye <- y + err
    dataset <-  data.frame(x, ye) 
    
    
    
    
  #  datadata <- data
    
  #  datadata <- subset(datadata, rating %in% input$checkGroups)
    
    
  #})
  
})
testData <- reactive({
  df<-newData()
  testData <- data.frame(x=seq(min(df$x), max(df$x),length.out =  input$sampleSize))
})


#newdata2 <- reactive({
#  data.frame(x=seq(min(df$x), max(df$x),length.out = size()))
#})
#
#fit <- reactive({
#  lm(ye ~ poly(x, degree=input$degree), data=newdata2())
#})


  output$plot <- renderPlot({


   isolate({
  ggplot(newData(), aes(x, ye)) + 
        geom_point(alpha=2/10, shape=21, fill="blue", colour="black", size=5)+
        stat_smooth(method="lm", se=TRUE, fill=NA,
                    formula=y ~ poly(x, input$degree, raw=TRUE),colour="red")
})
  }, height=700)
  


}

