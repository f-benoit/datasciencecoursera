library(shiny)
library(ggplot2)
library(mixtools) 
library(MASS)
library(rsconnect)
rsconnect::setAccountInfo(name='benoitfedit',
                          token='CA2FD3FD4DF29E69ECBF45824DA40603',
                          secret='FSlBZkamfytpNvVXfMU93LWDdkdyMta28zKJQ0Jl')

nterm <- c(1,2,3,4,5,6,7,8,9,10)

fluidPage(

  titlePanel("Plot a best fit line on a 3 polynomial model"),

  sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min=20, max=1000,
                value=min(20, 1000), step=5, round=0),
    selectInput('degree', 'polydegree', nterm,nterm[[3]])
  ),

  mainPanel(
    plotOutput('plot')

  )
)

#deployApp(appName = "myapp")
