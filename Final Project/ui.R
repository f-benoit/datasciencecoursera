#
#This is the User interface for the "Predict the Next WOrd Application"
# 
#   
#

#Loading necessary Pakcages
library(shiny)
library(shinythemes)
library(rsconnect)
rsconnect::setAccountInfo(name='benoitfedit',
                          token='CA2FD3FD4DF29E69ECBF45824DA40603',
                          secret='FSlBZkamfytpNvVXfMU93LWDdkdyMta28zKJQ0Jl')
#Define Fluid Page
shinyUI(fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: #dec4de;
         }
         body, label, input, button, select { 
         font-family: "Arial";
         }')
  )),
  # Application title
  titlePanel("Predict the next word based on Swiftkey corpus by B.Fedit"),

                      # Sidebar layout with an option to enter text and description of how to use the application
                      sidebarLayout(
                        sidebarPanel(id="sidebar",
                          textInput(inputId="text1", label = "Type you sentence here", value =""),
                          h4("Instruction", style = "color:brown"),
                          p("1. Wait for the Swiftkey dataset to be loaded (around a minute)"),
                          p("2. Type your sentence"),
                          p("3. If your sentence contains more than 3 words, the model will use only the last 3 words to predict the next word"),
                          p("4  This is a naive algorithm based on the frequence of the words appearing in the SwiftKey corpus"),
                          h4("Drawback", style = "color:brown"),
                          p("1  For performance reason I'm only using only 15,000 lines of the total swiftkey dataset"),
                          h4("Future Improvements", style = "color:brown"),
                          p("1  Alow the app to track user sentences and add more words into the corpus"),
                          p("2  Allow the user to validate the prediction hence helps the model to learn")
                        ),
                      
                        
                        # Main pannel to show the predicted word
                        mainPanel(
                          
                          h1( " "),
                          h1( " "),
                          h1( " "),
                          h1( " "),
                          h1( " "),
                          htmlOutput("html1")
                        )
                      )
            
             # end of "Read Me" tabPanel

))