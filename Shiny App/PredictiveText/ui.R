#Leigh Matthews
#January 31, 2018
#Coursera Data Science Capstone - Shiny App
#Predictive Text 

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

library(shiny)

shinyUI(
  fixedPage(
    column(width = 10, offset = 1,
      titlePanel("Data Science Capstone Shiny App - Predictive Text"),
      wellPanel("Predictive Text Analysis uses Natural Language Process.
                The goal is to use the text that has already been typed to 
                predict the next word, commonly known as auto-complete.
                As you type, auto-complete gives several possible options 
                based on probable words that come after the last word or phrase.
                The R package \"wordprediction\" allows this functionality."),
      fixedRow(
        sidebarPanel(
          span(textInput("phrase", "Text Input: (Enter phrase ommitting last word)", value = ""),
            actionButton("predictButton", "Predict"))
        ),
        mainPanel(
          strong("Text input Analyzed"),
          textOutput("Phrase"),
          strong("Prediction:"),
          textOutput("word") )
      )
    )
  )
)