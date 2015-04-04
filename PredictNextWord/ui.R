library(shiny)

shinyUI(fluidPage(
    titlePanel("Predict next word"),
    hr(),
    sidebarLayout(
        sidebarPanel(textInput("text", label = h3("Your phrase:"), value = "Enter your phrase here to see the next word on your"),
                     column(12, align="right",submitButton(" Predict ")),br(),hr(),h4("Help" ),
                    p("This application predicts the next word, based on the phrase in the input box."),
                    p('So enter the phrase in the input box, click "Predict" Button and wait few seconds to see the result in the main panel on the right. ')
                    ),
        mainPanel(h3("The next word is:"),h1(htmlOutput("next_word")),hr(),
                  htmlOutput("phrase1"),br(),htmlOutput("phrase2"),uiOutput("phrase3"),br(),column(8, align="center",tableOutput("table")))
    )
))