library(shiny)
shinyUI(fluidPage(
  titlePanel("Next Word Predictor App"),
  sidebarLayout(
    sidebarPanel(
    h5('Start entering your sentence. Please enter space after last word and wait for your results.'),
    textInput(inputId="text1", label = "Your Input", "This is a beautiful "),
    submitButton('Submit')
  ),
  mainPanel(
    h5('Your Possible Next Word'),
    textOutput('text1')
  )
)))
