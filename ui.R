library(shiny)

shinyUI(fluidPage(
  navbarPage("Text Prediction App",
             tabPanel("Prediction Panel",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Please insert some words below"),
                          h5("Bitte fuegen Sie einige Woerter unten ein"),
                          tags$textarea(id="text_input"),
                          
                          radioButtons("choices", label = h5(""),
                                       choices = list("One Word (Ein Wort)" = 1, 
                                                      "Two Words (Zwei Woerter)" = 2,
                                                      "Three Words (Drei Woerter)" = 3,
                                                      "Four Words (Vier Woerter)" = 4,
                                                      "Five Words (Fuenf Woerter)" = 5))),
                        mainPanel(
                          h4("Predicted word (Vorhergesagtes Wort)"),
                          verbatimTextOutput("predicted_word")
                          )
                      )
             ),
             tabPanel("Instructions Panel",
                      mainPanel(
                        h3("Instructions"),
                        h5("The App is very user friendly and it is used to predict up to 5 possible words usually
                            digited after any input sentence or single word.
                            The dataset used is in german and as a consequence the input word/sentence must
                            be written in german."),
                        strong("A space is necessary at the end of the input sentence!!!"),
                        h3("Anleitung"),
                        h5("Die App ist sehr user friendly und wird verwendet, um bis zu 5 moegliche Woerter
                            vorherzusagen, die normalerweise nach jedem Satz oder jedem Wort ausgedruckt werden.
                            Der verwendete Datensatz ist in Deutsch und als Folge muss das eingegebene Wort/Satz in
                            Deutsch geschrieben werden."),
                        strong("Am Ende des Eingabesatzes ist ein Leerzeichen erforderlich!!!")
                                              )
             )
  )
))