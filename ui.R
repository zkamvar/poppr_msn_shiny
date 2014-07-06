
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Minimum spanning networks in poppr"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", 
                  "choose dataset",
                  choices = c("Pinf",
                              "partial_clone", 
                              "Aeut", 
                              "nancycats", 
                              "microbov",
                              "H3N2")
                  ),
      checkboxInput("genclone", "convert to genclone?", TRUE),
      
      selectInput("distance", 
                  "choose distance calculation", 
                  choices = c("dissimilarity",
                              "Bruvo",
                              "Nei",
                              "Rogers",
                              "Edwards",
                              "Provesti",
                              "Reynolds")
                  )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("plot")
    )
  )
))
