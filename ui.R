
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(paste("Minimum spanning networks in", "poppr")),

  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", 
                  "choose dataset",
                  choices = c("<choose>",
                              "Pinf",
                              "partial_clone", 
                              "Aeut", 
                              "nancycats", 
                              "microbov",
                              "H3N2"),
                  selected = "<choose>"
      ),
      textInput("userdata",
                "OR enter the name of your data",
                NULL),
      checkboxInput("genclone", "convert to genclone?", TRUE),
      actionButton("action", "Go!"),
      selectInput("distance", 
                  "choose distance calculation", 
                  choices = c("Dissimilarity",
                              "Bruvo",
                              "Nei",
                              "Rogers",
                              "Edwards",
                              "Provesti",
                              "Reynolds")
      ),
      checkboxInput("reticulate", "include reticulations?", TRUE), 
      sliderInput("greyslide",
                  "Adjust Grey Scale",
                  min = 0,
                  max = 25,
                  value = 3,
                  step = 1
      ),
      numericInput("seed", 
                   "Random Seed",
                   "69"
      ),
      textInput("inds",
                "Individuals to label (sample names separated by spaces)",
                "ALL"),
      selectInput("pal", "Indicate a color palette to be used",
                  choices=c("rainbow", 
                            "cm.colors", 
                            "topo.colors", 
                            "terrain.colors", 
                            "gray.colors",
                            "funky",
                            "spectral",
                            "seasun",
                            "azur",
                            "wasp")
      ),
      conditionalPanel("input.distance == 'Dissimilarity'",
        numericInput("cutoff", 
                     "Distance Cutoff",
                     NULL,
                     min = 2
        )
      ),
      conditionalPanel("input.distance != 'Dissimilarity'",
        numericInput("cutoff",
                     "Distance Cutoff",
                     NULL,
                     step = 0.001)
      ),
      checkboxInput("beforecut", "Keep graph position", TRUE)
    ),


    mainPanel(
      tabsetPanel(
          tabPanel("Plot", plotOutput("plot", height = '600px')),
          tabPanel("Data", verbatimTextOutput("summary")),
          tabPanel("Command", verbatimTextOutput("cmd"))
        )
    )
  )
))
