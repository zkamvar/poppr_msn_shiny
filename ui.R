
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(paste("Minimum spanning networks in", "poppr")),

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
                  choices = c("Dissimilarity",
                              "Bruvo",
                              "Nei",
                              "Rogers",
                              "Edwards",
                              "Provesti",
                              "Reynolds")
      ),
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
        )
    ),


    mainPanel(
      tabsetPanel(
          tabPanel("Plot", plotOutput("plot"), verbatimTextOutput("cmd")),
          tabPanel("Summary", verbatimTextOutput("summary"))
        )
    )
  )
))
