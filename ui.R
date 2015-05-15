
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(paste("Minimum spanning networks in", "poppr")),

  sidebarLayout(
    sidebarPanel(
      tags$h5("status"),
      conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
        tagAppendChild(tags$div(class="progress"),
        tagAppendChild(tags$div(class="progress-bar progress-bar-success", 
                       role="progressbar", `aria-valuenow`="100", 
                       `aria-valuemin`="0", `aria-valuemax`="100", 
                       style="width: 100%"), tags$strong("ready")))
      ),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
        tagAppendChild(tags$div(class="progress"),
        tagAppendChild(tags$div(class="progress-bar progress-bar-striped active", 
                       role="progressbar", `aria-valuenow`="100", 
                       `aria-valuemin`="0", `aria-valuemax`="100", 
                       style="width: 100%"), tags$strong("loading")))
      ),
      hr(),
      uiOutput("selectUI"),
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
      conditionalPanel("input.distance == 'Bruvo'",
        selectInput("bruvo_model",
                    "Select a model for missing data",
                    choices = c("Genome Addition",
                                "Genome Loss",
                                "Infinite",
                                "Average Addition/Loss"),
                    selected = "Average Addition/Loss"),
        textInput("replen", "SSR repeat lengths (comma separated)", "1, 2, 3")
      ),
      conditionalPanel("input.distance != 'Bruvo'",
        uiOutput("distargsUI")
      ),
      checkboxInput("reticulate", "include reticulations?", TRUE), 

      actionButton("submitdist", "Calculate MSN", icon("gears")),
      
      checkboxInput("pop.leg", "population legend", TRUE), 
      checkboxInput("scale.leg", "scale bar", TRUE), 
      sliderInput("greyslide",
                  "Grey Scale",
                  min = 0,
                  max = 25,
                  value = 3,
                  step = 1
      ),
      numericInput("nodebase",
                   "Node Size Scale (log(size, value))",
                   "1.15", 
                   min = 1.0001,
                   step = 0.0001),
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
      checkboxInput("beforecut", "Keep graph position", TRUE),
      actionButton("submit", "Show My Graph!", icon("check-circle"))
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
