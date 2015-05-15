
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
      tagAppendChildren(
        tags$div(style="display:inline-block"),
        list(
          actionButton("submit", "Show My Graph!", icon("check-circle")),
          actionButton("update-data", "Update Data", icon("refresh")),
          actionButton("update-graph", "Update Graph", icon("refresh"))
        )
      ),

      uiOutput("selectUI"),
      uiOutput("selectPops"),
      
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
        textInput("replen", "SSR repeat lengths\n(comma separated or a valid R expression)", "1, 2, 3")
      ),
      conditionalPanel("input.distance != 'Bruvo'",
        uiOutput("distargsUI")
      ),
      checkboxInput("reticulate", "include reticulations?", TRUE), 
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
