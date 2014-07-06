
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(poppr)

shinyServer(function(input, output) {
  
  dataset <- reactive({
    if (input$dataset == "microbov") data("microbov", package="adegenet")
    if (input$dataset == "nancycats") data("nancycats", package="adegenet")
    if (input$dataset == "H3N2") data("H3N2", package="adegenet")
    if (input$dataset == "partial_clone") data("partial_clone", package="poppr")
    if (input$dataset == "Aeut") data("Aeut", package="poppr")
    if (input$dataset == "Pinf") data("Pinf", package="poppr")
    dat <- get(input$dataset)
    if (input$genclone) dat <- as.genclone(dat)
    return(dat)
  })

  minspan <- reactive({
    indist <- input$distance
    switch(indist,
      "dissimilarity" = indist <- "diss.dist",
      "Bruvo" = indist <- "bruvo.dist",
      "Nei" = indist <- "nei.dist",
      "Rogers" = indist <- "rogers.dist",
      "Edwards" = indist <- "edwards.dist",
      "Provesti" = indist <- "provesti.dist",
      "Reynolds" = indist <- "reynolds.dist"
      )
    DIST <- match.fun(indist)
    if (indist == "bruvo.dist"){
      out <- bruvo.msn(dataset(), showplot = FALSE)
    } else {
      if (indist == "diss.dist"){
        DIST <- function(x) diss.dist(x, percent = FALSE)
      }
      dist <- DIST(dataset())
      out <- poppr.msn(dataset(), dist, showplot = FALSE)
    }
    return(out)

  })


  output$summary <- renderPrint({
    dat <- dataset()
    show(dat)
  })

  output$plot <- renderPlot({
    plot_poppr_msn(dataset(), minspan(), ind = "none")
  })

})
