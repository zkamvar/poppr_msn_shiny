
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
      "Dissimilarity" = indist <- "diss.dist",
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
        dist <- DIST(dataset())
      } else {
        dat  <- missingno(dataset(), "mean")
        dist <- DIST(dat)
      }
      out <- poppr.msn(dataset(), dist, showplot = FALSE)
    }
    return(out)

  })

  slide <- reactive({
    input$greyslide
  })
  
  seed <- reactive({
    input$seed
  })

  inds <- reactive({
    return(strsplit(input$inds, "[[:blank:]]")[[1]])
  })
  
  usrPal <- reactive({
    input$pal
  })
  
  cutoff <- reactive({
    cutoff <- as.numeric(input$cutoff)
    if (is.na(cutoff)) cutoff <- NULL
    cutoff
  })
  
  cmd <- reactive({
    dat <- input$dataset
    paste0("plot_poppr_msn(", dat, ", ", input$distance, "(", dat, "), ", ind = input$inds,", gadj = ", 
      input$slide,", palette = ",input$pal,", cutoff = ",input$cutoff,", quantiles = FALSE",")")
  })

  output$summary <- renderPrint({
    dat <- dataset()
    show(dat)
  })

  output$plot <- renderPlot({
    set.seed(seed())
    plot_poppr_msn(dataset(), minspan(), ind = inds(), gadj = slide(), palette = usrPal(), 
                   cutoff = cutoff(), quantiles = FALSE)
  })
  
  output$cmd <- renderPrint({
    cat(paste0("set.seed(", seed(),")\n"))
    cat(paste0("plot_poppr_msn(", input$dataset, ")"))
  })

})
