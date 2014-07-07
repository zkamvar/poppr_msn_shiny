
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(poppr)

get_dist <- function(indist){
  indist <- switch(indist,
      Dissimilarity = "diss.dist",
      Bruvo         = "bruvo.dist",
      Nei           = "nei.dist",
      Rogers        = "rogers.dist",
      Edwards       = "edwards.dist",
      Provesti      = "provesti.dist",
      Reynolds      = "reynolds.dist"
  )
  return(indist)
}

make_dput <- function(x){
  if (length(x) > 1){
    x <- paste0("c(\"",paste0(x, collapse = "\", \""),"\")")
  } else if (length(x) > 0){
    x <- paste0("\"", x, "\"")
  } else {
    x <- "NULL"
  }
  return(x)
}

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

  distfun <- reactive({ 
    get_dist(input$distance) 
  })


  minspan <- reactive({
    indist <- distfun()
    DIST   <- match.fun(indist)
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
    print(cutoff)
    cutoff
  })
  
  distcmd <- reactive({
    dat <- input$dataset
    distfunk <- distfun()
    closer   <- ")"
    if (distfunk == "diss.dist"){
      distfunk <- paste0("poppr.msn(", dat, ", ", distfunk)
      closer   <- ", percent = FALSE))"
    } else if (distfunk == "bruvo.dist"){
      distfunk <- "bruvo.msn"
    } else { 
      distfunk <- paste0("poppr.msn(", dat, ", ", distfunk, "(missingno")
      closer   <- paste0(", type = 'mean')", closer, ")")
    }
    return(paste0(distfunk, "(", dat, closer))
  })
  
  cmd <- reactive({
    dat <- input$dataset
    paste0("plot_poppr_msn(", dat, 
           ",\n\t       min_span_net", 
           ",\n\t       inds = ", make_dput(inds()), 
           ",\n\t       gadj = ", input$greyslide,
           ",\n\t       palette = ", make_dput(input$pal),
           ",\n\t       cutoff = ", input$cutoff,
           ",\n\t       quantiles = FALSE",")")
  })

  output$summary <- renderPrint({
    dat <- dataset()
    show(dat)
  })

  output$plot <- renderPlot({
    set.seed(seed())
    plot_poppr_msn(dataset(), minspan(), ind = inds(), gadj = slide(), palette = usrPal(), 
                   cutoff = cutoff(), quantiles = FALSE, beforecut = TRUE)
  })
  
  output$cmd <- renderPrint({
    cat(paste0("min_span_net <- ", distcmd(), "\n"))
    cat(paste0("set.seed(", seed(),")\n"))
    cat(cmd())
  })

})
