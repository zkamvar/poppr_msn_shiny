
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
  return(capture.output(dput(x)))
}

is_usable <- function(object, objclass = c("genind", "genclone")){
  any(objclass %in% class(get(object, .GlobalEnv)))
}


get_globals <- function(objclass = c("genind", "genclone")){
  myobjs <- ls(envir = .GlobalEnv)
  if (length(myobjs) == 0) return(myobjs)
  gens <- vapply(myobjs, FUN = is_usable, FUN.VALUE = logical(1), objclass)
  myobjs[gens]
}


globals <- get_globals()

shinyServer(function(input, output, session) {
  
  # Module to add the user's global environment. 
  output$selectUI <- renderUI({
    selectInput("dataset", 
                "choose dataset",
                choices = c("Example: Pinf",
                            "Example: partial_clone",
                            "Example: Aeut",
                            "Example: nancycats",
                            "Example: microbov",
                            "Example: H3N2",
                            globals),
                selected = "Example: partial_clone"
    )
  })
  
  dataset <- reactive({
    if (!is.null(input$dataset) && !grepl("<choose>", input$dataset)){
      if(grepl("Example: ", input$dataset)){
        env <- new.env()
        if (input$dataset == "Example: microbov"){ 
          data("microbov", package="adegenet", envir = env) 
        }
        else if (input$dataset == "Example: nancycats"){ 
          data("nancycats", package="adegenet", envir = env) 
        }
        else if (input$dataset == "Example: H3N2"){ 
          data("H3N2", package="adegenet", envir = env) 
        }
        else if (input$dataset == "Example: partial_clone"){ 
          data("partial_clone", package="poppr", envir = env) 
        }
        else if (input$dataset == "Example: Aeut"){ 
          data("Aeut", package="poppr", envir = env) 
        }
        else if (input$dataset == "Example: Pinf"){ 
          data("Pinf", package="poppr", envir = env) 
        }
        exam <- substr(input$dataset, start = 10, stop = nchar(input$dataset))
        dat <- get(exam, envir = env)
      } else {
        dat <- get(input$dataset, envir = .GlobalEnv)
      }
    } else {
      dat <- new("genind")
    }
    if (input$genclone) dat <- as.genclone(dat)
    return(dat)
  })

  
  dataname <- reactive({
    if (!grepl("<choose>", input$dataset)){
      if(grepl("Example: ", input$dataset)){
        dat <- substr(input$dataset, start = 10, stop = nchar(input$dataset))
      } else {
        dat <- input$dataset
      }
    } else {
      dat <- "no data"
    }
    return(dat)
  })
  
  distfun <- reactive({ 
    get_dist(input$distance) 
  })

  output$distargsUI <- renderUI({
    the_args <- formals(distfun())[-1]
    the_args <- paste(names(the_args), unlist(the_args), sep = " = ", 
                      collapse = ", ")
    textInput("distargs", label = "Distance arguments", the_args)
  })

  reticulation <- reactive({
    input$reticulate
  })
  
  
  distargs <- reactive({
    input$distargs
  })
  
  minspan <- reactive({
    indist <- distfun()
    ret    <- reticulation()
    args   <- distargs()
    if (indist == "bruvo.dist"){
      fun <- paste0("bruvo.msn(dataset(), ", args, ", showplot = FALSE, include.ties = ret)")
      out <- eval(parse(text = fun))
    } else {
      if (indist != "diss.dist"){
        dat <- missingno(dataset(), "mean")
      } else {
        dat <- dataset()
      }
      if (length(args) == 1 && args == ""){
        fun <- paste0(indist, "(dat)")
      } else {
        fun <- paste0(indist, "(dat, ", args, ")")
      }
      dist <- eval(parse(text = fun))
      out <- poppr.msn(dataset(), dist, showplot = FALSE, include.ties = ret)
    }
    return(out)

  })

  slide <- reactive({
    input$setGrey
    isolate({
      return(input$greyslide)
    })
  })
  
  seed <- reactive({
    input$seed
  })

  nodebase <- reactive({
    input$setNode
    isolate({
      return(input$nodebase)
    })
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
  
  distcmd <- reactive({
    dat      <- dataname()
    distfunk <- distfun()
    args     <- distargs()
    closer   <- paste0("showplot = FALSE, include.ties = ", reticulation(), ")")

    if (distfunk == "bruvo.dist"){
      distfunk <- "bruvo.msn"
      closer <- paste0(args, ", ", closer)
      return_cmd <- paste0(distfunk, "(", dat, closer)
    } else { 
      missfunk <- paste0("missingno(", dat, ", type = 'mean')\n")
      distfunk <- paste0(distfunk, "(", dat, "_nomiss, ", args, ")\n")
      msnfunk <- paste0("poppr.msn(", dat, ", ", dat, "_dist, ", closer, "\n")
      return_cmd <- paste0(dat, "_nomiss <- ", missfunk, 
                           dat, "_dist <- ", distfunk,
                           "min_span_net <- ", msnfunk)
    }
    return(return_cmd)
  })
  
  cmd <- reactive({
    dat <- dataname()
    paste0("plot_poppr_msn(", dat, 
           ",\n\t       min_span_net", 
           ",\n\t       inds = ", make_dput(inds()), 
           ",\n\t       gadj = ", input$greyslide,
           ",\n\t       palette = ", make_dput(input$pal),
           ",\n\t       cutoff = ", ifelse(is.null(cutoff()), "NULL", cutoff()),
           ",\n\t       quantiles = FALSE",
           ",\n\t       beforecut = ", bcut(), ")")
  })

  bcut <- reactive({
    input$beforecut
  })

  output$summary <- renderPrint({
    dat <- dataset()
    show(dat)
  })

  output$plot <- renderPlot({
    set.seed(seed())
    plot_poppr_msn(dataset(), minspan(), ind = inds(), gadj = slide(), 
                   palette = usrPal(), cutoff = cutoff(), quantiles = FALSE, 
                   beforecut = bcut(), nodebase = nodebase())
  })
  
  output$cmd <- renderPrint({
    cat(paste0(distcmd(), "\n"))
    cat(paste0("set.seed(", seed(),")\n"))
    cat(cmd())
  })

})
