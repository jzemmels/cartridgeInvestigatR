# https://annielyu.com/2020/02/04/viscover-shiny/


library(shinydashboard)
library(shinyFiles)
library(shiny)
library(rgl)
library(ggplot2)
library(x3ptools)
library(dplyr)
library(bulletxtrctr)
library(shinybusy)


options(shiny.maxRequestSize=2*1024^3,
        shiny.reactlog=TRUE)
options()

if (!exists("shiny.tt")) {
  shiny.tt <- tibble()
  NOSHINY_TT <- TRUE
  # LOADED_SHINY_TT <- TRUE
} else {
  NOSHINY_TT <- FALSE
  # LOADED_SHINY_TT <- FALSE
}

shiny.r <- reactiveValues(data = shiny.tt)

onStop(function() {
  rm(shiny.r, envir = globalenv())
  rm(NOSHINY_TT, envir = globalenv())
  if(nrow(shiny.tt) == 0) { rm(shiny.tt, envir = globalenv()) }
})