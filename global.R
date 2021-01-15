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
library(shinyAce)
library(formatR)
library(shinyhelper)
# library(rintrojs)


options(shiny.maxRequestSize=2*1024^3,
        shiny.reactlog=TRUE,
        width = 105)


# create a temp folder
userdir <- tempfile()
dir.create(userdir, recursive = TRUE)
sapply(file.path(userdir, dir(userdir)[grep("code_", dir(userdir))]), file.remove)

# cat(fs::path_wd())

source("R/global_helper.R", local = TRUE)

if (!exists("shiny.tt")) {
  shiny.tt <- tibble()
  NOSHINY_TT <- TRUE
  # LOADED_SHINY_TT <- TRUE
} else {
  NOSHINY_TT <- FALSE
  # LOADED_SHINY_TT <- FALSE
}

init_code_all_R(userdir, NOSHINY_TT)

if(!NOSHINY_TT) {
  if(!assertthat::has_name(shiny.tt, "type")) { shiny.tt$type <- 'NA' }
  if(!assertthat::has_name(shiny.tt, "comments")) { shiny.tt$comments <- '' }

}


shiny.r <- reactiveValues(data = shiny.tt)
dataPar <- data_CheckPar(isolate(shiny.r$data))

global_sig1 <- c()
global_sig2 <- c()

onStop(function() {
  rm(shiny.r, envir = globalenv())
  rm(NOSHINY_TT, envir = globalenv())
  rm(dataPar, envir = globalenv())
  rm(global_sig1, envir = globalenv())
  rm(global_sig2, envir = globalenv())
  if(nrow(shiny.tt) == 0) { rm(shiny.tt, envir = globalenv()) }
})














