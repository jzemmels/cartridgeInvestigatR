library(patchwork)
library(tidyverse)

library(impressions)
library(cmcR)
library(scored)

library(shinyFiles)
library(shinybusy)
library(gganimate)
library(prettyunits)
library(progress)
library(ggiraph)
library(raster)
# remotes::install_github("https://github.com/thomasp85/shinyFiles.git")

server = function(input, output, session) {
  
  source("code/cartridgeInvestigatR_helpers.R",local = TRUE)
  source("code/preProcessFunction_helpers.R",local = TRUE)
  
  # hard resetting will take you back to the Import tab
  observeEvent(input$app_hardReset,{
    
    session$reload()
  })
  
  output$app_help_ui <- renderUI({
    
    req(input$stages)
    
    if(input$stages == "preprocessing"){
      
      # browser()
      
      req(input$preprocessingTabs)
      
      if(input$preprocessingTabs == "Import"){
        
        return(actionButton(inputId = "importHelp",
                            label = "Help",
                            icon = fontawesome::fa_i("question-circle")))
        
      }
      else if(input$preprocessingTabs == "Automatic Pre-process"){
        
        return(actionButton(inputId = "automaticPreprocessHelp",
                            label = "Help",
                            icon = fontawesome::fa_i("question-circle")))
        
      }
      else if(str_detect(input$preprocessingTabs,"Manual Deletion")){
        
        return(actionButton(inputId = "manualDeletionHelp",
                            label = "Help",
                            icon = fontawesome::fa_i("question-circle")))
        
      }
      
    }
    else if(input$stages == "comparing"){
      
      req(input$comparingTabs)
      
      if(input$comparingTabs == "Cell Grid Comparison"){
        
        return(actionButton(inputId = "comparisonParametersHelp",
                            label = "Help",
                            icon = fontawesome::fa_i("question-circle")))
        
      }
      else if(input$comparingTabs == "Results Summary"){
        
        return(actionButton(inputId = "comparisonResultsSummaryHelp",
                            label = "Help",
                            icon = fontawesome::fa_i("question-circle")))
        
      }
      else if(input$comparingTabs == "Individual Cell Results"){
        
        return(actionButton(inputId = "comparisonResultsIndividualCellsHelp",
                            label = "Help",
                            icon = fontawesome::fa_i("question-circle")))
        
      }
      else if(input$comparingTabs == "Custom Cell"){
        
        return(actionButton(inputId = "customCellHelp",
                            label = "Help",
                            icon = fontawesome::fa_i("question-circle")))
      }
      
    }
    else if(input$stages == "scoring"){
      
      if(input$scoringTabs == "ACES Algorithm"){
        
        return(actionButton(inputId = "acesAlgorithmHelp",
                            label = "Help",
                            icon = fontawesome::fa_i("question-circle")))
        
      }
      else if(input$scoringTabs == "Congruent Matching Cells"){
        
        return(actionButton(inputId = "congruentMatchingCellsHelp",
                            label = "Help",
                            icon = fontawesome::fa_i("question-circle")))
        
      }
    }
    else if(input$stages == "exporting"){
      
      return(actionButton(inputId = "exportHelp",
                          label = "Help",
                          icon = fontawesome::fa_i("question-circle")))
      
    }
    else{
      
      return(NULL)
      
    }
    
  })
  
  # upload from rds file
  volumes <- c(Home = fs::path_wd(), "R Installation" = R.home(), shinyFiles::getVolumes()())
  # shinyFileChoose(input, "file1", roots = volumes, session = session)
  
  # upload from x3p folder
  shinyFiles::shinyDirChoose(input, "x3pdir", roots = volumes, session = session, 
                             restrictions = system.file(package = "base"))
  
  
  ## Initialize the Preprocessing tab to show the import tab first 
  
  showTab(inputId = "preprocessingTabs",target = "Import",session = session)
  hideTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
  hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Select a Scan",session = session)
  hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Annotate",session = session)
  hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Preview Final Annotations",session = session)
  hideTab(inputId = "preprocessingTabs",target = "Pre-processing Completed",session = session)
  hideTab(inputId = "comparingTabs",target = "Results Summary",session = session)
  hideTab(inputId = "comparingTabs",target = "Individual Cell Results",session = session)
  # hideTab(inputId = "comparingTabs",target = "Custom Cell",session = session)
  
  
  # import interactivity of the Import tab
  source("code/importAndPreprocessMenu/importTab.R",local = TRUE)
  
  shiny.r <- list()
  
  source("code/importAndPreprocessMenu/automaticPreprocessTab.R",local = TRUE)
  
  source("code/importAndPreprocessMenu/manualDeletionTab.R",local = TRUE)
  
  ################ Compare menu code
  
  source("code/compareMenu/comparisonSettingsTab.R",local = TRUE)
  
  shinyjs::toggle(id = "comparisonSettingsMenu",anim = TRUE)
  shinyjs::toggle(id = "customCellMenu",anim = TRUE)
  
  source("code/compareMenu/resultsSummaryTab.R",local = TRUE)
  
  source("code/compareMenu/individualCellResultsTab.R",local = TRUE)
  
  # source("code/compareMenu/cellTrajectoriesTab.R",local = TRUE)
  
  source("code/compareMenu/customCellTab.R",local = TRUE)
  
  ################################# Score menu code
  
  source("code/scoreMenu/cmcMethodTab.R",local = TRUE)
  source("code/scoreMenu/acesAlgorithmTab.R",local = TRUE)
  
  observeEvent(list(input$score_referenceSelect,input$score_targetSelect),{
    
    if(input$score_referenceSelect != "" & input$score_targetSelect != ""){
      
      shinyjs::enable(id = "score_previewScans")
      
    }
    else{
      
      shinyjs::disable(id = "score_previewScans")
      
    }
    
  })
  
  # Update sidebar UI depending on if ACES or CMC methods are selected
  observeEvent(input$scoringTabs,{
    
    req(input$scoringTabs)
    
    if(input$scoringTabs == "Congruent Matching Cells"){
      
      output$scoreSettings_ui <- renderUI({
        
        ret <- tagList(
          # uiOutput(outputId = "cmcTab_useExploreResults_ui"),
          actionButton(inputId = "cmcTabSettings_button",
                       icon = fontawesome::fa_i("cog"),
                       label = "Settings"),
          br(),
          shinyjs::hidden(div(id = "cmcTabMenu",
                              textInput(inputId = "cmcTab_numCells",
                                        label = "Cells Grid Size (use comma separation)",
                                        value = "4,4"),
                              bsTooltip("cmcTab_numCells",title = "Enter number of cells in grid"),
                              numericInput(inputId = "cmcTab_maxNonMissingProp",
                                           label = "Maximum Proportion of NAs per Cell",
                                           value = .99,
                                           min = .0001,
                                           max = .9999),
                              bsTooltip("cmcTab_maxNonMissingProp",title = "Enter maxmimum proportion that can be missing in a cell to be considered in comparison"),
                              numericInput(inputId = "cmcTab_cellRegionProp",
                                           label = "Sidelength ratio between target regions and reference cells",
                                           value = 2,min = 1),
                              numericInput(inputId = "cmcTab_thetaRangeMin",
                                           label = "Minimum Rotation Value (degrees)",
                                           min = -181,
                                           max = 181,
                                           value = -30),
                              bsTooltip("cmcTab_thetaRangeMin",title = "Enter minimum rotation to be considered (between -180 and 180 degrees)"),
                              numericInput(inputId = "cmcTab_thetaRangeMax",
                                           label = "Maximum Rotation Value (degrees)",
                                           min = -181,
                                           max = 181,
                                           value = 30),
                              bsTooltip("cmcTab_thetaRangeMax",title = "Enter maximum rotation to be considered (greater than minimum, less than 180 degrees)"),
                              numericInput(inputId = "cmcTab_thetaStep",
                                           label = "Rotation Step Size (degrees)",
                                           value = 3,
                                           step = 1,
                                           min = 1),
                              bsTooltip("cmcTab_thetaStep",title = "Enter distance  between consecutive rotations (in degrees)"))),
          br(),
          actionButton(inputId = "cmcPlotExecute",
                       style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                       icon = icon("play"),
                       label = "Compute CMCs")
        )
        
        return(ret)
        
      })
    }
    
    if(input$scoringTabs == "ACES Algorithm"){
      
      shinyjs::enable(id = "score_scanPreview_ui")
      
      output$scoreSettings_ui <- renderUI({
        
        ret <- tagList(#br(),
          actionButton(inputId = "acesCalculate",
                       style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                       icon = icon("play"),
                       label = "Estimate Match Probability"))
        
        return(ret)
      })
      
    }
    
    
  })
  
  source("code/exportMenu/exportTab.R",local = TRUE)
  
}
