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
  
  observeEvent(input$info,{
    
    showModal(modalDialog(
      title = h3("Welcome to cartridgeInvestigatR!"),
      easyClose = TRUE,
      h4("Use this app to compare 3D topographical scans of cartridge cases."),
      h4("Click the 'Help' button on each tab to learn about its functionality."),
      # h4("Scroll to the bottom of each page to see a Tutorial of how to use the page."),
      h4("See below for more information or visit ",HTML('<a href="https://github.com/jzemmels/cartridgeInvestigatR">https://github.com/jzemmels/cartridgeInvestigatR</a>'),"."),
      # br(),
      h2(strong("Background")),
      h4("This application uses computer algorithms to process and compare scans of cartridge cases.",
         'A', tags$i("cartridge case"),' is the metal casing that houses the bullet and gunpowder prior to firing.',
         'When a gun is fired, as the bullet moves down the barrel, the cartridge case moves backwards and slams against the back wall of the barrel (the ',tags$i("breech face"),') with considerable force.',
         'Any markings on the breech face are "stamped" into the surface of the cartridge case.',
         'This leaves so-called ',HTML("<i><a href='https://www.firearmsid.com/A_CCIDImpres.htm'>breech face impressions</a></i>"),' that forensic examiners use to identify the gun from which a cartridge case was fired.',
         "Think of these impressions as analogous to a gun's",'"fingerprint" left on the cartridge case.',
         "The computer algorithms used in this app compare the breech face impressions on two cartridge cases."),
      # br(),
      h2(strong("About this app")),
      h4("This app allows individuals to engage with cartridge case comparison algorithms without needing to program.",
         "If you are interested in cartridge case identification, but do not have expertise in the R programming language, then this app is for you.",
         "For more information about the computer algorithms used in this application, visit ",HTML("<a href='https://jzemmels.github.io/research.html'>https://jzemmels.github.io/research.html</a>"),"."),
      h4("To use this app, you must have cartridge case scans stored on your computer as ",HTML("<a href='https://tsapps.nist.gov/NRBTD/Home/DataFormat'>.x3p files</a>"),".",
         "An X3P (XML 3D Surface Profile) is an ISO standard file format for saving cartridge case scans.",
         "You can download example cartridge case .x3p files from the ",HTML("<a href='https://tsapps.nist.gov/NRBTD/'>NIST Ballistics Toolmark Research Database</a>.")),
      h4("The functionality of this app encompasses three stages of the cartridge case comparison procedure: Pre-processing, Exploring, and Scoring.",
         "These stages are separated into the three tabs that you can see on the left sidebar.",
         "You must complete the Import + Pre-processing stage before moving on to Explore or Score."),
      # br(),
      h3(strong("Basic workflow")),
      HTML("<div class='row' align='center'> <img src='workflowDiagram.png' alt='Workflow Diagram' width='500'> </div>"),
      # tags$p(tags$img(src = "workflowDiagram.png",width = "30%",class = "text-align:center")),
      h4("To start, make sure that you have cartridge case scans stored as x3p files on your computer."),
      h4("If you would simply like to explore this app, you can find example scans in the home directory of this app."),
      # br(),
      h3(strong("Import + Pre-process")),
      h4("Use the 'Import + Pre-process' tab to upload scans to the app.",
         "Click the 'Select a folder containing x3p files' to upload scans.",
         'You may need to pre-process scans to highlight the breech face impressions prior to comparison.',
         "Note that the example scans in the home directory have already been pre-processed to some extent, but you're welcome to experiment with additional pre-processing steps."),
      # br(),
      h4(strong("If your scans require pre-processing:")),
      h4("The 'Automatic Pre-process' tab allows you to string-together algorithms to automatically pre-process your scans.",
         "Click the 'Add Another Pre-processing Step' to add a pre-processing algorithm to the sequence.",
         "You can also select parameters for the selected pre-processing algorithm.",
         "Click 'Perform Automatic Pre-processing' once you're happy with the pre-processing sequence or 'Reset All Pre-processing Steps' to start over."),
      h4("A pre-processing sequence that we have found to work well for many unprocessed scans is as follows:"),
      h4(HTML("<ol>"),
         HTML("<li>"),"Crop with parameters Region set to 'Interior' and Offset set to a positive value (try about 10% of the dimensions of the scan).",HTML("</li>"),
         HTML("<li>"),"Crop with parameters Region set to 'Exterior' and Offset set to a negative value (try about 30% of the dimensions of the scan).",HTML("</li>"),
         HTML("<li>"),"Level with parameter Statistic set to 'Median' (the default).",HTML("</li>"),
         HTML("<li>"),"Filter with paramaters Filtertype set to 'Bandpass' and Wavelength(s) set to '16, 500' (the defaults).",HTML("</li>"),
         HTML("<li>"),"Downsample with parameter Stride set to '2' (the default).",HTML("</li>"),
         HTML("</ol>")),
      h4("The Erode step with the Radius parameter set to a positive value will 'shave off' the interior/exterior of scans.",
         "The Delete step is useful if your uploaded scans include masks identifying regions that you would like to remove.",
         "We encourage you to experiment with different pre-processing steps and orders."),
      # br(),
      h4("Move to the 'Manual Deletion' tab to remove specific regions from scans."),
      h4(strong("After your scans are pre-processed")),
      h4("Press the 'I would like to compare these scans' button at any time to complete the Import + Pre-process phase.
         Once completed, you cannot return to the Import + Pre-process phase and will need to restart the app if you decide additional pre-processing is needed.
         You can next move on to the Explore or Score phases."),
      h3(strong("Explore")),
      h4("In the 'Cell Grid Comparison' tab, select a reference and target scan to compare.",
         "If you're simply exploring the app, we have chosen default parameter settings that work well.",
         "However, we encourage experimentation if you're interested.",
         "Click on the 'Perform Comparison' button at the bottom of the sidebar once you're happy with the parameters.",
         "Once the comparison finishes (the loading bar vanishes), you can assess the comparison results using the 'Results Summary' or 'Individual Cell Results' tabs."),
      h4("In the 'Results Summary' tab, you can select a scan from the dropdown.",
         "The plots that appear depict the similarity features extracted during the cell-based comparison procedure.",),
      h4("In the 'Individual Cell Results' tab, you can select a scan from the dropdown.",
         "Click on a cell in the plot that appears to visualize its alignment in the compared scan.",
         "A comparison plot appears at the bottom of the page that shows similarities and differences between the selected cell and the region to which it aligns in the other scan."),
      h4("Finally, the 'Custom Cell' tab allows you to draw your own cell (either rectangular or hand-drawn) on a scan and compare this it to another scan.",
         "This tab can be used to analyze how a specific region of interest aligns in another scan.",
         "All of these plots are interactable: clicking on one element will highlight elements in the other plots corresponding to the same cell."),
      h3(strong("Score")),
      h4("Select a reference and target scan to compare.",
         "Use the 'ACES Algorithm' tab to apply the Automatic Cartridge Evidence Scoring algorithm that computes the probability that the two scans match.",
         "Use the 'Congruent Matching Cells' tab to apply Congruent Matching Cells algorithm."),
      h3(strong("Export")),
      h4("Use this tab to export scans or results computed in the app.",
         "You can export results as Comma-Separated Variable (.csv) or .RData files.",
         "You can also download an R script to reproduce the comparison results."),
      h2(strong("References")),
      h4("Zheng, X., Soons, J., Thompson, R., Singh, S., & Constantin, C. (2020). NIST Ballistics Toolmark Research Database. In Journal of Research of the National Institute of Standards and Technology (Vol. 125). National Institute of Standards and Technology (NIST). https://doi.org/10.6028/jres.125.004 "),
      h4("J. Song. Proposed “NIST Ballistics Identification System (NBIS)” Based on 3D Topography Measurements on Correlation Cells. American Firearm and Tool Mark Examiners Journal, 45(2):11, 2013. URL https://tsapps.nist.gov/publication/get_pdf.cfm?pub_id=910868.")
    ))
    
  })
  
  
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
  volumes <- c(Home = "data", "R Installation" = R.home(), shinyFiles::getVolumes()())
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
