# show popup when the user clicks the Help button
observeEvent(input$exportHelp,
             {
               
               showModal(modalDialog(
                 title = "Help: Export Tab",easyClose = TRUE,
                 strong("Why would I use this tab?"),
                 "Export scans and results from the app",
                 br(),
                 br(),
                 strong("What do I need to do before using this tab?"),
                 "At the very least, upload scans to the app in the Import + Pre-process tab.",
                 "The export options change depending on what you've done in the app.",
                 "For example, you will be able to export results from the Score tab once you run the ACES or CMC algorithms.",
                 br(),
                 br(),
                 strong("How do I use this tab?"),
                 "Check the boxes for objects you would like to export.",
                 "For the non-x3p files, you may choose to export as a .csv or .RData.",
                 "If exporting as .RData, the files include additional data that make them considerably larger compared to a .csv.",
                 br(),
                 br(),
                 strong("What is next?"),
                 "You can return to the Explore or Scored tabs to continue working with the current scans.",
                 "Alternatively, click the Reset App button to start over from scratch.",
                 "Note that the reproducibleScript.R file must be run in the same directory in which it is downloaded to properly reproduce the results."
               ))
               
             })

output$export_checkboxUI <- renderUI({
  
  ret <- tagList(#id = "exportOptions",
    checkboxGroupInput(inputId = "preprocessExport",
                       label = "Import + Pre-process Tab",
                       choices = c("Original Scans (.x3p)",
                                   "Pre-processed Scans (.x3p)")),
    uiOutput(outputId = "compareExport_ui"),
    uiOutput(outputId = "scoreExport_ui"),
    checkboxInput(inputId = "scriptExport",label = "Reproducible R script (.R)",value = TRUE),
    radioButtons(inputId = "exportFileType",
                 label = "Exported file type (note: .csv does not contain aligned cell .x3ps)",
                 choices = c(".csv",".RData"),
                 selected = ".csv"),
    downloadButton("exportButton",
                   label = "Export to .zip file",
                   style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                   icon = icon("file-archive"))
  )
  
  return(ret)
  
})

## Explore Tab
#### Comparison Settings:
observeEvent(input$comparisonButton,{
  
  if(!is.null(shiny.r$customCellResults)){
    
    output$compareExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "compareExport",
                         label = "Explore Tab",
                         choices = c("Comparison Results (large if exported as .RData)",
                                     "Custom Cell Results"))
      
    }) 
    
  }
  else{
    
    output$compareExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "compareExport",
                         label = "Explore Tab",
                         choices = c("Comparison Results (large if exported as .RData)"))
      
    })
    
  }
  
})

#### Custom Cell tab
observeEvent(input$customCellExecute,{
  
  if(!is.null(shiny.r$comparisonData_refToTarget)){
    
    output$compareExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "compareExport",
                         label = "Explore Tab",
                         choices = c("Comparison Results (large if exported as .RData)",
                                     "Custom Cell Results"))
      
    })
    
  }
  else{
    
    output$compareExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "compareExport",
                         label = "Explore Tab",
                         choices = c("Custom Cell Results"))
      
    })
    
  }
  
})

## Score tab
#### ACES Algorithm tab
observeEvent(input$acesCalculate,{
  
  if(!is.null(shiny.r$cmcClassifs)){
    
    output$scoreExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "scoreExport",
                         label = "Score Tab",
                         choices = c(
                           "All ACES Results (large if export as .RData)",
                           "ACES Features",
                           "All CMC Results (large if export as .RData)",
                           "CMC Counts"))
      
    })
    
  }
  else{
    
    output$scoreExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "scoreExport",
                         label = "Score Tab",
                         choices = c("ACES Registrations (large if export as .RData)",
                                     "ACES Features"))
      
    })
    
  }
  
})

#### Congruent Matching Cells tab
observeEvent(input$cmcPlotExecute,{
  
  if(!is.null(shiny.r$acesRegistrations)){
    
    output$scoreExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "scoreExport",
                         label = "Score Tab",
                         choices = c(
                           "All ACES Results (large if export as .RData)",
                           "ACES Features",
                           "All CMC Results (large if export as .RData)",
                           "CMC Counts"))
      
    })
    
  }
  else{
    
    output$scoreExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "scoreExport",
                         label = "Score Tab",
                         choices = c("All CMC Results (large if export as .RData)",
                                     "CMC Counts"))
      
    })
    
  }
  
})


output$exportButton <- downloadHandler(
  filename = function() paste0("output.zip"),
  content = function(filename) {
    
    datetime <- paste0("x3pComparison", paste0("_", format(Sys.time(),"%Y%m%d%H%M%S")))
    filePath <- file.path(datetime)
    dir.create(filePath, recursive = TRUE)
    
    
    numFiles <- length(c(input$preprocessExport,
                         input$compareExport,
                         input$scoreExport,
                         input$exportFileType))
    
    progressBarSegments <- (1:numFiles)/numFiles
    
    shinybusy::show_modal_progress_line(value = 0,text = "Saving files")
    
    if(!is.null(input$preprocessExport)){
      
      tmp <- isolate(shiny.r$data)
      
      if("Original Scans (.x3p)" %in% input$preprocessExport){
        
        shinybusy::update_modal_progress(text = paste0("Saving original scans as .x3p files"),value = progressBarSegments[1])
        progressBarSegments <- progressBarSegments[-1]
        
        tmp %>%
          dplyr::select(x3pName,x3p) %>%
          purrr::pwalk(~ {
            
            x3ptools::x3p_write(..2,file = paste0(filePath,"/",..1,"_original.x3p"))
            
          })
        
      }
      if("Pre-processed Scans (.x3p)" %in% input$preprocessExport){
        
        shinybusy::update_modal_progress(text = paste0("Saving pre-processed scans as .x3p files"),value = progressBarSegments[1])
        progressBarSegments <- progressBarSegments[-1]
        
        tmp %>%
          dplyr::select(x3pName,x3p_processed) %>%
          purrr::pwalk(~ {
            
            x3ptools::x3p_write(..2,file = paste0(filePath,"/",..1,"_processed.x3p"))
            
          })
        
      }
      
      
      
    }
    if(!is.null(input$compareExport)){
      
      if("Comparison Results (large if exported as .RData)" %in% input$compareExport){
        
        shinybusy::update_modal_progress(text = paste0("Saving Explore tab comparison results as ",input$exportFileType," file."),
                                         value = progressBarSegments[1])
        progressBarSegments <- progressBarSegments[-1]
        
        exploreComparisonResults <- isolate(shiny.r$comparisonData_refToTarget) %>%
          mutate(direction = "reference_vs_target")
        
        if(!is.null(shiny.r$comparisonData_targetToRef)){
          
          exploreComparisonResults <- bind_rows(
            exploreComparisonResults,
            isolate(shiny.r$comparisonData_targetToRef) %>%
              mutate(direction = "target_vs_reference")
          )
          
        }
        
        exploreComparisonResults <- exploreComparisonResults %>%
          mutate(comparisonName = paste(input$referenceSelect,"_vs_",input$targetSelect))
        
        if(input$exportFileType == ".csv"){
          readr::write_csv(exploreComparisonResults %>%
                             dplyr::select(-c(cellHeightValues,alignedTargetCell)),
                           file = paste0(filePath,'/exploreComparisonResults.csv'))
        }
        else{
          save(exploreComparisonResults,file = paste0(filePath,"/exploreComparisonResults.RData"))
        }
        
      }
      
      if("Custom Cell Results" %in% input$compareExport){
        
        shinybusy::update_modal_progress(text = paste0("Saving custom cell comparison results as ",input$exportFileType," file."),
                                         value = progressBarSegments[1])
        progressBarSegments <- progressBarSegments[-1]
        
        manualCellResults <- shiny.r$customCellResults %>%
          mutate(comparisonName = paste0(input$customCellSelection,"_vs_",input$targetSelect_customCell))
        
        if(input$exportFileType == ".csv"){
          readr::write_csv(manualCellResults %>%
                             dplyr::select(-c(cellHeightValues,alignedTargetCell)),
                           file = paste0(filePath,'/manualCellResults.csv'))
        }
        else{
          save(manualCellResults,file = paste0(filePath,"/manualCellResults.RData"))
        }
        
      }
      
    }
    if(!is.null(input$scoreExport)){
      
      if("All ACES Results (large if export as .RData)" %in% input$scoreExport){
        
        shinybusy::update_modal_progress(text = paste0("Saving all ACES results as a ",input$exportFileType," file."),
                                         value = progressBarSegments[1])
        progressBarSegments <- progressBarSegments[-1]
        
        acesResults_all <- isolate(shiny.r$acesRegistrations) %>%
          mutate(comparisonName = paste0(input$score_referenceSelect,"_vs_",input$score_targetSelect))
        
        if(input$exportFileType == ".csv"){
          readr::write_csv(acesResults_all %>%
                             dplyr::select(-c(cellHeightValues,alignedTargetCell)),
                           file = paste0(filePath,'/acesResults_all.csv'))
        }
        else{
          save(acesResults_all,file = paste0(filePath,"/acesResults_all.RData"))
        }
        
      }
      if("ACES Features" %in% input$scoreExport){
        
        shinybusy::update_modal_progress(text = paste0("Saving ACES features as ",input$exportFileType," file."),
                                         value = progressBarSegments[1])
        progressBarSegments <- progressBarSegments[-1]
        
        acesFeatures <- isolate(shiny.r$acesFeatures) %>%
          mutate(comparisonName = paste0(input$score_referenceSelect,"_vs_",input$score_targetSelect))
        
        if(input$exportFileType == ".csv"){
          readr::write_csv(acesFeatures,
                           file = paste0(filePath,'/acesFeatures.csv'))
        }
        else{
          save(acesFeatures,file = paste0(filePath,"/acesFeatures.RData"))
        }
        
      }
      if("All CMC Results (large if export as .RData)" %in% input$scoreExport){
        
        shinybusy::update_modal_progress(text = paste0("Saving all CMC results as ",input$exportFileType," file."),
                                         value = progressBarSegments[1])
        progressBarSegments <- progressBarSegments[-1]
        
        cmcResults_all <- isolate(shiny.r$cmcClassifs) %>%
          mutate(comparisonName = paste0(input$score_referenceSelect,"_vs_",input$score_targetSelect)) 
        
        if(input$exportFileType == ".csv"){
          readr::write_csv(cmcResults_all %>%
                             dplyr::select(-c(cellHeightValues,alignedTargetCell)),
                           file = paste0(filePath,'/cmcResults_all.csv'))
        }
        else{
          save(cmcResults_all,file = paste0(filePath,"/cmcResults_all.RData"))
        }
        
      }
      if("CMC Counts" %in% input$scoreExport){
        
        shinybusy::update_modal_progress(text = paste0("Saving CMC counts as ",input$exportFileType," file."),
                                         value = progressBarSegments[1])
        progressBarSegments <- progressBarSegments[-1]
        
        cmcCounts <- isolate(shiny.r$cmcCounts)
        
        cmcCounts <- bind_rows(
          cmcCounts[[1]][[1]] %>% 
            dplyr::mutate(cmcType = "originalMethod",
                          direction = "reference_vs_target"),
          cmcCounts[[1]][[2]] %>% 
            dplyr::mutate(cmcType = "originalMethod",
                          direction = "reference_vs_target"),
          cmcCounts[[2]] %>% 
            dplyr::mutate(cmcType = "highCMC",direction = "symmetric"),
        )%>%
          mutate(comparisonName = paste0(input$score_referenceSelect,"_vs_",input$score_targetSelect)) 
        
        if(input$exportFileType == ".csv"){
          readr::write_csv(cmcCounts,
                           file = paste0(filePath,'/cmcCounts.csv'))
        }
        else{
          save(cmcCounts,file = paste0(filePath,"/cmcCounts.RData"))
        }
        
      }
      
    }
    if(!is.null(input$scriptExport)){
      
      shinybusy::update_modal_progress(text = paste0("Saving reproducible script as .R file."),
                                       value = progressBarSegments[1])
      progressBarSegments <- progressBarSegments[-1]
      
      file.create(paste0(filePath,"/reproducibleScript.R"))
      
    }
    
    shinybusy::update_modal_progress(text = "Zipping folder",value = 1)
    
    utils::zip(zipfile = filename,
               files = list.files(filePath,full.names = TRUE))
    
    shinybusy::remove_modal_progress()
    
  },
  contentType = "application/zip")
