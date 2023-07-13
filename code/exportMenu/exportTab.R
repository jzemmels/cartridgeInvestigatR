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
                       label = "Import + Pre-process Tab - export if you wish to further analyze the original/pre-processed cartridge cases",
                       choices = c("Original Scans (.x3p)",
                                   "Pre-processed Scans (.x3p)"),
                       selected = c("Original Scans (.x3p)",
                                    "Pre-processed Scans (.x3p)"),
                       width = 1000),
    uiOutput(outputId = "scoreExport_ui"),
    uiOutput(outputId = "compareExport_ui"),
    checkboxInput(inputId = "scriptExport",label = "Reproducible R script (.R) - export to ensure that all results are reproducible",
                  value = TRUE,
                  width = 1000),
    radioButtons(inputId = "exportFileType",
                 label = "Exported file type (note: csv does not contain aligned X3Ps, but RData tends to be larger)",
                 choices = c(".csv",".RData"),
                 selected = ".csv",
                 width = 1000),
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
                         label = "Explore Tab - export if you want a record of your experimentation",
                         choices = c("Comparison Results (large if exported as .RData)",
                                     "Custom Cell Results"),
                         width = 1000)
      
    }) 
    
  }
  else{
    
    output$compareExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "compareExport",
                         label = "Explore Tab - export if you want a record of your experimentation",
                         choices = c("Comparison Results (large if exported as .RData)"),
                         width = 1000)
      
    })
    
  }
  
})

#### Custom Cell tab
observeEvent(input$customCellExecute,{
  
  if(!is.null(shiny.r$comparisonData_refToTarget)){
    
    output$compareExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "compareExport",
                         label = "Explore Tab - export if you want a record of your experimentation",
                         choices = c("Comparison Results (large if exported as .RData)",
                                     "Custom Cell Results"),
                         width = 1000)
      
    })
    
  }
  else{
    
    output$compareExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "compareExport",
                         label = "Explore Tab - export if you want a record of your experimentation",
                         choices = c("Custom Cell Results"),
                         width = 1000)
      
    })
    
  }
  
})

## Score tab
#### ACES Algorithm tab
observeEvent(input$acesCalculate,{
  
  if(!is.null(shiny.r$cmcClassifs)){
    
    output$scoreExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "scoreExport",
                         label = "Score Tab - export if you want a record of the similarity scores",
                         choices = c(
                           "All ACES Results (large if export as .RData)",
                           "ACES Features",
                           "All CMC Results (large if export as .RData)",
                           "CMC Counts"),
                         width = 1000)
      
    })
    
  }
  else{
    
    output$scoreExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "scoreExport",
                         label = "Score Tab - export if you want a record of the similarity scores",
                         choices = c("ACES Registrations (large if export as .RData)",
                                     "ACES Features"),
                         width = 1000)
      
    })
    
  }
  
})

#### Congruent Matching Cells tab
observeEvent(input$cmcPlotExecute,{
  
  if(!is.null(shiny.r$acesRegistrations)){
    
    output$scoreExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "scoreExport",
                         label = "Score Tab - export if you want a record of the similarity scores",
                         choices = c(
                           "All ACES Results (large if export as .RData)",
                           "ACES Features",
                           "All CMC Results (large if export as .RData)",
                           "CMC Counts"),
                         width = 1000)
      
    })
    
  }
  else{
    
    output$scoreExport_ui <- renderUI({
      
      checkboxGroupInput(inputId = "scoreExport",
                         label = "Score Tab - export if you want a record of the similarity scores",
                         choices = c("All CMC Results (large if export as .RData)",
                                     "CMC Counts"),
                         width = 1000)
      
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
    
    if(!is.null(input$scriptExport)){
      
      shinybusy::update_modal_progress(text = paste0("Saving reproducible script as .R file."),
                                       value = progressBarSegments[1])
      progressBarSegments <- progressBarSegments[-1]
      
      file.create(paste0(filePath,"/reproducibleScript.R"))
      
      reproScript <- readLines(paste0(filePath,"/reproducibleScript.R"))
      reproScript <- paste0(reproScript,
                            "#Set working directory\nthis.dir <- dirname(parent.frame(2)$ofile)\nsetwd(this.dir)\n
                            #Necessary libraries\nlibrary(cmcR)\nlibrary(scored)\nlibrary(impressions)\nlibrary(tidyverse)\n")
      write(reproScript,file = paste0(filePath,"/reproducibleScript.R"))
      
    }
    
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
        
        # write pre-processing pipeline to script
        if(!is.null(input$scriptExport)){
          
          reproScript <- paste0(readLines(paste0(filePath,"/reproducibleScript.R")),collapse = "\n")
          
          reproScript <- paste0(reproScript,
                                "#Pre-processing pipeline code:")
          
          preProcessHelpers <- paste0(readLines("code/preProcessFunction_helpers.R"),collapse = "\n")
          
          reproScript <- paste0(reproScript,
                                "\n##Pre-processing helper functions\n",
                                preProcessHelpers,
                                collapse = "\n")
          
          originalScans <- list.files(paste0(filePath,"/"),pattern = "_original\\.x3p")
          
          tmpInput <- shiny::reactiveValuesToList(input)
          
          # each preprocessing step is in-order as specified by letter# where # is
          # a number
          preProcessSteps <- tmpInput[stringr::str_detect(names(tmpInput),"letter")]
          
          if(length(preProcessSteps) > 0){
            
            preProcessingPipelineCode  <- 
              paste0('# the steps may be out of order depending on which was last updated by
                   # the user. this will correct the order
                   tmpInput <- ',paste0(deparse(dput(tmpInput[str_detect(names(tmpInput),"params")])),collapse = ""),'
                   preProcessSteps <- ',paste0(deparse(dput(preProcessSteps)),collapse=""),'
                   preProcessSteps <- preProcessSteps[order(names(preProcessSteps))]
                   preProcessFunctions <- purrr::map(1:length(preProcessSteps),
                                              function(ind){
                                                
                                                if(!is.null(preProcessSteps[[ind]])){
                                                  
                                                  # get the parameters
                                                  paramValues <- tmpInput[names(tmpInput) %in% paste0("params",c(1,2),"_",ind)]
                                                  
                                                  
                                                  # returns a preprocessing function with the necessary parameters
                                                  # filled-in
                                                  return(preProcess_partial(preProcessSteps[[ind]],paramValues))
                                                  
                                                }
                                              }) %>%
              purrr::discard(~ all(is.null(.)))
            
            purrr::walk(',paste0(deparse(dput(originalScans)),collapse=""),',
                       function(filePath){
                         
                         scan <- x3ptools::x3p_read(filePath)
                         
                         scanName <- filePath %>%
                           stringr::str_remove(paste0(filePath,"/")) %>%
                           stringr::str_remove("_original\\\\.x3p")
                         
                         purrr::walk(preProcessFunctions,
                                     function(func){
                                       
                                       scan <<- func(x3p = scan)
                                       
                                     })
                        
                        # copy the mask from the processed scan, if applicable
                        if(file.exists(paste0(scanName,"_processed.x3p"))){
                          manualMask <- x3ptools::x3p_read(paste0(scanName,"_processed.x3p"))$mask
                          if(!is.null(manualMask)){
                            scan$mask <- t(manualMask)
                            maskValues <- unique(c(scan$mask))
                               
                            # some mask values have an extra "FF" at the end
                            # that is not part of the hexidecimal color ID
                            for(color in maskValues){
                              scan$mask[scan$mask == color] <- str_sub(color,1,7) 
              
                            }
                            maskCounts <- table(c(scan$mask))
            
                            if(length(maskCounts) > 1){
                              # assume that the most common color in the mask
                              # is the one to keep
                              maskColorKeep <- names(maskCounts)[which.max(maskCounts)]
            
                              # replace non-mask values with NA
                              scan$surface.matrix[t(as.matrix(scan$mask)) != maskColorKeep] <- NA
                            }
                          }
                        }
                        
                        x3ptools::x3p_write(scan,file = paste0(scanName,"_processed.x3p"))
                       })
            ',collapse = "")
            
          } else{
            
            preProcessingPipelineCode  <- 
              paste0('
                     purrr::walk(',paste0(deparse(dput(originalScans)),collapse=""),',
                       function(filePath){
                         
                         scan <- x3ptools::x3p_read(filePath)
                         
                         scanName <- filePath %>%
                           stringr::str_remove(paste0(filePath,"/")) %>%
                           stringr::str_remove("_original\\\\.x3p")
                        
                        # copy the mask from the processed scan, if applicable
                        if(file.exists(paste0(scanName,"_processed.x3p"))){
                          manualMask <- x3ptools::x3p_read(paste0(scanName,"_processed.x3p"))$mask
                          if(!is.null(manualMask)){
                            scan$mask <- t(manualMask)
                            maskValues <- unique(c(scan$mask))
                               
                            # some mask values have an extra "FF" at the end
                            # that is not part of the hexidecimal color ID
                            for(color in maskValues){
                              scan$mask[scan$mask == color] <- str_sub(color,1,7) 
              
                            }
                            maskCounts <- table(c(scan$mask))
            
                            if(length(maskCounts) > 1){
                              # assume that the most common color in the mask
                              # is the one to keep
                              maskColorKeep <- names(maskCounts)[which.max(maskCounts)]
            
                              # replace non-mask values with NA
                              scan$surface.matrix[t(as.matrix(scan$mask)) != maskColorKeep] <- NA
                            }
                          }
                        }
                        
                        x3ptools::x3p_write(scan,file = paste0(scanName,"_processed.x3p"))
                       })
            ',collapse = "")
            
          }
          
          reproScript <- paste0(reproScript,
                                "\n##Pre-processing pipeline code\n",
                                preProcessingPipelineCode,collapse = "\n")
          
          write(reproScript,file = paste0(filePath,"/reproducibleScript.R"))
          
        }
        
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
        
        # write pre-processing pipeline to script
        if(!is.null(input$scriptExport)){
          
          reproScript <- paste0(readLines(paste0(filePath,"/reproducibleScript.R")),collapse = "\n")
          
          # reproScript <- paste0(reproScript,
          #                       "\n#Cell Grid Comparison code:\n")
          
          comparisonCode <- 
            paste0('
                   reference <- x3ptools::x3p_read("',input$referenceSelect,'_processed.x3p")
                   target <- x3ptools::x3p_read("',input$targetSelect,'_processed.x3p")
                   
                   # rarely, the resolution of two scans are barely different from one another
                   # (on the order of 1e-10). This forces the resolutions to be the same
                   if(!isTRUE(all.equal(reference$header.info$incrementX,target$header.info$incrementX))){
                     if(reference$header.info$incrementX > target$header.info$incrementX){
                       target <- x3ptools::x3p_interpolate(target,resx = reference$header.info$incrementX)
                     }
                     else{
                       reference <- x3ptools::x3p_interpolate(reference,resx = target$header.info$incrementX)
                     }
                   }
                   
                   exploreComparisonResults <- 
                     scored::comparison_cellBased(reference = reference,
                                                  target = target,
                                                  direction = "both",
                                                  returnX3Ps = TRUE, 
                                                  thetas = seq(',input$thetaRangeMin,',',input$thetaRangeMax,",by = ",input$thetaStep,'),
                                                  numCells = ',deparse(dput(str_split(input$numCells,",")[[1]] %>% map_dbl(as.numeric))),',
                                                  maxMissingProp = ',input$maxNonMissingProp,',
                                                  sideLengthMultiplier = ',input$cellRegionProp,')
                   
                   if("',input$exportFileType,'" == ".csv"){
                     readr::write_csv(exploreComparisonResults %>%
                             dplyr::select(-c(cellHeightValues,alignedTargetCell)),
                           file = "exploreComparisonResults.csv")
                   } else{
                     save(exploreComparisonResults,file = "exploreComparisonResults.RData")
                   }',
                   collapse = "")
          
          reproScript <- paste0(reproScript,
                                "\n##Cell Grid Comparison code\n",
                                comparisonCode,collapse = "\n")
          
          write(reproScript,file = paste0(filePath,"/reproducibleScript.R"))
          
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
      if(("ACES Features" %in% input$scoreExport) | ("All ACES Results (large if export as .RData)" %in% input$scoreExport)){
        # write pre-processing pipeline to script
        if(!is.null(input$scriptExport)){
          
          reproScript <- paste0(readLines(paste0(filePath,"/reproducibleScript.R")),collapse = "\n")
          
          # reproScript <- paste0(reproScript,
          #                       "\n#Cell Grid Comparison code:\n")
          
          acesCode <- 
            paste0('
                   reference <- x3ptools::x3p_read("',input$score_referenceSelect,'_processed.x3p")
                   target <- x3ptools::x3p_read("',input$score_targetSelect,'_processed.x3p")
                   
                   # rarely, the resolution of two scans are barely different from one another
                   # (on the order of 1e-10). This forces the resolutions to be the same
                   if(!isTRUE(all.equal(reference$header.info$incrementX,target$header.info$incrementX))){
                     if(reference$header.info$incrementX > target$header.info$incrementX){
                       target <- x3ptools::x3p_interpolate(target,resx = reference$header.info$incrementX)
                     }
                     else{
                       reference <- x3ptools::x3p_interpolate(reference,resx = target$header.info$incrementX)
                     }
                   }
                   
                   compName <- paste0("',input$score_referenceSelect,'","_vs_","',input$score_targetSelect,'")
                   
                   # first align full reference and target across theta grid
                   fullScanRegistrations <- scored::comparison_fullScan(reference = reference,target = target,
                                                       thetas = seq(-30,30,by = 3),returnX3Ps = FALSE) %>%
                       dplyr::mutate(comparisonName = compName) %>%
                       dplyr::group_by(direction,cellIndex) %>%
                       dplyr::filter(fft_ccf == max(fft_ccf)) %>%
                       dplyr::slice(1) %>%
                       dplyr::ungroup()  %>%
                       dplyr::arrange(direction) %>%
                       dplyr::group_by(direction) %>%
                       dplyr::group_split() %>%
                       purrr::map_dfr(function(dat){
                           scored::comparison_fullScan(reference = reference,
                                                       target = target,
                                                       thetas = unique(dat$theta),
                                                       returnX3Ps = TRUE) %>%
                       dplyr::filter(fft_ccf == max(fft_ccf)) %>%
                       dplyr::mutate(comparisonName = compName,
                                     direction = unique(dat$direction))
                       })
  
                  fullScanFeatures <- fullScanRegistrations %>%
                      dplyr::group_by(comparisonName,direction) %>%
                      scored::feature_aLaCarte(features = c("registration","visual")) %>%
                      dplyr::group_by(comparisonName) %>%
                      dplyr::summarize(across(tidyselect::where(is.numeric),~ mean(.,na.rm = TRUE))) %>%
                      purrr::set_names(paste0("fullScan_",names(.))) %>%
                      dplyr::rename(comparisonName = fullScan_comparisonName)
                  
                  cellBasedRegistrations <- fullScanRegistrations %>%
                      dplyr::select(direction,theta,cellHeightValues,alignedTargetCell) %>%
                      dplyr::group_by(direction) %>%
                      dplyr::group_split() %>%
                      purrr::map_dfr(function(dat){
                          scored::comparison_cellBased(reference = dat$cellHeightValues[[1]],
                                                       target = dat$alignedTargetCell[[1]],
                                                       direction = "one",
                                                       thetas = -2:2,
                                                       maxMissingProp = .99,
                                                       numCells = c(4,4),
                                                       sideLengthMultiplier = 1.1,
                                                       returnX3Ps = TRUE) %>%
                      mutate(comparisonName = compName,
                             direction = unique(dat$direction))
                      })
                  
                  cellBasedFeatures <- cellBasedRegistrations %>%
                      dplyr::group_by(comparisonName,direction) %>%
                      scored::feature_aLaCarte(features = "all",quiet = TRUE,eps = 5,minPts = 4) %>%
                      dplyr::group_by(comparisonName) %>%
                      dplyr::mutate(clusterInd = as.numeric(clusterInd)) %>%
                      dplyr::summarize(across(tidyselect::where(~ any(is.numeric(.) | is.na(.))),~ mean(.,na.rm = TRUE))) %>%
                      dplyr::ungroup() %>%
                      purrr::set_names(paste0("cellBased_",names(.))) %>%
                      dplyr::select(-cellBased_comparisonName)
                  
                  acesResults_all <- dplyr::bind_rows(fullScanRegistrations %>%
                                            dplyr::mutate(comparisonType = "Full-Scan"),
                                          cellBasedRegistrations %>%
                                            dplyr::mutate(comparisonType = "Cell-Based"))
                  
                  if("',input$exportFileType,'" == ".csv"){
                      readr::write_csv(acesResults_all %>%
                             dplyr::select(-c(cellHeightValues,alignedTargetCell)),
                           file = "acesResults_all.csv")
                  } else{
                    save(acesResults_all,file = "acesResults_all.RData")
                  }
                  
                  acesFeatures <- bind_cols(fullScanFeatures,cellBasedFeatures) %>%
                      dplyr::mutate(comparisonName = compName)
                  
                  if("',input$exportFileType,'" == ".csv"){
                      readr::write_csv(acesFeatures,
                           file = "acesFeatures.csv")
                  } else{ 
                    save(acesFeatures,file = "acesFeatures.RData")
                  }',
                  collapse = "")
          
          reproScript <- paste0(reproScript,
                                "\n##ACES Features code\n",
                                acesCode,collapse = "\n")
          
          write(reproScript,file = paste0(filePath,"/reproducibleScript.R"))
          
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
            dplyr::mutate(cmcType = "highCMC",direction = "symmetric"))%>%
          mutate(comparisonName = paste0(input$score_referenceSelect,"_vs_",input$score_targetSelect)) 
        
        if(input$exportFileType == ".csv"){
          readr::write_csv(cmcCounts,
                           file = paste0(filePath,'/cmcCounts.csv'))
        }
        else{
          save(cmcCounts,file = paste0(filePath,"/cmcCounts.RData"))
        }
        
      }
      
      if(("CMC Counts" %in% input$scoreExport) | ("All CMC Results (large if export as .RData)" %in% input$scoreExport)){
        # write pre-processing pipeline to script
        if(!is.null(input$scriptExport)){
          
          reproScript <- paste0(readLines(paste0(filePath,"/reproducibleScript.R")),collapse = "\n")
          
          # reproScript <- paste0(reproScript,
          #                       "\n#Cell Grid Comparison code:\n")
          
          cmcCode <- 
            paste0('
                   reference <- x3ptools::x3p_read("',input$score_referenceSelect,'_processed.x3p")
                   target <- x3ptools::x3p_read("',input$score_targetSelect,'_processed.x3p")
                   
                   # rarely, the resolution of two scans are barely different from one another
                   # (on the order of 1e-10). This forces the resolutions to be the same
                   if(!isTRUE(all.equal(reference$header.info$incrementX,target$header.info$incrementX))){
                     if(reference$header.info$incrementX > target$header.info$incrementX){
                       target <- x3ptools::x3p_interpolate(target,resx = reference$header.info$incrementX)
                     }
                     else{
                       reference <- x3ptools::x3p_interpolate(reference,resx = target$header.info$incrementX)
                     }
                   }
                   
                   compName <- paste0("',input$score_referenceSelect,'","_vs_","',input$score_targetSelect,'")
                   
                   compData <- scored::comparison_cellBased(reference = reference,
                                           target = target,
                                           direction = "both",
                                           thetas = seq(',input$cmcTab_thetaRangeMin,',',input$cmcTab_thetaRangeMax,",by = ",input$cmcTab_thetaStep,'),
                                           numCells = ',deparse(dput(str_split(input$cmcTab_numCells,",")[[1]] %>% map_dbl(as.numeric))),',
                                           maxMissingProp = ',input$cmcTab_maxNonMissingProp,',
                                           sideLengthMultiplier = ',input$cmcTab_cellRegionProp,',
                                           returnX3Ps = TRUE)
                   
                   cmc1 <- compData %>%
                       dplyr::filter(direction == "reference_vs_target") %>%
                       dplyr::mutate(originalMethodClassif = cmcR::decision_CMC(cellIndex=cellIndex,
                                                             x=x,y=y,theta = theta,corr = pairwiseCompCor,
                                                             xThresh = 20,thetaThresh = 6,corrThresh = .5),
                                     highCMCClassif = cmcR::decision_CMC(cellIndex=cellIndex,
                                                      x=x,y=y,theta = theta,corr = pairwiseCompCor,
                                                      xThresh = 20,thetaThresh = 6,corrThresh = .5,tau = 1))
                   cmc2 <- compData %>%
                       dplyr::filter(direction == "target_vs_reference") %>%
                       dplyr::mutate(originalMethodClassif = cmcR::decision_CMC(cellIndex=cellIndex,
                                                             x=x,y=y,theta = theta,corr = pairwiseCompCor,
                                                             xThresh = 20,thetaThresh = 6,corrThresh = .5),
                                     highCMCClassif = cmcR::decision_CMC(cellIndex=cellIndex,
                                                      x=x,y=y,theta = theta,corr = pairwiseCompCor,
                                                      xThresh = 20,thetaThresh = 6,corrThresh = .5,tau = 1))
                   
                   cmcCounts <- cmcR::decision_combineDirections(cmc1 %>%
                                                    dplyr::select(-c(cellHeightValues,alignedTargetCell))
                                                  ,cmc2 %>%
                                                    dplyr::select(-c(cellHeightValues,alignedTargetCell)))
                   
                   cmcCounts <- bind_rows(
                       cmcCounts[[1]][[1]] %>%
                         dplyr::mutate(cmcType = "originalMethod",direction = "reference_vs_target"),
                       cmcCounts[[1]][[2]] %>%
                         dplyr::mutate(cmcType = "originalMethod",direction = "reference_vs_target"),
                       cmcCounts[[2]] %>%
                         dplyr::mutate(cmcType = "highCMC",direction = "symmetric")) %>%
                    dplyr::mutate(comparisonName = compName)
          
                   if("',input$exportFileType,'" == ".csv"){
                      readr::write_csv(cmcCounts,file = "cmcCounts.csv")
                   } else{
                       save(cmcCounts,file = "cmcCounts.RData")
                   }
                   
                   cmcResults_all <- bind_rows(cmc1,cmc2)
                   
                   if("',input$exportFileType,'" == ".csv"){
                       readr::write_csv(cmcResults_all %>%
                                         dplyr::select(-c(cellHeightValues,alignedTargetCell)),
                                        file = "cmcResults_all.csv")
                   } else{ 
                   save(cmcResults_all,file = "cmcResults_all.RData")
                   }
                   ',
                   collapse = "")
          
          reproScript <- paste0(reproScript,
                                "\n##CMC Method code\n",
                                cmcCode,collapse = "\n")
          
          write(reproScript,file = paste0(filePath,"/reproducibleScript.R"))
          
        }
      }
      
    }
    
    shinybusy::update_modal_progress(text = "Zipping folder",value = 1)
    
    utils::zip(zipfile = filename,
               files = list.files(filePath,full.names = TRUE))
    
    shinybusy::remove_modal_progress()
    
  },
  contentType = "application/zip")
