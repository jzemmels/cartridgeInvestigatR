#################################### Code for Congruent Matching Cells tab


# show popup when the user clicks the Help button
observeEvent(input$congruentMatchingCellsHelp,
             {
               
               showModal(modalDialog(
                 title = "Help: Congruent Matching Cells Tab",easyClose = TRUE,
                 strong("Why would I use this tab?"),
                 "Measure the similarity between two cartridge cases using the Congruent Matching Cells method.",
                 br(),
                 br(),
                 strong("What do I need to do before using this tab?"),
                 "Click the 'Perform Comparison' button in the 'Comparison Settings' tab and wait for the comparison to finish.",
                 br(),
                 br(),
                 strong("How do I use this tab?"),
                 "Choose a reference scan that was divided into a grid of cells in the comparison procedure (possibly one or two choices, depending on if the 'Compare in both directions' checkbox was clicked in the previous tab).",
                 "Also select parameters for the Congruent Matching Cells method.",
                 "Click the 'Visualize CMCs' button to show the reference and target scans with overlaid Congruent Matching Cells (CMCs) and non-CMCs.",
                 br(),
                 br(),
                 strong("What is next?"),
                 "The Congruent Matching Cells count is a measure of similarity between the two cartridge cases.",
                 "The larger the CMC count, the more similar the scans.",
                 "Use the CMC count to inform your evidentiary conclusion."
               ))
               
             })

observeEvent(input$cmcTabSettings_button,{
  
  shinyjs::toggle(id = "cmcTabMenu",anim = TRUE)
  
})

# output$cmcTab_useExploreResults_ui <- renderUI({
#   
#   return(shinyjs::hidden(checkboxInput(inputId = "cmcTab_useExploreResults",
#                                        label = "Use results from Explore tab",
#                                        value = FALSE)))
#   
# })
# 
# observeEvent(input$comparisonButton,{
#   
#   output$cmcTab_useExploreResults_ui <- renderUI({
#     
#     return(checkboxInput(inputId = "cmcTab_useExploreResults",
#                          label = "Use results from Explore tab",
#                          value = TRUE))
#     
#   })
#   
# })
# 
# observeEvent(input$cmcTab_useExploreResults,ignoreInit = TRUE,{
#   
#   if(input$cmcTab_useExploreResults){
#     updateSelectInput(session = session,
#                       inputId = "score_referenceSelect",
#                       selected = input$referenceSelect)
#     updateSelectInput(session = session,
#                       inputId = "score_targetSelect",
#                       selected = input$targetSelect)
#     shinyjs::disable(id = "score_scanPreview_ui")
#     shinyjs::disable(id = "cmcTabSettings_button")
#   }
#   else{
#     shinyjs::enable(id = "cmcTabSettings_button")
#   }
# })

observeEvent(input$score_previewScans,{
  
  # visualize selected reference scan
  
  req(input$score_referenceSelect != "")
  req(input$score_targetSelect != "")
  
  tmp <- isolate(shiny.r$data)
  
  req(!is.null(tmp$x3p_processed))
  
  reference <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == input$score_referenceSelect) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  output$cmc_referencePreview <- renderPlot({
    
    plt <- x3pListPlot(list(reference) %>% purrr::set_names(input$score_referenceSelect))
    
    return(plt)
  })
  
  
  # visualize selected target scan
  
  req(input$score_targetSelect != "")
  
  tmp <- isolate(shiny.r$data)
  
  req(!is.null(tmp$x3p_processed))
  
  target <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == input$score_targetSelect) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  output$cmc_targetPreview <- renderPlot({
    
    plt <- x3pListPlot(list(target) %>% purrr::set_names(input$score_targetSelect))
    
    return(plt)
  })
  
})


observeEvent(input$cmcPlotExecute,{
  
  req(shiny.r$data)
  
  # if user doesn't want to use the Explore results...
  # if(!input$cmcTab_useExploreResults){
  
  req(input$score_referenceSelect)
  req(input$score_targetSelect)
  
  refName <- input$score_referenceSelect
  targName <- input$score_targetSelect
  
  tmp <- isolate(shiny.r$data)
  
  reference <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == refName) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  target <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == targName) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  cellGrid <- input$cmcTab_numCells %>%
    stringr::str_split(",") %>%
    .[[1]] %>%
    purrr::map_int(as.integer)
  
  shinybusy::show_modal_progress_line(text = paste0("Comparing cells"),value = 0)
  
  compData <- scored::comparison_cellBased(reference = reference,
                                           target = target,
                                           direction = "both",
                                           thetas = seq(input$cmcTab_thetaRangeMin,input$cmcTab_thetaRangeMax,
                                                        by = input$cmcTab_thetaStep),
                                           numCells = cellGrid,
                                           maxMissingProp = input$cmcTab_maxNonMissingProp,
                                           sideLengthMultiplier = input$cmcTab_cellRegionProp,
                                           returnX3Ps = TRUE)
  
  shinybusy::update_modal_progress(text = paste0("Computing cell features"),value = .5)
  
  # browser()
  
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
  
  cmcCombined <- cmcR::decision_combineDirections(cmc1 %>%
                                                    dplyr::select(-c(cellHeightValues,alignedTargetCell))
                                                  ,cmc2 %>%
                                                    dplyr::select(-c(cellHeightValues,alignedTargetCell))
  )
  
  shiny.r$cmcClassifs <<- bind_rows(cmc1,cmc2)
  shiny.r$cmcCounts <<- cmcCombined
  
  output$cmcMethodPlot_refToTarget <- renderPlot(bg = "white",{
    
    plt1 <- cmcR::cmcPlot(reference = reference,
                          target = target,
                          cmcClassifs = bind_rows(cmc1 %>% 
                                                    right_join(cmcCombined[[1]][[1]] %>% 
                                                                 dplyr::select(cellIndex,theta),
                                                               by = c("cellIndex","theta")),
                                                  cmc1 %>%
                                                    filter(!(cellIndex %in% cmcCombined[[1]][[1]]$cellIndex)) %>%
                                                    group_by(cellIndex) %>%
                                                    filter(fft_ccf == max(fft_ccf)) %>%
                                                    ungroup()),
                          cmcCol = "originalMethodClassif",
                          type = "list")
    
    return(patchwork::wrap_plots(plt1[[1]],
                                 plt1[[2]],
                                 plt1[[3]],nrow = 3,
                                 heights = c(1,1.3,.1)))
    
  })
  
  output$cmcMethodPlot_targetToRef <- renderPlot(bg = "white",{
    plt2 <- cmcR::cmcPlot(reference = target,
                          target = reference,
                          cmcClassifs = bind_rows(cmc2 %>%
                                                    right_join(cmcCombined[[1]][[2]] %>%
                                                                 dplyr::select(cellIndex,theta),
                                                               by = c("cellIndex","theta")),
                                                  cmc2 %>%
                                                    filter(!(cellIndex %in% cmcCombined[[1]][[2]]$cellIndex)) %>%
                                                    group_by(cellIndex) %>%
                                                    filter(fft_ccf == max(fft_ccf)) %>%
                                                    ungroup()),
                          cmcCol = "originalMethodClassif",
                          type = "list")
    
    
    return(patchwork::wrap_plots(plt2[[1]],
                                 plt2[[2]],
                                 plt2[[3]],nrow = 3,
                                 heights = c(1,1.3,.1)))
    
  })
  
  
  output$cmcMethodInformation_refToTarget <- renderUI({
    
    cmcCount <- nrow(cmcCombined[[1]][[1]])
    
    compData <- compData %>%
      filter(direction == "reference_vs_target")
    
    maxCorrData <- compData %>%
      group_by(cellIndex) %>%
      filter(fft_ccf == max(fft_ccf)) %>%
      ungroup()
    
    estimated_theta <-  maxCorrData %>%
      pull(theta) %>%
      median()
    
    estimated_x <- maxCorrData %>%
      pull(x) %>%
      median()
    
    estimated_y <- maxCorrData %>%
      pull(y) %>%
      median()
    
    ret <- tags$div(
      h4(strong("Results from ",input$score_referenceSelect," vs. ",input$score_targetSelect)),
      h5(tags$strong("CMC Count: "),cmcCount),
      h5(tags$strong("Estimated Rotation: "),estimated_theta," degrees"),
      h5(tags$strong("Estimated Shift: "),abs(estimated_x)," pixels ",ifelse(estimated_x < 0,"left","right"),", ",
         abs(estimated_y)," pixels ",ifelse(estimated_y < 0,"down","up"),"."))
    
    return(ret)
    
  })
  
  output$cmcMethodInformation_targetToRef <- renderUI({
    
    cmcCount <- nrow(cmcCombined[[1]][[2]])
    
    compData <- compData %>%
      filter(direction == "target_vs_reference")
    
    maxCorrData <- compData %>%
      group_by(cellIndex) %>%
      filter(fft_ccf == max(fft_ccf)) %>%
      ungroup()
    
    estimated_theta <-  maxCorrData %>%
      pull(theta) %>%
      median()
    
    estimated_x <- maxCorrData %>%
      pull(x) %>%
      median()
    
    estimated_y <- maxCorrData %>%
      pull(y) %>%
      median()
    
    ret <- tags$div(
      h4(strong("Results from ",input$score_targetSelect," vs. ",input$score_referenceSelect)),
      h5(tags$strong("CMC Count: "),cmcCount),
      h5(tags$strong("Estimated Rotation: "),estimated_theta," degrees"),
      h5(tags$strong("Estimated Shift: "),abs(estimated_x)," pixels ",ifelse(estimated_x < 0,"left","right"),", ",
         abs(estimated_y)," pixels ",ifelse(estimated_y < 0,"down","up"),"."))
    
    return(ret)
    
  })
  
  remove_modal_progress()
  
})
