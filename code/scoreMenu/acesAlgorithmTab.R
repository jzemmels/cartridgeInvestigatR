############################ Code for ACES Algorithm tab

# show popup when the user clicks the Help button
observeEvent(input$acesAlgorithmHelp,
             {
               
               showModal(modalDialog(
                 title = "Help: ACES Algorithm Tab",easyClose = TRUE,
                 strong("Why would I use this tab?"),
                 "Apply the Automatic Cartridge Evidence Scoring (ACES) algorithm to estimate the probability that two cartridge cases match.",
                 br(),
                 br(),
                 strong("What do I need to do before using this tab?"),
                 "At the very least, upload scans to the app.",
                 br(),
                 br(),
                 strong("How do I use this tab?"),
                 "Select a reference and target scan to compare.",
                 "Note that the ACES match probability is symmetric, so for a particular pair it does not matter which scan is the reference and which is the target.",
                 "The choice of reference and target is used internally to keep track of the scans.",
                 br(),
                 "After selecting a reference and target, press the Estimate Match Probability button.",
                 "The algorithm may take a while to run.",
                 "When it is done running, a plot and table will appear at the bottom of the page showing the estimated match probability based on three statistical models: a Decision Tree, a Random Forest, and a Logistic Regression",
                 br(),
                 br(),
                 strong("What is next?"),
                 "Move to the Export tab if you would like to download results from the ACES algorithm.",
                 "Alternatively, you may move to the Congruent Matching Cells tab to calculate the CMC count similarity score."
               ))
               
             })



observeEvent(input$score_previewScans,{
  
  # visualize selected reference scan
  
  req(input$score_referenceSelect != "")
  
  tmp <- isolate(shiny.r$data)
  
  req(!is.null(tmp$x3p_processed))
  
  reference <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == input$score_referenceSelect) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  output$aces_referencePreview <- renderPlot({
    
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
  
  output$aces_targetPreview <- renderPlot({
    
    plt <- x3pListPlot(list(target) %>% purrr::set_names(input$score_targetSelect))
    
    return(plt)
  })
  
})

# when "Estimate Match Probability" is clicked, run a full ACES comparison +
# feature calculation, then 

observeEvent(input$acesCalculate,{
  
  req(shiny.r$data)
  req(shiny.r$data$x3p_processed)
  
  tmp <- isolate(shiny.r$data)
  
  reference <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == input$score_referenceSelect) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  target <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == input$score_targetSelect) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
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
  
  show_modal_progress_line(text = paste0("Comparing full scans"),value = 0)
  
  compName <- paste0(input$score_referenceSelect,"_vs_",input$score_targetSelect)
  
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
  
  shinybusy::update_modal_progress(text = paste0("Calculating full scan features"),value = .25)
  
  fullScanFeatures <- fullScanRegistrations %>%
    dplyr::group_by(comparisonName,direction) %>%
    scored::feature_aLaCarte(features = c("registration","visual")) %>%
    dplyr::group_by(comparisonName) %>%
    dplyr::summarize(across(tidyselect::where(is.numeric),~ mean(.,na.rm = TRUE))) %>%
    purrr::set_names(paste0("fullScan_",names(.))) %>%
    dplyr::rename(comparisonName = fullScan_comparisonName)
  
  update_modal_progress(value = .5,
                        text = "Comparing cells")
  
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
  
  update_modal_progress(value = .75,
                        text = "Calculating cell-based features")
  
  cellBasedFeatures <- cellBasedRegistrations %>%
    dplyr::group_by(comparisonName,direction) %>%
    scored::feature_aLaCarte(features = "all",quiet = TRUE,eps = 5,minPts = 4) %>%
    dplyr::group_by(comparisonName) %>%
    dplyr::mutate(clusterInd = as.numeric(clusterInd)) %>%
    dplyr::summarize(across(tidyselect::where(~ any(is.numeric(.) | is.na(.))),~ mean(.,na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    # dplyr::mutate(clusterIndTRUE = clusterInd == 1) %>%
    purrr::set_names(paste0("cellBased_",names(.))) %>%
    dplyr::select(-cellBased_comparisonName)
  
  allFeatures <- bind_cols(fullScanFeatures,cellBasedFeatures)
  
  shinybusy::update_modal_progress(text = paste0("Estimating match probability"),value = .99)
  
  # load fitted models and predict match probability 
  
  library(randomForest)
  library(rpart)
  load("models/logisticReg_allACES_minimal.RData")
  load("models/rf_allACES_minimal.RData")
  load("models/cart_allACES_minimal.RData")
  load("models/aces_trainingData.RData")
  
  allFeatures <- allFeatures %>%
    # delete this once the models are re-run with updated feature names:
    rename(fullScan_pairwiseCompCorMean = fullScan_pairwiseCompCorAve,
           cellBased_pairwiseCompCorMean = cellBased_pairwiseCompCorAve,
           cellBased_clusterIndTRUE = cellBased_clusterInd) %>%
    map2_dfc(names(.),
             function(column,colName){
               
               if(any(str_detect(colName,names(aces_trainingData)))){
                 
                 if(any(is.numeric(column),na.rm = TRUE)){
                   column[is.na(column) | is.nan(column)] <- median(aces_trainingData[,colName][[1]],na.rm = TRUE)
                 }
                 
                 return(data.frame(x = column) %>%
                          set_names(colName))
                 
               }
               
             })
  
  # model predicts if comparison is a non-match, so switch to match prob
  matchProbs <- data.frame(logisticProb = 1 - predict(logisticReg_allACES,newdata = allFeatures,type = "response"),
                           rfProb = predict(rf_allACES,newdata = allFeatures,type = "prob")[,1],
                           cartProb = predict(cart_allACES,newdata = allFeatures)[,1])
  
  shiny.r$acesRegistrations <<- bind_rows(fullScanRegistrations %>%
                                            mutate(comparisonType = "Full-Scan"),
                                          cellBasedRegistrations %>%
                                            mutate(comparisonType = "Cell-Based"))
  shiny.r$acesFeatures <<- bind_cols(allFeatures,matchProbs)
  
  matchProbs <- matchProbs %>%
    tidyr::pivot_longer(cols = everything()) %>%
    dplyr::mutate(valueLabel = paste0(round(value*100,1),"%"),
                  modelLabel = factor(ifelse(name == "logisticProb","Logistic Regression",
                                             ifelse(name == "rfProb","Random Forest",
                                                    "Decision Tree")),
                                      levels = c("Decision Tree","Random Forest","Logistic Regression")))
  
  output$aces_matchProbPlot <- renderPlot({
    
    matchProbs %>%
      ggplot(aes(y = modelLabel)) +
      geom_segment(aes(x = 0,xend = value,yend = modelLabel)) +
      geom_point(aes(x = value),size = 3) +
      # ggrepel::geom_text_repel(aes(x = value,label = valueLabel),min.segment.length = 0) +
      theme_bw() +
      scale_x_continuous(limits = c(0,1),
                         labels = scales::percent,
                         expand = expansion(.05)) +
      scale_y_discrete(labels = function(x) stringr::str_wrap(x,width = 10)) +
      labs(title = "Model-Estimated Match Probability") +
      theme(axis.title = element_blank())
    
  })
  
  output$aces_trainingFeatureDistribution <- renderPlot({
    
    # browser()
    
    aces_trainingData %>%
      dplyr::select(.outcome,everything()) %>%
      tidyr::pivot_longer(cols = !contains(".outcome")) %>%
      mutate(value = ifelse(name == "cellBased_filteredRatio_ave",value <= 15,
                            ifelse(name == "cellBased_filteredRatio_sd",value <= 15,
                                   ifelse(name == "cellBased_translationDiff",value <= 20,
                                          ifelse(name == "fullScan_neighborhoodSizeAve_ave",value <= 250,
                                                 value)))),
             name = case_when(name == "cellBased_clusterIndTRUE" ~ "Cluster Indicator",
                              name == "cellBased_clusterSize" ~ "Cluster Size",
                              name == "cellBased_differenceCor_ave" ~ "Cell-Based Mean Differences Correlation",
                              name == "cellBased_filteredRatio_ave" ~ "Cell-Based Mean Similarities vs. Differences Ratio",
                              name == "cellBased_filteredRatio_sd" ~ "Cell-Based Similarities vs. Differences Ratio SD",
                              name == "cellBased_neighborhoodSizeAve_ave" ~ "Cell-Based Average Labeled Neighborhood Size",
                              name == "cellBased_neighborhoodSizeSD_ave" ~ "Cell-Based Labeled Neighborhood Size SD",
                              name == "cellBased_pairwiseCompCorMean" ~ "Cell-Based Pairwise Complete Correlation Mean",
                              name == "cellBased_pairwiseCompCorSD" ~ "Cell-Based Pairwise Complete Correlation SD",
                              name == "cellBased_thetaDiff" ~ "Estimated Rotation Difference",
                              name == "cellBased_thetaRotSD" ~ "Cell-Based Rotation SD",
                              name == "cellBased_translationDiff" ~ "Estimated Translation Difference",
                              name == "cellBased_xTransSD" ~ "Cell-Based Vertical Translation SD",
                              name == "cellBased_yTransSD" ~ "Cell-Based Horizontal Translation SD",
                              name == "fullScan_differenceCor_ave" ~ "Full-Scan Differences Correlation",
                              name == "fullScan_filteredRatio_ave" ~ "Full-Scan Similarities vs. Differences Ratio",
                              name == "fullScan_neighborhoodSizeAve_ave" ~ "Full-Scan Average Labeled Neighborhood Size",
                              name == "fullScan_neighborhoodSizeSD_ave" ~ "Full-Scan Labeled Neighborhood Size SD",
                              name == "fullScan_pairwiseCompCorMean" ~ "Full-Scan Pairwise Complete Correlation",
                              TRUE ~ "")) %>%
      ggplot() +
      geom_density(aes(x = value,fill = .outcome),alpha = .5) +
      facet_wrap(~ name,scales = "free",labeller = label_wrap_gen()) +
      theme_bw() +
      theme(legend.position = "bottom") +
      geom_vline(data = allFeatures %>% 
                   pivot_longer(everything()) %>% 
                   mutate(name = case_when(name == "cellBased_clusterIndTRUE" ~ "Cluster Indicator",
                                           name == "cellBased_clusterSize" ~ "Cluster Size",
                                           name == "cellBased_differenceCor_ave" ~ "Cell-Based Mean Differences Correlation",
                                           name == "cellBased_filteredRatio_ave" ~ "Cell-Based Mean Similarities vs. Differences Ratio",
                                           name == "cellBased_filteredRatio_sd" ~ "Cell-Based Similarities vs. Differences Ratio SD",
                                           name == "cellBased_neighborhoodSizeAve_ave" ~ "Cell-Based Average Labeled Neighborhood Size",
                                           name == "cellBased_neighborhoodSizeSD_ave" ~ "Cell-Based Labeled Neighborhood Size SD",
                                           name == "cellBased_pairwiseCompCorMean" ~ "Cell-Based Pairwise Complete Correlation Mean",
                                           name == "cellBased_pairwiseCompCorSD" ~ "Cell-Based Pairwise Complete Correlation SD",
                                           name == "cellBased_thetaDiff" ~ "Estimated Rotation Difference",
                                           name == "cellBased_thetaRotSD" ~ "Cell-Based Rotation SD",
                                           name == "cellBased_translationDiff" ~ "Estimated Translation Difference",
                                           name == "cellBased_xTransSD" ~ "Cell-Based Vertical Translation SD",
                                           name == "cellBased_yTransSD" ~ "Cell-Based Horizontal Translation SD",
                                           name == "fullScan_differenceCor_ave" ~ "Full-Scan Differences Correlation",
                                           name == "fullScan_filteredRatio_ave" ~ "Full-Scan Similarities vs. Differences Ratio",
                                           name == "fullScan_neighborhoodSizeAve_ave" ~ "Full-Scan Average Labeled Neighborhood Size",
                                           name == "fullScan_neighborhoodSizeSD_ave" ~ "Full-Scan Labeled Neighborhood Size SD",
                                           name == "fullScan_pairwiseCompCorMean" ~ "Full-Scan Pairwise Complete Correlation",
                                           TRUE ~ "")),
                 aes(xintercept = value)) +
      labs(title = "Questioned Comparison Feature Values vs. Known-Source Comparison Feature Values",
           fill = "Ground Truth")
    
  })
  
  output$aces_matchProbText <- renderText({
    
    HTML(paste0("<br/>",
                purrr::pmap_chr(matchProbs %>%
                                  dplyr::select(modelLabel,value),
                                ~ {
                                  
                                  paste0("<h3>The ",..1," model estimates a ",round(..2*100,1),"% probability of a match.</h3>")
                                  
                                }) %>%
                  paste0(collapse = "<br/>")))
    
  })
  
  output$aces_featureTable <- renderTable({
    
    return(allFeatures %>% 
             tidyr::pivot_longer(everything(),
                                 names_to = "Feature Name",
                                 values_to = "Feature Value") %>%
             mutate(`Feature Name` = 
                      case_when(`Feature Name` == "cellBased_clusterIndTRUE" ~ "Cluster Indicator",
                                `Feature Name` == "cellBased_clusterSize" ~ "Cluster Size",
                                `Feature Name` == "cellBased_differenceCor_ave" ~ "Cell-Based Mean Differences Correlation",
                                `Feature Name` == "cellBased_filteredRatio_ave" ~ "Cell-Based Mean Similarities vs. Differences Ratio",
                                `Feature Name` == "cellBased_filteredRatio_sd" ~ "Cell-Based Similarities vs. Differences Ratio SD",
                                `Feature Name` == "cellBased_neighborhoodSizeAve_ave" ~ "Cell-Based Average Labeled Neighborhood Size",
                                `Feature Name` == "cellBased_neighborhoodSizeSD_ave" ~ "Cell-Based Labeled Neighborhood Size SD",
                                `Feature Name` == "cellBased_pairwiseCompCorMean" ~ "Cell-Based Pairwise Complete Correlation Mean",
                                `Feature Name` == "cellBased_pairwiseCompCorSD" ~ "Cell-Based Pairwise Complete Correlation SD",
                                `Feature Name` == "cellBased_thetaDiff" ~ "Estimated Rotation Difference",
                                `Feature Name` == "cellBased_thetaRotSD" ~ "Cell-Based Rotation SD",
                                `Feature Name` == "cellBased_translationDiff" ~ "Estimated Translation Difference",
                                `Feature Name` == "cellBased_xTransSD" ~ "Cell-Based Vertical Translation SD",
                                `Feature Name` == "cellBased_yTransSD" ~ "Cell-Based Horizontal Translation SD",
                                `Feature Name` == "fullScan_differenceCor_ave" ~ "Full-Scan Differences Correlation",
                                `Feature Name` == "fullScan_filteredRatio_ave" ~ "Full-Scan Similarities vs. Differences Ratio",
                                `Feature Name` == "fullScan_neighborhoodSizeAve_ave" ~ "Full-Scan Average Labeled Neighborhood Size",
                                `Feature Name` == "fullScan_neighborhoodSizeSD_ave" ~ "Full-Scan Labeled Neighborhood Size SD",
                                `Feature Name` == "fullScan_pairwiseCompCorMean" ~ "Full-Scan Pairwise Complete Correlation",
                                TRUE ~ "")))
    
  })
  
  remove_modal_progress()
  
})