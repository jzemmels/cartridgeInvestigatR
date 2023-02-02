################################### code for Results Summary tab

# show popup when the user clicks the Help button
observeEvent(input$comparisonResultsSummaryHelp,
             {
               
               showModal(modalDialog(
                 title = "Help: Results Summary Tab",easyClose = TRUE,
                 strong("Why would I use this tab?"),
                 "Explore the distribution of similarity features extracted from the comparison procedure executed in the 'Comparison Settings' tab.",
                 br(),
                 br(),
                 strong("What do I need to do before using this tab?"),
                 "Click the 'Perform Comparison' button in the 'Comparison Settings' tab and wait for the comparison to finish.",
                 br(),
                 br(),
                 strong("How do I use this tab?"),
                 "Choose a reference scan that was divided into a grid of cells in the comparison procedure (possibly one or two choices, depending on if the 'Compare in both directions' checkbox was clicked in the previous tab).",
                 "The plots that appear depict the similarity features extracted during the cell-based comparison procedure.",
                 br(),
                 br(),
                 "In the top-left is a scatterplot of the estimated translations for each cell (represented as points), faceted by rotation angle.",
                 br(),
                 br(),
                 "In the bottom-left is a dot plot of the features that maximize the pairwise-complete correlation for each cell.",
                 "Consider these features as each cell's top vote for its registration in the target scan",
                 br(),
                 br(),
                 "In the top-right is a visualization of the reference scan with an overlaid cell grid.",
                 br(),
                 br(),
                 "In the bottom-right is a visualization of the target scan with cells located at the registration that maximizes the pairwise-complete correlation.",
                 br(),
                 br(),
                 "All four of these plots are interactable: clicking on one element will highlight elements in the other plots corresponding to the same cell.",
                 br(),
                 br(),
                 strong("What is next?"),
                 # "To study the registration of individual cells, move onto the 'Individual Cell Results' tab.",
                 "To draw your own cells on a scan, move onto the 'Custom Cell' tab.",
                 "Otherwise, if you would like to measure the similarity between the two compared cartridge cases, move onto the Scoring stage."
               ))
               
             })

observeEvent(input$comparisonSummary_referenceSelect,{
  
  output$comparisonSummary_histograms <-
    renderGirafe({
      # renderPlot({
      
      req(shiny.r$data)
      req(input$comparisonSummary_referenceSelect)
      req(shiny.r$comparisonData_refToTarget)
      req(input$comparisonSummary_featureSelect)
      
      tmp <- isolate(shiny.r$data)
      comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
      
      selectedScan <- input$comparisonSummary_referenceSelect
      # if we're performing a self-comparison...
      if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
        
        otherScan <- input$comparisonSummary_referenceSelect
        
        compData <- comparisonData_refToTarget
        
      }
      # otherwise there are two disticnt scans selected
      else{
        
        otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$comparisonSummary_referenceSelect)]
        
        comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
        
        compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$comparisonSummary_referenceSelect)]]
        
      }
      
      reference <- tmp %>%
        # #mutate(x3pName  = paste0("x3p",1:nrow(.))) %>%
        dplyr::filter(x3pName == selectedScan) %>%
        pull(x3p_processed) %>%
        .[[1]]
      
      target <- tmp %>%
        #mutate(x3pName  = paste0("x3p",1:nrow(.))) %>%
        dplyr::filter(x3pName == otherScan) %>%
        pull(x3p_processed) %>%
        .[[1]]
      
      cmcPlts <- cmcPlot_interactive(reference = reference,
                                     target = target,
                                     cmcClassifs = compData %>%
                                       group_by(cellIndex) %>%
                                       dplyr::filter(fft_ccf == max(fft_ccf)) %>%
                                       ungroup() %>%
                                       mutate(originalMethod = "CMC"),
                                     type = "list")
      
      if("Cell-wise translations by rotations scatterplots" %in% input$comparisonSummary_featureSelect){
        
        # choose a sensible number of rows based on the number of rotations to plot
        numThetas <- length(seq(from = input$thetaRangeMin,
                                to = input$thetaRangeMax,
                                by = input$thetaStep))
        
        numRows <- 1
        if(numThetas > 1){
          
          possibleNumRows <- map_lgl(2:ceiling(sqrt(numThetas)),
                                     function(val){
                                       
                                       return((numThetas %% val) == 0)
                                       
                                     })
          
          numRows <- min((2:ceiling(sqrt(numThetas)))[possibleNumRows])
        }
        
        scatterplot_transByRot <- compData %>%
          # dplyr::filter(theta %in% input$comparisonSummary_rotations) %>%
          mutate(data_id = paste0(cellIndex,", ",theta)) %>%
          dplyr::select('cellIndex','data_id','x','y','theta','fft_ccf','pairwiseCompCor') %>%
          ggplot(aes(x=x,y=y)) +#,alpha=pairwiseCompCor)) +
          # geom_point() +
          geom_jitter_interactive(alpha = .5,size = 3,
                                  width = 0,
                                  height= 0,
                                  aes(
                                    # data_id = data_id
                                    tooltip = cellIndex,
                                    data_id = cellIndex
                                  )) +
          labs(title = "Cell-wise translations by rotations")
        
        if(!is.infinite(numRows)){
          
          scatterplot_transByRot <- scatterplot_transByRot +
            facet_wrap(~ theta, nrow = numRows) +
            theme_bw() +
            coord_fixed()
          
        }
        else{
          
          scatterplot_transByRot <- scatterplot_transByRot +
            facet_wrap(~ theta) +
            theme_bw() +
            coord_fixed()
          
        }
        
      }
      else{
        scatterplot_transByRot <- NULL
      }
      
      if("Cell-wise registrations dot plots" %in% input$comparisonSummary_featureSelect){
        
        dat <- compData %>%
          group_by(cellIndex) %>%
          dplyr::filter(fft_ccf == max(fft_ccf)) %>%
          ungroup() %>%
          mutate(data_id = paste0(cellIndex,", ",theta)) %>%
          dplyr::select('cellIndex','data_id','x','y','theta','fft_ccf','pairwiseCompCor') %>%
          pivot_longer(cols = 3:7,names_to = "var",values_to = "value") %>%
          dplyr::filter(var != "fft_ccf") %>%
          mutate(varLabel = factor(case_when(var == "pairwiseCompCor" ~ "Pairwise-complete Correlation",
                                             var == "x" ~ "Horizontal Translation",
                                             var == "y" ~ "Vertical Translation",
                                             var == "theta" ~ "Rotation"),
                                   levels = c("Pairwise-complete Correlation",
                                              "Rotation",
                                              "Horizontal Translation","Vertical Translation")))
        
        dotPlot_cellwiseReg <- dat %>%
          ggplot(aes(x=value)) +
          geom_dotplot_interactive(aes(tooltip = cellIndex,
                                       data_id = cellIndex
                                       # data_id = data_id
          ),
          # size = 2,
          stackgroups = TRUE,
          binpositions = "all",
          stackratio = 1.1) +
          facet_wrap(~varLabel,
                     # space = "free",
                     scales = "free") +
          theme_bw() +
          labs(title = "Distributions of cell-wise registrations")  +
          # adjust facet limits
          geom_blank(data = data.frame(value = 
                                         c(dat %>%
                                             dplyr::filter(var %in% c("pairwiseCompCor")) %>% pull(value) %>% range(),
                                           dat %>%
                                             dplyr::filter(var %in% c("x","y")) %>% pull(value) %>% range() %>% rep(times = 2),
                                           dat %>%
                                             dplyr::filter(var %in% c("theta")) %>% pull(value) %>% range()),
                                       varLabel = factor(c("Pairwise-complete Correlation","Pairwise-complete Correlation",
                                                           "Horizontal Translation","Horizontal Translation",
                                                           "Vertical Translation","Vertical Translation",
                                                           "Rotation","Rotation"),
                                                         levels = c("Pairwise-complete Correlation",
                                                                    "Rotation",
                                                                    "Horizontal Translation","Vertical Translation"))))
        
      }
      else{
        dotPlot_cellwiseReg <- NULL
      }
      
      if(any(map_lgl(c("Registration-based features","Density-based features","Visual Diagnostic Features"),
                     ~ any(stringr::str_detect(.,input$comparisonSummary_featureSelect))))){
       
        load("models/logisticReg_allAces.RData")
        
        acesFeatures_combined <- isolate(shiny.r$acesFeatures) %>%
          mutate(clusterInd = as.numeric(clusterInd)) %>%
          dplyr::summarize(across(tidyselect::where(is.numeric),~ mean(.)))
        
        names(acesFeatures_combined) <- paste0("cellBased_",names(acesFeatures_combined))
         
      }
      
      if("Registration-based features" %in% input$comparisonSummary_featureSelect){
        
        registrationFeatures <- acesFeatures_combined %>%
          dplyr::select(cellBased_pairwiseCompCorAve,cellBased_pairwiseCompCorSD,
                        cellBased_xTransSD,cellBased_yTransSD,cellBased_thetaRotSD) %>%
          tidyr::pivot_longer(cols = everything()) %>%
          filter(!is.na(value))
        
        registrationFeaturePlt <- logisticReg_allACES$data %>%
          dplyr::rename(cellBased_pairwiseCompCorAve = cellBased_pairwiseCompCorMean) %>%
          dplyr::select(.outcome,
                        cellBased_pairwiseCompCorAve,cellBased_pairwiseCompCorSD,
                        cellBased_xTransSD,cellBased_yTransSD,cellBased_thetaRotSD) %>%
          tidyr::pivot_longer(2:6) %>%
          mutate(name = case_when(name == "cellBased_pairwiseCompCorAve" ~ "Average Pairwise-Complete Correlation",
                                  name == "cellBased_pairwiseCompCorSD" ~ "Pairwise-Complete Correlation SD",
                                  name == "cellBased_xTransSD" ~ "Horizontal Translation SD",
                                  name == "cellBased_yTransSD"~ "Vertical Translation SD",
                                  name == "cellBased_thetaRotSD" ~ "Rotation SD")) %>%
          
          ggplot() +
          geom_density(aes(x = value,fill = .outcome),alpha = .5) +
          theme_bw() +
          facet_wrap(~ name,scales = "free") +
          theme(legend.position = "bottom") +
          geom_vline(data = registrationFeatures,
                     aes(xintercept = value)) +
          labs(title = "Registration-based Features",
               fill = "Comparison Type")
        
      }
      else{
        
        registrationFeaturePlt <- NULL
        
      }
      
      if("Density-based features" %in% input$comparisonSummary_featureSelect){
        
        densityFeatures <- acesFeatures_combined %>%
          dplyr::select(cellBased_clusterInd,cellBased_clusterSize,
                        cellBased_thetaDiff,cellBased_translationDiff) %>%
          tidyr::pivot_longer(cols = everything()) %>%
          filter(!is.na(value))
        
        densityFeaturePlt <- logisticReg_allACES$data %>%
          dplyr::select(.outcome,
                        cellBased_clusterIndTRUE,cellBased_clusterSize,
                        cellBased_thetaDiff,cellBased_translationDiff) %>%
          tidyr::pivot_longer(2:5)  %>%
          mutate(name = case_when(name == "cellBased_clusterInd" ~ "Cluster Indicator",
                                  name == "cellBased_clusterSize" ~ "Cluster Size",
                                  name == "cellBased_thetaDiff" ~ "Rotation Difference",
                                  name == "cellBased_translationDiff" ~ "Translation Difference")) %>%
          
          ggplot() +
          geom_density(aes(x = value,fill = .outcome),alpha = .5) +
          theme_bw() +
          facet_wrap(~ name,scales = "free") +
          theme(legend.position = "bottom") +
          geom_vline(data = registrationFeatures,
                     aes(xintercept = value)) +
          labs(title = "Density-based Features",
               fill = "Comparison Type")
        
      }
      else{
        
        densityFeaturePlt <- NULL
        
      }
      
      if("Visual diagnostic features" %in% input$comparisonSummary_featureSelect){
        
        visualDiagnosticFeatures <- acesFeatures_combined %>%
          dplyr::select(.outcome,
                        cellBased_neighborhoodSizeAve_ave,cellBased_neighborhoodSizeSD_ave,
                        cellBased_differenceCor_ave,
                        cellBased_filteredRatio_ave,cellBased_filteredRatio_sd) %>%
          tidyr::pivot_longer(cols = everything()) %>%
          filter(!is.na(value))
        
        visualDiagnosticFeaturePlt <- logisticReg_allACES$data %>%
          dplyr::select(.outcome,
                        cellBased_clusterIndTRUE,cellBased_clusterSize,
                        cellBased_thetaDiff,cellBased_translationDiff) %>%
          tidyr::pivot_longer(2:6)  %>%
          mutate(name = case_when(name == "cellBased_neighborhoodSizeAve_ave" ~ "Average cell-wise neighborhood size",
                                  name == "cellBased_neighborhoodSizeSD_ave" ~ "Average SD of cell-wise neighborhood sizes",
                                  name == "cellBased_differenceCor_ave" ~ "Average cell-wise differences correlation",
                                  name == "cellBased_filteredRatio_ave" ~ "Average cell-based similarities vs. differences ratio",
                                  name == "cellBased_filteredRatio_sd" ~ "SD of the cell-based similarities vs. differences ratio")) %>%
          
          ggplot() +
          geom_density(aes(x = value,fill = .outcome),alpha = .5) +
          theme_bw() +
          facet_wrap(~ name,scales = "free") +
          theme(legend.position = "bottom") +
          geom_vline(data = registrationFeatures,
                     aes(xintercept = value)) +
          labs(title = "Visual Diagnostic Features",
               fill = "Comparison Type")
        
      }
      else{
        
        visualDiagnosticFeaturePlt <- NULL
        
      }
      
      
      library(patchwork)
      
      
      allPlts <- list((cmcPlts[[1]] | cmcPlts[[2]]),
                      scatterplot_transByRot,dotPlot_cellwiseReg,
                      registrationFeaturePlt,densityFeaturePlt,visualDiagnosticFeaturePlt)
      
      
      allPlts <- allPlts[map_lgl(allPlts, ~ !all(is.null(.)))]
      
      return(girafe(code = print(
        wrap_plots(allPlts,ncol = 1)
      ),
      width_svg = 24,height_svg = 15,
      options = list(opts_selection(css = "fill:orange;stroke:orange;color:black;"))))
      
    })
  
})


