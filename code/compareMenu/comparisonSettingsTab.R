################################################# Code for Comparison Settings tab

# show popup when the user clicks the Help button
observeEvent(input$comparisonParametersHelp,
             {
               
               showModal(modalDialog(
                 title = h3("Help: Cell Grid Comparison Tab"),
                 easyClose = TRUE,
                 h4(strong("Why would I use this tab?")),
                 "Compare two processed cartridge cases.",
                 "This tab is primarily useful for doing a exploring specific regions of a cartridge case or investigating the behavior of the cell-based comparison procedure.",
                 "Move to the Score stage if you are primarily interested in computing similarity scores for two cartridge cases.",
                 br(),
                 br(),
                 h4(strong("What do I need to do before using this tab?")),
                 "Complete the Import + Pre-processing stage.",
                 br(),
                 br(),
                 h4(strong("How do I use this tab?")),
                 "Select a reference scan to be divided into a grid of cells.",
                 "Select a target scan to which each reference cell will be compared.",
                 HTML(paste0("Click the ",strong("Settings")," button to set various parameters for the cell-based comparison procedure.")),
                 HTML(paste0("Click the ",strong('Perform Comparison')," button when you are happy with the comparison procedure parameters.")),
                 br(),
                 br(),
                 h4(strong("What is next?")),
                 "Move to the Results Summary or Individual Cell Results to explore results from the comparison procedure."
               ))
               
             })

observeEvent(input$previewComparison,{
  
  output$comparison1text_ui <- renderUI({
    
    h3(paste0("Comparison 1: ",input$referenceSelect," vs. ",input$targetSelect))
    
  })
  
  # plot the reference scan broken up into cells
  output$preComparisonReference <- renderPlot({
    
    req(input$referenceSelect != "")
    
    validate(need(input$referenceSelect,"Select a reference scan!"),
             need(0 <= input$maxNonMissingProp & input$maxNonMissingProp <= 1,"Enter a value between 0 and 1 to Maxmimum Proportion of NAs per Cell"))
    
    # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Preparing Reference Scan")
    
    tmp <- isolate(shiny.r$data)
    
    req(!is.null(tmp$x3p_processed))
    
    cellGrid <- input$numCells %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_int(as.integer)
    
    validate(need(length(cellGrid) == 2,"Enter cell grid size as two comma-separated numbers (e.g., 8,8)"))
    
    referenceCell_df <- tmp %>%
      # mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pName == input$referenceSelect) %>%
      pull(x3p_processed) %>%
      .[[1]] %>%
      cmcR::comparison_cellDivision(numCells = cellGrid)
    
    # update the shiny.r object with the data frame containing the divided
    # scan so that this doesn't have to be repeated below
    # shiny.r$referenceCell_df <<- referenceCell_df
    
    surfaceMat_df <- referenceCell_df %>%
      group_by(cellIndex) %>%
      group_split() %>%
      purrr::map_dfr(function(dat){
        
        x3p_to_dataFrame(dat$cellHeightValues[[1]]) %>%
          mutate(cellIndex = unique(dat$cellIndex),
                 tooManyMissing = (mean(is.na(value)) >= input$maxNonMissingProp))
        
      }) %>%
      tidyr::separate(col = cellIndex,into = c("rowInd","colInd"),sep = ",",remove = TRUE,convert = TRUE)
    
    cellPlt_refToTarget <- surfaceMat_df %>%
      ggplot(aes(x=x,y=y,fill = value,alpha=tooManyMissing)) +
      geom_raster() +
      facet_grid(rows = vars(rowInd),
                 cols = vars(colInd)) +
      ggplot2::scale_fill_gradientn(colours = c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb", "#f7f7f7", "#fee0b6", "#fdb863" ,"#e08214" ,"#b35806","#7f3b08"),
                                    values = scales::rescale(quantile(surfaceMat_df$value,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                    breaks = function(lims){
                                      dat <- quantile(surfaceMat_df$value,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                      
                                      dat <- dat %>%
                                        setNames(paste0(names(dat)," [",round(dat,3),"]"))
                                      
                                      return(dat)
                                    },
                                    na.value = "gray65") +
      scale_alpha_manual(values = c(1,.2)) +
      theme_minimal() +
      coord_fixed(expand = FALSE) +
      theme(axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(hjust = .5,
                                               size = 11)
            ,legend.position = "none") +
      ggplot2::labs(title = input$referenceSelect) +
      ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(3,"in"),
                                                      label.theme = ggplot2::element_text(size = 8),
                                                      title.theme = ggplot2::element_text(size = 10),
                                                      frame.colour = "black",
                                                      ticks.colour = "black"),
                      colour =  'none')
    
    # shinybusy::remove_modal_spinner()
    
    return(cellPlt_refToTarget)
    
  })
  
  # plot the scan to be used as target
  output$preComparisonWholeScanTarget <- renderPlot({
    
    req(input$targetSelect != "")
    
    need(input$targetSelect,"Select a target scan!")
    
    # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Visualizing Target Scan")
    
    tmp <- isolate(shiny.r$data)
    
    req(!is.null(tmp$x3p_processed))
    
    target <- tmp %>%
      # mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pName == input$targetSelect) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    plt <- x3pListPlot(list(target) %>% purrr::set_names(input$targetSelect)) +
      theme(legend.position = "none")
    
    # shinybusy::remove_modal_spinner()
    
    return(plt)
    
  })
  
  if(input$bothDirectionsCheck){
    
    
    output$comparison2text_ui <- renderUI({
      
      h3(paste0("Comparison 2: ",input$targetSelect," vs. ",input$referenceSelect))
      
    })
    
    req(shiny.r$data)
    req(shiny.r$data$x3p_processed)
    
    validate(need(input$referenceSelect,"Select a reference scan!"),
             need(input$targetSelect,"Select a target scan!"),
             need(0 <= input$maxNonMissingProp & input$maxNonMissingProp <= 1,"Enter a value between 0 and 1 to Maxmimum Proportion of NAs per Cell"))
    
    tmp <- isolate(shiny.r$data)
    
    # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Preparing Target Scan")
    
    # plot of target scan broken into cells
    output$preComparisonTarget <- renderPlot({
      
      cellGrid <- input$numCells %>%
        stringr::str_split(",") %>%
        .[[1]] %>%
        purrr::map_int(as.integer)
      
      targetCell_df <- tmp %>%
        # mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
        filter(x3pName == input$targetSelect) %>%
        pull(x3p_processed) %>%
        .[[1]] %>%
        cmcR::comparison_cellDivision(numCells = cellGrid)
      
      # update the shiny.r object with the data frame containing the divided
      # scan so that this doesn't have to be repeated below
      # shiny.r$targetCell_df <<- targetCell_df
      
      surfaceMat_df <- targetCell_df %>%
        group_by(cellIndex) %>%
        group_split() %>%
        purrr::map_dfr(function(dat){
          
          x3p_to_dataFrame(dat$cellHeightValues[[1]]) %>%
            mutate(cellIndex = unique(dat$cellIndex),
                   tooManyMissing = (mean(is.na(value)) >= input$maxNonMissingProp))
          
        }) %>%
        tidyr::separate(col = cellIndex,into = c("rowInd","colInd"),sep = ", ",remove = TRUE,convert = TRUE)
      
      cellPlt_targetToRef <- surfaceMat_df %>%
        ggplot(aes(x=x,y=y,fill = value,alpha = tooManyMissing)) +
        geom_raster() +
        facet_grid(rows = vars(rowInd),
                   cols = vars(colInd)) +
        ggplot2::scale_fill_gradientn(colours = c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb", "#f7f7f7", "#fee0b6", "#fdb863" ,"#e08214" ,"#b35806","#7f3b08"),
                                      values = scales::rescale(quantile(surfaceMat_df$value,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                      breaks = function(lims){
                                        dat <- quantile(surfaceMat_df$value,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                        
                                        dat <- dat %>%
                                          setNames(paste0(names(dat)," [",round(dat,3),"]"))
                                        
                                        return(dat)
                                      },
                                      na.value = "gray65") +
        scale_alpha_manual(values = c(1,.2)) +
        theme_minimal() +
        coord_fixed(expand = FALSE) +
        theme(axis.title.x = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_blank(),
              axis.ticks.x = ggplot2::element_blank(),
              axis.title.y = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              plot.title = ggplot2::element_text(hjust = .5,
                                                 size = 11)
              ,legend.position = "none") +
        ggplot2::labs(title = input$targetSelect) +
        ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(3,"in"),
                                                        label.theme = ggplot2::element_text(size = 8),
                                                        title.theme = ggplot2::element_text(size = 10),
                                                        frame.colour = "black",
                                                        ticks.colour = "black"),
                        colour =  'none')
      
      # shinybusy::remove_modal_spinner()
      
      return(cellPlt_targetToRef)
      
    })
    
    # plot of full reference scan
    output$preComparisonWholeScanReference <- renderPlot({
      
      req(input$referenceSelect != "")
      
      # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Visualizing Reference Scan")
      
      tmp <- isolate(shiny.r$data)
      
      req(!is.null(tmp$x3p_processed))
      
      target <- tmp %>%
        # mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
        filter(x3pName == input$referenceSelect) %>%
        pull(x3p_processed) %>%
        .[[1]]
      
      plt <- x3pListPlot(list(target) %>% purrr::set_names(input$referenceSelect)) +
        theme(legend.position = "none")
      
      # shinybusy::remove_modal_spinner()
      
      return(plt)
      
    })
    
  }
  
})

observeEvent(input$bothDirectionsCheck,{
  
  if(input$bothDirectionsCheck){
    
    # allow for calculation of ACES features if both direction button is checked
    updateSelectInput(session = session,inputId = input$comparisonSummary_featureSelect,
                      choices = c("Cell-wise translations by rotations scatterplots",
                                  "Cell-wise registrations dot plots",
                                  "Registration-based features",
                                  "Density-based features",
                                  "Visual diagnostic features"))
    
  }
  else{
    
    # allow for calculation of ACES features if both direction button is checked
    updateSelectInput(session = session,inputId = input$comparisonSummary_featureSelect,
                      choices = c("Cell-wise translations by rotations scatterplots",
                                  "Cell-wise registrations dot plots"))
    
  }
  
})

# Toggle the settings menu

observeEvent(input$comparisonSettingsMenuButton,{
  
  shinyjs::toggle(id = "comparisonSettingsMenu",anim = TRUE)
  
})

# perform the comparison procedure (possibly in both directions) once the
# comparisonButton is pressed
observeEvent(input$comparisonButton,{
  
  req(shiny.r$data)
  req(shiny.r$data$x3p_processed)
  
  validate(need(input$referenceSelect,"Select a reference scan!"),
           need(input$targetSelect,"Select a target scan!"),
           need(0 <= input$maxNonMissingProp & input$maxNonMissingProp <= 1,"Enter a value between 0 and 1 to Maxmimum Proportion of NAs per Cell"),
           need(input$thetaRangeMin <= input$thetaRangeMax,"Enter a minimum rotation value that is less than the maximum rotation value."))
  
  # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Performing Comparisons")
  
  thetas <- seq(from = input$thetaRangeMin,to = input$thetaRangeMax,by = input$thetaStep)
  
  tmp <- isolate(shiny.r$data)
  
  cellGrid <- input$numCells %>%
    stringr::str_split(",") %>%
    .[[1]] %>%
    purrr::map_int(as.integer)
  
  validate(need(length(cellGrid) == 2,"Enter cell grid size as two comma-separated numbers (e.g., 8,8)"))
  
  reference <- tmp %>%
    # mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == input$referenceSelect) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  target <- tmp %>%
    # mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == input$targetSelect) %>%
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
  
  show_modal_progress_line(text = paste0("Comparing reference vs. target scans at rotation ",thetas[1],"°"),value = 0)
  
  comparisonData_refToTarget <- 
    purrr::map2_dfr(thetas,
                    1:length(thetas),
                    function(theta,ind){
                      
                      dat <- reference %>% 
                        comparison_cellDivision(cellGrid) %>% 
                        dplyr::mutate(cellPropMissing = comparison_calcPropMissing(cellHeightValues), 
                                      refMissingCount = purrr::map_dbl(cellHeightValues, 
                                                                       ~sum(is.na(.$surface.matrix)))) %>%
                        filter(cellPropMissing <= input$maxNonMissingProp) %>% 
                        dplyr::mutate(regionHeightValues = comparison_getTargetRegions(cellHeightValues = cellHeightValues, 
                                                                                       target = target, theta = theta, 
                                                                                       sideLengthMultiplier = input$cellRegionProp)) %>% 
                        dplyr::mutate(targMissingProp = comparison_calcPropMissing(regionHeightValues), 
                                      targMissingCount = purrr::map_dbl(regionHeightValues, 
                                                                        ~sum(is.na(.$surface.matrix)))) %>% 
                        dplyr::filter(targMissingProp <= input$maxNonMissingProp) 
                      
                      if(nrow(dat) == 0){
                        
                        remove_modal_progress()
                        
                        showNotification("Comparison could not be completed. Try increasing the Maximum Proportion of NAs per Cell.",type = "error")
                        
                        validate(need(nrow(dat) > 0,"Comparison could not be completed. Try increasing the Maximum Proportion of NAs per Cell."))
                        
                      }
                      
                      dat <- dat %>% 
                        dplyr::mutate(cellHeightValues = comparison_standardizeHeights(cellHeightValues), 
                                      regionHeightValues = comparison_standardizeHeights(regionHeightValues)) %>% 
                        dplyr::mutate(cellHeightValues_replaced = comparison_replaceMissing(cellHeightValues), 
                                      regionHeightValues_replaced = comparison_replaceMissing(regionHeightValues)) %>% 
                        dplyr::mutate(fft_ccf_df = comparison_fft_ccf(cellHeightValues = cellHeightValues_replaced, 
                                                                      regionHeightValues = regionHeightValues_replaced)) %>% 
                        dplyr::mutate(alignedTargetCell = comparison_alignedTargetCell(cellHeightValues = cellHeightValues, 
                                                                                       regionHeightValues = regionHeightValues, target = target, 
                                                                                       theta = theta, fft_ccf_df = fft_ccf_df)) %>% 
                        dplyr::mutate(jointlyMissing = purrr::map2_dbl(cellHeightValues, 
                                                                       alignedTargetCell, ~sum(is.na(.x$surface.matrix) & 
                                                                                                 is.na(.y$surface.matrix))), 
                                      pairwiseCompCor = purrr::map2_dbl(cellHeightValues, 
                                                                        alignedTargetCell, ~cor(c(.x$surface.matrix), 
                                                                                                c(.y$surface.matrix), use = "pairwise.complete.obs"))) %>% 
                        tidyr::unnest(fft_ccf_df) %>% 
                        dplyr::mutate(theta = theta) %>%
                        dplyr::select('cellIndex','x', 
                                      'y','fft_ccf','pairwiseCompCor','theta', 
                                      'refMissingCount','targMissingCount','jointlyMissing', 
                                      'cellHeightValues','alignedTargetCell')
                      
                      update_modal_progress(value = (ind)*(1/length(thetas)),
                                            text = paste0("Comparing reference vs. target scans at rotation ",theta,"°"))
                      
                      return(dat)
                    })
  
  update_modal_progress(value = .99,
                        text = paste0("Comparing reference vs. target scans at rotation ",thetas[[length(thetas)]],"°"))
  
  # remove_modal_progress()
  
  shiny.r$comparisonData_refToTarget <<- comparisonData_refToTarget
  
  updateSelectInput(session = session,
                    inputId = "comparisonSummary_referenceSelect",
                    choices = c(input$referenceSelect))
  
  updateSelectInput(session = session,
                    inputId = "postComparisonScanSelect",
                    choices = c(input$referenceSelect))
  
  updateSelectInput(session = session,
                    inputId = "cellTrajectoryScan",
                    choices = c(input$referenceSelect))
  
  # updateSelectInput(session = session,
  #                   inputId = "customCellSelection",
  #                   choices = c("",input$referenceSelect))
  
  updateSelectInput(session = session,
                    inputId = "comparisonSummary_rotations",
                    choices = thetas,
                    selected = thetas)
  
  showTab(inputId = "comparingTabs",target = "Results Summary",session = session)
  showTab(inputId = "comparingTabs",target = "Individual Cell Results",session = session)
  
  updateSelectInput(session = session,
                    inputId = "cmcMethodReferenceSelect",
                    choices = c("",input$referenceSelect))
  
  updateSelectInput(session = session,
                    inputId = "cmcMethodTargetSelect",
                    choices = c("",input$targetSelect))
  
  # perform the comparison in both directions if 
  if(input$bothDirectionsCheck){
    
    show_modal_progress_line(text = "Comparing target vs. reference scans at rotation ",thetas[1],"°",value = 0)
    
    comparisonData_targetToRef <- 
      purrr::map2_dfr(thetas,
                      1:length(thetas),
                      function(theta,ind){
                        
                        dat <- cmcR::comparison_allTogether(reference = target,
                                                            target = reference,
                                                            theta = theta,
                                                            numCells = cellGrid,
                                                            maxMissingProp = input$maxNonMissingProp,
                                                            sideLengthMultiplier = input$cellRegionProp,
                                                            returnX3Ps = TRUE)
                        
                        update_modal_progress(value = (ind)*(1/length(thetas)),
                                              text = paste0("Comparing target vs. reference scans at rotation ",theta,"°"))
                        
                        return(dat)
                      })
    
    update_modal_progress(value = 1,
                          text = paste0("Comparing target vs. reference scans at rotation ",thetas[[length(thetas)]],"°"))
    
    shiny.r$comparisonData_targetToRef <<- comparisonData_targetToRef
    
    shiny.r$acesFeatures <<- bind_rows(comparisonData_refToTarget  %>%
                                         dplyr::mutate(direction = "reference_vs_target"),
                                       comparisonData_targetToRef %>%
                                         dplyr::mutate(direction = "target_vs_reference")) %>%
      mutate(comparisonName = "reference_vs_target") %>%
      dplyr::group_by(comparisonName,direction) %>%
      scored::feature_aLaCarte(features = "all",eps = 5,minPts = 4)
    
    updateSelectInput(session = session,
                      inputId = "postComparisonScanSelect",
                      choices = c(input$referenceSelect,input$targetSelect))
    
    updateSelectInput(session = session,
                      inputId = "cellTrajectoryScan",
                      choices = c(input$referenceSelect,input$targetSelect))
    
    updateSelectInput(session = session,
                      inputId = "comparisonSummary_referenceSelect",
                      choices = c(input$referenceSelect,input$targetSelect))
    
    updateSelectInput(session = session,
                      inputId = "cmcMethodReferenceSelect",
                      choices = c(input$referenceSelect,input$targetSelect))
    
    updateSelectInput(session = session,
                      inputId = "cmcMethodTargetSelect",
                      choices = c(input$referenceSelect,input$targetSelect))
    
  }
  
  remove_modal_progress()
  
})

