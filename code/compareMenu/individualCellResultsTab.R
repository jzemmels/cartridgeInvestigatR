# show popup when the user clicks the Help button
observeEvent(input$comparisonResultsIndividualCellsHelp,
             {
               
               showModal(modalDialog(
                 title = "Help: Individual Cell Results Tab",easyClose = TRUE,
                 strong("Why would I use this tab?"),
                 "Explore the registrations of individual reference cells.",
                 # "For example, in the 'Results Summary' tab you may identify cells that exhibit erratic behavior",
                 "Use this tab to zoom into a cell and identify potential causes for noteworthy behavior.",
                 br(),
                 br(),
                 strong("What do I need to do before using this tab?"),
                 "Click the 'Perform Comparison' button in the 'Comparison Settings' tab and wait for the comparison to finish.",
                 br(),
                 br(),
                 strong("How do I use this tab?"),
                 " Choose a reference scan that was divided into a grid of cells in the comparison procedure (possibly one or two choices, depending on if the 'Compare in both directions' checkbox was clicked in the previous tab).",
                 br(),
                 br(),
                 "Click on a cell that appears in the plot below to visualize its registration in the other scan.",
                 "A plot will appear showing the cell's alignment in the other scan.",
                 "Next to this will appear a comparison plot showing the similarities and differences between the reference cell and patch in the target scan to which the reference cell aligned.",
                 br(),
                 br(),
                 "These visualizations may help explain why a cell aligns to a specific region in the target scan.",
                 "For example, the comparison plot separates each cell pair, shown in the left column, into similarities, shown in the middle column, and differences, shown in the right column.",
                 br(),
                 br(),
                 strong("What is next?"),
                 "If you would like to draw your own cells on a reference and target scan, move onto the 'Custom Cell' tab.",
                 "Otherwise, if you would like to measure the similarity between the two compared cartridge cases, move onto the Scoring stage."
               ))
               
             })

# on the comparison results tab, the user will select an x3p to explore
output$postComparisonPlot <- renderPlot(bg = "white",{
  
  tmp <- isolate(shiny.r$data)
  
  req(!is.null(tmp$x3p_processed))
  req(input$postComparisonScanSelect)
  
  
  cellGrid <- input$numCells %>%
    stringr::str_split(",") %>%
    .[[1]] %>%
    purrr::map_int(as.integer)
  
  req(shiny.r$comparisonData_refToTarget)
  comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
  
  selectedScan <- input$postComparisonScanSelect
  # if we're performing a self-comparison...
  if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
    
    otherScan <- input$postComparisonScanSelect
    
    compData <- comparisonData_refToTarget
    
  }
  # otherwise there are two disticnt scans selected
  else{
    
    otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$postComparisonScanSelect)]
    
    comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
    
    compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$postComparisonScanSelect)]]
    
  }
  
  referenceCell_df <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == selectedScan) %>%
    pull(x3p_processed) %>%
    .[[1]] %>%
    cmcR::comparison_cellDivision(numCells = cellGrid)
  
  # update the shiny.r object with the data frame containing the divided
  # scan so that this doesn't have to be repeated below
  # shiny.r$referenceCell_df <<- referenceCell_df
  surfaceMat_df <- purrr::map_dfr(referenceCell_df %>%
                                    group_by(cellIndex) %>%
                                    group_split(),
                                  function(dat){
                                    
                                    impressions::x3p_to_dataFrame(dat$cellHeightValues[[1]])%>%
                                      mutate(cellIndex = unique(dat$cellIndex),
                                             tooManyMissing = (mean(is.na(value)) >= input$maxNonMissingProp))
                                    
                                  }) %>%
    filter(cellIndex %in% unique(compData$cellIndex)) %>%
    tidyr::separate(col = cellIndex,into = c("rowInd","colInd"),sep = ", ",remove = TRUE,convert = TRUE)
  
  cellPlt <- surfaceMat_df %>%
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
    scale_alpha_manual(values = c(1,0)) +
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
  
  return(cellPlt)
  
})

observeEvent(input$postComparisonScanSelect,{
  
  req(shiny.r$data)
  req(shiny.r$comparisonData_refToTarget)
  req(input$postComparisonClick$panelvar1)
  req(input$postComparisonClick$panelvar2)
  req(input$postComparisonScanSelect)
  
  output$cellComparisonPlot <- NULL
  output$targetScanCellPlot <- NULL
  
})

# on the Comparison Results tab, the user can click on the reference cell
# plot. We will show the five plot comaprison from the impressions package.
# Note that either scan used in the comparison can be considered in this
# plot, so we need to do some fancy selection to make sure we keep what the
# "reference" and "target" scan given the value of
# input$postComparisonScanSelect
observeEvent(input$postComparisonClick,{
  
  req(shiny.r$data)
  req(shiny.r$comparisonData_refToTarget)
  req(input$postComparisonClick$panelvar1)
  req(input$postComparisonClick$panelvar2)
  req(input$postComparisonScanSelect)
  
  # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Pulling Reference Cell Data")
  
  tmp <- isolate(shiny.r$data)
  
  req(shiny.r$comparisonData_refToTarget)
  comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
  
  selectedScan <- input$postComparisonScanSelect
  # if we're performing a self-comparison...
  if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
    
    otherScan <- input$postComparisonScanSelect
    
    compData <- comparisonData_refToTarget
    
  }
  # otherwise there are two disticnt scans selected
  else{
    
    otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$postComparisonScanSelect)]
    
    comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
    
    compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$postComparisonScanSelect)]]
    
  }
  
  reference <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == selectedScan) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  target <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == otherScan) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  if(!isTRUE(all.equal(reference$header.info$incrementX,target$header.info$incrementX))){
    
    if(reference$header.info$incrementX > target$header.info$incrementX){
      
      target <- x3ptools::x3p_interpolate(target,resx = reference$header.info$incrementX)
      
    }
    else{
      
      reference <- x3ptools::x3p_interpolate(reference,resx = target$header.info$incrementX)
      
    }
  }
  
  cellGrid <- input$numCells %>%
    stringr::str_split(",") %>%
    .[[1]] %>%
    purrr::map_int(as.integer)
  
  # for now, we consider the aligned target cell that maxmizes the
  # pairwise-complete correlation with the selected reference cell
  alignedCellData <- compData %>%
    tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ",remove = FALSE) %>%
    group_by(cellIndex) %>%
    filter(fft_ccf == max(fft_ccf)) %>%
    ungroup()
  
  # visualize the five plot comparison for the user-selected cell
  output$cellComparisonPlot <- renderPlot(bg = "white",{
    
    ret <- alignedCellData %>%
      filter(cellIndex == paste0(input$postComparisonClick$panelvar2,", ",
                                 input$postComparisonClick$panelvar1)) %>%
      dplyr::select('cellHeightValues','alignedTargetCell')
    
    return(impressions::x3p_comparisonPlot(x3p1 = ret$cellHeightValues[[1]],
                                           x3p2 = ret$alignedTargetCell[[1]]))
    
  })
  
  # visualize on the target scan where the aligned target cell is
  output$targetScanCellPlot <- renderPlot(bg = "white",{
    
    # debugonce(cmcPlot_colorChange)
    
    plts <- cmcPlot_colorChange(reference = reference,
                                target = target,
                                cmcClassifs = alignedCellData %>% 
                                  mutate(originalMethod = "CMC"),
                                cellToPlot = paste0(input$postComparisonClick$panelvar2,", ",
                                                    input$postComparisonClick$panelvar1),
                                type = "list")
    
    return(plts[[1]] +
             scale_fill_manual(values = "black"))
    
    # return(plts[[2]] +
    #          scale_fill_manual(values = "black"))
    
  })
  
  # # shinybusy::remove_modal_spinner()
  
})