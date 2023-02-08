######################################### Code for the Custom Cell tab

# show popup when the user clicks the Help button
observeEvent(input$customCellHelp,
             {
               
               showModal(modalDialog(
                 title = h3("Help: Custom Cell Tab"),
                 easyClose = TRUE,
                 h4(strong("Why would I use this tab?")),
                 "Draw your own cell on a reference scan to compare to a target scan",
                 "For example, you may identify a region of interest on a scan after a visual inspection (e.g., in the Preprocessing stage) or after studying the comparison results (e.g., using the 'Comparison Results' tabs)",
                 br(),
                 br(),
                 h4(strong("What do I need to do before using this tab?")),
                 "Preprocess uploaded scans in the Preprocessing stage or click the 'Skip Automatic Pre-processing' button in the 'Import' tab.",
                 br(),
                 br(),
                 h4(strong("How do I use this tab?")),
                 "Select the type of cell that you would like to draw (either rectangular or hand-drawn).",
                 "Then select a reference scan upon which you will draw your custom cell.",
                 "Select a target scan to which the custom cell will be compared.",
                 "Choose a range of rotation values for the comparison.",
                 br(),
                 br(),
                 "If you chose to draw a rectangular cell, then click and drag your cursor on the reference scan plot that appears.",
                 br(),
                 br(),
                 "Alternatively, if you chose a hand-drawn cell, then click and drag your cursor on the reference scan plot to zoom into a region of the scan.",
                 "Left-click on the zoomed-in plot that appears below to place a point.",
                 "Place three or more points to create a region that will be compared to the target scan.",
                 "To start over, click the 'Reset Hand-drawn Cell' button.",
                 br(),
                 br(),
                 HTML(paste0("Click the ",strong('Compare Custom Cell')," button once you are happy with the custom cell.")),
                 "A plot will appear showing the cell's alignment in the other scan.",
                 "Next to this will appear a comparison plot showing the similarities and differences between the reference cell and patch in the target scan to which the reference cell aligned.",
                 br(),
                 br(),
                 h4(strong("What is next?")),
                 "Move on to the Score stage to compute automatic similarity scores between the two scans."
               ))
               
             })


# initialize and empty df to store manually annotated points
plottedPointsCustomCell_blank <- data.frame(x = numeric(0),
                                            y = numeric(0),
                                            pointNum = numeric(0))
plottedPointsCustomCell <- reactiveValues(dat = plottedPointsCustomCell_blank)

output$customCellFullScanPlot <- renderPlot(bg = "white",{
  
  req(shiny.r$data)
  req(input$customCellSelection != "")
  
  validate(need(input$customCellSelection,"Select a reference scan!"),
           need(input$thetaRangeMin <= input$thetaRangeMax,"Enter a minimum rotation value that is less than the maximum rotation value."))
  
  tmp <- isolate(shiny.r$data)
  
  selectedScan <- input$customCellSelection
  
  reference <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == selectedScan) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  
  customCellPlt <<- x3pListPlot(list(reference) %>% set_names(selectedScan))
  
  
  if(input$customCellType == "Hand-drawn"){
    
    customCellPlt <<- customCellPlt +
      geom_point(data = plottedPointsCustomCell$dat,
                 aes(x=x,y=y)) +
      geom_polygon(data = plottedPointsCustomCell$dat,
                   aes(x=x,y=y),inherit.aes = FALSE,fill = "black",alpha = .2)
    
  }
  
  return(customCellPlt)
  
})

observeEvent(list(input$customCellType,input$customCellSelection,input$targetSelect_customCell),{
  
  req(shiny.r$data)
  # req(shiny.r$comparisonData_refToTarget)
  req(input$customCellSelection)
  req(input$targetSelect_customCell)
  
  output$targetScanCustomCellPlot <- NULL
  output$customCellComparisonPlot <- NULL
  
})

observeEvent(input$customHandDrawnCellClick,{
  
  pts <- isolate(plottedPointsCustomCell$dat)
  
  pts <- bind_rows(pts,
                   data.frame(x = input$customHandDrawnCellClick$x,
                              y = input$customHandDrawnCellClick$y,
                              pointNum = nrow(pts) + 1))
  
  plottedPointsCustomCell$dat <<- pts
  
})

observeEvent(input$customCellBrush,{
  
  output$customHandDrawnCell <- renderPlot(bg = "white",{
    
    req(input$customCellSelection)
    req(input$customCellBrush)
    req(input$customCellType == "Hand-drawn")
    
    validate(need(input$customCellSelection,"Select a reference scan!"))
    
    customCellPlt_zoom <- customCellPlt +
      coord_fixed(xlim = c(unique(input$customCellBrush$xmin),unique(input$customCellBrush$xmax)),
                  ylim = c(unique(input$customCellBrush$ymin),unique(input$customCellBrush$ymax)),
                  expand = FALSE,
                  ratio = 1) +
      geom_point(data = plottedPointsCustomCell$dat,
                 aes(x=x,y=y)) +
      geom_polygon(data = plottedPointsCustomCell$dat,
                   aes(x=x,y=y),inherit.aes = FALSE,fill = "black",alpha = .2) +
      ggrepel::geom_text_repel(data = plottedPointsCustomCell$dat,
                               aes(x=x,y=y,label = pointNum),inherit.aes = FALSE)
    
    return(customCellPlt_zoom)
    
  })
  
})

observeEvent(input$resetHandDrawnCustomCell,{
  
  output$customHandDrawnCell <- NULL
  output$targetScanCustomCellPlot <- NULL
  output$customCellComparisonPlot <- NULL
  
  plottedPointsCustomCell$dat <<- plottedPointsCustomCell_blank
  
})

observeEvent(input$customCellMenuButton,{
  
  shinyjs::toggle(id = "customCellMenu",anim = TRUE)
  
})

# the custom cell analysis doesn't require that the comparison procedure to
# have been completed
observeEvent(input$customCellExecute,{
  
  req(shiny.r$data)
  req(input$customCellBrush$xmin)
  req(input$customCellBrush$ymin)
  req(input$customCellBrush$xmax)
  req(input$customCellBrush$ymax)
  
  tmp <- isolate(shiny.r$data)
  
  selectedScan <- input$customCellSelection
  
  validate(need(input$customCellSelection,"Select a reference scan!"),
           need(input$targetSelect_customCell,"Select a target scan!"),
           need(input$thetaRangeMin_customCell <= input$thetaRangeMax_customCell,"Enter a minimum rotation value that is less than the maximum rotation value."))
  
  reference <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == selectedScan) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  if(input$customCellType == "Rectangular"){
    
    refCell <- reference
    
    topRow <- nrow(refCell$surface.matrix) - round(input$customCellBrush$ymax)
    bottomRow <- nrow(refCell$surface.matrix) - round(input$customCellBrush$ymin)
    
    leftCol <- round(input$customCellBrush$xmin)
    rightCol <- round(input$customCellBrush$xmax)
    
    refCell$surface.matrix <- refCell$surface.matrix[topRow:bottomRow,leftCol:rightCol]
    refCell$header.info$sizeX <- nrow(refCell$surface.matrix)
    refCell$header.info$sizeY <- ncol(refCell$surface.matrix)
    
    refCell$cmcR.info$cellRange <- paste0("rows: ",topRow," - ",bottomRow,", cols: ",leftCol," - ",rightCol)
    
  }
  else{
    
    pts <- isolate(plottedPointsCustomCell$dat)
    
    refCell <- reference %>%
      impressions::x3p_to_dataFrame() %>%
      mutate(inPoly = sp::point.in.polygon(x,y,pts$x,pts$y)) %>%
      mutate(value = ifelse(inPoly == 0,NA,value)) %>%
      rename(xnew = y,
             ynew = x) %>%
      mutate(x=xnew,y=ynew) %>%
      mutate(y = max(y) - y,
             x = max(x) - x) %>%
      dplyr::select('x','y','value') %>%
      x3ptools::df_to_x3p() 
    
    # 
    
    topRow <- min(which(rowSums(!is.na(refCell$surface.matrix)) > 0))
    bottomRow <- max(which(rowSums(!is.na(refCell$surface.matrix)) > 0))
    
    leftCol <- min(which(colSums(!is.na(refCell$surface.matrix)) > 0))
    rightCol <- max(which(colSums(!is.na(refCell$surface.matrix)) > 0))
    
    refCell$header.info$incrementX <- reference$header.info$incrementX
    refCell$header.info$incrementY <- reference$header.info$incrementY
    
    refCell$cmcR.info$cellRange <- paste0("rows: ",topRow," - ",bottomRow,", cols: ",leftCol," - ",rightCol)
    
    refCell <- cmcR:::preProcess_cropWS(refCell,croppingProp = .5)
    
  }
  
  target <- tmp %>%
    #mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pName == input$targetSelect_customCell) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  if(!isTRUE(all.equal(refCell$header.info$incrementX,target$header.info$incrementX))){
    
    if(refCell$header.info$incrementX > target$header.info$incrementX){
      
      target <- x3ptools::x3p_interpolate(target,resx = refCell$header.info$incrementX)
      
    }
    else{
      
      refCell <- x3ptools::x3p_interpolate(refCell,resx = target$header.info$incrementX)
      
    }
  }
  
  thetas <- seq(from = input$thetaRangeMin_customCell,
                to = input$thetaRangeMax_customCell,
                by = input$thetaStep_customCell)
  
  show_modal_progress_line(text = paste0("Comparing custom cell to ",  input$targetSelect_customCell," at rotation ",thetas[1],"°"),value = 0)
  
  compData <- map2_dfr(thetas,
                       1:length(thetas),
                       function(theta,ind){
                         
                         dat <- comparison_customCell(refCell,
                                                      target,
                                                      theta,
                                                      sideLengthMultiplier = input$cellRegionProp_customCell)
                         
                         update_modal_progress(value = (ind)*(1/length(thetas)),
                                               text = paste0("Comparing custom cell to ", input$targetSelect_customCell," at rotation ",theta,"°"))
                         
                         return(dat)
                         
                       })
  
  update_modal_progress(value = 1,
                        text = paste0("Comparing custom cell to ",  input$targetSelect_customCell," at rotation ",thetas[[length(thetas)]],"°"))
  
  shiny.r$customCellResults <<- compData
  
  # 
  
  # for now, we consider the aligned target cell that maxmizes the
  # pairwise-complete correlation with the selected reference cell
  alignedCellData <- compData %>%
    tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ",remove = FALSE) %>%
    filter(fft_ccf == max(fft_ccf))
  
  
  # visualize the five plot comparison for the user-selected cell
  output$customCellComparisonPlot <- renderPlot(bg = "white",{
    
    ret <- alignedCellData %>%
      dplyr::select('cellHeightValues','alignedTargetCell')
    
    return(impressions::x3p_comparisonPlot(x3p1 = ret$cellHeightValues[[1]],
                                           x3p2 = ret$alignedTargetCell[[1]]))
    
  })
  
  
  # visualize on the target scan where the aligned target cell is
  output$targetScanCustomCellPlot <- renderPlot(bg = "white",{
    
    plts <- cmcPlot_colorChange(reference = reference,
                                target = target,
                                cmcClassifs = alignedCellData %>%
                                  mutate(originalMethod = "CMC"),
                                targetName = input$targetSelect_customCell,
                                type = "list")
    
    return(plts[[1]] +
             scale_fill_manual(values = "black"))
    
  })
  
  remove_modal_progress()
})