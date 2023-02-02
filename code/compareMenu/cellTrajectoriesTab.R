##################################  Code for the Cell Trajectories tab


observeEvent(input$cellTrajectoryScan,{
  
  tmp <- isolate(shiny.r$data)
  
  req(!is.null(tmp$x3p_processed))
  req(input$cellTrajectoryScan)
  
  output$cellTrajectoryAnimation <- NULL
  
  cellGrid <- input$numCells %>%
    stringr::str_split(",") %>%
    .[[1]] %>%
    purrr::map_int(as.integer)
  
  req(shiny.r$comparisonData_refToTarget)
  comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
  
  selectedScan <- input$cellTrajectoryScan
  # if we're performing a self-comparison...
  if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
    
    otherScan <- input$cellTrajectoryScan
    
    compData <- comparisonData_refToTarget
    
  }
  # otherwise there are two disticnt scans selected
  else{
    
    otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$cellTrajectoryScan)]
    
    comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
    
    compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$cellTrajectoryScan)]]
    
  }
  
  referenceCell_df <- tmp %>%
    mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pNames == selectedScan) %>%
    pull(x3p_processed) %>%
    .[[1]] %>%
    cmcR::comparison_cellDivision(numCells = cellGrid)
  
  targetScan <- tmp %>%
    mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pNames == otherScan) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  targetCell_df <- targetScan %>%
    cmcR::comparison_cellDivision(numCells = cellGrid)
  
  output$cellTrajectoryFullScanPlot <- renderPlot(bg = "white",{
    
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
  
  updateSelectInput(inputId = "cellTrajectorySelections",
                    choices = unique(compData$cellIndex))
  
  
  
})

observeEvent(input$cellTrajectoryClick,{
  
  req(shiny.r$data)
  req(shiny.r$comparisonData_refToTarget)
  req(input$cellTrajectoryClick$panelvar1)
  req(input$cellTrajectoryClick$panelvar2)
  req(input$cellTrajectoryScan)
  
  # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Pulling Reference Cell Data")
  
  tmp <- isolate(shiny.r$data)
  
  req(shiny.r$comparisonData_refToTarget)
  comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
  
  if(input$bothDirectionsCheck){
    
    comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
    
    compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$cellTrajectoryScan)]]
    
  }
  else{
    compData <- comparisonData_refToTarget
  }
  
  currentlySelectedCells <- input$cellTrajectorySelections
  
  updateSelectInput(inputId = "cellTrajectorySelections",
                    selected = c(paste0(input$cellTrajectoryClick$panelvar2,", ",
                                        input$cellTrajectoryClick$panelvar1),
                                 currentlySelectedCells))
  
})

observeEvent(input$cellTrajectoryExecute,{
  
  tmp <- isolate(shiny.r$data)
  
  req(!is.null(tmp$x3p_processed))
  req(input$cellTrajectoryScan)
  req(input$cellTrajectorySelections)
  
  output$saveCellTrajectorySpot <- renderUI({
    
    downloadButton("saveCellTrajectoryAnim","Save Animation")
    
  })
  
  cellGrid <- input$numCells %>%
    stringr::str_split(",") %>%
    .[[1]] %>%
    purrr::map_int(as.integer)
  
  req(shiny.r$comparisonData_refToTarget)
  comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
  
  selectedScan <- input$cellTrajectoryScan
  # if we're performing a self-comparison...
  if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
    
    otherScan <- input$cellTrajectoryScan
    
    compData <- comparisonData_refToTarget
    
  }
  # otherwise there are two disticnt scans selected
  else{
    
    otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$cellTrajectoryScan)]
    
    comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
    
    compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$cellTrajectoryScan)]]
    
  }
  
  targetScan <- tmp %>%
    mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    filter(x3pNames == otherScan) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  
  cellTraj <- compData %>%
    filter(cellIndex %in% input$cellTrajectorySelections) %>%
    dplyr::select(theta,alignedTargetCell,cellIndex) %>%
    pmap_dfr(~ {
      
      impressions:::targetCellCorners(..2,..3,..1,"CMC",targetScan)
      
    }) #%>%
  # group_by(cellIndex,theta) %>%
  # summarise(x = mean(x),y = mean(y))
  
  plt <- x3pListPlot(list(targetScan) %>% 
                       set_names(otherScan)) +
    geom_raster(data = cellTraj,
                aes(x=x,y=y),
                alpha = .3,
                inherit.aes = FALSE)
  # geom_line(data = cellTraj,
  #           aes(x=x,y=y,group = cellIndex),size = 1)
  
  for(cellInd in input$cellTrajectorySelections){
    
    plt <- plt +
      geom_text(data = cellTraj %>%
                  filter(cellIndex == cellInd)%>%
                  group_by(cellIndex,theta) %>%
                  summarise(x = mean(x),y = mean(y)),
                aes(x=x,y=y,label=cellIndex),size = 6)
    
  }
  
  plt <- plt +
    theme(legend.position = "none",
          strip.text = element_blank(),
          plot.title = element_text(hjust = .5)) +
    transition_manual(theta,cumulative = FALSE) +
    labs(title = "Rotation: {current_frame} degrees")
  # labs(title = "Rotation: {3*round(frame_along/3)} degrees")
  
  # temporarily update an internal gganimate function to show a custom
  # rendering progress bar in the app
  rlang::env_unlock(env = asNamespace("gganimate"))
  rlang::env_binding_unlock(env = asNamespace("gganimate"))
  assign("draw_frames",my_draw_frames,envir = asNamespace("gganimate"))
  rlang::env_binding_lock(env = asNamespace('gganimate'))
  rlang::env_lock(asNamespace('gganimate'))
  
  output$cellTrajectoryAnimation <- renderImage({
    
    # from https://github.com/thomasp85/gganimate/pull/331
    progress <- shiny::Progress$new(max = 100)
    progress$set(message = "Rendering",value = 0)
    on.exit(progress$close())
    
    updateShinyProgress <- function(det){
      
      progress$inc(1,detail = det)
      
    }
    
    gganimate::anim_save(animation = gganimate::animate(plt,update_progress = updateShinyProgress,duration = length(unique(cellTraj$theta))*2),
                         filename = "www/anim.gif")
    
    # reset the gganimate namespace with the original draw_frames function
    rlang::env_unlock(env = asNamespace("gganimate"))
    rlang::env_binding_unlock(env = asNamespace("gganimate"))
    assign("draw_frames",old_draw_frames,envir = asNamespace("gganimate"))
    rlang::env_binding_lock(env = asNamespace('gganimate'))
    rlang::env_lock(asNamespace('gganimate'))
    
    return(list(src = "www/anim.gif",content = "image/gif"))
    
  },deleteFile = FALSE)
  
})

# save the animation and delete it locally once the saveCellTrajectoryAnim button is clicked

output$saveCellTrajectory <- 
  downloadHandler(
    filename = "cell_animation.gif",
    contentType = "image/gif",
    content = function(file){
      file.copy("www/anim.gif",file)
    }
  )
