x3pActionButtonUI <- function(id, label = "Click me!", col1 = 3, col2 = 8, offset = 1) {
  ns <- NS(id)
  
  # tagList(
  #   actionButton(ns("compute_button"), label),
  #   verbatimTextOutput(ns("text_window"))
  # )
  # fluidRow(
  #   column(col1,
  #          actionButton(ns("compute_button"), label)),
  #   column(col2, offset = offset,
  #          verbatimTextOutput(ns("text_window")))
  # 
  # )
  tagList(
    actionButton(ns("compute_button"), label),
    br(),
    verbatimTextOutput(ns("text_window"))
  )
  
}

# displa the dimensions of the x3ps
x3p_show_xmlServer <- function(id, data, userdir) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$compute_button, {
        
        output$text_window <- renderText({ NULL })
        
        validate(need(assertthat::has_name(isolate(data()), "x3p"), message = "Couldn't find column x3p."))
        
        output$text_window <- renderText({
          isolate(data()$x3p) %>% 
            purrr::map2_chr(.x = .,
                            .y = paste0("x3p",1:length(.),": "),
                            ~ {
                              
                              paste0(.y,
                                     x3p_show_xml(.x,element = "size") %>% 
                                       head(2) %>% 
                                       unlist() %>% 
                                       paste(collapse = " by "),
                                     "\n")
                              
                              # x3p_show_xml(x3p = .x,"size") %>% 
                              #   head(2) %>% 
                              #   unlist()
                              
                            })
        })
        
      })
      
    }
  )
}

# shiny_tt is a tibble containing the x3ps and related information. This
# function performs the pre-processing procedures when instructed

prepare_tt_Server <- function(id, data, userdir) {
  
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$compute_button, {
        
        if(assertthat::has_name(isolate(data()), "x3p")) {
          tmp <- isolate(data())
          
          show_modal_spinner(spin = "atom", text = "Preprocessing Scans...")
          
          if(assertthat::has_name(tmp, "source")) {
            obj <- tmp$source
            cs <- paste(Reduce(intersect, strsplit(obj, "/")), collapse = '/')
            tmp$scan_id <- obj %>% stringr::str_remove(paste(cs, "/", sep = '')) %>%
              stringr::str_remove(".x3p")
          }
          
          if(is.null(tmp$estimatedCartridgeCaseCenter_x)){
            tmp <- tmp %>%
              mutate(estimatedCartridgeCaseCenter_x = rep(-1,times = nrow(.)),
                     estimatedCartridgeCaseCenter_y = rep(-1,times = nrow(.)))
          }
          if(is.null(tmp$estimatedFiringPinCenter_x)){
            tmp <- tmp %>%
              mutate(estimatedFiringPinCenter_x = rep(-1,times = nrow(.)),
                     estimatedFiringPinCenter_y = rep(-1,times = nrow(.)))
          }
          
          tmp <- tmp %>%
            dplyr::mutate(x3p_processed = purrr::pmap(.l = list(x3p,estimatedCartridgeCaseCenter_x,
                                                                estimatedCartridgeCaseCenter_y,
                                                                estimatedFiringPinCenter_x,
                                                                estimatedFiringPinCenter_y),
                                                      function(x3pDat,cartridgeCaseCenter_x,
                                                               cartridgeCaseCenter_y,
                                                               firingPinCenter_x,
                                                               firingPinCenter_y){
                                                        # browser()
                                                        
                                                        x3pDat %>%
                                                          x3ptools::x3p_sample(m = unique(tmp$initialDownsample)) %>%
                                                          cmcR::preProcess_crop(region = "exterior",
                                                                                radiusOffset = -30 + unique(tmp$exteriorCrop),
                                                                                croppingThresh = 1,
                                                                                agg_function = median,
                                                                                roughEstimateExterior = FALSE,
                                                                                scheme = 3,
                                                                                high_connectivity = FALSE,
                                                                                tolerance = 0,
                                                                                ... = list(cartridgeCaseCenter_x,cartridgeCaseCenter_y)) %>%
                                                          cmcR::preProcess_crop(region = "interior",
                                                                                radiusOffset = 200 + unique(tmp$interiorCrop),
                                                                                croppingThresh = 1,
                                                                                agg_function = median,
                                                                                scheme = 3,
                                                                                high_connectivity = FALSE,
                                                                                tolerance = 0,
                                                                                roughEstimateExterior = FALSE,
                                                                                ... = list(firingPinCenter_x,firingPinCenter_y)) %>%
                                                          cmcR::preProcess_removeTrend(statistic = "quantile",
                                                                                       tau = .5,
                                                                                       method = "fn") %>%
                                                          cmcR::preProcess_gaussFilter(wavelength = c(unique(tmp$gaussFilterLow),
                                                                                                      unique(tmp$gaussFilterHigh)),
                                                                                       filtertype = "bp") %>%
                                                          x3ptools::x3p_sample(m = unique(tmp$finalDownsample))
                                                      }))
          
          shiny.r$data <<- tmp
          dataPar <<- data_CheckPar(isolate(shiny.r$data))
          shiny.tt <<- shiny.r$data
          
          remove_modal_spinner()
          
          output$text_window <- renderText({
            paste("Finished data preparation! You are ready to start the investigation!")
          })
          
          # return(dataPar)
          
        } else {
          output$text_window <- renderText({
            validate(
              # need(hasname_x3p(), message = "x3p files not loeaded"),
              need(data(), message = "Could not prepare, no x3p files found.")
            )
            
          })
        }
      }, ignoreInit = TRUE)
      
      
    })
}

# the input server object contains preprocessing parameters followed by the
# index specifying the order in which the functions are to be applied. this
# function matches the preprocessing step to the parameters + index
preProcess_params <- function(preProcStep,ind){
  
  if(preProcStep == "Downsample"){
    
    return(c(paste0("downsamp",ind)))
    
  }
  if(preProcStep == "Crop"){
    
    return(c(paste0("cropRegion",ind),paste0("cropOffset",ind)))
    
  }
  if(preProcStep == "Level"){
    
    return(c(paste0("level",ind)))
    
  }
  if(preProcStep == "Erode"){
    
    return(c(paste0("erodeRegion",ind),paste0("erodeRadius",ind)))
  
    }
  if(preProcStep == "Filter"){
    
    return(c(paste0("filterType",ind),paste0("filterParams",ind)))
  
    }
  
}

# this is a helper for the preprocess procedure on the server side. this will
# return a preprocessing function that with necessary parameters filled-in
preProcess_partial <- function(preProcStep,paramValues){
  
  # browser()
  
  if(preProcStep == "Downsample"){
    
    if((paramValues[[1]] <= 0) | ((paramValues[[1]] %% 1) != 0)){
      
      showNotification("Enter a positive whole number for the Stride parameter of the Downsample step",type = "error")
      validate(need(paramValues[[1]] > 0 & ((paramValues[[1]] %% 1) == 0),message = FALSE))
      
    }
    
    return(purrr::partial(x3ptools::x3p_sample,m = !!paramValues[[1]]))
    
  }
  if(preProcStep == "Crop"){
    
    cropOffset <- paramValues[[which(str_detect(string = names(paramValues),pattern = "params2"))]]
    
    if(((cropOffset %% 1) != 0)){
      
      showNotification("Enter a positive whole number for the Stride parameter of the Downsample step",type = "error")
      validate(need(((cropOffset %% 1) == 0),message = FALSE))
      
    }
    
    return(purrr::partial(cmcR::preProcess_crop,region = tolower(!!paramValues[[1]]),offset = !!paramValues[[2]]))
    
  }
  if(preProcStep == "Level"){
    
    if(paramValues[[1]] == "Mean"){
     
      return(purrr::partial(cmcR::preProcess_removeTrend,statistic = tolower(!!paramValues[[1]])))
       
    }
    else{
      return(purrr::partial(cmcR::preProcess_removeTrend,statistic = "quantile",method = "fn",tau = .5))
    }
  }
  if(preProcStep == "Erode"){
    
    erodeRadius <- paramValues[[which(str_detect(string = names(paramValues),pattern = "params2"))]]
    
    if((erodeRadius <= 0) | ((erodeRadius %% 1) != 0)){
      
      showNotification("Enter a positive whole number for the Radius parameter of the Erode step",type = "error")
      validate(need(erodeRadius > 0 & ((erodeRadius %% 1) == 0),message = FALSE))
      
    }
    
    return(purrr::partial(cmcR::preProcess_erode,region = tolower(!!paramValues[[1]]),morphRadius = !!paramValues[[2]]))
    
  }
  if(preProcStep == "Filter"){
    
    # sometimes(?) the filter type comes after the wavelength cutoffs, so we'll
    # look for the element starting with "params2"
    filtParams <- paramValues[[which(str_detect(string = names(paramValues),pattern = "params2"))]] %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_dbl(as.numeric)
    
    if(paramValues[[1]] == "Lowpass"){
      
      if(length(filtParams) != 1 | any(filtParams) <= 0){
        
        showNotification("Enter one positive whole number for the Wavelength parameter of the Filter step",type = "error")
        validate(need(length(filtParams) == 1 & all(filtParams) > 0,message = FALSE))
        
      }
      
      return(purrr::partial(cmcR::preProcess_gaussFilter,wavelength = !!filtParams,filtertype = "lp"))
    
      
      }
    else{
      
      if(length(filtParams) != 2 | any(filtParams) <= 0){
        
        showNotification("Enter two positive, comma-separated whole numbers for the Wavelengths parameter of the Filter step",type = "error")
        validate(need(length(filtParams) == 2 & all(filtParams) > 0,message = FALSE))
        
      }
      
      return(purrr::partial(cmcR::preProcess_gaussFilter,wavelength = !!filtParams,filtertype = "bp"))
      
      }
    
    
  }
  if(preProcStep == "Delete"){
    
    return(purrr::partial(x3pDeleteMask,color = "#ff0000"))
    
  }
  
}

x3pDeleteMask <- function(x3p,color = "#ff0000"){
  
  x3p$surface.matrix[x3p$mask == color] <- NA
  
  return(x3p)
  
}


`-.gg` <- function(plot, layer) {
  if (missing(layer)) {
    stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
  }
  if (!is.ggplot(plot)) {
    stop('Need a plot on the left side')
  }
  plot$layers = c(layer, plot$layers)
  plot
}


comparison_customCell <- function(refCell,target,theta,sideLengthMultiplier){
  
  compData <- data_frame(cellIndex = "1, 1",cellHeightValues = list(refCell))
  
  compData %>%
    dplyr::mutate(cellPropMissing = comparison_calcPropMissing(cellHeightValues),
                  refMissingCount = purrr::map_dbl(cellHeightValues,~ sum(is.na(.$surface.matrix)))) %>%
    # dplyr::filter(cellPropMissing <= maxNonMissingProp) %>%
    dplyr::mutate(regionHeightValues = comparison_getTargetRegions(cellHeightValues = cellHeightValues,
                                                                   target = target,
                                                                   theta = theta,
                                                                   sideLengthMultiplier = sideLengthMultiplier)) %>%
    dplyr::mutate(targMissingProp = comparison_calcPropMissing(regionHeightValues),
                  targMissingCount = purrr::map_dbl(regionHeightValues,~ sum(is.na(.$surface.matrix)))) %>%
    # dplyr::filter(targMissingProp <= maxNonMissingProp) %>%
    dplyr::mutate(cellHeightValues = comparison_standardizeHeights(cellHeightValues),
                  regionHeightValues = comparison_standardizeHeights(regionHeightValues)) %>%
    dplyr::mutate(cellHeightValues_replaced = comparison_replaceMissing(cellHeightValues),
                  regionHeightValues_replaced = comparison_replaceMissing(regionHeightValues)) %>%
    dplyr::mutate(fft_ccf_df = comparison_fft_ccf(cellHeightValues = cellHeightValues_replaced,
                                                  regionHeightValues = regionHeightValues_replaced)) %>%
    dplyr::mutate(alignedTargetCell = comparison_alignedTargetCell(cellHeightValues = cellHeightValues,
                                                                   regionHeightValues = regionHeightValues,
                                                                   target = target,
                                                                   theta = theta,
                                                                   fft_ccf_df = fft_ccf_df)) %>%
    dplyr::mutate(jointlyMissing = purrr::map2_dbl(cellHeightValues,alignedTargetCell,~ sum(is.na(.x$surface.matrix) & is.na(.y$surface.matrix))),
                  pairwiseCompCor = purrr::map2_dbl(cellHeightValues,alignedTargetCell,
                                                    ~ cor(c(.x$surface.matrix),c(.y$surface.matrix),
                                                          use = "pairwise.complete.obs"))) %>%
    tidyr::unnest(fft_ccf_df) %>%
    dplyr::mutate(theta = theta) %>%
    dplyr::select('cellIndex','x','y','fft_ccf','pairwiseCompCor','theta',
                  'refMissingCount','targMissingCount','jointlyMissing',
                  'cellHeightValues','alignedTargetCell')
  
}

cmcPlot_colorChange <- function(reference,
                          target,
                          cmcClassifs,
                          cellToPlot = NULL,
                          type = "faceted",
                          cmcCol = "originalMethod",
                          corrCol = "pairwiseCompCor"){
  
  #check that the necessary columns are in cmcClassifs
  
  stopifnot("Make sure that there is a column called 'cellHeightValues' that is the result of the comparison_alignedTargetCell() function." = any(stringr::str_detect(names(cmcClassifs),"cellHeightValues")))
  
  stopifnot("Make sure that there is a column called 'alignedTargetCell' that is the result of the comparison_alignedTargetCell() function." = any(stringr::str_detect(names(cmcClassifs),"alignedTargetCell")))
  
  stopifnot("Make sure that there is a column called 'cellIndex'" = any(stringr::str_detect(names(cmcClassifs),"cellIndex")))
  
  stopifnot("Make sure that there is a column called 'theta'" = any(stringr::str_detect(names(cmcClassifs),"theta")))
  
  stopifnot(any(stringr::str_detect(names(cmcClassifs),cmcCol)))
  
  stopifnot(any(stringr::str_detect(names(cmcClassifs),corrCol)))
  
  # get the indices for the necessary columns
  referenceCellCol <- which(stringr::str_detect(names(cmcClassifs),"cellHeightValues"))
  
  targetCellCol <- which(stringr::str_detect(names(cmcClassifs),"alignedTargetCell"))
  
  cellIndexCol <- which(stringr::str_detect(names(cmcClassifs),"cellIndex"))
  
  thetaCol <- which(stringr::str_detect(names(cmcClassifs),"theta"))
  
  cmcIndexCol <- which(stringr::str_detect(names(cmcClassifs),cmcCol))
  
  # cmcClassifs <- cmcClassifs %>%
  #   dplyr::group_by(cellIndex) %>%
  #   dplyr::filter(!!as.name(corrCol) == max(!!as.name(corrCol)))
  
  targetCellData <- cmcClassifs %>%
    dplyr::select(tidyselect::all_of(c(targetCellCol,cellIndexCol,thetaCol,cmcIndexCol))) %>%
    purrr::pmap_dfr(~ impressions:::targetCellCorners(alignedTargetCell = ..1,
                                        cellIndex = ..2,
                                        theta = ..3,
                                        cmcClassif = ..4,
                                        target = target))
  
  # referenceCells <- cmcClassifs %>%
  #   dplyr::pull(referenceCellCol)
  
  # cellData <- cmcClassifs %>%
  #   dplyr::select(c('cellIndexCol','referenceCellCol','cmcIndexCol')) %>%
  #   purrr::pmap_dfr(~ {
  #     
  #     cellInds <- ..2$cmcR.info$cellRange %>%
  #       stringr::str_remove("rows: ") %>%
  #       stringr::str_remove("cols: ") %>%
  #       stringr::str_split(pattern = ", ")
  #     
  #     cellInds_rows <- stringr::str_split(cellInds[[1]][1]," - ")[[1]]
  #     cellInds_cols <- stringr::str_split(cellInds[[1]][2]," - ")[[1]]
  #     
  #     return(data.frame(rowStart = as.numeric(cellInds_rows[1]),
  #                       rowEnd = as.numeric(cellInds_rows[2]),
  #                       colStart = as.numeric(cellInds_cols[1]),
  #                       colEnd = as.numeric(cellInds_cols[2])) %>%
  #              dplyr::mutate(cellIndex = ..1,
  #                            cmcClassif = ..3))
  #     
  #   }) %>%
  #   dplyr::mutate(rowStart = max(rowEnd) - rowStart,
  #                 rowEnd = max(rowEnd) - rowEnd,
  #                 colMean = purrr::map2_dbl(colStart,colEnd,~ mean(c(.x,.y))),
  #                 rowMean = purrr::map2_dbl(rowStart,rowEnd,~ mean(c(.x,.y))))
  
  # ggplot2 complains about the guides
  suppressWarnings({
    
    # refPlt <- x3pListPlot(list("reference" = reference)
    #                       # ,height.colors =
    #                       #   c('#1B1B1B','#404040','#7B7B7B','#B0B0B0','#DBDBDB','#F7F7F7','#E4E4E4','#C5C5C5','#999999','#717171','#4E4E4E')
    #                       ) +
    #   ggplot2::guides(fill = "none") +
    #   ggnewscale::new_scale_fill() +
    #   ggplot2::geom_rect(data = cellData,
    #                      ggplot2::aes(xmin = colStart,xmax = colEnd,ymin = rowStart,ymax = rowEnd,fill = cmcClassif),
    #                      alpha = .2,
    #                      inherit.aes = FALSE) +
    #   ggplot2::scale_fill_manual(values = c("black")) +
    #   # ggplot2::scale_fill_manual(values = c("#313695","#a50026")) +
    #   ggplot2::geom_text(data = cellData,
    #                      ggplot2::aes(x = colMean,y = rowMean,label = cellIndex),inherit.aes = FALSE) +
    #   ggplot2::guides(fill = ggplot2::guide_legend(order = 1)) +
    #   ggplot2::theme(
    #     legend.direction = "horizontal"
    #   ) +
    #   ggplot2::labs(fill = "CMC Classif.")
    # 
    # cmcLegend <- ggplotify::as.ggplot(cowplot::get_legend(refPlt)$grobs[[1]])
    # 
    # refPlt <- refPlt +
    #   ggplot2::theme(legend.position = "none")
    
    plt <- x3pListPlot(list("target" = target)
                       # , height.colors =
                       #   c('#1B1B1B','#404040','#7B7B7B','#B0B0B0','#DBDBDB','#F7F7F7','#E4E4E4','#C5C5C5','#999999','#717171','#4E4E4E')
                       ) +
      ggplot2::theme(legend.position = "none")
    
    if(!is.null(cellToPlot)){
      
      targetCellData <- targetCellData %>%
        filter(cellIndex == cellToPlot)

          }
    
    plt <- plt +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_raster(data = targetCellData,
                           ggplot2::aes(x = x,y = y,fill = cmcClassif),
                           alpha = .2) +
      ggplot2::scale_fill_manual(values = c("#313695","#a50026")) #+
      # ggplot2::geom_text(data = targetCellData %>%
      #                      dplyr::group_by(cellIndex) %>%
      #                      dplyr::summarise(x = mean(x),
      #                                       y = mean(y),
      #                                       theta = unique(theta)),
      #                    ggplot2::aes(x=x,y=y,label = cellIndex,angle = -1*theta))
    
  })
  
  # library(patchwork)
  # return((refPlt | plt))
  if(type == "list"){
    return(list(
      # "reference" = refPlt,
                "target" = plt
                # ,"legend" = cmcLegend
                ))
  }
  
  # return(patchwork::wrap_plots(refPlt,plt,cmcLegend,nrow = 2,heights = c(1,.1)))
  return(plt)
}

cmcPlot_interactive <- function(reference,
                                target,
                                cmcClassifs,
                                type = "faceted",
                                cmcCol = "originalMethod",
                                corrCol = "pairwiseCompCor"){
  
  #check that the necessary columns are in cmcClassifs
  
  stopifnot("Make sure that there is a column called 'cellHeightValues' that is the result of the comparison_alignedTargetCell() function." = any(stringr::str_detect(names(cmcClassifs),"cellHeightValues")))
  
  stopifnot("Make sure that there is a column called 'alignedTargetCell' that is the result of the comparison_alignedTargetCell() function." = any(stringr::str_detect(names(cmcClassifs),"alignedTargetCell")))
  
  stopifnot("Make sure that there is a column called 'cellIndex'" = any(stringr::str_detect(names(cmcClassifs),"cellIndex")))
  
  stopifnot("Make sure that there is a column called 'theta'" = any(stringr::str_detect(names(cmcClassifs),"theta")))
  
  stopifnot(any(stringr::str_detect(names(cmcClassifs),cmcCol)))
  
  stopifnot(any(stringr::str_detect(names(cmcClassifs),corrCol)))
  
  # get the indices for the necessary columns
  referenceCellCol <- which(stringr::str_detect(names(cmcClassifs),"cellHeightValues"))
  
  targetCellCol <- which(stringr::str_detect(names(cmcClassifs),"alignedTargetCell"))
  
  cellIndexCol <- which(stringr::str_detect(names(cmcClassifs),"cellIndex"))
  
  thetaCol <- which(stringr::str_detect(names(cmcClassifs),"theta"))
  
  cmcIndexCol <- which(stringr::str_detect(names(cmcClassifs),cmcCol))
  
  # cmcClassifs <- cmcClassifs %>%
  #   dplyr::group_by(cellIndex) %>%
  #   dplyr::filter(!!as.name(corrCol) == max(!!as.name(corrCol)))
  
  targetCellData <- cmcClassifs %>%
    dplyr::select(all_of(c(targetCellCol,cellIndexCol,thetaCol,cmcIndexCol))) %>%
    purrr::pmap_dfr(~ impressions:::targetCellCorners(alignedTargetCell = ..1,
                                        cellIndex = ..2,
                                        theta = ..3,
                                        cmcClassif = ..4,
                                        target = target))
  
  referenceCells <- cmcClassifs %>%
    dplyr::pull(referenceCellCol)
  
  cellData <- cmcClassifs %>%
    dplyr::select(all_of(c(cellIndexCol,referenceCellCol,cmcIndexCol))) %>%
    purrr::pmap_dfr(~ {
      
      cellInds <- ..2$cmcR.info$cellRange %>%
        stringr::str_remove("rows: ") %>%
        stringr::str_remove("cols: ") %>%
        stringr::str_split(pattern = ", ")
      
      cellInds_rows <- stringr::str_split(cellInds[[1]][1]," - ")[[1]]
      cellInds_cols <- stringr::str_split(cellInds[[1]][2]," - ")[[1]]
      
      return(data.frame(rowStart = as.numeric(cellInds_rows[1]),
                        rowEnd = as.numeric(cellInds_rows[2]),
                        colStart = as.numeric(cellInds_cols[1]),
                        colEnd = as.numeric(cellInds_cols[2])) %>%
               dplyr::mutate(cellIndex = ..1,
                             cmcClassif = ..3))
      
    }) %>%
    dplyr::mutate(rowStart = max(rowEnd) - rowStart,
                  rowEnd = max(rowEnd) - rowEnd,
                  colMean = purrr::map2_dbl(colStart,colEnd,~ mean(c(.x,.y))),
                  rowMean = purrr::map2_dbl(rowStart,rowEnd,~ mean(c(.x,.y))))
  
  # ggplot2 complains about the guides
  suppressWarnings({
    
    refPlt <- x3pListPlot(list("reference" = reference),
                          # height.colors =
                          #   c('#1B1B1B','#404040','#7B7B7B','#B0B0B0','#DBDBDB','#F7F7F7','#E4E4E4','#C5C5C5','#999999','#717171','#4E4E4E')
                          ) +
      ggplot2::guides(fill = "none") +
      ggnewscale::new_scale_fill() +
      geom_rect(data = cellData,
                         ggplot2::aes(xmin = colStart,xmax = colEnd,
                                      ymin = rowStart,ymax = rowEnd,fill = cmcClassif,
                                      data_id = cellIndex),
                         alpha = .2,
                         inherit.aes = FALSE) +
      ggplot2::scale_fill_manual(values = c("black")) +
      geom_label_interactive(data = cellData,
                         ggplot2::aes(x = colMean,y = rowMean,label = cellIndex,
                                      data_id = cellIndex),
                         alpha = .7,
                         size = 6,
                         inherit.aes = FALSE,
                         color = "black") +
      ggplot2::guides(fill = ggplot2::guide_legend(order = 1)) +
      ggplot2::theme(
        legend.direction = "horizontal"
      ) +
      ggplot2::labs(fill = "CMC Classif.")
    
    cmcLegend <- ggplotify::as.ggplot(cowplot::get_legend(refPlt)$grobs[[1]])
    
    refPlt <- refPlt +
      ggplot2::theme(legend.position = "none")
    
    plt <- x3pListPlot(list("target" = target),
                       # height.colors =
                       #   c('#1B1B1B','#404040','#7B7B7B','#B0B0B0','#DBDBDB','#F7F7F7','#E4E4E4','#C5C5C5','#999999','#717171','#4E4E4E')
                       ) +
      ggplot2::theme(legend.position = "none")
    
    plt <- plt +
      ggnewscale::new_scale_fill() +
      geom_raster_interactive(data = targetCellData,
                           ggplot2::aes(x = x,y = y,fill = cmcClassif,
                                        data_id = cellIndex),
                           alpha = .2) +
      ggplot2::scale_fill_manual(values = c("black")) +
      geom_label_interactive(data = targetCellData %>%
                           dplyr::group_by(cellIndex) %>%
                           dplyr::summarise(x = mean(x),
                                            y = mean(y),
                                            theta = unique(theta)),
                         ggplot2::aes(x=x,y=y,label = cellIndex,angle = -1*theta,
                                      data_id = cellIndex),
                         alpha = .7,
                         size = 6,
                         color = "black")
    
  })
  
  # library(patchwork)
  # return((refPlt | plt))
  if(type == "list"){
    return(list("reference" = refPlt,
                "target" = plt,
                "legend" = cmcLegend))
  }
  
  return(patchwork::wrap_plots(refPlt,plt,cmcLegend,nrow = 2,heights = c(1,.1)))
}

## Updated gganimate::draw_frames function that allows for a custom message in the progress  bar

my_draw_frames <- function(plot, frames, device, ref_frame, update_progress = NULL, ...) {
  stream <- device == 'current'
  
  dims <- tryCatch(
    plot_dims(plot, ref_frame),
    error = function(e) {
      warning('Cannot get dimensions of plot table. Plot region might not be fixed', call. = FALSE)
      list(widths = NULL, heights = NULL)
    }
  )
  
  dir <- tempfile(pattern = '')
  dir.create(dir, showWarnings = FALSE)
  files <- file.path(dir, sprintf('gganim_plot%04d', seq_along(frames)))
  files <- switch(
    tolower(device),
    png = paste0(files, '.png'),
    jpg = ,
    jpeg = paste0(files, '.jpg'),
    tif = ,
    tiff = paste0(files, '.tif'),
    bmp = paste0(files, '.bmp'),
    svglite = ,
    svg = paste0(files, '.svg'),
    current = files,
    stop('Unsupported device', call. = FALSE)
  )
  device <- switch(
    device,
    png = png,
    jpg = ,
    jpeg = jpeg,
    tif = ,
    tiff = tiff,
    bmp = bmp,
    svg = svg,
    svglite = svglite::svglite
  )
  
  pb <- progress_bar$new(
    'Rendering [:bar] at :fps fps ~ eta: :eta',
    total = length(frames)
  )
  start <- Sys.time()
  pb$tick(0)
  
  for (i in seq_along(frames)) {
    if (!stream) device(files[i], ...)
    
    tryCatch(
      plot$scene$plot_frame(plot, frames[i], widths = dims$widths, heights = dims$heights),
      error = function(e) {
        warning(conditionMessage(e), call. = FALSE)
      }
    )
    
    rate <- i/as.double(Sys.time() - start, units = 'secs')
    if (is.nan(rate)) rate <- 0
    
    # send update to shiny progress
    if(is.function(update_progress)) {
      frames_left <- length(frames) - i
      projected_secs <- as.difftime(frames_left / rate, units = "secs")
      eta <- prettyunits::vague_dt(projected_secs, format = "terse")
      detail <- paste0("at ", format(rate, digits = 2), " fps ~ eta: ", eta)
      update_progress(detail)
    }
    
    rate <- format(rate, digits = 2)
    pb$tick(tokens = list(fps = rate))
    
    if (!stream) dev.off()
  }
  
  frame_vars <- plot$scene$frame_vars[frames, , drop = FALSE]
  if (!stream) frame_vars$frame_source <- files
  frame_vars
}


## keep a copy of the old draw_frames function to reset the namespace after the
## animation is rendered
old_draw_frames <- function (plot, frames, device, ref_frame, ...) 
{
  stream <- device == "current"
  dims <- tryCatch(plot_dims(plot, ref_frame), error = function(e) {
    warning("Cannot get dimensions of plot table. Plot region might not be fixed", 
            call. = FALSE)
    list(widths = NULL, heights = NULL)
  })
  dir <- tempfile(pattern = "")
  dir.create(dir, showWarnings = FALSE)
  files <- file.path(dir, sprintf("gganim_plot%04d", 
                                  seq_along(frames)))
  files <- switch(tolower(device), ragg_png = , png = paste0(files, 
                                                             ".png"), jpg = , jpeg = paste0(files, ".jpg"), 
                  tif = , tiff = paste0(files, ".tif"), bmp = paste0(files, 
                                                                     ".bmp"), svglite = , svg = paste0(files, ".svg"), 
                  current = files, stop("Unsupported device", call. = FALSE))
  device <- switch(device, ragg_png = ragg::agg_png, png = png, 
                   jpg = , jpeg = jpeg, tif = , tiff = tiff, bmp = bmp, 
                   svg = svg, svglite = svglite::svglite)
  pb <- progress_bar$new("Rendering [:bar] at :fps fps ~ eta: :eta", 
                         total = length(frames))
  start <- Sys.time()
  pb$tick(0)
  for (i in seq_along(frames)) {
    if (!stream) 
      device(files[i], ...)
    tryCatch(plot$scene$plot_frame(plot, frames[i], widths = dims$widths, 
                                   heights = dims$heights), error = function(e) {
                                     warning(conditionMessage(e), call. = FALSE)
                                   })
    rate <- i/as.double(Sys.time() - start, units = "secs")
    if (is.nan(rate)) 
      rate <- 0
    rate <- format(rate, digits = 2)
    pb$tick(tokens = list(fps = rate))
    if (!stream) 
      dev.off()
  }
  frame_vars <- plot$scene$frame_vars[frames, , drop = FALSE]
  if (!stream) 
    frame_vars$frame_source <- files
  frame_vars
}

