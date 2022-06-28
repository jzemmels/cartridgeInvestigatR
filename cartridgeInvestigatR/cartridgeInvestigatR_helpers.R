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
  
  if(preProcStep == "Downsample"){
    
    return(purrr::partial(x3ptools::x3p_sample,m = paramValues[[1]]))
    
  }
  if(preProcStep == "Crop"){
    
    return(purrr::partial(cmcR::preProcess_crop,region = tolower(paramValues[[1]]),offset = paramValues[[2]]))
    
  }
  if(preProcStep == "Level"){
    
    if(paramValues[[1]] == "Mean"){
     
      return(purrr::partial(cmcR::preProcess_removeTrend,statistic = tolower(paramValues[[1]])))
       
    }
    else{
      return(purrr::partial(cmcR::preProcess_removeTrend,statistic = "quantile",method = "fn",tau = .5))
    }
  }
  if(preProcStep == "Erode"){
    
    return(purrr::partial(cmcR::preProcess_erode,region = tolower(paramValues[[1]]),morphRadius = paramValues[[2]]))
    
  }
  if(preProcStep == "Filter"){
    
    filtParams <- paramValues[[2]] %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_dbl(as.numeric)
    
    if(paramValues[[1]] == "Lowpass"){
      return(purrr::partial(cmcR::preProcess_gaussFilter,wavelength = filtParams,filtertype = "lp"))
    }
    else{
      return(purrr::partial(cmcR::preProcess_gaussFilter,wavelength = filtParams,filtertype = "bp"))
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

x3pToDF <- function(x3p,preserveResolution = FALSE){
  
  if(!preserveResolution){
    
    x3p$header.info$incrementX <- 1
    x3p$header.info$incrementY <- 1
    
  }
  
  ret <- x3p %>%
    x3ptools::x3p_to_df() %>%
    #perform some transformations on the x,y values so that the plot is
    #representative of the actual surface matrix (i.e., element [1,1] of the
    #surface matrix is in the top-left corner)
    dplyr::mutate(xnew = max(y) - y,
                  ynew = max(x) - x) %>%
    dplyr::select(-c(x,y)) %>%
    dplyr::rename(x=xnew,
                  y=ynew)
  
  return(ret)
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

fiveplot <- function(comparisonResults, referenceScan, targetScan, cell) {
  
  referenceCell <- comparisonResults %>%
    pull(cellHeightValues) %>%
    .[[1]]
  
  targetCell <- comparisonResults %>%
    pull(alignedTargetCell) %>%
    .[[1]]
  
  referenceCell$surface.matrix <- (referenceCell$surface.matrix*referenceCell$cmcR.info$scaleByVal + referenceCell$cmcR.info$centerByVal)*1e6
  targetCell$surface.matrix <- (targetCell$surface.matrix*targetCell$cmcR.info$scaleByVal + targetCell$cmcR.info$centerByVal)*1e6
  
  
  patchComparisonPlts <- impressions::x3pComparisonPlot(
    reference = referenceCell,
    target = targetCell,
    cutoffThresh = sd(c(c(referenceCell$surface.matrix),c(targetCell$surface.matrix)),na.rm = TRUE),
    plotNames = c(sprintf("%s Cell %s", referenceScan, cell),
                  sprintf("%s Aligned Cell", targetScan),
                  "Filtered Element-wise Average",
                  sprintf("%s Cell %s\nFiltered Differences", referenceScan, cell),
                  sprintf("%s Aligned Cell\nFiltered Differences", targetScan))
  )
  
  patchComparisonLegend_match <-
    cowplot::plot_grid(patchComparisonPlts$legend$grobs[[1]])
  
  combinedValues <-  referenceCell %>%
    impressions::x3pToDF() %>%
    rename(refValue = value) %>%
    left_join(targetCell %>%
                impressions::x3pToDF() %>%
                rename(targValue = value),
              by = c("x","y"))
  
  blobBoundaries <- comparisonResults %>%
    as.data.frame() %>%
    dplyr::select(cellIndex,cellHeightValues,alignedTargetCell) %>%
    pmap(~ {
      
      reference <-  ..2
      
      target <- ..3
      
      reference$surface.matrix <- (reference$surface.matrix*reference$cmcR.info$scaleByVal + reference$cmcR.info$centerByVal)*1e6
      target$surface.matrix <- (target$surface.matrix*target$cmcR.info$scaleByVal + target$cmcR.info$centerByVal)*1e6
      
      averageBinarized <- bind_rows(reference %>%
                                      impressions::x3pToDF() %>%
                                      mutate(value = value),
                                    target %>%
                                      impressions::x3pToDF() %>%
                                      mutate(value = value)) %>%
        group_by(x,y) %>%
        summarise(difference = diff(value),
                  absDifference = abs(diff(value)),
                  average = mean(value),
                  .groups = "drop")  %>%
        mutate(cellIndex = ..1)%>%
        mutate(value = ifelse(absDifference > sd(c(c(reference$surface.matrix),c(target$surface.matrix)),na.rm = TRUE),TRUE,FALSE))
      
      suppressWarnings({
        
        averageMat <- averageBinarized %>%
          mutate(x = x+1,
                 y=y+1) %>%
          as.data.frame() %>%
          dplyr::select(x,y,value) %>%
          imager::as.cimg() %>%
          as.matrix()
        
      })
      
      averageMat[is.na(averageMat)] <- 0
      
      # we pad the matrix so that the contours one the edge blobs are properly
      # identified. the padding is removed in the last lines of the creation of
      # the outline object below
      averageMat  <- averageMat %>%
        imager::as.cimg() %>%
        imager::pad(nPix = 10,axes = "xy",val = 0)
      
      labels <- imager::label(averageMat)
      
      bounds <- map(unique(labels[labels > 0]),
                    function(lab){
                      
                      imager::boundary(labels == lab)
                      
                    })
      
      return(list(bounds,labels))
      
    })
  
  if(length(blobBoundaries[[1]][[1]]) > 0){
   
    # combine all labeled blobs into one image
    boundaryPx <- Reduce("+",blobBoundaries[[1]][[1]] %>%
                           map(as.matrix)) %>%
      imager::as.cimg()
     
    # the mask used to dilate the blobs will grow them towards the bottom-right of
    # the matrix
    dilatedPx <- imager::dilate_rect(boundaryPx,sx = 2,sy = 2)
    dilatedPx_labels <- imager::dilate_rect(blobBoundaries[[1]][[2]],sx = 2,sy = 2)
    
    # flip the image and re-apply the dilation to grow the borders to the other
    # corners. flip back after dilation
    dilatedPx_mirrorx <- imager::mirror(imager::dilate_rect(imager::mirror(boundaryPx,axis="x"),sx = 2,sy = 2),axis="x")
    dilatedPx_mirrorx_labels <- imager::mirror(imager::dilate_rect(imager::mirror(blobBoundaries[[1]][[2]],axis="x"),sx = 2,sy = 2),axis="x")
    
    dilatedPx_mirrory <- imager::mirror(imager::dilate_rect(imager::mirror(boundaryPx,axis="y"),sx = 2,sy = 2),"y")
    dilatedPx_mirrory_labels <- imager::mirror(imager::dilate_rect(imager::mirror(blobBoundaries[[1]][[2]],axis="y"),sx = 2,sy = 2),"y")
    
    dilatedPx_mirrorxy <- imager::mirror(imager::dilate_rect(imager::mirror(boundaryPx,axis="xy"),sx = 3,sy = 3),"xy")
    dilatedPx_mirrorxy_labels <- imager::mirror(imager::dilate_rect(imager::mirror(blobBoundaries[[1]][[2]],axis="xy"),sx = 3,sy = 3),"xy")
    
    # combine all of the dilated images together into one image
    dilatedPx_comb <- dilatedPx + dilatedPx_mirrorx + dilatedPx_mirrory + dilatedPx_mirrorxy
    
    # we just want a binary labeling
    dilatedPx_comb[dilatedPx_comb > 0] <- 1
    
    # the dilated boundaries will have also grown into the blobs, so we take those
    # pixels out
    dilatedPx_comb[blobBoundaries[[1]][[2]] > 0] <- 0
    
    # from: https://stackoverflow.com/questions/34756755/plot-outline-around-raster-cells
    outline <- dilatedPx_comb %>%
      as.data.frame() %>%
      filter(value > 0) %>%
      mutate(x = x-1,
             y = y-1) %>%
      raster::rasterFromXYZ() %>%
      raster::rasterToPolygons(dissolve = TRUE) %>%
      fortify() %>%
      #the boundaries around the filtered blobs all share a common value in the
      #"hole" column of TRUE
      filter(hole) %>%
      # remove padding used previously
      mutate(lat = lat-5,
             long = long-5)
    
  }
  else{
    outline <- data.frame(lat = 0,long = 0,group = 1)
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
  
  topLeft <- patchComparisonPlts[[1]] +
    cowplot::theme_nothing() +
    labs(subtitle = sprintf("%s Cell %s", referenceScan, cell)) +
    theme(plot.margin = margin(0,0,5,0),
          plot.subtitle = element_text(hjust = .5,size = 8,vjust = -1)) +
    geom_raster(data = combinedValues %>%
                  filter(is.na(refValue) & !is.na(targValue)),
                fill = "gray40")
  
  bottomLeft <-patchComparisonPlts[[2]] +
    cowplot::theme_nothing() +
    labs(subtitle = paste0(targetScan," Aligned Cell\nat ",unique(comparisonResults$theta),"°")) +
    theme(plot.margin = margin(-20,-100,30,-100),
          plot.subtitle = element_text(hjust = .5,vjust = -88,size = 8)) +
    geom_raster(data = combinedValues %>%
                  filter(!is.na(refValue) & is.na(targValue)),
                fill = "gray40")
  
  middle <- patchComparisonPlts[[3]] +
    cowplot::theme_nothing() +
    labs(subtitle = "Filtered Element-wise Average\nAbs. Differences at Most 1") +
    theme(plot.margin = margin(0,25,0,25),
          plot.subtitle = element_text(hjust = .5,size = 8,vjust = 3)) -
    geom_raster(fill = "gray80") +
    geom_path(data = outline,  color = "grey40",
              aes(x=long,y=lat,group=group),
              colour = "gray40",
              inherit.aes = FALSE,
              size = .2)
  
  topRight <- patchComparisonPlts[[4]] +
    cowplot::theme_nothing() +
    labs(subtitle = sprintf("Filtered %s Cell %s\nAbs. Differences Greater Than 1",referenceScan, cell)) +
    theme(plot.margin = margin(0,0,5,0),
          plot.subtitle = element_text(hjust = .5,size = 8,vjust = 1)) -
    geom_raster(fill = "gray80") +
    geom_path(data = outline,  color = "grey40",
              aes(x=long,y=lat,group=group),
              colour = "gray40",
              inherit.aes = FALSE,
              size = .1)
  
  bottomRight <- patchComparisonPlts[[5]] +
    cowplot::theme_nothing() +
    labs(subtitle = sprintf("Filtered %s Aligned Cell\nAbs. Differences Greater Than 1",targetScan)) +
    theme(plot.margin = margin(-20,-100,30,-100),
          plot.subtitle = element_text(hjust = .5,vjust = -88,size = 8)) -
    geom_raster(fill = "gray80") +
    geom_path(data = outline, color = "grey40",
              aes(x=long,y=lat,group=group),
              colour = "gray40",
              inherit.aes = FALSE,
              size = .1)
  
  design <- "ACCD\nBCCE"
  
  patchwork::wrap_plots(topLeft,bottomLeft,middle,topRight,bottomRight,design = design) +
    inset_element(patchComparisonLegend_match,left = -2.1,bottom = .05,right = -2.1,top = .05,on_top = FALSE,align_to = 'full')
}

comparison_customCell <- function(refCell,target,theta,maxNonMissingProp){
  
  compData <- data_frame(cellIndex = "1, 1",cellHeightValues = list(refCell))
  
  compData %>%
    dplyr::mutate(cellPropMissing = comparison_calcPropMissing(.data$cellHeightValues),
                  refMissingCount = purrr::map_dbl(.data$cellHeightValues,~ sum(is.na(.$surface.matrix)))) %>%
    # dplyr::filter(.data$cellPropMissing <= maxNonMissingProp) %>%
    dplyr::mutate(regionHeightValues = comparison_getTargetRegions(cellHeightValues = .data$cellHeightValues,
                                                                   target = target,
                                                                   theta = theta,
                                                                   sideLengthMultiplier = 3)) %>%
    dplyr::mutate(targMissingProp = comparison_calcPropMissing(.data$regionHeightValues),
                  targMissingCount = purrr::map_dbl(.data$regionHeightValues,~ sum(is.na(.$surface.matrix)))) %>%
    # dplyr::filter(.data$targMissingProp <= maxNonMissingProp) %>%
    dplyr::mutate(cellHeightValues = comparison_standardizeHeights(.data$cellHeightValues),
                  regionHeightValues = comparison_standardizeHeights(.data$regionHeightValues)) %>%
    dplyr::mutate(cellHeightValues_replaced = comparison_replaceMissing(.data$cellHeightValues),
                  regionHeightValues_replaced = comparison_replaceMissing(.data$regionHeightValues)) %>%
    dplyr::mutate(fft_ccf_df = comparison_fft_ccf(cellHeightValues = .data$cellHeightValues_replaced,
                                                  regionHeightValues = .data$regionHeightValues_replaced)) %>%
    dplyr::mutate(alignedTargetCell = comparison_alignedTargetCell(cellHeightValues = .data$cellHeightValues,
                                                                   regionHeightValues = .data$regionHeightValues,
                                                                   target = target,
                                                                   theta = theta,
                                                                   fft_ccf_df = .data$fft_ccf_df)) %>%
    dplyr::mutate(jointlyMissing = purrr::map2_dbl(.data$cellHeightValues,.data$alignedTargetCell,~ sum(is.na(.x$surface.matrix) & is.na(.y$surface.matrix))),
                  pairwiseCompCor = purrr::map2_dbl(.data$cellHeightValues,.data$alignedTargetCell,
                                                    ~ cor(c(.x$surface.matrix),c(.y$surface.matrix),
                                                          use = "pairwise.complete.obs"))) %>%
    tidyr::unnest(.data$fft_ccf_df) %>%
    dplyr::mutate(theta = theta) %>%
    dplyr::select(.data$cellIndex,.data$x,.data$y,.data$fft_ccf,.data$pairwiseCompCor,.data$theta,.data$refMissingCount,.data$targMissingCount,.data$jointlyMissing,.data$cellHeightValues,.data$alignedTargetCell)
  
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
    dplyr::select(c(targetCellCol,cellIndexCol,thetaCol,cmcIndexCol)) %>%
    purrr::pmap_dfr(~ cmcR:::targetCellCorners(alignedTargetCell = ..1,
                                        cellIndex = ..2,
                                        theta = ..3,
                                        cmcClassif = ..4,
                                        target = target))
  
  # referenceCells <- cmcClassifs %>%
  #   dplyr::pull(referenceCellCol)
  
  # cellData <- cmcClassifs %>%
  #   dplyr::select(c(cellIndexCol,referenceCellCol,cmcIndexCol)) %>%
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
  #   dplyr::mutate(rowStart = max(.data$rowEnd) - .data$rowStart,
  #                 rowEnd = max(.data$rowEnd) - .data$rowEnd,
  #                 colMean = purrr::map2_dbl(.data$colStart,.data$colEnd,~ mean(c(.x,.y))),
  #                 rowMean = purrr::map2_dbl(.data$rowStart,.data$rowEnd,~ mean(c(.x,.y))))
  
  # ggplot2 complains about the guides
  suppressWarnings({
    
    # refPlt <- x3pListPlot(list("reference" = reference)
    #                       # ,height.colors =
    #                       #   c('#1B1B1B','#404040','#7B7B7B','#B0B0B0','#DBDBDB','#F7F7F7','#E4E4E4','#C5C5C5','#999999','#717171','#4E4E4E')
    #                       ) +
    #   ggplot2::guides(fill = "none") +
    #   ggnewscale::new_scale_fill() +
    #   ggplot2::geom_rect(data = cellData,
    #                      ggplot2::aes(xmin = .data$colStart,xmax = .data$colEnd,ymin = .data$rowStart,ymax = .data$rowEnd,fill = .data$cmcClassif),
    #                      alpha = .2,
    #                      inherit.aes = FALSE) +
    #   ggplot2::scale_fill_manual(values = c("black")) +
    #   # ggplot2::scale_fill_manual(values = c("#313695","#a50026")) +
    #   ggplot2::geom_text(data = cellData,
    #                      ggplot2::aes(x = .data$colMean,y = .data$rowMean,label = .data$cellIndex),inherit.aes = FALSE) +
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
                           ggplot2::aes(x = .data$x,y = .data$y,fill = .data$cmcClassif),
                           alpha = .2) +
      ggplot2::scale_fill_manual(values = c("#313695","#a50026")) +
      ggplot2::geom_text(data = targetCellData %>%
                           dplyr::group_by(.data$cellIndex) %>%
                           dplyr::summarise(x = mean(.data$x),
                                            y = mean(.data$y),
                                            theta = unique(.data$theta)),
                         ggplot2::aes(x=.data$x,y=.data$y,label = .data$cellIndex,angle = -1*.data$theta))
    
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
