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