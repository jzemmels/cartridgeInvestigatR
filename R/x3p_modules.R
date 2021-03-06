x3p_init <- function(userdir, filename) {
  cat(paste("", "\n"), file = file.path(userdir, filename), append = FALSE)
}


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

x3p_flip_yServer <- function(id, data, userdir) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$compute_button, {
        
        output$text_window <- renderText({ NULL })
        
        validate(need(assertthat::has_name(isolate(data()), "x3p"), message = "Couldn't find column x3p."))
        shiny.r$data <<- data() %>% mutate(x3p = x3p %>% purrr::map(.f = y_flip_x3p))
        
        output$text_window <- renderText({
          paste("good for x3p_flip_y")
        })
        
        
        # tmp <- try(data() %>% mutate(x3p = x3p %>% purrr::map(.f = y_flip_x3p)))
        # 
        # browser()
        # 
        # output$text_window <- renderText({
        #   validate(need(tmp, message = "Couldn't find column x3p."))
        #   shiny.r$data <<- tmp
        #   paste("good for x3p_flip_y")
        # })        
        
        interpolate(~(bullet <- bullet %>% mutate(x3p = x3p %>% purrr::map(.f = y_flip_x3p))),
                    mydir = userdir,
                    `_env` = environment(),
                    file = "code_x3p.R",
                    append = TRUE, eval = FALSE)
        
      })
    }
  )}

x3p_sampleUI <- function(id, label = "Click me!") {
  ns <- NS(id)
  
  tagList(
    numericInput(ns("ds_num"), "Down sample by:", 2, min = 1, max = 500),
    actionButton(ns("compute_button"), label),
    verbatimTextOutput(ns("text_window"))
  )
}


x3p_sampleServer <- function(id, data, userdir) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$compute_button, {
        
        output$text_window <- renderText({ NULL })
        
        validate(need(assertthat::has_name(isolate(data()), "x3p"), message = "Couldn't find column x3p."))
        shiny.r$data <<- data() %>% mutate(
          x3p = x3p %>% purrr::map(.f = function(x) x %>% x3p_sample(m = isolate(input$ds_num))))
        
        output$text_window <- renderText({
          paste("good for x3p_sample, m:", isolate(input$ds_num))
        })
        
        interpolate(~(bullet <- bullet %>% mutate(
          x3p = x3p %>% purrr::map(.f = function(x) x %>% x3p_sample(m = num)))
        ),
        num = isolate(input$ds_num),
        mydir = userdir,
        `_env` = environment(),
        file = "code_x3p.R",
        append = TRUE, eval = FALSE)
        
      })
    }
  )}


x3p_mtomumServer <- function(id, data, userdir) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$compute_button, {
        
        output$text_window <- renderText({ NULL })
        
        validate(need(assertthat::has_name(isolate(data()), "x3p"), message = "Couldn't find column x3p."))
        shiny.r$data <<- data() %>% mutate(x3p = x3p %>% purrr::map(.f = x3p_m_to_mum))
        
        output$text_window <- renderText({
          paste("good for x3p_m_to_mum")
        })
        
        interpolate(~(bullet <- bullet %>% mutate(x3p = x3p %>% purrr::map(.f = x3p_m_to_mum))),
                    mydir = userdir,
                    `_env` = environment(),
                    file = "code_x3p.R",
                    append = TRUE, eval = FALSE)
      })
    }
  )}

x3p_rotateUI <- function(id, label = "Click me!") {
  ns <- NS(id)
  
  tagList(
    numericInput(ns("r_angle"), "Rotate, angle value:", -90, min = -360, max = 360),
    actionButton(ns("compute_button"), label),
    verbatimTextOutput(ns("text_window"))
  )
}


x3p_rotateServer <- function(id, data, userdir) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$compute_button, {
        
        output$text_window <- renderText({ NULL })
        
        validate(need(assertthat::has_name(isolate(data()), "x3p"), message = "Couldn't find column x3p."))
        shiny.r$data <<- data() %>% mutate(
          x3p = x3p %>% purrr::map(.f = function(x) x %>% rotate_x3p(angle = isolate(input$r_angle))))
        
        output$text_window <- renderText({
          paste("Finish rotation, angle is:", isolate(input$r_angle))
        })
        
        interpolate(~(bullet <- bullet %>% mutate(
          x3p = x3p %>% purrr::map(.f = function(x) x %>% rotate_x3p(angle = num)))
        ),
        num = isolate(input$r_angle),
        mydir = userdir,
        `_env` = environment(),
        file = "code_x3p.R",
        append = TRUE, eval = FALSE)
        
      })
    }
  )}

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

# Helper function for fpCenterMat function defined below (please excuse the
# awful naming scheme). Performs the firing pin estimation using the label
# function in imager
fpCenterCalc <- function(x3p,
                         scheme = 3,
                         high_connectivity = FALSE,
                         tolerance = 0,
                         centerOffset = c(0,0)){
  
  mat_bfRegion <- x3p$surface.matrix
  
  mat_bfRegionBinarized <- mat_bfRegion
  mat_bfRegionBinarized[!is.na(mat_bfRegionBinarized)] <- 1
  mat_bfRegionBinarized[is.na(mat_bfRegionBinarized)] <- 0
  
  #Label the different regions of the scan using the edges as borders
  mat_bfRegionLabeled <- mat_bfRegionBinarized %>%
    imager::as.cimg() %>%
    imager::imgradient(scheme = 3) %>%
    imager::enorm() %>%
    imager::add() %>%
    imager::label(high_connectivity = high_connectivity,
                  tolerance = tolerance) %>%
    as.matrix()
  
  #The pixel in the center of the image should be a part of the firing pin
  #impression hole
  mat_bfRegioncenterLabel <- mat_bfRegionLabeled[round(nrow(mat_bfRegionLabeled)/2),round(ncol(mat_bfRegionLabeled)/2)]
  
  #Identify all pixels that share a label with the center pixel (these are
  #assumed to be only pixels that are a part of the firing pin impression)
  mat_bfRegionfpHoleIndices <- which(mat_bfRegionLabeled == mat_bfRegioncenterLabel,arr.ind = TRUE)
  
  #The center pixel of this region is assumed to be the center of the firing pin
  #impression hole
  mat_bfRegionfpHoleCenter <- round(colMeans(mat_bfRegionfpHoleIndices)) + centerOffset
  
  return(mat_bfRegionfpHoleCenter)
}

#Pads a surface matrix with NA rows/cols based on estimated firing pin center
fpCenterMat <- function(x3p,
                        scheme = 3,
                        high_connectivity = FALSE,
                        tolerance = 0,
                        centerOffset = c(0,0)){
  
  mat <- x3p$surface.matrix
  
  fpCenter <- fpCenterCalc(x3p,
                           scheme = scheme,
                           high_connectivity = high_connectivity,
                           tolerance = tolerance,
                           centerOffset = centerOffset)
  
  dimToPad <- 2*round(fpCenter - dim(mat)/2)
  
  rowPad <- matrix(NA,nrow = abs(dimToPad[1]),ncol = ncol(mat))
  
  #if fp center is below (larger in row index, starting from the top left corner),
  #then we want to pad the first few rows of the matrix
  if(sign(dimToPad[1]) == 1){
    mat1_padded <- rbind(mat,rowPad)
  }
  else{
    mat1_padded <- rbind(rowPad,mat)
  }
  
  colPad <- matrix(NA,nrow = nrow(mat1_padded),ncol = abs(dimToPad[2]))
  
  #if fp center is to the right of (larger in col index, starting from the top
  #left corner), then we want to pad the first few cols of the matrix
  if(sign(dimToPad[2]) == 1){
    mat1_padded <- cbind(mat1_padded,colPad)
  }
  else{
    mat1_padded <- cbind(colPad,mat1_padded)
  }
  #update metainformation in x3p too
  x3p$surface.matrix <- mat1_padded
  x3p$header.info$sizeX <- nrow(mat1_padded)
  x3p$header.info$sizeY <- ncol(mat1_padded)
  return(x3p)
}

#adapted from wvtools package. rotationResolution is the change in theta value
#between pixels (e.g., rotationResolution = .5 means output matrix will be
#360/.5 = 720 elements wide). Could perhaps do the same with radius resolution?
#cropWhitespace means that the NA rows/columns around the transformed breech
#face surface will be removed.
car2pol <- function (x3p, method = "nearest",rotationResolution = .5,cropWhitespace = TRUE){
  
  i.img <- x3p$surface.matrix
  
  if (method == "nearest") {
    p <- nrow(i.img)
    q <- ncol(i.img)
    m <- max(p, q)
    conv.img <- matrix(data = 0,
                       nrow = (trunc(m/2) - 1), #since we're unwrapping relative the center of the matrix, we want the height of the unwrapped image to be equal to have of the matrix dimension
                       ncol = length(seq(0,360,by = rotationResolution) + rotationResolution))
    
    for (th in seq(0,360,by = rotationResolution) + rotationResolution){
      for (r in 1:(trunc(m/2) - 1)) {
        th.rad <- pi * (th - 1)/180
        
        if(any(dim(i.img) < c(round(p/2 + (r - 1) * cos(th.rad)) - 1,
                              round(q/2 + (r - 1) * sin(th.rad))))){
          conv.img[r, th*as.integer(1/rotationResolution) - 1] <- NA
        }
        else if(length(i.img[round(p/2 + (r - 1) * cos(th.rad)) - 1,
                             round(q/2 + (r - 1) * sin(th.rad))]) != 1){
          conv.img[r, th*as.integer(1/rotationResolution) - 1] <- NA
        }
        
        else{
          conv.img[r, th*as.integer(1/rotationResolution) - 1] <- i.img[round(p/2 + (r - 1) * cos(th.rad)) - 1,
                                                                        round(q/2 + (r - 1) * sin(th.rad))]
        }
      }
    }
    
  }
  else if (method == "bilinear") {
    p <- nrow(i.img)
    q <- ncol(i.img)
    m <- max(p, q)
    xc <- p%/%2
    yc <- q%/%2
    conv.img <- matrix(data = 0,
                       nrow = (trunc(m/2) - 1),
                       ncol = length(seq(0,360,by = rotationResolution) + rotationResolution))
    
    for (th in seq(0,360,by = rotationResolution) + rotationResolution) {
      for (r in 1:(trunc(m/2) - 1)) {
        th.rad <- pi * (th - 1)/180
        Sxy <- c(xc + (r - 1) * cos(th.rad), yc +
                   (r - 1) * sin(th.rad))
        xy <- trunc(Sxy)
        dxy <- Sxy - xy
        
        if(any(nrow(i.img) < c(xy[1],xy[1] + 1)) |
           any(ncol(i.img) < c(xy[2],xy[2] + 1))){
          conv.img[r, th*as.integer(1/rotationResolution) - 1] <- NA
        }
        else{
          a <- matrix(c(i.img[xy[1], xy[2]], i.img[xy[1] + 1, xy[2]],
                        i.img[xy[1], xy[2] + 1], i.img[xy[1] + 1, xy[2] + 1]), 2, 2)
          
          b <- matrix(c(1 - dxy[1], dxy[1]), 1, 2)
          
          c <- matrix(c(1 - dxy[2], dxy[2]), 2, 1)
          
          conv.img[r, th*as.integer(1/rotationResolution) - 1] <- b %*% a %*% c
        }
      }
    }
  }
  
  #Remove rows/columns containing all 0s (which seems to often be the last
  #column of the matrix, perhaps due to some weirdness with interpolating the
  #end of the image.)
  zerosPerRow <- rowSums({conv.img == 0},na.rm = TRUE)
  
  conv.img <- conv.img[zerosPerRow < ncol(conv.img),]
  
  zerosPerCol <- colSums({conv.img == 0},na.rm = TRUE)
  
  conv.img <- conv.img[,zerosPerCol < nrow(conv.img)]
  
  if(cropWhitespace){
    #Remove rows/columns containing only NAs
    naPerRow <- conv.img %>%
      is.na() %>%
      rowSums()
    
    conv.img <- conv.img[naPerRow < ncol(conv.img),]
    
    naPerCol <- conv.img %>%
      is.na() %>%
      colSums()
    
    conv.img <- conv.img[,naPerCol < nrow(conv.img)]
  }
  
  x3p$surface.matrix <- conv.img
  x3p$header.info$sizeX <- nrow(conv.img)
  x3p$header.info$sizeY <- ncol(conv.img)
  
  return(x3p)
}

#Rotates a target scan to align with a reference scan by estimating the rotation
#in the polar domain
preProcess_rotateScan <- function(reference,
                                  target,
                                  polarInterpolation = "nearest",
                                  rotationResolution = .5,
                                  rotationInterpolation = 0,
                                  scheme = 3,
                                  high_connectivity = FALSE,
                                  tolerance = 0){
  
  #transform both surface matrices into polar coordinates after centering on
  #firing pin hole by interpolation
  reference_polar <- reference %>%
    fpCenterMat(scheme = scheme,
                high_connectivity = high_connectivity,
                tolerance = tolerance) %>%
    car2pol(method = polarInterpolation,
            rotationResolution = rotationResolution,
            cropWhitespace = FALSE)
  
  target_polar <- target %>%
    fpCenterMat(scheme = scheme,
                high_connectivity = high_connectivity,
                tolerance = tolerance) %>%
    car2pol(method = polarInterpolation,
            rotationResolution = rotationResolution,cropWhitespace = FALSE)
  
  #Finding optimal rotation via FFT-based CCF requires replacing missing values
  reference_polar$surface.matrix[is.na(reference_polar$surface.matrix)] <- mean(reference_polar$surface.matrix,na.rm = TRUE)
  target_polar$surface.matrix[is.na(target_polar$surface.matrix)] <- mean(target_polar$surface.matrix,na.rm = TRUE)
  
  ccfMat <- cmcR:::filterViaFFT(reference_polar$surface.matrix,target_polar$surface.matrix)
  # ccfMat <- imager::correlate(im = as.cimg(reference_polar$surface.matrix),
  #                             filter = as.cimg(target_polar$surface.matrix)) %>%
  #   as.matrix()
  
  #Find index at which maximum CCF occurs. The corresponding column represents
  #the rotation at which the two scans best align. Need to multiple by the
  #rotation resolution to get back the correct rotation value
  alignment <- (which(ccfMat == max(ccfMat),arr.ind = TRUE) - dim(ccfMat)/2)*rotationResolution
  
  #Imager by default pads the images with 0s, so we need to play some tricks to
  #make sure we can distinguish the original height values from these fake 0s
  #after rotating. We re-scale the values (to avoid numerical issues with
  #interpolation) and add 1, which we undo later on
  target_rotated <- target
  target_rotated$surface.matrix <- target_rotated$surface.matrix*1e5 + 1
  
  target_rotated$surface.matrix <- target_rotated$surface.matrix %>%
    imager::as.cimg() %>%
    imager::imrotate(cx = nrow(.)/2,
                     cy = ncol(.)/2,
                     angle = alignment[2],
                     interpolation = rotationInterpolation) %>%
    as.matrix()
  
  #Since we rescaled and shifted "true" values above, we know that any of the 0s
  #in the scan are added by imager
  target_rotated$surface.matrix[target_rotated$surface.matrix == 0] <- NA
  
  target_rotated$surface.matrix <- (target_rotated$surface.matrix - 1)/1e5
  
  return(target_rotated)
}

arrangeCMCPlot_shiny <- function(reference,
                                 target,
                                 allCells,
                                 x3pNames,
                                 pltType = "faceted",
                                 legend.quantiles = c(0,.01,.25,.5,.75,.99,1),
                                 height.colors = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                 cell.colors = c("#a50026","#313695"),
                                 cell.alpha = .2,
                                 na.value = "gray80",
                                 polar = FALSE){
  
  target_cellGrid <- allCells %>%
    dplyr::mutate(firstRow = (reference$header.info$incrementY*1e6)*(.data$firstRow),
                  lastRow = (reference$header.info$incrementY*1e6)*(.data$lastRow),
                  firstCol = (reference$header.info$incrementY*1e6)*(.data$firstCol),
                  lastCol = (reference$header.info$incrementY*1e6)*(.data$lastCol)) %>%
    dplyr::mutate(firstRowCentered = .data$firstRow - max(.data$lastRow)/2,
                  lastRowCentered = .data$lastRow - max(.data$lastRow)/2,
                  firstColCentered = .data$firstCol - max(.data$lastCol)/2,
                  lastColCentered = .data$lastCol - max(.data$lastCol)/2) %>%
    # dplyr::mutate(firstRow = firstColCentered + max(.data$lastRow)/2,
    #               lastRow = lastColCentered + max(.data$lastRow)/2,
    #               firstCol = -firstRowCentered + max(.data$lastCol)/2,
    #               lastCol = -lastRowCentered + max(.data$lastCol)/2) %>%
    dplyr::mutate(x_1 = .data$firstCol,
                  y_1 = .data$firstRow,
                  x_2 = .data$lastCol,
                  y_2 = .data$firstRow,
                  x_3 = .data$lastCol,
                  y_3 = .data$lastRow,
                  x_4 = .data$firstCol,
                  y_4 = .data$lastRow) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(c("x","y")),
                        names_to = c(".value","order"),
                        names_pattern = "(.+)_(.+)") %>%
    dplyr::mutate(midCol = (.data$lastCol + .data$firstCol)/2,
                  midRow = (.data$lastRow + .data$firstRow)/2,
                  x3p = rep(x3pNames[1],times = nrow(.)),
                  theta = rep(0,times = nrow(.)),
                  cellIndex = stringr::str_remove_all(string = cellIndex,pattern = " "))
  
  reference_cellGrid <- allCells %>%
    dplyr::mutate(firstRow = (target$header.info$incrementY*1e6)*(.data$firstRow),
                  lastRow = (target$header.info$incrementY*1e6)*(.data$lastRow),
                  firstCol = (target$header.info$incrementY*1e6)*(.data$firstCol),
                  lastCol = (target$header.info$incrementY*1e6)*(.data$lastCol)) %>%
    dplyr::mutate(firstRowCentered = .data$firstRow - max(.data$lastRow)/2,
                  lastRowCentered = .data$lastRow - max(.data$lastRow)/2,
                  firstColCentered = .data$firstCol - max(.data$lastCol)/2,
                  lastColCentered = .data$lastCol - max(.data$lastCol)/2) %>%
    dplyr::mutate(topLeftCorner_col = .data$firstColCentered*cos((.data$theta - median(.data$theta))*(pi/180)) - .data$lastRowCentered*sin((.data$theta - median(.data$theta))*(pi/180)) + max(.data$lastCol)/2 + (target$header.info$incrementY*1e6)*.data$y,
                  topLeftCorner_row = .data$firstColCentered*sin((.data$theta - median(.data$theta))*(pi/180)) + .data$lastRowCentered*cos((.data$theta - median(.data$theta))*(pi/180)) + max(.data$lastRow)/2 - (target$header.info$incrementY*1e6)*.data$x,
                  topRightCorner_col = .data$lastColCentered*cos((.data$theta - median(.data$theta))*(pi/180)) - .data$lastRowCentered*sin((.data$theta - median(.data$theta))*(pi/180)) + max(.data$lastCol)/2 + (target$header.info$incrementY*1e6)*.data$y,
                  topRightCorner_row = .data$lastColCentered*sin((.data$theta - median(.data$theta))*(pi/180)) + .data$lastRowCentered*cos((.data$theta - median(.data$theta))*(pi/180)) + max(.data$lastRow)/2 - (target$header.info$incrementY*1e6)*.data$x,
                  bottomRightCorner_col = .data$lastColCentered*cos((.data$theta - median(.data$theta))*(pi/180)) - .data$firstRowCentered*sin((.data$theta - median(.data$theta))*(pi/180)) + max(.data$lastCol)/2 + (target$header.info$incrementY*1e6)*.data$y,
                  bottomRightCorner_row = .data$lastColCentered*sin((.data$theta - median(.data$theta))*(pi/180)) + .data$firstRowCentered*cos((.data$theta - median(.data$theta))*(pi/180)) + max(.data$lastRow)/2 - (target$header.info$incrementY*1e6)*.data$x,
                  bottomLeftCorner_col = .data$firstColCentered*cos((.data$theta - median(.data$theta))*(pi/180)) - .data$firstRowCentered*sin((.data$theta - median(.data$theta))*(pi/180)) + max(.data$lastCol)/2 + (target$header.info$incrementY*1e6)*.data$y,
                  bottomLeftCorner_row = .data$firstColCentered*sin((.data$theta - median(.data$theta))*(pi/180)) + .data$firstRowCentered*cos((.data$theta - median(.data$theta))*(pi/180)) + max(.data$lastRow)/2 - (target$header.info$incrementY*1e6)*.data$x) %>%
    #this is redundant, but are the names attributed to the x and y columns are
    #set-up down below, so I won't change it
    # dplyr::mutate(x_1 = .data$topLeftCorner_col,
    #               y_1 = .data$topLeftCorner_row,
    #               x_2 = .data$topRightCorner_col,
    #               y_2 = .data$topRightCorner_row,
    #               x_3 = .data$bottomRightCorner_col,
    #               y_3 = .data$bottomRightCorner_row,
    #               x_4 = .data$bottomLeftCorner_col,
    #               y_4 = .data$bottomLeftCorner_row) %>%
    dplyr::mutate(x_1 = .data$topLeftCorner_col,
                  y_1 = .data$topLeftCorner_row,
                  x_2 = .data$topRightCorner_col,
                  y_2 = .data$topRightCorner_row,
                  x_3 = .data$bottomRightCorner_col,
                  y_3 = .data$bottomRightCorner_row,
                  x_4 = .data$bottomLeftCorner_col,
                  y_4 = .data$bottomLeftCorner_row) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(c("x","y")),
                        names_to = c(".value","order"),
                        names_pattern = "(.+)_(.+)") %>%
    dplyr::mutate(midCol = (.data$topRightCorner_col + .data$bottomLeftCorner_col)/2,
                  midRow = (.data$topRightCorner_row + .data$bottomLeftCorner_row)/2,
                  x3p = rep(x3pNames[2],times = nrow(.)),
                  theta = .data$theta - median(.data$theta),
                  cellIndex = stringr::str_remove_all(string = cellIndex,pattern = " "))
  
  if(!polar){
    target_rotate <- 90 #- abs(median(allCells %>%
    # dplyr::filter(cmc != "non-CMC") %>%
    # dplyr::pull(theta)))
    
    x3pPlt <- x3pListPlot(x3pList = list(reference,target) %>%
                            setNames(x3pNames),
                          type = pltType,
                          rotate = c(0,median(allCells$theta)),
                          legend.quantiles = legend.quantiles,
                          height.colors = height.colors,
                          na.value = na.value,
                          guide = "none")
  }
  else{
    
    x3pPlt <- x3pListPlot(x3pList = list(reference %>%
                                           x3ptools::x3p_rotate(angle = 90) %>%
                                           x3ptools::x3p_flip_y() %>%
                                           x3ptools::x3p_rotate(270),
                                         target %>%
                                           x3ptools::x3p_rotate(angle = 90) %>%
                                           x3ptools::x3p_flip_y() %>%
                                           x3ptools::x3p_rotate(270)) %>%
                            setNames(x3pNames),
                          type = pltType,
                          # rotate = c(90,
                          #            ifelse(is.na(target_rotate),90,target_rotate)),
                          legend.quantiles = legend.quantiles,
                          height.colors = height.colors,
                          na.value = na.value,
                          guide = "none")
    
    if(pltType == "faceted"){
      ranges <- ggplot_build(x3pPlt)$layout$panel_params[[1]][c('x.range', 'y.range')]
      sizes <- sapply(ranges, diff)
      aspect <- sizes[1] / sizes[2]
      
      suppressMessages({
        x3pPlt <- x3pPlt +
          ggplot2::facet_wrap(~ x3p,ncol = 1) +
          ggplot2::coord_flip(expand = FALSE) +
          ggplot2::theme_update(aspect.ratio = aspect)
      })
    }
    else{
      x3pPlt <- x3pPlt %>%
        purrr::map(~ {
          ranges <- ggplot_build(.)$layout$panel_params[[1]][c('x.range', 'y.range')]
          sizes <- sapply(ranges, diff)
          aspect <- sizes[1] / sizes[2]
          
          suppressMessages({
            . +
              ggplot2::facet_wrap(~ x3p,ncol = 1) +
              ggplot2::coord_flip(expand = FALSE) +
              ggplot2::theme_update(aspect.ratio = aspect)
          })
        })
    }
    
  }
  
  if(pltType == "faceted"){
    
    x3pPlt <- x3pPlt +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_polygon(data = target_cellGrid,
                            mapping = ggplot2::aes(x = .data$x,
                                                   y = .data$y,
                                                   group = .data$cellIndex,
                                                   fill = .data$cmc),
                            alpha = cell.alpha,
                            size = 2) +
      ggplot2::geom_polygon(data = reference_cellGrid,
                            mapping = ggplot2::aes(x = .data$x,
                                                   y = .data$y,
                                                   group = .data$cellIndex,
                                                   fill = .data$cmc),
                            alpha = cell.alpha,
                            size = 2) +
      ggplot2::geom_text(data = dplyr::bind_rows(target_cellGrid,
                                                 reference_cellGrid),
                         ggplot2::aes(x = .data$midCol,
                                      y = .data$midRow,
                                      label = .data$cellIndex,
                                      colour = .data$cmc,
                                      angle = .data$theta),
                         size = 3) +
      ggplot2::scale_colour_manual(values = cell.colors,
                                   aesthetics = c("fill","colour")) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Cell Type")) +
      ggplot2::theme(legend.position = "bottom")
  }
  else if(pltType == "list"){
    x3pPlt[[1]] <- x3pPlt[[1]] +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_polygon(data = target_cellGrid,
                            mapping = ggplot2::aes(x = .data$x,
                                                   y = .data$y,
                                                   group = .data$cellIndex,
                                                   fill = .data$cmc),
                            alpha = cell.alpha,
                            size = 2) +
      ggplot2::scale_colour_manual(values = cell.colors,
                                   aesthetics = c("fill","colour")) +
      ggplot2::geom_text(data = target_cellGrid,
                         ggplot2::aes(x = .data$midCol,
                                      y = .data$midRow,
                                      label = .data$cellIndex,
                                      colour = .data$cmc,
                                      angle = .data$theta),
                         size = 3) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Cell Type")) +
      ggplot2::theme(legend.position = "bottom")
    
    x3pPlt[[2]] <- x3pPlt[[2]] +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_polygon(data = reference_cellGrid,
                            mapping = ggplot2::aes(x = .data$x,
                                                   y = .data$y,
                                                   group = .data$cellIndex,
                                                   fill = .data$cmc),
                            alpha = cell.alpha,
                            size = 2) +
      ggplot2::scale_colour_manual(values = cell.colors,
                                   aesthetics = c("fill","colour")) +
      ggplot2::geom_text(data = reference_cellGrid,
                         ggplot2::aes(x = .data$midCol,
                                      y = .data$midRow,
                                      label = .data$cellIndex,
                                      colour = .data$cmc,
                                      angle = .data$theta),
                         size = 3) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Cell Type")) +
      ggplot2::theme(legend.position = "bottom")
  }
  
  return(x3pPlt)
}


cmcPlot_shiny <- function(reference,
                          target,
                          reference_v_target_CMCs,
                          target_v_reference_CMCs = reference_v_target_CMCs,
                          numCells = 64,
                          corColName = "pairwiseCompCor",
                          type = "faceted",
                          x3pNames = c("reference","target"),
                          legend.quantiles = c(0,.01,.25,.5,.75,.99,1),
                          height.colors = c("#1B1B1B","#404040","#7B7B7B","#B0B0B0","#DBDBDB","#F7F7F7","#E4E4E4","#C5C5C5","#999999","#717171","#4E4E4E"),
                          cell.colors = c("#a50026","#313695"),
                          cell.alpha = .2,
                          na.value = "gray80",
                          polar = FALSE){
  
  if(polar){
    reference_v_target_CMCs <- reference_v_target_CMCs %>%
      dplyr::mutate(theta = 0)
    
    target_v_reference_CMCs <- target_v_reference_CMCs %>%
      dplyr::mutate(theta = 0)
  }
  
  reference_cellCorners <- reference %>%
    comparison_cellDivision(numCells = numCells) %>%
    purrr::pmap_dfr(~ {
      idNum <- ..2$cmcR.info$cellRange %>%
        stringr::str_extract_all(string = ..2$cmcR.info$cellRange,
                                 pattern = "[0-9]{1,}") %>%
        unlist() %>%
        as.numeric()
      
      
      if(polar){
        data.frame(cellIndex = ..1,
                   firstRow = idNum[3],
                   lastRow = idNum[4],
                   firstCol = idNum[1],
                   lastCol = idNum[2],
                   stringsAsFactors = FALSE)
      }
      else{
        data.frame(cellIndex = ..1,
                   firstRow = idNum[1],
                   lastRow = idNum[2],
                   firstCol = idNum[3],
                   lastCol = idNum[4],
                   stringsAsFactors = FALSE)
      }
    })
  
  target_cellCorners <- target %>%
    comparison_cellDivision(numCells = numCells) %>%
    purrr::pmap_dfr(~ {
      idNum <- ..2$cmcR.info$cellRange %>%
        stringr::str_extract_all(string = ..2$cmcR.info$cellRange,
                                 pattern = "[0-9]{1,}") %>%
        unlist() %>%
        as.numeric()
      
      if(polar){
        data.frame(cellIndex = ..1,
                   firstRow = idNum[3],
                   lastRow = idNum[4],
                   firstCol = idNum[1],
                   lastCol = idNum[2],
                   stringsAsFactors = FALSE)
      }
      else{
        data.frame(cellIndex = ..1,
                   firstRow = idNum[1],
                   lastRow = idNum[2],
                   firstCol = idNum[3],
                   lastCol = idNum[4],
                   stringsAsFactors = FALSE)
      }
    })
  
  if(nrow(reference_v_target_CMCs) == 0){
    
    originalMethodCMCs <- as.data.frame(matrix(nrow = 0,ncol = ncol(reference_v_target_CMCs))) %>%
      setNames(names(reference_v_target_CMCs))
    
  }
  else{
    originalMethodCMCs <- reference_v_target_CMCs %>%
      dplyr::filter(.data$originalMethodClassif == "CMC")
  }
  
  nonoriginalMethodCMCs <- reference_v_target_CMCs %>%
    dplyr::filter(!(.data$cellIndex %in% originalMethodCMCs$cellIndex))
  
  if(nrow(nonoriginalMethodCMCs) > 0){
    nonoriginalMethodCMCs <- nonoriginalMethodCMCs %>%
      dplyr::group_by(.data$cellIndex) %>%
      dplyr::filter((!!as.name(corColName)) == max((!!as.name(corColName))))
  }
  
  allInitialCells_reference_v_target <- dplyr::bind_rows(originalMethodCMCs,nonoriginalMethodCMCs) %>%
    dplyr::mutate(cmc = ifelse(.data$originalMethodClassif == "CMC","Original Method CMC","non-CMC")) %>%
    dplyr::mutate(cmc = factor(.data$cmc,levels = c("non-CMC","Original Method CMC"))) %>%
    dplyr::left_join(reference_cellCorners,
                     by = "cellIndex")
  
  originalMethodCMCsPlt_reference_v_target <- arrangeCMCPlot_shiny(reference = reference,
                                                                   target = target,
                                                                   allCells = allInitialCells_reference_v_target,
                                                                   x3pNames = x3pNames,
                                                                   pltType = type,
                                                                   legend.quantiles = legend.quantiles,
                                                                   height.colors = height.colors,
                                                                   cell.colors = cell.colors,
                                                                   cell.alpha = cell.alpha,
                                                                   na.value = na.value,
                                                                   polar = polar)
  
  #If only data for one comparison direction were given, only plot the original
  #method CMCs in that direction
  if(assertthat::are_equal(reference_v_target_CMCs, target_v_reference_CMCs)){
    
    if(!is.null(reference_v_target_CMCs$highCMCClassif)){
      #If the necessary data to construct the High CMCs were given, then plot them
      #too.
      
      highCMCs_reference_v_target <- reference_v_target_CMCs %>%
        dplyr::filter(.data$highCMCClassif == "CMC")
      
      #Remaining cells not identified as High CMCs
      non_highCMCs_reference_v_target <- reference_v_target_CMCs %>%
        dplyr::filter(!(.data$cellIndex %in% highCMCs_reference_v_target$cellIndex)) %>%
        dplyr::group_by(.data$cellIndex) %>%
        dplyr::filter((!!as.name(corColName)) == max((!!as.name(corColName))))
      
      highCMC_plotData_reference_v_target <- dplyr::bind_rows(highCMCs_reference_v_target,
                                                              non_highCMCs_reference_v_target) %>%
        dplyr::mutate(cmc = ifelse(.data$highCMCClassif == "CMC","High CMC","non-CMC")) %>%
        dplyr::mutate(cmc = factor(.data$cmc,levels = c("non-CMC","High CMC"))) %>%
        dplyr::left_join(reference_cellCorners,
                         by = "cellIndex")
      
      highCMCPlt_reference_v_target <- arrangeCMCPlot_shiny(reference = reference,
                                                            target = target,
                                                            allCells = highCMC_plotData_reference_v_target,
                                                            x3pNames = x3pNames,
                                                            pltType = type,
                                                            legend.quantiles = legend.quantiles,
                                                            height.colors = height.colors,
                                                            cell.colors = cell.colors,
                                                            cell.alpha = cell.alpha,
                                                            na.value = na.value)
      
      return(list("Original Method CMCs" = originalMethodCMCsPlt_reference_v_target,
                  "High CMCs" = highCMCPlt_reference_v_target))
    }
    
    return(list("Original Method CMCs" = originalMethodCMCsPlt_reference_v_target))
    
  }
  
  #otherwise, create the original CMC plots for the other direction and return:
  
  if(nrow(target_v_reference_CMCs) == 0){
    
    originalMethodCMCs <- as.data.frame(matrix(nrow = 0,ncol = ncol(target_v_reference_CMCs))) %>%
      setNames(names(target_v_reference_CMCs))
    
  }
  else{
    originalMethodCMCs <- target_v_reference_CMCs %>%
      dplyr::filter(.data$originalMethodClassif == "CMC")
  }
  
  nonoriginalMethodCMCs <- target_v_reference_CMCs %>%
    dplyr::filter(!(.data$cellIndex %in% originalMethodCMCs$cellIndex)) %>%
    dplyr::group_by(.data$cellIndex) %>%
    dplyr::filter((!!as.name(corColName)) == max((!!as.name(corColName))))
  
  allInitialCells_target_v_reference <- dplyr::bind_rows(originalMethodCMCs,nonoriginalMethodCMCs) %>%
    dplyr::mutate(cmc = ifelse(.data$originalMethodClassif == "CMC","Original Method CMC","non-CMC")) %>%
    dplyr::mutate(cmc = factor(.data$cmc,levels = c("non-CMC","Original Method CMC"))) %>%
    dplyr::left_join(reference_cellCorners,
                     by = "cellIndex")
  
  originalMethodCMCsPlt_target_v_reference <- arrangeCMCPlot_shiny(reference = target,
                                                                   target = reference,
                                                                   allCells = allInitialCells_target_v_reference,
                                                                   x3pNames = rev(x3pNames),
                                                                   pltType = type,
                                                                   legend.quantiles = legend.quantiles,
                                                                   height.colors = height.colors,
                                                                   cell.colors = cell.colors,
                                                                   cell.alpha = cell.alpha,
                                                                   na.value = na.value,
                                                                   polar = polar)
  
  if(!hasName(reference_v_target_CMCs,"highCMCClassif") | !hasName(target_v_reference_CMCs,"highCMCClassif")){
    
    return(list("originalMethodCMCs_reference_v_target" = originalMethodCMCsPlt_reference_v_target,
                "originalMethodCMCs_target_v_reference" = originalMethodCMCsPlt_target_v_reference))
    
  }
  
  #If the necessary data to construct the High CMCs were given, then plot them
  #too.
  
  highCMCs_reference_v_target <- reference_v_target_CMCs %>%
    dplyr::filter(.data$highCMCClassif == "CMC")
  
  #Remaining cells not identified as High CMCs
  non_highCMCs_reference_v_target <- reference_v_target_CMCs %>%
    dplyr::filter(!(.data$cellIndex %in% highCMCs_reference_v_target$cellIndex)) %>%
    dplyr::group_by(.data$cellIndex) %>%
    dplyr::filter((!!as.name(corColName)) == max((!!as.name(corColName))))
  
  highCMC_plotData_reference_v_target <- dplyr::bind_rows(highCMCs_reference_v_target,
                                                          non_highCMCs_reference_v_target) %>%
    dplyr::mutate(cmc = ifelse(.data$highCMCClassif == "CMC","High CMC","non-CMC")) %>%
    dplyr::mutate(cmc = factor(.data$cmc,levels = c("non-CMC","High CMC"))) %>%
    dplyr::left_join(reference_cellCorners,
                     by = "cellIndex")
  
  highCMCPlt_reference_v_target <- arrangeCMCPlot_shiny(reference = reference,
                                                        target = target,
                                                        allCells = highCMC_plotData_reference_v_target,
                                                        x3pNames = x3pNames,
                                                        pltType = type,
                                                        legend.quantiles = legend.quantiles,
                                                        height.colors = height.colors,
                                                        cell.colors = cell.colors,
                                                        cell.alpha = cell.alpha,
                                                        na.value = na.value)
  
  #Different High CMCs may have been identified in the other direction -- we
  #need to plot those separately
  
  highCMCs_target_v_reference <- target_v_reference_CMCs %>%
    dplyr::filter(.data$highCMCClassif == "CMC")
  
  #Remaining cells not identified as High CMCs
  non_highCMCs_target_v_reference <- target_v_reference_CMCs %>%
    dplyr::filter(!(.data$cellIndex %in% highCMCs_target_v_reference$cellIndex)) %>%
    dplyr::group_by(.data$cellIndex) %>%
    dplyr::filter((!!as.name(corColName)) == max((!!as.name(corColName))))
  
  highCMC_plotData_target_v_reference <- dplyr::bind_rows(highCMCs_target_v_reference,
                                                          non_highCMCs_target_v_reference) %>%
    dplyr::mutate(cmc = ifelse(.data$highCMCClassif == "CMC","High CMC","non-CMC")) %>%
    dplyr::mutate(cmc = factor(.data$cmc,levels = c("non-CMC","High CMC"))) %>%
    dplyr::left_join(target_cellCorners,
                     by = "cellIndex")
  
  highCMCPlt_target_v_reference <- arrangeCMCPlot_shiny(reference = target,
                                                        target = reference,
                                                        allCells = highCMC_plotData_target_v_reference,
                                                        x3pNames = rev(x3pNames),
                                                        pltType = type,
                                                        legend.quantiles = legend.quantiles,
                                                        height.colors = height.colors,
                                                        cell.colors = cell.colors,
                                                        cell.alpha = cell.alpha,
                                                        na.value = na.value)
  
  
  return(list("originalMethodCMCs_reference_v_target" = originalMethodCMCsPlt_reference_v_target,
              "originalMethodCMCs_target_v_reference" = originalMethodCMCsPlt_target_v_reference,
              "highCMC_reference_v_target"= highCMCPlt_reference_v_target,
              "highCMC_target_v_reference"= highCMCPlt_target_v_reference))
  
}