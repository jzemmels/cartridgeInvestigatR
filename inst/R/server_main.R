kk <- reactiveVal(1)
hasname_x3p <- reactive({ assertthat::has_name(shiny.r$data, "x3p") })
hasname_scanid <- reactive({ assertthat::has_name(shiny.r$data, "scan_id") })

all_scan_id <- reactive({
  if(hasname_scanid()) {shiny.r$data$scan_id}
  else { NULL }
})

FROM_CONFIRM <- FALSE

# CONDITIONAL PANEL SETUP
output$hasname_x3p <- hasname_x3p
outputOptions(output, "hasname_x3p", suspendWhenHidden = FALSE)

output$hasname_scanid <- hasname_scanid
outputOptions(output, "hasname_scanid", suspendWhenHidden = FALSE)

###################################
output$selectk <- renderUI({
  # cat("render k select, using:", kk(),"\n")
  if(kk() <= dataPar$n) {
    selectInput("k","Investigate kth land:", selected = kk(),
                choices=1:dataPar$n,
                width = "100%")
  } else {
    NULL
  }
  
})

output$selectid <- renderUI({
  # cat("render scanID select \n")
  if(dataPar$hasname_scanid & kk() <= dataPar$n) {
    selectInput("scanID","Investigate according to the Scan ID :",
                selected = dataPar$all_scan_id[kk()],
                choices = dataPar$all_scan_id,
                width = "100%")
  } else {
    NULL
  }
})

observeEvent(input$scanID, {
  scanidx <- shiny.r$data %>% tibble::rowid_to_column() %>%
    filter(scan_id == isolate(input$scanID)) %>% pull(rowid)
  
  # cat("input$scanID is observed \n")
  
  if(!(FROM_CONFIRM & scanidx < kk())) {
    if(scanidx != kk()) {
      kk(scanidx)
      FROM_CONFIRM <<- FALSE
      # cat("assigning k a new value from scanID \n")
      # cat("the value of input$scanID:", isolate(input$scanID), "\n")
      # cat("the value of scanidx:", scanidx, "\n\n")
    }
  }
  
  
})
observeEvent(input$k, {
  
  # cat("input$k is observed, the value is:", input$k,  "\n")
  # cat(FROM_CONFIRM, "\n")
  # cat("the value of kk:", kk(), "\n")
  if(!(FROM_CONFIRM & as.numeric(input$k) < kk())) {
    if(input$k != kk()) {
      # cat("assigning k a new value:", input$k ,"\n")
      kk(as.numeric(isolate(input$k)))
      FROM_CONFIRM <<- FALSE
    }
  }
  
  if(FROM_CONFIRM & as.numeric(input$k) == kk()) { FROM_CONFIRM <<- FALSE }
  
})

# CONFIRM
observeEvent(input$confirm,{
  
  k.tmp <- isolate(kk())
  comments <- isolate(input$x3p_comment_box)
  
  if(!is.null(comments)) { shiny.r$data$comments[k.tmp] <- comments }
  
  # cat("Confirmed", k.tmp, "\n")
  if(k.tmp + 1 <= dataPar$n) {
    kk(k.tmp + 1)
    FROM_CONFIRM <<- TRUE
    # cat(FROM_CONFIRM ,"\n")
  } 
  # else {
  #   kk(1)
  #   FROM_CONFIRM <<- TRUE
  # }
  
})
#######################################

########################
# x3p types and comments

# render ui for x3p type
output$x3p_type_select_ui <- renderUI({
  if(kk() <= dataPar$n) {
    selectInput("x3p_type","type:", selected = shiny.r$data$type[kk()],
                choices=c("NA", "type1-regular", "type2-tankrashed", "type3-damaged",
                          "type4-too many missing values", "mark"))
  } else {
    NULL
  }
})

# change the x3p type
observeEvent(input$x3p_type, {
  shiny.r$data$type[kk()] <- input$x3p_type
})

# render ui for x3p comment box
output$x3p_comment_box_ui <- renderUI({
  if(kk() <= dataPar$n) {
    textAreaInput("x3p_comment_box", "Your Comments", value = shiny.r$data$comments[kk()],
                  rows = 2, width = "100%")
  } else {
    NULL
  }
})

# use the "confirm" button to save comments

########################################

observeEvent(input$saveCurrentEnv, {
  shiny.tt <<- shiny.r$data
  output$saveptp <- renderText({
    paste("Successfully saved your progress. A object called 'shiny.tt' should be found in your R environment")
  })
  
})

# check shiny.tt
observeEvent(input$ttcheck, {
  x3p_checker <- isolate(dataPar$hasname_x3p)
  crosscut_checker <- isolate(dataPar$hasname_crosscut)
  grooves_checker <- isolate(dataPar$hasname_grooves)
  scanid_checker <- isolate(dataPar$hasname_scanid)
  
  if(NOSHINY_TT) {
    output$checkresult <- renderText({
      "Could not find shiny.tt in the current environment. shiny.tt should be a tibble object."
    })
  } else {
    output$checkresult <- renderText({
      paste("Check if shiny.tt has the following variable names: \nx3p: ", x3p_checker,
            "\ncrosscut: ", crosscut_checker,
            "\ngrooves: ", grooves_checker,
            "\nscan_id: ", scanid_checker)
    })
  }
  
  if(x3p_checker & crosscut_checker & grooves_checker & scanid_checker) {
    output$suggest <- renderText({
      "Ready to go!"
    })
  } else if((!x3p_checker) & (!grooves_checker)) {
    output$suggest <- renderText({
      "Could not find x3p, could not find grooves. The app could not work properly."
    })
  } else {
    suggestion <- c("Could not find x3p; Functions that depend on x3p files would not be available.",
                    "Could not find grooves; grooves could be computed under Data Preparation tab.",
                    "Could not find crosscut; crosscut could be computed under Data Preparation tab.",
                    "Could not find scan_id; Searching by scan_id would not be available.")
    sug.idx <- c()
    if(!x3p_checker) { sug.idx <- c(sug.idx, 1) }
    if(!grooves_checker) { sug.idx <- c(sug.idx, 2) }
    if(!crosscut_checker) { sug.idx <- c(sug.idx, 3) }
    if(!scanid_checker) { sug.idx <- c(sug.idx, 4) }
    
    output$suggest <- renderText({
      paste(suggestion[sug.idx], collapse = "\n")
    })
  }
})


observe({
  if(!is.null(input$k)) {
    # if grooves are provided, draw plots
    if(dataPar$hasname_grooves){
      output$groovePlot <- renderPlot({
        k <- kk()
        
        if(assertthat::has_name(shiny.r$data$grooves[[k]], "groove")){
          p <- shiny.r$data$grooves[[k]]$plot +
            geom_vline(xintercept = shiny.r$data$grooves[[k]]$groove[1], colour="red") +
            geom_vline(xintercept = shiny.r$data$grooves[[k]]$groove[2], colour="red")
        }
        p
      })
      
      output$groovelocations <- renderText({
        paste("Left Groove: ",shiny.r$data$grooves[[kk()]]$groove[1],
              "    Right Groove: ",shiny.r$data$grooves[[kk()]]$groove[2])
      })
    }
    
    # if x3p are provided, give all other stuff
    if(dataPar$hasname_x3p){
      output$sigPlot <- renderPlot({
        k <- kk()
        ggplot() + geom_blank()
      })
      
      if(dataPar$hasname_crosscut){
        output$ccvalue <- renderText({
          paste("Current Crosscut Value: ",
                shiny.r$data$crosscut[kk()] ) })
      }
      
    }
  }
})



# SAVE 
output$downloadData <- downloadHandler(
  filename = function() {
    paste("processed_bullets.rds")
  },
  content = function(file) {
    saveRDS(isolate(shiny.r$data), file)
  }
)

# MARK
observeEvent(input$mark,{
  
  k <- isolate(kk())
  shiny.r$data$grooves[[k]]$marked <<- TRUE
  cat("Marked Groove:", k, "\n")
  if (dataPar$hasname_scanid) {
    cat("Marked Groove Bullet ID:", isolate(shiny.r$data$scan_id[k]), "\n")
  } else {
    cat("Bullet ID is not available. \n")
  }
  
})

# UNMARK
observeEvent(input$unmark,{
  shiny.r$data$grooves[[kk()]]$marked <<- FALSE
  cat("Unmarked Groove:", isolate(kk()), "\n")
})

# EVENT: UPDATE CROSSCUT VALUE
observeEvent(input$updateCC, {
  
  k <- kk()
  tmp.tt <- shiny.r$data %>% slice(k)
  
  tmp.tt$crosscut <- as.numeric(input$cc)
  
  tmp.tt <- tmp.tt %>%
    mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = bulletxtrctr::x3p_crosscut)
    ) %>%
    mutate(grooves = ccdata %>% purrr::map(.f = bulletxtrctr::cc_locate_grooves, method = "middle", adjust = 30, return_plot = TRUE))
  
  shiny.r$data[k, c("crosscut", "ccdata", "grooves")] <<- tmp.tt %>% select(crosscut, ccdata, grooves)
  
  cat("updated crosscut value: ", shiny.r$data$crosscut[k], "\n")
  if (isolate(dataPar$hasname_scanid)) {
    cat("Bullet ID:", shiny.r$data$scan_id[k], "\n")
  } else {
    cat("Bullet ID is not available. \n")
  }
  output$ccvalue <- renderText({
    # cat("test1")
    paste("Current Crosscut Value: ", shiny.r$data$crosscut[kk()]) })
  
  # shiny.r$data$changed_crosscut[k] <<- 'TRUE'
  
})

# Event: check if the crosscuts are changed
observeEvent(input$cc_status, {
  
  show_modal_spinner(spin = "atom", text = "Checking...")
  
  if(!assertthat::has_name(shiny.r$data, "crosscut_auto")) {
    shiny.r$data <<- shiny.r$data %>% mutate(
      crosscut_auto = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)
    ) 
  }
  
  shiny.r$data$changed_crosscut <<- as.character(shiny.r$data$crosscut != shiny.r$data$crosscut_auto)
  
  remove_modal_spinner()
})

# EVENT: DRAW SIGNATURE
observeEvent(input$drawsig, {
  k <- kk()
  
  # update: compute ccdata if null
  tmp.tt <- shiny.r$data %>% slice(k)
  
  if (!assertthat::has_name(tmp.tt, "ccdata")) {
    tmp.tt <- tmp.tt %>% mutate( 
      ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = bulletxtrctr::x3p_crosscut),
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)})
    )
  } else {
    tmp.tt <- tmp.tt %>% mutate(
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
      }))
  }
  
  output$sigPlot <- renderPlot({
    k <- kk()
    p <- tmp.tt$sigs[[1]] %>% filter(!is.na(sig), !is.na(raw_sig)) %>%
      ggplot(aes(x = x)) +
      geom_line(aes(y = raw_sig), colour = "grey70") +
      geom_line(aes(y = sig), colour = "grey30") +
      ylab("value") + ylim(c(-7.5, 7.5)) + theme_bw()
    p
  })
})

# EVENT: PLOT CLICK
observeEvent(input$plot_click,{
  k <- kk()
  xloc <- input$plot_click$x
  
  tmp <- isolate(shiny.r$data)
  
  gr <- tmp$grooves[[k]]$groove
  if (abs(gr[1]-xloc) < abs(gr[2]-xloc)) {
    shiny.r$data$grooves[[k]]$groove[1] <<- xloc
  } else {
    shiny.r$data$grooves[[k]]$groove[2] <<- xloc
  }
})

# EVENT: DISPLAY X3P
observeEvent(input$displayx3p, {
  req(shiny.r$data)
  req(input$k)
  
  k <- kk()
  
  tmp <- isolate(shiny.r$data)
  
  output$x3prgl <- renderRglwidget({
    if(isolate(dataPar$hasname_crosscut)){
      image_x3p(
        tmp$x3p[k][[1]] %>% x3p_add_hline(
          yintercept = tmp$crosscut[k], size = 10)) 
    } else {
      image_x3p(tmp$x3p[k][[1]]) 
    }
  })
})


# EVENT: SELECT K
observeEvent(input$k, {
  output$sigPlot <- renderPlot({
    k <- isolate(kk())
    ggplot() + geom_blank()
  })
})


# # EVENT: INPUT CROSSCUT VALUE WITH TEXT
# observeEvent(input$cc, {
#   updateSliderInput(session, "cc_slide", "change crosscut with a slider",
#                     min = 0, max = 1000,
#                     value = as.numeric(input$cc), step = 0.1)
# })
# # EVENT: INPUT CROSSCUT VALUE WITH SLIDER
# observeEvent(input$cc_slide, {
#   updateNumericInput(session, "cc", "change crosscut to:",
#                      as.numeric(input$cc_slide), min = 0, max = 1000)
# })

# SAVE CSV
output$save_csv <- downloadHandler(
  filename = function() {
    paste("my_output.csv")
  },
  content = function(file) {
    tmp <- isolate(shiny.r$data)
    if(!assertthat::has_name(tmp, "scan_id")) { tmp$scan_id <- NA }
    if(!assertthat::has_name(tmp, "crosscut_auto")) { tmp$crosscut_auto <- NA }
    if(!assertthat::has_name(tmp, "changed_crosscut")) { tmp$changed_crosscut <- NA }
    tmp$study <- isolate(input$save_csv_study_name)
    tmp$manual_code <- isolate(input$save_csv_id)
    tmp$manual_rep <- 1
    tmp$groove_left_manual <- sapply(tmp$grooves, function(g) {g$groove[1]} )
    tmp$groove_right_manual <- sapply(tmp$grooves, function(g) {g$groove[2]} )
    
    csv_tmp <- tmp %>% select(study, source, scan_id, crosscut, 
                              manual_code, manual_rep, 
                              groove_left_manual, groove_right_manual,
                              type, comments, crosscut_auto, changed_crosscut)
    # cat("good?", file)
    write.csv(csv_tmp, file)
    
  }
)

observeEvent(input$x3pdir,{
  if(!is.integer(input$x3pdir)) {
    dir <- parseDirPath(volumes, input$x3pdir)
    
    x3pNames <- list.files(dir)
    
    # initialize shiny.r
    shiny.r$data <<- try(read_bullet(dir))
    shiny.r$data <<- shiny.r$data %>%
      mutate(x3pNames = x3pNames)
    # shiny.r$data <<- shiny.r$data %>%
    #   mutate(estimatedCartridgeCaseCenter = rep(NA,length.out = nrow(.)),
    #          estimatedFiringPinCenter = rep(NA,length.out = nrow(.)))
    
    tmp <- isolate(shiny.r$data)
    
    updateSelectInput(inputId = "individualX3P",
                      label = "X3P",
                      choices = shiny.r$data$x3pNames,
                      session = getDefaultReactiveDomain())#1:nrow(shiny.r$data))
    updateSelectInput(inputId = "referenceSelect",
                      label = "Select Reference Scan",
                      choices = shiny.r$data$x3pNames,
                      session = getDefaultReactiveDomain())#1:nrow(shiny.r$data),selected = 1)
    updateSelectInput(inputId = "targetSelect",
                      label = "Select Target Scan",
                      choices = shiny.r$data$x3pNames,
                      ifelse(nrow(tmp) > 1,tmp$x3pNames[[2]],tmp$x3pNames[[1]]),
                      session = getDefaultReactiveDomain())
                      # choices = 1:nrow(shiny.r$data),
                      # selected = ifelse(nrow(shiny.r$data) > 1,2,1))
  }
})

# observeEvent({input$rotationselection},
#              {
#                
#                
#                
#                updateNumericInput(inputId = "thetaRangeMin",value = NULL)
#                updateNumericInput(inputId = "thetaRangeMax",value = NULL)
#                updateNumericInput(inputId = "thetaStep",value = NULL)
#                
#                updateTextInput(inputId = "thetaManualInput",value = "-3,0,3")
#              })

observeEvent(input$displayx3p2, {
  req(shiny.r$data)
  req(nrow(shiny.r$data) >= 1)
  req(input$plotSampling)
  
  k <- 1
  
  tmp <- isolate(shiny.r$data)
  
  show_modal_spinner(spin = "fingerprint", text = "Rendering plots...")
  
  output$x3prgl2 <- renderPlot({
    
    cmcR::x3pListPlot(tmp$x3p %>% purrr::map(~ sample_x3p(x3p = .,m = input$plotSampling)))
    
  })
  
  remove_modal_spinner()
})

observeEvent(input$displayx3p3, {
  req(shiny.r$data)
  req(nrow(shiny.r$data) >= 1)
  
  k <- 1
  
  tmp <- isolate(shiny.r$data)
  
  show_modal_spinner(spin = "fingerprint", text = "Rendering plots...")
  
  output$x3prgl3 <- renderPlot({
    
    cmcR::x3pListPlot(tmp$x3p_processed)
    
  })
  
  remove_modal_spinner()
  
})

observeEvent(input$toggleIndividualX3P, {
  req(shiny.r$data)
  req(nrow(shiny.r$data) >= 1)
  
  k <- 1
  
  tmp <- isolate(shiny.r$data)
  
  show_modal_spinner(spin = "fingerprint", text = "Rendering plots...")
  
  output$x3prgl_individual <- renderPlot({
    
    cmcR::x3pListPlot(list(shiny.r$data %>%
                             filter(x3pNames == input$individualX3P) %>%
                             pull(x3p) %>%
                             sample_x3p(m = input$plotSampling)))
    
  })
  
  remove_modal_spinner()
  
})

observeEvent(input$toggleIndividualX3P,{
  
  shinyjs::toggle("individualX3P_panel")
  
})

observeEvent(
  {input$clickExterior},{
    
    req(shiny.r$data)
    req(nrow(shiny.r$data) >= 1)
    
    tmp <- isolate(shiny.r$data)
    
    individualX3P <- tmp %>%
      filter(x3pNames == input$individualX3P)%>%
      pull(x3p)
    
    clickExterior <- input$clickExterior
    
    clickExterior$x <- nrow(individualX3P$surface.matrix)*1e6*individualX3P$header.info$incrementX - clickExterior$x
    clickExterior$y <- ncol(individualX3P$surface.matrix)*1e6*individualX3P$header.info$incrementY - clickExterior$y
    
    individualX3P_df <- individualX3P %>%
      x3ptools::x3p_to_df() %>%
      mutate(x = x*1e6,
             y = y*1e6) %>%
      mutate(distanceToClick = sqrt((x - clickExterior$x)^2 + (y - clickExterior$y)^2)) %>%
      top_n(-distanceToClick,n = 1) %>%
      mutate(x = x/1e6/individualX3P$header.info$incrementX,
             y = y/1e6/individualX3P$header.info$incrementY) %>%
      select(-c(distanceToClick,value)) %>%
      summarise(x = mean(x),
                y = mean(y))
    
    output$userDefinedCartridgeCaseCenter <- renderPrint({
      
      print("Estimated Cartridge Case Center")
      individualX3P_df %>%
        mutate(x = nrow(individualX3P$surface.matrix)*1e6*individualX3P$header.info$incrementX - x,
               y = ncol(individualX3P$surface.matrix)*1e6*individualX3P$header.info$incrementY - y)
      
    })
    
    suppressWarnings({
      if(is.null(shiny.r$data$estimatedCartridgeCaseCenter_x)){
        shiny.r$data <<- shiny.r$data %>%
          mutate(estimatedCartridgeCaseCenter_x = rep(-1,times = nrow(.)),
                 estimatedCartridgeCaseCenter_y = rep(-1,times = nrow(.)))
      }
      
      shiny.r$data <- shiny.r$data %>%
        mutate(x3pInd = 1:nrow(.),
               estimatedCartridgeCaseCenter_x = ifelse(x3pInd == which(tmp$x3pNames == input$individualX3P),
                                                       individualX3P_df$x[[1]],
                                                       estimatedCartridgeCaseCenter_x),
               estimatedCartridgeCaseCenter_y = ifelse(x3pInd == which(tmp$x3pNames == input$individualX3P),
                                                       individualX3P_df$y[[1]],
                                                       estimatedCartridgeCaseCenter_y))
    })
  })

observeEvent(
  {input$dbclickInterior},{
    
    req(shiny.r$data)
    req(nrow(shiny.r$data) >= 1)
    
    tmp <- isolate(shiny.r$data)
    
    individualX3P <- tmp %>%
      filter(x3pNames == input$individualX3P)%>%
      pull(x3p)
    
    dbclickInterior <- input$dbclickInterior
    
    dbclickInterior$x <- nrow(individualX3P$surface.matrix)*1e6*individualX3P$header.info$incrementX - dbclickInterior$x
    dbclickInterior$y <- ncol(individualX3P$surface.matrix)*1e6*individualX3P$header.info$incrementY - dbclickInterior$y
    
    
    individualX3P_df <- individualX3P %>%
      x3ptools::x3p_to_df() %>%
      mutate(x = x*1e6,
             y = y*1e6) %>%
      mutate(distanceToClick = sqrt((x - dbclickInterior$x)^2 + (y - dbclickInterior$y)^2)) %>%
      top_n(-distanceToClick,n = 1) %>%
      mutate(x = x/1e6/individualX3P$header.info$incrementX,
             y = y/1e6/individualX3P$header.info$incrementY) %>%
      select(-c(distanceToClick,value)) %>%
      summarise(x = mean(x),
                y = mean(y))
    
    output$userDefinedFiringPinCenter <- renderPrint({
      
      print("Estimated Firing Pin Center")
      individualX3P_df %>%
        mutate(x = nrow(individualX3P$surface.matrix)*1e6*individualX3P$header.info$incrementX - x,
               y = ncol(individualX3P$surface.matrix)*1e6*individualX3P$header.info$incrementY - y)
      
    })
    
    suppressWarnings({
      if(is.null(shiny.r$data$estimatedFiringPinCenter_x)){
        shiny.r$data <<- shiny.r$data %>%
          mutate(estimatedFiringPinCenter_x = rep(-1,times = nrow(.)),
                 estimatedFiringPinCenter_y = rep(-1,times = nrow(.)))
      }
      
      shiny.r$data <- shiny.r$data %>%
        mutate(x3pInd = 1:nrow(.),
               estimatedFiringPinCenter_x = ifelse(x3pInd == which(tmp$x3pNames == input$individualX3P),
                                                   individualX3P_df$x[[1]],
                                                   estimatedFiringPinCenter_x),
               estimatedFiringPinCenter_y = ifelse(x3pInd == which(tmp$x3pNames == input$individualX3P),
                                                   individualX3P_df$y[[1]],
                                                   estimatedFiringPinCenter_y))
    })
  })

observeEvent(input$removeBrush,{
  
  req(shiny.r$data)
  req(nrow(shiny.r$data) >= 1)
  
  tmp <- shiny.r$data %>%
    filter(x3pNames == input$individualX3P) %>%
    pull(x3p)
  
  brushRange <- input$removeBrush
  
  #Brush coordinates are flipped relative the actual surface matrix
  brushRange$xmax1 <- nrow(tmp$surface.matrix)*1e6*tmp$header.info$incrementX - brushRange$xmin
  brushRange$ymax1 <- ncol(tmp$surface.matrix)*1e6*tmp$header.info$incrementY - brushRange$ymin
  brushRange$xmin1 <- nrow(tmp$surface.matrix)*1e6*tmp$header.info$incrementX - brushRange$xmax
  brushRange$ymin1 <- ncol(tmp$surface.matrix)*1e6*tmp$header.info$incrementY - brushRange$ymax
  
  shiny.r$data$x3p[[which(tmp$x3pNames == input$individualX3P)]] <<- tmp %>%
    x3ptools::x3p_to_df() %>%
    mutate(x = x*1e6,
           y = y*1e6) %>%
    mutate(value = ifelse(x <= brushRange$xmax1 & x >= brushRange$xmin1 &
                            y <= brushRange$ymax1 & y >= brushRange$ymin1,
                          NA,value)) %>%
    mutate(x = x/1e6,
           y = y/1e6) %>%
    df_to_x3p()
  
  output$x3prgl_individual <- renderPlot({
    
    cmcR::x3pListPlot(list(shiny.r$data %>%
                             filter(x3pNames == input$individualX3P) %>%
                             pull(x3p) %>%
                             sample_x3p(m = input$plotSampling)))
    
  })
  
})

observeEvent(input$resetProcessing,
             {
               # 
               
               dir <- parseDirPath(volumes, input$x3pdir)
               
               x3pFileNames <- list.files(dir,"*.x3p",full.names = TRUE) 
               
               shiny.r$data$x3p[[which(shiny.r$data$x3pNames == input$individualX3P)]] <<- x3p_read(x3pFileNames[[which(shiny.r$data$x3pNames == input$individualX3P)]])
               
               shiny.r$data <<- shiny.r$data %>%
                 mutate(estimatedFiringPinCenter_x = rep(-1,times = nrow(.)),
                        estimatedFiringPinCenter_y = rep(-1,times = nrow(.)),
                        estimatedCartridgeCaseCenter_x = rep(-1,times = nrow(.)),
                        estimatedCartridgeCaseCenter_y = rep(-1,times = nrow(.)))
               
               output$x3prgl_individual <- renderPlot({
                 
                 cmcR::x3pListPlot(list(shiny.r$data$x3p[[which(shiny.r$data$x3pNames == input$individualX3P)]] %>%
                                          sample_x3p(m = input$plotSampling)))
                 
               })
               
               shinyjs::reset("clickExterior")
               
               output$userDefinedCartridgeCaseCenter <- renderPrint({
                 
                 print("Estimated Cartridge Case Center Reset")
                 
               })
               
               shinyjs::reset("clickInterior")
               
               output$userDefinedFiringPinCenter <- renderPrint({
                 
                 print("Estimated Firing Pin Center Reset")
                 
               })
               
               shinyjs::reset("removeBrush")
               
             })

observeEvent(input$skipPreprocessing,
             {
               
               # 
               shiny.r$data <<- shiny.r$data %>%
                 mutate(x3p_processed = x3p,
                        x3pNames = stringr::str_extract(source,"[^/]*$"))
               
               output$skipPreprocessingConfirm <- renderText({
                 
                 "Preprocessing skipped - continue to Comparison."
                 
               })
             })

output$preComparisonReference <- renderPlot({
  
  req(shiny.r$data)
  req(shiny.r$data$x3p_processed)
  
  reference <- shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$referenceSelect)]]
  target <- shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$targetSelect)]]
  
  referenceComparison <- reference %>%
    cmcR::comparison_cellDivision(numCells = input$numCells)
  
  #Transpose the row and col indices
  referenceComparison <- referenceComparison %>%
    tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ") %>%
    mutate(rowIndex = as.numeric(rowIndex),
           colIndex = as.numeric(colIndex))
  
  numCellsRoot <- as.integer(sqrt(input$numCells))
  # 
  for(ind in 1:floor(as.integer(sqrt(input$numCells))/2)){
    referenceComparison <- referenceComparison %>%
      mutate(colIndex = case_when(as.formula(colIndex == ind ~ as.double((numCellsRoot + (numCellsRoot - (ind - 1))))),
                                  as.formula(colIndex == (numCellsRoot - (ind-1)) ~ as.double(numCellsRoot + ind)),
                                  TRUE ~ as.double(colIndex)))
  }
  #Loop above misses the middle column index if cell array is odd x odd
  if((numCellsRoot %% 2) == 1){
    referenceComparison <- referenceComparison %>%
      mutate(colIndex = ifelse(colIndex == (numCellsRoot + 1)/2,
                               colIndex + numCellsRoot,
                               colIndex))
  }
  
  referenceComparison <- referenceComparison %>%
    mutate(colIndex = colIndex - numCellsRoot) %>%
    #Swap the row and col indices here
    mutate(cellIndex = paste0(colIndex,", ",rowIndex)) %>%
    select(c(cellIndex,cellHeightValues)) %>%
    mutate(cellPropMissing = cmcR::comparison_calcPropMissing(cellHeightValues))
  
  shiny.r$referenceComparison <<- referenceComparison
  
  surfaceMat_df <- referenceComparison %>%
    purrr::pmap_dfr(~ {
      
      x3p_to_df(..2) %>%
        dplyr::mutate(value = value - median(value,na.rm = TRUE)) %>%
        dplyr::mutate(height = value*1e6) %>%
        mutate(cellIndex = ..1,
               cellMissingProp = ..3)
      
    })
  
  surfaceMat_df <- surfaceMat_df %>%
    mutate(missingBelowThreshold = ifelse(cellMissingProp <= input$maxNonMissingProp,
                                          TRUE,FALSE)) %>%
    tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ") %>%
    mutate(x = max(x) - x,
           y = max(y) - y) %>%
    mutate(rowIndex = as.numeric(rowIndex),
           colIndex = as.numeric(colIndex))
  
  # numCellsRoot <- as.integer(sqrt(input$numCells))
  # 
  # for(ind in 1:floor(as.integer(sqrt(input$numCells))/2)){
  #   surfaceMat_df <- surfaceMat_df %>%
  #     mutate(colIndex = case_when(as.formula(colIndex == ind ~ as.double((numCellsRoot + (numCellsRoot - (ind - 1))))),
  #                                 as.formula(colIndex == (numCellsRoot - (ind-1)) ~ as.double(numCellsRoot + ind)),
  #                                 TRUE ~ as.double(colIndex)))
  # }
  # #Loop above misses the middle column index if cell array is odd x odd
  # if((numCellsRoot %% 2) == 1){
  #   surfaceMat_df <- surfaceMat_df %>%
  #     mutate(colIndex = ifelse(colIndex == (numCellsRoot + 1)/2,
  #                              colIndex + numCellsRoot,
  #                              colIndex))
  # }
  
  plt <- surfaceMat_df %>%
    # mutate(colIndex = as.integer(colIndex - numCellsRoot)) %>%
    ggplot(aes(x = x,y = y)) +
    geom_raster(aes(fill = height,alpha = missingBelowThreshold)) +
    ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6',
                                                  '#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                  values = scales::rescale(quantile(surfaceMat_df$height,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                  breaks = function(lims){
                                    dat <- quantile(surfaceMat_df$height,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                    
                                    dat <- dat %>%
                                      setNames(paste0(names(dat)," [",round(dat,1),"]"))
                                    
                                    return(dat)
                                  },
                                  na.value = "grey80") +
    facet_grid(rows = vars(rowIndex),
               cols = vars(colIndex)) +
    scale_alpha_manual(values = c(.3,1)) +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(2.5,"in"),
                                                    label.theme = ggplot2::element_text(size = 8),
                                                    title.theme = ggplot2::element_text(size = 10),
                                                    frame.colour = "black",
                                                    ticks.colour = "black"),
                    alpha =  'none') + 
    ggplot2::labs(fill = expression("Rel. Height ["*mu*"m]")) +
    ggtitle(input$referenceSelect)
  
  plt
  
})

#Under the Comparison tab, want to show both the scan partitioned into cells and
#the scan to which it will be compared
output$preComparisonWholeScanTarget <- renderPlot({
  
  plt <- cmcR::x3pListPlot(list(shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$targetSelect)]]) %>%
                             magrittr::set_names(input$targetSelect),
                           type = "list")
  
  plt[[1]] +
    theme_replace(axis.title.x = ggplot2::element_blank(),
                  axis.text.x = ggplot2::element_blank(),
                  axis.ticks.x = ggplot2::element_blank(),
                  axis.title.y = ggplot2::element_blank(),
                  axis.text.y = ggplot2::element_blank(),
                  axis.ticks.y = ggplot2::element_blank(),
                  panel.grid.major = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank())
})

#If comparison is to be made in both directions, plot the target vs. reference
observeEvent(input$bothDirectionsCheck,{
  
  req(shiny.r$data)
  req(shiny.r$data$x3p_processed)
  
  output$preComparisonTarget <- renderPlot({
    
    reference <- shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$referenceSelect)]]
    target <- shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$targetSelect)]]
    
    targetComparison <- target %>%
      cmcR::comparison_cellDivision(numCells = input$numCells)
    
    #Transpose the row and col indices
    targetComparison <- targetComparison %>%
      tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ") %>%
      mutate(rowIndex = as.numeric(rowIndex),
             colIndex = as.numeric(colIndex))
    
    numCellsRoot <- as.integer(sqrt(input$numCells))
    # 
    for(ind in 1:floor(as.integer(sqrt(input$numCells))/2)){
      targetComparison <- targetComparison %>%
        mutate(colIndex = case_when(as.formula(colIndex == ind ~ as.double((numCellsRoot + (numCellsRoot - (ind - 1))))),
                                    as.formula(colIndex == (numCellsRoot - (ind-1)) ~ as.double(numCellsRoot + ind)),
                                    TRUE ~ as.double(colIndex)))
    }
    #Loop above misses the middle column index if cell array is odd x odd
    if((numCellsRoot %% 2) == 1){
      targetComparison <- targetComparison %>%
        mutate(colIndex = ifelse(colIndex == (numCellsRoot + 1)/2,
                                 colIndex + numCellsRoot,
                                 colIndex))
    }
    
    targetComparison <- targetComparison %>%
      mutate(colIndex = colIndex - numCellsRoot) %>%
      #Swap the row and col indices here
      mutate(cellIndex = paste0(colIndex,", ",rowIndex)) %>%
      select(c(cellIndex,cellHeightValues)) %>%
      mutate(cellPropMissing = cmcR::comparison_calcPropMissing(cellHeightValues))
    
    shiny.r$targetComparison <<- targetComparison
    
    surfaceMat_df <- targetComparison %>%
      purrr::pmap_dfr(~ {
        
        x3p_to_df(..2) %>%
          dplyr::mutate(value = value - median(value,na.rm = TRUE)) %>%
          dplyr::mutate(height = value*1e6) %>%
          mutate(cellIndex = ..1,
                 cellMissingProp = ..3)
        
      })
    
    surfaceMat_df <- surfaceMat_df %>%
      mutate(missingBelowThreshold = ifelse(cellMissingProp <= input$maxNonMissingProp,
                                            TRUE,FALSE)) %>%
      tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ") %>%
      mutate(x = max(x) - x,
             y = max(y) - y) %>%
      mutate(rowIndex = as.numeric(rowIndex),
             colIndex = as.numeric(colIndex))
    
    # numCellsRoot <- as.integer(sqrt(input$numCells))
    # 
    # for(ind in 1:floor(as.integer(sqrt(input$numCells))/2)){
    #   surfaceMat_df <- surfaceMat_df %>%
    #     mutate(colIndex = case_when(as.formula(colIndex == ind ~ as.double((numCellsRoot + (numCellsRoot - (ind - 1))))),
    #                                 as.formula(colIndex == (numCellsRoot - (ind-1)) ~ as.double(numCellsRoot + ind)),
    #                                 TRUE ~ as.double(colIndex)))
    # }
    # #Loop above misses the middle column index if cell array is odd x odd
    # if((numCellsRoot %% 2) == 1){
    #   surfaceMat_df <- surfaceMat_df %>%
    #     mutate(colIndex = ifelse(colIndex == (numCellsRoot + 1)/2,
    #                              colIndex + numCellsRoot,
    #                              colIndex))
    # }
    
    plt <- surfaceMat_df %>%
      # mutate(colIndex = as.integer(colIndex - numCellsRoot)) %>%
      ggplot(aes(x = x,y = y)) +
      geom_raster(aes(fill = height,alpha = missingBelowThreshold)) +
      ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6',
                                                    '#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                    values = scales::rescale(quantile(surfaceMat_df$height,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                    breaks = function(lims){
                                      dat <- quantile(surfaceMat_df$height,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                      
                                      dat <- dat %>%
                                        setNames(paste0(names(dat)," [",round(dat,1),"]"))
                                      
                                      return(dat)
                                    },
                                    na.value = "grey80") +
      facet_grid(rows = vars(rowIndex),
                 cols = vars(colIndex)) +
      scale_alpha_manual(values = c(.3,1)) +
      ggplot2::coord_fixed(expand = FALSE) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(2.5,"in"),
                                                      label.theme = ggplot2::element_text(size = 8),
                                                      title.theme = ggplot2::element_text(size = 10),
                                                      frame.colour = "black",
                                                      ticks.colour = "black"),
                      alpha =  'none') + 
      ggplot2::labs(fill = expression("Rel. Height ["*mu*"m]")) +
      ggtitle(input$targetSelect)
    
    plt
    
  })
  
  output$preComparisonWholeScanReference <- renderPlot({
    
    plt <- cmcR::x3pListPlot(list(shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$referenceSelect)]]) %>%
                               magrittr::set_names(input$referenceSelect),
                             type = "list")
    
    plt[[1]] +
      theme_replace(axis.title.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank(),
                    axis.title.y = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank())
    
  })
})

observeEvent(input$comparisonButton,{
  
  req(shiny.r$data)
  req(shiny.r$referenceComparison)
  
  reference <- shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$referenceSelect)]]
  target <- shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$targetSelect)]]
  
  
  show_modal_spinner(spin = "atom", text = "Comparing Scans...")
  if(input$preRotateScans){
    
    target <- preProcess_rotateScan(reference = reference,
                                    target = target)
    
  }
  
  # if(input$rotationselection == "by Range"){
  #   thetas <- seq(input$thetaRangeMin,
  #                 input$thetaRangeMax,
  #                 by = input$thetaStep)
  # }
  # else if(input$rotationselection == "Manual"){
  #   thetas <- stringr::str_split(input$thetaManualInput,pattern = ",") %>%
  #     unlist() %>%
  #     as.numeric()
  # }
  if(input$rotationSelection){
    thetas <- stringr::str_split(input$thetaManualInput,pattern = ",") %>%
      unlist() %>%
      as.numeric()
  }
  else{
    thetas <- seq(input$thetaRangeMin,
                  input$thetaRangeMax,
                  by = input$thetaStep)
  }
  
  comparisonData <- 
    purrr::map_dfr(thetas,
                   function(theta){
                     # 
                     dat <- reference %>%
                       cmcR::comparison_cellDivision(numCells = input$numCells) 
                     
                     #Transpose the row and col indices
                     dat <- dat %>%
                       tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ") %>%
                       mutate(rowIndex = as.numeric(rowIndex),
                              colIndex = as.numeric(colIndex)) 
                     
                     numCellsRoot <- as.integer(sqrt(input$numCells))
                     # 
                     for(ind in 1:floor(as.integer(sqrt(input$numCells))/2)){
                       dat <- dat %>%
                         mutate(colIndex = case_when(as.formula(colIndex == ind ~ as.double((numCellsRoot + (numCellsRoot - (ind - 1))))),
                                                     as.formula(colIndex == (numCellsRoot - (ind-1)) ~ as.double(numCellsRoot + ind)),
                                                     TRUE ~ as.double(colIndex)))
                     }
                     #Loop above misses the middle column index if cell array is odd x odd
                     if((numCellsRoot %% 2) == 1){
                       dat <- dat %>%
                         mutate(colIndex = ifelse(colIndex == (numCellsRoot + 1)/2,
                                                  colIndex + numCellsRoot,
                                                  colIndex))
                     }
                     
                     dat <- dat %>%
                       mutate(colIndex = colIndex - numCellsRoot) %>%
                       #Swap the row and col indices here
                       mutate(cellIndex = paste0(colIndex,", ",rowIndex)) %>%
                       select(c(cellIndex,cellHeightValues)) %>%
                       dplyr::mutate(regionHeightValues = cmcR::comparison_getTargetRegions(cellHeightValues = cellHeightValues,
                                                                                            target = target,
                                                                                            theta = theta)) %>%
                       dplyr::mutate(cellPropMissing = cmcR::comparison_calcPropMissing(cellHeightValues),
                                     regionPropMissing = cmcR::comparison_calcPropMissing(regionHeightValues)) %>%
                       dplyr::filter(cellPropMissing <= input$maxNonMissingProp & regionPropMissing <= input$maxNonMissingProp) %>%
                       dplyr::mutate(cellHeightValues = cmcR::comparison_standardizeHeights(cellHeightValues),
                                     regionHeightValues = cmcR::comparison_standardizeHeights(regionHeightValues)) %>%
                       dplyr::mutate(cellHeightValues_replaced = cmcR::comparison_replaceMissing(cellHeightValues),
                                     regionHeightValues_replaced = cmcR::comparison_replaceMissing(regionHeightValues)) %>%
                       dplyr::mutate(fft_ccf_df = cmcR::comparison_fft_ccf(cellHeightValues = cellHeightValues_replaced,
                                                                           regionHeightValues = regionHeightValues_replaced)) %>%
                       dplyr::mutate(pairwiseCompCor = cmcR::comparison_cor(cellHeightValues,regionHeightValues,fft_ccf_df)) %>%
                       tidyr::unnest(fft_ccf_df) %>%
                       dplyr::select(cellIndex,x,y,fft_ccf,pairwiseCompCor,cellHeightValues,regionHeightValues,cellHeightValues_replaced,regionHeightValues_replaced) %>%
                       dplyr::mutate(theta = theta,
                                     direction = "refToTarget")
                     
                     dat
                     
                   })
  
  #if comparisons are to be performed in both directions, then we want to compare target to reference too
  if(input$bothDirectionsCheck){
    
    req(shiny.r$targetComparison)
    
    targetToRefComparison <- 
      purrr::map_dfr(thetas,
                     function(theta){
                       
                       dat <- target %>%
                         cmcR::comparison_cellDivision(numCells = input$numCells)
                       
                       #Transpose the row and col indices
                       dat <- dat %>%
                         tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ") %>%
                         mutate(rowIndex = as.numeric(rowIndex),
                                colIndex = as.numeric(colIndex))
                       
                       numCellsRoot <- as.integer(sqrt(input$numCells))
                       # 
                       for(ind in 1:floor(as.integer(sqrt(input$numCells))/2)){
                         dat <- dat %>%
                           mutate(colIndex = case_when(as.formula(colIndex == ind ~ as.double((numCellsRoot + (numCellsRoot - (ind - 1))))),
                                                       as.formula(colIndex == (numCellsRoot - (ind-1)) ~ as.double(numCellsRoot + ind)),
                                                       TRUE ~ as.double(colIndex)))
                       }
                       #Loop above misses the middle column index if cell array is odd x odd
                       if((numCellsRoot %% 2) == 1){
                         dat <- dat %>%
                           mutate(colIndex = ifelse(colIndex == (numCellsRoot + 1)/2,
                                                    colIndex + numCellsRoot,
                                                    colIndex))
                       }
                       
                       dat <- dat %>%
                         mutate(colIndex = colIndex - numCellsRoot) %>%
                         #Swap the row and col indices here
                         mutate(cellIndex = paste0(colIndex,", ",rowIndex)) %>%
                         select(c(cellIndex,cellHeightValues)) %>%
                         dplyr::mutate(regionHeightValues = cmcR::comparison_getTargetRegions(cellHeightValues = cellHeightValues,
                                                                                              target = reference,
                                                                                              theta = theta)) %>%
                         dplyr::mutate(cellPropMissing = cmcR::comparison_calcPropMissing(cellHeightValues),
                                       regionPropMissing = cmcR::comparison_calcPropMissing(regionHeightValues)) %>%
                         dplyr::filter(cellPropMissing <= input$maxNonMissingProp & regionPropMissing <= input$maxNonMissingProp) %>%
                         dplyr::mutate(cellHeightValues = cmcR::comparison_standardizeHeights(cellHeightValues),
                                       regionHeightValues = cmcR::comparison_standardizeHeights(regionHeightValues)) %>%
                         dplyr::mutate(cellHeightValues_replaced = cmcR::comparison_replaceMissing(cellHeightValues),
                                       regionHeightValues_replaced = cmcR::comparison_replaceMissing(regionHeightValues)) %>%
                         dplyr::mutate(fft_ccf_df = cmcR::comparison_fft_ccf(cellHeightValues = cellHeightValues_replaced,
                                                                             regionHeightValues = regionHeightValues_replaced)) %>%
                         dplyr::mutate(pairwiseCompCor = cmcR::comparison_cor(cellHeightValues,regionHeightValues,fft_ccf_df)) %>%
                         tidyr::unnest(fft_ccf_df) %>%
                         dplyr::select(cellIndex,x,y,fft_ccf,pairwiseCompCor,cellHeightValues,regionHeightValues,cellHeightValues_replaced,regionHeightValues_replaced) %>%
                         dplyr::mutate(theta = theta,
                                       direction = "targetToRef")
                       
                       dat
                       
                     })
    
    comparisonData <- bind_rows(comparisonData,
                                targetToRefComparison)
  }
  
  shiny.r$comparisonData <<- comparisonData
  
  output$postComparisonReference <- renderPlot({
    
    surfaceMat_df <- shiny.r$referenceComparison %>%
      purrr::pmap_dfr(~ {
        
        x3p_to_df(..2) %>%
          dplyr::mutate(value = value - median(value,na.rm = TRUE)) %>%
          dplyr::mutate(#x = x*1e6,
            # y = y*1e6,
            height = value*1e6) %>%
          mutate(cellIndex = ..1,
                 cellMissingProp = ..3)
        
      })
    
    surfaceMat_df <- surfaceMat_df %>%
      mutate(missingBelowThreshold = ifelse(cellIndex %in% {comparisonData %>%
          filter(direction == "refToTarget") %>%
          pull(cellIndex) %>%
          unique()},
          TRUE,FALSE)) %>%
      # mutate(missingBelowThreshold = ifelse(cellPropMissing <= input$maxNonMissingProp & 
      #                                         regionPropMissing <= input$maxNonMissingProp,
      #                                       TRUE,FALSE)) %>%
      tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ") %>%
      mutate(x = max(x) - x,
             y = max(y) - y) %>%
      mutate(rowIndex = as.numeric(rowIndex),
             colIndex = as.numeric(colIndex)) 
    
    # numCellsRoot <- as.integer(sqrt(input$numCells))
    # # 
    # for(ind in 1:floor(as.integer(sqrt(input$numCells))/2)){
    #   surfaceMat_df <- surfaceMat_df %>%
    #     mutate(colIndex = case_when(as.formula(colIndex == ind ~ as.double((numCellsRoot + (numCellsRoot - (ind - 1))))),
    #                                 as.formula(colIndex == (numCellsRoot - (ind-1)) ~ as.double(numCellsRoot + ind)),
    #                                 TRUE ~ as.double(colIndex)))
    # }
    # #Loop above misses the middle column index if cell array is odd x odd
    # if((numCellsRoot %% 2) == 1){
    #   surfaceMat_df <- surfaceMat_df %>%
    #     mutate(colIndex = ifelse(colIndex == (numCellsRoot + 1)/2,
    #                              colIndex + numCellsRoot,
    #                              colIndex))
    # }
    
    plt <- surfaceMat_df %>%
      # mutate(colIndex = as.integer(colIndex - numCellsRoot)) %>%
      ggplot(aes(x = x,y = y)) +
      geom_raster(aes(fill = height,alpha = missingBelowThreshold)) +
      ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6',
                                                    '#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                    values = scales::rescale(quantile(surfaceMat_df$height,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                    breaks = function(lims){
                                      dat <- quantile(surfaceMat_df$height,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                      
                                      dat <- dat %>%
                                        setNames(paste0(names(dat)," [",round(dat,1),"]"))
                                      
                                      return(dat)
                                    },
                                    na.value = "grey80") +
      facet_grid(rows = vars(rowIndex),
                 cols = vars(colIndex)) +
      scale_alpha_manual(values = c(.3,1)) +
      ggplot2::coord_fixed(expand = FALSE) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(2.5,"in"),
                                                      label.theme = ggplot2::element_text(size = 8),
                                                      title.theme = ggplot2::element_text(size = 10),
                                                      frame.colour = "black",
                                                      ticks.colour = "black"),
                      alpha =  'none') + 
      ggplot2::labs(fill = expression("Rel. Height ["*mu*"m]")) +
      ggtitle(input$referenceSelect)
    
    plt
    
  })
  
  if(input$bothDirectionsCheck){
    
    output$postComparisonTarget <- renderPlot({
      
      surfaceMat_df <- shiny.r$targetComparison %>%
        purrr::pmap_dfr(~ {
          
          x3p_to_df(..2) %>%
            dplyr::mutate(value = value - median(value,na.rm = TRUE)) %>%
            dplyr::mutate(#x = x*1e6,
              # y = y*1e6,
              height = value*1e6) %>%
            mutate(cellIndex = ..1,
                   cellMissingProp = ..3)
          
        })
      
      surfaceMat_df <- surfaceMat_df %>%
        mutate(missingBelowThreshold = ifelse(cellIndex %in% {comparisonData %>%
            filter(direction == "targetToRef") %>%
            pull(cellIndex) %>%
            unique()},
            TRUE,FALSE)) %>%
        tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ") %>%
        mutate(x = max(x) - x,
               y = max(y) - y) %>%
        mutate(rowIndex = as.numeric(rowIndex),
               colIndex = as.numeric(colIndex))
      
      # numCellsRoot <- as.integer(sqrt(input$numCells))
      # # 
      # for(ind in 1:floor(as.integer(sqrt(input$numCells))/2)){
      #   surfaceMat_df <- surfaceMat_df %>%
      #     mutate(colIndex = case_when(as.formula(colIndex == ind ~ as.double((numCellsRoot + (numCellsRoot - (ind - 1))))),
      #                                 as.formula(colIndex == (numCellsRoot - (ind-1)) ~ as.double(numCellsRoot + ind)),
      #                                 TRUE ~ as.double(colIndex)))
      # }
      # #Loop above misses the middle column index if cell array is odd x odd
      # if((numCellsRoot %% 2) == 1){
      #   surfaceMat_df <- surfaceMat_df %>%
      #     mutate(colIndex = ifelse(colIndex == (numCellsRoot + 1)/2,
      #                              colIndex + numCellsRoot,
      #                              colIndex))
      # }
      
      plt <- surfaceMat_df %>%
        # mutate(colIndex = as.integer(colIndex - numCellsRoot)) %>%
        ggplot(aes(x = x,y = y)) +
        geom_raster(aes(fill = height,alpha = missingBelowThreshold)) +
        ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6',
                                                      '#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                      values = scales::rescale(quantile(surfaceMat_df$height,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                      breaks = function(lims){
                                        dat <- quantile(surfaceMat_df$height,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                        
                                        dat <- dat %>%
                                          setNames(paste0(names(dat)," [",round(dat,1),"]"))
                                        
                                        return(dat)
                                      },
                                      na.value = "grey80") +
        facet_grid(rows = vars(rowIndex),
                   cols = vars(colIndex)) +
        scale_alpha_manual(values = c(.3,1)) +
        ggplot2::coord_fixed(expand = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank()) +
        ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(2.5,"in"),
                                                        label.theme = ggplot2::element_text(size = 8),
                                                        title.theme = ggplot2::element_text(size = 10),
                                                        frame.colour = "black",
                                                        ticks.colour = "black"),
                        alpha =  'none') + 
        ggplot2::labs(fill = expression("Rel. Height ["*mu*"m]")) +
        ggtitle(input$targetSelect)
      
      plt
      
    })
  }
  
  remove_modal_spinner()
})

observeEvent({input$reference_click},
             {
               
               req(shiny.r$data)
               req(shiny.r$referenceComparison)
               req(input$reference_click$panelvar1)
               req(input$reference_click$panelvar2)
               
               show_modal_spinner(spin = "atom", text = "Pulling data for cell comparison...")
               
               comparisonData <- shiny.r$comparisonData %>%
                 filter(direction == "refToTarget") %>%
                 tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ")
               
               # numCellsRoot <- as.integer(sqrt(input$numCells))
               # # 
               # for(ind in 1:floor(as.integer(sqrt(input$numCells))/2)){
               #   comparisonData <- comparisonData %>%
               #     mutate(colIndex = case_when(as.formula(colIndex == ind ~ as.double((numCellsRoot + (numCellsRoot - (ind - 1))))),
               #                                 as.formula(colIndex == (numCellsRoot - (ind-1)) ~ as.double(numCellsRoot + ind)),
               #                                 TRUE ~ as.double(colIndex)))
               # }
               # #Loop above misses the middle column index if cell array is odd x odd
               # if((numCellsRoot %% 2) == 1){
               #   comparisonData <- comparisonData %>%
               #     mutate(colIndex = ifelse(colIndex == (numCellsRoot + 1)/2,
               #                              colIndex + numCellsRoot,
               #                              colIndex))
               # }
               # 
               comparisonData <- comparisonData %>%
                 # mutate(colIndex = as.integer(colIndex - numCellsRoot)) %>%
                 filter(rowIndex == input$reference_click$panelvar2 & colIndex == input$reference_click$panelvar1)
               
               surfaceMat_df <- comparisonData %>%
                 purrr::pmap_dfr(~ {
                   
                   x3p_to_df(..7) %>%
                     dplyr::mutate(value = value - median(value,na.rm = TRUE)) %>%
                     dplyr::mutate(height = value) %>%
                     mutate(rowIndex = ..1,
                            colIndex = ..2)
                   
                 }) %>%
                 mutate(x = max(x) - x,
                        y = max(y) - y) %>%
                 mutate(rowIndex = as.numeric(rowIndex),
                        colIndex = as.numeric(colIndex))
               
               output$cellPlot <- renderPlot({
                 
                 plt <- surfaceMat_df %>%
                   ggplot(aes(x = x,y = y)) +
                   geom_raster(aes(fill = height)) +
                   ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6',
                                                                 '#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                                 values = scales::rescale(quantile(surfaceMat_df$height,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                                 breaks = function(lims){
                                                   dat <- quantile(surfaceMat_df$height,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                                   
                                                   dat <- dat %>%
                                                     setNames(paste0(names(dat)," [",round(dat,1),"]"))
                                                   
                                                   return(dat)
                                                 },
                                                 na.value = "grey80") +
                   facet_grid(rows = vars(rowIndex),
                              cols = vars(colIndex)) +
                   ggplot2::coord_fixed(expand = FALSE) +
                   ggplot2::theme_minimal() +
                   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                  axis.text.x = ggplot2::element_blank(),
                                  axis.ticks.x = ggplot2::element_blank(),
                                  axis.title.y = ggplot2::element_blank(),
                                  axis.text.y = ggplot2::element_blank(),
                                  axis.ticks.y = ggplot2::element_blank(),
                                  panel.grid.major = ggplot2::element_blank(),
                                  panel.grid.minor = ggplot2::element_blank(),
                                  panel.background = ggplot2::element_blank(),
                                  legend.position = "bottom") +
                   ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = grid::unit(2.5,"in"),
                                                                   reverse = TRUE,
                                                                   label.theme = ggplot2::element_text(size = 8,angle = 90),
                                                                   title.theme = ggplot2::element_text(size = 10),
                                                                   frame.colour = "black",
                                                                   ticks.colour = "black"),
                                   alpha =  'none') + 
                   ggplot2::labs(fill = expression("Rel. Height ["*mu*"m]")) +
                   ggtitle("Cell")
                 
                 plt
                 
               })
               
               mostSimilarRegions <- comparisonData %>%
                 arrange(desc(pairwiseCompCor)) %>%
                 slice(1:(min(nrow(.),3))) %>%
                 purrr::pmap(~ {
                   
                   translatedCenter <- dim(..8$surface.matrix)/2 - c(..3,-1*..4)
                   # translatedCenter <- dim(..7$surface.matrix)/2 - c(..10,..9)/2
                   
                   minRowCol <- (translatedCenter - dim(..7$surface.matrix)/2)*..8$header.info$incrementX
                   maxRowCol <- (translatedCenter + dim(..7$surface.matrix)/2)*..8$header.info$incrementX
                   
                   region_df <- x3p_to_df(..8) %>%
                     mutate(x = max(x) - x,
                            y = max(y) - y)
                   
                   geom_rect_df <- data.frame(xmin = minRowCol[2],
                                              xmax = maxRowCol[2],
                                              ymin = minRowCol[1],
                                              ymax = maxRowCol[1])
                   
                   rotation <- ..11
                   vertShift <- round(-1*..3)
                   horizShift <- round(..4)
                   correlation <- round(..6,3)
                   
                   plt1 <- region_df %>%
                     ggplot2::ggplot(ggplot2::aes()) +
                     ggplot2::geom_raster(ggplot2::aes(x = x,y = y,fill = value))  +
                     ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                                   values = scales::rescale(quantile(region_df %>%
                                                                                       pull(value),
                                                                                     c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),
                                                                                     na.rm = TRUE)),
                                                   limits = quantile(region_df %>%
                                                                       pull(value),
                                                                     c(0,1),
                                                                     na.rm = TRUE),
                                                   na.value = "grey80") +
                     ggplot2::coord_fixed(expand = FALSE) +
                     ggplot2::theme_minimal() +
                     ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                    axis.text.x = ggplot2::element_blank(),
                                    axis.ticks.x = ggplot2::element_blank(),
                                    axis.title.y = ggplot2::element_blank(),
                                    axis.text.y = ggplot2::element_blank(),
                                    axis.ticks.y = ggplot2::element_blank(),
                                    panel.grid.major = ggplot2::element_blank(),
                                    panel.grid.minor = ggplot2::element_blank(),
                                    panel.background = ggplot2::element_blank(),
                                    legend.position = "none") +
                     geom_rect(data = geom_rect_df,
                               aes(xmin = xmin,
                                   xmax = xmax,
                                   ymin = ymin,
                                   ymax = ymax),
                               alpha = 0,
                               colour = "black") +
                     ggplot2::labs(fill = expression("Rel. Height ["*mu*"m]")) +
                     ggtitle(paste0("Rotation = ",rotation,", Pairwise-complete Corr. = ",correlation,", \nVert. Shift = ",vertShift,",Horiz. Shift = ",horizShift))
                   
                   return(plt1)
                 })
               
               # 
               
               output$regionPlots <- renderPlot({
                 
                 # mostSimilarRegions[[1]]
                 
                 plt <- mostSimilarRegions[[1]]
                 
                 if(length(mostSimilarRegions) == 1){
                   return(plt)
                 }
                 else{
                   for(ind in 2:length(mostSimilarRegions)){
                     plt <- plt / mostSimilarRegions[[ind]]
                   }
                   
                   return(plt)
                 }
                 
               })
               
               ccfMaps <- comparisonData %>%
                 arrange(desc(pairwiseCompCor)) %>%
                 slice(1:(min(nrow(.),3))) %>%
                 purrr::pmap(~ {
                   
                   ccfMat <- cmcR:::filterViaFFT(..9$surface.matrix,..10$surface.matrix)/(sqrt(sum(..9$surface.matrix^2)) * sqrt(sum(..10$surface.matrix^2)))
                   
                   ccfMax <- which(ccfMat == max(ccfMat,na.rm = TRUE),arr.ind = TRUE)
                   
                   ccfMat_x3p <- ..9
                   ccfMat_x3p$surface.matrix <- ccfMat
                   ccfMat_x3p$header.info$sizeY <- ncol(ccfMat)
                   ccfMat_x3p$header.info$sizeX <- nrow(ccfMat)
                   
                   ccfMat_df <- ccfMat_x3p %>%
                     x3p_to_df() %>%
                     mutate(x = x - max(x)/2,
                            y = y - max(y)/2)
                   
                   plt1 <- ccfMat_df %>%
                     ggplot() +
                     geom_tile(aes(x = x,y = y,fill = value)) +
                     geom_point(data = ccfMat_df %>% 
                                  top_n(n = 1,
                                        wt = value),aes(x = x,y = y),
                                colour = "red") +
                     # coord_flip() +
                     coord_fixed(expand = FALSE) +
                     ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                                   values = scales::rescale(quantile(ccfMat_df$value,c(0,.25,.5,.75,1),na.rm = TRUE)),
                                                   breaks = function(lims){
                                                     dat <- quantile(ccfMat_df$value,c(0,.25,.5,.75,1),na.rm = TRUE)
                                                     
                                                     dat <- dat %>%
                                                       setNames(round(dat,3))
                                                     
                                                     return(dat)
                                                   },
                                                   na.value = "grey50") +
                     theme(legend.position = "bottom") +
                     ggplot2::guides(fill = ggplot2::guide_colourbar(
                       reverse = TRUE,
                       barwidth = grid::unit(3,"in"),
                       title = "FFT-Based\nCCF",
                       label.theme = ggplot2::element_text(size = 8,angle = 90),
                       title.theme = ggplot2::element_text(size = 10),
                       frame.colour = "black",
                       ticks.colour = "black"),
                       colour =  'none')
                   
                   return(plt1)
                 })
               
               
               output$comparisonDiagnosticPlots <- renderPlot({
                 
                 plt <- ccfMaps[[1]]
                 
                 if(length(ccfMaps) == 1){
                   return(plt)
                 }
                 else{
                   for(ind in 2:length(ccfMaps)){
                     plt <- plt / ccfMaps[[ind]]
                   }
                   
                   return(plt)
                 }
                 
               })
               
               output$postComparisonTarget <- renderPlot({
                 
                 target <- shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$targetSelect)]]
                 
                 plt <- cmcR::x3pListPlot(list(target) %>%
                                            magrittr::set_names(input$targetSelect),
                                          type = "list")
                 
                 comparisonData <- comparisonData %>%
                   # mutate(colIndex = as.integer(colIndex - numCellsRoot)) %>%
                   filter(rowIndex == input$reference_click$panelvar2 & colIndex == input$reference_click$panelvar1)
                 
                 regionLocations <- comparisonData$regionHeightValues[1] %>%
                   names() %>%
                   stringr::str_extract_all("[0-9]{1,}") %>%
                   unlist()
                 
                 regionCenter <- c(mean(c(as.numeric(regionLocations[[3]]),as.numeric(regionLocations[[4]]))),
                                   mean(c(as.numeric(regionLocations[[1]]),as.numeric(regionLocations[[2]]))))
                 
                 newCenter_dx <- cos(pi - sqrt(sum((target$header.info$incrementY*1e6)*regionCenter^2)) * sin(abs(comparisonData$theta)*pi/180)/sin(((180 - abs(comparisonData$theta))/2)*pi/180) - atan(abs(regionCenter[1]/regionCenter[2])))*
                   sqrt(sum((target$header.info$incrementY*1e6)*regionCenter^2))*sin(abs(comparisonData$theta)*pi/180)/sin(((180 - comparisonData$theta)/2)*pi/180)
                 
                 newCenter_dy <- sin(pi - sqrt(sum((target$header.info$incrementY*1e6)*regionCenter^2)) * sin(abs(comparisonData$theta)*pi/180)/sin(((180 - abs(comparisonData$theta))/2)*pi/180) - atan(abs(regionCenter[1]/regionCenter[2])))*
                   sqrt(sum((target$header.info$incrementY*1e6)*regionCenter^2))*sin(abs(comparisonData$theta)*pi/180)/sin(((180 - abs(comparisonData$theta))/2)*pi/180)
                 
                 comparisonData <- comparisonData %>% 
                   dplyr::mutate(firstRow = regionCenter[1] - nrow(comparisonData$regionHeightValues[[1]]$surface.matrix)/2,
                                 lastRow = regionCenter[1] + nrow(comparisonData$regionHeightValues[[1]]$surface.matrix)/2,
                                 firstCol = ncol(target$surface.matrix) - regionCenter[2]- ncol(comparisonData$regionHeightValues[[1]]$surface.matrix)/2,
                                 lastCol = ncol(target$surface.matrix) - regionCenter[2] + ncol(comparisonData$regionHeightValues[[1]]$surface.matrix)/2) %>%
                   dplyr::mutate(firstRow = (target$header.info$incrementY*1e6)*(firstRow),
                                 lastRow = (target$header.info$incrementY*1e6)*(lastRow),
                                 firstCol = (target$header.info$incrementY*1e6)*(firstCol),
                                 lastCol = (target$header.info$incrementY*1e6)*(lastCol)) %>%
                   dplyr::mutate(firstRowCentered = firstRow - mean(c(firstRow,lastRow)),
                                 lastRowCentered = lastRow - mean(c(firstRow,lastRow)),
                                 firstColCentered = firstCol - mean(c(firstCol,lastCol)),
                                 lastColCentered = lastCol - mean(c(firstCol,lastCol))) %>%
                   dplyr::mutate(topLeftCorner_col = firstColCentered*cos((theta)*(pi/180)) - lastRowCentered*sin((theta)*(pi/180)) + mean(c(firstCol,lastCol)) - newCenter_dy,
                                 topLeftCorner_row = firstColCentered*sin((theta)*(pi/180)) + lastRowCentered*cos((theta)*(pi/180)) + mean(c(firstRow,lastRow)) + newCenter_dx,
                                 topRightCorner_col = lastColCentered*cos((theta)*(pi/180)) - lastRowCentered*sin((theta)*(pi/180)) + mean(c(firstCol,lastCol)) - newCenter_dy,
                                 topRightCorner_row = lastColCentered*sin((theta)*(pi/180)) + lastRowCentered*cos((theta)*(pi/180)) + mean(c(firstRow,lastRow)) + newCenter_dx,
                                 bottomRightCorner_col = lastColCentered*cos((theta)*(pi/180)) - firstRowCentered*sin((theta)*(pi/180)) + mean(c(firstCol,lastCol)) - newCenter_dy,
                                 bottomRightCorner_row = lastColCentered*sin((theta)*(pi/180)) + firstRowCentered*cos((theta)*(pi/180)) + mean(c(firstRow,lastRow)) + newCenter_dx,
                                 bottomLeftCorner_col = firstColCentered*cos((theta)*(pi/180)) - firstRowCentered*sin((theta)*(pi/180)) + mean(c(firstCol,lastCol)) - newCenter_dy,
                                 bottomLeftCorner_row = firstColCentered*sin((theta)*(pi/180)) + firstRowCentered*cos((theta)*(pi/180)) + mean(c(firstRow,lastRow)) + newCenter_dx)  %>%
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
                   mutate(cellIndex = paste0(rowIndex,",",colIndex))
                 
                 
                 plt[[1]] +
                   geom_polygon(data = comparisonData,
                                mapping = aes(x = x,
                                              y = y,
                                              group = cellIndex),
                                colour = "black",
                                fill = NA) +
                   theme_replace(axis.title.x = ggplot2::element_blank(),
                                 axis.text.x = ggplot2::element_blank(),
                                 axis.ticks.x = ggplot2::element_blank(),
                                 axis.title.y = ggplot2::element_blank(),
                                 axis.text.y = ggplot2::element_blank(),
                                 axis.ticks.y = ggplot2::element_blank(),
                                 panel.grid.major = ggplot2::element_blank(),
                                 panel.grid.minor = ggplot2::element_blank(),
                                 panel.background = ggplot2::element_blank())
                 
                 
                 
               })
               
               remove_modal_spinner()
               
             })

observeEvent({input$target_click},
             {
               
               req(shiny.r$data)
               req(shiny.r$targetComparison)
               req(input$target_click$panelvar1)
               req(input$target_click$panelvar2)
               
               show_modal_spinner(spin = "atom", text = "Pulling data for cell comparison...")
               
               req(input$bothDirectionsCheck)
               
               comparisonData <- shiny.r$comparisonData %>%
                 filter(direction == "targetToRef") %>%
                 tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ")
               
               comparisonData <- comparisonData %>%
                 # mutate(colIndex = as.integer(colIndex - numCellsRoot)) %>%
                 filter(rowIndex == input$target_click$panelvar2 & colIndex == input$target_click$panelvar1)
               
               surfaceMat_df <- comparisonData %>%
                 purrr::pmap_dfr(~ {
                   
                   x3p_to_df(..7) %>%
                     dplyr::mutate(value = value - median(value,na.rm = TRUE)) %>%
                     dplyr::mutate(#x = x*1e6,
                       # y = y*1e6,
                       height = value) %>%
                     mutate(rowIndex = ..1,
                            colIndex = ..2)
                   
                 }) %>%
                 mutate(x = max(x) - x,
                        y = max(y) - y) %>%
                 mutate(rowIndex = as.numeric(rowIndex),
                        colIndex = as.numeric(colIndex))
               
               output$cellPlot <-  renderPlot({
                 
                 plt <- surfaceMat_df %>%
                   ggplot(aes(x = x,y = y)) +
                   geom_raster(aes(fill = height)) +
                   ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6',
                                                                 '#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                                 values = scales::rescale(quantile(surfaceMat_df$height,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                                 breaks = function(lims){
                                                   dat <- quantile(surfaceMat_df$height,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                                   
                                                   dat <- dat %>%
                                                     setNames(paste0(names(dat)," [",round(dat,1),"]"))
                                                   
                                                   return(dat)
                                                 },
                                                 na.value = "grey80") +
                   facet_grid(cols = vars(rowIndex),
                              rows = vars(colIndex)) +
                   ggplot2::coord_fixed(expand = FALSE) +
                   ggplot2::theme_minimal() +
                   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                  axis.text.x = ggplot2::element_blank(),
                                  axis.ticks.x = ggplot2::element_blank(),
                                  axis.title.y = ggplot2::element_blank(),
                                  axis.text.y = ggplot2::element_blank(),
                                  axis.ticks.y = ggplot2::element_blank(),
                                  panel.grid.major = ggplot2::element_blank(),
                                  panel.grid.minor = ggplot2::element_blank(),
                                  panel.background = ggplot2::element_blank(),
                                  legend.position = "bottom") +
                   ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = grid::unit(2.5,"in"),
                                                                   label.theme = ggplot2::element_text(size = 8,angle = 90),
                                                                   title.theme = ggplot2::element_text(size = 10),
                                                                   frame.colour = "black",
                                                                   ticks.colour = "black"),
                                   alpha =  'none') + 
                   ggplot2::labs(fill = expression("Rel. Height ["*mu*"m]")) +
                   ggtitle("Cell")
                 
                 plt
                 
               })
               
               mostSimilarRegions <- comparisonData %>%
                 arrange(desc(pairwiseCompCor)) %>%
                 slice(1:(min(nrow(.),3))) %>%
                 purrr::pmap(~ {
                   
                   translatedCenter <- dim(..8$surface.matrix)/2 - c(..3,-1*..4)
                   
                   minRowCol <- (translatedCenter - dim(..7$surface.matrix)/2)*..8$header.info$incrementX
                   maxRowCol <- (translatedCenter + dim(..7$surface.matrix)/2)*..8$header.info$incrementX
                   
                   region_df <- x3p_to_df(..8) %>%
                     mutate(x = max(x) - x,
                            y = max(y) - y)
                   
                   geom_rect_df <- data.frame(xmin = minRowCol[2],
                                              xmax = maxRowCol[2],
                                              ymin = minRowCol[1],
                                              ymax = maxRowCol[1])
                   
                   rotation <- ..11
                   vertShift <- round(-1*..3)
                   horizShift <- round(..4)
                   correlation <- round(..6,3)
                   
                   plt1 <- region_df %>%
                     ggplot2::ggplot(ggplot2::aes()) +
                     ggplot2::geom_raster(ggplot2::aes(x = x,y = y,fill = value))  +
                     ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                                   values = scales::rescale(quantile(region_df %>%
                                                                                       pull(value),
                                                                                     c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),
                                                                                     na.rm = TRUE)),
                                                   limits = quantile(region_df %>%
                                                                       pull(value),
                                                                     c(0,1),
                                                                     na.rm = TRUE),
                                                   na.value = "grey80") +
                     ggplot2::coord_fixed(expand = FALSE) +
                     ggplot2::theme_minimal() +
                     ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                    axis.text.x = ggplot2::element_blank(),
                                    axis.ticks.x = ggplot2::element_blank(),
                                    axis.title.y = ggplot2::element_blank(),
                                    axis.text.y = ggplot2::element_blank(),
                                    axis.ticks.y = ggplot2::element_blank(),
                                    panel.grid.major = ggplot2::element_blank(),
                                    panel.grid.minor = ggplot2::element_blank(),
                                    panel.background = ggplot2::element_blank(),
                                    legend.position = "none") +
                     geom_rect(data = geom_rect_df,
                               aes(xmin = xmin,
                                   xmax = xmax,
                                   ymin = ymin,
                                   ymax = ymax),
                               alpha = 0,
                               colour = "black") +
                     ggplot2::labs(fill = expression("Rel. Height ["*mu*"m]")) +
                     ggtitle(paste0("Rotation = ",rotation,", Pairwise-complete Corr. = ",correlation,",\nVert. Shift = ",vertShift,", Horiz. Shift = ",horizShift))
                   
                   return(plt1)
                 })
               
               
               output$regionPlots <- renderPlot({
                 
                 plt <- mostSimilarRegions[[1]]
                 
                 if(length(mostSimilarRegions) == 1){
                   return(plt)
                 }
                 else{
                   for(ind in 2:length(mostSimilarRegions)){
                     plt <- plt + mostSimilarRegions[[ind]]
                   }
                   
                   return(plt)
                 }
                 
               })
               
               ccfMaps <- comparisonData %>%
                 arrange(desc(pairwiseCompCor)) %>%
                 slice(1:(min(nrow(.),3))) %>%
                 purrr::pmap(~ {
                   
                   ccfMat <- cmcR:::filterViaFFT(..9$surface.matrix,..10$surface.matrix)/(sqrt(sum(..9$surface.matrix^2)) * sqrt(sum(..10$surface.matrix^2)))
                   
                   ccfMax <- which(ccfMat == max(ccfMat,na.rm = TRUE),arr.ind = TRUE)
                   
                   ccfMat_x3p <- ..9
                   ccfMat_x3p$surface.matrix <- ccfMat
                   ccfMat_x3p$header.info$sizeY <- ncol(ccfMat)
                   ccfMat_x3p$header.info$sizeX <- nrow(ccfMat)
                   
                   ccfMat_df <- ccfMat_x3p %>%
                     x3p_to_df() %>%
                     mutate(x = x - max(x)/2,
                            y = y - max(y)/2)
                   
                   plt1 <- ccfMat_df %>%
                     ggplot() +
                     geom_tile(aes(x = x,y = y,fill = value)) +
                     geom_point(data = ccfMat_df %>% 
                                  top_n(n = 1,
                                        wt = value),aes(x = x,y = y),
                                colour = "red") +
                     # coord_flip() +
                     coord_fixed(expand = FALSE) +
                     ggplot2::scale_fill_gradientn(colours = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
                                                   values = scales::rescale(quantile(ccfMat_df$value,c(0,.25,.5,.75,1),na.rm = TRUE)),
                                                   breaks = function(lims){
                                                     dat <- quantile(ccfMat_df$value,c(0,.25,.5,.75,1),na.rm = TRUE)
                                                     
                                                     dat <- dat %>%
                                                       setNames(round(dat,3))
                                                     
                                                     return(dat)
                                                   },
                                                   na.value = "grey50") +
                     theme(legend.position = "bottom") +
                     ggplot2::guides(fill = ggplot2::guide_colourbar(
                       barwidth = grid::unit(3,"in"),
                       title = "FFT-Based\nCCF",
                       label.theme = ggplot2::element_text(size = 8,angle = 90),
                       title.theme = ggplot2::element_text(size = 10),
                       frame.colour = "black",
                       ticks.colour = "black"),
                       colour =  'none')
                   
                   return(plt1)
                 })
               
               
               output$comparisonDiagnosticPlots <- renderPlot({
                 
                 plt <- ccfMaps[[1]]
                 
                 if(length(ccfMaps) == 1){
                   return(plt)
                 }
                 else{
                   for(ind in 2:length(ccfMaps)){
                     plt <- plt + ccfMaps[[ind]]
                   }
                   
                   return(plt)
                 }
                 
               })
               
               remove_modal_spinner()
               
             })



observeEvent(input$comparisonButton,
             {
               
               comparisonData <- shiny.r$comparisonData
               
               reference <- shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$referenceSelect)]]
               target <- shiny.r$data$x3p_processed[[which(shiny.r$data$x3pNames == input$targetSelect)]]
               
               if(input$preRotateScans){
                 
                 target <- preProcess_rotateScan(reference = reference,
                                                 target = target)
                 
               }
               
               comparisonData <- comparisonData %>%
                 group_by(direction) %>%
                 mutate(originalMethodClassif = cmcR::decision_CMC(cellIndex = cellIndex,
                                                                   x = x,y = y,
                                                                   theta = theta,
                                                                   corr = pairwiseCompCor,
                                                                   xThresh = input$translationThreshold,
                                                                   thetaThresh = input$rotationThreshold,
                                                                   corrThresh = input$corrThreshold),
                        highCMCClassif = cmcR::decision_CMC(cellIndex = cellIndex,
                                                            x = x,y = y,
                                                            theta = theta,
                                                            corr = pairwiseCompCor,
                                                            xThresh = input$translationThreshold,
                                                            thetaThresh = input$rotationThreshold,
                                                            corrThresh = input$corrThreshold,
                                                            tau = input$highCMCThreshold)) %>%
                 ungroup()
               
               if(!input$bothDirectionsCheck){
                 
                 
                 plt <- cmcPlot_shiny(reference = reference,
                                      target = target,
                                      reference_v_target_CMCs = comparisonData %>%
                                        filter(direction == "refToTarget"),
                                      x3pNames = c(input$referenceSelect,input$targetSelect))
                 
                 output$originalMethodCMCPlot <- renderPlot({
                   plt[[1]]
                 })
                 
                 output$highCMCPlot <- renderPlot({
                   
                   plt[[2]]
                   
                 })
                 
                 output$originalMethodCMC_refToTarget_diagnosticPlots <- renderPlot({
                   
                   
                   originalMethodData <- comparisonData %>%
                     group_by(cellIndex) %>%
                     filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
                     ungroup() %>%
                     mutate(med_x = median(x),
                            med_y = median(y),
                            med_theta  = median(theta)) %>%
                     mutate(xThresh_minus = med_x - input$translationThreshold,
                            yThresh_minus = med_y - input$translationThreshold,
                            xThresh_plus = med_x + input$translationThreshold,
                            xThresh_plus = med_y + input$translationThreshold,
                            thetaThresh_minus = med_theta - input$rotationThreshold,
                            thetaThresh_plus = med_theta + input$rotationThreshold,
                            corThresh = input$corrThreshold) %>%
                     mutate(xClassif = factor(ifelse(abs(x - med_x) <= input$translationThreshold,"Congruent","Not Congruent")),
                            yClassif = factor(ifelse(abs(y - med_y) <= input$translationThreshold,"Congruent","Not Congruent")),
                            thetaClassif = factor(ifelse(abs(theta - med_theta) <= input$rotationThreshold,"Congruent","Not Congruent")),
                            corClassif = factor(ifelse(pairwiseCompCor >= input$corrThreshold,"Congruent","Not Congruent")))
                   
                   xPlot <- originalMethodData %>%
                     ggplot() +
                     geom_histogram(aes(x=x,fill = xClassif),
                                    alpha = .7,
                                    binwidth = 1) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_ribbon(mapping = aes(y = seq(0,originalMethodData %>%
                                                         group_by(x) %>%
                                                         tally() %>%
                                                         pull(n) %>%
                                                         max() %>%
                                                         unique(),
                                                       length.out = nrow(originalMethodData)),
                                               xmin = med_x - input$translationThreshold - .5,
                                               xmax = med_x + input$translationThreshold + .5),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"x")), 
                                        breaks = seq(-max(abs(comparisonData$x)),max(abs(comparisonData$x)),by = 50),
                                        limits = c(-max(abs(comparisonData$x)) - 1,max(abs(comparisonData$x)) + 1),
                                        oob = scales::oob_keep
                     ) +
                     scale_y_continuous(limits = c(0,originalMethodData %>%
                                                     group_by(x) %>%
                                                     tally() %>%
                                                     pull(n) %>%
                                                     max() %>%
                                                     unique())) +
                     geom_vline(xintercept = unique(originalMethodData$med_x),
                                colour = "#7570b3") +
                     ylab("# Cell Pairs")+
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   yPlot <- originalMethodData %>%
                     ggplot() +
                     geom_histogram(aes(x=y,fill = yClassif),
                                    alpha = .7,
                                    binwidth = 1) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_ribbon(mapping = aes(y = seq(0,originalMethodData %>%
                                                         group_by(x) %>%
                                                         tally() %>%
                                                         pull(n) %>%
                                                         max() %>%
                                                         unique(),
                                                       length.out = nrow(originalMethodData)),
                                               xmin = med_y - input$translationThreshold - .5,
                                               xmax = med_y + input$translationThreshold + .5),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"y")), 
                                        breaks = seq(-max(abs(comparisonData$y)),max(abs(comparisonData$y)),by = 50),
                                        limits = c(-max(abs(comparisonData$y)) - 1,max(abs(comparisonData$y)) + 1),
                                        oob = scales::oob_keep
                     ) +
                     scale_y_continuous(limits = c(0,originalMethodData %>%
                                                     group_by(y) %>%
                                                     tally() %>%
                                                     pull(n) %>%
                                                     max() %>%
                                                     unique())) +
                     geom_vline(xintercept = unique(originalMethodData$med_y),
                                colour = "#7570b3") +
                     ylab("# Cell Pairs")+
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   thetaPlot <- originalMethodData %>%
                     ggplot() +
                     geom_bar(aes(x = theta, 
                                  fill = thetaClassif),
                              alpha = .7) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_ribbon(aes(y = seq(0,originalMethodData %>%
                                               group_by(theta) %>%
                                               tally() %>%
                                               pull(n) %>%
                                               max(),
                                             length.out = nrow(originalMethodData)),
                                     xmin = med_theta - input$rotationThreshold - 1.5,
                                     xmax = med_theta + input$rotationThreshold + 1.5),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated rotation angle ", theta)), 
                                        breaks = seq(-max(c(abs(comparisonData$theta),30)),max(c(abs(comparisonData$theta),30)),by = 15),
                                        limits = c(-max(c(abs(comparisonData$theta),30)),max(c(abs(comparisonData$theta),30))) + c(-2,2)) +
                     # scale_y_continuous(limits = c(0,7),
                     #                    breaks = seq(0,7,by = 1)) +
                     ylab("# Cell Pairs") +
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), 
                           axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   corrPlot <- originalMethodData %>%
                     ggplot(aes(x = pairwiseCompCor,fill = corClassif)) +
                     geom_histogram(binwidth = .01,
                                    alpha = .7) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_vline(aes(xintercept = input$corrThreshold),
                                colour = "#7570b3") +
                     geom_ribbon(mapping = aes(y = seq(-1,
                                                       originalMethodData %>%
                                                         mutate(bin = (trunc(pairwiseCompCor*100))/100) %>%
                                                         group_by(bin) %>%
                                                         tally() %>%
                                                         pull(n) %>%
                                                         max() %>%
                                                         unique(),
                                                       length.out = nrow(originalMethodData)),
                                               xmin = input$corrThreshold,
                                               xmax = 1),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated CCF"[max])), 
                                        breaks = seq(0,1,by = .5)) +
                     coord_cartesian(xlim = c(0,1),
                                     ylim = c(0,NA),
                                     expand = FALSE) +
                     ylab("# Cell Pairs") +
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   return((xPlot / yPlot / thetaPlot / corrPlot))
                   
                 })
                 
                 output$highCMC_refToTarget_diagnosticPlot <- renderPlot({
                   
                   comparisonData %>%
                     mutate(cmcThetaDistribClassif = decision_highCMC_cmcThetaDistrib(cellIndex = cellIndex,
                                                                                      x = x,
                                                                                      y = y,
                                                                                      theta = theta,
                                                                                      corr = pairwiseCompCor,
                                                                                      xThresh = input$translationThreshold,
                                                                                      yThresh = input$translationThreshold,
                                                                                      corrThresh = input$corrThreshold)) %>%
                     decision_highCMC_identifyHighCMCThetas(tau = input$highCMCThreshold) %>%
                     filter(cmcThetaDistribClassif == "CMC Candidate") %>%
                     ggplot() +
                     geom_bar(aes(x = theta, fill = thetaCMCIdentif),
                              stat = "count",
                              alpha = .7) +
                     geom_hline(aes(yintercept = max(cmcCandidateCount) - input$highCMCThreshold),
                                linetype = "dashed") +
                     scale_fill_manual(values = c("black","gray50")) +
                     theme_bw() +
                     ylab("CMC Candidate Count") +
                     xlab(expression(theta)) +
                     xlim(c(min(comparisonData$theta),max(comparisonData$theta)))
                   
                 })
               }
               else{
                 
                 
                 plt <- cmcPlot_shiny(reference = reference,
                                      target = target,
                                      reference_v_target_CMCs = comparisonData %>%
                                        filter(direction == "refToTarget"),
                                      target_v_reference_CMCs = comparisonData %>%
                                        filter(direction == "targetToRef"),
                                      x3pNames = c(input$referenceSelect,input$targetSelect))
                 
                 output$originalMethodCMCPlot <- renderPlot({
                   plt[[1]]
                 })
                 
                 output$originalMethodCMCPlot_targetToRef <- renderPlot({
                   plt[[2]]
                 })
                 
                 output$highCMCPlot <- renderPlot({
                   plt[[3]]
                 })
                 
                 output$highCMCPlot_targetToRef <- renderPlot({
                   plt[[4]]
                 })
                 
                 output$originalMethodCMC_refToTarget_diagnosticPlots <- renderPlot({
                   
                   
                   originalMethodData_refToTarget <- comparisonData %>%
                     filter(direction == "refToTarget") %>%
                     group_by(cellIndex) %>%
                     filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
                     ungroup() %>%
                     mutate(med_x = median(x),
                            med_y = median(y),
                            med_theta  = median(theta)) %>%
                     mutate(xThresh_minus = med_x - input$translationThreshold,
                            yThresh_minus = med_y - input$translationThreshold,
                            xThresh_plus = med_x + input$translationThreshold,
                            xThresh_plus = med_y + input$translationThreshold,
                            thetaThresh_minus = med_theta - input$rotationThreshold,
                            thetaThresh_plus = med_theta + input$rotationThreshold,
                            corThresh = input$corrThreshold) %>%
                     mutate(xClassif = factor(ifelse(abs(x - med_x) <= input$translationThreshold,"Congruent","Not Congruent")),
                            yClassif = factor(ifelse(abs(y - med_y) <= input$translationThreshold,"Congruent","Not Congruent")),
                            thetaClassif = factor(ifelse(abs(theta - med_theta) <= input$rotationThreshold,"Congruent","Not Congruent")),
                            corClassif = factor(ifelse(pairwiseCompCor >= input$corrThreshold,"Congruent","Not Congruent")))
                   
                   xPlot <- originalMethodData_refToTarget %>%
                     ggplot() +
                     geom_histogram(aes(x=x,fill = xClassif),
                                    alpha = .7,
                                    binwidth = 1) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_ribbon(mapping = aes(y = seq(0,originalMethodData_refToTarget %>%
                                                         group_by(x) %>%
                                                         tally() %>%
                                                         pull(n) %>%
                                                         max() %>%
                                                         unique(),
                                                       length.out = nrow(originalMethodData_refToTarget)),
                                               xmin = med_x - input$translationThreshold - .5,
                                               xmax = med_x + input$translationThreshold + .5),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"x")), 
                                        breaks = seq(-max(abs(comparisonData$x)),max(abs(comparisonData$x)),by = 50),
                                        limits = c(-max(abs(comparisonData$x)) - 1,max(abs(comparisonData$x)) + 1),
                                        oob = scales::oob_keep
                     ) +
                     scale_y_continuous(limits = c(0,originalMethodData_refToTarget %>%
                                                     group_by(x) %>%
                                                     tally() %>%
                                                     pull(n) %>%
                                                     max() %>%
                                                     unique())) +
                     geom_vline(xintercept = unique(originalMethodData_refToTarget$med_x),
                                colour = "#7570b3") +
                     ylab("# Cell Pairs")+
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   yPlot <- originalMethodData_refToTarget %>%
                     ggplot() +
                     geom_histogram(aes(x=y,fill = yClassif),
                                    alpha = .7,
                                    binwidth = 1) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_ribbon(mapping = aes(y = seq(0,originalMethodData_refToTarget %>%
                                                         group_by(x) %>%
                                                         tally() %>%
                                                         pull(n) %>%
                                                         max() %>%
                                                         unique(),
                                                       length.out = nrow(originalMethodData_refToTarget)),
                                               xmin = med_y - input$translationThreshold - .5,
                                               xmax = med_y + input$translationThreshold + .5),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"y")), 
                                        breaks = seq(-max(abs(comparisonData$y)),max(abs(comparisonData$y)),by = 50),
                                        limits = c(-max(abs(comparisonData$y)) - 1,max(abs(comparisonData$y)) + 1),
                                        oob = scales::oob_keep
                     ) +
                     scale_y_continuous(limits = c(0,originalMethodData_refToTarget %>%
                                                     group_by(y) %>%
                                                     tally() %>%
                                                     pull(n) %>%
                                                     max() %>%
                                                     unique())) +
                     geom_vline(xintercept = unique(originalMethodData_refToTarget$med_y),
                                colour = "#7570b3") +
                     ylab("# Cell Pairs")+
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   thetaPlot <- originalMethodData_refToTarget %>%
                     ggplot() +
                     geom_bar(aes(x = theta, 
                                  fill = thetaClassif),
                              alpha = .7) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_ribbon(aes(y = seq(0,originalMethodData_refToTarget %>%
                                               group_by(theta) %>%
                                               tally() %>%
                                               pull(n) %>%
                                               max(),
                                             length.out = nrow(originalMethodData_refToTarget)),
                                     xmin = med_theta - input$rotationThreshold - 1.5,
                                     xmax = med_theta + input$rotationThreshold + 1.5),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated rotation angle ", theta)), 
                                        breaks = seq(-max(c(abs(comparisonData$theta),30)),max(c(abs(comparisonData$theta),30)),by = 15),
                                        limits = c(-max(c(abs(comparisonData$theta),30)),max(c(abs(comparisonData$theta),30))) + c(-2,2)) +
                     # scale_y_continuous(limits = c(0,7),
                     #                    breaks = seq(0,7,by = 1)) +
                     ylab("# Cell Pairs") +
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), 
                           axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   corrPlot <- originalMethodData_refToTarget %>%
                     ggplot(aes(x = pairwiseCompCor,fill = corClassif)) +
                     geom_histogram(binwidth = .01,
                                    alpha = .7) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_vline(aes(xintercept = input$corrThreshold),
                                colour = "#7570b3") +
                     geom_ribbon(mapping = aes(y = seq(-1,
                                                       originalMethodData_refToTarget %>%
                                                         mutate(bin = (trunc(pairwiseCompCor*100))/100) %>%
                                                         group_by(bin) %>%
                                                         tally() %>%
                                                         pull(n) %>%
                                                         max() %>%
                                                         unique(),
                                                       length.out = nrow(originalMethodData_refToTarget)),
                                               xmin = input$corrThreshold,
                                               xmax = 1),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated CCF"[max])), 
                                        breaks = seq(0,1,by = .5)) +
                     coord_cartesian(xlim = c(0,1),
                                     ylim = c(0,NA),
                                     expand = FALSE) +
                     ylab("# Cell Pairs") +
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   return((xPlot / yPlot / thetaPlot / corrPlot))
                   
                 })
                 
                 output$originalMethodCMC_targetToRef_diagnosticPlots <- renderPlot({
                   
                   
                   originalMethodData_targetToRef <- comparisonData %>%
                     filter(direction == "targetToRef") %>%
                     group_by(cellIndex) %>%
                     filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
                     ungroup() %>%
                     mutate(med_x = median(x),
                            med_y = median(y),
                            med_theta  = median(theta)) %>%
                     mutate(xThresh_minus = med_x - input$translationThreshold,
                            yThresh_minus = med_y - input$translationThreshold,
                            xThresh_plus = med_x + input$translationThreshold,
                            xThresh_plus = med_y + input$translationThreshold,
                            thetaThresh_minus = med_theta - input$rotationThreshold,
                            thetaThresh_plus = med_theta + input$rotationThreshold,
                            corThresh = input$corrThreshold) %>%
                     mutate(xClassif = factor(ifelse(abs(x - med_x) <= input$translationThreshold,"Congruent","Not Congruent")),
                            yClassif = factor(ifelse(abs(y - med_y) <= input$translationThreshold,"Congruent","Not Congruent")),
                            thetaClassif = factor(ifelse(abs(theta - med_theta) <= input$rotationThreshold,"Congruent","Not Congruent")),
                            corClassif = factor(ifelse(pairwiseCompCor >= input$corrThreshold,"Congruent","Not Congruent")))
                   
                   xPlot <- originalMethodData_targetToRef %>%
                     ggplot() +
                     geom_histogram(aes(x=x,fill = xClassif),
                                    alpha = .7,
                                    binwidth = 1) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_ribbon(mapping = aes(y = seq(0,originalMethodData_targetToRef %>%
                                                         group_by(x) %>%
                                                         tally() %>%
                                                         pull(n) %>%
                                                         max() %>%
                                                         unique(),
                                                       length.out = nrow(originalMethodData_targetToRef)),
                                               xmin = med_x - input$translationThreshold - .5,
                                               xmax = med_x + input$translationThreshold + .5),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"x")), 
                                        breaks = seq(-max(abs(comparisonData$x)),max(abs(comparisonData$x)),by = 50),
                                        limits = c(-max(abs(comparisonData$x)) - 1,max(abs(comparisonData$x)) + 1),
                                        oob = scales::oob_keep
                     ) +
                     scale_y_continuous(limits = c(0,originalMethodData_targetToRef %>%
                                                     group_by(x) %>%
                                                     tally() %>%
                                                     pull(n) %>%
                                                     max() %>%
                                                     unique())) +
                     geom_vline(xintercept = unique(originalMethodData_targetToRef$med_x),
                                colour = "#7570b3") +
                     ylab("# Cell Pairs")+
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   yPlot <- originalMethodData_targetToRef %>%
                     ggplot() +
                     geom_histogram(aes(x=y,fill = yClassif),
                                    alpha = .7,
                                    binwidth = 1) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_ribbon(mapping = aes(y = seq(0,originalMethodData_targetToRef %>%
                                                         group_by(x) %>%
                                                         tally() %>%
                                                         pull(n) %>%
                                                         max() %>%
                                                         unique(),
                                                       length.out = nrow(originalMethodData_targetToRef)),
                                               xmin = med_y - input$translationThreshold - .5,
                                               xmax = med_y + input$translationThreshold + .5),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"y")), 
                                        breaks = seq(-max(abs(comparisonData$y)),max(abs(comparisonData$y)),by = 50),
                                        limits = c(-max(abs(comparisonData$y)) - 1,max(abs(comparisonData$y)) + 1),
                                        oob = scales::oob_keep
                     ) +
                     scale_y_continuous(limits = c(0,originalMethodData_targetToRef %>%
                                                     group_by(y) %>%
                                                     tally() %>%
                                                     pull(n) %>%
                                                     max() %>%
                                                     unique())) +
                     geom_vline(xintercept = unique(originalMethodData_targetToRef$med_y),
                                colour = "#7570b3") +
                     ylab("# Cell Pairs")+
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   thetaPlot <- originalMethodData_targetToRef %>%
                     ggplot() +
                     geom_bar(aes(x = theta, 
                                  fill = thetaClassif),
                              alpha = .7) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_ribbon(aes(y = seq(0,originalMethodData_targetToRef %>%
                                               group_by(theta) %>%
                                               tally() %>%
                                               pull(n) %>%
                                               max(),
                                             length.out = nrow(originalMethodData_targetToRef)),
                                     xmin = med_theta - input$rotationThreshold - 1.5,
                                     xmax = med_theta + input$rotationThreshold + 1.5),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated rotation angle ", theta)), 
                                        breaks = seq(-max(c(abs(comparisonData$theta),30)),max(c(abs(comparisonData$theta),30)),by = 15),
                                        limits = c(-max(c(abs(comparisonData$theta),30)),max(c(abs(comparisonData$theta),30))) + c(-2,2)) +
                     # scale_y_continuous(limits = c(0,7),
                     #                    breaks = seq(0,7,by = 1)) +
                     ylab("# Cell Pairs") +
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), 
                           axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   corrPlot <- originalMethodData_targetToRef %>%
                     ggplot(aes(x = pairwiseCompCor,fill = corClassif)) +
                     geom_histogram(binwidth = .01,
                                    alpha = .7) +
                     scale_fill_manual(values = c("black","gray50")) +
                     geom_vline(aes(xintercept = input$corrThreshold),
                                colour = "#7570b3") +
                     geom_ribbon(mapping = aes(y = seq(-1,
                                                       originalMethodData_targetToRef %>%
                                                         mutate(bin = (trunc(pairwiseCompCor*100))/100) %>%
                                                         group_by(bin) %>%
                                                         tally() %>%
                                                         pull(n) %>%
                                                         max() %>%
                                                         unique(),
                                                       length.out = nrow(originalMethodData_targetToRef)),
                                               xmin = input$corrThreshold,
                                               xmax = 1),
                                 fill = "#7570b3",
                                 alpha = .3) +
                     theme_bw() +
                     scale_x_continuous(expression(paste("Estimated CCF"[max])), 
                                        breaks = seq(0,1,by = .5)) +
                     coord_cartesian(xlim = c(0,1),
                                     ylim = c(0,NA),
                                     expand = FALSE) +
                     ylab("# Cell Pairs") +
                     theme(legend.position = "none",
                           axis.text = element_text(size = 8),        
                           axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)
                           ,plot.margin=unit(c(0,.1,0,.1), "cm")
                     )
                   
                   return((xPlot / yPlot / thetaPlot / corrPlot))
                   
                 })
                 
                 
                 output$highCMC_refToTarget_diagnosticPlot <- renderPlot({
                   
                   plt <- comparisonData %>%
                     filter(direction == 'refToTarget') %>%
                     mutate(cmcThetaDistribClassif = decision_highCMC_cmcThetaDistrib(cellIndex = cellIndex,
                                                                                      x = x,
                                                                                      y = y,
                                                                                      theta = theta,
                                                                                      corr = pairwiseCompCor,
                                                                                      xThresh = input$translationThreshold,
                                                                                      yThresh = input$translationThreshold,
                                                                                      corrThresh = input$corrThreshold)) %>%
                     decision_highCMC_identifyHighCMCThetas(tau = input$highCMCThreshold) %>%
                     filter(cmcThetaDistribClassif == "CMC Candidate") %>%
                     ggplot() +
                     geom_bar(aes(x = theta, fill = thetaCMCIdentif),
                              stat = "count",
                              alpha = .7) +
                     geom_hline(aes(yintercept = max(cmcCandidateCount) - input$highCMCThreshold),
                                linetype = "dashed") +
                     scale_fill_manual(values = c("black","gray50")) +
                     theme_bw() +
                     ylab("CMC Candidate Count") +
                     xlab(expression(theta)) +
                     ggtitle("CMC-Theta Distribution, Reference vs. Target") +
                     xlim(c(min(comparisonData$theta),max(comparisonData$theta)))
                   
                   return(plt)
                   
                 })
                 
                 output$highCMC_targetToRef_diagnosticPlot <- renderPlot({
                   
                   plt <- comparisonData %>%
                     filter(direction == 'targetToRef') %>%
                     mutate(cmcThetaDistribClassif = decision_highCMC_cmcThetaDistrib(cellIndex = cellIndex,
                                                                                      x = x,
                                                                                      y = y,
                                                                                      theta = theta,
                                                                                      corr = pairwiseCompCor,
                                                                                      xThresh = input$translationThreshold,
                                                                                      yThresh = input$translationThreshold,
                                                                                      corrThresh = input$corrThreshold)) %>%
                     decision_highCMC_identifyHighCMCThetas(tau = input$highCMCThreshold) %>%
                     filter(cmcThetaDistribClassif == "CMC Candidate") %>%
                     ggplot() +
                     geom_bar(aes(x = theta, fill = thetaCMCIdentif),
                              stat = "count",
                              alpha = .7) +
                     geom_hline(aes(yintercept = max(cmcCandidateCount) - input$highCMCThreshold),
                                linetype = "dashed") +
                     scale_fill_manual(values = c("black","gray50")) +
                     theme_bw() +
                     ylab("CMC Candidate Count") +
                     xlab(expression(theta)) +
                     ggtitle("CMC-Theta Distribution, Target vs. Reference") +
                     xlim(c(min(comparisonData$theta),max(comparisonData$theta)))
                   
                   return(plt)
                 })
                 
               }
               
             })


