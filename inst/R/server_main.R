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

observeEvent(input$displayx3p2, {
  req(shiny.r$data)
  req(nrow(shiny.r$data) >= 1)
  
  k <- 1
  
  tmp <- isolate(shiny.r$data)
  
  output$x3prgl2 <- renderRglwidget({
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

























