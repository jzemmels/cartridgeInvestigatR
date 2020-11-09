n <- reactive({ nrow(shiny.r$data) })
hasname_x3p <- reactive({ assertthat::has_name(shiny.r$data, "x3p") }) 
hasname_scanid <- reactive({ assertthat::has_name(shiny.r$data, "scan_id") }) 
hasname_grooves <- reactive({ assertthat::has_name(shiny.r$data, "grooves") }) 
hasname_crosscut <- reactive({ assertthat::has_name(shiny.r$data, "crosscut") }) 
check_names_all <- reactive({ shiny.r$data %>% assertthat::has_name(c("x3p", "crosscut", "grooves", "scan_id")) %>% all() })

all_scan_id <- reactive({
  if(hasname_scanid()) {shiny.r$data$scan_id}
  else { NULL }
})

# CONDITIONAL PANEL SETUP
output$hasname_x3p <- hasname_x3p
outputOptions(output, "hasname_x3p", suspendWhenHidden = FALSE)

output$hasname_scanid <- hasname_scanid
outputOptions(output, "hasname_scanid", suspendWhenHidden = FALSE)

output$selectk <- renderUI({
  # cat("bug\n")
  selectInput("k","Investigate kth land:", selected = 1,
              choices=1:n())
})

observeEvent(input$saveCurrentEnv, {
  shiny.tt <<- shiny.r$data
  output$saveptp <- renderText({
    paste("Successfully saved your progress. A object called 'shiny.tt' should be found in your R environment")
  })
  
})

# check shiny.tt
observeEvent(input$ttcheck, {
  x3p_checker <- isolate(hasname_x3p())
  crosscut_checker <- isolate(hasname_crosscut())
  grooves_checker <- isolate(hasname_grooves())
  scanid_checker <- isolate(hasname_scanid())
  
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


# observe related to scanID
observe({
  if(hasname_scanid()) {
    output$selectid <- renderUI({
      selectInput("scanID","Investigate according to the Scan ID :",
                  selected = all_scan_id()[1],
                  choices=all_scan_id())
    })
    
    observeEvent(input$scanID, {
      scanidx <- shiny.r$data %>% tibble::rowid_to_column() %>%
        filter(scan_id == isolate(input$scanID)) %>% pull(rowid)
      # cat((scanidx))
      
      # if(scanidx != isolate(input$k)) {
        updateSelectInput(session, "k","Investigate kth land:",
                          selected = scanidx,
                          choices=1:isolate(n()))
      # }

    })
    
    observeEvent(input$k, {
      updateSelectInput(session, "scanID","Investigate according to the Scan ID :",
                        selected = all_scan_id()[as.numeric(isolate(input$k))],
                        choices = all_scan_id())
    })
  } else {
    output$selectid <- renderUI({NULL})
  }
})

# upload rds file
volumes <- c(Home = fs::path_wd(), "R Installation" = R.home(), getVolumes()())
shinyFileChoose(input, "file1", roots = volumes, session = session)
observeEvent(input$file1, {
  if(!is.integer(input$file1)) {
    file <- parseFilePaths(volumes, input$file1)
    req(file)
    ext <- tools::file_ext(file$datapath)
    
    if(ext != "rds") {
      output$file1_prompt <- renderText({
        paste("Please upload a rds file.")
      })
    }
    
    validate(need(ext == "rds", "Please upload a rds file"))
    show_modal_spinner(spin = "atom", text = "Loading rds files...")
    
    shiny.r$data <<- readRDS(file$datapath)
    
    remove_modal_spinner()
    
    output$file1_prompt <- renderText({
      paste("finished loading the rds file.")
    })
    NOSHINY_TT <<- FALSE
  }
})


observe({
  if(!is.null(input$k)) {
    # if grooves are provided, draw plots
    if(hasname_grooves()){
      output$groovePlot <- renderPlot({
        k <- as.numeric(input$k)
        
        if(assertthat::has_name(shiny.r$data$grooves[[k]], "groove")){
          p <- shiny.r$data$grooves[[k]]$plot +
            geom_vline(xintercept = shiny.r$data$grooves[[k]]$groove[1], colour="red") +
            geom_vline(xintercept = shiny.r$data$grooves[[k]]$groove[2], colour="red")
        }
        p
      })
      
      output$groovelocations <- renderText({
        paste("Left Groove: ",shiny.r$data$grooves[[as.numeric(input$k)]]$groove[1],
              "    Right Groove: ",shiny.r$data$grooves[[as.numeric(input$k)]]$groove[2])
      })
    }
    
    # if x3p are provided, give all other stuff
    if(hasname_x3p()){
      output$sigPlot <- renderPlot({
        k <- as.numeric(input$k)
        ggplot() + geom_blank()
      })
      
      if(hasname_crosscut()){
        output$ccvalue <- renderText({
          paste("Current Crosscut Value: ",
                shiny.r$data$crosscut[as.numeric(input$k)] ) })
      }
      
    }
  }
})

# CONFIRM
observeEvent(input$confirm,{
  
  k <- as.numeric(isolate(input$k))
  cat("Confirmed", k, "\n")
  updateSelectInput(session, "k","Investigate kth land:",
                    selected = k+1,
                    choices=1:isolate(n()))
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
  # req(isolate(input$mark))
  # req(isolate(input$k))
  # req(isolate(shiny.r))
  
  k <- as.numeric(isolate(input$k))
  shiny.r$data$grooves[[k]]$marked <<- TRUE
  cat("Marked Groove:", k, "\n")
  if (isolate(hasname_scanid())) {
    cat("Marked Groove Bullet ID:", isolate(shiny.r$data$scan_id[k]), "\n")
  } else {
    cat("Bullet ID is not available. \n")
  }
})

# UNMARK
observeEvent(input$unmark,{
  shiny.r$data$grooves[[as.numeric(input$k)]]$marked <<- FALSE
  cat("Unmarked Groove:", input$k, "\n")
})

# EVENT: UPDATE CROSSCUT VALUE
observeEvent(input$updateCC, {
  
  k <- as.numeric(input$k)
  tmp.tt <- shiny.r$data %>% slice(k)
  
  tmp.tt$crosscut <- as.numeric(input$cc)
  
  tmp.tt <- tmp.tt %>%
    mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = bulletxtrctr::x3p_crosscut)
    ) %>%
    mutate(grooves = ccdata %>% purrr::map(.f = bulletxtrctr::cc_locate_grooves, method = "middle", adjust = 30, return_plot = TRUE))
  
  shiny.r$data[k, c("crosscut", "ccdata", "grooves")] <<- tmp.tt %>% select(crosscut, ccdata, grooves)
  
  cat("updated crosscut value: ", shiny.r$data$crosscut[k], "\n")
  if (isolate(hasname_scanid())) {
    cat("Bullet ID:", shiny.r$data$scan_id[k], "\n")
  } else {
    cat("Bullet ID is not available. \n")
  }
  output$ccvalue <- renderText({
    cat("test1")
    paste("Current Crosscut Value: ", shiny.r$data$crosscut[as.numeric(input$k)] ) })
  
})

# EVENT: DRAW SIGNATURE
observeEvent(input$drawsig, {
  k <- as.numeric(input$k)
  
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
    k <- as.numeric(input$k)
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
  k <- as.numeric(input$k)
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
  
  k <- as.numeric(input$k)
  
  tmp <- isolate(shiny.r$data)
  
  output$x3prgl <- renderRglwidget({
    if(isolate(hasname_crosscut())){
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
    if(isolate(hasname_crosscut())){
      image_x3p(
        tmp$x3p[k][[1]] %>% x3p_add_hline(
          yintercept = tmp$crosscut[k], size = 10)) 
    } else {
      image_x3p(tmp$x3p[k][[1]]) 
    }
  })
})

# observeEvent(input$displayx3p2, {
#   k <- as.numeric(input$k)
# 
#   tmp <- isolate(shiny.r$data)
# 
#   output$x3prgl2 <- renderRglwidget({
#     image_x3p(
#       tmp$x3p[k][[1]] %>% x3p_add_hline(
#         yintercept = tmp$crosscut[k], size = 10))
#   })
# })

# EVENT: SELECT K
observeEvent(input$k, {
  output$sigPlot <- renderPlot({
    k <- as.numeric(isolate(input$k))
    ggplot() + geom_blank()
  })
})


# EVENT: INPUT CROSSCUT VALUE WITH TEXT
observeEvent(input$cc, {
  updateSliderInput(session, "cc_slide", "change crosscut with a slider",
                    min = 0, max = 1000,
                    value = as.numeric(input$cc), step = 0.1)
})
# EVENT: INPUT CROSSCUT VALUE WITH SLIDER
observeEvent(input$cc_slide, {
  updateNumericInput(session, "cc", "change crosscut to:",
                     as.numeric(input$cc_slide), min = 0, max = 1000)
})








