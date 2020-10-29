server = function(input, output, session) {
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
      paste("Successfully updated shiny.tt")
    })
    
    # if (LOADED_SHINY_TT) {
    #   
    #   output$saveptp <- renderText({
    #     paste("Could not update. Please save your result directly.")
    #   })
    # } else {
    #   shiny.tt <<- shiny.r$data
    #   output$saveptp <- renderText({
    #     paste("Successfully updated shiny.tt")
    #   })
    # }
    
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
        
        updateSelectInput(session, "k","Investigate kth land:",
                          selected = scanidx,
                          choices=1:isolate(n()))
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
  
  ###############################################
  ####### things related to x3p tag #############
  ###############################################
  
  # upload x3p folder
  shinyDirChoose(input, "x3pdir", roots = volumes, session = session, 
                 restrictions = system.file(package = "base"))
  observeEvent(input$x3pdir, {
    if(!is.integer(input$x3pdir)) {
      dir <- parseDirPath(volumes, input$x3pdir)
      
      show_modal_spinner(spin = "atom", text = "Loading x3p files...")
      
      shiny.r$data <<- try(read_bullet(dir))
      # shiny.r$data <<- validate(need(read_bullet(dir), "No bullet files found."))
      # browser()
      remove_modal_spinner()
      
      output$x3pdir_prompt <- renderText({
        validate(need(shiny.r$data, message = "No x3p files found in this directory!"))
        paste("finished loading x3p files.")
      })
      NOSHINY_TT <<- FALSE
    } 
  })
  
  observe({
    if(input$ck_rotation) {
      output$r_angle_out <- renderUI({
        numericInput("r_angle", "Rotation Angle:", -90, min = -360, max = 360)
      })
    } else {
      output$r_angle_out <- renderUI({NULL})
    }
  })
  
  observe({
    if(input$ck_downsample) {
      output$ds_m_out <- renderUI({
        numericInput("ds_num", "Down sample by:", 2, min = 1, max = 500)
      })
    } else {
      output$ds_m_out <- renderUI({NULL})
    }
  })
  
  observeEvent(input$cp_size, {
    if(isolate(hasname_x3p())) {
      output$x3pinfo <- renderText({
        paste(isolate(shiny.r$data$x3p[[1]]) %>%
                x3p_show_xml("size") %>% head(2) %>% unlist())
        # print(isolate(shiny.r$data$x3p[[1]]))
      })
    }
  })
  
  observeEvent(input$process_x3p, {
    # req(shiny.r$data)
    
    # cat(input$process_x3p)
    
    if(hasname_x3p()) {
      # cat(isolate(input$cp_flip))
      tmp <- isolate(shiny.r$data)
      ppt <- c()
      
      show_modal_spinner(spin = "atom", text = "Processing x3p files...")
      
      if(isolate(input$ck_downsample)) {
        tmp <- tmp %>% mutate(
          x3p = x3p %>% purrr::map(.f = function(x) x %>% x3p_sample(m = isolate(input$ds_num))))
        ppt <- c(ppt, paste("Finished down sampling with m:", isolate(input$ds_num)))
      }
      
      if(isolate(input$ck_mtomum)) {
        tmp <- tmp %>% mutate(x3p = x3p %>% purrr::map(.f = x3p_m_to_mum))
        ppt <- c(ppt, "Finished m_to_mum")
      }
      
      if(isolate(input$ck_rotation)) {
        tmp <-  tmp %>% mutate(
          x3p = x3p %>% purrr::map(.f = function(x) x %>% rotate_x3p(angle = isolate(input$r_angle))))
        ppt <- c(ppt, paste("Finished rotation with angle:", isolate(input$r_angle)))
      }
      
      if(isolate(input$ck_flip)) {
        tmp <- tmp %>% mutate(x3p = x3p %>% purrr::map(.f = y_flip_x3p))
        ppt <- c(ppt, "Finished y flip")
      }
      
      output$x3pinfo <- renderText({
        paste(ppt, collapse = ".\n")
      })
      
      shiny.r$data <- tmp
      NOSHINY_TT <<- FALSE
      
      remove_modal_spinner()
      
    } else {
      output$x3pinfo <- renderText({
        validate(need(shiny.r$data, message = "Could not process, no x3p files found."))
        # print("No x3p found.")
      })
    }
  })
  
  
  observeEvent(input$prep_shinytt, {
    # req(shiny.r$data)
    # cat(input$prep_shinytt)
    
    if(hasname_x3p()) {
      tmp <- isolate(shiny.r$data)
      
      # output$prep_shiny_ppt1 <- renderText({
      #   paste("Preparing...")
      # })
      show_modal_spinner(spin = "atom", text = "Preparing...")
      
      if(assertthat::has_name(tmp, "source")) {
        obj <- tmp$source
        cs <- paste(Reduce(intersect, strsplit(obj, "/")), collapse = '/')
        tmp$scan_id <- obj %>% stringr::str_remove(paste(cs, "/", sep = '')) %>%
          stringr::str_remove(".x3p")
      }
      
      tmp <- tmp %>% mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize))
      
      tmp <- tmp %>% mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut,
                                                 .f = x3p_crosscut))
      
      tmp <- tmp %>% mutate(grooves = ccdata %>% purrr::map(.f = cc_locate_grooves,
                                                            method = "middle", adjust = 30, return_plot = TRUE))
      
      shiny.r$data <- tmp
      shiny.tt <<- shiny.r$data
      
      remove_modal_spinner()
      
      output$prep_shiny_ppt <- renderText({
        paste("Finished shiny.tt preparation! Please go back to the first tab and check")
      })
      
    } else {
      output$prep_shiny_ppt <- renderText({
        validate(
          # need(hasname_x3p(), message = "x3p files not loeaded"),
          need(shiny.r$data, message = "Could not prepare, no x3p files found.")
        )
        
      })
    }
  }, ignoreInit = TRUE)
  
  ########################################
  ########################################
  
  
  
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
    cat("Confirmed", input$k, "\n")
    updateSelectInput(session, "k","Investigate kth land:",
                      selected = as.numeric(input$k)+1,
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
    k <- as.numeric(input$k)
    shiny.r$data$grooves[[as.numeric(input$k)]]$marked <<- TRUE
    cat("Marked Groove:", input$k, "\n")
    if (hasname_scanid) {
      cat("Marked Groove Bullet ID:", shiny.r$data$scan_id[k], "\n")
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
      k <- as.numeric(input$k)
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
  
  # onStop(function() {
  #   cat("Session stopped\n")
  #   rm(shiny.r)
  #   rm(NOSHINY_TT)
  # })
  
}