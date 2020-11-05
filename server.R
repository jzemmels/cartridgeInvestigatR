server = function(input, output, session) {
  
  ## Update directory
  newuserdir <- tempfile()
  dir.create(newuserdir, recursive = TRUE)
  sapply(file.path(newuserdir, dir(newuserdir)[grep("code_", dir(newuserdir))]), file.remove)
  file.copy(file.path(userdir, "code_All.R"), newuserdir)
  userdir <- newuserdir
  dir.create(file.path(userdir, "data"))
  
  ## Check for file update every 5 seconds
  code <- reactiveFileReader(500, session, file.path(userdir, "code_All.R"), clean_readlines)
  
  ## load the main part of the server
  source("something/server_main.R", local = TRUE)
  
  ###################################
  ## x3p tag related
  ###################################
  
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
      
      interpolate(~(bullet <- read_bullet(dir)),
                  dir = dir,
                  mydir = userdir,
                  `_env` = environment(),
                  file = "code_x3p.R",
                  append = TRUE, eval = FALSE)
      
      NOSHINY_TT <<- FALSE
    } 
  })
  
  # observe({
  #   if(input$ck_rotation) {
  #     output$r_angle_out <- renderUI({
  #       numericInput("r_angle", "Rotation Angle:", -90, min = -360, max = 360)
  #     })
  #   } else {
  #     output$r_angle_out <- renderUI({NULL})
  #   }
  # })
  
  # observe({
  #   if(input$ck_downsample) {
  #     output$ds_m_out <- renderUI({
  #       numericInput("ds_num", "Down sample by:", 2, min = 1, max = 500)
  #     })
  #   } else {
  #     output$ds_m_out <- renderUI({NULL})
  #   }
  # })
  
  observeEvent(input$cp_size, {
    if(isolate(hasname_x3p())) {
      output$x3pinfo <- renderText({
        paste(isolate(shiny.r$data$x3p[[1]]) %>%
                x3p_show_xml("size") %>% head(2) %>% unlist())
        # print(isolate(shiny.r$data$x3p[[1]]))
      })
    }
  })
  
  # observeEvent(input$process_x3p, {
  #   
  #   if(hasname_x3p()) {
  #     tmp <- isolate(shiny.r$data)
  #     ppt <- c()
  #     
  #     show_modal_spinner(spin = "atom", text = "Processing x3p files...")
  #     
  #     if(isolate(input$ck_downsample)) {
  #       tmp <- tmp %>% mutate(
  #         x3p = x3p %>% purrr::map(.f = function(x) x %>% x3p_sample(m = isolate(input$ds_num))))
  #       ppt <- c(ppt, paste("Finished down sampling with m:", isolate(input$ds_num)))
  #     }
  #     
  #     if(isolate(input$ck_mtomum)) {
  #       tmp <- tmp %>% mutate(x3p = x3p %>% purrr::map(.f = x3p_m_to_mum))
  #       ppt <- c(ppt, "Finished m_to_mum")
  #     }
  #     
  #     if(isolate(input$ck_rotation)) {
  #       tmp <-  tmp %>% mutate(
  #         x3p = x3p %>% purrr::map(.f = function(x) x %>% rotate_x3p(angle = isolate(input$r_angle))))
  #       ppt <- c(ppt, paste("Finished rotation with angle:", isolate(input$r_angle)))
  #     }
  #     
  #     if(isolate(input$ck_flip)) {
  #       tmp <- tmp %>% mutate(x3p = x3p %>% purrr::map(.f = y_flip_x3p))
  #       ppt <- c(ppt, "Finished y flip")
  #     }
  #     
  #     output$x3pinfo <- renderText({
  #       paste(ppt, collapse = ".\n")
  #     })
  #     
  #     shiny.r$data <- tmp
  #     NOSHINY_TT <<- FALSE
  #     
  #     remove_modal_spinner()
  #     
  #   } else {
  #     output$x3pinfo <- renderText({
  #       validate(need(shiny.r$data, message = "Could not process, no x3p files found."))
  #       # print("No x3p found.")
  #     })
  #   }
  # })
  
  
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
  
  
  # output code
  observe({    
    updateAceEditor(session, "myEditor", value = paste(code(), collapse = "\n"))
  })
  
  observeEvent(input$clean_code_window, {
    shiny.r <<- reactiveValues(data = tibble())
    sapply(file.path(userdir, dir(userdir)[grep("code_", dir(userdir))]), file.remove)
    init_code_all_R(userdir)
  })
  
  x3p_init(userdir, "code_x3p.R")
  cat(userdir)
  x3p_flip_yServer("x3p_flip", reactive(shiny.r$data), userdir) 
  x3p_sampleServer("x3p_sample", reactive(shiny.r$data), userdir)
  x3p_mtomumServer("x3p_m_to_mum", reactive(shiny.r$data), userdir)
  x3p_rotateServer("x3p_rotate", reactive(shiny.r$data), userdir)
  x3p_show_xmlServer("x3p_show_xml", reactive(shiny.r$data), userdir)
  prepare_tt_Server("prepare_shinytt", reactive(shiny.r$data), userdir)
  
  observeEvent(input$updateCode, {
    if(!file.exists(file.path(userdir, "code_x3p.R"))) {
      x3p_init(userdir, "code_x3p.R")
    }
    
    init_code_all_R(userdir)
    cat(paste0("\n\n", paste(readLines(file.path(userdir, "code_x3p.R")), collapse = "\n")),
        file = file.path(userdir, "code_All.R"), append = TRUE)
  })
  
  
  ###########???
  ##############
  output$mydownload <- downloadHandler(
    filename = function() {  paste("bynvtgR_report-", Sys.Date(), ".R", sep="") },
    content = function(file) { 
      NULL
    }
  )
  
  
}