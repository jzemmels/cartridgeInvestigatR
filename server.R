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
  # cat(userdir)
  x3p_flip_yServer("x3p_flip", reactive(shiny.r$data), userdir) 
  x3p_sampleServer("x3p_sample", reactive(shiny.r$data), userdir)
  x3p_mtomumServer("x3p_m_to_mum", reactive(shiny.r$data), userdir)
  x3p_rotateServer("x3p_rotate", reactive(shiny.r$data), userdir)
  x3p_show_xmlServer("x3p_show_xml", reactive(shiny.r$data), userdir)
  prepare_tt_Server("prepare_shinytt", reactive(shiny.r$data), userdir)
  
  ###################################
  # plug in other modules (functions)
  ###################################
  
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