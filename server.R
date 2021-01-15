server = function(input, output, session) {
  
  observe_helpers(withMathJax = FALSE)
  
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
  # check the location after installation of the package
  ################ important!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  source("inst/R/server_main.R", local = TRUE)
  source("inst/R/server_sig_compare.R", local = TRUE)
  
  # if shiny.tt exits in the current environment, add a comment
  # interpolate(~("# abv"),
  #             mydir = userdir,
  #             `_env` = environment(),
  #             file = "code_x3p.R",
  #             append = TRUE, eval = FALSE)
  
  
  # upload from rds file
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
      
      # initialize shiny.r
      shiny.r$data <<- readRDS(file$datapath)
      dataPar <<- data_CheckPar(isolate(shiny.r$data))
      if(!assertthat::has_name(shiny.r$data, "type")) { shiny.r$data$type <<- 'NA' }
      if(!assertthat::has_name(shiny.r$data, "comments")) { shiny.r$data$comments <<- '' }
      # if(!assertthat::has_name(shiny.r$data, "changed_crosscut")) { shiny.r$data$changed_crosscut <<- 'FALSE' }
      
      remove_modal_spinner()
      
      output$file1_prompt <- renderText({
        paste("finished loading the rds file.")
      })
      NOSHINY_TT <<- FALSE
      
      interpolate(~(bullet <- readRDS(dir)),
                  dir = file$datapath,
                  mydir = userdir,
                  `_env` = environment(),
                  file = "code_x3p.R",
                  append = TRUE, eval = FALSE)
      
    }
  })
  
  ###################################
  ## x3p tag related
  ###################################
  
  # upload from x3p folder
  shinyDirChoose(input, "x3pdir", roots = volumes, session = session, 
                 restrictions = system.file(package = "base"))
  observeEvent(input$x3pdir, {
    if(!is.integer(input$x3pdir)) {
      dir <- parseDirPath(volumes, input$x3pdir)
      
      show_modal_spinner(spin = "atom", text = "Loading x3p files...")
      
      # initialize shiny.r
      shiny.r$data <<- try(read_bullet(dir))
      dataPar <<- try(data_CheckPar(isolate(shiny.r$data)))
      shiny.r$data$type <<- 'NA'
      shiny.r$data$comments <<- ''
      # shiny.r$data$changed_crosscut <<- 'FALSE'
      
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
    dataPar <<- data_CheckPar(isolate(shiny.r$data))
    
    NOSHINY_TT <<- TRUE
    
    sapply(file.path(userdir, dir(userdir)[grep("code_", dir(userdir))]), file.remove)
    init_code_all_R(userdir, NOSHINY_TT)
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
    
    init_code_all_R(userdir, NOSHINY_TT)
    cat(paste0("\n\n", paste(readLines(file.path(userdir, "code_x3p.R")), collapse = "\n")),
        file = file.path(userdir, "code_All.R"), append = TRUE)
  })
  
  
  ###########???
  ##############
  output$download_codes <- downloadHandler(
    filename = function() {  paste("output_codes.R") },
    content = function(file) {
      file.copy(file.path(userdir, "code_All.R"), file)
    }
  )
  
  
}