x3p_init <- function(userdir, filename) {
  cat(paste("", "\n"), file = file.path(userdir, filename), append = FALSE)
}


x3pActionButtonUI <- function(id, label = "Click me!") {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("compute_button"), label),
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
          paste(isolate(data()$x3p[[1]]) %>% x3p_show_xml("size") %>% head(2) %>% unlist())
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
          
          tmp <- tmp %>% mutate(grooves = ccdata %>% 
                                  purrr::map(.f = cc_locate_grooves,
                                             method = "middle", adjust = 30, return_plot = TRUE))
          interpolate(~(bullet <- bullet %>% 
                          mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize))),
                      mydir = userdir,
                      `_env` = environment(),
                      file = "code_x3p.R",
                      append = TRUE, eval = FALSE)
          
          interpolate(~(bullet <- bullet %>% 
                          mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut))),
                      mydir = userdir,
                      `_env` = environment(),
                      file = "code_x3p.R",
                      append = TRUE, eval = FALSE)
          
          interpolate(~(bullet <- bullet %>% mutate(
            grooves = ccdata %>% purrr::map(.f = cc_locate_grooves, method = "middle",  
                                            adjust = 30, return_plot = TRUE))),
                      mydir = userdir,
                      `_env` = environment(),
                      file = "code_x3p.R",
                      append = TRUE, eval = FALSE)
          
          shiny.r$data <<- tmp
          dataPar <<- data_CheckPar(shiny.r$data)
          shiny.tt <<- shiny.r$data
          
          remove_modal_spinner()
          
          output$text_window <- renderText({
            paste("Finished data preparation! You are ready to start the investigation!")
          })
          
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







