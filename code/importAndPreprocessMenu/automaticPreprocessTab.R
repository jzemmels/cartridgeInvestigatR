observeEvent(input$automaticPreprocessHelp,
             {
               
               showModal(modalDialog(
                 title = h3("Help: Automatic Pre-process Tab"),
                 easyClose = TRUE,
                 h4(strong("Why would I use this tab?")),
                 "Automatically preprocess cartridge case scans using a sequence of algorithms.",
                 br(),
                 br(),
                 h4(strong("What do I need to do before using this tab?")),
                 "Upload scans in the Import tab.",
                 br(),
                 br(),
                 h4(strong("How do I use this tab?")),
                 HTML(paste0("Click the ",strong('Add Another Pre-processing Step')," button to add a preprocessing step to the sequence.")),
                 "Select a preprocessing step from the 'Select Pre-processing Step' dropdown.",
                 "Choose parameters associated with the preprocessing step.",
                 br(),
                 br(),
                 HTML(paste0("Add as many preprocessing steps as you'd like by clicking ",strong('Add Another Pre-processing Step'),".")),
                 HTML(paste0("Once you're happy with the preprocessing procedure, click the ",strong('Perform Automatic Pre-processing')," button.")),
                 HTML(paste0("You may reset the sequence of steps by clicking ",strong('Reset All Pre-processing Steps'),".")),
                 br(),
                 br(),
                 h4(strong("What is next?")),
                 HTML(paste0("If there are regions of a scan that you would like to manually remove, move onto the ",strong('Needs Manual Pre-processing')," button.")),
                 HTML(paste0("Otherwise, if you are happy with the preprocessed scans, then press the ",strong('I would like to compare these scans')," button to move to the next stages.")),
               ))
               
             })

# Render the "Perform Automatic Pre-processing" button after at least one preprocessing step is added

observeEvent(input$insertBtn,{
  
  output$preProcessExecute_ui <- renderUI({fluidRow(column(width = 3,
                                                           actionButton("preProcessExecute",
                                                                        label = "Perform Automatic Pre-processing",
                                                                        width = 300,
                                                                        style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                                                                        icon = fontawesome::fa_i("play")),
                                                           bsTooltip(id = "preProcessExecute",title = "Execute selected preprocessing steps")))})
  
})


# in the Pre-Processing tab, add a dynamic number steps by including a "Add
# another step" button

ctn <- reactiveVal(0)
# if the Reset button is pressed, then ctn will need to go back to value 1.
# However, the way the code below is set up the insertBtn event will cause
# an update to the output any time ctn is updated, which we don't want if
# we're sending ctn back to 1. this new reactive value will be subraacted
# away from ctn below if the reset button is pressed so that ctn can
# properly be set to 1.
ctn_sub <- reactiveVal(0)
Id <- reactive({
  function(id,ind){
    paste0(id, ind)
  }
})

observeEvent(input$insertBtn, {
  
  ctn(ctn() + 1 - ctn_sub())
  ctn_sub(0)
  
  insertUI(
    selector = '#placeholder',
    ui = div(
      id = Id()('div',ctn()),
      fluidRow(column(4,selectInput(Id()('letter',ctn()), 'Select Pre-processing Step', c("Downsample","Crop","Level","Erode","Filter","Delete","Trim"),selected = NULL,multiple = FALSE)),
               column(8,uiOutput(Id()('input',ctn()))))
    )
  )
  
})

observeEvent(ctn(), {
  
  ctn <- ctn()
  
  id <- Id()('input',ctn)
  selection <- Id()('letter',ctn)
  
  output[[id]] <- renderUI({
    req(input[[Id()('letter',ctn)]])
    switch(
      input[[selection]],
      'Downsample' = column(width = 8,numericInput(Id()('params1_',ctn), 'Stride',value = 2,min = 1)),
      'Crop' = column(width = 8,selectInput(Id()('params1_',ctn), 'Region',choices = c("Interior","Exterior")),
                      numericInput(Id()('params2_',ctn), 'Offset',value = 0)),
      'Level' = column(width = 8,selectInput(Id()('params1_',ctn),"Statistic",choices = c("Median","Mean"))),
      'Erode' = column(width = 8,selectInput(Id()('params1_',ctn), 'Region',choices = c("Interior","Exterior")),
                       numericInput(Id()('params2_',ctn), 'Radius',value = 1,min = 1)),
      'Filter' = column(width = 8,selectInput(Id()('params1_',ctn), 'Filtertype',choices = c("Bandpass","Lowpass")),
                        textInput(Id()('params2_',ctn), 'Wavelength(s) - Use comma separation for Bandpass',value = "16, 500")),
      'Delete' = column(width = 8,textInput(Id()('params1_',ctn), 'Mask Hex Value to Delete',value = "#000000FF")),
      'Trim' = column(width = 8)
    )
  })
  
}, ignoreInit = TRUE)

# button to delete all preprocessing select inputs
observeEvent(input$restartPreprocess,{
  
  ctn <- ctn()
  
  for(ind in 1:ctn){
    
    # this removes the selectInputs/numericInputs seen on screen 
    removeUI(selector = paste0("div#div",ind),multiple = TRUE,immediate = TRUE)
    removeUI(selector = paste0("div#input",ind),multiple = TRUE,immediate = TRUE)
    
    # this updates the input environment object so that the
    # preprocessing steps/parameters are set back to NULL
    session$sendCustomMessage("resetValue",paste0("letter",ind))
    session$sendCustomMessage("resetValue",paste0("downsamp",ind))
    session$sendCustomMessage("resetValue",paste0("cropOffset",ind))
    session$sendCustomMessage("resetValue",paste0("cropRegion",ind))
    session$sendCustomMessage("resetValue",paste0("Statistic",ind))
    session$sendCustomMessage("resetValue",paste0("erodeRegion",ind))
    session$sendCustomMessage("resetValue",paste0("erodeRadius",ind))
    session$sendCustomMessage("resetValue",paste0("filterType",ind))
    session$sendCustomMessage("resetValue",paste0("filterParams",ind))
  }
  
  #update ctn_sub from 0 to the current value of ctn so that it can be
  #*sub*tracted off of the value of ctn above
  ctn_sub(ctn)
})

# perform the preprocessing procedures
observeEvent(input$preProcessExecute,{
  
  # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Preprocessing scans")
  
  # the input object is updated as the user adds more preprocessing steps
  # or changes the selection per step.
  tmpInput <- shiny::reactiveValuesToList(input)
  
  # each preprocessing step is in-order as specified by letter# where # is
  # a number
  preProcessSteps <- tmpInput[stringr::str_detect(names(tmpInput),"letter")]
  
  if(length(preProcessSteps) == 0){
    
    showNotification("You haven't selected any preprocessing steps. Either add a preprocessing step or click 'Skip Automatic Pre-processing' on the 'Import' tab to skip.",type = "error")
    validate(need(length(preProcessSteps) > 0,"No preprocessing steps selected. Either add a preprocessing step or click 'Skip Automatic Pre-processing' on the 'Import' tab to skip."))
    
  }
  
  # the steps may be out of order depending on which was last updated by
  # the user. this will correct the order
  preProcessSteps <- preProcessSteps[order(names(preProcessSteps))]
  
  preProcessFunctions <- purrr::map(1:length(preProcessSteps),
                                    function(ind){
                                      
                                      if(!is.null(preProcessSteps[[ind]])){
                                        
                                        # get the parameters
                                        paramValues <- tmpInput[names(tmpInput) %in% paste0("params",c(1,2),"_",ind)]
                                        
                                        
                                        # returns a preprocessing function with the necessary parameters
                                        # filled-in
                                        return(preProcess_partial(preProcessSteps[[ind]],paramValues))
                                        
                                      }
                                    }) %>%
    purrr::discard(~ all(is.null(.)))
  
  # shiny.r$preProcessFunctions <<- preProcessFunctions
  
  req(nrow(shiny.r$data) > 0)
  
  tmp <- isolate(shiny.r$data)
  
  tmp <- tmp %>%
    mutate(x3p_processed = purrr::map(x3p,
                                      function(scan){
                                        
                                        purrr::walk(preProcessFunctions,
                                                    function(func){
                                                      
                                                      scan <<- func(x3p = scan)
                                                      
                                                    })
                                        
                                        return(scan)
                                      }))
  
  shiny.r$data <<- tmp
  
  
  # plot the x3p_processed scans on the Preprocess tab
  output$preProcessedPlot <- renderPlot(bg = "white",{
    
    x3pListPlot(tmp$x3p_processed) +
      theme(legend.position = "none")
    
  })
  
})

###############################################################

# Tab transitions:

# If the Reset button is pressed, then de-render all of the action buttons up to this point

# after performing preprocessing, render buttons to move to the next steps
observeEvent(input$preProcessExecute,{
  
  output$automaticPreprocess_nextStep_ui <- renderUI({
    box(width = 12,#align = "center",
        h4('Preview the scans to the right.
           If you are satisfied with the automatic pre-processing, then choose one of the options below.
           Otherwise, press "Reset All Pre-processing Steps" to start over.'),
        # br(),
        actionButton(inputId = "automaticPreprocess_goToManualPreprocess",
                     label = "I would like to manually pre-process specific scans.",
                     width = 350,
                     icon = fontawesome::fa_i("hand-scissors")),
        br(),
        br(),
        actionButton(inputId = "automaticPreprocess_goToComparison",
                     label = "Looks good! I would like to compare these scans.",
                     width = 350,
                     style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                     icon = icon("pencil-ruler"))
    )})
  
})

observeEvent(input$restartPreprocess,{
  
  output$preProcessExecute_ui <- renderUI(NULL)
  output$preProcessedPlot <- renderPlot(NULL,bg = "white")
  output$automaticPreprocess_nextStep_ui <- renderUI(NULL)
  
})


# If "Looks Good!" button is selected, then move onto the Compare Stage

observeEvent(input$automaticPreprocess_goToComparison,{
  
  # hideTab(inputId = "preprocessingTabs","Import",session = session)
  # hideTab(inputId = "preprocessingTabs","Manual Deletion - Select a Scan",session = session)
  # hideTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
  showTab(inputId = "preprocessingTabs",target = "Pre-processing Completed",session = session)
  
  # output$import_nextStep_ui <- renderUI("Click on '2. Compare' in the sidebar menu")
  
  output$comparing_ui <- renderMenu({
    
    menuItem("Explore", tabName = "comparing",icon = icon("magnifying-glass"))
    
  })
  output$scoring_ui <- renderMenu({
    
    menuItem("Score", 
             tabName = "scoring",
             icon = icon("weight"))
    
  })
  output$exporting_ui <- renderMenu({
    
    menuItem("Export", 
             tabName = "exporting",
             icon = icon("file-export"))
    
  })
  
  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  
  # updateTabItems(session = session,inputId = "stages",selected = "comparing")
  updateTabsetPanel(session = session,"preprocessingTabs",selected = "Pre-processing Completed")
  
})

# if "Needs Manual Preprocessing" is selected, then move to the appropriate tab

observeEvent(input$automaticPreprocess_goToManualPreprocess,{
  
  # hideTab(inputId = "preprocessingTabs","Import",session = session)
  # hideTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
  showTab(inputId = "preprocessingTabs",target = "Manual Deletion - Select a Scan",session = session)
  
  updateTabsetPanel(session = session,"preprocessingTabs",selected = "Manual Deletion - Select a Scan")
  
})