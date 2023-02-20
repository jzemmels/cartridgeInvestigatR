# show popup when the user clicks the Help button
observeEvent(input$importHelp,
             {
               
               showModal(modalDialog(
                 title = h3("Help: Import Tab"),easyClose = TRUE,
                 h4(strong("Why would I use this tab?")),
                 "Upload x3p files to the app.",
                 br(),
                 br(),
                 h4(strong("What do I need to do before using this tab?")),
                 "Download cartridge case scans to your computer as x3p files.",
                 "The NIST Ballistics Toolmark Research Database contains examples of x3p files: ",tags$a(href = "https://tsapps.nist.gov/NRBTD/Studies/Search",
                                                                                                          "https://tsapps.nist.gov/NRBTD/Studies/Search",
                                                                                                          target = "_blank"),
                 "Alternatively, this application comes pre-loaded with scans in the 'app_x3pScans' folder for demonstration.",
                 br(),
                 br(),
                 h4(strong("How do I use this tab?")),
                 HTML(paste0("Click the ",strong('Select a folder containing x3p files')," button to upload x3p scans to the app.")),
                 br(),
                 br(),
                 h4(strong("What is next?")),
                 HTML(paste0("Click the ",strong('I would like to compare these scans')," button if the scans are already pre-processed to your liking to move on to the next stages.")),
                 HTML(paste0("Otherwise, click the ",strong('Needs Automatic Pre-processing')," button if all of the scans require the same pre-processing or ",strong('Needs Manual Pre-processing')," if specific scans require pre-processing."))
               ))
               
             })

observeEvent(input$x3pdir, {
  
  if(!is.integer(input$x3pdir)) {
    dir <- shinyFiles::parseDirPath(volumes, input$x3pdir)
    
    # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Loading x3p files...")
    
    # initialize shiny.r
    shiny.r$data <<- try(bulletxtrctr::read_bullet(dir))
    dataPar <<- try(data_CheckPar(isolate(shiny.r$data)))
    shiny.r$data$type <<- 'NA'
    shiny.r$data$comments <<- ''
    
    # # shinybusy::remove_modal_spinner()
    
    output$x3pdir_prompt <- renderText({
      validate(need(shiny.r$data, message = "No x3p files found in this directory!"))
      paste("finished loading x3p files.")
    })
    
    
    NOSHINY_TT <<- FALSE
  } 
})

# once a folder of x3p files is selected, update a bunch of stuff including:
# - plot the scans in the first tab
# - print the dimensions of the scans
# - update the Initial X3P select inputs to reflect the choices of x3p
# - update the referenceSelect and targetSelect selectInputs in the Comparison Parameters tab
observeEvent(input$x3pdir,
             {
               req(shiny.r$data)
               req(nrow(shiny.r$data) > 0)
               req(input$x3pdir)
               tmp <- isolate(shiny.r$data)
               
               tmp <- tmp %>%
                 mutate(x3p = map(x3p,~ {
                   
                   if(max(abs(.$surface.matrix),na.rm = TRUE) < 1e-4){
                     
                     .$surface.matrix <- .$surface.matrix*1e6
                     
                   }
                   
                   return(.)
                   
                 }),
                 # x3pName = paste0("x3p",1:nrow(.)),
                 x3pName = source %>% 
                   stringr::str_split("/") %>% 
                   map_chr(~ str_remove(.[length(.)],"\\.x3p")),
                 x3p_processed = x3p,
                 autoPreprocessPerformed = FALSE)
               
               shiny.r$data <<- tmp
               
               output$infoInitX3P <- renderText({
                 
                 purrr::map2_chr(tmp$x3p,1:length(tmp$x3p),
                                 ~ {
                                   
                                   paste0("x3p",.y,
                                          " has ",nrow(.x$surface.matrix)," rows and ",
                                          ncol(.x$surface.matrix)," columns")
                                   
                                 }) %>%
                   paste0(collapse = "\n")
                 
               })
               
               output$pltInitX3P <- renderPlot(bg = "white",{
                 
                 cmcR::x3pListPlot(tmp$x3p %>% set_names(tmp$x3pName))
                 
               }) 
               
               updateSelectInput(session = session,
                                 inputId = "manualDeletionSelection",
                                 choices = c("",tmp$x3pName))
               
               updateSelectInput(session = session,
                                 inputId = "referenceSelect",
                                 choices = c("",tmp$x3pName))
               
               updateSelectInput(session = session,
                                 inputId = "targetSelect",
                                 choices = c("",tmp$x3pName))
               
               updateSelectInput(session = session,
                                 inputId = "customCellSelection",
                                 choices = c("",tmp$x3pName))
               
               updateSelectInput(session = session,
                                 inputId = "targetSelect_customCell",
                                 choices = c("",tmp$x3pName))
               
               updateSelectInput(session = session,
                                 inputId = "score_referenceSelect",
                                 choices = c("",tmp$x3pName))
               
               updateSelectInput(session = session,
                                 inputId = "score_targetSelect",
                                 choices = c("",tmp$x3pName))
               
               # add a menu below the import plot allowing the user to select
               # the preprocessing steps they would like to use
               # output$preprocessSelection_ui <- renderUI({
               #   
               #   box(width = 4,
               #       radioButtons(inputId = "preprocessRequired",
               #                    label = "These scans...",
               #                    choices = c("do not need any further preprocessing",
               #                                "require additional preprocessing"),
               #                    selected = character(0)),
               #       conditionalPanel(condition = 'input.preprocessRequired == "require additional preprocessing"',
               #                        checkboxGroupInput(inputId = "preprocessStepsNeeded",
               #                                           label = "The preprocessing steps I need to apply include...",
               #                                           choices = c("automatic preprocessing of all scans",
               #                                                       "manual preprocessing of specific scans to remove particular regions"))),
               #       uiOutput(outputId = "preprocessRequiredMessage",container = shiny::em))
               #   
               # })
               # 
               # output$preprocessRequiredMessage <- renderText({
               #   
               #   req(shiny.r$data)
               #   req(nrow(shiny.r$data) > 0)
               #   req(input$preprocessRequired)
               #   
               #   return(case_when(
               #     (input$preprocessRequired == "require additional preprocessing" & "automatic preprocessing of all scans" %in% input$preprocessStepsNeeded & "manual preprocessing of specific scans to remove particular regions" %in% input$preprocessStepsNeeded) ~ "You may proceed to the Automatic Pre-process tab followed by the Manual Deletion tab.",
               #     (input$preprocessRequired == "require additional preprocessing" & "automatic preprocessing of all scans" %in% input$preprocessStepsNeeded & !("manual preprocessing of specific scans to remove particular regions" %in% input$preprocessStepsNeeded)) ~ "You may proceed to the Automatic Pre-process tab followed by the Comparing step",
               #     (input$preprocessRequired == "require additional preprocessing" & !("automatic preprocessing of all scans" %in% input$preprocessStepsNeeded) &  "manual preprocessing of specific scans to remove particular regions" %in% input$preprocessStepsNeeded) ~ "You may proceed to the Manual Deletion tab followed by the Comparing step",
               #     (input$preprocessRequired == "do not need any further preprocessing") ~ "You may proceed to the Comparing step",
               #     TRUE ~ "Select one or more preprocessing procedures"))
               #   
               # })
               
             })

###############################################################

# Tab transitions:

# after importing data, render buttons to move to the next steps
observeEvent(input$x3pdir,{
  
  output$import_nextStep_ui <- renderUI({
    box(width = 12,align = "center",
        h4("Choose one of the following"),
        br(),
        actionButton(inputId = "import_goToAutomaticPreprocess",
                     label = "Needs Automatic Pre-processing",
                     fontawesome::fa_i("fas fa-cut"),
                     width = 300),
        br(),
        br(),
        actionButton(inputId = "import_goToManualPreprocess",
                     label = "Needs Manual Pre-processing",
                     icon = fontawesome::fa_i("hand-scissors"),
                     width = 300),
        br(),
        br(),
        actionButton(inputId = "import_goToComparison",
                     label = "I would like to compare these scans",
                     style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                     icon = icon("pencil-ruler"),
                     width = 300))
  })
  
})

# If "Looks Good!" button is selected, then move onto the Compare Stage

observeEvent(input$import_goToComparison,{
  
  hideTab(inputId = "preprocessingTabs","Import",session = session)
  hideTab(inputId = "preprocessingTabs","Manual Deletion - Select a Scan",session = session)
  hideTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
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
  
  shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")
  
  # updateTabItems(session = session,inputId = "stages",selected = "comparing")
  updateTabsetPanel(session = session,"preprocessingTabs",selected = "Pre-processing Completed")
  
})

# if the "Needs Automatic Pre-processing" button is selected, then move to the AUtomatic Preprocess tab

observeEvent(input$import_goToAutomaticPreprocess,{
  
  hideTab(inputId = "preprocessingTabs","Import",session = session)
  hideTab(inputId = "preprocessingTabs","Manual Deletion - Select a Scan",session = session)
  showTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
  
  updateTabsetPanel(session = session,"preprocessingTabs",selected = "Automatic Pre-process")
  
})

# of the "Needs Manual Preprocessing" button is selected, then move to the Manual Preprocessing tab

# if the "Needs Automatic Pre-processing" button is selected, then move to the AUtomatic Preprocess tab

observeEvent(input$import_goToManualPreprocess,{
  
  hideTab(inputId = "preprocessingTabs","Import",session = session)
  hideTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
  showTab(inputId = "preprocessingTabs",target = "Manual Deletion - Select a Scan",session = session)
  
  updateTabsetPanel(session = session,"preprocessingTabs",selected = "Manual Deletion - Select a Scan")
  
})


###################################################

# Old checkbox code:

# reset the checkboxes if the user decides that they do not need to apply
# preprocessing
# observeEvent(input$preprocessRequired == "do not need any further preprocessing",{
#   
#   req(shiny.r$data)
#   req(nrow(shiny.r$data) > 0)
#   
#   if(!is.null(input$preprocessRequired)){
#     
#     if(input$preprocessRequired  == "do not need any further preprocessing"){
#       
#       hideTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
#       hideTab(inputId = "preprocessingTabs",target = "Manual Deletion",session = session)
#       
#       updateCheckboxGroupInput(inputId = "preprocessStepsNeeded",selected = FALSE)
#       
#     }
#   }
#   
# })
# 
# 
# observeEvent(input$preprocessStepsNeeded,ignoreNULL = FALSE,{
#   
#   req(shiny.r$data)
#   req(nrow(shiny.r$data) > 0)
#   
#   hideTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
#   hideTab(inputId = "preprocessingTabs",target = "Manual Deletion",session = session)
#   
#   if(!is.null(input$preprocessRequired)){
#     
#     if("automatic preprocessing of all scans" %in% input$preprocessStepsNeeded){
#       
#       showTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
#       
#     }
#     
#     if("manual preprocessing of specific scans to remove particular regions" %in% input$preprocessStepsNeeded){
#       
#       showTab(inputId = "preprocessingTabs",target = "Manual Deletion")
#       
#     }
#     
#   }
#   
# })