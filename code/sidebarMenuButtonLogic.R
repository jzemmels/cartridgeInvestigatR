# hard resetting will take you back to the Import tab
observeEvent(input$app_hardReset,{
  
  session$reload()
  
  # shiny.r <<- list()
  # 
  # output$comparing_ui <- NULL
  # output$scoring_ui <- NULL
  # 
  # updateTabItems(session = session,inputId = "stages",selected = "preprocessing")
  # 
  # # help button should show info on the Import tab, by default
  # output$preprocess_help_ui <- renderUI({
  #   
  #   actionButton(inputId = "importHelp",
  #                label = "Help",
  #                icon = fontawesome::fa_i("question-circle"))
  #   
  # })
  # 
  # output$pltInitX3P <- renderPlot(NULL)
  # output$import_nextStep_ui <- renderUI(NULL)
  # 
  # showTab(inputId = "preprocessingTabs",target = "Import",session = session)
  # hideTab(inputId = "preprocessingTabs",target = "Automatic Pre-process",session = session)
  # hideTab(inputId = "preprocessingTabs",target = "Pre-processing Completed",session = session)
  # hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Select a Scan",session = session)
  # hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Annotate",session = session)
  # hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Preview Final Annotations",session = session)
  # hideTab(inputId = "comparingTabs",target = "",session = session)
  
})

