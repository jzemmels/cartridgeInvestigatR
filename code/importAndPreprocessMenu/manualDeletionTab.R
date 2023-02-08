####################################### Code for Manual Deletion tab

# show popup when the user clicks the Help button
observeEvent(input$manualDeletionHelp,
             {
               
               showModal(modalDialog(
                 title = h3("Help: Manual Deletion Tab"),
                 easyClose = TRUE,
                 h4(strong("Why would I use this tab?")),
                 "Manually delete regions from a scan.",
                 br(),
                 br(),
                 h4(strong("What do I need to do before using this tab?")),
                 "Import scans to the app",
                 br(),
                 br(),
                 h4(strong("How do I use this tab?")),
                 tags$ol(tags$li("Select a scan using 'Select an x3p to annotate'"),
                         tags$li(HTML(paste0("Press ",strong('Next: Start Annotating')," to being annotating the scan."))),
                         tags$li("Left-click on the plot to place a point. Place three or more points to create a connected region."),
                         tags$li(HTML(paste0("Click ",strong('Confirm Current Region')," if you are happy the region you've created."))),
                         tags$li(HTML(paste0("Click ",strong('Reset Current Region')," to remove all points."))),
                         tags$li("Start a new region by drawing a new rectangle on the plot of the full scan at the top of the page."),
                         tags$li(HTML(paste0("If you would like to start over, click ",strong('Choose a Different Scan'),"."))),
                         tags$li(HTML(paste0("Once you are happy with the annotated regions, click ",strong('Next: Preview Annotations')," to preview the deletions. The plot on the right shows the selected scan with the annotated regions removed."))),
                         tags$li(HTML(paste0("Click ",strong("I'm Happy with these Annotations")," if you are happy with the annotations. Otherwise, click ",strong("Choose a Different Scan")," to start over."))),
                         tags$li("Return to step 1. to select a new scan or continue on to the next stages.")
                 ),
                h4("What is next?"),
                HTML(paste0("Click ",strong("I would like to compare these scans")," once you are happy with the manual annotations to move onto the next stages."))
               ))
               
             })

# the following code controls the workflow when clicking the various buttons

hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Annotate Scan",session = session)
hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Preview Final Annotations",session = session)

observeEvent(input$manualDeletion_selectToAnnotate,{
  
  showTab(inputId = "preprocessingTabs",target = "Manual Deletion - Annotate Scan",select = TRUE,session = session)
  
  hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Select a Scan",session = session)
  
})

observeEvent(input$manualDeletion_annotateRestart,{
  
  # shinyjs::disable("manualDeletion_selectToAnnotate")
  
  showTab(inputId = "preprocessingTabs",target = "Manual Deletion - Select a Scan",select = TRUE,session = session)
  
  hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Annotate Scan",session = session)
  
  shinyjs::hide("manualDeletion_annotateToConfirmation",anim = TRUE)
  
  plottedPoints$dat <<- plottedPoints_blank
  
  # output$manualDeletion_annotatePlot <- NULL
  # output$manualDeletion_annotatePlot_zoom <- renderPlot(NULL)
  output$manualDeletion_annotateOptions <- renderUI(NULL)
  # output$x3p1_selectedPolygons <- renderTable(NULL)
  
})

observeEvent(input$manualDeletion_annotateToConfirmation,{
  
  showTab(inputId = "preprocessingTabs",target = "Manual Deletion - Preview Final Annotations",select = TRUE,session = session)
  
  hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Annotate Scan",session = session)
  
  # unload these plots since they're not needed anymore
  # output$manualDeletion_annotatePlot <- renderPlot({
  #   
  #   ggplot(data.frame(x=1))
  #   
  # })
  # 
  # output$manualDeletion_annotatePlot_zoom <- renderPlot({
  #   
  #   ggplot(data.frame(x=1))
  #   
  # })
  
})

observeEvent(input$manualDeletion_previewRestart,{
  
  showTab(inputId = "preprocessingTabs",target = "Manual Deletion - Select a Scan",select = TRUE,session = session)
  
  hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Preview Final Annotations",session = session)
  
  plottedPoints$dat <<- plottedPoints_blank
  
  # unload these plots since they're not needed anymore
  # output$manualDeletion_annotatePlot <- renderPlot({
  #   
  #   ggplot(data.frame(x=1))
  #   
  # })
  # 
  # output$manualDeletion_annotatePlot_zoom <- renderPlot({
  #   
  #   ggplot(data.frame(x=1))
  #   
  # })
  
})

observeEvent(input$manualDeletion_confirmAnnotations,{
  
  showTab(inputId = "preprocessingTabs",target = "Manual Deletion - Select a Scan",select = TRUE,session = session)
  
  hideTab(inputId = "preprocessingTabs",target = "Manual Deletion - Preview Final Annotations",session = session)
  
  # unload these plots since they're not needed anymore
  
  # output$manualDeletion_annotatePlot <- renderPlot({
  #   
  #   ggplot(data.frame(x=1))
  #   
  # })
  # 
  # output$manualDeletion_annotatePlot_zoom <- renderPlot({
  #   
  #   ggplot(data.frame(x=1))
  #   
  # })
  
  # updateSelectInput(session = session,inputId = "manualDeletionSelection",selected = "")
  
})

# show the user-selected scan in the Select a Scan tab
output$manualDeletion_selectPlot <- renderPlot(bg = "white",
                                               width = 750,
                                               height= 750,{
                                                 
                                                 req(shiny.r$data)
                                                 req(nrow(shiny.r$data) > 0)
                                                 tmp <- isolate(shiny.r$data)
                                                 
                                                 
                                                 if(input$manualDeletionSelection != ""){
                                                   
                                                   x3pManualPlt <- x3pListPlot(list(tmp %>%
                                                                                      filter(x3pName == input$manualDeletionSelection) %>%
                                                                                      pull(x3p_processed) %>%
                                                                                      .[[1]]) %>%
                                                                                 set_names(input$manualDeletionSelection))
                                                   
                                                   return(x3pManualPlt)
                                                   
                                                 }
                                                 
                                               })

observeEvent(input$manualDeletionSelection,{
  
  req(input$manualDeletionSelection != "")
  
  shinyjs::show("manualDeletion_selectToAnnotate",anim = TRUE)
  shinyjs::hide("manualDeletion_annotateToConfirmation")
  shinyjs::disable("manualDeletionSelection_gray")
  shinyjs::disable("manualDeletionSelection_gray2")
  
  # these will act as "dummy" selectInputs that just remind the user which scan
  # they selected
  updateSelectInput(session = session,inputId = "manualDeletionSelection_gray",
                    choices = input$manualDeletionSelection)
  updateSelectInput(session = session,inputId = "manualDeletionSelection_gray2",
                    choices = input$manualDeletionSelection)
  
})

# initialize and empty df to store manually annotated points
plottedPoints_blank <- data.frame(x = numeric(0),
                                  y = numeric(0),
                                  group = numeric(0),
                                  pointNum = numeric(0),
                                  x3p = character(0))
plottedPoints <- reactiveValues(dat = plottedPoints_blank)

# visualize the full scan along with any manual annotations
output$manualDeletion_annotatePlot <- renderPlot(bg = "white",{
  
  req(shiny.r$data)
  req(nrow(shiny.r$data) > 0)
  
  req(input$manualDeletionSelection != "")
  tmp <- isolate(shiny.r$data)
  
  x3pManualPlt <<- x3pListPlot(list(tmp %>%
                                      filter(x3pName == input$manualDeletionSelection) %>%
                                      pull(x3p_processed) %>%
                                      .[[1]]) %>%
                                 set_names(input$manualDeletionSelection)) +
    geom_point(data = plottedPoints$dat %>%
                 filter(x3p == input$manualDeletionSelection),
               aes(x=x,y=y)) +
    geom_polygon(data = plottedPoints$dat %>%
                   filter(x3p == input$manualDeletionSelection),
                 aes(x=x,y=y,group = group),inherit.aes = FALSE,fill = "black",alpha = .2)
  
  return(x3pManualPlt)
  
})

# show message once an x3p has been selected
observeEvent(input$manualDeletionSelection,{
  
  req(input$manualDeletionSelection != "")
  req(input$manualDeletionSelection)
  
  # if needed, reset the ui elements below the selected scan
  # output$editPolygonSelect_ui <- NULL
  output$regionClickMessage <- NULL
  output$regionResetButton_ui <- NULL
  # output$newRegionMessage <- NULL
  
  # output$manualDeletion_annotatePlot_zoom <- NULL
  # output$x3p1_selectedPolygons <- NULL
  
  output$annotationConfirmationButton_ui <- NULL
  output$confirmationMessage <- NULL
  
  # output$deleteAnnotationsButton_ui <- NULL
  output$postConfirmationMessage <- NULL
  output$deleteAnnotationsMessage <- NULL
  
  selectedZoom <<- reactiveValues(dat = data.frame(x3p = character(0),
                                                   group = numeric(0),
                                                   xmin = numeric(0),
                                                   xmax = numeric(0),
                                                   ymin = numeric(0),
                                                   ymax = numeric(0)))
  
  numRegions$val <<- 0
  
})

# numRegions <- 0

numRegions <- reactiveValues(val = 0)

# we will be jumping back-and-forth between annotated regions, so we will
# create a globally-available data frame to update the zoomed-in region
# as-needed.
selectedZoom <- reactiveValues(dat = data.frame(x3p = character(0),
                                                group = numeric(0),
                                                xmin = numeric(0),
                                                xmax = numeric(0),
                                                ymin = numeric(0),
                                                ymax = numeric(0)))

# once the user zooms-into the scan, initialize a new region to annotate
observeEvent(input$manualProcZoom,{
  
  req(plottedPoints$dat)
  req(input$manualProcZoom$xmin)
  req(!is.null(input$manualProcZoom$xmin))
  
  pts <- isolate(plottedPoints$dat)
  
  pts <- pts %>%
    filter(x3p == input$manualDeletionSelection)
  
  # the user may decide to redraw a rectangle without creating any points. In
  # this situation, we don't want to create a new region -- this if block
  # ensure this
  redrawnRectangle <- FALSE
  
  # we only need to update the number of regions if the user has selected a
  # different zoom window. For some reason, either the manualProcZoom or
  # selectedZoom$dat values sometimes change by a small value - on the scale
  # of 1e-11, so we'll compare the values on a rounded scale. If the window
  # hasn't actually changed, then this should break it out of the observeEvent
  # call
  if(nrow(selectedZoom$dat) > 0){
    
    req(abs(input$manualProcZoom$xmin - unique(selectedZoom$dat$xmin[numRegions$val])) > .001 |
          abs(input$manualProcZoom$xmax - unique(selectedZoom$dat$xmax[numRegions$val])) > .001 | 
          abs(input$manualProcZoom$ymin - unique(selectedZoom$dat$ymin[numRegions$val])) > .001 | 
          abs(input$manualProcZoom$ymax - unique(selectedZoom$dat$ymax[numRegions$val])) > .001)
    
  }
  
  numRegions$val <<- numRegions$val + 1
  
  output$manualDeletion_annotateOptions <- renderUI({
    
    return(list(
      h4("On the zoomed in plot that appears, left-click to place points. Place three or more points to form an annotated region to be removed from the scan."),
      h4('Once you are happy with the region, click "Confirm Current Region." Start a new region by drawing a new rectangle on the scan plot.'),
      h4('Press "Next: Preview Annotations" when you are finished annotating.'),
      actionButton(inputId = "newManualRegion",
                   icon = fontawesome::fa_i("check"),
                   style="color: #000; background-color: #fff; border-color: #95bb72",
                   label = "Confirm Current Region"),
      actionButton("regionResetButton",
                   icon = fontawesome::fa_i("trash"),
                   style="color: #000; background-color: #fff; border-color: #ff4122",
                   label = "Reset Current Region")))
    
  })
  
  # visualize the zoomed-in region
  pts_filtered <- pts %>%
    filter(group == numRegions$val)
  
  # update the zoom data frame in case the user draws a new rectangle
  zoom <- isolate(selectedZoom$dat)
  zoom <- bind_rows(zoom,
                    data.frame(x3p = input$manualDeletionSelection,
                               group = as.numeric(numRegions$val),
                               xmin = input$manualProcZoom$xmin,
                               xmax = input$manualProcZoom$xmax,
                               ymin = input$manualProcZoom$ymin,
                               ymax = input$manualProcZoom$ymax)) %>%
    distinct()
  
  # we may need to toss out the last row if the user hasn't actually updated
  # the rectangle
  if(nrow(zoom) > {zoom %>% dplyr::select(-'group') %>% distinct() %>% nrow()}){
    
    zoom <- zoom %>% slice(1:(nrow(zoom) - 1))
    
  }
  
  selectedZoom$dat <<- zoom
  
  output$x3p1_selectedPolygons <-
    shiny::renderTable({
      
      req(plottedPoints$dat)
      # req(input$editPolygonSelect)
      
      pts <- isolate(plottedPoints$dat)
      
      ret <- pts %>% 
        filter(x3p == input$manualDeletionSelection & group == numRegions$val) %>% #& group == input$editPolygonSelect) %>%
        mutate(x = as.integer(round(x)),
               y = as.integer(round(y)),
               pointNum = as.integer(pointNum),
               group = factor(group)) %>%
        rename(Scan = x3p,
               # `Region Number` = group,
               Point = pointNum,
               Column = x,
               Row = y) %>%
        dplyr::select(c('Scan',
                        # `Region Number`,
                        'Point','Column','Row'))
      
      return(ret)
      
    })
  
  # once the manualProcZoom variable is updated, visualize the zoomed-in part. 
  output$manualDeletion_annotatePlot_zoom <- renderPlot(bg = "white",{
    
    req(plottedPoints$dat)
    req(input$manualDeletionSelection != "")
    req(input$manualProcZoom)
    # req(input$editPolygonSelect)
    
    pts <- isolate(plottedPoints$dat)
    
    pts <- pts %>%
      filter(x3p == input$manualDeletionSelection)
    
    pts_filtered <- pts %>%
      filter(group == numRegions$val)#input$editPolygonSelect)
    
    zoom <- isolate(selectedZoom$dat)
    
    zoom <- zoom %>%
      filter(group == numRegions$val)#input$editPolygonSelect)
    
    req(nrow(zoom) == 1)
    
    x3pManualPlt_zoom <- x3pManualPlt +
      coord_fixed(xlim = c(unique(zoom$xmin),unique(zoom$xmax)),
                  ylim = c(unique(zoom$ymin),unique(zoom$ymax)),
                  expand = FALSE,
                  ratio = 1) +
      geom_point(data = pts_filtered,
                 aes(x=x,y=y)) +
      geom_polygon(data = pts_filtered,
                   aes(x=x,y=y,group = group),inherit.aes = FALSE,fill = "black",alpha = .2) +
      ggrepel::geom_text_repel(data = pts_filtered,
                               aes(x=x,y=y,label = pointNum),inherit.aes = FALSE)
    
    return(x3pManualPlt_zoom)
    
  })
})

observeEvent(input$newManualRegion,{
  
  req(plottedPoints$dat)
  req(nrow(plottedPoints$dat) > 1)
  
  shinyjs::show(id = "manualDeletion_annotateToConfirmation",anim = TRUE)
  
  output$manualDeletion_annotatePlot_zoom <- renderPlot(bg = "white",{
    
    ggplot(data.frame(x=1))
    
  })
  
  output$x3p1_selectedPolygons <- renderTable({
    
    data.frame(Scan = NULL,Point = NULL,Column = NULL,Row = NULL)
    
  })
  
  output$regionResetButton_ui <- NULL
  
})

# update selected points if click happens on zoomed plot
observeEvent(input$pointPlacement_zoom,{
  
  pts <- isolate(plottedPoints$dat)
  
  pts_filtered <- pts %>%
    filter(x3p == input$manualDeletionSelection & group == numRegions$val)#& group == input$editPolygonSelect)
  
  pts <- bind_rows(pts,
                   data.frame(x = input$pointPlacement_zoom$x,
                              y = input$pointPlacement_zoom$y,
                              group = numRegions$val,
                              # group = as.numeric(input$editPolygonSelect),
                              pointNum =ifelse(nrow(pts_filtered) == 0,1,max(pts_filtered$pointNum) + 1),
                              x3p = input$manualDeletionSelection) %>%
                     arrange(x3p,group,pointNum))
  
  plottedPoints$dat <<- pts
  
})

# if the reset button is selected, update plottedPoints$dat object having
# removed all of the rows associated with the currently selected x3p & region
observeEvent(input$regionResetButton,{
  
  pts <- isolate(plottedPoints$dat)
  
  pts <- pts %>%
    filter(x3p == input$manualDeletionSelection & !(group == numRegions$val)) #& !(group == input$editPolygonSelect))
  
  plottedPoints$dat <<- pts
  
})

# preview what the deleted scan will look like before pressing the
# confirmation button
output$manualDeletion_deletionPreview_plot <- renderPlot(bg = "white",width = 1000,height = 500,{
  
  req(shiny.r$data)
  req(nrow(shiny.r$data) > 0)
  req(plottedPoints$dat)
  
  tmp <- isolate(shiny.r$data)
  
  x3p <- tmp %>%
    filter(x3pName == input$manualDeletionSelection) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  library(rgl)
  
  # filter-down to the annotations made for the selected scan only
  pts <- plottedPoints$dat %>%
    filter(x3p == input$manualDeletionSelection)
  
  x3pDat <- x3p %>%
    impressions::x3p_to_dataFrame()
  
  x3pDatPolyMembership <- pts %>%
    group_by(group) %>%
    group_split() %>%
    map_dfr(function(dat){
      
      x3pDat %>%
        mutate(inPoly = sp::point.in.polygon(x,y,dat$x,dat$y)) %>%
        filter(inPoly > 0) %>%
        mutate(group = unique(dat$group))
      
    })
  
  x3pDat <- x3pDat %>%
    left_join(x3pDatPolyMembership %>%
                dplyr::select('x','y','inPoly'),
              by = c('x','y')) %>%
    mutate(fillColor = ifelse(is.na(inPoly),"#e6bf98","#ff0000"))
  
  # create a surface matrix to be rendered in 3D and a companion color
  # matrix dictating the color of each element. For the sake of rendering
  # the 3D visualization in the same way as x3pListPlot, we populate the
  # matrix byrow
  surface <- matrix(x3pDat$value,nrow = nrow(x3p$surface.matrix),byrow = TRUE)
  colorSurface <- matrix(x3pDat$fillColor,nrow = nrow(x3p$surface.matrix),byrow = TRUE)
  
  # now that we've calculated the correct mask, we'll delete the
  # user-selected mask from the surface values x3p
  x3pMasked <- x3p
  # the colorSurface matrix technically needs to be transposed to "agree"
  # with the actual surface matrix
  x3pMasked <- x3ptools::x3p_add_mask(x3pMasked,t(colorSurface))
  
  # delete the masked values from the surface matrix
  x3pMasked <- x3pDeleteMask(x3pMasked)
  
  x3pMasked$mask <- NULL
  
  # return a ggplot of the mask-deleted x3p file
  return(x3pListPlot(list(x3p,x3pMasked) %>% 
                       set_names(paste0(input$manualDeletionSelection,", Original"),paste0(input$manualDeletionSelection,", Manual Deletion Preview"))))
  
})

# when the user wants to confirm the annotations. we'll delete the regions
# from the associated scan and then re-write the x3p_processed column with the
# deleted scan
observeEvent(input$manualDeletion_confirmAnnotations,{
  
  req(shiny.r$data)
  req(nrow(shiny.r$data) > 0)
  req(plottedPoints$dat)
  
  tmp <- isolate(shiny.r$data)
  
  x3p <- tmp %>%
    filter(x3pName == input$manualDeletionSelection) %>%
    pull(x3p_processed) %>%
    .[[1]]
  
  library(rgl)
  
  # filter-down to the annotations made for the selected scan only
  pts <- plottedPoints$dat %>%
    filter(x3p == input$manualDeletionSelection)
  
  x3pDat <- x3p %>%
    impressions::x3p_to_dataFrame()
  
  x3pDatPolyMembership <- pts %>%
    group_by(group) %>%
    group_split() %>%
    map_dfr(function(dat){
      
      x3pDat %>%
        mutate(inPoly = sp::point.in.polygon(x,y,dat$x,dat$y)) %>%
        filter(inPoly > 0) %>%
        mutate(group = unique(dat$group))
      
    })
  
  x3pDat <- x3pDat %>%
    left_join(x3pDatPolyMembership %>%
                dplyr::select('x','y','inPoly'),
              by = c('x','y')) %>%
    mutate(fillColor = ifelse(is.na(inPoly),"#e6bf98","#ff0000"))
  
  # create a surface matrix to be rendered in 3D and a companion color
  # matrix dictating the color of each element. For the sake of rendering
  # the 3D visualization in the same way as x3pListPlot, we populate the
  # matrix byrow
  surface <- matrix(x3pDat$value,nrow = nrow(x3p$surface.matrix),byrow = TRUE)
  colorSurface <- matrix(x3pDat$fillColor,nrow = nrow(x3p$surface.matrix),byrow = TRUE)
  
  # now that we've calculated the correct mask, we'll delete the
  # user-selected mask from the surface values x3p
  x3pMasked <- x3p
  # the colorSurface matrix technically needs to be transposed to "agree"
  # with the actual surface matrix
  x3pMasked <- x3ptools::x3p_add_mask(x3pMasked,t(colorSurface))
  
  # delete the masked values from the surface matrix
  x3pMasked <- x3pDeleteMask(x3pMasked)
  
  # x3pMasked$mask <- NULL
  
  # save the masked data out to a file
  # x3ptools::write_x3p(x3pMasked,
  #                     file = paste0("data/User_scans/",input$manualDeletionSelection,"_user",session$user,
  #                                   # "_time",str_replace_all(str_replace_all(str_squish(Sys.time()),":","-")," ","_"),
  #                                   ".x3p"))
  
  tmp <- tmp %>%
    mutate(x3p_processed = ifelse(x3pName == input$manualDeletionSelection,
                                  list(x3pMasked),
                                  x3p_processed))
  
  shiny.r$data <<- tmp
  
  output$manualDeletion_selectPlot <- renderPlot(bg = "white",{
    
    req(shiny.r$data)
    req(nrow(shiny.r$data) > 0)
    tmp <- isolate(shiny.r$data)
    
    
    if(input$manualDeletionSelection != ""){
      
      x3pManualPlt <- x3pListPlot(list(tmp %>%
                                         filter(x3pName == input$manualDeletionSelection) %>%
                                         pull(x3p_processed) %>%
                                         .[[1]]) %>%
                                    set_names(input$manualDeletionSelection))
      
      return(x3pManualPlt)
      
    }
    
  })
  
  
})


###############################################################

# Tab transitions:

# If "Looks Good!" button is selected, then move onto the Compare Stage

# If "Looks Good!" button is selected, then move onto the Compare Stage

observeEvent(input$manualDeletion_goToComparison,{
  
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


