library(patchwork)
library(cmcR)
library(tidyverse)
library(impressions)
library(shinyFiles)
library(shinybusy)
library(gganimate)
library(prettyunits)
library(progress)
library(ggiraph)
# remotes::install_github("https://github.com/thomasp85/shinyFiles.git")

server = function(input, output, session) {
  
  source("cartridgeInvestigatR_helpers.R")
  
  
  # upload from rds file
  volumes <- c(Home = fs::path_wd(), "R Installation" = R.home(), shinyFiles::getVolumes()())
  # shinyFileChoose(input, "file1", roots = volumes, session = session)
  
  ###################################
  ## x3p tag related
  ###################################
  
  shiny.r <- list()
  
  # upload from x3p folder
  shinyFiles::shinyDirChoose(input, "x3pdir", roots = volumes, session = session, 
                             restrictions = system.file(package = "base"))
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
                 
                 output$infoInitX3P <- renderText({
                   
                   purrr::map2_chr(tmp$x3p,1:length(tmp$x3p),
                                   ~ {
                                     
                                     paste0("x3p",.y,
                                            " has ",nrow(.x$surface.matrix)," rows and ",
                                            ncol(.x$surface.matrix)," columns")
                                     
                                   }) %>%
                     paste0(collapse = "\n")
                   
                 })
                 
                 output$pltInitX3P <- renderPlot({
                   
                   cmcR::x3pListPlot(tmp$x3p) + theme(legend.position = "none")
                   
                 }) 
                 
                 updateSelectInput(session = session,
                                   inputId = "x3prgl1_select",
                                   choices = c("",paste0("x3p",1:nrow(tmp))))
                 
                 updateSelectInput(session = session,
                                   inputId = "referenceSelect",
                                   choices = c("",paste0("x3p",1:nrow(tmp))))
                 
                 updateSelectInput(session = session,
                                   inputId = "targetSelect",
                                   choices = c("",paste0("x3p",1:nrow(tmp))))
                 
                 updateSelectInput(session = session,
                                   inputId = "customCellSelection",
                                   choices = c("",paste0("x3p",1:nrow(tmp))))
                 
                 updateSelectInput(session = session,
                                   inputId = "targetSelect_customCell",
                                   choices = c("",paste0("x3p",1:nrow(tmp))))
                 
               })
  
  
  ####################################### Code for Manual Deletion tab
  
  # initialize and empty df to store manually annotated points
  plottedPoints_blank <- data.frame(x = numeric(0),
                                    y = numeric(0),
                                    group = numeric(0),
                                    pointNum = numeric(0),
                                    x3p = character(0))
  plottedPoints <- reactiveValues(dat = plottedPoints_blank)
  
  # visualize the full scan along with any manual annotations
  output$x3p1_ggplot <- renderPlot({
    
    req(shiny.r$data)
    req(nrow(shiny.r$data) > 0)
    req(input$x3prgl1_select != "")
    tmp <- isolate(shiny.r$data)
    
    # pts <- isolate(plottedPoints$dat)
    
    x3pManualPlt <<- x3pListPlot(list(tmp %>%
                                        mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
                                        filter(x3pName == input$x3prgl1_select) %>%
                                        pull(x3p) %>%
                                        .[[1]]) %>%
                                   set_names(input$x3prgl1_select)) +
      geom_point(data = plottedPoints$dat %>%
                   filter(x3p == input$x3prgl1_select),
                 aes(x=x,y=y)) +
      geom_polygon(data = plottedPoints$dat %>%
                     filter(x3p == input$x3prgl1_select),
                   aes(x=x,y=y,group = group),inherit.aes = FALSE,fill = "black",alpha = .2)
    
    return(x3pManualPlt)
    
  })
  
  # show message once an x3p has been selected
  observeEvent(input$x3prgl1_select,{
    
    req(input$x3prgl1_select != "")
    req(input$x3prgl1_select)
    
    output$zoomInMessage <- renderText({
      
      return("Zoom-in by drawing a rectangle on the scan")
      
    })
    
    # if needed, reset the ui elements below the selected scan
    output$editPolygonSelect_ui <- NULL
    output$regionClickMessage <- NULL
    output$regionResetButton_ui <- NULL
    output$newRegionMessage <- NULL
    
    # output$x3p1_ggplot_zoom <- NULL
    # output$x3p1_selectedPolygons <- NULL
    
    output$annotationConfirmationButton_ui <- NULL
    output$confirmationMessage <- NULL
    
    # output$deleteAnnotationsButton_ui <- NULL
    output$postConfirmationMessage <- NULL
    output$deleteAnnotationsMessage <- NULL
    
    output$x3p1_rgl <- NULL
    
    selectedZoom <<- reactiveValues(dat = data.frame(x3p = character(0),
                                                     group = character(0),
                                                     xmin = numeric(0),
                                                     xmax = numeric(0),
                                                     ymin = numeric(0),
                                                     ymax = numeric(0)))
    
    numRegions$val <<- 0
    
    
    session$sendCustomMessage(type = "resetValue",message = "editPolygonSelect")
  })
  
  # numRegions <- 0
  
  numRegions <- reactiveValues(val = 0)
  
  # we will be jumping back-and-forth between annotated regions, so we will
  # create a globally-available data frame to update the zoomed-in region
  # as-needed.
  selectedZoom <- reactiveValues(dat = data.frame(x3p = character(0),
                                                  group = character(0),
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
      filter(x3p == input$x3prgl1_select)
    
    # browser()
    
    # the user may decide to redraw a rectangle without creating any points. In
    # this situation, we don't want to create a new region -- this if block
    # ensure this
    redrawnRectangle <- FALSE
    
    if(numRegions$val > 0){
      
      pts_lastGroup <- pts %>%
        filter(group == numRegions$val)
      
      if(nrow(pts_lastGroup) > 0){
        
        numRegions$val <<- numRegions$val + 1
        
      }
      else{
        
        # this variable is used when updating the zoom data frame down below
        redrawnRectangle <- TRUE
        
      }
      
    }
    else{
      
      numRegions$val <<- numRegions$val + 1
      
    }
    
    output$editPolygonSelect_ui <- renderUI({
      
      return(selectInput(inputId = "editPolygonSelect",
                         label = "Select Region to Edit",
                         choices = numRegions$val:1))
      
    })
    
    # insert a new message about clicking on the plot
    output$regionClickMessage <- renderText({
      
      return("2. Place a point by clicking on the plot to the right.\nPlace 3 or more points to start a region.")
      
    })
    
    addTooltip(session = session,id = "editPolygonSelect",title = "Swap between annotated regions")
    
    # insert a reset button
    output$regionResetButton_ui <- renderUI({
      
      return(actionButton("regionResetButton",label = "Reset Region"))
      
    })
    
    addTooltip(session = session,id = "regionResetButton",title = "Reset your manual annotations for this region")
    
    # visualize the zoomed-in region
    pts_filtered <- pts %>%
      filter(group == input$editPolygonSelect)
    
    output$annotationConfirmationButton_ui <- renderUI({
      
      return(actionButton("x3prgl_execute","3. Confirm Annotations"))
      
    })
    addTooltip(session = session,id = "x3prgl_execute",title = "Confirm annotations for selected x3p")
    
    output$deleteAnnotationsButton_ui <- renderUI({
      
      actionButton("deleteAnnotationsButton","Delete Annotations")
      
    })
    
    if(!is.null(input$editPolygonSelect)){
      
      zoom <- isolate(selectedZoom$dat)
      
      # if the user redrew the rectangle without creating a new region, then
      # we'll take the most recent values in input$manualProcZoom
      if(redrawnRectangle){
        
        zoom <- zoom %>%
          mutate(xmin = ifelse(x3p == input$x3prgl1_select & group == input$editPolygonSelect,input$manualProcZoom$xmin,xmin),
                 xmax = ifelse(x3p == input$x3prgl1_select & group == input$editPolygonSelect,input$manualProcZoom$xmax,xmax),
                 ymin = ifelse(x3p == input$x3prgl1_select & group == input$editPolygonSelect,input$manualProcZoom$ymin,ymin),
                 ymax = ifelse(x3p == input$x3prgl1_select & group == input$editPolygonSelect,input$manualProcZoom$ymax,ymax)) %>%
          distinct()
        
      }
      else{
        
        # otherwise, we're dealing with a new region and will update zoom with a
        # new row
        zoom <- bind_rows(zoom,
                          data.frame(x3p = input$x3prgl1_select,
                                     group = input$editPolygonSelect,
                                     xmin = input$manualProcZoom$xmin,
                                     xmax = input$manualProcZoom$xmax,
                                     ymin = input$manualProcZoom$ymin,
                                     ymax = input$manualProcZoom$ymax)) %>%
          distinct()
        
      }
      
      selectedZoom$dat <<- zoom
      
    }
  })
  
  observeEvent(input$editPolygonSelect,{
    
    req(input$editPolygonSelect)
    
    zoom <- isolate(selectedZoom$dat)
    
    zoom <- bind_rows(zoom,
                      data.frame(x3p = input$x3prgl1_select,
                                 group = input$editPolygonSelect,
                                 xmin = input$manualProcZoom$xmin,
                                 xmax = input$manualProcZoom$xmax,
                                 ymin = input$manualProcZoom$ymin,
                                 ymax = input$manualProcZoom$ymax)) %>%
      distinct()
    
    selectedZoom$dat <<- zoom
    
  })
  
  output$x3p1_selectedPolygons <-
    shiny::renderTable({
      
      req(plottedPoints$dat)
      req(input$editPolygonSelect)
      
      pts <- isolate(plottedPoints$dat)
      
      ret <- pts %>% 
        filter(x3p == input$x3prgl1_select & group == input$editPolygonSelect) %>%
        mutate(x = as.integer(round(x)),
               y = as.integer(round(y)),
               pointNum = as.integer(pointNum),
               group = factor(group)) %>%
        rename(Scan = x3p,
               `Region Number` = group,
               Point = pointNum,
               Column = x,
               Row = y) %>%
        select(c(Scan,`Region Number`,Point,Column,Row))
      
      return(ret)
      
    })
  
  # once the manualProcZoom variable is updated, visualize the zoomed-in part. 
  output$x3p1_ggplot_zoom <- renderPlot({
    
    req(plottedPoints$dat)
    req(input$x3prgl1_select != "")
    req(input$manualProcZoom)
    req(input$editPolygonSelect)
    
    pts <- isolate(plottedPoints$dat)
    
    # browser()
    
    pts <- pts %>%
      filter(x3p == input$x3prgl1_select)
    
    pts_filtered <- pts %>%
      filter(group == input$editPolygonSelect)
    
    zoom <- isolate(selectedZoom$dat)
    
    zoom <- zoom %>%
      filter(group == input$editPolygonSelect)
    
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
  
  # update selected points if click happens on zoomed plot
  observeEvent(input$pointPlacement_zoom,{
    
    pts <- isolate(plottedPoints$dat)
    
    pts_filtered <- pts %>%
      filter(x3p == input$x3prgl1_select & group == input$editPolygonSelect)
    
    pts <- bind_rows(pts,
                     data.frame(x = input$pointPlacement_zoom$x,
                                y = input$pointPlacement_zoom$y,
                                group = as.numeric(input$editPolygonSelect),
                                pointNum =ifelse(nrow(pts_filtered) == 0,1,max(pts_filtered$pointNum) + 1),
                                x3p = input$x3prgl1_select) %>%
                       arrange(x3p,group,pointNum))
    
    plottedPoints$dat <<- pts
    
    
    # show new message about how to create a new region
    output$newRegionMessage <- renderText({
      
      return("To start a new region, draw a new rectangle on the plot above.")
      
    })
    
    output$confirmationMessage <- renderText({
      
      return("Once you're done annotating, press Confirm Annotations to lock them in and view a 3D rendering of the annotated scan.\nClicking Delete Annotations will clear all annotations made on the selected x3p.")
      
    })
    
  })
  
  # if the reset button is selected, update plottedPoints$dat object having
  # removed all of the rows associated with the currently selected x3p & region
  observeEvent(input$regionResetButton,{
    
    pts <- isolate(plottedPoints$dat)
    
    pts <- pts %>%
      filter(x3p == input$x3prgl1_select & !(group == input$editPolygonSelect))
    
    plottedPoints$dat <<- pts
    
  })
  
  #visualize the x3p file with the manually annotated regions shown as colored
  #points in an rgl window
  observeEvent(input$x3prgl_execute,{
    
    output$x3p1_rgl <- rgl::renderRglwidget({
      
      req(shiny.r$data)
      req(nrow(shiny.r$data) > 0)
      tmp <- isolate(shiny.r$data)
      
      x3p <- tmp %>%
        mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
        filter(x3pName == input$x3prgl1_select) %>%
        pull(x3p) %>%
        .[[1]]
      
      library(rgl)
      
      # filter-down to the annotations made for the selected scan only
      pts <- plottedPoints$dat %>%
        filter(x3p == input$x3prgl1_select)
      
      x3pDat <- x3p %>%
        x3pToDF()
      
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
                    select(x,y,inPoly),
                  by = c('x','y')) %>%
        mutate(fillColor = ifelse(is.na(inPoly),"#e6bf98","#ff0000"))
      
      # create a surface matrix to be rendered in 3D and a companion color
      # matrix dictating the color of each element. For the sake of rendering
      # the 3D visualization in the same way as x3pListPlot, we populate the
      # matrix byrow
      surface <- matrix(x3pDat$value,nrow = nrow(x3p$surface.matrix),byrow = TRUE)
      colorSurface <- matrix(x3pDat$fillColor,nrow = nrow(x3p$surface.matrix),byrow = TRUE)
      
      # now that we've calculated the correct mask, we'll "bake" the
      # user-selected mask into the x3p
      x3pMasked <- x3p
      # the colorSurface matrix technically needs to be transposed to "agree"
      # with the actual surface matrix
      x3pMasked$mask <- t(colorSurface)
      
      tmp <- tmp %>%
        mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
        mutate(x3p = ifelse(x3pName == input$x3prgl1_select,
                            list(x3pMasked),
                            x3p))
      
      shiny.r$data <<- tmp
      
      # now visualize the mask using an rgl device
      z <- 10 * surface
      yidx <- ncol(z):1
      y <- x3p$header.info$incrementY * yidx
      x <- x3p$header.info$incrementX * (1:nrow(z))
      params <- rgl::r3dDefaults
      params$windowRect <- c(40, 125, 40 + c(750,250)[1], 125 + c(750,250)[2])
      params$userMatrix <- diag(c(1, 1, 1, 1))
      params$zoom <- .7
      xyz <- matrix(c(min(y) - diff(range(y)), mean(y), max(z, 
                                                            na.rm = TRUE)), ncol = 3)
      
      open3d(params = params)
      rgl.pop("lights")
      light3d(x = xyz, diffuse = "gray40", specular = "gray40", 
              ambient = "grey10", viewpoint.rel = TRUE)
      light3d(diffuse = "gray20", specular = "gray20")
      surface3d(x, y, z, color = c(colorSurface), back = "fill")
      rgl::rglwidget()
    })
    
    output$postConfirmationMessage <- renderText({
      
      return("Annotations confirmed.\n
             If you would like to annotate another scan, please select a new scan in step 1.\n
             If you're happy with the manual annotations, move onto the Preprocess tab and use the Delete option to remove annotated regions.")
      
    })
    
  })
  
  observeEvent(input$deleteAnnotationsButton,{
    
    tmp <- isolate(shiny.r$data)
    
    # delete the mask added to the x3p
    x3pMask <- tmp %>%
      mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
      filter(x3pName == input$x3prgl1_select) %>%
      pull(x3p) %>%
      .[[1]]
    
    x3pMask$mask <- NULL
    
    tmp <- tmp %>%
      mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
      mutate(x3p = ifelse(x3pName == input$x3prgl1_select,
                          list(x3pMask),
                          x3p))
    
    shiny.r$data <<- tmp
    
    # delete the annotations from plottedPoints data set too
    pts <- isolate(plottedPoints$dat)
    
    pts <- pts %>%
      filter(x3p != input$x3prgl1_select)
    
    plottedPoints$dat <<- pts
    
    output$x3p1_rgl <- NULL
    
    output$deleteAnnotationsMessage <- renderText({
      
      if(!is.null(tmp$x3p_processed)){
        
        return("Annotations have been deleted. You will need to redo the preprocessing in the next tab to restore the annotated regions.")
        
      }
      else{
        return("Annotations have been deleted.")
      }
      
    })
    
  })
  
  
  #################################### Code for Preprocess tab
  
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
        fluidRow(column(4,selectInput(Id()('letter',ctn()), 'Select Preprocessing Step', c("Downsample","Crop","Level","Erode","Filter","Delete"),selected = NULL,multiple = FALSE)),
                 column(8,uiOutput(Id()('input',ctn()))))
      )
    )
    
  })
  
  observeEvent(ctn(), {
    
    ctn <- ctn()
    
    id <- Id()('input',ctn)
    selection <- Id()('letter',ctn)
    
    # 
    
    output[[id]] <- renderUI({
      req(input[[Id()('letter',ctn)]])
      switch(
        input[[selection]],
        'Downsample' = numericInput(Id()('params1_',ctn), 'Stride',value = 2,min = 1),
        'Crop' = column(width = 8,selectInput(Id()('params1_',ctn), 'Region',choices = c("Interior","Exterior")),
                        numericInput(Id()('params2_',ctn), 'Offset',value = 0)),
        'Level' = selectInput(Id()('params1_',ctn),"Statistic",choices = c("Median","Mean")),
        'Erode' = column(width = 8,selectInput(Id()('params1_',ctn), 'Region',choices = c("Interior","Exterior")),
                         numericInput(Id()('params2_',ctn), 'Radius',value = 1,min = 1)),
        'Filter' = column(width = 8,selectInput(Id()('params1_',ctn), 'Filtertype',choices = c("Bandpass","Lowpass")),
                          textInput(Id()('params2_',ctn), 'Wavelength(s) - Use comma separation for Bandpass',value = "16, 500")),
        'Delete' = column(width = 8)
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
    
    # 
    
    # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Preprocessing scans")
    
    # the input object is updated as the user adds more preprocessing steps
    # or changes the selection per step.
    tmpInput <- shiny::reactiveValuesToList(input)
    
    # each preprocessing step is in-order as specified by letter# where # is
    # a number
    preProcessSteps <- tmpInput[stringr::str_detect(names(tmpInput),"letter")]
    
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
    output$preProcessedPlot <- renderPlot({
      
      x3pListPlot(tmp$x3p_processed) +
        theme(legend.position = "none")
      
    })
    
  })
  
  # skip the preprocessing procedures if the imported x3ps are already
  # processed
  observeEvent(input$skipPreprocessing,{
    
    tmp <- isolate(shiny.r$data)
    
    tmp <- tmp %>%
      mutate(x3p_processed = x3p)
    
    shiny.r$data <<- tmp
    
    output$skipPreprocessingConfirm <- renderText({return("Preprocessing Skipped")})
    
  })
  
  # add the "Perform Comparison" button to the Comparison tab only if the
  # preProcessExecute or skipPreprocessing buttons have been clicked
  observeEvent({list(input$preProcessExecute,input$skipPreprocessing)},
               {
                 
                 req(shiny.r$data)
                 
                 output$comparisonButtonMessage <- renderUI({
                   
                   return(shiny::actionButton("comparisonButton",label = "Perform Comparison"))
                   
                 })
                 
                 addTooltip(session = session,id = "comparisonButton",title = "Execute comparison procedure under selected parameters")
                 
               })
  
  ################################################# Code for Comparison Parameters tab
  
  # plot the reference scan broken up into cells
  output$preComparisonReference <- renderPlot({
    
    req(input$referenceSelect != "")
    
    # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Preparing Reference Scan")
    
    tmp <- isolate(shiny.r$data)
    
    req(!is.null(tmp$x3p_processed))
    
    cellGrid <- input$numCells %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_int(as.integer)
    
    referenceCell_df <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == input$referenceSelect) %>%
      pull(x3p_processed) %>%
      .[[1]] %>%
      cmcR::comparison_cellDivision(numCells = cellGrid)
    
    # update the shiny.r object with the data frame containing the divided
    # scan so that this doesn't have to be repeated below
    # shiny.r$referenceCell_df <<- referenceCell_df
    
    surfaceMat_df <- referenceCell_df %>%
      group_by(cellIndex) %>%
      group_split() %>%
      purrr::map_dfr(function(dat){
        
        x3pToDF(dat$cellHeightValues[[1]]) %>%
          mutate(cellIndex = unique(dat$cellIndex),
                 tooManyMissing = (mean(is.na(value)) >= input$maxNonMissingProp))
        
      }) %>%
      tidyr::separate(col = cellIndex,into = c("rowInd","colInd"),sep = ", ",remove = TRUE) %>%
      mutate(rowInd = as.factor(rowInd),
             colInd = as.factor(colInd))
    
    cellPlt_refToTarget <- surfaceMat_df %>%
      ggplot(aes(x=x,y=y,fill = value,alpha=tooManyMissing)) +
      geom_raster() +
      facet_grid(rows = vars(rowInd),
                 cols = vars(colInd)) +
      ggplot2::scale_fill_gradientn(colours = c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb", "#f7f7f7", "#fee0b6", "#fdb863" ,"#e08214" ,"#b35806","#7f3b08"),
                                    values = scales::rescale(quantile(surfaceMat_df$value,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                    breaks = function(lims){
                                      dat <- quantile(surfaceMat_df$value,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                      
                                      dat <- dat %>%
                                        setNames(paste0(names(dat)," [",round(dat,3),"]"))
                                      
                                      return(dat)
                                    },
                                    na.value = "gray65") +
      scale_alpha_manual(values = c(1,.2)) +
      theme_minimal() +
      coord_fixed(expand = FALSE) +
      theme(axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(hjust = .5,
                                               size = 11)
            ,legend.position = "none") +
      ggplot2::labs(title = input$referenceSelect) +
      ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(3,"in"),
                                                      label.theme = ggplot2::element_text(size = 8),
                                                      title.theme = ggplot2::element_text(size = 10),
                                                      frame.colour = "black",
                                                      ticks.colour = "black"),
                      colour =  'none')
    
    # shinybusy::remove_modal_spinner()
    
    return(cellPlt_refToTarget)
    
  })
  
  # plot the scan to be used as target
  output$preComparisonWholeScanTarget <- renderPlot({
    
    req(input$targetSelect != "")
    
    # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Visualizing Target Scan")
    
    tmp <- isolate(shiny.r$data)
    
    req(!is.null(tmp$x3p_processed))
    
    target <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == input$targetSelect) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    plt <- x3pListPlot(list(target) %>% purrr::set_names(input$targetSelect)) +
      theme(legend.position = "none")
    
    # shinybusy::remove_modal_spinner()
    
    return(plt)
    
  })
  
  # if both comparions directions are checked, plot the target scan broken up into cells and the whole reference scan
  observeEvent(input$bothDirectionsCheck,{
    
    req(shiny.r$data)
    req(shiny.r$data$x3p_processed)
    
    tmp <- isolate(shiny.r$data)
    
    # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Preparing Target Scan")
    
    # plot of target scan broken into cells
    output$preComparisonTarget <- renderPlot({
      
      cellGrid <- input$numCells %>%
        stringr::str_split(",") %>%
        .[[1]] %>%
        purrr::map_int(as.integer)
      
      targetCell_df <- tmp %>%
        mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
        filter(x3pNames == input$targetSelect) %>%
        pull(x3p_processed) %>%
        .[[1]] %>%
        cmcR::comparison_cellDivision(numCells = cellGrid)
      
      # update the shiny.r object with the data frame containing the divided
      # scan so that this doesn't have to be repeated below
      # shiny.r$targetCell_df <<- targetCell_df
      
      surfaceMat_df <- targetCell_df %>%
        group_by(cellIndex) %>%
        group_split() %>%
        purrr::map_dfr(function(dat){
          
          x3pToDF(dat$cellHeightValues[[1]]) %>%
            mutate(cellIndex = unique(dat$cellIndex),
                   tooManyMissing = (mean(is.na(value)) >= input$maxNonMissingProp))
          
        }) %>%
        tidyr::separate(col = cellIndex,into = c("rowInd","colInd"),sep = ", ",remove = TRUE) %>%
        mutate(rowInd = as.factor(rowInd),
               colInd = as.factor(colInd))
      
      cellPlt_targetToRef <- surfaceMat_df %>%
        ggplot(aes(x=x,y=y,fill = value,alpha = tooManyMissing)) +
        geom_raster() +
        facet_grid(rows = vars(rowInd),
                   cols = vars(colInd)) +
        ggplot2::scale_fill_gradientn(colours = c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb", "#f7f7f7", "#fee0b6", "#fdb863" ,"#e08214" ,"#b35806","#7f3b08"),
                                      values = scales::rescale(quantile(surfaceMat_df$value,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                      breaks = function(lims){
                                        dat <- quantile(surfaceMat_df$value,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                        
                                        dat <- dat %>%
                                          setNames(paste0(names(dat)," [",round(dat,3),"]"))
                                        
                                        return(dat)
                                      },
                                      na.value = "gray65") +
        scale_alpha_manual(values = c(1,.2)) +
        theme_minimal() +
        coord_fixed(expand = FALSE) +
        theme(axis.title.x = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_blank(),
              axis.ticks.x = ggplot2::element_blank(),
              axis.title.y = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              plot.title = ggplot2::element_text(hjust = .5,
                                                 size = 11)
              ,legend.position = "none") +
        ggplot2::labs(title = input$targetSelect) +
        ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(3,"in"),
                                                        label.theme = ggplot2::element_text(size = 8),
                                                        title.theme = ggplot2::element_text(size = 10),
                                                        frame.colour = "black",
                                                        ticks.colour = "black"),
                        colour =  'none')
      
      # shinybusy::remove_modal_spinner()
      
      return(cellPlt_targetToRef)
      
    })
    
    # plot of full reference scan
    output$preComparisonWholeScanReference <- renderPlot({
      
      req(input$referenceSelect != "")
      
      # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Visualizing Reference Scan")
      
      tmp <- isolate(shiny.r$data)
      
      req(!is.null(tmp$x3p_processed))
      
      target <- tmp %>%
        mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
        filter(x3pNames == input$referenceSelect) %>%
        pull(x3p_processed) %>%
        .[[1]]
      
      plt <- x3pListPlot(list(target) %>% purrr::set_names(input$referenceSelect)) +
        theme(legend.position = "none")
      
      # shinybusy::remove_modal_spinner()
      
      return(plt)
      
    })
    
  })
  
  # perform the comparison procedure (possibly in both directions) once the
  # comparisonButton is pressed
  observeEvent(input$comparisonButton,{
    
    req(shiny.r$data)
    req(shiny.r$data$x3p_processed)
    
    # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Performing Comparisons")
    
    thetas <- seq(from = input$thetaRangeMin,to = input$thetaRangeMax,by = input$thetaStep)
    
    tmp <- isolate(shiny.r$data)
    
    cellGrid <- input$numCells %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_int(as.integer)
    
    reference <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == input$referenceSelect) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    target <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == input$targetSelect) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    show_modal_progress_line(text = paste0("Comparing reference vs. target scans at rotation ",thetas[1],"°"),value = 0)
    
    comparisonData_refToTarget <- 
      purrr::map2_dfr(thetas,
                      1:length(thetas),
                      function(theta,ind){
                        
                        dat <- cmcR::comparison_allTogether(reference = reference,
                                                            target = target,
                                                            theta = theta,
                                                            numCells = cellGrid,
                                                            maxMissingProp = input$maxNonMissingProp,
                                                            sideLengthMultiplier = 3,
                                                            returnX3Ps = TRUE)
                        
                        update_modal_progress(value = (ind)*(1/length(thetas)),
                                              text = paste0("Comparing reference vs. target scans at rotation ",theta,"°"))
                        
                        return(dat)
                      })
    
    update_modal_progress(value = 1,
                          text = paste0("Comparing reference vs. target scans at rotation ",thetas[[length(thetas)]],"°"))
    
    remove_modal_progress()
    
    shiny.r$comparisonData_refToTarget <<- comparisonData_refToTarget
    
    updateSelectInput(session = session,
                      inputId = "comparisonSummary_referenceSelect",
                      choices = c("",input$referenceSelect))
    
    updateSelectInput(session = session,
                      inputId = "postComparisonScanSelect",
                      choices = c("",input$referenceSelect))
    
    updateSelectInput(session = session,
                      inputId = "cellTrajectoryScan",
                      choices = c("",input$referenceSelect))
    
    updateSelectInput(session = session,
                      inputId = "customCellSelection",
                      choices = c("",input$referenceSelect))
    
    updateSelectInput(session = session,
                      inputId = "comparisonSummary_rotations",
                      choices = thetas,
                      selected = thetas)
    
    # perform the comparison in both directions if 
    if(input$bothDirectionsCheck){
      
      show_modal_progress_line(text = "Comparing target vs. reference scans at rotation ",thetas[1],"°",value = 0)
      
      comparisonData_targetToRef <- 
        purrr::map2_dfr(thetas,
                        1:length(thetas),
                        function(theta,ind){
                          
                          dat <- cmcR::comparison_allTogether(reference = target,
                                                              target = reference,
                                                              theta = theta,
                                                              numCells = cellGrid,
                                                              maxMissingProp = input$maxNonMissingProp,
                                                              sideLengthMultiplier = 3,
                                                              returnX3Ps = TRUE)
                          
                          update_modal_progress(value = (ind)*(1/length(thetas)),
                                                text = paste0("Comparing target vs. reference scans at rotation ",theta,"°"))
                          
                          return(dat)
                        })
      
      update_modal_progress(value = 1,
                            text = paste0("Comparing target vs. reference scans at rotation ",thetas[[length(thetas)]],"°"))
      
      remove_modal_progress()
      
      shiny.r$comparisonData_targetToRef <<- comparisonData_targetToRef
      
      updateSelectInput(session = session,
                        inputId = "postComparisonScanSelect",
                        choices = c("",input$referenceSelect,input$targetSelect))
      
      updateSelectInput(session = session,
                        inputId = "cellTrajectoryScan",
                        choices = c("",input$referenceSelect,input$targetSelect))
      
      updateSelectInput(session = session,
                        inputId = "customCellSelection",
                        choices = c("",input$referenceSelect,input$targetSelect))
      
      updateSelectInput(session = session,
                        inputId = "comparisonSummary_referenceSelect",
                        choices = c("",input$referenceSelect,input$targetSelect))
      
      # reset all of the UI in the comparison Results, Custom Cell, and Cell
      # Trajectories if-needed
      output$postComparisonPlot <- NULL
      output$targetScanCellPlot <- NULL
      output$cellComparisonPlot <- NULL
      output$customCellFullScanPlot <- NULL
      output$targetScanCustomCellPlot <- NULL
      output$customCellComparisonPlot <- NULL
      output$cellTrajectoryFullScanPlot <- NULL
      output$cellTrajectoryAnimation <- NULL
      output$comparisonSummary_histograms <- NULL
      
    }
    
  })
  
  ################################### code for Comparison Results - Summary tab
  
  observeEvent(input$comparisonSummary_referenceSelect,{
    
    output$comparisonSummary_histograms <-
      renderGirafe({
        # renderPlot({
        
        req(shiny.r$data)
        req(input$comparisonSummary_referenceSelect)
        req(shiny.r$comparisonData_refToTarget)
        
        tmp <- isolate(shiny.r$data)
        comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
        
        selectedScan <- input$comparisonSummary_referenceSelect
        # if we're performing a self-comparison...
        if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
          
          otherScan <- input$comparisonSummary_referenceSelect
          
          compData <- comparisonData_refToTarget
          
        }
        # otherwise there are two disticnt scans selected
        else{
          
          otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$comparisonSummary_referenceSelect)]
          
          comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
          
          compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$comparisonSummary_referenceSelect)]]
          
        }
        
        # choose a sensible number of rows based on the number of rotations to plot
        numThetas <- length(seq(from = input$thetaRangeMin,
                                to = input$thetaRangeMax,
                                by = input$thetaStep))
        
        numRows <- 1
        if(numThetas > 1){
          
          possibleNumRows <- map_lgl(2:ceiling(sqrt(numThetas)),
                                     function(val){
                                       
                                       return((numThetas %% val) == 0)
                                       
                                     })
          
          numRows <- min((2:ceiling(sqrt(numThetas)))[possibleNumRows])
        }
        
        pltScatter <- compData %>%
          # filter(theta %in% input$comparisonSummary_rotations) %>%
          mutate(data_id = paste0(cellIndex,", ",theta)) %>%
          select(cellIndex,data_id,x,y,theta,fft_ccf,pairwiseCompCor) %>%
          ggplot(aes(x=x,y=y)) +#,alpha=pairwiseCompCor)) +
          # geom_point() +
          geom_jitter_interactive(alpha = .5,size = 2,
                                  width = 0,
                                  height= 0,
                                  aes(
                                    # data_id = data_id
                                    tooltip = cellIndex,
                                    data_id = cellIndex
                                  )) 
        
        if(!is.infinite(numRows)){
          
          pltScatter <- pltScatter +
            facet_wrap(~ theta, nrow = numRows) +
            theme_bw() +
            coord_fixed()
          
        }
        else{
          
          pltScatter <- pltScatter +
            facet_wrap(~ theta) +
            theme_bw() +
            coord_fixed()
          
        }
        
        dat <- compData %>%
          group_by(cellIndex) %>%
          filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
          ungroup() %>%
          mutate(data_id = paste0(cellIndex,", ",theta)) %>%
          select(cellIndex,data_id,x,y,theta,fft_ccf,pairwiseCompCor) %>%
          pivot_longer(cols = 3:7,names_to = "var",values_to = "value")
        
        plt <- dat %>%
          ggplot(aes(x=value)) +
          geom_dotplot_interactive(aes(tooltip = cellIndex,
                                       data_id = cellIndex
                                       # data_id = data_id
          ),
          stackgroups = TRUE,
          binpositions = "all",
          stackratio = 1.1) +
          facet_wrap(~var,scales = "free_x") +
          theme_bw()
        
        reference <- tmp %>%
          mutate(x3pName  = paste0("x3p",1:nrow(.))) %>%
          filter(x3pName == selectedScan) %>%
          pull(x3p_processed) %>%
          .[[1]]
        
        target <- tmp %>%
          mutate(x3pName  = paste0("x3p",1:nrow(.))) %>%
          filter(x3pName == otherScan) %>%
          pull(x3p_processed) %>%
          .[[1]]
        
        cmcPlts <- cmcPlot_interactive(reference = reference,
                                       target = target,
                                       cmcClassifs = compData %>%
                                         group_by(cellIndex) %>%
                                         filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
                                         ungroup() %>%
                                         mutate(originalMethod = "CMC"),
                                       type = "list")
        
        library(patchwork)
        
        return(girafe(code = print(
          wrap_plots((pltScatter + cmcPlts[[1]]),(plt + cmcPlts[[2]]),nrow = 2)
        ),
        # ((pltScatter + cmcPlts[[1]]) / (plt + cmcPlts[[2]])) +
        #                            patchwork::plot_layout(nrow = 2,heights = c(1,1))),
        width_svg = 24,height_svg = 15,
        options = list(opts_selection(css = "fill:orange;stroke:orange;color:black;"))))
        
      })
    
  })
  
  observeEvent(input$comparisonSummary_rePlot,{
    
    output$comparisonSummary_histograms <-
      renderGirafe({
        
        req(shiny.r$data)
        req(input$comparisonSummary_referenceSelect)
        req(shiny.r$comparisonData_refToTarget)
        
        tmp <- isolate(shiny.r$data)
        comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
        
        selectedScan <- input$comparisonSummary_referenceSelect
        # if we're performing a self-comparison...
        if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
          
          otherScan <- input$comparisonSummary_referenceSelect
          
          compData <- comparisonData_refToTarget
          
        }
        # otherwise there are two disticnt scans selected
        else{
          
          otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$comparisonSummary_referenceSelect)]
          
          comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
          
          compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$comparisonSummary_referenceSelect)]]
          
        }
        
        # choose a sensible number of rows based on the number of rotations to plot
        numThetas <- length(input$comparisonSummary_rotations)
        numRows <- 1
        if(numThetas > 1){
          
          possibleNumRows <- map_lgl(2:ceiling(sqrt(numThetas)),
                                     function(val){
                                       
                                       return((numThetas %% val) == 0)
                                       
                                     })
          
          numRows <- min((2:ceiling(sqrt(numThetas)))[possibleNumRows])
          
        }
        
        pltScatter <- compData %>%
          filter(theta %in% input$comparisonSummary_rotations) %>%
          mutate(data_id = paste0(cellIndex,", ",theta)) %>%
          select(cellIndex,data_id,x,y,theta,fft_ccf,pairwiseCompCor) %>%
          ggplot(aes(x=x,y=y)) +
          geom_jitter_interactive(alpha = .5,size = 2,
                                  width = input$comparisonSummary_jitterAmount,
                                  height= input$comparisonSummary_jitterAmount,
                                  aes(
                                    tooltip = cellIndex,
                                    data_id = cellIndex
                                  ))
        
        
        if(!is.infinite(numRows)){
          
          pltScatter <- pltScatter +
            facet_wrap(~ theta, nrow = numRows) +
            theme_bw() +
            coord_fixed()
          
        }
        else{
          
          pltScatter <- pltScatter +
            facet_wrap(~ theta) +
            theme_bw() +
            coord_fixed()
          
        }
        
        dat <- compData %>%
          group_by(cellIndex) %>%
          filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
          ungroup() %>%
          mutate(data_id = paste0(cellIndex,", ",theta)) %>%
          select(cellIndex,data_id,x,y,theta,fft_ccf,pairwiseCompCor) %>%
          pivot_longer(cols = 3:7,names_to = "var",values_to = "value")
        
        plt <- dat %>%
          ggplot(aes(x=value)) +
          geom_dotplot_interactive(aes(tooltip = cellIndex,
                                       data_id = cellIndex
          ),
          stackgroups = TRUE,
          binpositions = "all",
          stackratio = 1.1) +
          facet_wrap(~var,scales = "free_x") +
          theme_bw()
        
        reference <- tmp %>%
          mutate(x3pName  = paste0("x3p",1:nrow(.))) %>%
          filter(x3pName == selectedScan) %>%
          pull(x3p_processed) %>%
          .[[1]]
        
        target <- tmp %>%
          mutate(x3pName  = paste0("x3p",1:nrow(.))) %>%
          filter(x3pName == otherScan) %>%
          pull(x3p_processed) %>%
          .[[1]]
        
        cmcPlts <- cmcPlot_interactive(reference = reference,
                                       target = target,
                                       cmcClassifs = compData %>%
                                         group_by(cellIndex) %>%
                                         filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
                                         ungroup() %>%
                                         mutate(originalMethod = "CMC"),
                                       type = "list")
        
        library(patchwork)
        
        return(girafe(code = print(
          wrap_plots((pltScatter + cmcPlts[[1]]),(plt + cmcPlts[[2]]),nrow = 2)
        ),
        # ((pltScatter + cmcPlts[[1]]) / (plt + cmcPlts[[2]])) +
        #                            patchwork::plot_layout(nrow = 2,heights = c(1,1))),
        width_svg = 24,height_svg = 15,
        options = list(opts_selection(css = "fill:orange;stroke:orange;font-color:black;"))))
        
      })
    
  })
  
  ################################### code for Comparison Results - Individual Cell tab
  
  # on the comparison results tab, the user will select an x3p to explore
  output$postComparisonPlot <- renderPlot({
    
    tmp <- isolate(shiny.r$data)
    
    req(!is.null(tmp$x3p_processed))
    req(input$postComparisonScanSelect)
    
    
    cellGrid <- input$numCells %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_int(as.integer)
    
    req(shiny.r$comparisonData_refToTarget)
    comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
    
    selectedScan <- input$postComparisonScanSelect
    # if we're performing a self-comparison...
    if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
      
      otherScan <- input$postComparisonScanSelect
      
      compData <- comparisonData_refToTarget
      
    }
    # otherwise there are two disticnt scans selected
    else{
      
      otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$postComparisonScanSelect)]
      
      comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
      
      compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$postComparisonScanSelect)]]
      
    }
    
    referenceCell_df <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == selectedScan) %>%
      pull(x3p_processed) %>%
      .[[1]] %>%
      cmcR::comparison_cellDivision(numCells = cellGrid)
    
    # update the shiny.r object with the data frame containing the divided
    # scan so that this doesn't have to be repeated below
    # shiny.r$referenceCell_df <<- referenceCell_df
    surfaceMat_df <- purrr::map_dfr(referenceCell_df %>%
                                      group_by(cellIndex) %>%
                                      group_split(),
                                    function(dat){
                                      
                                      x3pToDF(dat$cellHeightValues[[1]])%>%
                                        mutate(cellIndex = unique(dat$cellIndex),
                                               tooManyMissing = (mean(is.na(value)) >= input$maxNonMissingProp))
                                      
                                    }) %>%
      filter(cellIndex %in% unique(compData$cellIndex)) %>%
      tidyr::separate(col = cellIndex,into = c("rowInd","colInd"),sep = ", ",remove = TRUE) %>%
      mutate(rowInd = as.factor(rowInd),
             colInd = as.factor(colInd))
    
    cellPlt <- surfaceMat_df %>%
      ggplot(aes(x=x,y=y,fill = value,alpha=tooManyMissing)) +
      geom_raster() +
      facet_grid(rows = vars(rowInd),
                 cols = vars(colInd)) +
      ggplot2::scale_fill_gradientn(colours = c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb", "#f7f7f7", "#fee0b6", "#fdb863" ,"#e08214" ,"#b35806","#7f3b08"),
                                    values = scales::rescale(quantile(surfaceMat_df$value,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                    breaks = function(lims){
                                      dat <- quantile(surfaceMat_df$value,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                      
                                      dat <- dat %>%
                                        setNames(paste0(names(dat)," [",round(dat,3),"]"))
                                      
                                      return(dat)
                                    },
                                    na.value = "gray65") +
      scale_alpha_manual(values = c(1,0)) +
      theme_minimal() +
      coord_fixed(expand = FALSE) +
      theme(axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(hjust = .5,
                                               size = 11)
            ,legend.position = "none") +
      ggplot2::labs(title = input$referenceSelect) +
      ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(3,"in"),
                                                      label.theme = ggplot2::element_text(size = 8),
                                                      title.theme = ggplot2::element_text(size = 10),
                                                      frame.colour = "black",
                                                      ticks.colour = "black"),
                      colour =  'none')
    
    return(cellPlt)
    
  })
  
  observeEvent(input$postComparisonScanSelect,{
    
    req(shiny.r$data)
    req(shiny.r$comparisonData_refToTarget)
    req(input$postComparisonClick$panelvar1)
    req(input$postComparisonClick$panelvar2)
    req(input$postComparisonScanSelect)
    
    output$cellComparisonPlot <- NULL
    output$targetScanCellPlot <- NULL
    
  })
  
  # on the Comparison Results tab, the user can click on the reference cell
  # plot. We will show the five plot comaprison from the impressions package.
  # Note that either scan used in the comparison can be considered in this
  # plot, so we need to do some fancy selection to make sure we keep what the
  # "reference" and "target" scan given the value of
  # input$postComparisonScanSelect
  observeEvent(input$postComparisonClick,{
    
    req(shiny.r$data)
    req(shiny.r$comparisonData_refToTarget)
    req(input$postComparisonClick$panelvar1)
    req(input$postComparisonClick$panelvar2)
    req(input$postComparisonScanSelect)
    
    # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Pulling Reference Cell Data")
    
    tmp <- isolate(shiny.r$data)
    
    selectedScan <- input$postComparisonScanSelect
    # if we're performing a self-comparison...
    if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
      
      otherScan <- input$referenceSelect 
      
    }
    # otherwise there are two disticnt scans selected
    else{
      
      otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$postComparisonScanSelect)]
      
    }
    
    reference <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == selectedScan) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    target <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == otherScan) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    req(shiny.r$comparisonData_refToTarget)
    comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
    
    if(input$bothDirectionsCheck){
      
      comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
      
      compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$postComparisonScanSelect)]]
      
    }
    else{
      compData <- comparisonData_refToTarget
    }
    
    cellGrid <- input$numCells %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_int(as.integer)
    
    # for now, we consider the aligned target cell that maxmizes the
    # pairwise-complete correlation with the selected reference cell
    alignedCellData <- compData %>%
      tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ",remove = FALSE) %>%
      group_by(cellIndex) %>%
      filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
      ungroup()
    
    # visualize the five plot comparison for the user-selected cell
    output$cellComparisonPlot <- renderPlot({
      
      return(fiveplot(alignedCellData %>%
                        filter(cellIndex == paste0(input$postComparisonClick$panelvar2,", ",
                                                   input$postComparisonClick$panelvar1)),
                      reference = input$referenceSelect,
                      target = input$targetSelect,
                      cell = paste0(input$postComparisonClick$panelvar2,", ",
                                    input$postComparisonClick$panelvar1)))
      
    })
    
    # visualize on the target scan where the aligned target cell is
    output$targetScanCellPlot <- renderPlot({
      
      # debugonce(cmcPlot_colorChange)
      
      plts <- cmcPlot_colorChange(reference = reference,
                                  target = target,
                                  cmcClassifs = alignedCellData %>% 
                                    mutate(originalMethod = "CMC"),
                                  cellToPlot = paste0(input$postComparisonClick$panelvar2,", ",
                                                      input$postComparisonClick$panelvar1),
                                  type = "list")
      
      return(plts[[1]] +
               scale_fill_manual(values = "black"))
      
      # return(plts[[2]] +
      #          scale_fill_manual(values = "black"))
      
    })
    
    # # shinybusy::remove_modal_spinner()
    
  })
  
  observeEvent(input$cellTrajectoryScan,{
    
    tmp <- isolate(shiny.r$data)
    
    req(!is.null(tmp$x3p_processed))
    req(input$cellTrajectoryScan)
    
    output$cellTrajectoryAnimation <- NULL
    
    cellGrid <- input$numCells %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_int(as.integer)
    
    req(shiny.r$comparisonData_refToTarget)
    comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
    
    selectedScan <- input$cellTrajectoryScan
    # if we're performing a self-comparison...
    if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
      
      otherScan <- input$cellTrajectoryScan
      
      compData <- comparisonData_refToTarget
      
    }
    # otherwise there are two disticnt scans selected
    else{
      
      otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$cellTrajectoryScan)]
      
      comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
      
      compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$cellTrajectoryScan)]]
      
    }
    
    selectedScan <- input$cellTrajectoryScan
    # if we're performing a self-comparison...
    if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
      
      otherScan <- input$referenceSelect 
      
      comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
      
      compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$cellTrajectoryScan)]]
      
    }
    # otherwise there are two disticnt scans selected
    else{
      
      otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$cellTrajectoryScan)]
      
      compData <- comparisonData_refToTarget
      
    }
    
    referenceCell_df <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == selectedScan) %>%
      pull(x3p_processed) %>%
      .[[1]] %>%
      cmcR::comparison_cellDivision(numCells = cellGrid)
    
    targetScan <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == otherScan) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    targetCell_df <- targetScan %>%
      cmcR::comparison_cellDivision(numCells = cellGrid)
    
    output$cellTrajectoryFullScanPlot <- renderPlot({
      
      # update the shiny.r object with the data frame containing the divided
      # scan so that this doesn't have to be repeated below
      # shiny.r$referenceCell_df <<- referenceCell_df
      surfaceMat_df <- purrr::map_dfr(referenceCell_df %>%
                                        group_by(cellIndex) %>%
                                        group_split(),
                                      function(dat){
                                        
                                        x3pToDF(dat$cellHeightValues[[1]])%>%
                                          mutate(cellIndex = unique(dat$cellIndex),
                                                 tooManyMissing = (mean(is.na(value)) >= input$maxNonMissingProp))
                                        
                                      }) %>%
        filter(cellIndex %in% unique(compData$cellIndex)) %>%
        tidyr::separate(col = cellIndex,into = c("rowInd","colInd"),sep = ", ",remove = TRUE) %>%
        mutate(rowInd = as.factor(rowInd),
               colInd = as.factor(colInd))
      
      cellPlt <- surfaceMat_df %>%
        ggplot(aes(x=x,y=y,fill = value,alpha=tooManyMissing)) +
        geom_raster() +
        facet_grid(rows = vars(rowInd),
                   cols = vars(colInd)) +
        ggplot2::scale_fill_gradientn(colours = c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb", "#f7f7f7", "#fee0b6", "#fdb863" ,"#e08214" ,"#b35806","#7f3b08"),
                                      values = scales::rescale(quantile(surfaceMat_df$value,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
                                      breaks = function(lims){
                                        dat <- quantile(surfaceMat_df$value,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
                                        
                                        dat <- dat %>%
                                          setNames(paste0(names(dat)," [",round(dat,3),"]"))
                                        
                                        return(dat)
                                      },
                                      na.value = "gray65") +
        scale_alpha_manual(values = c(1,0)) +
        theme_minimal() +
        coord_fixed(expand = FALSE) +
        theme(axis.title.x = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_blank(),
              axis.ticks.x = ggplot2::element_blank(),
              axis.title.y = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              plot.title = ggplot2::element_text(hjust = .5,
                                                 size = 11)
              ,legend.position = "none") +
        ggplot2::labs(title = input$referenceSelect) +
        ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(3,"in"),
                                                        label.theme = ggplot2::element_text(size = 8),
                                                        title.theme = ggplot2::element_text(size = 10),
                                                        frame.colour = "black",
                                                        ticks.colour = "black"),
                        colour =  'none')
      
      return(cellPlt)
      
    })
    
    updateSelectInput(inputId = "cellTrajectorySelections",
                      choices = unique(compData$cellIndex))
    
    
    
  })
  
  observeEvent(input$cellTrajectoryClick,{
    
    req(shiny.r$data)
    req(shiny.r$comparisonData_refToTarget)
    req(input$cellTrajectoryClick$panelvar1)
    req(input$cellTrajectoryClick$panelvar2)
    req(input$cellTrajectoryScan)
    
    # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Pulling Reference Cell Data")
    
    tmp <- isolate(shiny.r$data)
    
    req(shiny.r$comparisonData_refToTarget)
    comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
    
    if(input$bothDirectionsCheck){
      
      comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
      
      compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$cellTrajectoryScan)]]
      
    }
    else{
      compData <- comparisonData_refToTarget
    }
    
    currentlySelectedCells <- input$cellTrajectorySelections
    
    updateSelectInput(inputId = "cellTrajectorySelections",
                      selected = c(paste0(input$cellTrajectoryClick$panelvar2,", ",
                                          input$cellTrajectoryClick$panelvar1),
                                   currentlySelectedCells))
    
  })
  
  observeEvent(input$cellTrajectoryExecute,{
    
    tmp <- isolate(shiny.r$data)
    
    req(!is.null(tmp$x3p_processed))
    req(input$cellTrajectoryScan)
    req(input$cellTrajectorySelections)
    
    output$saveCellTrajectorySpot <- renderUI({
      
      downloadButton("saveCellTrajectoryAnim","Save Animation")
      
    })
    
    cellGrid <- input$numCells %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      purrr::map_int(as.integer)
    
    req(shiny.r$comparisonData_refToTarget)
    comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
    
    selectedScan <- input$cellTrajectoryScan
    # if we're performing a self-comparison...
    if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
      
      otherScan <- input$referenceSelect 
      
      comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
      
      compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$cellTrajectoryScan)]]
      
    }
    # otherwise there are two disticnt scans selected
    else{
      
      otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$cellTrajectoryScan)]
      
      compData <- comparisonData_refToTarget
      
    }
    
    targetScan <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == otherScan) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    
    cellTraj <- compData %>%
      filter(cellIndex %in% input$cellTrajectorySelections) %>%
      select(theta,alignedTargetCell,cellIndex) %>%
      pmap_dfr(~ {
        
        impressions::targetCellCorners(..2,..3,..1,"CMC",targetScan)
        
      }) #%>%
    # group_by(cellIndex,theta) %>%
    # summarise(x = mean(x),y = mean(y))
    
    plt <- x3pListPlot(list(targetScan) %>% 
                         set_names(otherScan)) +
      geom_raster(data = cellTraj,
                  aes(x=x,y=y),
                  alpha = .3,
                  inherit.aes = FALSE)
    # geom_line(data = cellTraj,
    #           aes(x=x,y=y,group = cellIndex),size = 1)
    
    for(cellInd in input$cellTrajectorySelections){
      
      plt <- plt +
        geom_text(data = cellTraj %>%
                    filter(cellIndex == cellInd)%>%
                    group_by(cellIndex,theta) %>%
                    summarise(x = mean(x),y = mean(y)),
                  aes(x=x,y=y,label=cellIndex),size = 6)
      
    }
    
    plt <- plt +
      theme(legend.position = "none",
            strip.text = element_blank(),
            plot.title = element_text(hjust = .5)) +
      transition_manual(theta,cumulative = FALSE) +
      labs(title = "Rotation: {current_frame} degrees")
    # labs(title = "Rotation: {3*round(frame_along/3)} degrees")
    
    # temporarily update an internal gganimate function to show a custom
    # rendering progress bar in the app
    rlang::env_unlock(env = asNamespace("gganimate"))
    rlang::env_binding_unlock(env = asNamespace("gganimate"))
    assign("draw_frames",my_draw_frames,envir = asNamespace("gganimate"))
    rlang::env_binding_lock(env = asNamespace('gganimate'))
    rlang::env_lock(asNamespace('gganimate'))
    
    output$cellTrajectoryAnimation <- renderImage({
      
      # from https://github.com/thomasp85/gganimate/pull/331
      progress <- shiny::Progress$new(max = 100)
      progress$set(message = "Rendering",value = 0)
      on.exit(progress$close())
      
      updateShinyProgress <- function(det){
        
        progress$inc(1,detail = det)
        
      }
      
      gganimate::anim_save(animation = gganimate::animate(plt,update_progress = updateShinyProgress,duration = length(unique(cellTraj$theta))*2),
                           filename = "www/anim.gif")
      
      # reset the gganimate namespace with the original draw_frames function
      rlang::env_unlock(env = asNamespace("gganimate"))
      rlang::env_binding_unlock(env = asNamespace("gganimate"))
      assign("draw_frames",old_draw_frames,envir = asNamespace("gganimate"))
      rlang::env_binding_lock(env = asNamespace('gganimate'))
      rlang::env_lock(asNamespace('gganimate'))
      
      return(list(src = "www/anim.gif",content = "image/gif"))
      
    },deleteFile = FALSE)
    
  })
  
  # save the animation and delete it locally once the saveCellTrajectoryAnim button is clicked
  
  output$saveCellTrajectory <- 
    downloadHandler(
      filename = "cell_animation.gif",
      contentType = "image/gif",
      content = function(file){
        file.copy("www/anim.gif",file)
      }
    )
  
  # initialize and empty df to store manually annotated points
  plottedPointsCustomCell_blank <- data.frame(x = numeric(0),
                                              y = numeric(0),
                                              pointNum = numeric(0))
  plottedPointsCustomCell <- reactiveValues(dat = plottedPointsCustomCell_blank)
  
  output$customCellFullScanPlot <- renderPlot({
    
    req(shiny.r$data)
    req(input$customCellSelection != "")
    
    tmp <- isolate(shiny.r$data)
    
    selectedScan <- input$customCellSelection
    
    reference <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == selectedScan) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    
    customCellPlt <<- x3pListPlot(list(reference) %>% set_names(selectedScan))
    
    
    if(input$customCellType == "Hand-drawn"){
      
      customCellPlt <<- customCellPlt +
        geom_point(data = plottedPointsCustomCell$dat,
                   aes(x=x,y=y)) +
        geom_polygon(data = plottedPointsCustomCell$dat,
                     aes(x=x,y=y),inherit.aes = FALSE,fill = "black",alpha = .2)
      
    }
    
    return(customCellPlt)
    
  })
  
  observeEvent(list(input$customCellType,input$customCellSelection,input$targetSelect_customCell),{
    
    req(shiny.r$data)
    # req(shiny.r$comparisonData_refToTarget)
    req(input$customCellSelection)
    req(input$targetSelect_customCell)
    
    output$targetScanCustomCellPlot <- NULL
    output$customCellComparisonPlot <- NULL
    
  })
  
  observeEvent(input$customHandDrawnCellClick,{
    
    pts <- isolate(plottedPointsCustomCell$dat)
    
    pts <- bind_rows(pts,
                     data.frame(x = input$customHandDrawnCellClick$x,
                                y = input$customHandDrawnCellClick$y,
                                pointNum = nrow(pts) + 1))
    
    plottedPointsCustomCell$dat <<- pts
    
  })
  
  observeEvent(input$customCellBrush,{
    
    output$customHandDrawnCell <- renderPlot({
      
      req(input$customCellSelection)
      req(input$customCellBrush)
      req(input$customCellType == "Hand-drawn")
      
      customCellPlt_zoom <- customCellPlt +
        coord_fixed(xlim = c(unique(input$customCellBrush$xmin),unique(input$customCellBrush$xmax)),
                    ylim = c(unique(input$customCellBrush$ymin),unique(input$customCellBrush$ymax)),
                    expand = FALSE,
                    ratio = 1) +
        geom_point(data = plottedPointsCustomCell$dat,
                   aes(x=x,y=y)) +
        geom_polygon(data = plottedPointsCustomCell$dat,
                     aes(x=x,y=y),inherit.aes = FALSE,fill = "black",alpha = .2) +
        ggrepel::geom_text_repel(data = plottedPointsCustomCell$dat,
                                 aes(x=x,y=y,label = pointNum),inherit.aes = FALSE)
      
      return(customCellPlt_zoom)
      
    })
    
  })
  
  observeEvent(input$resetHandDrawnCustomCell,{
    
    output$customHandDrawnCell <- NULL
    output$targetScanCustomCellPlot <- NULL
    output$customCellComparisonPlot <- NULL
    
    plottedPointsCustomCell$dat <<- plottedPointsCustomCell_blank
    
  })
  
  # observeEvent(input$customCellBrush,{
  #   
  #   
  #   
  # })
  
  # the custom cell analysis doesn't require that the comparison procedure to
  # have been completed
  observeEvent(input$customCellExecute,{
    
    req(shiny.r$data)
    req(input$customCellBrush$xmin)
    req(input$customCellBrush$ymin)
    req(input$customCellBrush$xmax)
    req(input$customCellBrush$ymax)
    
    tmp <- isolate(shiny.r$data)
    
    selectedScan <- input$customCellSelection
    
    reference <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == selectedScan) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    if(input$customCellType == "Rectangular"){
      
      refCell <- reference
      
      topRow <- nrow(refCell$surface.matrix) - round(input$customCellBrush$ymax)
      bottomRow <- nrow(refCell$surface.matrix) - round(input$customCellBrush$ymin)
      
      leftCol <- round(input$customCellBrush$xmin)
      rightCol <- round(input$customCellBrush$xmax)
      
      refCell$surface.matrix <- refCell$surface.matrix[topRow:bottomRow,leftCol:rightCol]
      refCell$header.info$sizeX <- nrow(refCell$surface.matrix)
      refCell$header.info$sizeY <- ncol(refCell$surface.matrix)
      
      refCell$cmcR.info$cellRange <- paste0("rows: ",topRow," - ",bottomRow,", cols: ",leftCol," - ",rightCol)
      
    }
    else{
      
      pts <- isolate(plottedPointsCustomCell$dat)
      
      refCell <- reference %>%
        x3pToDF() %>%
        mutate(inPoly = sp::point.in.polygon(x,y,pts$x,pts$y)) %>%
        mutate(value = ifelse(inPoly == 0,NA,value)) %>%
        rename(xnew = y,
               ynew = x) %>%
        mutate(x=xnew,y=ynew) %>%
        mutate(y = max(y) - y,
               x = max(x) - x) %>%
        select(x,y,value) %>%
        x3ptools::df_to_x3p() 
      
      # browser()
      
      topRow <- min(which(rowSums(!is.na(refCell$surface.matrix)) > 0))
      bottomRow <- max(which(rowSums(!is.na(refCell$surface.matrix)) > 0))
      
      leftCol <- min(which(colSums(!is.na(refCell$surface.matrix)) > 0))
      rightCol <- max(which(colSums(!is.na(refCell$surface.matrix)) > 0))
      
      refCell$header.info$incrementX <- reference$header.info$incrementX
      refCell$header.info$incrementY <- reference$header.info$incrementY
      
      refCell$cmcR.info$cellRange <- paste0("rows: ",topRow," - ",bottomRow,", cols: ",leftCol," - ",rightCol)
      
      refCell <- cmcR:::preProcess_cropWS(refCell,croppingProp = .5)
      
    }
    
    target <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == input$targetSelect_customCell) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    thetas <- seq(from = input$thetaRangeMin_customCell,
                  to = input$thetaRangeMax_customCell,
                  by = input$thetaStep_customCell)
    
    show_modal_progress_line(text = paste0("Comparing custom cell to ",  input$targetSelect_customCell," at rotation ",thetas[1],"°"),value = 0)
    
    compData <- map2_dfr(thetas,
                         1:length(thetas),
                         function(theta,ind){
                           
                           # if(input$customCellType == "Rectangular"){
                           
                           # browser()
                           
                           dat <- comparison_customCell(refCell,target,theta,maxNonMissingProp = .99)
                           
                           # }
                           # else{
                           #   
                           #   # browser()
                           #   
                           #   dat <- comparison_allTogether(reference = refCell,
                           #                                 target = target,
                           #                                 theta = theta,
                           #                                 numCells = c(1,1),
                           #                                 sideLengthMultiplier = nrow(target$surface.matrix)/nrow(refCell$surface.matrix),
                           #                                 maxMissingProp = 1,
                           #                                 returnX3Ps = TRUE)
                           #   
                           # }
                           
                           update_modal_progress(value = (ind)*(1/length(thetas)),
                                                 text = paste0("Comparing custom cell to ", input$targetSelect_customCell," at rotation ",theta,"°"))
                           
                           return(dat)
                           
                         })
    
    update_modal_progress(value = 1,
                          text = paste0("Comparing custom cell to ",  input$targetSelect_customCell," at rotation ",thetas[[length(thetas)]],"°"))
    
    # browser()
    
    # for now, we consider the aligned target cell that maxmizes the
    # pairwise-complete correlation with the selected reference cell
    alignedCellData <- compData %>%
      tidyr::separate(col = cellIndex,into = c("rowIndex","colIndex"),sep = ", ",remove = FALSE) %>%
      filter(pairwiseCompCor == max(pairwiseCompCor))
    
    
    # visualize the five plot comparison for the user-selected cell
    output$customCellComparisonPlot <- renderPlot({
      
      return(fiveplot(alignedCellData,
                      reference = input$referenceSelect,
                      target = input$targetSelect,
                      cell = unique(alignedCellData$cellIndex)))
      
    })
    
    
    # visualize on the target scan where the aligned target cell is
    output$targetScanCustomCellPlot <- renderPlot({
      
      plts <- cmcPlot_colorChange(reference = reference,
                                  target = target,
                                  cmcClassifs = alignedCellData %>%
                                    mutate(originalMethod = "CMC"),
                                  type = "list")
      
      return(plts[[1]] +
               scale_fill_manual(values = "black"))
      
    })
    
    remove_modal_progress()
  })
  
}
