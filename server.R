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
  
  # show popup when the user clicks the Help button
  observeEvent(input$importHelp,
               {
                 
                 showModal(modalDialog(
                   title = "Help: Import Tab",easyClose = TRUE,
                   strong("Why would I use this tab?"),
                   "To upload x3p files to the app.",
                   br(),
                   br(),
                   strong("What do I need to do before using this tab?"),
                   "Download cartridge case scans to your computer as x3p files",
                   br(),
                   br(),
                   strong("How do I use this tab?"),
                   "Click the 'Select a folder containing x3p files' button to upload x3p scans to the app.",
                   br(),
                   br(),
                   strong("What is next?"),
                   "If the scans are already preprocessed to your liking, then click the 'Skip Automatic Preprocessing' button and move on to 'Manual Deletion' tab or the Comparing stage.",
                   "Otherwise, continue onto the 'Automatic Preprocess' tab."
                 ))
                 
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
                   mutate(x3pName = paste0("x3p",1:nrow(.)))
                 
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
  
  # show popup when the user clicks the Help button
  observeEvent(input$manualDeletionHelp,
               {
                 
                 showModal(modalDialog(
                   title = "Help: Manual Deletion Tab",easyClose = TRUE,
                   strong("Why would I use this tab?"),
                   "Manually delete regions from a scan.",
                   br(),
                   br(),
                   strong("What do I need to do before using this tab?"),
                   "To have clicked the 'Skip Automatic Preprocessing' button in the 'Import' tab or the 'Perform Automatic Preprocessing' button in the 'Automatic Preprocess' tab.",
                   br(),
                   br(),
                   strong("How do I use this tab?"),
                   "Select a scan from the 'Select a processed scan to annotate' dropdown.",
                   "Click and drag your cursor on the plot that appears to zoom-in to the region that you wish to delete.",
                   br(),
                   br(),
                   "Scroll down the page to see the zoomed-in region.",
                   "Left-click on this plot to place a point.",
                   "Place three or more points to create a connected region.",
                   "Click 'Reset Region' to remove all points.",
                   br(),
                   br(),
                   "Start a new region by drawing a new rectangle on the plot of the full scan at the top of the page.",
                   "Once you are happy with the annotated regions, click 'Confirm Annotations' to delete these regions from the scan.",
                   "If you would like to start over with manually annotating the selected scan, click the 'Reset Annotations' button.",
                   br(),
                   br(),
                   "Select another scan to manually delete regions from using the 'Select a processed scan to annotate' dropdown.",
                   br(),
                   br(),
                   strong("What is next?"),
                   "Once you are happy with the manually-deleted regions, move onto the Comparing stage."
                 ))
                 
               })
  
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
                                        # mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
                                        filter(x3pName == input$x3prgl1_select) %>%
                                        pull(x3p_processed) %>%
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
      
      actionButton("deleteAnnotationsButton","Reset Annotations")
      
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
      
      return("Once you're done annotating, press 'Confirm Annotations' to lock them in and view a visualization of the scan with deleted regions.\nClicking 'Reset Annotations' will clear all annotations made on the selected x3p.")
      
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
    
    output$x3p1_rgl <-
      renderPlot({
        # rgl::renderRglwidget({
        
        req(shiny.r$data)
        req(nrow(shiny.r$data) > 0)
        tmp <- isolate(shiny.r$data)
        
        x3p <- tmp %>%
          # mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
          filter(x3pName == input$x3prgl1_select) %>%
          pull(x3p_processed) %>%
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
        x3pMasked <- x3ptools::x3p_add_mask(x3pMasked,t(colorSurface))
        
        # delete the masked values from the surface matrix
        x3pMasked <- x3pDeleteMask(x3pMasked)
        
        # browser()
        
        # save the masked data out to a file
        x3ptools::write_x3p(x3pMasked,
                            file = paste0("data/User_scans/",input$x3prgl1_select,"_user",session$user,
                                          # "_time",str_replace_all(str_replace_all(str_squish(Sys.time()),":","-")," ","_"),
                                          ".x3p"))
        
        x3pMasked$mask <- NULL
        
        tmp <- tmp %>%
          # mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
          mutate(x3p_processed = ifelse(x3pName == input$x3prgl1_select,
                                        list(x3pMasked),
                                        x3p_processed))
        
        shiny.r$data <<- tmp
        
        # remove the currently-selected scan from the list of possible scans 
        
        # updateSelectInput(session = session,
        #                   inputId = "x3prgl1_select",
        #                   choices = c("",
        #                               tmp %>%
        #                                 mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
        #                                 filter(x3pName != input$x3prgl1_select) %>%
        #                                 pull(x3pName)))
        
        # return a ggplot of the mask-deleted x3p file
        
        return(x3pListPlot(list(x3pMasked)))
        
        # now visualize the mask using an rgl device
        # z <- 10 * surface
        # yidx <- ncol(z):1
        # y <- x3p$header.info$incrementY * yidx
        # x <- x3p$header.info$incrementX * (1:nrow(z))
        # params <- rgl::r3dDefaults
        # params$windowRect <- c(40, 125, 40 + c(750,250)[1], 125 + c(750,250)[2])
        # params$userMatrix <- diag(c(1, 1, 1, 1))
        # params$zoom <- .7
        # xyz <- matrix(c(min(y) - diff(range(y)), mean(y), max(z, 
        #                                                       na.rm = TRUE)), ncol = 3)
        # 
        # open3d(params = params)
        # rgl.pop("lights")
        # light3d(x = xyz, diffuse = "gray40", specular = "gray40", 
        #         ambient = "grey10", viewpoint.rel = TRUE)
        # light3d(diffuse = "gray20", specular = "gray20")
        # surface3d(x, y, z, color = c(colorSurface), back = "fill")
        # rgl::rglwidget()
      })
    
    output$postConfirmationMessage <- renderText({
      
      return("Annotations confirmed.\n
             If you would like to annotate another scan, please select a new scan in step 1.\n
             If you're happy with the manual annotations, move onto the Comparing stage.")
      
    })
    
  })
  
  observeEvent(input$deleteAnnotationsButton,{
    
    tmp <- isolate(shiny.r$data)
    
    # delete the mask added to the x3p
    x3pMask <- tmp %>%
      # mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
      filter(x3pName == input$x3prgl1_select) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    x3pMask$mask <- NULL
    
    tmp <- tmp %>%
      # mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
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
  
  # code to render Tutorial
  observeEvent(input$manualDeletionTutorialButton,{
    
    output$manualDeletionTutorial <- renderUI({
      
      return(HTML(paste0('<div style="max-width: 100%; width: 800px; margin: 0 auto;"> <div style="display: none;"> <p style="display: none; text-align: center; margin-top: 10px;"> <i style="font-style: italic; font-weight: bold; color: #CCCCCC; font-size: 18px;">12 STEPS</i> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 1. Whether you perform preprocessing in the Automatic Preprocessing tab or upload scans that are preprocessed, you may wish to manually remove certain regions of a scan. In the example shown, we wish to remove the highlighted dark purple region. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=1&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 1 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 2. Click on the <b style",
                         '="font-weight:normal;color:#FF00D6">Manual Deletion</b> tab. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=2&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 2 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 3. Select a scan from the dropdown. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=3&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 3 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 4. In this case, we are interested in removing a region from x3p3. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=4&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 4 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 5. A plot will appear of the selected scan. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=5&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 5 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 6. With your mouse, click and drag to highlight a region of interest. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=6&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 6 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 6b. Drop </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=6b&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 6b image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 7. A zoomed-in visualization of the region will appear below. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=7&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 7 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 8. Click on the visual to place a point. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=8&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 8 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 9. Place three or more points to start a polygon. This polygon will be removed from the scan. Continue placing points until the region of interest is highlighted. In the example show, we will highlight the dark purple region. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=9&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 9 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 10. Once the region of interest is fully highlighted, click <b style",
                         '="font-weight:normal;color:#FF00D6">Confirm Annotations.</b> </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=10&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 10 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 11. Inspect plot that appears to determine whether additional deletion is needed. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=11&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 11 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 12. Once you're happy with the manual deletions, you may continue on to the <b style",
                         '="font-weight:normal;color:#FF00D6">Comparing</b> stage. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984835&step_number=12&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 12 image" /> </p> </div> </div> <h3 style="display: none; font-size: 18px; margin-top: 89px; margin-bottom: 15px;">',
                         "Here's an interactive tutorial </h3> <i style",
                         '="display: none; font-size: 15px; margin-top: 0; margin-bottom: 43px;">** Best experienced in Full Screen (click the icon in the top right corner before you begin) **</i> <p style="display: none;"> <a href="https://www.iorad.com/player/1984835/cartridgeInvestigatR-Tutorial-3--Manual-Deletion-tab">https://www.iorad.com/player/1984835/cartridgeInvestigatR-Tutorial-3--Manual-Deletion-tab</a> </p> <p style="border: 0; min-width: 100%; margin-bottom: 0; height: 801px;"> <iframe src="https://www.iorad.com/player/1984835/cartridgeInvestigatR-Tutorial-3--Manual-Deletion-tab?src=iframe&oembed=1" width="100%" height="500px" style="width: 100%; height: 800px; " referrerpolicy="strict-origin-when-cross-origin" frameborder="0" webkitallowfullscreen="webkitallowfullscreen" mozallowfullscreen="mozallowfullscreen" allowfullscreen="allowfullscreen" allow="camera; microphone"></iframe> </p>'
      )))
      
    })
    
  })
  
  #################################### Code for Preprocess tab
  
  # show popup when the user clicks the Help button
  observeEvent(input$automaticPreprocessHelp,
               {
                 
                 showModal(modalDialog(
                   title = "Help: Automatic Preprocess Tab",easyClose = TRUE,
                   strong("Why would I use this tab?"),
                   "Automatically preprocess cartridge case scans using a sequence of algorithms.",
                   br(),
                   br(),
                   strong("What do I need to do before using this tab?"),
                   "To have uploaded scans in the Import tab.",
                   br(),
                   br(),
                   strong("How do I use this tab?"),
                   "Click the 'Add another preprocessing step' button to add a preprocessing step to the sequence.",
                   "Select a preprocessing step from the 'Select Preprocessing Step' dropdown.",
                   "Choose parameters associated with the preprocessing step.",
                   br(),
                   br(),
                   "Add as many preprocessing steps as you'd like by clicking 'Add another preprocessing step.'",
                   "Once you're happy with the preprocessing procedure, click the 'Perform Automatic Preprocessing' button.",
                   "You may reset the sequence of steps by clicking 'Reset.'",
                   br(),
                   br(),
                   strong("What is next?"),
                   "If there are regions of a scan that you would like to manually remove, move onto the 'Manual Deletion' tab.",
                   "Otherwise, if you are happy with the preprocessed scans, then continue onto the Comparing stage.",
                 ))
                 
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
  
  # code to render Tutorial
  observeEvent(input$importTutorialButton,{
    
    output$importTutorial <- renderUI({
      
      return(HTML(paste0('<div style="max-width: 100%; width: 800px; margin: 0 auto;"> <div style="display: none;"> <p style="display: none; text-align: center; margin-top: 10px;"> <i style="font-style: italic; font-weight: bold; color: #CCCCCC; font-size: 18px;">5 STEPS</i> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 1. Click <b style",
                         '="font-weight:normal;color:#FF00D6">Select a folder containing x3p files</b> </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984732&step_number=1&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 1 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 2. Select a folder containing x3p files to upload </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984732&step_number=2&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 2 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 3. After selecting a folder, click <b style",
                         '="font-weight:normal;color:#FF00D6">Select</b> to upload the scans </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984732&step_number=3&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 3 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 4. If the uploaded scans are already preprocessed (they are not in the example shown), then click <b style",
                         '="font-weight:normal;color:#FF00D6">Skip Automatic Preprocessing.</b> </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984732&step_number=4&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 4 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 5. Otherwise, if the scans require automatic preprocessing, proceed to the <b style",
                         '="font-weight:normal;color:#FF00D6">Automatic Preprocess </b>tab. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984732&step_number=5&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 5 image" /> </p> </div> </div> <h3 style="display: none; font-size: 18px; margin-top: 89px; margin-bottom: 15px;">',
                         "Here's an interactive tutorial </h3> <i style",
                         '="display: none; font-size: 15px; margin-top: 0; margin-bottom: 43px;">** Best experienced in Full Screen (click the icon in the top right corner before you begin) **</i> <p style="display: none;"> <a href="https://www.iorad.com/player/1984732/cartridgeInvestigatR-Tutorial-1--Import-tab">https://www.iorad.com/player/1984732/cartridgeInvestigatR-Tutorial-1--Import-tab</a> </p> <p style="border: 0; min-width: 100%; margin-bottom: 0; height: 801px;"> <iframe src="https://www.iorad.com/player/1984732/cartridgeInvestigatR-Tutorial-1--Import-tab?src=iframe&oembed=1" width="100%" height="500px" style="width: 100%; height: 800px; " referrerpolicy="strict-origin-when-cross-origin" frameborder="0" webkitallowfullscreen="webkitallowfullscreen" mozallowfullscreen="mozallowfullscreen" allowfullscreen="allowfullscreen" allow="camera; microphone"></iframe> </p>'
      )))
      
    })
    
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
  
  # code to render Tutorial
  observeEvent(input$autoPreprocessTutorialButton,{
    
    output$autoPreprocessTutorial <- renderUI({
      
      return(HTML(paste0('<div style="max-width: 100%; width: 800px; margin: 0 auto;"> <div style="display: none;"> <p style="display: none; text-align: center; margin-top: 10px;"> <i style="font-style: italic; font-weight: bold; color: #CCCCCC; font-size: 18px;">22 STEPS</i> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 1. After uploading x3p scans that require preprocessing, click on the <b style",
                         '="font-weight:normal;color:#FF00D6">Automatic Preprocess </b>tab. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=1&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 1 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 2. Add sequential preprocessing steps by clicking <b style",
                         '="font-weight:normal;color:#FF00D6">Add another preprocessing step </b>an arbitrary number of times. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=2&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 2 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 3. Select a preprocessing step from the drop-down menu. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=3&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 3 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 4. If you select <b style",
                         '="font-weight:normal;color:#FF00D6">Crop, </b>then choose which <b style="font-weight:normal;color:#FF00D6">Region</b> of the scan you wish to crop and an <b style="font-weight:normal;color:#FF00D6">Offset</b> value to change the amount of cropping. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=4&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 4 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 5. Select a <b style",
                         '="font-weight:normal;color:#FF00D6">Region</b> from the dropdown. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=5&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 5 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 6. Selecting <b style",
                         '="font-weight:normal;color:#FF00D6">Exterior</b> will crop the outside of the cartridge case primer. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=6&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 6 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 7. Enter a number into the <b style",
                         '="font-weight:normal;color:#FF00D6">Offset </b>field to change the amount of cropping (a negative value will remove more of the primer observations from the scan when<b style="font-weight:normal;color:#FF00D6"> Region </b>is set to "Exterior"). </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=7&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 7 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 8. Continue adding preprocessing steps by selecting <b style",
                         '="font-weight:normal;color:#FF00D6">Add another preprocessing step</b> </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=8&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 8 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 9. Selecting <b style",
                         '="font-weight:normal;color:#FF00D6">Interior</b> for a <b style="font-weight:normal;color:#FF00D6">Crop</b> preprocessing step will remove observations in the firing pin impression region. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=9&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 9 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 10. A positive value will remove more of the primer observations from the scan when<b style",
                         '="font-weight:normal;color:#FF00D6"> Region </b>is set to "Interior". </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=10&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 10 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 11. The <b style",
                         '="font-weight:normal;color:#FF00D6">Level </b>preprocessing step removes the global trend from a scan by subtracting an estimated conditional statistic (Median or Mean). </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=11&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 11 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 12. Select a conditional <b style",
                         '="font-weight:normal;color:#FF00D6">Statistic</b> to estimate and remove in the scan. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=12&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 12 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 13. A <b style",
                         '="font-weight:normal;color:#FF00D6">Filter</b> preprocessing step applied a Gaussian filter </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=13&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 13 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 14. Select a type of Gaussian filter (a bandpass or lowpass). </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=14&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 14 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 15. Enter number(s) into the <b style",
                         '="font-weight:normal;color:#FF00D6">Wavelengths</b> field. Use one value for a lowpass filter and two comma-separated values for a bandpass filter. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=15&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 15 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 16. A <b style",
                         '="font-weight:normal;color:#FF00D6">Downsample </b>preprocess step reduces the dimension of the scan. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=16&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 16 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 17. Enter a whole number into the <b style",
                         '="font-weight:normal;color:#FF00D6">Stride</b> field. Larger values reduce the size of scan more. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=17&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 17 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 18. Once you're happy with the preprocessing steps, click <b style",
                         '="font-weight:normal;color:#FF00D6">Perform Automatic Preprocessing</b>. Preprocessing may take a few minutes to complete. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=18&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 18 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 19. A visualization of the preprocessed scans will appear at the bottom of the page. Inspect this plot to determine whether alternative or further processing is required. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=19&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 19 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 20. If you're happy with the preprocessing performed, you may continue onto the <b style",
                         '="font-weight:normal;color:#FF00D6">Comparing</b> stage. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=20&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 20 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 21. If would like to change the automatic preprocessing steps, click the Reset button to start over. </p> <p style",
                         '="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=21&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 21 image" /> </p> <p style',
                         "='font-size: 15px; line-height: 136%; margin-top: 59px; margin-bottom: 51px;'> 22. Continue to the <b style",
                         '="font-weight:normal;color:#FF00D6">Manual Deletion</b> tab if you wish to manually remove regions of the scan. </p> <p style="text-align: center;"> <img src="https://www.iorad.com/api/tutorial/stepScreenshot?tutorial_id=1984814&step_number=22&width=800&height=600&mobile_width=450&mobile_height=400&apply_resize=true&min_zoom=0.5" style="max-width: 100%;max-height: 100%;border: none;" alt="Step 22 image" /> </p> </div> </div> <h3 style="display: none; font-size: 18px; margin-top: 89px; margin-bottom: 15px;">',
                         "Here's an interactive tutorial </h3> <i style",
                         '="display: none; font-size: 15px; margin-top: 0; margin-bottom: 43px;">** Best experienced in Full Screen (click the icon in the top right corner before you begin) **</i> <p style="display: none;"> <a href="https://www.iorad.com/player/1984814/cartridgeInvestigatR-Tutorial-2--Automatic-Preprocess-tab">https://www.iorad.com/player/1984814/cartridgeInvestigatR-Tutorial-2--Automatic-Preprocess-tab</a> </p> <p style="border: 0; min-width: 100%; margin-bottom: 0; height: 801px;"> <iframe src="https://www.iorad.com/player/1984814/cartridgeInvestigatR-Tutorial-2--Automatic-Preprocess-tab?src=iframe&oembed=1" width="100%" height="500px" style="width: 100%; height: 800px; " referrerpolicy="strict-origin-when-cross-origin" frameborder="0" webkitallowfullscreen="webkitallowfullscreen" mozallowfullscreen="mozallowfullscreen" allowfullscreen="allowfullscreen" allow="camera; microphone"></iframe> </p>'
      )))
      
    })
    
  })
  
  ################################################# Code for Comparison Parameters tab
  
  # show popup when the user clicks the Help button
  observeEvent(input$comparisonParametersHelp,
               {
                 
                 showModal(modalDialog(
                   title = "Help: Comparison Parameters Tab",easyClose = TRUE,
                   strong("Why would I use this tab?"),
                   "To compare two processed cartridge cases.",
                   br(),
                   br(),
                   strong("What do I need to do before using this tab?"),
                   "Preprocess uploaded scans in the Preprocessing stage or click the 'Skip Automatic Preprocessing' button in the 'Import' tab.",
                   br(),
                   br(),
                   strong("How do I use this tab?"),
                   "Select a reference scan to be divided into a grid of cells.",
                   "Select a target scan to which each reference cell will be compared.",
                   "Set various parameters for the cell-based comparison procedure.",
                   "Once you are happy with the comparison procedure parameters, click the 'Perform Comparison' button.",
                   br(),
                   br(),
                   strong("What is next?"),
                   "Move on to the ",
                   # 'Comparison Results - Summary' tab to explore the distribution of the similarity features or the 
                   "'Comparison Results - Individual Cells' tab to study the registration of specific cells."
                 ))
                 
               })
  
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
    
    # updateSelectInput(session = session,
    #                   inputId = "customCellSelection",
    #                   choices = c("",input$referenceSelect))
    
    updateSelectInput(session = session,
                      inputId = "comparisonSummary_rotations",
                      choices = thetas,
                      selected = thetas)
    
    updateSelectInput(session = session,
                      inputId = "cmcMethodReferenceSelect",
                      choices = c("",input$referenceSelect))
    
    updateSelectInput(session = session,
                      inputId = "cmcMethodTargetSelect",
                      choices = c("",input$targetSelect))
    
    # reset all of the UI in the comparison Results, Custom Cell, and Cell
    # Trajectories if-needed
    # output$postComparisonPlot <- NULL
    # output$targetScanCellPlot <- NULL
    # output$cellComparisonPlot <- NULL
    # output$customCellFullScanPlot <- NULL
    # output$targetScanCustomCellPlot <- NULL
    # output$customCellComparisonPlot <- NULL
    # output$cellTrajectoryFullScanPlot <- NULL
    # output$cellTrajectoryAnimation <- NULL
    # output$comparisonSummary_histograms <- NULL
    # output$cmcMethodPlot <- NULL
    
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
      
      # updateSelectInput(session = session,
      #                   inputId = "customCellSelection",
      #                   choices = c("",input$referenceSelect,input$targetSelect))
      
      updateSelectInput(session = session,
                        inputId = "comparisonSummary_referenceSelect",
                        choices = c("",input$referenceSelect,input$targetSelect))
      
      updateSelectInput(session = session,
                        inputId = "cmcMethodReferenceSelect",
                        choices = c("",input$referenceSelect,input$targetSelect))
      
      updateSelectInput(session = session,
                        inputId = "cmcMethodTargetSelect",
                        choices = c("",input$referenceSelect,input$targetSelect))
      
    }
    
  })
  
  ################################### code for Comparison Results - Summary tab
  
  # show popup when the user clicks the Help button
  observeEvent(input$comparisonResultsSummaryHelp,
               {
                 
                 showModal(modalDialog(
                   title = "Help: Comparison Results - Summary Tab",easyClose = TRUE,
                   strong("Why would I use this tab?"),
                   "To explore the distribution of similarity features extracted from the comparison procedure executed in the 'Comparison Parameters' tab.",
                   br(),
                   br(),
                   strong("What do I need to do before using this tab?"),
                   "Click the 'Perform Comparison' button in the 'Comparison Parameters' tab and wait for the comparison to finish.",
                   br(),
                   br(),
                   strong("How do I use this tab?"),
                   "Choose a reference scan that was divided into a grid of cells in the comparison procedure (possibly one or two choices, depending on if the 'Compare in both directions' checkbox was clicked in the previous tab).",
                   "The plots that appear depict the similarity features extracted during the cell-based comparison procedure.",
                   br(),
                   br(),
                   "In the top-left is a scatterplot of the estimated translations for each cell (represented as points), faceted by rotation angle.",
                   br(),
                   br(),
                   "In the bottom-left is a dot plot of the features that maximize the pairwise-complete correlation for each cell.",
                   "Consider these features as each cell's top vote for its registration in the target scan",
                   br(),
                   br(),
                   "In the top-right is a visualization of the reference scan with an overlaid cell grid.",
                   br(),
                   br(),
                   "In the bottom-right is a visualization of the target scan with cells located at the registration that maximizes the pairwise-complete correlation.",
                   br(),
                   br(),
                   "All four of these plots are interactable: clicking on one element will highlight elements in the other plots corresponding to the same cell.",
                   br(),
                   br(),
                   strong("What is next?"),
                   # "To study the registration of individual cells, move onto the 'Comparison Results - Individual Cells' tab.",
                   "To draw your own cells on a scan, move onto the 'Custom Cell' tab.",
                   "Otherwise, if you would like to measure the similarity between the two compared cartridge cases, move onto the Scoring stage."
                 ))
                 
               })
  
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
  
  # show popup when the user clicks the Help button
  observeEvent(input$comparisonResultsIndividualCellsHelp,
               {
                 
                 showModal(modalDialog(
                   title = "Help: Comparison Results - Individual Cells Tab",easyClose = TRUE,
                   strong("Why would I use this tab?"),
                   "To explore the registrations of individual reference cells.",
                   # "For example, in the 'Comparison Results - Summary' tab you may identify cells that exhibit erratic behavior",
                   "Use this tab to zoom into a cell and identify potential causes for notable behavior.",
                   br(),
                   br(),
                   strong("What do I need to do before using this tab?"),
                   "Click the 'Perform Comparison' button in the 'Comparison Parameters' tab and wait for the comparison to finish.",
                   br(),
                   br(),
                   strong("How do I use this tab?"),
                   " Choose a reference scan that was divided into a grid of cells in the comparison procedure (possibly one or two choices, depending on if the 'Compare in both directions' checkbox was clicked in the previous tab).",
                   br(),
                   br(),
                   "Click on a cell that appears in the plot below to visualize its registration in the other scan.",
                   "A plot will appear showing the cell's alignment in the other scan.",
                   "Next to this will appear a comparison plot showing the similarities and differences between the reference cell and patch in the target scan to which the reference cell aligned.",
                   br(),
                   br(),
                   "These visualizations may help explain why a cell aligns to a specific region in the target scan.",
                   "For example, the comparison plot separates each cell pair, shown in the left column, into similarities, shown in the middle column, and differences, shown in the right column.",
                   br(),
                   br(),
                   strong("What is next?"),
                   "If you would like to draw your own cells on a reference and target scan, move onto the 'Custom Cell' tab.",
                   "Otherwise, if you would like to measure the similarity between the two compared cartridge cases, move onto the Scoring stage."
                 ))
                 
               })
  
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
  
  
  ##################################  Code for the Cell Trajectories tab
  
  
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
      
      otherScan <- input$cellTrajectoryScan
      
      compData <- comparisonData_refToTarget
      
    }
    # otherwise there are two disticnt scans selected
    else{
      
      otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$cellTrajectoryScan)]
      
      comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
      
      compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$cellTrajectoryScan)]]
      
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
  
  
  ######################################### Code for the Custom Cell tab
  
  # show popup when the user clicks the Help button
  observeEvent(input$customCellHelp,
               {
                 
                 showModal(modalDialog(
                   title = "Help: Custom Cell Tab",easyClose = TRUE,
                   strong("Why would I use this tab?"),
                   "To draw your own cell on a reference scan to compare to a target scan",
                   "For example, you may identify a region of interest on a scan after a visual inspection (e.g., in the Preprocessing stage) or after studying the comparison results (e.g., using the 'Comparison Results' tabs)",
                   br(),
                   br(),
                   strong("What do I need to do before using this tab?"),
                   "Preprocess uploaded scans in the Preprocessing stage or click the 'Skip Automatic Preprocessing' button in the 'Import' tab.",
                   br(),
                   br(),
                   strong("How do I use this tab?"),
                   "Select the type of cell that you would like to draw (either rectangular or hand-drawn).",
                   "Then select a reference scan upon which you will draw your custom cell.",
                   "Select a target scan to which the custom cell will be compared.",
                   "Choose a range of rotation values for the comparison.",
                   br(),
                   br(),
                   "If you chose to draw a rectangular cell, then click and drag your cursor on the reference scan plot that appears.",
                   br(),
                   br(),
                   "Alternatively, if you chose a hand-drawn cell, then click and drag your cursor on the reference scan plot to zoom into a region of the scan.",
                   "Left-click on the zoomed-in plot that appears below to place a point.",
                   "Place three or more points to create a region that will be compared to the target scan.",
                   "To start over, click the 'Reset Hand-drawn Cell' button.",
                   br(),
                   br(),
                   "Once you are happy with the custom cell, click the 'Compare Custom Cell' button.",
                   "A plot will appear showing the cell's alignment in the other scan.",
                   "Next to this will appear a comparison plot showing the similarities and differences between the reference cell and patch in the target scan to which the reference cell aligned.",
                   br(),
                   br(),
                   strong("What is next?"),
                   "You can return to either of the 'Comparison Results' tabs to identify other cells or regions of interest.",
                   "Otherwise, if you would like to measure the similarity between the two compared cartridge cases, move onto the Scoring stage."
                 ))
                 
               })
  
  
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
  
  
  #################################### Code for Congruent Matching Cells tab
  
  # show popup when the user clicks the Help button
  observeEvent(input$congruentMatchingCellsHelp,
               {
                 
                 showModal(modalDialog(
                   title = "Help: Congruent Matching Cells Tab",easyClose = TRUE,
                   strong("Why would I use this tab?"),
                   "To measure the similarity between two cartridge cases using the Congruent Matching Cells method.",
                   br(),
                   br(),
                   strong("What do I need to do before using this tab?"),
                   "Click the 'Perform Comparison' button in the 'Comparison Parameters' tab and wait for the comparison to finish.",
                   br(),
                   br(),
                   strong("How do I use this tab?"),
                   "Choose a reference scan that was divided into a grid of cells in the comparison procedure (possibly one or two choices, depending on if the 'Compare in both directions' checkbox was clicked in the previous tab).",
                   "Also select parameters for the Congruent Matching Cells method.",
                   "Click the 'Visualize CMCs' button to show the reference and target scans with overlaid Congruent Matching Cells (CMCs) and non-CMCs.",
                   br(),
                   br(),
                   strong("What is next?"),
                   "The Congruent Matching Cells count is a measure of similarity between the two cartridge cases.",
                   "The larger the CMC count, the more similar the scans.",
                   "Use the CMC count to inform your evidentiary conclusion."
                 ))
                 
               })
  
  observeEvent(input$cmcPlotExecute,{
    
    output$cmcMethodPlot <- renderPlot({
      
      req(shiny.r$data)
      req(shiny.r$comparisonData_refToTarget)
      
      # browser()
      
      comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
      
      req(input$cmcMethodReferenceSelect)
      # req(input$cmcMethodTargetSelect)
      selectedScan <- input$cmcMethodReferenceSelect
      # if we're performing a self-comparison...
      if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
        
        otherScan <- input$cmcMethodReferenceSelect
        
        compData <- comparisonData_refToTarget
        
      }
      # otherwise there are two disticnt scans selected
      else{
        
        otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$cmcMethodReferenceSelect)]
        
        comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
        
        compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$cmcMethodReferenceSelect)]]
        
      }
      
      tmp <- isolate(shiny.r$data)
      
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
      
      compData <- compData %>%
        mutate(cmcClassif = cmcR::decision_CMC(cellIndex = cellIndex,
                                               x=x,
                                               y=y,
                                               theta=theta,
                                               corr = pairwiseCompCor,
                                               # corr=ifelse(input$corrSelection == "Pairwise-Complete Correlation",!!("pairwiseCompCor"),!!("fft_ccf")),
                                               xThresh = input$translationThreshold,
                                               thetaThresh = input$rotationThreshold,
                                               corrThresh = input$corrThreshold))
      
      cmcs <- compData %>%
        filter(cmcClassif == "CMC")
      
      nonCMCs <- compData %>%
        filter(!(cellIndex %in% cmcs$cellIndex)) %>%
        group_by(cellIndex) %>%
        filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
        ungroup()
        # group_split() %>%
        # map_dfr(function(dat){
        #   
        #   if(nrow(dat) > 0){
        #     
        #     # corDat <- ifelse(input$corrSelection == "Pairwise-Complete Correlation",dat$pairwiseCompCor,dat$fft_ccf)
        #     
        #     return(dat %>%
        #              slice(which.max(corDat)))
        #     
        #   }
        #   
        # })
      
      plt <- cmcR::cmcPlot(reference = reference,
                           target = target,
                           cmcClassifs = bind_rows(cmcs,nonCMCs),
                           cmcCol = "cmcClassif")
      
      return(plt)
      
    })
    
  })
  
}
