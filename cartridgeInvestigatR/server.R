library(patchwork)
library(cmcR)
library(tidyverse)
library(impressions)
library(shinyFiles)
library(shinybusy)
# remotes::install_github("https://github.com/thomasp85/shinyFiles.git")

server = function(input, output, session) {
  
  ## load the main part of the server
  # check the location after installation of the package
  ################ important!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # source("inst/R/server_main.R", local = TRUE)
  # source("inst/R/server_sig_compare.R", local = TRUE)
  source("cartridgeInvestigatR_helpers.R")
  
  # if shiny.tt exits in the current environment, add a comment
  # interpolate(~("# abv"),
  #             mydir = userdir,
  #             `_env` = environment(),
  #             file = "code_x3p.R",
  #             append = TRUE, eval = FALSE)
  
  
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
                                   choices = paste0("x3p",1:nrow(tmp)))
                 
                 updateSelectInput(session = session,
                                   inputId = "x3prgl2_select",
                                   choices = paste0("x3p",1:nrow(tmp)),
                                   selected = ifelse(nrow(tmp) > 1,"x3p2","x3p1"))
                 
                 updateSelectInput(session = session,
                                   inputId = "referenceSelect",
                                   choices = c("",paste0("x3p",1:nrow(tmp))))
                 
                 updateSelectInput(session = session,
                                   inputId = "targetSelect",
                                   choices = c("",paste0("x3p",1:nrow(tmp))))
                 
               })
  
  # observeEvent(input$plotUnits,{
  #     
  #     req(shiny.r$data)
  #     
  #     tmp <- isolate(shiny.r$data)
  #     
  #     req(nrow(tmp) > 0)
  #     
  #     tmp <- tmp %>%
  #         mutate(x3p = map(x3p,function(scan){
  #             
  #             if(input$plotUnits == "Meter"){
  #                 
  #                 scan$surface.matrix <- scan$surface.matrix*1e6
  #                 scan$header.info$incrementY <- scan$header.info$incrementY*1e6
  #                 scan$header.info$incrementX <- scan$header.info$incrementX*1e6
  #                 
  #                 return(scan)
  #             }
  #             else{
  #                 return(scan)
  #             }
  #             
  #         }))
  #     
  #     shiny.r$data <<- tmp
  #     
  # })
  
  # on the Compare X3Ps tab, this will show two RGL widgets of the two scans
  observeEvent(input$x3prgl_execute,{
    
    req(shiny.r$data)
    req(nrow(shiny.r$data) > 0)
    tmp <- isolate(shiny.r$data)
    
    # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Rendering plots...")
    
    output$x3prgl1 <- rgl::renderRglwidget(expr = {
      
      tmp %>%
        mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
        filter(x3pName == input$x3prgl1_select) %>%
        pull(x3p) %>% 
        .[[1]] %>%
        x3ptools::image_x3p(zoom = .7,col = "gray80")
      
      rgl::rglwidget()
      
    })
    
    output$x3prgl2 <- rgl::renderRglwidget(expr = {
      
      tmp %>%
        mutate(x3pName = paste0("x3p",1:nrow(.))) %>%
        filter(x3pName == input$x3prgl2_select) %>%
        pull(x3p) %>% 
        .[[1]] %>%
        x3ptools::image_x3p(zoom = .7,col = "gray80")
      
      rgl::rglwidget()
      
    })
    
    # # shinybusy::remove_modal_spinner()
    
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
        fluidRow(column(4,selectInput(Id()('letter',ctn()), 'Select Preprocessing Step', c("Downsample","Crop","Level","Erode","Filter"),selected = NULL,multiple = FALSE)),
                 column(8,uiOutput(Id()('input',ctn()))))
      )
    )
    
  })
  
  observeEvent(ctn(), {
    
    ctn <- ctn()
    
    id <- Id()('input',ctn)
    selection <- Id()('letter',ctn)
    
    # browser()
    
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
                          textInput(Id()('params2_',ctn), 'Wavelength(s) - Use comma separation for Bandpass',value = "16, 500"))
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
    
    # browser()
    
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
    
    # # shinybusy::remove_modal_spinner()
    
  })
  
  # plot the x3p_processed scans on the Preprocess tab
  observeEvent(input$preProcessDisplay,{
    
    tmp <- isolate(shiny.r$data)
    
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
    
    cellPlt_refToTarget <<- surfaceMat_df %>%
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
      
      cellPlt_targetToRef <<- surfaceMat_df %>%
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
    
    # browser()
    
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
                      inputId = "postComparisonScanSelect",
                      choices = c("",input$referenceSelect))
    
    updateSelectInput(session = session,
                      inputId = "customCellSelection",
                      choices = c(input$referenceSelect))
    
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
                        inputId = "customCellSelection",
                        choices = c(input$referenceSelect,input$targetSelect))
    }
    
  })
  
  # on the comparison results tab, the user will select an x3p to explore
  output$postComparisonPlot <- renderPlot({
    
    req(shiny.r$comparisonData_refToTarget)
    req(input$postComparisonScanSelect)
    
    # # shinybusy::show_modal_spinner(spin = "fingerprint", text = "Preparing Selected Scan")
    
    comparisonData_refToTarget <- isolate(shiny.r$comparisonData_refToTarget)
    
    if(input$bothDirectionsCheck){
      
      comparisonData_targetToRef <- isolate(shiny.r$comparisonData_targetToRef)
      
      # compData <- list(comparisonData_refToTarget,comparisonData_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$postComparisonScanSelect)]]
      cellPlt <- list(cellPlt_refToTarget,cellPlt_targetToRef)[[which(c(input$referenceSelect,input$targetSelect) == input$postComparisonScanSelect)]]
      
      return(cellPlt +
               scale_alpha_manual(values = c(1,0)))
    }
    else{
      # compData <- comparisonData_refToTarget
      return(cellPlt_refToTarget +
               scale_alpha_manual(values = c(1,0)))
    }
    
    # tmp <- isolate(shiny.r$data)
    # 
    # cellGrid <- input$numCells %>%
    #   stringr::str_split(",") %>%
    #   .[[1]] %>%
    #   purrr::map_int(as.integer)
    # 
    # referenceCell_df <- tmp %>%
    #   mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
    #   filter(x3pNames == input$postComparisonScanSelect) %>%
    #   pull(x3p_processed) %>%
    #   .[[1]] %>%
    #   cmcR::comparison_cellDivision(numCells = cellGrid)
    # 
    # # this creates a "blank" data frame representing the full extent of the
    # # selected cell
    # surfaceMat_df <- referenceCell_df %>%
    #   group_by(cellIndex) %>%
    #   group_split() %>%
    #   purrr::map_dfr(function(dat){
    #     
    #     x3pToDF(dat$cellHeightValues[[1]]) %>%
    #       mutate(cellIndex = unique(dat$cellIndex))
    #     
    #   }) %>%
    #   select(-value)
    # 
    # # we want to show the cells that were used in the comparison and the
    # # other elements as white. this comparedCellValues df will be joined to
    # # surfaceMat_df
    # comparedCellValues <- compData %>%
    #   select(cellIndex,cellHeightValues) %>%
    #   distinct() %>%
    #   group_by(cellIndex) %>%
    #   group_split() %>%
    #   purrr::map_dfr(function(dat){
    #     
    #     x3pToDF(dat$cellHeightValues[[1]]) %>%
    #       mutate(cellIndex = unique(dat$cellIndex),
    #              comparedCell = TRUE)
    #     
    #   })
    # 
    # surfaceMat_df <- surfaceMat_df %>%
    #   left_join(comparedCellValues,
    #             by = c("x","y","cellIndex")) %>%
    #   mutate(comparedCell = ifelse(is.na(comparedCell),FALSE,TRUE)) %>%
    #   tidyr::separate(col = cellIndex,into = c("rowInd","colInd"),sep = ", ",remove = FALSE) %>%
    #   mutate(rowInd = as.factor(rowInd),
    #          colInd = as.factor(colInd)) 
    # 
    # cellPlt <- surfaceMat_df %>%
    #   ggplot(aes(x=x,y=y,fill = value,alpha = comparedCell)) +
    #   geom_raster() +
    #   facet_grid(rows = vars(rowInd),
    #              cols = vars(colInd)) +
    #   ggplot2::scale_fill_gradientn(colours = c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb", "#f7f7f7", "#fee0b6", "#fdb863" ,"#e08214" ,"#b35806","#7f3b08"),
    #                                 values = scales::rescale(quantile(surfaceMat_df$value,c(0,.01,.025,.1,.25,.5,.75,0.9,.975,.99,1),na.rm = TRUE)),
    #                                 breaks = function(lims){
    #                                   dat <- quantile(surfaceMat_df$value,c(0,.01,.25,.5,.75,.99,1),na.rm = TRUE)
    #                                   
    #                                   dat <- dat %>%
    #                                     setNames(paste0(names(dat)," [",round(dat,3),"]"))
    #                                   
    #                                   return(dat)
    #                                 },
    #                                 na.value = "gray65") +
    #   scale_alpha_manual(values = c(0,1)) +
    #   theme_minimal() +
    #   coord_fixed(expand = FALSE) +
    #   theme(axis.title.x = ggplot2::element_blank(),
    #         axis.text.x = ggplot2::element_blank(),
    #         axis.ticks.x = ggplot2::element_blank(),
    #         axis.title.y = ggplot2::element_blank(),
    #         axis.text.y = ggplot2::element_blank(),
    #         axis.ticks.y = ggplot2::element_blank(),
    #         panel.grid.major = ggplot2::element_blank(),
    #         panel.grid.minor = ggplot2::element_blank(),
    #         panel.background = ggplot2::element_blank(),
    #         plot.title = ggplot2::element_text(hjust = .5,
    #                                            size = 11)
    #         ,legend.position = "none") +
    #   ggplot2::labs(title = input$referenceSelect) +
    #   ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = grid::unit(3,"in"),
    #                                                   label.theme = ggplot2::element_text(size = 8),
    #                                                   title.theme = ggplot2::element_text(size = 10),
    #                                                   frame.colour = "black",
    #                                                   ticks.colour = "black"),
    #                   colour =  'none')
    # 
    # # # shinybusy::remove_modal_spinner()
    # 
    # return(cellPlt)
    
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
      # filter(as.numeric(rowIndex) == (as.numeric(input$postComparisonClick$panelvar2)) & 
      #            as.numeric(colIndex) == (as.numeric(input$postComparisonClick$panelvar1))) %>%
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
  
  output$customCellFullScanPlot <- renderPlot({
    
    req(shiny.r$data)
    
    tmp <- isolate(shiny.r$data)
    
    selectedScan <- input$customCellSelection
    
    reference <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == selectedScan) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    return(x3pListPlot(list(reference) %>% set_names(selectedScan)))
    
  })
  
  observeEvent(input$customCellExecute,{
    
    req(shiny.r$data)
    req(shiny.r$comparisonData_refToTarget)
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
    
    refCell <- reference
    
    topRow <- nrow(refCell$surface.matrix) - round(input$customCellBrush$ymax)
    bottomRow <- nrow(refCell$surface.matrix) - round(input$customCellBrush$ymin)
    
    leftCol <- round(input$customCellBrush$xmin)
    rightCol <- round(input$customCellBrush$xmax)
    
    refCell$surface.matrix <- refCell$surface.matrix[topRow:bottomRow,leftCol:rightCol]
    refCell$header.info$sizeX <- nrow(refCell$surface.matrix)
    refCell$header.info$sizeY <- ncol(refCell$surface.matrix)
    
    refCell$cmcR.info$cellRange <- paste0("rows: ",topRow," - ",bottomRow,", cols: ",leftCol," - ",rightCol)
    
    # if we're performing a self-comparison...
    if(length(unique(c(input$referenceSelect,input$targetSelect))) == 1){
      
      otherScan <- input$customCellSelection
      
    }
    # otherwise there are two distinct scans selected
    else{
      
      otherScan <- c(input$referenceSelect,input$targetSelect)[which(c(input$referenceSelect,input$targetSelect) != input$customCellSelection)]
      
    }
    
    target <- tmp %>%
      mutate(x3pNames = paste0("x3p",1:nrow(.))) %>%
      filter(x3pNames == otherScan) %>%
      pull(x3p_processed) %>%
      .[[1]]
    
    thetas <- seq(from = input$thetaRangeMin,to = input$thetaRangeMax,by = input$thetaStep)
    
    show_modal_progress_line(text = paste0("Comparing custom cell to ", otherScan," at rotation ",thetas[1],"°"),value = 0)
    
    compData <- map2_dfr(thetas,
                         1:length(thetas),
                         function(theta,ind){
                           
                           dat <- comparison_customCell(refCell,target,theta,input$maxNonMissingProp)
                           
                           update_modal_progress(value = (ind)*(1/length(thetas)),
                                                 text = paste0("Comparing custom cell to ", otherScan," at rotation ",theta,"°"))
                           
                           return(dat)
                           
                         })
    
    update_modal_progress(value = 1,
                          text = paste0("Comparing custom cell to ", otherScan," at rotation ",thetas[[length(thetas)]],"°"))
    
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
      
      # plts <- cmcR::cmcPlot(reference = reference,
      #                       target = target,
      #                       cmcClassifs = alignedCellData %>%
      #                           mutate(originalMethod = "CMC"),
      #                       type = "list")
      
      plts <- cmcPlot_colorChange(reference = reference,
                                  target = target,
                                  cmcClassifs = alignedCellData %>%
                                    mutate(originalMethod = "CMC"),
                                  type = "list")
      
      # return(plts[[2]])
      return(plts[[1]] +
               scale_fill_manual(values = "black"))
      
    })
    
    remove_modal_progress()
  })
  
  
}

