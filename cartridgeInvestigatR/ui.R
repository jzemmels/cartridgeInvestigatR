library(shiny)
library(shinydashboard)
library(ggiraph)
library(shinyBS)

source("cartridgeInvestigatR_helpers.R")

ui <- dashboardPage(
  dashboardHeader(title = "cartridgeInvestigatR",
                  titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<a href='https://forensicstats.org/'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='csafe-logo-90.png' width = '186'></a>",
        "<br>",
        "<a href='https://github.com/CSAFE-ISU/cmcR'><p style='text-align:center'>cmcR</p></a>"
      )),
      menuItem("0. Introduction", tabName = "info", icon = icon("info")),
      menuItem("1. Preprocessing", icon = icon("scissors"), tabName = "data_related"),
      # menuItem("Analysis", icon = icon("pencil-ruler"), startExpanded = TRUE,
      #          menuSubItem("Comparison", tabName = "comparison"),
      #          menuSubItem("Scoring", tabName = "decisionRule")),
      menuItem("2. Comparison", tabName = "comparison",icon = icon("pencil-ruler")),
      menuItem("3. Scoring", tabName = "decisionRule",icon = icon("stats",lib = "glyphicon"))
      
    )
  ),
  dashboardBody(shinybusy::add_busy_spinner(spin = "circle",height = "75px",width = "75px",color = "black"),
                tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
  "),
  tabItems(
    
    tabItem(tabName = "info",
            h2("Welcome to cartridgeInvestigatR!"),
            h4("cmcR in an interactive way to process and compare cartridge cases."),
            br(),
            h4("Steps for comparing preprocessed scans:"),
            h4("1. Click on Select a folder containing x3p files and select a directory of already preprocessed scans."),
            h4("2. Click the Skip Preprocessing button at the bottom of the Import tab."),
            h4("3. Click to the Comparison tab on the left. Plots will be created showing the selected Reference and Target scans, although this may take some time to load."),
            h4("4. Select desired comparison parameters and click the Perform Comparison button. This may take some time to run depending on selected parameters."),
            h4("5. Under the Comparison Results tab you can click on a particular cell to see some summary information of that cell's most similar regions in the paired scan (measured by the pairwise-complete correlation)."),
            h4("6. Click to the Scoring tab to see a visualziation of the CMCs identified under the Original Method and High CMC method."),
            # br(),
            h4("Following is a description of each tab:"),
            h3(strong("1. Preprocessing")),
            h4(" Import: import a directory of scans."),
            h4("\t- Click the Skip Preprocessing button to skip the preprocessing step."),
            # br(),
            h3(strong("2. Comparison")),
            h4(" Comparison Parameters: set parameters used in the cell-based comparison procedure."),
            h4("\t- Start the comparison procedure by clicking the Perform Comparison button at the bottom of the Comparison Parameters tab."),
            h4("\t- Cells containing too many missing values (defined by Max. Proportion of NAs per Cell) will be grayed-out."),
            h4("\t- By clicking Manually Enter Rotations, only those rotations entered in the Comma-Separated Rotation Values box will be considered."),
            h4(" Comparison Results: visualize the cell/region comparisons."),
            h4("\t- Click on a cell to visualize the 3 most similar regions (as measured by the Pairwise-Complete Correlation)."),
            h4("\t- A cross-correlation map computed using the Cross-Correlation Theorem is shown below each region. These indicate how the translation values are estimated."),
            # br(),
            h3(strong("3. Scoring")),
            h4(" Original Method of Song (2013): visualize the CMCs identified under the Original Method of Song (2013)."),
            h4("\t- The diagnostic plots show the x, y, theta, and CCF values at which each cell/region pair attained its CCF across all rotations considered in the comparison."),
            h4(" High CMC Method: visualize the CMCs identified under the High CMC method."),
            h4("\t- The diagnostic plot(s) show(s) the CMC-theta distribution used to identify High CMCs.")
            
            
            
    ),
    # Second tab content
    tabItem(tabName = "data_related",
            h2("Preprocess X3P Files"),
            
            tabsetPanel(
              tabPanel("Import",
                       
                       br(),
                       br(),
                       
                       box(
                         shinyFiles::shinyDirButton("x3pdir", "Select a folder containing x3p files", "Upload"), 
                         width = NULL, title = "Import x3p", solidHeader = TRUE),
                       fluidRow(width = 12,
                                plotOutput("pltInitX3P")#,
                                # selectInput("plotUnits",label = "Scan Units",choices = c("Meters","Micrometers","Standard Deviations"))
                       ),
                       box(
                         actionButton(inputId = "skipPreprocessing",
                                      label = "Skip Preprocessing"),
                         bsTooltip("skipPreprocessing",title = "Click if scans are already preprocessed."),
                         conditionalPanel(condition = "input.skipPreprocessing",textOutput(outputId = "skipPreprocessingConfirm")),
                         verbatimTextOutput("infoInitX3P")
                       ),
                       
              ),
              tabPanel("Manual Annotation",
                       
                       fluidRow(column(width = 3,
                                       selectInput(inputId = "x3prgl1_select",label = "1. Select x3p to Annotate",choices = ""),
                                       bsTooltip("x3prgl1_select",title = "Select an X3P to manually annotate."),
                                       textOutput(outputId = "zoomInMessage")),
                                column(width = 9,plotOutput("x3p1_ggplot",brush = "manualProcZoom"))),
                       fluidRow(column(width = 3,
                                       textOutput("regionClickMessage"),
                                       uiOutput(outputId = "editPolygonSelect_ui"),
                                       textOutput("newRegionMessage"),
                                       br(),
                                       uiOutput(outputId = "regionResetButton_ui")),
                                column(width = 5,
                                       plotOutput("x3p1_ggplot_zoom",click = "pointPlacement_zoom")),
                                column(width = 4,
                                       shiny::tableOutput(outputId = "x3p1_selectedPolygons")),
                                tags$script("
                                            Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                            Shiny.onInputChange(variableName, null);
                                            });
                                            ")),
                       fluidRow(column(width = 3,
                                       br(),
                                       textOutput(outputId = "confirmationMessage"),
                                       br(),
                                       uiOutput(outputId = "annotationConfirmationButton_ui"),
                                       br(),
                                       textOutput(outputId = "postConfirmationMessage"),
                                       br(),
                                       uiOutput(outputId = "deleteAnnotationsButton_ui"),
                                       br(),
                                       textOutput(outputId = "deleteAnnotationsMessage")),
                                column(width = 9,
                                       rgl::rglwidgetOutput(outputId = "x3p1_rgl",width = "512px",height = "512px"))),
                       h4("Open FiX3P: ",a("https://talenfisher.github.io/fix3p/",href = "https://talenfisher.github.io/fix3p/"))
              ),
              
              tabPanel("Preprocess",
                       
                       box(
                         fluidRow(column(actionButton("insertBtn", "Add another preprocessing step"),width = 4),
                                  column(actionButton("restartPreprocess","Reset"),width = 4),
                                  bsTooltip("restartPreprocess",title = "Remove all preprocessing steps")),
                         h5(""),
                         div(id = 'placeholder'),
                         h5(""),
                         fluidRow(column(width = 3,
                                         actionButton("preProcessExecute",label = "Perform Preprocessing"),
                                         bsTooltip(id = "preProcessExecute",title = "Execute selected preprocessing steps"))),
                         width = NULL
                       ),
                       fluidRow(
                         box(fluidRow(column(1,
                                             conditionalPanel(
                                               condition = "output.hasname_x3p",
                                               actionButton("preProcessDisplay", "Display x3ps")
                                             ),
                         )),
                         width = 12
                         )
                       ),
                       fluidRow(
                         plotOutput("preProcessedPlot"),
                         width = 12
                       )
                       
              )
              
              
            ),
            
            
    ),
    
    # First tab content
    tabItem(tabName = "comparison",
            # h3("Select a Bullet Land"),
            
            tabsetPanel(
              
              tabPanel("Comparison Parameters",
                       fluidRow(
                         column(2,
                                br(),
                                selectInput(inputId = "referenceSelect",
                                            label = "Select Reference Scan",
                                            choices = NULL),
                                bsTooltip("referenceSelect",title = "Select scan to be divided into a cell grid"),
                                selectInput(inputId = "targetSelect",
                                            label = "Select Target Scan",
                                            choices = NULL),
                                bsTooltip("targetSelect",title = "Select scan to which each reference cell is compared"),
                                checkboxInput(inputId = "bothDirectionsCheck",
                                              label = "Compare in both directions",
                                              value = FALSE),
                                bsTooltip("bothDirectionsCheck",title = "After reference-to-target comparison, divide target scan into cells and compare to reference"),
                                textInput(inputId = "numCells",
                                          label = "Cells Grid Size (Use comma-separation)",
                                          value = "8,8"),
                                bsTooltip("numCells",title = "Enter number of cells in grid"),
                                numericInput(inputId = "maxNonMissingProp",
                                             label = "Max. Proportion of NAs per Cell",
                                             value = .85,
                                             min = 0,
                                             max = 1),
                                bsTooltip("maxNonMissingProp",title = "Enter maxmimum proportion that can be missing in a cell to be considered in comparison"),
                                # checkboxInput(inputId = "preRotateScans",label = "Rotate by Estimated Angle First",value = FALSE),
                                #Conditional panel doesn't seem to want to work right now...
                                # radioButtons(inputId = "rotationselection",
                                #              label = "Rotation",
                                #              choices = c("by Range",
                                #                          "Manual"),
                                #              selected = "by Range"),
                                # conditionalPanel("input.rotationselection == 'by range'",
                                numericInput(inputId = "thetaRangeMin",
                                             label = "Min. Rotation Value (deg)",
                                             min = -181,
                                             max = 181,
                                             value = -30),
                                bsTooltip("thetaRangeMin",title = "Enter minimum rotation to be considered (between -180 and 180 degrees)"),
                                numericInput(inputId = "thetaRangeMax",
                                             label = "Max. Rotation Value (deg)",
                                             min = -181,
                                             max = 181,
                                             value = 30),
                                bsTooltip("thetaRangeMax",title = "Enter maximum rotation to be considered (greater than minimum, less than 180 degrees)"),
                                numericInput(inputId = "thetaStep",
                                             label = "Rotation Step Size (deg)",
                                             value = 3,
                                             min = 1),
                                bsTooltip("thetaStep",title = "Enter distance  between consecutive rotations (in degrees)"),
                                # ),
                                # checkboxInput(inputId = "rotationSelection",label = "Manually Enter Rotations",value = FALSE),
                                # # conditionalPanel("input.rotationselection == 'manual'",
                                # textInput(inputId = "thetaManualInput",
                                #           label = "Comma-Separated Rotation Values (deg)",
                                #           value = NULL), #delete comma if conditionalPanel works
                                # ),
                                uiOutput("comparisonButtonMessage")
                         ),
                         column(10,
                                fluidRow(column(5,plotOutput(outputId = "preComparisonReference")),
                                         column(5,plotOutput(outputId = "preComparisonWholeScanTarget"))),
                                conditionalPanel(condition = "input.bothDirectionsCheck",
                                                 fluidRow(column(5,plotOutput(outputId = "preComparisonTarget")),
                                                          column(5,plotOutput(outputId = "preComparisonWholeScanReference"))
                                                 )
                                )
                         )
                       ),
                       
                       
                       
              ),
              tabPanel("Comparison Results",
                       fluidRow(selectInput("postComparisonScanSelect","Select a scan to view",NULL),
                                bsTooltip("postComparisonScanSelect",title = "Select a scan that was divided into a grid of cells"),
                                h4("Click on a cell to see where it aligns in the other scan")),
                       column(4,
                              plotOutput(outputId = "postComparisonPlot",click = "postComparisonClick")
                       ),
                       column(3,
                              plotOutput(outputId = "targetScanCellPlot")),
                       column(5,
                              plotOutput(outputId = "cellComparisonPlot"))
              ),
              tabPanel("Cell Trajectories",
                       fluidRow(selectInput("cellTrajectoryScan","Select a scan to analyze",NULL),
                                bsTooltip("cellTrajectoryScan",title = "Select a scan that was divided into a grid of cells"),
                                selectInput("cellTrajectorySelections",
                                            multiple = TRUE,
                                            choices = " ",
                                            label = "Click on cells below to visualize their trajectory"),
                                bsTooltip("cellTrajectorySelections",title = "Either click directly on plot below or manually enter cell indices")),
                       column(4,
                              plotOutput(outputId = "cellTrajectoryFullScanPlot",click =  "cellTrajectoryClick"),
                              actionButton(inputId = "cellTrajectoryExecute",label = "Visualize Cell Trajectories"),
                              bsTooltip("cellTrajectoryExecute",title = "Render animation of cells trajectories"),
                              downloadButton("saveCellTrajectory","Save Animation"),
                              bsTooltip("saveCellTrajectory",title = "Save cell trajectory animation (requires clicking the Visualize button)")),
                       # column(3,
                       #        plotOutput(outputId = "allCellTrajectoryPlot")),
                       column(5,
                              imageOutput(outputId = "cellTrajectoryAnimation"))
              ),
              tabPanel("Custom Cell",
                       column(2,
                              selectInput(inputId = "customCellType",
                                          label = "Type of Cell",
                                          choices = c("Rectangular","Hand-drawn")),
                              selectInput(inputId = "customCellSelection",
                                          label = "Select Reference Scan",
                                          choices = NULL),
                              bsTooltip("customCellSelection",title = "Select scan to draw your own cell"),
                              selectInput(inputId = "targetSelect_customCell",
                                          label = "Select Target Scan",
                                          choices = NULL),
                              bsTooltip("targetSelect_customCell",title = "Select scan to which the custom cell is compared"),
                              numericInput(inputId = "thetaRangeMin_customCell",
                                           label = "Min. Rotation Value (deg)",
                                           min = -181,
                                           max = 181,
                                           value = -30),
                              bsTooltip("thetaRangeMin_customCell",title = "Enter minimum rotation to be considered (between -180 and 180 degrees)"),
                              numericInput(inputId = "thetaRangeMax_customCell",
                                           label = "Max. Rotation Value (deg)",
                                           min = -181,
                                           max = 181,
                                           value = 30),
                              bsTooltip("thetaRangeMax_customCell",title = "Enter maximum rotation to be considered (greater than minimum, less than 180 degrees)"),
                              numericInput(inputId = "thetaStep_customCell",
                                           label = "Rotation Step Size (deg)",
                                           value = 3,
                                           min = 1),
                              bsTooltip("thetaStep_customCell",title = "Enter distance  between consecutive rotations (in degrees)"),
                              conditionalPanel(condition = 'input.customCellType == "Hand-drawn"',
                                               actionButton(inputId = "resetHandDrawnCustomCell","Reset Hand-drawn Cell")),
                              br(),
                              actionButton(inputId = "customCellExecute",label = "Compare Custom Cell"),
                              bsTooltip("customCellExecute",title = "Compare custom cell to other cartridge case")
                       ),
                       h4("Draw your own cell on the scan."),
                       fluidRow(column(4,
                                       plotOutput(outputId = "customCellFullScanPlot",brush = "customCellBrush")),
                                column(4,
                                       plotOutput(outputId = "customHandDrawnCell",click = "customHandDrawnCellClick"))),
                       fluidRow(column(2,hr()),
                                column(5,
                                       plotOutput(outputId = "targetScanCustomCellPlot")),
                                column(5,
                                       plotOutput(outputId = "customCellComparisonPlot")))
              ),
              
            )),
    
    
    
    tabItem(tabName = "decisionRule",
            column(2,
                   br(),
                   br(),
                   br(),
                   numericInput(inputId = "translationThreshold",
                                label = "Translation Threshold (px)",
                                value = 20,
                                min = 0),
                   
                   numericInput(inputId = "rotationThreshold",
                                label = "Rotation Threshold (deg)",
                                value = 6,
                                min = 0,
                                max = 360),
                   numericInput(inputId = "corrThreshold",
                                label = "Correlation Threshold",
                                value = .45,
                                min = 0,
                                max = 1,
                                step = .05),
                   numericInput(inputId = "highCMCThreshold",
                                label = "High CMC Threshold (tau)",
                                value = 1,
                                min = 1)),
            column(10,
                   tabsetPanel(
                     tabPanel("Original Method of Song (2013)",
                              fluidRow(
                                plotOutput(outputId = "originalMethodCMCPlot"),
                                plotOutput(outputId = "originalMethodCMC_refToTarget_diagnosticPlots")
                              ),
                              conditionalPanel(condition = "input.bothDirectionsCheck",
                                               fluidRow(
                                                 plotOutput(outputId = "originalMethodCMCPlot_targetToRef"),
                                                 plotOutput(outputId = "originalMethodCMC_targetToRef_diagnosticPlots")
                                               )
                              )
                     ),
                     
                     tabPanel("High CMC Method",
                              fluidRow(
                                plotOutput(outputId = "highCMCPlot"),
                                plotOutput(outputId = "highCMC_refToTarget_diagnosticPlot")
                              ),
                              conditionalPanel(condition = "input.bothDirectionsCheck",
                                               fluidRow(
                                                 plotOutput(outputId = "highCMCPlot_targetToRef"),
                                                 plotOutput(outputId = "highCMC_targetToRef_diagnosticPlot")
                                               )
                              )
                     )#,
                     # tabPanel("Convergence Method",
                     #          fluidRow(
                     #            
                     #          ))
                   )
            )
    )
    
  )
  )
)
