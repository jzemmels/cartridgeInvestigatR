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
        "<a href='https://github.com/jzemmels/cartridgeInvestigatR'><p style='text-align:center'>cartridgeInvestigatR</p></a>"
      )),
      menuItem("0. Introduction", tabName = "info", icon = icon("info")),
      menuItem("1. Preprocessing", icon = icon("scissors"), tabName = "data_related"),
      # menuItem("Analysis", icon = icon("pencil-ruler"), startExpanded = TRUE,
      #          menuSubItem("Comparison", tabName = "comparison"),
      #          menuSubItem("Scoring", tabName = "decisionRule")),
      menuItem("2. Comparing", tabName = "comparison",icon = icon("pencil-ruler")),
      menuItem("3. Scoring", tabName = "decisionRule",icon = icon("stats",lib = "glyphicon"))
      
    )
  ),
  dashboardBody(shinybusy::add_busy_spinner(spin = "circle",height = "75px",width = "75px",color = "black"),
  #               tags$script("
  #   Shiny.addCustomMessageHandler('resetValue', function(variableName) {
  #     Shiny.onInputChange(variableName, null);
  #   });
  # "),
  tabItems(
    
    tabItem(tabName = "info",
            h1(strong("Welcome to cartridgeInvestigatR!")),
            h4("Use this app to compare 3D topographical scans of cartridge cases."),
            h4("An overview of the app functionality is provided below."),
            h4("For more information, visit ",HTML('<a href="https://github.com/jzemmels/cartridgeInvestigatR">https://github.com/jzemmels/cartridgeInvestigatR</a>'),"."),
            br(),
            
            h2(strong("0. Introduction")),
            h4("This application uses computer algorithms to process and compare scans of cartridge cases.",
               'A', tags$i("cartridge case"),' is the metal casing that houses the bullet and gunpowder prior to firing.',
               'When a gun is fired, as the bullet moves down the barrel, the cartridge case moves backwards and slams against the back wall of the barrel (a.k.a. the ',tags$i("breech face"),') with considerable force.',
               'Any markings on the breech face are "stamped" into the surface of the cartridge case.',
               'This leaves so-called ',HTML("<i><a href='https://www.firearmsid.com/A_CCIDImpres.htm'>breech face impressions</a></i>"),' that forensic examiners use to identify the gun from which a cartridge case was fired.',
               "Think of these impressions as analogous to a gun's",'"fingerprint" left on the cartridge case.',
               "The computer algorithms used in this app compare the breech face impressions on two cartridge cases."),
            # br(),
            h4("The purpose of this app is to allow individuals to engage with these algorithms without needing to program.",
               "If you are interested in cartridge case identification, but do not have expertise in programming, specifically in the R programming language, then this app is for you.",
               "For more information about the computer algorithms used in this application, visit ",HTML("<a href='https://github.com/CSAFE-ISU/cmcR'>https://github.com/CSAFE-ISU/cmcR</a>"),"."),
            # br(),
            h4("To use this app, you must have cartridge case scans stored on your computer as ",HTML("<a href='https://tsapps.nist.gov/NRBTD/Home/DataFormat'>.x3p files</a>"),".",
               "X3P (XML 3D Surface Profile) is an ISO standard file format for saving cartridge case scans.",
               "You can download example .x3p files containing cartridge case scan from the ",HTML("<a href='https://tsapps.nist.gov/NRBTD/'>NIST Ballistics Toolmark Research Database</a>.")),
            # br(),
            h4("The functionality of this app encompasses three stages of the cartridge case comparison procedure: preprocessing, comparing, and scoring.",
               "These stages are separated into the three tabs that you can see on the left sidebar.",
               "You must complete one stage before moving onto the next.",
               "Following is a description of each stage and how to complete it."),
            br(),
            
            h2(strong("1. Preprocessing")),
            h4("Breech face impressions are commony found on the annular region surrounding the firing pin impression on cartridge case primer.",
               "The algorithms will not work properly if regions other than the breech face impression region are left in the scan.",
               'As such, before comparing two cartridge cases, scans are often "preprocessed" to isolate the breech face impression region.',
               "The Preprocessing tab provides both manual and automatic tools to preprocess a cartridge case scan to isolate the breech face impressions."),
            
            h3(strong("Import")),
            h4("Use this tab to upload x3p files to the application.",
               'Click the "Select a folder containing x3p files" button to open a file system explorer and select a folder containing x3p files.',
               "Once the files are successfully uploaded, you will see a visualization of the cartridge case scans below.",
               'At this point, if the uploaded cartridge case scans are already preprocessed to your liking, you may click the "Skip Preprocessing" button and move on to the Comparison stage.',
               'Otherwise, move on to the ',strong("Manual Deletion")," or ",strong("Preprocess")," tabs to perform preprocessing."),
            
            h3(strong("Manual Deletion")),
            h4("The automatic preprocessing alorithms available in this app may not work perfectly to isolate the breech face impression region in a scan.",
               "You may wish to manually annotate a region of the scan, such as primer roll-off, to delete it from the scan",
               "The steps to manually annotate a scan are:"),
            h4(HTML("<ol>"),
               HTML("<li>"),"Select a scan to annotate from the drop-down. A plot of the selected scan will appear on the right.",HTML("</li>"),
               HTML("<li>"),"Click and drag your cursor to draw a rectangle on the plot. A zoomed-in visualization of the selected region will appear below.",HTML("</li>"),
               HTML("<li>"),'Click on the zoomed-in visualization to place a point. When three or more points are placed, they will be connected to form a region. Pressing "Reset Region" will remove the points for the current region. You may start a new region by drawing a new rectangle on the plot at the top of the page.',HTML("</li>"),
               HTML("<li>"),'Once you are happy with the annotations, press the "Confirm Annotations" button to "lock" them into the scan. A 3D rendering will appear to the right showing the annotated regions in red. Pressing the "Delete Annotations" button will reset all annotations for the selected scan.',HTML("</li>"),
               HTML("<li>"),"You can select a new scan to annotate from the drop-down or move on to the ",strong("Preprocess"),' tab if you are finished manually annotating. Note that selecting a new scan from the drop-down before pressing "Confirm Annotations" for the currently-selected scan will remove all annotations - remember to click "Confirm Annotations" before moving on.',HTML("</li>"),
               HTML("</ol>")),
            
            h3(strong("Preprocess")),
            h4("Use this tab to apply automatic preprocessing algorithms to the uploaded x3p files.",
               "You may add an arbitrary number of preprocessing steps to be performed sequentially.",
               'Press the "Perform Preprocessing" button once you are happy with the preprocessing procedure or "Reset" to remove all steps.',
               "The list of possible preprocessing steps is:"),
            h4(HTML("<ul>"),
               HTML("<li>"),tags$u("Downsample:")," Decrease the dimension of scans by sampling every [Stride] rows/columns (e.g., Stride = 2 means every other row/column is selected).",HTML("</li>"),
               HTML("<li>"),tags$u("Crop:")," Remove primer roll-of on the exterior or interior of the breech face impression (BFI) region. The function estimates the radius of the selected [Region]. This estimate can be increased or decreased by setting the [Offset] parameter to a positive or negative value, respectively. You will likely need to experiment with different offset values to find one that isolates the region of interest.",HTML("</li>"),
               HTML("<li>"),tags$u("Level:")," Remove the global trend in a scan by fitting and subtracting a conditional [Statistic] plane to the surface.",HTML("</li>"),
               HTML("<li>"),tags$u("Erode:")," Supplementary/alternative to cropping the exterior or interior of the BFI region, apply the morphological operation of erosion with a set mask [Radius] value. A larger radius leads to more erosion.",HTML("</li>"),
               HTML("<li>"),tags$u("Filter:")," Apply a low/high/bandpass Gaussian filter with set [Wavelength] cutoff(s) to the filter surface.",HTML("</li>"),
               HTML("<li>"),tags$u("Delete:")," Remove the manually-annotated region(s) created in the ",strong("Manual Deletetion")," tab.",HTML("</li>"),
               HTML("</ul>")),
            
            h3(strong("Note")),
            h4('To complete the Preprocessing stage, you must click either the "Skip Preprocessing" button in the ',strong("Import"),' tab or the "Perform Preprocessing" button in the ',strong("Preprocess")," tab.",
               "The comparison procedure will not be available if you do not click one of these two buttons."),
            br(),
            
            h2(strong("2. Comparing")),
            h4("After preprocessing scans, the next step is to compare them and extract similarity features.",
               "Use this tab to automatically compare two cartridge cases and explore the distributions of the similarity features.",
               "The comparison algorithm used in this tab is based on the ",tags$i("Congruent Matching Cells")," (CMC) method introduced in Song (2013).",
               'Briefly, this algorithm begins by selecting a "Reference" scan to divide into a grid of cells.',
               'Each Reference cell is allowed to roam the surface of a second, "Target" scan to identify its matching position.',
               "The Target scan is rotated by a range of angles where for each angle, each Reference cell determines the translation at which it maximizes the ",tags$i("cross-correlation function")," (CCF) in the Target scan.",
               "Five similarity features are collected for each Reference cell and rotation: the translation at which the CCF is maximized, the maxmimum CCF value, and the pairwise-complete correlation between the Reference cell and the Target scan.",
               'Ultimately, we are interested in determining whether multiple cells come to a "consensus" on the translation and rotation at which the correlation is maxmimized.',
               "A consensus reached amongst multiple cells is evidence to suggest that the cartridge case pair matches."),
            h3(strong("Comparison Parameters")),
            h4("Use this tab to set parameters for and execute the cell-based comparison procedure described above.",
               "The steps to excecute the comparison procedure are as:"),
            h4(HTML("<ol>"),
               HTML("<li>"),"Select Reference and Target scans from the associated drop-down menus. A visualization of the Reference scan divided into a grid of cells and the selected Target scan will appear to the right. Some cells in the Reference scan will be grayed-out. These are cells containing a proportion of missing values that exceed the [Max. Proportion of NAs per Cell] threshold.",HTML("</li>"),
               HTML("<li>"),'Change the cell grid by entering comma-separeted integers into the "Cell Grid Size" field.',HTML("</li>"),
               HTML("<li>"),'Enter a value between 0 and 1 into the "Max. Proportion of NAs per Cell" field. Cells that exceed this threshold are excluded from the comparison procedure. Enter a value of 0.999 to include any cells containing non-NA values.',HTML("</li>"),
               HTML("<li>"),'Change the range of rotations considered for the Target scan by entering a minimum/maximum angle into the appropriate fields. The [Rotation Step Size] field controls the distance between successive angles (e.g., a step size of 3 implies a rotation grid of -30, -27, -24, etc. degrees)',HTML("</li>"),
               HTML("<li>"),'Click the "Compare in both directions" if, after the selected Reference cells are compared to the selected Target scan, you wish to break the Target scan into a grid of cells and compare those to the Reference scan. This will effectively double the processing time.',HTML("</li>"),
               HTML("<li>"),'Once you are happy with the comparison parameters, press the "Perform Comparison" button to initiate the comparison procedure. A progress bar will appear until the comparison procedure concludes. After the progress bar disappears, you may move on to the other Comparing tabs or the Scoring stage.',HTML("</li>"),
               HTML("</ol>")),
            
            h3(strong("Comparison Results - Summary")),
            h3(strong("Comparison Results - Individual Cells")),
            h3(strong("Cell Trajectories")),
            h3(strong("Custom Cell")),
            br(),
            
            h2(strong("3. Scoring"))
            # ,
            # h4(" Original Method of Song (2013): visualize the CMCs identified under the Original Method of Song (2013)."),
            # h4("\t- The diagnostic plots show the x, y, theta, and CCF values at which each cell/region pair attained its CCF across all rotations considered in the comparison."),
            # h4(" High CMC Method: visualize the CMCs identified under the High CMC method."),
            # h4("\t- The diagnostic plot(s) show(s) the CMC-theta distribution used to identify High CMCs.")
            
            
            
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
              tabPanel("Manual Deletion",
                       
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
              tabPanel("Comparison Results - Summary",
                       column(width = 2,
                              selectInput(inputId = "comparisonSummary_referenceSelect",label = "Choose a reference scan",choices = NULL),
                              selectInput(inputId = "comparisonSummary_rotations",label = "Choose rotations to focus on",
                                          choices = NULL,multiple = TRUE),
                              numericInput(inputId = "comparisonSummary_jitterAmount",label = "Scatterplot Jitter Value",value = 0,min = 0),
                              actionButton(inputId = "comparisonSummary_rePlot",label = "Re-Draw Plots")),
                       column(width = 10,
                              girafeOutput(outputId = "comparisonSummary_histograms",width = "1300px",height = "1300px")
                              ),
                       
              ),
              tabPanel("Comparison Results - Individual Cells",
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
