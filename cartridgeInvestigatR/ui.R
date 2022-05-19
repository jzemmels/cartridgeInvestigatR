library(shiny)
library(shinydashboard)

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
            menuItem("1. Preprocessing", icon = icon("table"), tabName = "data_related"),
            # menuItem("Analysis", icon = icon("pencil-ruler"), startExpanded = TRUE,
            #          menuSubItem("Comparison", tabName = "comparison"),
            #          menuSubItem("Decision Rules", tabName = "decisionRule")),
            menuItem("2. Comparison", tabName = "comparison",icon = icon("pencil-ruler")),
            menuItem("3. Decision Rules", tabName = "decisionRule",icon = icon("filter"))
            
        )
    ),
    dashboardBody(
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
                    h4("6. Click to the Decision Rules tab to see a visualziation of the CMCs identified under the Original Method and High CMC method."),
                    br(),
                    h4("Following is a description of each tab:"),
                    h3(strong("1. Preprocessing")),
                    tags$pre(h4(" Import: import a directory of scans."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4("\t- Click the Skip Preprocessing button to skip the preprocessing step."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4(" Initial X3P: visualize the imported X3Ps."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4("\t- The Focus on a Single X3P button allows you to manually specify the cartridge case center (single-click) the firing pin center (double-click) or remove parts of the scan (drag)."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4(" Preprocess: perform preprocessing procedures (note: this will be changed in the future to allow for custom preprocessing)"),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    br(),
                    h3(strong("2. Comparison")),
                    tags$pre(h4(" Comparison Parameters: set parameters used in the cell-based comparison procedure."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4("\t- Start the comparison procedure by clicking the Perform Comparison button at the bottom of the Comparison Parameters tab."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4("\t- Cells containing too many missing values (defined by Max. Proportion of NAs per Cell) will be grayed-out."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4("\t- By clicking Manually Enter Rotations, only those rotations entered in the Comma-Separated Rotation Values box will be considered."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4(" Comparison Results: visualize the cell/region comparisons."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4("\t- Click on a cell to visualize the 3 most similar regions (as measured by the Pairwise-Complete Correlation)."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4("\t- A cross-correlation map computed using the Cross-Correlation Theorem is shown below each region. These indicate how the translation values are estimated."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    br(),
                    h3(strong("3. Decision Rules")),
                    tags$pre(h4(" Original Method of Song (2013): visualize the CMCs identified under the Original Method of Song (2013)."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4("\t- The diagnostic plots show the x, y, theta, and CCF values at which each cell/region pair attained its CCF across all rotations considered in the comparison."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4(" High CMC Method: visualize the CMCs identified under the High CMC method."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside")),
                    tags$pre(h4("\t- The diagnostic plot(s) show(s) the CMC-theta distribution used to identify High CMCs."),.noWS = c("before", "after", "outside", "after-begin", "before-end","inside"))
                    
                    
                    
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
                                     conditionalPanel(condition = "input$skipPreprocessing",textOutput(outputId = "skipPreprocessingConfirm")),
                                     verbatimTextOutput("infoInitX3P")
                                 ),
                                 
                        ),
                        tabPanel("Compare X3Ps",
                                 
                                 br(),
                                 br(),
                                 
                                 fluidRow(
                                     box(actionButton("x3prgl_execute",label = "Show X3Ps"),
                                         width = 12)
                                 ),
                                 fluidRow(
                                     column(style='padding:0px;',
                                         selectInput(inputId = "x3prgl1_select",label = "Select x3p",choices = NULL),
                                         rgl::rglwidgetOutput("x3prgl1"),width = 6),
                                     column(style='padding:0px;',
                                         selectInput(inputId = "x3prgl2_select",label = "Select x3p",choices = NULL),
                                         rgl::rglwidgetOutput("x3prgl2"),width = 6),
                                     width = 12),
                                 br(),
                                 br(),
                                 h4("Open FiX3P: ",a("https://talenfisher.github.io/fix3p/",href = "https://talenfisher.github.io/fix3p/"))
                        ),
                        
                        tabPanel("Preprocess",
                                 
                                 box(
                                     fluidRow(column(actionButton("insertBtn", "Add another step"),width = 4),
                                              column(actionButton("restartPreprocess","Reset"),width = 4)),
                                     h5(""),
                                     div(id = 'placeholder'),
                                     h5(""),
                                     fluidRow(column(3,actionButton("preProcessExecute",label = "Perform Preprocessing"))),
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
                                            selectInput(inputId = "targetSelect",
                                                        label = "Select Target Scan",
                                                        choices = NULL),
                                            checkboxInput(inputId = "bothDirectionsCheck",
                                                          label = "Compare in both directions",
                                                          value = FALSE),
                                            textInput(inputId = "numCells",
                                                         label = "Cells Grid Size (Use comma-separation)",
                                                         value = "8,8"),
                                            numericInput(inputId = "maxNonMissingProp",
                                                         label = "Max. Proportion of NAs per Cell",
                                                         value = .85,
                                                         min = 0,
                                                         max = 1),
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
                                                         min = -180,
                                                         max = 180,
                                                         value = -30),
                                            numericInput(inputId = "thetaRangeMax",
                                                         label = "Max. Rotation Value (deg)",
                                                         min = -180,
                                                         max = 180,
                                                         value = 30),
                                            numericInput(inputId = "thetaStep",
                                                         label = "Rotation Step Size (deg)",
                                                         value = 3,
                                                         min = 1), #delete comma if conditionalPanel works
                                            # ),
                                            # checkboxInput(inputId = "rotationSelection",label = "Manually Enter Rotations",value = FALSE),
                                            # # conditionalPanel("input.rotationselection == 'manual'",
                                            # textInput(inputId = "thetaManualInput",
                                            #           label = "Comma-Separated Rotation Values (deg)",
                                            #           value = NULL), #delete comma if conditionalPanel works
                                            # ),
                                            actionButton(inputId = "comparisonButton",
                                                         label = "Perform Comparison")
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
                                          h4("Click on a cell to see where it aligns in the other scan")),
                                 # fluidRow(
                                 column(4,
                                        plotOutput(outputId = "postComparisonPlot",click = "postComparisonClick")
                                        # plotOutput(outputId = "postComparisonReference",click = "reference_click"),
                                        # plotOutput(outputId = "postComparisonTarget",click = "target_click")
                                        ),
                                 column(3,
                                        plotOutput(outputId = "targetScanCellPlot")),
                                 column(5,
                                        plotOutput(outputId = "cellComparisonPlot"))
                                 # )
                                 # column(5,plotOutput(outputId = "alignedTargetCellPlot",click = "region_click",height="800px"))
                                 # ,plotOutput(outputId = "comparisonDiagnosticPlots")
                                 # ),
                        ),
                        tabPanel("Custom Cell",
                                 fluidRow(selectInput("customCellSelection","Select a scan to analyze",NULL),
                                                   h4("Draw your own cell on the scan.")),
                                 column(4,
                                        plotOutput(outputId = "customCellFullScanPlot",brush = "customCellBrush"),
                                        actionButton(inputId = "customCellExecute",label = "Compare Custom Cell")),
                                 column(3,
                                        plotOutput(outputId = "targetScanCustomCellPlot")),
                                 column(5,
                                        plotOutput(outputId = "customCellComparisonPlot"))
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
