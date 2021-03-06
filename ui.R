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
    tabItems(
      
      tabItem(tabName = "info",
              h2("Welcome to cartridgeInvestigatR!"),
              h4("cmcR in an interactive way to process and compare cartridge cases. Below is a description of each tab."),
              br(),
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
                           shinyDirButton("x3pdir", "Select a folder containing x3p files", "Upload"),
                           # p("some text..."), 
                           width = NULL, title = "Import x3p", solidHeader = TRUE),
                         
                         box(
                           shinyFilesButton("file1", "Select a rds file (your bullet object)", 
                                            "Please select a rds file (your bullet object); column x3p/grooves is expected", 
                                            multiple = FALSE, viewtype = "detail"),
                           verbatimTextOutput("file1_prompt"),
                           width = NULL, title = "Import rds", solidHeader = TRUE),
                         
                         box(
                           p("If your create shiny.tt in your current environment ..."),
                           width = NULL, title = "From R Session", solidHeader = TRUE),
                         box(
                           actionButton(inputId = "skipPreprocessing",
                                        label = "Skip Preprocessing"),
                           conditionalPanel(condition = "input$skipPreprocessing",textOutput(outputId = "skipPreprocessingConfirm"))
                         ),
                         
                ),
                tabPanel("Initial X3P",
                         
                         br(),
                         br(),
                         
                         fluidRow(
                           box(x3pActionButtonUI("x3p_show_xml", "Show x3p dimensions (px)"),
                               width = 12)
                         ),
                         
                         # fluidRow(
                         #   box(x3pActionButtonUI("x3p_flip", "Flip y!"),
                         #       width = 12)
                         # ),
                         
                         fluidRow(
                           box(fluidRow(column(1,
                                               conditionalPanel(
                                                 condition = "output.hasname_x3p",
                                                 actionButton("displayx3p2", "Display x3ps")
                                               ),
                           ),
                           column(3,
                                  conditionalPanel(
                                    condition = "output.hasname_x3p",
                                    numericInput(inputId = "plotSampling", 
                                                 label = "Downsample for Plotting",
                                                 value = 4,
                                                 min = 1)
                                  ),
                           )
                           ),
                           width = 12
                           )
                         ),
                         fluidRow(
                           plotOutput("x3prgl2"),
                           width = 12
                         ),
                         br(),
                         br(),
                         br(),
                         br(),
                         shinyjs::useShinyjs(),
                         actionButton("toggleIndividualX3P","Focus on a Single X3P"),
                         div(id = "individualX3P_panel",
                             fluidRow(
                               column(5,
                                      selectInput(inputId = "individualX3P",
                                                  label = "X3P",
                                                  choices = "1",
                                                  selected = "1"),
                                      shiny::verbatimTextOutput("userDefinedCartridgeCaseCenter"),
                                      shiny::verbatimTextOutput("userDefinedFiringPinCenter"),
                                      actionButton(inputId = "resetProcessing",label = "Reset Changes")),
                               column(7,plotOutput("x3prgl_individual",
                                                   click = "clickExterior",
                                                   dblclick = "dbclickInterior",
                                                   brush = "removeBrush")),
                               width = 12
                             )) %>% shinyjs::hidden(),
                ),
                
                tabPanel("Preprocess",
                         
                         box(
                           # fluidRow(column(3,
                           #                 actionButton(inputId = "skipPreprocessing",
                           #                              label = "Skip Preprocessing"),
                           #                 conditionalPanel(condition = "input$skipPreprocessing",textOutput(outputId = "skipPreprocessingConfirm"))
                           # ),
                           # ),
                           fluidRow(column(3,numericInput(inputId = "initialDownsample",
                                                          label = "Initial Downsampling",
                                                          value = 1,
                                                          min = 1))),
                           fluidRow(column(3,numericInput(inputId = "exteriorCrop",
                                                          label = "Exterior Cropping Offset",
                                                          value = 0,
                                                          min = 0))),
                           fluidRow(column(3,numericInput(inputId = "interiorCrop",
                                                          label = "Interior Cropping Offset",
                                                          value = 0,
                                                          min = 0))),
                           fluidRow(column(3,numericInput(inputId = "gaussFilterLow",
                                                          label = "Gaussian Filter Lowpass Wavelength",
                                                          value = 16,
                                                          min = 0)),
                                    column(3,numericInput(inputId = "gaussFilterHigh",
                                                          label = "Gaussian Filter Highpass Wavelength",
                                                          value = 500,
                                                          min = 0))),
                           fluidRow(column(3,numericInput(inputId = "finalDownsample",
                                                          label = "Final Downsampling",
                                                          value = 2,
                                                          min = 1))),
                           fluidRow(column(3,x3pActionButtonUI("prepare_shinytt", "Perform Preprocessing"),
                                           p(" (This might take some time...)"))),
                           width = NULL
                         ),
                         fluidRow(
                           box(fluidRow(column(1,
                                               conditionalPanel(
                                                 condition = "output.hasname_x3p",
                                                 actionButton("displayx3p3", "Display x3ps")
                                               ),
                           )),
                           width = 12
                           )
                         ),
                         fluidRow(
                           plotOutput("x3prgl3"),
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
                                  numericInput(inputId = "numCells",
                                               label = "Number of Cells",
                                               value = 64,
                                               min = 1),
                                  numericInput(inputId = "maxNonMissingProp",
                                               label = "Max. Proportion of NAs Per Cell",
                                               value = .85,
                                               min = 0,
                                               max = 1),
                                  checkboxInput(inputId = "preRotateScans",label = "Rotate by Estimated Angle First",value = FALSE),
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
                                  checkboxInput(inputId = "rotationSelection",label = "Manually Enter Rotations",value = FALSE),
                                  # conditionalPanel("input.rotationselection == 'manual'",
                                  textInput(inputId = "thetaManualInput",
                                            label = "Comma-Separated Rotation Values (deg)",
                                            value = NULL), #delete comma if conditionalPanel works
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
                         
                         fluidRow(column(4,
                                         plotOutput(outputId = "postComparisonReference",click = "reference_click"),
                                         conditionalPanel(condition = "input.bothDirectionsCheck",
                                                          plotOutput(outputId = "postComparisonTarget",click = "target_click"))),
                                  column(8,
                                         plotOutput(outputId = "cellPlot"),
                                         plotOutput(outputId = "regionPlots",click = "region_click"),
                                         plotOutput(outputId = "comparisonDiagnosticPlots"))
                         ),
                )
                
              ),
              
      ),
      
      
      
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



























