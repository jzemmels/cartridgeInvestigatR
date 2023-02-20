library(shiny)
library(shinydashboard)
library(fresh)

library(ggiraph)
library(shinyBS)
library(shinyjs)

source("code/cartridgeInvestigatR_helpers.R")

knitr::plot_crop("images/workflowDiagram.png")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "cartridgeInvestigatR",
                  titleWidth = 300,
                  tags$li(class = "dropdown",
                          tags$li(actionButton(inputId = "info",
                                               label = "About",
                                               icon = icon("info")),
                                  class = "dropdown"),
                          tags$li(uiOutput(outputId = "app_help_ui"),
                                  class = "dropdown"),
                          tags$li(actionButton(inputId = "app_hardReset",
                                               icon = fontawesome::fa_i("times"),
                                               style="color: #fff; background-color: #ff4122; border-color: #c61a09",
                                               label = "Reset App"),
                                  class = "dropdown")
                  )),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     id = "stages",
                     HTML(paste0(
                       "<br>",
                       "<a href='https://forensicstats.org/'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='csafe-logo-90.png' width = '186'></a>",
                       "<br>",
                       "<a href='https://github.com/jzemmels/cartridgeInvestigatR'><p style='text-align:center'>cartridgeInvestigatR</p></a>"
                     )),
                     # menuItem("About the App", tabName = "info", icon = icon("info")),
                     menuItem("Import + Pre-process", icon = icon("edit"), tabName = "preprocessing",selected = TRUE),
                     menuItemOutput(outputId = "comparing_ui"),
                     menuItemOutput(outputId = "scoring_ui"),
                     menuItemOutput(outputId = "exporting_ui")
                     # br(),
                     # menuItem("Explore", tabName = "comparing",icon = icon("pencil-ruler")),
                     # menuItem("3. Scoring", tabName = "decisionRule",icon = icon("stats",lib = "glyphicon"))
                     
                   )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML('
                              .navbar-custom-menu {
                              position: absolute;
                              display: inline-block;
                              margin-top: 5px;
                              }
                              
                              .navbar-custom-menu li {
                              margin-left: 15px;
                              }
                              
                              body {min-height: 1000px !important; }'))),
    shinybusy::add_busy_spinner(spin = "circle",height = "75px",width = "75px",color = "black"),
    tabItems(
      
      # tabItem(tabName = "info",
      #         
      #         
      # ),
      # Second tab content
      tabItem(tabName = "preprocessing",
              tabsetPanel(id = "preprocessingTabs",selected = TRUE,
                          tabPanel(icon = fontawesome::fa_i("file-import"),
                                   title = "Import",
                                   br(),
                                   column(width = 3,
                                          shinyFiles::shinyDirButton("x3pdir", 
                                                                     icon = fontawesome::fa_i("folder"),
                                                                     width = 300,
                                                                     style="color: #000; background-color: #ffe9a2; border-color: #f8d775; border-width: medium",
                                                                     "Select a folder containing x3p files", 
                                                                     "Upload"),
                                          br(),
                                          br(),
                                          uiOutput(outputId = "import_nextStep_ui")),
                                   column(width = 9,
                                          plotOutput("pltInitX3P")),
                                   # ,box(width = 12,
                                   # actionButton(inputId = "importTutorialButton",label = "Show Tutorial"),
                                   # uiOutput(outputId = "importTutorial"))
                                   
                          ), # end file Import tab
                          
                          tabPanel(title = "Automatic Pre-process",
                                   icon = fontawesome::fa_i("fas fa-cut"),
                                   box(
                                     h4('Add as many sequential pre-processing steps as you would like by pressing "Add Another Pre-processing Step" and changing the settings.'),
                                     # br(),
                                     h4('Press "Perform Automatic Pre-processing" to execute the pre-processing steps.'),
                                     h5(""),
                                     div(id = 'placeholder'),
                                     h5(""),
                                     fluidRow(column(width = 2,
                                                     actionButton("insertBtn", 
                                                                  label = "Add Another Pre-processing Step",
                                                                  width = 300,
                                                                  icon = fontawesome::fa_i("plus"))),
                                              column(width = 2,
                                                     actionButton("restartPreprocess",
                                                                  label = "Reset All Pre-processing Steps",
                                                                  width = 300,
                                                                  style="color: #fff; background-color: #ff4122; border-color: #c61a09",
                                                                  icon = fontawesome::fa_i("trash")))),
                                     br(),
                                     uiOutput("preProcessExecute_ui"),
                                     width = NULL
                                   ),
                                   column(width = 3,uiOutput("automaticPreprocess_nextStep_ui")),
                                   column(width = 9,plotOutput("preProcessedPlot")),
                                   # increase height of app:
                                   div(style = "width: 100%; height: 90vh")
                          ), # end Automatic preprocess tab
                          
                          tabPanel(title = "Manual Deletion - Select a Scan",
                                   icon = fontawesome::fa_i("hand-scissors"),
                                   column(width = 3,
                                          box(width = 12,align = "center",
                                              selectInput(inputId = "manualDeletionSelection",
                                                          label = "Select a processed x3p to annotate",
                                                          choices = ""),
                                              bsTooltip("manualDeletionSelection",title = 'Click "Skip Pre-processing" or "Perform Pre-processing" on the previous tabs before using this tab.'),
                                              shinyjs::hidden(actionButton(inputId = "manualDeletion_selectToAnnotate",
                                                                           icon = fontawesome::fa_i("long-arrow-right"),
                                                                           width = 300,
                                                                           "Next: Start Annotating")),
                                              br(),
                                              br(),
                                              actionButton(inputId = "manualDeletion_goToComparison",
                                                           icon = icon("pencil-ruler"),
                                                           width = 300,
                                                           style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                                                           label = "I would like to compare these scans"))),
                                   column(width = 9,plotOutput("manualDeletion_selectPlot",width = "100%",height = "100%"))
                          ), # end Manual Deletion - Select a Scan tab
                          
                          tabPanel(title = "Manual Deletion - Annotate Scan",
                                   icon = fontawesome::fa_i("hand-scissors"),
                                   column(width = 3,
                                          box(width = 12,align = "center",
                                              selectInput(inputId = "manualDeletionSelection_gray",
                                                          choices = "",
                                                          label = "Select a processed x3p to annotate"),
                                              shinyjs::hidden(actionButton(inputId = "manualDeletion_annotateToConfirmation",
                                                                           width = 300,
                                                                           icon = fontawesome::fa_i("long-arrow-right"),
                                                                           label = "Next: Preview Annotations")),
                                              br(),
                                              br(),
                                              actionButton(inputId = "manualDeletion_annotateRestart",
                                                           style="color: #fff; background-color: #ec9006; border-color: #d24e01",
                                                           width = 300,
                                                           icon = fontawesome::fa_i("redo"),
                                                           label = "Choose a Different Scan"))),
                                   box(width = 9,
                                       h4("Draw a rectangle on the scan plot to zoom-in on a particular region"),
                                       uiOutput(outputId = "manualDeletion_annotateOptions"),
                                       br(),
                                       splitLayout(cellWidth = c("40%","40%","19%"),
                                                   plotOutput("manualDeletion_annotatePlot",
                                                              brush = "manualProcZoom"),
                                                   plotOutput("manualDeletion_annotatePlot_zoom",
                                                              click = "pointPlacement_zoom"),
                                                   shiny::tableOutput(outputId = "x3p1_selectedPolygons")),
                                       tags$script("Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                                                            Shiny.onInputChange(variableName, null);
                                                                            });"))
                          ), #end Manual Deletion - Select a Scan tab
                          
                          tabPanel(title = "Manual Deletion - Preview Final Annotations",
                                   column(width = 3,
                                          box(width = 12,align = "center",
                                              selectInput(inputId = "manualDeletionSelection_gray2",choices = "",
                                                          label = "Select a processed x3p to annotate"),
                                              actionButton(inputId = "manualDeletion_confirmAnnotations",
                                                           style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                                                           icon = fontawesome::fa_i("check"),
                                                           width = 300,
                                                           label = "I'm Happy with these Annotations"),
                                              br(),
                                              br(),
                                              actionButton(inputId = "manualDeletion_previewRestart",
                                                           style="color: #fff; background-color: #ec9006; border-color: #d24e01",
                                                           icon = fontawesome::fa_i("redo"),
                                                           width = 300,
                                                           label = "Choose a Different Scan"))),
                                   box(width = 9,
                                       plotOutput(outputId = "manualDeletion_deletionPreview_plot",
                                                  width = "100%",height = "100%"))
                          ), # end Manual Deletion - Preview Final Annotations tab
                          
                          tabPanel("Pre-processing Completed",
                                   h3("You have completed the pre-processing phase."),
                                   h3('Move to the Explore or Score tabs to continue comparing the scans you have imported.'),
                                   h4("The Explore tab is intended to analyze specific regions of scans and understand the behavior of the Cell Grid Comparison procedure."),
                                   h4('The Score tab will allow you to calculate model-based similarity scores for two cartridge cases. You can apply the Automatic Cartridge Evidence Scoring (ACES) algorithm to estimate the probability that two scans match or the Congruent Matching Cells (CMC) method to count the number of "congruent" regions between two scans.'),
                                   h3("Move to the Export tab at any time when you would like to download scans/results."),
                                   h3("Press the Reset App button to start over from scratch.")
                          )# end Preprocessing Completed tab
                          
                          
              ) # end pre-processing tabsetPanel
              
              
      ), #end preprocessing tabItem
      
      # third tab content
      tabItem(tabName = "comparing",
              tabsetPanel(
                id = "comparingTabs",
                
                tabPanel("Cell Grid Comparison",
                         icon = fontawesome::fa_i("sliders-h"),
                         # br(),
                         fluidRow(
                           br(),
                           column(2,
                                  box(width = 12,
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
                                                    value = TRUE),
                                      bsTooltip("bothDirectionsCheck",title = "After reference-to-target comparison, divide target scan into cells and compare to reference"),
                                      actionButton(inputId = "previewComparison",
                                                   icon = fontawesome::fa_i("eye"),
                                                   "Preview Comparison(s)"),
                                      br(),
                                      br(),
                                      actionButton(inputId = "comparisonSettingsMenuButton",
                                                   icon = fontawesome::fa_i("cog"),
                                                   label = "Settings"),
                                      br(),
                                      div(id = "comparisonSettingsMenu",
                                          textInput(inputId = "numCells",
                                                    label = "Cells Grid Size (use comma separation)",
                                                    value = "4,4"),
                                          bsTooltip("numCells",title = "Enter number of cells in grid"),
                                          numericInput(inputId = "maxNonMissingProp",
                                                       label = "Maximum Proportion of NAs per Cell",
                                                       value = .99,
                                                       min = .0001,
                                                       max = .9999),
                                          bsTooltip("maxNonMissingProp",title = "Enter maxmimum proportion that can be missing in a cell to be considered in comparison"),
                                          numericInput(inputId = "cellRegionProp",
                                                       label = "Sidelength ratio between target regions and reference cells",
                                                       value = 2,min = 1),
                                          numericInput(inputId = "thetaRangeMin",
                                                       label = "Minimum Rotation Value (degrees)",
                                                       min = -181,
                                                       max = 181,
                                                       value = -30),
                                          bsTooltip("thetaRangeMin",title = "Enter minimum rotation to be considered (between -180 and 180 degrees)"),
                                          numericInput(inputId = "thetaRangeMax",
                                                       label = "Maximum Rotation Value (degrees)",
                                                       min = -181,
                                                       max = 181,
                                                       value = 30),
                                          bsTooltip("thetaRangeMax",title = "Enter maximum rotation to be considered (greater than minimum, less than 180 degrees)"),
                                          numericInput(inputId = "thetaStep",
                                                       label = "Rotation Step Size (degrees)",
                                                       value = 3,
                                                       step = 1,
                                                       min = 1),
                                          bsTooltip("thetaStep",title = "Enter distance  between consecutive rotations (in degrees)")),
                                      br(),
                                      shiny::actionButton("comparisonButton",
                                                          icon = fontawesome::fa_i("play"),
                                                          style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                                                          label = "Perform Comparison")
                                      # ,bsTooltip(id = "comparisonButton",title = "Execute comparison procedure under selected parameters")
                                  )
                           ),
                           column(10,
                                  column(width = 5,align = "center",
                                         uiOutput("comparison1text_ui"),
                                         plotOutput(outputId = "preComparisonReference"),
                                         plotOutput(outputId = "preComparisonWholeScanTarget")),
                                  conditionalPanel(condition = "input.bothDirectionsCheck",
                                                   column(width = 5,align = "center",
                                                          uiOutput("comparison2text_ui"),
                                                          plotOutput(outputId = "preComparisonTarget"),
                                                          plotOutput(outputId = "preComparisonWholeScanReference")),
                                  )
                           )
                         ),
                         
                         
                         
                ), # end Comparison Settings tab
                
                tabPanel(title = "Results Summary",
                         icon = fontawesome::fa_i("chart-bar"),
                         br(),
                         column(width = 2,
                                box(width = 12,
                                    selectInput(inputId = "comparisonSummary_referenceSelect",
                                                label = "Choose a reference scan",
                                                choices = NULL),
                                    checkboxGroupInput(inputId = "comparisonSummary_featureSelect",
                                                       label = "Select visual summaries",
                                                       c("Cell-wise translations by rotations scatterplots",
                                                         "Cell-wise registrations dot plots"
                                                         # ,"Registration-based features",
                                                         # "Density-based features",
                                                         # "Visual diagnostic features"
                                                       ),
                                                       selected = c("Cell-wise translations by rotations scatterplots",
                                                                    "Cell-wise registrations dot plots")))
                         ),
                         column(width = 10,
                                girafeOutput(outputId = "comparisonSummary_histograms",width = "100%",height = 2000)
                         ),
                         # increase height of app:
                         div(style = "width: 100%; height: 90vh")
                         
                ), # end Results Summary tab
                
                tabPanel("Individual Cell Results",
                         icon = fontawesome::fa_i("search-plus"),
                         br(),
                         fluidRow(
                           column(width = 2,
                                  box(width = 12,
                                      selectInput("postComparisonScanSelect",
                                                           label = "Select a scan to view",
                                                           choices = NULL),
                                      h4("Click on a cell to see where it aligns in the other scan"))),
                           column(width = 10,
                                  align = "center",
                                  fluidRow(column(width = 5,
                                                  plotOutput(outputId = "postComparisonPlot",click = "postComparisonClick")),
                                           column(width = 5,
                                                  plotOutput(outputId = "targetScanCellPlot"))),
                                  fluidRow(
                                    column(width = 10,
                                           plotOutput(width = 700,height = 500,outputId = "cellComparisonPlot")))),
                           # increase height of app:
                           div(style = "width: 100%; height: 90vh"))
                ), #end Individual Cell Results tab
                
                tabPanel("Custom Cell",
                         icon = fontawesome::fa_i("microscope"),
                         br(),
                         # br(),
                         column(2,
                                box(width = 12,
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
                                    actionButton(inputId = "customCellMenuButton",
                                                 icon = fontawesome::fa_i("cog"),
                                                 label = "Settings"),
                                    div(id = "customCellMenu",
                                        numericInput(inputId = "cellRegionProp_customCell",
                                                     label = "Sidelength ratio between target regions and reference cells",
                                                     value = 2,min = 1),
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
                                                         actionButton(inputId = "resetHandDrawnCustomCell","Reset Hand-drawn Cell"))),
                                    br(),
                                    br(),
                                    actionButton(inputId = "customCellExecute",
                                                 style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                                                 icon = icon("play"),
                                                 label = "Compare Custom Cell"),
                                    bsTooltip("customCellExecute",title = "Compare custom cell to other cartridge case"))
                         ),
                         h4("Draw your own cell on the scan."),
                         column(width = 10,
                                fluidRow(column(width = 5,
                                                plotOutput(outputId = "customCellFullScanPlot",
                                                           brush = "customCellBrush")),
                                         column(width = 5,
                                                plotOutput(outputId = "customHandDrawnCell",
                                                           click = "customHandDrawnCellClick"))),
                                fluidRow(
                                  column(width = 5,
                                         plotOutput(outputId = "targetScanCustomCellPlot")),
                                  column(width = 5,
                                         plotOutput(outputId = "customCellComparisonPlot")))),
                         # increase height of app:
                         div(style = "width: 100%; height: 90vh")
                ) # end Custom Cell tab
                
              ) # end Comparing tabsetPanel
      ), # end comparing tabItem
      
      
      
      tabItem(tabName = "scoring",
              fluidRow(column(width = 2,
                              box(width = 12,
                                  div(id = "score_scanPreview_ui",
                                      selectInput(inputId = "score_referenceSelect",
                                                  label = "Select a Reference Scan",
                                                  choices = ""),
                                      selectInput(inputId = "score_targetSelect",
                                                  label = "Select a Target Scan",
                                                  choices = ""),
                                      shinyjs::disabled(actionButton(inputId = "score_previewScans",
                                                                     icon = fontawesome::fa_i("eye"),
                                                                     label = "Preview Scans"))))),
                       column(width = 10,
                              column(width = 5,
                                     plotOutput(outputId = "aces_referencePreview")),
                              column(width = 5,
                                     plotOutput(outputId = "aces_targetPreview")))),
              fluidRow(tabsetPanel(id = "scoringTabs",
                                   column(width = 2,
                                          box(width = 12,uiOutput(outputId = "scoreSettings_ui")),
                                          # increase height of app:
                                          div(style = "width: 100%; height: 90vh")),
                                   tabPanel(title = "ACES Algorithm",
                                            id = "acesTab",
                                            icon = icon("fas fa-calculator"),
                                            column(width = 10,
                                                   column(width = 7,
                                                          plotOutput(outputId = "aces_matchProbPlot"),
                                                          plotOutput(outputId = "aces_trainingFeatureDistribution",height = 700)),
                                                   column(width = 3,
                                                          htmlOutput(outputId = "aces_matchProbText"),
                                                          tableOutput(outputId = "aces_featureTable")
                                                   ))),
                                   tabPanel(title = "Congruent Matching Cells",
                                            id = "cmcTab",
                                            icon = fontawesome::fa_i("fas fa-th"),
                                            column(width = 10,
                                                   column(width = 5,align = "center",
                                                          uiOutput("cmcMethodInformation_refToTarget"),
                                                          plotOutput(outputId = "cmcMethodPlot_refToTarget",height = 1000)),
                                                   column(width = 5,align = "center",
                                                          uiOutput("cmcMethodInformation_targetToRef"),
                                                          plotOutput(outputId = "cmcMethodPlot_targetToRef",height = 1000)))),
                                   # increase height of app:
                                   div(style = "width: 100%; height: 90vh")
              )
              )
      ),
      
      tabItem(tabName = "exporting",
              column(width = 12,uiOutput(outputId = "export_checkboxUI")))
      
    )
  )
)
