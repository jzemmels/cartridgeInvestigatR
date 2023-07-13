library(shiny)
library(shinydashboard)
# library(shinydashboardPlus)
library(fresh)

library(ggiraph)
library(shinyBS)
library(shinyjs)

source("code/cartridgeInvestigatR_helpers.R")

knitr::plot_crop("images/workflowDiagram.png")

# javascript code to collapse box
# from: https://stackoverflow.com/a/49404809/14000041
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "cartridgeInvestigatR",
                  # titleWidth = 300,
                  tags$li(class = "dropdown",
                          style = "padding-left:100px",
                          tags$li(uiOutput(outputId = "app_help_ui"),
                                  class = "dropdown")#,
                          # tags$li(actionButton(inputId = "info",
                          #                      label = "About the app",
                          #                      icon = icon("info")),
                          #         class = "dropdown")
                          # tags$li(actionButton(inputId = "app_hardReset",
                          #                      icon = fontawesome::fa_i("times"),
                          #                      style="color: #fff; background-color: #ff4122; border-color: #c61a09",
                          #                      label = "Reset App"),
                          #         class = "dropdown")
                  )),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     id = "stages",
                     # HTML(paste0(
                     #   "<br>",
                     #   "<a href='https://forensicstats.org/'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='csafe-logo-90.png' width = '186'></a>",
                     #   "<br>",
                     #   "<a href='https://github.com/jzemmels/cartridgeInvestigatR'><p style='text-align:center'>cartridgeInvestigatR</p></a>"
                     # )),
                     # menuItem("About the App", tabName = "info", icon = icon("info")),
                     menuItem("Import + Pre-process", icon = icon("edit"), tabName = "preprocessing",selected = TRUE),
                     menuItemOutput(outputId = "scoring_ui"),
                     menuItemOutput(outputId = "comparing_ui"),
                     menuItemOutput(outputId = "exporting_ui"),
                     menuItem("About the App",tabName = "info",icon = icon("info"))
                     # br(),
                     # menuItem("Explore", tabName = "comparing",icon = icon("pencil-ruler")),
                     # menuItem("3. Scoring", tabName = "decisionRule",icon = icon("stats",lib = "glyphicon"))
                     
                   )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jscode,functions = c("collapse")),
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
              h2("Welcome to cartridgeInvestigatR!"),
              h4("You can use this app to compare 3D topographical scans of cartridge case surfaces.
              To begin, you can either use sample scans that come pre-loaded with the app or import scans from your own computer."),
              h4("At any time, click the Help button at the top of the app to learn about how to use the tab you're currently on.
                 You can refresh the page to reset the application's state and start over from scratch."),
              tabsetPanel(id = "preprocessingTabs",selected = TRUE, # end Manual Deletion - Preview Final Annotations tab
                          tabPanel("Pre-processing Completed",
                                   box(width = 12,
                                       h3("Nice work! You've completed the pre-processing phase and your scans are ready to be compared."),
                                       h3("Click on one of the tabs on the left to progress to another stage of the app."),
                                       h4('First, you can move to the ',strong("Score"),' tab to apply two automatic scoring algorithms.
                                   This tab is useful if you want a quick measure of similarity for two cartridge cases and are not interested in experimentation.
                                   The Automatic Cartridge Evidence Scoring (ACES) algorithm computes a similarity score ranging from 0 to 1 where larger values indicate higher similarity.
                                      The Congruent Matching Cells (CMC) method computes a whole number similarity scoring indicating the number of regions that are similar between two scans.'),
                                       h4('Alternatively, you can move to the ',strong("Explore"),' tab to do a deep-dive into the behavior of the automatic scoring algorithms.
                                      This tab is useful if you are interested in understanding or experimenting with the behavior of the algorithm for a particular pair of cartridge cases.'),
                                       h4('Once you have performed a comparison, you can move to the ',strong("Export"),' tab to download the scans and/or results computed in  the app.'))
                          ),# end Preprocessing Completed tab,
                          tabPanel(icon = fontawesome::fa_i("file-import"),
                                   title = "Import",selected = TRUE,
                                   # br(),
                                   box(width = 12,column(width = 12,
                                                         radioButtons(inputId = "appUsageChoice",
                                                                      label = "How do you want to use the app?",
                                                                      choices = c("I'm just exploring. I'll use the sample scans." = "example",
                                                                                  "I have my own scans that I want to import." = "custom")),
                                                         actionButton(inputId = "letsGoButton",
                                                                      label = "Let's Start Comparing!",
                                                                      icon = icon("flag-checkered"),
                                                                      style="color: #fff; background-color: #95bb72; border-color: #4b6043"))),
                                   fluidRow(
                                     shinyjs::hidden(div(id = "importMessage",
                                                         box(
                                                           width = 12,
                                                           column(width = 12,
                                                                  h4("First select a set of scans you'd like to work with below.
                                                                   These can either be sample scans available in the app or scans saved on your computer."),
                                                                  h4("You can preview the scans below.
                                            Before moving on to comparing the scans, you may wish to apply additional pre-processing to better isolate the breech face impressions.
                                               There are two options in cartridgeInvestigatR to pre-process scans."),
                                                                  h4('The first is to apply the same sequence of ',strong("automatic pre-processing"),' steps to all scans. This step is generally needed if you have not applied any pre-processing to your scans. Refer to the sample scans for examples of unprocessed scans.'),
                                                                  h4('The second is to apply ',strong("manual pre-processing")," to specific scans. This step is generally needed if you notice extreme, non-breech face markings in one or more scans that you would like to remove."),
                                                                  h4('Click "Looks good! I would like to compare these scans." if ',"you don't need to do any pre-processing.")))))),
                                   shinyjs::hidden(div(id = "importUI",
                                                       column(width = 3,
                                                              box(width = 12,
                                                                  conditionalPanel(condition = "input.appUsageChoice == 'example'",
                                                                                   selectInput(inputId = "exampleScanSet",
                                                                                               label = "Select a sample set of scans",
                                                                                               choices = c("Matching Pair",
                                                                                                           "Non-Matching Pair",
                                                                                                           "Matching Pair (Unprocessed)",
                                                                                                           "Unknown Source Triplet"))),
                                                                  conditionalPanel(condition = "input.appUsageChoice == 'custom'",
                                                                                   shinyFiles::shinyDirButton("x3pdir", 
                                                                                                              icon = fontawesome::fa_i("folder"),
                                                                                                              width = 300,
                                                                                                              style="color: #000; background-color: #ffe9a2; border-color: #f8d775; border-width: medium",
                                                                                                              "Select a folder containing x3p files", 
                                                                                                              "Upload")),
                                                                  # uiOutput(outputId = "import_nextStep_ui")
                                                                  br(),
                                                                  actionButton(inputId = "import_goToAutomaticPreprocess",
                                                                               label = "I would like to automatically pre-process all scans.",
                                                                               fontawesome::fa_i("fas fa-cut"),
                                                                               width = 350),
                                                                  br(),
                                                                  br(),
                                                                  actionButton(inputId = "import_goToManualPreprocess",
                                                                               label = "I would like to manually pre-process specific scans.",
                                                                               icon = fontawesome::fa_i("hand-scissors"),
                                                                               width = 350),
                                                                  br(),
                                                                  br(),
                                                                  actionButton(inputId = "import_goToComparison",
                                                                               label = "Looks good! I would like to compare these scans.",
                                                                               style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                                                                               icon = icon("pencil-ruler"),
                                                                               width = 350)
                                                              )),
                                                       column(width = 9,
                                                              plotOutput("pltInitX3P")))),
                                   # ,box(width = 12,
                                   # actionButton(inputId = "importTutorialButton",label = "Show Tutorial"),
                                   # uiOutput(outputId = "importTutorial"))
                                   
                          ), # end file Import tab
                          
                          tabPanel(title = "Automatic Pre-process",
                                   icon = fontawesome::fa_i("fas fa-cut"),
                                   box(
                                     h4('Use this tab to apply a sequence of pre-processing steps to all scans. This tab is useful if you have raw, unprocessed scans.
                                        Add as many sequential pre-processing steps as you would like by pressing "Add Another Pre-processing Step" and changing the settings.'),
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
                                   box(width = 12,
                                       h4("Use this tab to manually remove regions from specific scans.
                                      To begin, select a scan that you'd like to manually pre-process.")),
                                   column(width = 3,
                                          box(width = 12,#align = "center",
                                              selectInput(inputId = "manualDeletionSelection",
                                                          label = "Select a processed x3p to annotate",
                                                          choices = ""),
                                              bsTooltip("manualDeletionSelection",title = 'Click "Skip Pre-processing" or "Perform Pre-processing" on the previous tabs before using this tab.'),
                                              shinyjs::hidden(actionButton(inputId = "manualDeletion_selectToAnnotate",
                                                                           icon = fontawesome::fa_i("long-arrow-right"),
                                                                           width = 350,
                                                                           "Next: Start Annotating"))),
                                          box(width = 12,
                                              h4("Click the button below once you're done with manually pre-processing the scans."),
                                              actionButton(inputId = "manualDeletion_goToComparison",
                                                           icon = icon("pencil-ruler"),
                                                           width = 350,
                                                           style="color: #fff; background-color: #95bb72; border-color: #4b6043",
                                                           label = "Looks good! I would like to compare these scans."))),
                                   column(width = 9,plotOutput("manualDeletion_selectPlot",width = "100%",height = "100%"))
                          ), # end Manual Deletion - Select a Scan tab
                          
                          tabPanel(title = "Manual Deletion - Annotate Scan",
                                   icon = fontawesome::fa_i("hand-scissors"),
                                   box(width = 12,
                                       h4("To start, click + drag your cursor on the plot below to draw a rectangle around the region you want to remove.
                                          A zoomed-in view of the rectangular region will appear."),
                                       h4("Then, left-click on the zoomed-in preview to place a point.
                                       Placing three or more points will create a polygonal region using the points as vertices.
                                       All observations falling within this polygonal region will be removed in the next step, so you'll want to place points around the region that you want to remove."),
                                       h4('Click "Confirm Current Region" once you are happy with the polygonal region or "Reset Current Region" if you made a mistake.
                                          Start a new polygonal region by drawing a new rectangle on the full scan plot and repeating the point-placing process.'),
                                       h4('After creating at least one annotated region, click "Next: Preview Annotations" to preview what the scan looks like after deleting the annotated regions.')),
                                   column(width = 3,
                                          box(width = 12,#align = "center",
                                              selectInput(inputId = "manualDeletionSelection_gray",
                                                          choices = "",
                                                          label = "Select a processed x3p to annotate"),
                                              # h4("Click here once you're happy with the annotations."),
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
                                                           label = "Choose a Different Scan to Annotate"))),
                                   box(width = 9,
                                       # h4("Draw a rectangle on the scan plot to zoom-in on a particular region"),
                                       splitLayout(cellWidth = c("40%","40%","19%"),
                                                   plotOutput("manualDeletion_annotatePlot",
                                                              brush = "manualProcZoom"),
                                                   plotOutput("manualDeletion_annotatePlot_zoom",
                                                              click = "pointPlacement_zoom"),
                                                   shiny::tableOutput(outputId = "x3p1_selectedPolygons")),
                                       uiOutput(outputId = "manualDeletion_annotateOptions"),
                                       tags$script("Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                                                            Shiny.onInputChange(variableName, null);
                                                                            });"))
                          ), #end Manual Deletion - Select a Scan tab
                          
                          tabPanel(title = "Manual Deletion - Preview Final Annotations",
                                   box(width = 12,
                                       h4("The left plot below shows the original scan.
                                          The right plot below previews the scan after removing observations you annotated in the last step."),
                                       h4('Clicking "',"I'm",' Happy with these Annotations" will remove these observations from this scan as you move to the next stage of the application.'),
                                       h4("If you've changed your mind about these annotations, click ",'"Choose a Different Scan" to start over. This will reset all annotations you have made for this scan.')
                                   ),
                                   column(width = 3,
                                          box(width = 12,#align = "center",
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
                          )
                          
                          
              ) # end pre-processing tabsetPanel
              
              
      ), #end preprocessing tabItem
      
      # third tab content
      tabItem(tabName = "comparing",
              h2("Explore Tab"),
              h4("Use the ",strong("Explore")," tab to do a deep-dive into the cartridge case comparison algorithm. This tab is most useful for advanced users interested in experimenting with the algorithm."),
              h4("Move to the ",strong("Score")," tab if you instead want to compute a similarity score with little parameter fussing."),
              tabsetPanel(
                id = "comparingTabs",
                tabPanel("Cell Grid Comparison",
                         icon = fontawesome::fa_i("sliders-h"),
                         box(width = 12,
                             h4("Use this tab to perform a cell-based comparison procedure.
                             Click ",'"Preview Comparison(s)"'," to preview the selected reference and target scans to be compared.
                                We've set some default parameter settings, but you can change these in the ",'"Parameter Settings"', 'drop-down menu.'),
                             h4('Click "Perform Comparison" to execute the comparison. 
                                This may take a few minutes depending on the selected parameters and sizes of the scans.
                                Move to the "Results Summary" or "Individual Cell Results" tabs to explore results from the comparison procedure.')),
                         # br()
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
                                                   label = "Parameter Settings"),
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
                         # br(),
                         box(width = 12,
                             h4("Use this tab to explore the distribution of features extracted from the cell-based comparison procedure."),
                             h4("First, select a scan used as reference in the last tab.
                                The resulting plots show various visualizations of the cell-wise estimated registrations.
                                At the top, we visualize the reference's cell grid on the left and where each cell aligns in the target scan on the right.
                                Next, we show a dot plot of these registrations, which allows you to see which cell ",'"agree"', 'on the same registration.
                                Finally, we show the estimated translations faceted by rotation for each cell, which allows you to assess whether a "consensus" translation is reached by cells at a particular rotation.
                                These three visuals are interactively connected in that selecting a cell in one plot will highlight the same cell in the other two plots.')),
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
                         # br(),
                         box(width = 12,
                             h4("Use this tab to explore the registration of specific cells."),
                             h4("First, select a scan used as reference in the Cell Grid Comparison tab.
                                Click on a cell in the preview plot that appears to see where it aligns in the target scan.
                                The comparison plot below shows a zoomed-in view of the aligned cell pair partitioned into similarities and differences.")),
                         fluidRow(
                           column(width = 2,
                                  box(width = 12,
                                      selectInput("postComparisonScanSelect",
                                                  label = "Select a scan to view",
                                                  choices = NULL)
                                      # ,h4("Click on a cell to see where it aligns in the other scan")
                                  )),
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
                         box(width = 12,
                             h4("Use this tab to draw your own cell on a reference scan and see where it registers in the target scan."),
                             h4("First, select a reference and target scan to be compared.
                                The plot that appears shows the selected reference scan."),
                             h4("Next, select whether you want to draw a rectangular or hand-drawn cell.
                                A rectangular cell includes all surface values within a rectangle you draw on the reference scan.
                                Click and drag your cursor on the reference scan to draw a rectangular cell."),
                             h4("In contrast, a hand-drawn cell will include only the surface values within an arbitrary polygon that you draw on the reference scan, which allows you to focus on specific markings.
                                To draw a hand-drawn cell, first click and drag your cursor on the reference scan as you would a rectangular cell.
                                A zoomed-in visual of this rectangular region will appear on the right.
                                Left-click on this zoomed-in visual to place a point.
                                Placing three or more points will create a polygonal region using the points as vertices.
                                All observations falling outside of this polygonal region will be removed prior to comparison, meaning only values within the region will be registered."),
                             h4('After changing the parameter settings to your liking, click "Compare Custom Cell" to register the custom cell.
                                The output below shows where the custom cell aligns in the target scan and the comparison plot associated with this aligned cell pair.')),
                         column(2,
                                box(width = 12,
                                    selectInput(inputId = "customCellSelection",
                                                label = "Select Reference Scan",
                                                choices = NULL),
                                    bsTooltip("customCellSelection",title = "Select scan to draw your own cell"),
                                    selectInput(inputId = "targetSelect_customCell",
                                                label = "Select Target Scan",
                                                choices = NULL),
                                    selectInput(inputId = "customCellType",
                                                label = "Type of Cell",
                                                choices = c("Rectangular","Hand-drawn")),
                                    bsTooltip("targetSelect_customCell",title = "Select scan to which the custom cell is compared"),
                                    actionButton(inputId = "customCellMenuButton",
                                                 icon = fontawesome::fa_i("cog"),
                                                 label = "Parameter Settings"),
                                    div(id = "customCellMenu",
                                        numericInput(inputId = "cellRegionProp_customCell",
                                                     label = "Sidelength ratio between target region and reference cell",
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
                         # h4("Draw your own cell on the scan."),
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
              box(width = 12,
                  h4("Use this tab to compute similarity scores between pairs of cartridge cases.
                  To start, select a reference and target scan.
                     Then, choose either the ACES or CMC method to compute the similarity between these scans.")),
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
                                   # br(),
                                   column(width = 2,
                                          box(width = 12,uiOutput(outputId = "scoreSettings_ui")),
                                          # increase height of app:
                                          div(style = "width: 100%; height: 90vh")),
                                   tabPanel(title = "ACES Algorithm",
                                            id = "acesTab",
                                            icon = icon("fas fa-calculator"),
                                            box(width = 10,
                                                h4("The Automatic Cartridge Evidence Scoring (ACES) algorithm computes a set of numerical features that distinguish between same and different-source cartridge case pairs.
                                          Then, these features are used in a trained classifier model to predict a similarity score between 0 and 1 where larger values correspond with higher similarity.")),
                                            conditionalPanel(condition = "input.acesCalculate>0",
                                                             column(width = 10,
                                                                    htmlOutput(outputId = "aces_matchProbText"),
                                                                    box(id = "scoreDistPlotBox",
                                                                        title = actionLink(inputId = "scoreDistPlotTitle",
                                                                                           label = "How does this similarity score stack up to known-source comparisons?",
                                                                                           icon = icon("arrow-circle-down")),
                                                                        collapsible = TRUE,width = 12,collapsed = TRUE,
                                                                        # status = "info",solidHeader = TRUE,
                                                                        column(width = 10,
                                                                               plotOutput(outputId = "aces_similarityScoreDistribution",height = 400))),
                                                                    box(id = "featureDistPlotBox",
                                                                        title = actionLink(inputId = "featureDistPlotTitle",
                                                                                           label = "How do the feature values from this comparison stack up to known-source comparisons?",
                                                                                           icon = icon("arrow-circle-down")),
                                                                        collapsible = TRUE,width = 12,collapsed = TRUE,
                                                                        # status = "info",solidHeader = TRUE,
                                                                        column(width = 7,
                                                                               plotOutput(outputId = "aces_trainingFeatureDistribution",height = 700)),
                                                                        column(width = 3,
                                                                               tableOutput(outputId = "aces_featureTable")))
                                                             ))),
                                   tabPanel(title = "Congruent Matching Cells",
                                            id = "cmcTab",
                                            icon = fontawesome::fa_i("fas fa-th"),
                                            box(width = 10,
                                                h4('The Congruent Matching Cells (CMC) method uses a cell-based comparison procedure to compute an integer-valued similarity score for two cartridge cases.
                                                   Specifically, the CMC method aims to determine whether 1 or more cells "agree" on the same registration and then counts the number of agreeing cells.')),
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
              column(width = 12,
                     uiOutput(outputId = "export_checkboxUI"))),
      tabItem(tabName = "info",
              h1(strong("Welcome to cartridgeInvestigatR!")),
              h4("Use this app to compare 3D topographical scans of cartridge cases."),
              h4("Click the 'Help' button on each tab to learn about its functionality."),
              # h4("Scroll to the bottom of each page to see a Tutorial of how to use the page."),
              h4("See below for more information or visit ",HTML('<a href="https://github.com/jzemmels/cartridgeInvestigatR">https://github.com/jzemmels/cartridgeInvestigatR</a>'),"."),
              # br(),
              h2(strong("Background")),
              h4("This application uses computer algorithms to process and compare scans of cartridge cases.",
                 'A', tags$i("cartridge case"),' is the metal casing that houses the bullet and gunpowder prior to firing.',
                 'When a gun is fired, as the bullet moves down the barrel, the cartridge case moves backwards and slams against the back wall of the barrel (the ',tags$i("breech face"),') with considerable force.',
                 'Any markings on the breech face are "stamped" into the surface of the cartridge case.',
                 'This leaves so-called ',HTML("<i><a href='https://www.firearmsid.com/A_CCIDImpres.htm'>breech face impressions</a></i>"),' that forensic examiners use to identify the gun from which a cartridge case was fired.',
                 "Think of these impressions as analogous to a gun's",'"fingerprint" left on the cartridge case.',
                 "The computer algorithms used in this app compare the breech face impressions on two cartridge cases."),
              # br(),
              h2(strong("About this app")),
              h4("This app allows individuals to engage with cartridge case comparison algorithms without needing to program.",
                 "If you are interested in cartridge case identification, but do not have expertise in the R programming language, then this app is for you.",
                 "For more information about the computer algorithms used in this application, visit ",HTML("<a href='https://jzemmels.github.io/research.html'>https://jzemmels.github.io/research.html</a>"),"."),
              h4("To use this app, you must have cartridge case scans stored on your computer as ",HTML("<a href='https://tsapps.nist.gov/NRBTD/Home/DataFormat'>.x3p files</a>"),".",
                 "An X3P (XML 3D Surface Profile) is an ISO standard file format for saving cartridge case scans.",
                 "You can download example cartridge case .x3p files from the ",HTML("<a href='https://tsapps.nist.gov/NRBTD/'>NIST Ballistics Toolmark Research Database</a>.")),
              h4("The functionality of this app encompasses three stages of the cartridge case comparison procedure: Pre-processing, Exploring, and Scoring.",
                 "These stages are separated into the three tabs that you can see on the left sidebar.",
                 "You must complete the Import + Pre-processing stage before moving on to Explore or Score."),
              # br(),
              h3(strong("Basic workflow")),
              HTML("<div class='row' align='center'> <img src='https://raw.githubusercontent.com/jzemmels/cartridgeInvestigatR/main/images/workflowDiagram.png' alt='Workflow Diagram' width='500'> </div>"),
              # tags$p(tags$img(src = "workflowDiagram.png",width = "30%",class = "text-align:center")),
              h4("To start, make sure that you have cartridge case scans stored as x3p files on your computer."),
              h4("If you would simply like to explore this app, you can find example scans in the home directory of this app."),
              # br(),
              h3(strong("Import + Pre-process")),
              h4("Use the 'Import + Pre-process' tab to upload scans to the app.",
                 "Click the 'Select a folder containing x3p files' to upload scans.",
                 'You may need to pre-process scans to highlight the breech face impressions prior to comparison.',
                 "Note that the example scans in the home directory have already been pre-processed to some extent, but you're welcome to experiment with additional pre-processing steps."),
              # br(),
              h4(strong("If your scans require pre-processing:")),
              h4("The 'Automatic Pre-process' tab allows you to string-together algorithms to automatically pre-process your scans.",
                 "Click the 'Add Another Pre-processing Step' to add a pre-processing algorithm to the sequence.",
                 "You can also select parameters for the selected pre-processing algorithm.",
                 "Click 'Perform Automatic Pre-processing' once you're happy with the pre-processing sequence or 'Reset All Pre-processing Steps' to start over."),
              h4("A pre-processing sequence that we have found to work well for many unprocessed scans is as follows:"),
              h4(HTML("<ol>"),
                 HTML("<li>"),"Crop with parameters Region set to 'Interior' and Offset set to a positive value (try about 10% of the dimensions of the scan).",HTML("</li>"),
                 HTML("<li>"),"Crop with parameters Region set to 'Exterior' and Offset set to a negative value (try about 30% of the dimensions of the scan).",HTML("</li>"),
                 HTML("<li>"),"Level with parameter Statistic set to 'Median' (the default).",HTML("</li>"),
                 HTML("<li>"),"Filter with paramaters Filtertype set to 'Bandpass' and Wavelength(s) set to '16, 500' (the defaults).",HTML("</li>"),
                 HTML("<li>"),"Downsample with parameter Stride set to '2' (the default).",HTML("</li>"),
                 HTML("</ol>")),
              h4("The Erode step with the Radius parameter set to a positive value will 'shave off' the interior/exterior of scans.",
                 "The Delete step is useful if your uploaded scans include masks identifying regions that you would like to remove.",
                 "We encourage you to experiment with different pre-processing steps and orders."),
              # br(),
              h4("Move to the 'Manual Deletion' tab to remove specific regions from scans."),
              h4(strong("After your scans are pre-processed")),
              h4("Press the 'I would like to compare these scans' button at any time to complete the Import + Pre-process phase.
         Once completed, you cannot return to the Import + Pre-process phase and will need to restart the app if you decide additional pre-processing is needed.
         You can next move on to the Explore or Score phases."),
              h3(strong("Explore")),
              h4("In the 'Cell Grid Comparison' tab, select a reference and target scan to compare.",
                 "If you're simply exploring the app, we have chosen default parameter settings that work well.",
                 "However, we encourage experimentation if you're interested.",
                 "Click on the 'Perform Comparison' button at the bottom of the sidebar once you're happy with the parameters.",
                 "Once the comparison finishes (the loading bar vanishes), you can assess the comparison results using the 'Results Summary' or 'Individual Cell Results' tabs."),
              h4("In the 'Results Summary' tab, you can select a scan from the dropdown.",
                 "The plots that appear depict the similarity features extracted during the cell-based comparison procedure.",),
              h4("In the 'Individual Cell Results' tab, you can select a scan from the dropdown.",
                 "Click on a cell in the plot that appears to visualize its alignment in the compared scan.",
                 "A comparison plot appears at the bottom of the page that shows similarities and differences between the selected cell and the region to which it aligns in the other scan."),
              h4("Finally, the 'Custom Cell' tab allows you to draw your own cell (either rectangular or hand-drawn) on a scan and compare this it to another scan.",
                 "This tab can be used to analyze how a specific region of interest aligns in another scan.",
                 "All of these plots are interactable: clicking on one element will highlight elements in the other plots corresponding to the same cell."),
              h3(strong("Score")),
              h4("Select a reference and target scan to compare.",
                 "Use the 'ACES Algorithm' tab to apply the Automatic Cartridge Evidence Scoring algorithm that computes the probability that the two scans match.",
                 "Use the 'Congruent Matching Cells' tab to apply Congruent Matching Cells algorithm."),
              h3(strong("Export")),
              h4("Use this tab to export scans or results computed in the app.",
                 "You can export results as Comma-Separated Variable (.csv) or .RData files.",
                 "You can also download an R script to reproduce the comparison results."),
              h2(strong("References")),
              h4("Zheng, X., Soons, J., Thompson, R., Singh, S., & Constantin, C. (2020). NIST Ballistics Toolmark Research Database. In Journal of Research of the National Institute of Standards and Technology (Vol. 125). National Institute of Standards and Technology (NIST). https://doi.org/10.6028/jres.125.004 "),
              h4("J. Song. Proposed “NIST Ballistics Identification System (NBIS)” Based on 3D Topography Measurements on Correlation Cells. American Firearm and Tool Mark Examiners Journal, 45(2):11, 2013. URL https://tsapps.nist.gov/publication/get_pdf.cfm?pub_id=910868."))
      
    )
  )
)
