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
                     menuItem("Information", tabName = "info", icon = icon("info")),
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
                 'When a gun is fired, as the bullet moves down the barrel, the cartridge case moves backwards and slams against the back wall of the barrel (a.k.a. the ',tags$i("breech face"),') with considerable force.',
                 'Any markings on the breech face are "stamped" into the surface of the cartridge case.',
                 'This leaves so-called ',HTML("<i><a href='https://www.firearmsid.com/A_CCIDImpres.htm'>breech face impressions</a></i>"),' that forensic examiners use to identify the gun from which a cartridge case was fired.',
                 "Think of these impressions as analogous to a gun's",'"fingerprint" left on the cartridge case.',
                 "The computer algorithms used in this app compare the breech face impressions on two cartridge cases."),
              # br(),
              h2(strong("About this app")),
              h4("This app allows individuals to engage with cartridge case comparison algorithms without needing to program.",
                 "If you are interested in cartridge case identification, but do not have expertise in the R programming language, then this app is for you.",
                 "For more information about the computer algorithms used in this application, visit ",HTML("<a href='https://github.com/CSAFE-ISU/cmcR'>https://github.com/CSAFE-ISU/cmcR</a>"),"."),
              h4("To use this app, you must have cartridge case scans stored on your computer as ",HTML("<a href='https://tsapps.nist.gov/NRBTD/Home/DataFormat'>.x3p files</a>"),".",
                 "X3P (XML 3D Surface Profile) is an ISO standard file format for saving cartridge case scans.",
                 "You can download example cartridge case .x3p files from the ",HTML("<a href='https://tsapps.nist.gov/NRBTD/'>NIST Ballistics Toolmark Research Database</a>.")),
              h4("The functionality of this app encompasses three stages of the cartridge case comparison procedure: Pre-processing, Comparing, and Scoring.",
                 "These stages are separated into the three tabs that you can see on the left sidebar.",
                 "You must complete one stage before moving onto the next."),
              # br(),
              h2(strong("Basic workflow")),
              HTML("<div class='row' align='center'> <img src='workflowDiagram.png' alt='Workflow Diagram'> </div>"),
              # tags$p(tags$img(src = "workflowDiagram.png",width = "30%",class = "text-align:center")),
              h4("To start, make sure that you have cartridge case scans stored as x3p files on your computer."),
              h4("If you would simply like to explore this app, you can find three example scans in the 'data/DFSC_Baldwin_TopMatch/' folder included with this app."),
              # br(),
              h3("Import + Pre-process"),
              h4("Use the 'Import' tab to upload scans to the app.",
                 "Click the 'Select a folder containing x3p files' to upload scans.",
                 'You may need to preprocess scans to highlight the breech face impressions prior to comparison.',
                 "Note that the three example scans in the data/DFSC_Baldwin_TopMatch folder have already been preprocessed to some extent, but you're welcome to experiment with additional preprocessing steps."),
              # br(),
              h4(strong("If your scans require preprocessing:")),
              h4("The 'Automatic Pre-process' tab allows you to string-together algorithms to automatically preprocess your scans.",
                 "Click the 'Add another preprocess step' to add a preprocessing algorithm to the sequence.",
                 "You can also select parameters for the selected preprocessing algorithm.",
                 "Click 'Perform Automatic Pre-processing' once you're happy with the preprocessing sequence or 'Reset' to start over."),
              h4("A preprocessing sequence that we have found to work well for many unprocessed scans is as follows:"),
              h4(HTML("<ol>"),
                 HTML("<li>"),"Crop with parameters Region set to 'Interior' and Offset set to a positive value (try about 10% of the dimensions of the scan).",HTML("</li>"),
                 HTML("<li>"),"Crop with parameters Region set to 'Exterior' and Offset set to a negative value (try about 30% of the dimensions of the scan).",HTML("</li>"),
                 HTML("<li>"),"Level with parameter Statistic set to 'Median' (the default).",HTML("</li>"),
                 HTML("<li>"),"Filter with paramaters Filtertype set to 'Bandpass' and Wavelength(s) set to '16, 500' (the defaults).",HTML("</li>"),
                 HTML("<li>"),"Downsample with parameter Stride set to '2' (the default).",HTML("</li>"),
                 HTML("</ol>")),
              h4("We encourage you to experiment with different preprocessing steps and orders.",
                 "The Erode step with the Radius parameter set to a positive value will 'shave off' the interior/exterior of scans.",
                 "The Delete step is useful if your uploaded scans include masks identifying regions that you would like to remove."),
              # br(),
              h4(strong("After your scans are preprocessed")),
              h4("If your scans were already preprocessed prior to uploading, then you must click the 'Skip Automatic Pre-processing' button on the 'Import' tab before moving on.",
                 "You may identify regions of a cartridge case scan that you wish to remove (e.g., large dents in the surface of the scan).",
                 "Use the 'Manual Deletion' to remove such regions from a scan.",
                 "Once you're happy with your preprocessed scans, you will move onto the Comparing stage."),
              h3("Explore"),
              h4("In the 'Cell Grid Comparison' tab, select a reference and target scan to compare.",
                 "If you're simply exploring the app, we have chosen default parameter settings that work well.",
                 "However, we encourage experimentation if you're interested.",
                 "Click on the 'Perform Comparison' button at the bottom of the sidebar once you're happy with the parameters.",
                 "Once the comparison finishes (the loading bar vanishes), you can move onto the 'Individual Cell Results' tab or the Scoring stage."),
              h4("In the 'Individual Cell Results' tab, you can select a scan from the dropdown.",
                 "Click on a cell in the plot that appears to visualize its alignment in the compared scan.",
                 "A comparison plot appears at the bottom of the page that shows similarities and differences between the selected cell and the region to which it aligns in the other scan.",
                 # "The top- and bottom-left plots show the selected cell and where it aligns in the compared scan, respectively.",
                 # "The middle shows the similarities: element-wise average between these two regions with regions grayed-out of the element-wise absolute difference exceeds one standard deviation",
                 # "Finally, the top- and bottom-right plots show the differences: the selected cell and where it aligns in the compared scan, respectively, with values for which the element-wise absolute difference exceeds one standard deviation."
              ),
              h4("Finally, the 'Custom Cell' tab allows you to draw your own cell (either rectangular or hand-drawn) on a scan and compare this it to another scan.",
                 "This tab can be used to analyze how a specific region of interest aligns in another scan."),
              h3("3. Scoring"),
              h4("The 'Congruent Matching Cells' tab (currently under construction) provides a similarity score for the two selected cartridge cases.",
                 "Specifically, this tab uses the Congruent Matching Cells (CMC) method introduced by Song (2013) to calculate the number of similar cells between two scans.",
                 "After selecting desired parameters (the defaults tend to work well), click the 'Visualize CMCs' button to see a plot of the CMCs and non-CMCs overlaid on the reference and target scans.",
                 "The total number of CMCs is used as a similarity score - the higher the CMC count, the more similar the cartridge cases."),
              h2(strong("References")),
              h4("Zheng, X., Soons, J., Thompson, R., Singh, S., & Constantin, C. (2020). NIST Ballistics Toolmark Research Database. In Journal of Research of the National Institute of Standards and Technology (Vol. 125). National Institute of Standards and Technology (NIST). https://doi.org/10.6028/jres.125.004 "),
              h4("J. Song. Proposed “NIST Ballistics Identification System (NBIS)” Based on 3D Topography Measurements on Correlation Cells. American Firearm and Tool Mark Examiners Journal, 45(2):11, 2013. URL https://tsapps.nist.gov/publication/get_pdf.cfm?pub_id=910868.")
              
      ),
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
                           column(2,
                                  box(width = 12,
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
                         column(width = 2,
                                selectInput(inputId = "comparisonSummary_referenceSelect",label = "Choose a reference scan",choices = NULL),
                                checkboxGroupInput(inputId = "comparisonSummary_featureSelect",
                                                   label = "Select visual summaries",
                                                   c("Cell-wise translations by rotations scatterplots",
                                                     "Cell-wise registrations dot plots",
                                                     "Registration-based features",
                                                     "Density-based features",
                                                     "Visual diagnostic features"),
                                                   selected = c("Cell-wise translations by rotations scatterplots",
                                                                "Cell-wise registrations dot plots"))
                         ),
                         column(width = 10,
                                girafeOutput(outputId = "comparisonSummary_histograms",width = "1300px",height = "1300px")
                         ),
                         # increase height of app:
                         div(style = "width: 100%; height: 90vh")
                         
                ), # end Results Summary tab
                
                tabPanel("Individual Cell Results",
                         icon = fontawesome::fa_i("search-plus"),
                         br(),
                         # br(),
                         column(width = 2,fluidRow(selectInput("postComparisonScanSelect","Select a scan to view",NULL),
                                                   bsTooltip("postComparisonScanSelect",title = "Select a scan that was divided into a grid of cells"),
                                                   h4("Click on a cell to see where it aligns in the other scan"))),
                         column(width = 10,
                                column(5,plotOutput(outputId = "postComparisonPlot",click = "postComparisonClick")),
                                column(5,plotOutput(outputId = "targetScanCellPlot")),
                                plotOutput(width = "70%",outputId = "cellComparisonPlot")),
                         # increase height of app:
                         div(style = "width: 100%; height: 90vh")
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
                         fluidRow(column(4,
                                         plotOutput(outputId = "customCellFullScanPlot",brush = "customCellBrush"),
                                         plotOutput(outputId = "targetScanCustomCellPlot")),
                                  column(4,
                                         plotOutput(outputId = "customHandDrawnCell",click = "customHandDrawnCellClick"),
                                         plotOutput(outputId = "customCellComparisonPlot"))),
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
                                                   column(width = 5,
                                                          plotOutput(outputId = "aces_matchProbPlot")),
                                                   column(width = 5,
                                                          htmlOutput(outputId = "aces_matchProbText")
                                                   ))),
                                   tabPanel(title = "Congruent Matching Cells",
                                            id = "cmcTab",
                                            icon = fontawesome::fa_i("fas fa-th"),
                                            column(width = 10,
                                                   plotOutput(outputId = "cmcMethodPlot",height = 1000),
                                                   column(width = 5,
                                                          uiOutput("cmcMethodInformation_refToTarget")),
                                                   column(width = 5,
                                                          uiOutput("cmcMethodInformation_targetToRef"))
                                            )),
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
