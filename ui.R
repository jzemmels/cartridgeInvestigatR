ui <- dashboardPage(

  dashboardHeader(title = "bulletinvestigatR",
                  titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<a href='https://forensicstats.org/'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='csafe-logo-90.png' width = '186'></a>",
        "<br>",
        "<a href='https://github.com/heike/bulletxtrctr/'><p style='text-align:center'>bulletxtrctr</p></a>"
      )),
      menuItem("Introduction", tabName = "info", icon = icon("info")),
      menuItem("Data", icon = icon("table"), tabName = "data_related"),
      menuItem("Analysis", icon = icon("pencil-ruler"), startExpanded = TRUE,
               menuSubItem("Bullet Land Inspection", tabName = "investigation"),
               menuSubItem("Signature Comparison", tabName = "sig_compare"))
      
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "info",
              h2("Welcome to bulletInvestigatoR!"),
              h4("bulletxtrctr in an interactive way"),
              p("obtain 'good' bullet signatures"),
              p("- process x3p files"),
              p("- compute crosscut, ccdata, and grooves"),
              p("- check groove locations and update crosscut values if needed"),
              p("- save/load your work; output as a csv file (todo)"),
              h4("reproducible"),
              p("- automatically generate R codes"),
              p("- use these R codes one could obtain the exactly same object"),
              p("- output/download the R codes (todo)"),
              h4("extensible"),
              p("- plug in different modules to extend the functionality of this APP"),
              p("- modules are essentially functions(x3ptools, bulletxtrctr) calls"),
              

              
      ),
      
      # Second tab content
      tabItem(tabName = "data_related",
              h2("play with x3p files"),
              
              tabsetPanel(
                      tabPanel("Import",
                               
                               br(),
                               br(),
                               
                               box(
                                 shinyDirButton("x3pdir", "Select a folder containing x3p files", "Upload"),
                                 p("some text..."), 
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
                               
                               ),
                      tabPanel("x3p",
                               
                               br(),
                               br(),
                               
                               fluidRow(
                                 box(fluidRow(column(3,
                                            conditionalPanel(
                                              condition = "output.hasname_x3p",
                                              actionButton("displayx3p2", "Display x3p image")
                                              ))
                                            ),
                                     width = 12
                                     )
                               ),
                               
                               fluidRow(
                                 box(x3pActionButtonUI("x3p_show_xml", "Show x3p size"),
                                     width = 12)
                               ),
                               
                               fluidRow(
                                 box(x3pActionButtonUI("x3p_flip", "Flip y!"),
                                     width = 12)
                               ),
                               
                               fluidRow(
                                 box(x3p_sampleUI("x3p_sample", "Compute x3p_sample"),
                                     width = 12)
                               ),
                               
                               fluidRow(
                                 box(x3pActionButtonUI("x3p_m_to_mum", "Change m to mum"),
                                     width = 12)
                               ),
                               
                               fluidRow(
                                 box(x3p_rotateUI("x3p_rotate", "Rotate the x3p file"),
                                     actionButton("nnthing1", "Click for nothing 2"),
                                     width = 12)
                               )
                               
                               ),
                      
                      tabPanel("Prepare and Check",
                               
                               br(),
                               br(),
                               
                               box(
                                 x3pActionButtonUI("prepare_shinytt", "Prepare the data for investigation!"),
                                 p("This might take some time. We are computing crosscut, ccdata, and 
                                 grooves from your x3p files ..."),
                                 
                                 width = NULL
                               ),
                               
                               box(
                                 actionButton("ttcheck", "Check if the app is ready for investigation", 
                                              icon = icon("search")),
                                 verbatimTextOutput("checkresult"),
                                 verbatimTextOutput("suggest"),
                                 
                                 width = NULL
                                 
                               ),
                               
                               )
                      

              ),
              
              fluidRow(
                column(4,
                       box(
                         actionButton("updateCode", "update code", width = "100%",
                                      style="white-space: normal;text-align:center;"),
                         br(),
                         actionButton("clean_code_window", "clean codes and the loaded object", 
                                      width = "100%", style="white-space: normal;text-align:center;"),
                         br(),
                         downloadButton("download_codes", label = "Download R codes"),
                         
                         width = NULL
                         )
                       
                ),
                
                column(8,
                       aceEditor("myEditor", "", mode = "r", readOnly = TRUE, theme = "chrome")
                )
              ),
              
              
              rglwidgetOutput("x3prgl2"),
              
              
      ),
      
      # First tab content
      tabItem(tabName = "investigation",
              # h3("Select a Bullet Land"),
              
              tabsetPanel(
                
                tabPanel("Main",
                         
                         fluidRow(
                           column(6,
                                  uiOutput("selectk")
                                  ),
                           
                           column(6,
                                  uiOutput("selectid")
                                  ),
                           
                           width = NULL
                           
                         ),
                         
                         fluidRow(
                           column(9,
                                  textOutput("groovelocations"),
                                  plotOutput("groovePlot", click = "plot_click", height="300px"),
                                  
                                  column(9,
                                         uiOutput("x3p_type_select_ui"),
                                         uiOutput("x3p_comment_box_ui")
                                         ),
                                  
                                  column(3,
                                         br(),
                                         actionButton("confirm", "Confirm", 
                                                      style="white-space: normal;text-align:center;")
                                         )
                                  
                                  
                                  ),
                           
                           column(3, offset = 0,
                                  box(
                                    conditionalPanel(
                                      condition = "output.hasname_x3p",
                                      actionButton("displayx3p", "Display x3p image", width = "100%",
                                                   style="white-space: normal;text-align:center;") %>%
                                        helper(type = "inline",
                                               title = "Inline Help",
                                               content = c("This helpfile is defined entirely in the UI!",
                                                           "This is on a new line.",
                                                           "This is some <b>HTML</b>."),
                                               size = "s"),
                                      br(),
                                      
                                      textOutput("ccvalue"),
                                      numericInput("cc", "change crosscut to:", 100, min = 0, max = 1000),
                                      actionButton("updateCC", "Update Crosscut Value", width = "100%",
                                                   style="white-space: normal;text-align:center;") %>% 
                                        helper(type = "markdown",
                                               content = "test"),

                                      
                                      actionButton("drawsig", "Draw Signature", width = "100%",
                                                   style="white-space: normal;text-align:center;")
                                      
                                    ),
                                    width = NULL)
                                  )
                         ),
                         
                         fluidRow(
                           actionButton("saveCurrentEnv", "Save your progress!", width = "100%",
                                        style="white-space: normal;text-align:center;"),
                           br(),
                           textOutput("saveptp")

                         ),
                         
                         fluidRow(
                           column(9,
                                  conditionalPanel(
                                    condition = "output.hasname_x3p",
                                    plotOutput("sigPlot", height = "300px")
                                  )
                                  
                                  )
                           
                         ),
                         
                         fluidRow(
                           rglwidgetOutput("x3prgl")
                         )
                         
                         
                         
                         ),
                
                tabPanel("Output",
                         
                         box(
                           textInput("save_csv_id", "Your Manual Code", value = "", width = NULL,
                                     placeholder = TRUE),
                           textInput("save_csv_study_name", "Name of the Study", value = "", width = NULL,
                                     placeholder = TRUE),
                           actionButton("cc_status", "Check Crosscuts", width = "100%",
                                        style="white-space: normal;text-align:center;"),
                           downloadButton("save_csv", "Output a CSV file",
                                          style="white-space: normal;text-align:center;"),
                           width = NULL, title = "Output as a csv file", solidHeader = TRUE),
                         
                         box(downloadButton("downloadData", "Save the Processed Data as a RDS file.",
                                            style="white-space: normal;text-align:center;"),
                             p("It might take a few seconds to prepare for the saving."),
                             width = NULL, title = "Output as a rds file", solidHeader = TRUE)
                         
                         )
                
              ),
              
      ),
      

      
      tabItem(tabName = "sig_compare",
              h2("Compute two signatures"),
              
              box(
                uiOutput("sig1_select"),
                actionButton("sig1_display", "Display x3p"),
                actionButton("sig1_draw", "Draw Signature"),
                plotOutput("sig1_plot", height = "300px"),
                width = NULL
              ),
              
              box(
                uiOutput("sig2_select"),
                actionButton("sig2_display", "Display x3p"),
                actionButton("sig2_draw", "Draw Signature"),
                plotOutput("sig2_plot", height = "300px"),
                width = NULL
              ),
              
              box(
                numericInput("sig1_align_num", "Shift signature 1 (red)", 0, min = -5000, max = 5000),
                numericInput("sig2_align_num", "Shift signature 2 (blue)", 0, min = -5000, max = 5000),
                
                actionButton("sig_align", "Align two signatures"),
                plotOutput("sig_align_plot", height = "300px"),
                
                width = NULL
                
              ),
              
              rglwidgetOutput("sig1_display_rgl"),
              rglwidgetOutput("sig2_display_rgl")
      )
      
    )
  )
)



























