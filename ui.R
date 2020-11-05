ui <- dashboardPage(
  dashboardHeader(title = "bulletxtrctr Shiny Investigator",
                  titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<a href='https://forensicstats.org/'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='csafe-logo-90.png' width = '186'></a>",
        "<br>",
        "<a href='https://github.com/heike/bulletxtrctr/'><p style='text-align:center'>bulletxtrctr</p></a>"
      )),
      menuItem("Information", tabName = "info", icon = icon("info")),
      menuItem("Investigation", tabName = "investigation", icon = icon("pencil-ruler")),
      menuItem("Data Preparation", icon = icon("table"), startExpanded = FALSE,
               menuSubItem("x3p", tabName = "x3pp"),
               menuSubItem("Crosscut", tabName = "crosscut"),
               menuSubItem("Grooves", tabName = "grooves"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "info",
              h2("Welcome!"),
              p("this is a paragraph."),
              p("the second paragraph."),
              actionButton("ttcheck", "Check Shiny.tt", icon = icon("search")),
              verbatimTextOutput("checkresult"),
              verbatimTextOutput("suggest"),
              shinyFilesButton("file1", "Select a rds file", "Please select a file", 
                               multiple = FALSE, viewtype = "detail"),
              verbatimTextOutput("file1_prompt")
      ),
      
      # First tab content
      tabItem(tabName = "investigation",
              h3("Select a Bullet Land"),
              
              fluidRow(
                column(width = 4,
                       box(width = NULL, background = NULL, height = 100,
                           uiOutput("selectk"))
                ),
                
                column(width = 4,
                       box(width = NULL, background = NULL, height = 100,

                           uiOutput("selectid")
                       )
                ),
                column(width = 4,
                       downloadButton("downloadData", "Save the Processed Data as a RDS file.",
                                      style="white-space: normal;text-align:center;"),
                       p("It might take a few seconds to prepare for the saving.")
                )
              ),
              
              h3("Display Crosscut Data to Identify Grooves"),
              
              fluidRow(
                column(width = 3,
                       conditionalPanel(
                         condition = "output.hasname_x3p",
                         actionButton("displayx3p", "Display x3p image")
                       )),
                column(width = 9, 
                       actionButton("confirm", "Confirm"),
                       actionButton("mark", "Mark"),
                       actionButton("unmark", "Unmark"))
                
              ),
              
              fluidRow(
                column(width = 3,
                       conditionalPanel(
                         condition = "output.hasname_x3p",
                         box(width = NULL,
                             textOutput("ccvalue"),
                             numericInput("cc", "change crosscut to:", 100, min = 0, max = 1000),
                             sliderInput("cc_slide", "change crosscut with a slider:",
                                         min = 0, max = 1000,
                                         value = 100, step = 0.1),
                             actionButton("updateCC", "Update Crosscut Value", width = "100%",
                                          style="white-space: normal;text-align:center;")
                         ),
                         actionButton("saveCurrentEnv", "Save your progress!"),
                         textOutput("saveptp")
                       )),
                
                column(width = 9,
                       textOutput("groovelocations"),
                       plotOutput("groovePlot", click = "plot_click", height="300px"),
                       br(),
                       conditionalPanel(
                         condition = "output.hasname_x3p",
                         actionButton("drawsig", "Draw Signature", 
                                      style="white-space: normal;text-align:center;"),
                         br(),
                         plotOutput("sigPlot", height = "300px")
                       ))
              ),
              
              fluidRow(
                rglwidgetOutput("x3prgl")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "x3pp",
              h2("play with x3p files"),

              sidebarPanel(
                shinyDirButton("x3pdir", "Input directory", "Upload"),
                
                conditionalPanel(
                  condition = "output.hasname_x3p",
                  actionButton("displayx3p2", "Display x3p image")
                ),
                
                actionButton("updateCode", "update code"),
                actionButton("clean_code_window", "clean all the code"),
                
                x3pActionButtonUI("prepare_shinytt", "Prepare the data for investigation!"),
                
                downloadButton("mydownload")
              ),
              
              mainPanel(
                column(width = 6,
                       box(
                         x3pActionButtonUI("x3p_show_xml", "Show x3p size")
                       ),
                       box(
                         x3pActionButtonUI("x3p_flip", "Flip y!")
                       ),
                       
                       box(
                         x3p_sampleUI("x3p_sample", "Compute x3p_sample")
                       )
                ),
                
                column(width = 6,
                       box(x3pActionButtonUI("x3p_m_to_mum", "Change m to mum")), 
                       box(x3p_rotateUI("x3p_rotate", "Rotate the x3p file"))),
                
              ),

              aceEditor("myEditor", "", mode = "r", readOnly = TRUE, theme = "chrome"),
              
              rglwidgetOutput("x3prgl2")
              

      ),
      
      tabItem(tabName = "crosscut",
              h2("Compute crosscuts and ccdata")
      ),
      
      tabItem(tabName = "grooves",
              h2("Compute grooves")
      )
    )
  )
)