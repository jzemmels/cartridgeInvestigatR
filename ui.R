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
                           # selectInput("k","Investigate kth land:", selected = 1, choices=1:n))
                           uiOutput("selectk"))
                ),
                
                column(width = 4,
                       box(width = NULL, background = NULL, height = 100,
                           # conditionalPanel(
                           #   condition = "output.hasname_scanid",
                           #   selectInput("scanID","Investigate according to the Scan ID :",
                           #               selected = all_scan_id[1], choices=all_scan_id)
                           # )
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
              fluidRow(
                column(width = 3, offset = 1, 
                       shinyDirButton("x3pdir", "Input directory", "Upload")
                )
                
              ),
              sidebarPanel(
                actionButton("cp_size", "Compute x3p Object Size"),
                checkboxInput("ck_mtomum", "m_to_mum", FALSE),
                checkboxInput("ck_rotation", "rotation", FALSE),
                uiOutput("r_angle_out"),
                checkboxInput("ck_flip", "y_flip", FALSE),
                checkboxInput("ck_downsample", "down sample", FALSE),
                uiOutput("ds_m_out"),
                
                conditionalPanel(
                  condition = "output.hasname_x3p",
                  actionButton("displayx3p2", "Display x3p image")
                ),
                
                actionButton("process_x3p", "Process x3p"),
                actionButton("prep_shinytt", "Prepare for shiny.tt")
              ),
              
              mainPanel(
                verbatimTextOutput("x3pdir_prompt"),
                verbatimTextOutput("x3pinfo"),
                verbatimTextOutput("prep_shiny_ppt"),
                verbatimTextOutput("prep_shiny_ppt1"),
                rglwidgetOutput("x3prgl2")
              )
              

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