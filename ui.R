cat("\n > Enter in ui. R \n")

rm(list = ls())


#######################################################################
## UI definition. ----
#######################################################################


# Header Function ==================================
# ================ ----

header <- dashboardHeader(
  title = tags$a(
    href = "",
    tags$img(
      src = "images/", em("f"),"Datalyzer",
      style = "vertical-align:middle",
      height = 50, width = 75, align = "left"
    ),
    style = "font-size: 20px; font-family: Roboto;"
  ),
  titleWidth = 200,
  dropdownMenuCustom(
    type = "message",
    customSentence = customSentence,
    messageItem(
      from = "s4310036@gmail.com", #' Feedback and suggestions',
      message =  "", 
      icon = icon("envelope"),
      href = "mailto:s4310036@gmail.com"
    ),
    icon = icon("comment")
  ),
  tags$li(
    class = "dropdown",
    tags$style(".main-header {max-height: 20px}"),
    tags$style(".main-header .logo {height: 50px;}"),
  )
)

sidebar <- dashboardSidebar(
  collapsed = T, width = 200,
  sidebarMenuOutput(outputId = "sidebarmenu")
)


# Body Function ================================================== ----
body <- dashboardBody(
  useShinyjs(),
  use_font("roboto", "www/css/roboto.css"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/tel.css"),
  ),
  br(),
  br(),


  # **********************************************************************
  # * Data Explorer Menu                                              ----
  # **********************************************************************
  fluidRow(
    column(
      width = 12,
      # box(
      # width = NULL, height = "1024px", status = "danger", align = "center",

      login_ui(id = "module_login"),
      # uiOutput(outputId = "mainmenu"),
      # uiOutput(outputId = "sidebarmenu"),

      # )
    )
  ),
  
  tabItems(
    tabItem(tabName = "analysisTab", Analytics_UI(id = "analytics_1")),
    tabItem(tabName = "fleetinsightTab", fleet_insight_UI(id = "fleet_insight_UI_1")),
    # tabItem(tabName = "robotArmTab", robot_arm_tracking_UI(id = "robotArmTracking")),
    #tabItem(tabName = "quicktraceTab", h1("quicktraceTab")),
    #tabItem(tabName = "userUpTab", h1("userUpTab")),
    tabItem(tabName = "aboutTab", About_UI(id = "About_UI_1"))
  )
  # tabItem(
  #   tabName = "analysisTab",
  #   br(),
  #   br(),
  #   br(),
  #   br(),
  #   br(),
  #   tags$hr(style = "border: 2px solid #78BE20; border-radius: 5px;"),
  #   h2(class = "h2", "Turning data into insights", icon("far fa-brain"), "and insights into predictions!"),
  #   br(),
  #   fluidRow(
  #     # box(
  #     #   title = "Module 1 : Feature selection", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
  #     #   load_dataUI(id = "mod1")
  #     # ),
  #     column(
  #       width = 4,
  #       # box(
  #       #   title = "Module 0 : Test data", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
  #       #   load_testdataUI(id = "mod0")
  #       # ),
  #       box(
  #         title = "Module 0.5 : Load CSV data", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
  #         load_CSVdataUI(id = "mod0.5")
  #       ),
  #     ),
  #     column(
  #       width = 4,
  #       # box(
  #       #   title = "Module 0.5 : Load CSV data", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
  #       #   load_CSVdataUI(id = "mod0.5")
  #       # ),
  #       # box(
  #       #   title = "Module Y : Data inspection table", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
  #       #   data_inspectUI(id = "modX")
  #       # ),
  #       box(
  #         title = "Module X : Feature Selection", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
  #         load_dataUI(id = "mod1")
  #       ),
  #     ),
  #     column(
  #       width = 4,
  #       # box(
  #       #   title = "Module X : Data inspection table", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
  #       #   data_inspectUI(id = "modX")
  #       # ),
  #       box(
  #         title = "Module : ", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
  #         # load_dataUI(id = "mod1")
  #       ),
  #     )
  #   ),
  #   # tags$hr(style = "border: 2px solid #78BE20; border-radius: 5px;"),
  #   # h2("TEST AREA"),
  #   # box(
  #   #   title = "Module X : Data inspection table", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
  #   #   data_inspectUI(id = "modX")
  #   # ),
  #
  #   tags$hr(style = "border: 2px solid #78BE20; border-radius: 5px;"),
  #   br(),
  #   fluidRow(
  
  #     column(
  #       width = 6,
  #       box(
  #         title = "Module : Reduced dimension plot", width = NULL,
  #         PCA_dataUI(id = "mod2")
  #       ),
  #     ),
  #     column(
  #       width = 6,
  #       box(
  #         title = "Module : Scree plot ", width = NULL,
  #         Scree_dataUI(id = "mod5")
  #       ),
  #       box(
  #         title = "Details of the brushed points", width = NULL,
  #         Brushedpoint_dataUI(id = "mod6")
  #       ),
  #       # box(
  #       #   title = "Module : Summary of the data", width = NULL,
  #       #   #show_dataUI(id = "mod3")
  #       # ),
  #     ),
  #   ),
  #   # tags$hr(style = "border: 2px solid #78BE20; border-radius: 5px;"),
  #   # fluidRow(
  #   #   column(
  #   #     width = 4,
  #   #     box(
  #   #       title = " ", width = NULL,
  #   #
  #   #     ),
  #   #     box(
  #   #       title = "Module : Summary statistics trend plot", width = NULL,
  #   #       "Box2"
  #   #     )
  #   #   ),
  #   #   column(
  #   #     width = 4,
  #   #     box(
  #   #       title = "Module : ", width = NULL,
  #   #       #show_dataUI(id = "mod3")
  #   #     ),
  #   #       box(
  #   #       title = "Module : ", width = NULL,
  #   #       Corr_dataUI(id = "mod4")
  #   #     )
  #   #   ),
  #   #   column(
  #   #     width = 4,
  #   #     box(
  #   #       title = "Module : Row statistics table", width = NULL,
  #   #       show_tableUI(id = "mod3")
  #   #     ),
  #   #     box(
  #   #       title = "Box4", width = NULL,
  #   #       "Missing values plot"
  #   #     )
  #   #   )
  #   # ),
  #   tags$hr(style = "border: 2px solid #78BE20; border-radius: 5px;"),
  #   br(),
  #   h2(class = "h2", icon("fas fa-laptop-code"), "Machine Learning Algorithm Evaluation"),
  #   br(),
  #   fluidRow(
  #     column(
  #       width = 4,
  #       box(
  #         title = "Random forest model", width = NULL, collapsible = FALSE,  solidHeader = FALSE,
  #         "Box1"
  #       )
  #     ),
  #     column(
  #       width = 4,
  #       box(
  #         title = "Multivariate adaptive regression splines model", width = NULL, collapsible = FALSE, solidHeader = FALSE,
  #         "Box2"
  
  #       )
  #     ),
  #     column(
  #       width = 4,
  
  #       box(
  #         title = "XGBoost model", width = NULL, collapsible = FALSE, solidHeader = FALSE,
  #         "Box3"
  #       )
  #     )
  #   ),
  #   hr(),
  #   p(em("Developed by"), br("Roger Tsai"), style = "text-align:center; font-family: times")
  # )
)


# Set Dashboard UI ================================================== ----
ui <- dashboardPage( # skin = "black",
  header,
  sidebar,
  body
)

