#' Analytics_UI
#' @title Analytics_UI.module
#' @return UI page
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' # In Server
#' data_mod1 <- callModule(module = load_data, id = "mod1")
#' }

Analytics_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "analysisTab",
    br(),
    br(),
    br(),
    h2(class = "h2", "Turning data into actionable insights", emo::ji("search"),"and insights into predictions!"),
    br(),
    br(),
    br(),
    fluidRow(
      # box(
      #   title = "Module 1 : Feature selection", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
      #   load_dataUI(id = "mod1")
      # ),
      column(
        width = 4,
        # box(
        #   title = "Module 0 : Test data", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
        #   load_testdataUI(id = "mod0")
        # ),
        box(
          title = "Load CSV data", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
          load_CSVdataUI(id = "mod0.5")
        ),
      ),
      column(
        width = 4,
        # box(
        #   title = "Module 0.5 : Load CSV data", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
        #   load_CSVdataUI(id = "mod0.5")
        # ),
        # box(
        #   title = "Module Y : Data inspection table", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
        #   data_inspectUI(id = "modX")
        # ),
        box(
          title = "Feature Selection", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
          load_dataUI(id = "mod1")
        ),
      ),
      column(
        width = 4,
        dataSize_info_UI(id = "dataSizeInfo")
      )
    ),
    # tags$hr(style = "border: 2px solid #78BE20; border-radius: 5px;"),
    # h2("TEST AREA"),
    # box(
    #   title = "Module X : Data inspection table", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
    #   data_inspectUI(id = "modX")
    # ),
    
    #tags$hr(style = "border: 2px solid #78BE20; border-radius: 5px;"),
    br(),
    fluidRow(
      column(
        width = 8,
        box(
          title = "Reduced dimension plot", width = NULL, status = "danger", solidHeader = TRUE,
          PCA_dataUI(id = "mod2")
        ),
        box(
          title = "Correlation plot ", width = NULL, status = "danger", solidHeader = TRUE,
          Corr_dataUI(id = "mod4")
        ),
      ),
      column(
        width = 4,
        #box(
          #title = "Data Manipulation", width = NULL, status = "danger", solidHeader = TRUE,
          Brushedpoint_dataUI(id = "mod6")
        #),
      ),
      column(
        width = 6,
        box(
          title = "Module : Scree plot ", width = NULL, status = "danger", solidHeader = TRUE,
          Scree_dataUI(id = "mod5")
        ),
        # box(
        #   title = "N/A", width = NULL, status = "danger", solidHeader = TRUE, height = "500px"
        #   #show_dataUI(id = "mod3")
        # ),
      )
    ),
    # tags$hr(style = "border: 2px solid #78BE20; border-radius: 5px;"),
    # fluidRow(
    #   column(
    #     width = 4,
    #     box(
    #       title = "Row statistics table", width = NULL,
    #    
    #     ),
    #     box(
    #       title = "Module : Summary statistics trend plot", width = NULL,
    #       "Box2"
    #     )
    #   ),
    #   column(
    #     width = 4,
    #     box(
    #       title = "Correlation plot ", width = NULL,
    #       show_dataUI(id = "mod3")
    #     ),
    # 
    #   ),
    #   column(
    #     width = 4,
    #     box(
    #       title = "Module : Row statistics table", width = NULL,
    #       show_tableUI(id = "mod3")
    #     ),
    #     box(
    #       title = "Box4", width = NULL,
    #       "Missing values plot"
    #     )
    #   )
    # ),
    #tags$hr(style = "border: 2px solid #78BE20; border-radius: 5px;"),
    br(),
    h2(class = "h2", emo::ji("computer"), "Machine Learning Algorithm Evaluation"),
    br(),
    fluidRow(
      column(
        width = 4,
        box(
          title = "ML Model 1", width = NULL, status = "danger", solidHeader = TRUE, 
          #style = 'display:block;width:100%;height:85vh;overflow-y: scroll;',  
          RF_model_UI(id = "RF_model_UI_1")
        ),
        box(
          title = "ML Regression Model", width = NULL, status = "danger", solidHeader = TRUE, 
          RF_Regression_UI(id = "rfGress")
        )
      ),
      column(
        width = 4,
        box(
          title = "ML Model 2", width = NULL, status = "danger", solidHeader = TRUE,
          XGboost_model_UI(id = "XGboost_model_UI_1")
        )
      ),
      column(
        width = 4,
        box(
          title = "ML Model 3", width = NULL, status = "danger", solidHeader = TRUE,
          Ranger_model_UI(id = "Ranger_model_UI_1")
        )
      )
    ),
    hr(),
    p(em("Developed by"), br("Roger Tsai"), style = "text-align:center; font-family: times")
  )

}


#' Analytics_UI
#' @title Analytics_UI.module
#' @return UI page
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' # In Server
#' data_mod1 <- callModule(module = load_data, id = "mod1")
#' }

show_Analytics <- function(input, output, session) {
  ns <- session$ns
  
  cat("\n > Enter in Analytics_UI module \n")
  
}