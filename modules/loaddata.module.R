#' load_dataUI UI
#' @title load_dataUI
#' @return UI page
#' # In UI :
#' load_dataUI(id = "mod1")
#' # In Server
#' data_mod1 <- callModule(module = load_data, id = "mod1")
#' }

load_dataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      selectInput(
        ns("SI_fleet"),
        "Fleet:",
        choices = get.fleetInfo(),
        width = "100%"
      )
    ),
    column(
      width = 12,
      dateRangeInput(
        ns("date"),
        "Date range input:",
        start = Sys.Date() - 1,
        end = Sys.Date(),
        width = "100%"
      )
    ),
    column(
      width = 12,
      selectInput(
        ns("SI_toolpm"),
        "Tool/Chamber:",
        choices = "Not Loaded!",
        selected = "Not Loaded!",
        multiple = TRUE,
        width = "100%"
      )
    ),
    column(
      width = 12,
      pickerInput(
        ns("SI_statistics"),
        "Summary statistics:",
        choices = "Not Loaded!",
        selected = "Not Loaded!",
        multiple = TRUE,
        width = "100%"
      )
    ),
    column(
      width = 12,
      pickerInput(
        ns("SI_recipe"),
        "System recipe:",
        choices = "Not Loaded!",
        selected = "Not Loaded!",
        multiple = TRUE,
        width = "100%"
      )
    ),
    # column(
    #   width = 4,
    #   selectInput(
    #     ns(""),
    #     "Statistics:",
    #     choices = c("Avg", "Var", "SD", "Min", "Max", "Med", "Skew", "Kurt", "Slope", "Rcp"),
    #     selected = "Not Loaded!",
    #     multiple = TRUE,
    #     width = "100%"
    #   ),
    # ),
    # column(
    #   width = 4,
    #   selectInput(
    #     ns(""),
    #     "Step:",
    #     choices = "Not Loaded!",
    #     selected = "Not Loaded!",
    #     multiple = TRUE,
    #     width = "100%"
    #   ),
    # ),
    column(
      width = 12,
      actionButton(
        ns("AB_load"),
        "Load data",
        icon = icon("play-circle"),
        class = "btn btn-danger action-button",
        style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0"
      )
    )
  )
}


#' Variable selection module server-side processing
#' @title show_data
#' # In UI :
#' load_dataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = load_data, id = "mod1")

load_data <- function(input, output, session, getfile) {
  ns <- session$ns

  
  
  
  # Update selectInput according to dataset
  observe({
    updatePickerInput(session,
      "SI_recipe",
      choices = getfile()$System.Recipe,
      options = list(`actions-box` = TRUE),
      selected = "Not Loaded!"
    )
  })


  observe({
    updateSelectInput(session,
      "SI_toolpm",
      label = "Tool PM:",
      choices = getfile()$Tool.Chamber,
      selected = getfile()$Tool.Chamber[1]
    )
  })

  


  # Update selectInput according to dataset
  observe({
    df <- getfile()
    
    #df <- df[ , -grep("Time", names(df), value = TRUE)]
    # Remove column name with "Time"
    df <- df[, -grep("Time|Gas", colnames(df))]
   
    choices <- colnames(df)[sapply(df, is.numeric)]
    updatePickerInput(session,
      "SI_statistics",
      choices = choices,
      options = list(`actions-box` = TRUE),
      #multiple = T,
      selected = "Not Loaded!"
    )
  })

  

  eventReactive(
    input$AB_load,{
        data <- getfile()
        req(getfile())
        
        ### testing -------------------------
        
        date_start_selected <- "2018-01-01"
        date_end_selected <- "2022-02-14"
        
        ### ---------------------------------
        
        fleet <- input$SI_fleet
        #date_start_selected <- input$date[1]
        #date_end_selected <- input$date[2]
        toolpm_selected <- input$SI_toolpm
        statistics_selected <- input$SI_statistics
        print(statistics_selected)
        system_recipe_selected <- input$SI_recipe

        df <- data %>%
          filter(Start.Time >
                   date_start_selected &
                 Start.Time <
                   date_end_selected,
                 System.Recipe %in%
                   system_recipe_selected,
                 Tool.Chamber %in%
                   toolpm_selected) %>% 
          dplyr::select(Tool.Chamber, System.Recipe, Start.Time, statistics_selected)
  })
  
  # updated <- reactive({
  #   getfile()
  #   req(getfile())
  #   date_start_selected <- input$date[1]
  #   date_end_selected <- input$date[2]
  #   process_recipe_selected <- input$SI_recipe
  #   toolpm_selected <- input$SI_toolpm
  #   statistics_selected <- input$SI_statistics
  #   df <- getfile() %>%
  #     filter(
  #       Start.time >
  #         date_start_selected &
  #         End.time <
  #         date_end_selected,
  #       Process.recipe %in%
  #         process_recipe_selected,
  #       Tool.Chamber %in%
  #         toolpm_selected
  #     ) %>%
  #     select(statistics_selected)
  # })
}
