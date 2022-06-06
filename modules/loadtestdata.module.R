#' Variable selection UI
#' @title load_testdataUI
#' @return UI page
#' # In UI :
#' load_testdataUI(id = "mod0")
#' # In Server
#' data_mod1 <- callModule(module = load_testdata, id = "mod1")
#' }

load_testdataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 6,
      selectInput(
        ns("SI_dataset"),
        "Test dataset:",
        choices = ls("package:datasets"),
        selected = "iris",
        multiple = FALSE,
      ),
    ),
    column(
      width = 6,
      selectInput(
        ns("SI_var"),
        "Choose Variable:",
        choices = NULL,
        # selected = "Not Loaded!",
        multiple = FALSE,
      ),
      actionButton(
        ns("AB_load"),
        "Load data",
        icon = icon("play-circle"),
        class = "btn btn-danger action-button",
        style = "color: #538cc6; background-color: #FFFFFF; border-color: #538cc6"
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

load_testdata <- function(input, output, session) {
  ns <- session$ns

  # Using a reactiveValues
  # Define the ReactiveValue to return : "toReturn"
  # with slots "variable", "variable_name" & "trigger"
  toReturn <- reactiveValues(
    selected_df = NULL,
    variables = NULL,
    variable_name = NULL,
    trigger = 0
  )

  # Update selectInput according to dataset
  observe({
    if (!is.null(input$SI_dataset)) {
      df <- get(input$SI_dataset)
      choices <- colnames(df)[sapply(df, is.numeric)]
      updateSelectInput(session,
        "SI_var",
        choices = choices
      )
    }
  })


  # (Re)load button
  observeEvent(input$AB_load, {
    toReturn$selected_df <- get(input$SI_dataset)
    toReturn$variable <- get(input$SI_dataset)[, input$SI_var]
    toReturn$variable_name <- input$SI_var
    toReturn$trigger <- toReturn$trigger + 1
  })

  return(toReturn)
}
