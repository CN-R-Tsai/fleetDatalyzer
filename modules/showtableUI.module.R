#' @title show_tableUI
#' #' # In UI :
#' show_tableUI(id = "mod3")
#' # In Server
#' callModule(module = show_data,
#'     id = "mod3",
#'     variable = reactive(dataset$data_var_x))
#' }

show_tableUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      12,
      DT::dataTableOutput(ns("ui_data_tbl")) %>% withSpinner(color = "#78BE20")
    )
  )
}





#' # In UI :
#' show_dataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = show_data, id = "mod3", variable = variable, variable_name = variable_name)
#' }

show_table <- function(input, output, session, selected_df = NULL, variable = NULL, variable_name = NULL, useggplot = FALSE) {
  ns <- session$ns

  output$ui_data_tbl <- DT::renderDataTable({
    datatable(
      selected_df(),
      options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
    )
  })
}
