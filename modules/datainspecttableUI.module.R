#' @title DataInsepct_UI
#' #' # In UI :
#' DataInspect_dataUI(id = "modX")
#' # In Server
#' callModule(module = show_data,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }
data_inspectUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      12,
      #DT::dataTableOutput(ns("ui_inspect_tbl")) %>% withSpinner(color = "#78BE20"),
      DT::dataTableOutput(ns("ui_updated_tbl")) %>% withSpinner(color = "#78BE20")
    )
  )
}


#' # In UI :
#' DataInspect_dataUI(id = "modX")
#' # In Server
#' data_module1 <- callModule(module = show_data, id = "mod3", variable = variable)
#' }

dataInspec_table <- function(input, output, session, getfile, updated) {
  ns <- session$ns

  # output$ui_inspect_tbl <- DT::renderDataTable({
  #   datatable(
  #     getfile(),
  #     options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
  #   )
  # })
  # 
  output$ui_updated_tbl <- DT::renderDataTable({
    datatable(
      updated(),
      options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
    )
  })
}
