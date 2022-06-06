#' @title show_dataUI
#' #' # In UI :
#' show_dataUI(id = "mod3")
#' # In Server
#' callModule(module = show_data,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }
show_dataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      12,
      #uiOutput(ns("ui_PL_histogram_var")),
      #uiOutput(ns("ui_SLI_nb_bins")),
      verbatimTextOutput(ns("PR_summary_var")) %>% withSpinner(color = "#78BE20"),
    )
  )
}



#' # In UI :
#' show_dataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = show_data, id = "mod3", variable = variable, variable_name = variable_name)
#' }

show_data <- function(input, output, session, selected_df = NULL, variable = NULL, variable_name = NULL, useggplot = FALSE, getfile, updated) {
  
  ns <- session$ns

  # # If useggplot, then SliderInput for number of bins
  # output$ui_SLI_nb_bins <- renderUI({
  #   if (!is.null(variable()) && useggplot) {
  #     sliderInput(ns("SLI_nb_bins"), label = "Number of bins", min = 5, max = 100, value = 30)
  #   }
  # })
  # 
  # # Histogram
  # output$PL_histogram_var <- renderPlot({
  #   if (useggplot) {
  #     require(ggplot2)
  #     qplot(variable(),
  #       geom = "histogram",
  #       bins = input$SLI_nb_bins, main = "", xlab = variable_name(),
  #       fill = I("blue"), col = I("red"), alpha = I(0.2)
  #     )
  #   } else {
  #     hist(variable(), main = variable_name(), xlab = NULL)
  #   }
  # })
  # 
  # # Use a renderUI of renderPlot to print "no dataset loaded" if no data
  # output$ui_PL_histogram_var <- renderUI({
  #   if (is.null(variable())) {
  #     tags$span(class = "warn", "No dataset loaded")
  #   } else {
  #     plotOutput(ns("PL_histogram_var"))
  #   }
  # })

  # Summary
  output$PR_summary_var <- renderPrint({
    req(updated())
    print(summary(updated()))
  })

}
