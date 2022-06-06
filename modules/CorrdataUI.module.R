#' @title Corr_UI
#' #' # In UI :
#' Corr_dataUI(id = "mod4")
#' # In Server
#' callModule(module = show_data,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }
Corr_dataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      12,
      plotOutput(ns("ui_corr_plot"), height = "650px") %>% withSpinner(color = "#78BE20"),
    )
  )
}


#' # In UI :
#' Corr_dataUI(id = "mod4")
#' # In Server
#' callModule(module = show_corr, id = "mod4", variable = variable, variable_name = variable_name)
#' }

show_corr <- function(input, output, session, selected_df = NULL, variable = NULL, variable_name = NULL, getfile, updated, PCA, Corr_btn) {
  ns <- session$ns

  cat("\n > Enter in CorrdataUI module \n")
  
  
  vals <- reactiveValues(
    Corr_df = NULL,
  )
  
  observeEvent(Corr_btn(), {
    req(PCA$the_data())
    df <- dplyr::select(PCA$the_data(), -c(Start.Time, System.Recipe))
    vals$Corr_df <- df
    
  })
  
  ## ggpair
  output$ui_corr_plot <- renderPlot({
    req(vals$Corr_df)
    df <- vals$Corr_df
    ggpairs(df,
      aes(color = Tool.Chamber, alpha = 0.5),
      upper = list(continuous = "points")
    )
  })

}
