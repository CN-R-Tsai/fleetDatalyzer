#' @title Scree_dataUI
#' #' # In UI :
#' Scree_dataUI(id = "mod5")
#' # In Server
#' callModule(module = show_PCA,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }
Scree_dataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      12,
      plotOutput(ns("plot2")) %>% withSpinner(color = "#78BE20"),
    )
  )
}




#' # In UI :
#' Scree_dataUI(id = "mod5")
#' # In Server
#' data_module1 <- callModule(module = show_PCA, id = "mod4", variable = variable, variable_name = variable_name)
#' }



show_Scree <- function(input, output, session, updated, PCA) {
  ns <- session$ns

  output$plot2 <- renderPlot({
    req(PCA$pca_objects$pca_output)
    pca_output <- PCA$pca_objects$pca_output
    eig <- (pca_output$sdev)^2
    variance <- eig * 100 / sum(eig)
    cumvar <- paste(round(cumsum(variance), 1), "%")
    eig_df <- data.frame(
      eig = eig,
      PCs = colnames(pca_output$x),
      cumvar = cumvar
    )
    ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
      geom_bar(stat = "identity", fill = "white", colour = "black") +
      geom_text(
        label = cumvar, size = 4,
        vjust = -0.4
      ) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variances") +
      ylim(0, (max(eig_df$eig) * 1.1))
  })
}
