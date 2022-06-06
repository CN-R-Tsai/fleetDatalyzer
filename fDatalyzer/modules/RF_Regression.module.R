#' @title RF_Regression_UI
#' #' # In UI :
#' RF_Regression_UI(id = "rfGress")
#' # In Server
#' callModule(module = show_RF_Regre,
#'     id = "rfGress")
#' }
#' 
RF_Regression_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      textOutput(ns("txt")),
      plotOutput(ns("rf_model_plot"), height = "600px") %>% withSpinner(color = "#78BE20"),
      plotOutput(ns("rf_model_plot_r2"), height = "600px") %>% withSpinner(color = "#78BE20"),
      #plotOutput(ns("variable_plot"), height = "450px", click = "click_varPlot") %>% withSpinner(color = "#78BE20"),
      verbatimTextOutput(ns("regress_rf")) %>% withSpinner(color = "#78BE20"),
    ),
    # column(
    #   width = 12,
    #   uiOutput(ns("plot.ui")),
    #   #br(),
    #   #imageOutput(ns("gg_animate_plot"),  width = "100%") %>% withSpinner(color = "#78BE20"),
    # )
    # verbatimTextOutput(ns("rf_importance_classA")) %>% withSpinner(color = "#78BE20"),

    # verbatimTextOutput(ns("PR_predictions_rf")) %>% withSpinner(color = "#78BE20"),
  )
}



#' @title RF_Regression_UI
#' #' # In UI :
#' RF_Regression_UI(id = "rfGress")
#' # In Server
#' callModule(module = show_RF_Regre,
#'     id = "rfGress")
#' }
#' 
show_RF_Regre <- function(input, output, session, PCA, updated, rf_regress_btn, radioRegre_btn) {
  ns <- session$ns
  
  
  
  output$txt <- renderText({
    paste("You chose", radioRegre_btn())
  })
  
  
  rf_regress_btn_result <- reactiveValues()
  
  
  observeEvent(rf_regress_btn(), {
    
    dataset <- PCA$Combine_AandB_data()
  
    
    # # Create a list of 80% of the rows in the original dataset we can use for training
    Training_index <- createDataPartition(dataset$Y_data, p = 0.80, list = FALSE)
    # # select 20% of the data for validation
    test_dataset <- dataset[-Training_index, ] %>%
      dplyr::select (-c(Start.Time, System.Recipe))

    test_dataset_wTime <- dataset[-Training_index, ] 
    # # # use the remaining 80% of data to training and testing the models
    training_dataset <- dataset[Training_index, ] %>% 
      dplyr::select (-c(Start.Time, System.Recipe))
    #print(training_dataset)
    
    
    # # # # #` ` ` Note : If a dependent variable is a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.
    # # # # # ` ` `+ Random Forest +` ` ` # # #
    rf_classifier <- randomForest(Y_data ~ .,
                                  data = training_dataset,
                                  ntree = 1000,
                                  mtry = 2)
    
    predictions_rf <- predict(rf_classifier, test_dataset)
 
    
    
    test_dataset <- cbind(process_time = test_dataset_wTime$Start.Time, test_dataset)

    
    rf_regress_btn_result$rf_classifier <- rf_classifier
    rf_regress_btn_result$predictions_rf <- predictions_rf
    rf_regress_btn_result$test_dataset <- test_dataset
    
  })
  
  
  output$regress_rf <- renderPrint({

    print(rf_regress_btn_result$rf_classifier)

  })
  
  
  
  output$rf_model_plot <- renderPlot({
    
    
    req(rf_regress_btn_result$predictions_rf)
    predictions <- rf_regress_btn_result$predictions_rf
    test_dataset <- rf_regress_btn_result$test_dataset

    
    data_mod <- data.frame(
      Predicted = predictions,
      Observed = test_dataset$Y_data
    ) 
    
    data_mod <- cbind(data_mod, Process_date = as.Date(test_dataset$process_time))
    
  
    p <- ggplot(data_mod, aes(x = Process_date, y = Predicted)) +
      geom_point(size = 2, color = "blue") +
      geom_point(data = data_mod, aes(x = Process_date, y = Observed), size = 2, color = "red") +
      xlab("process_date") +
      ylab("Y_data") 
    
    p
    
  })
  
  
  
  
  output$rf_model_plot_r2 <- renderPlot({
    
    
    req(rf_regress_btn_result$predictions_rf)
    predictions <- rf_regress_btn_result$predictions_rf
    test_dataset <- rf_regress_btn_result$test_dataset
    
    
    data_mod <- data.frame(
      Predicted = predictions,
      Observed = test_dataset$Y_data
    ) 
    
    data_mod <- cbind(data_mod, Process_date = as.Date(test_dataset$process_time))

    lm_eqn <- function(data_mod) {
      m <- lm(Observed ~ Predicted, data_mod)
      eq <- substitute(
        italic(y) == a + b %.% italic(x) * "," ~ ~ italic(r)^2 ~ "=" ~ r2,
        list(
          a = format(unname(coef(m)[1]), digits = 2),
          b = format(unname(coef(m)[2]), digits = 2),
          r2 = format(summary(m)$r.squared, digits = 3)
        )
      )
      as.character(as.expression(eq))
    }
    
    annotations <- data.frame(
      xpos = c(Inf),
      ypos = c(Inf),
      annotateText = lm_eqn(data_mod),
      hjustvar = c(1),
      vjustvar = c(1)
    )
    
    p <- ggplot(data_mod, aes(x = Observed, y = Predicted)) +
            geom_point() +
            geom_text(data = annotations, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText), parse = TRUE) +
            geom_smooth(method = "lm", se = FALSE, colour = "red", alpha = .9)
    p
    
  })
  
  

}
