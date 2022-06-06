#' @title MARS_model_UI
#' #' # In UI :
#' MARS_model_UI(id = "mod2")
#' # In Server
#' callModule(module = show_PCA,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }

MARS_model_UI <- function(id) {
  ns <- NS(id)
  
  
  fluidRow(
    column(
      12,
      plotOutput(ns("MARS_model_plot"), height = "600px") %>% withSpinner(color = "#78BE20"),
      verbatimTextOutput(ns("predictions_MARS")) %>% withSpinner(color = "#78BE20")
    )
  )
  
}

#' @title MARS_model_UI
#' #' # In UI :
#' MARS_model_UI(id = "mod2")
#' # In Server
#' callModule(module = show_PCA,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }

show_MARS_model <- function(input, output, session, PCA, MARS_btn) {
  ns <- session$ns
  
  cat("\n > Enter in MARSmodelUI.module.R \n")
  
  algo_btn2_result <- reactiveValues()
  
  observeEvent(MARS_btn(), {
    
    dataset <- PCA$Combine_AandB_data()
    
    
    # Create a list of 80% of the rows in the original dataset we can use for training
    Training_index <- createDataPartition(dataset$Y_data, p = 0.80, list = FALSE)
    # select 20% of the data for validation
    test_dataset <- dataset[-Training_index, ]
    
    # # use the remaining 80% of data to training and testing the models
    training_dataset <- dataset[Training_index, ]
    
    mars1 <- earth(
      Y_data ~.,  
      data = training_dataset   
    )
    
    # Print model summary

    #predictions_mars <- predict(object = mars1, test_dataset)
    #print(predictions_mars)
    # 
    algo_btn2_result$mars1 <- mars1
    # algo_btn2_result$predictions_mars <- predictions_mars 
    # algo_btn2_result$test_dataset <- test_dataset 
    
  })
  
  output$predictions_MARS <- renderPrint({
    
    print(algo_btn2_result$mars1)
    
  })
    
  output$MARS_model_plot <- renderPlot({
    req(algo_btn2_result$mars1)
    plot(algo_btn2_result$mars1, which = 1)
  })
    
}