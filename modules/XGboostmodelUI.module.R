#' @title XGboost_model_UI
#' #' # In UI :
#' MARS_model_UI(id = "mod2")
#' # In Server
#' callModule(module = show_PCA,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }

XGboost_model_UI <- function(id) {
  ns <- NS(id)
  
  
  fluidRow(
    column(
      width = 12,
      plotOutput(ns("xgb_variable_plot"), height = "450px", click = "click_varPlot") %>% withSpinner(color = "#78BE20"),
      verbatimTextOutput(ns("classifier")) %>% withSpinner(color = "#78BE20"),
      fluidRow(
        column(
          width = 6,
          radioButtons(
            inputId = "radio_impBy",
            label = "Plot Type",
            choices = list(
              "Parameter" = 1,
              "Sensor" = 2,
              "Step" = 3
            )
          ),
          actionButton(
            ns("xgb_variableToShow_btn"),
            "Show Variable",
            width = "100%",
            #icon = icon("play-circle"),
            class = "btn btn-danger action-button",
            style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
          ),
          br(),
          br(),
        ),
        column(
          width = 6,
          radioButtons(
            inputId = "radio_impByStack",
            label = "Stack Option",
            choices = list(
              "By Step" = 1,
              "By Statistics" = 2
            ), selected = 1
          ),
        )
      ),
      fluidRow(
        column(
          width = 6,
          textInput(
            inputId = "text_modelDescription",
            label = NULL,
            placeholder = "Model Description",
            width = "100%"
          )
        ),
        column(
          width = 6,
          actionButton(
            ns("saveModel_btn"),
            "Save Model",
            width = "100%",
            #icon = icon("play-circle"),
            class = "btn btn-danger action-button",
            style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
          )
        )
      )
    ),
    column(
      width = 12,
      uiOutput(ns("plot.ui")),
      # br(),
      # imageOutput(ns("gg_animate_plot"),  width = "100%") %>% withSpinner(color = "#78BE20"),
    )
    # verbatimTextOutput(ns("rf_importance_classA")) %>% withSpinner(color = "#78BE20"),
    # plotOutput(ns("rf_model_plot"), height = "600px") %>% withSpinner(color = "#78BE20"),
    # verbatimTextOutput(ns("PR_predictions_rf")) %>% withSpinner(color = "#78BE20"),
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

show_XGboost_model <- function(input, output, session, PCA, XGboost_btn, updated) {
  ns <- session$ns
  
  cat("\n > Enter in XGboostmodelUI.module.R \n")
  
  algo_btn3_result <- reactiveValues()
  
  observeEvent(XGboost_btn(), {
    
    
    ## ` ` ` ClassA
    classA <- PCA$selected_df()$df_classA
    #add column called 'y'
    class <- "class A"
    classA$class <- class
    
    ## ` ` ` ClassA
    classB <- PCA$selected_df()$df_classB
    #add column called 'y'
    class <- "class B"
    classB$class <- class
    
    

    All_data <- rbind(classA, classB) 
    
    #
    data_test <- rbind(classA, classB) %>%
      dplyr::select(-c(Tool.Chamber, Start.Time, System.Recipe, class,MD, ln.MD))
    rownames(data_test) <- NULL
    

    
    data <- rbind(classA, classB) %>% 
      dplyr::select(-c(Tool.Chamber, Start.Time, System.Recipe, MD, ln.MD))
    rownames(data) <- NULL
    data <- data[data$class != "None",]
    
    data$class <- factor(as.character(data$class))

    ydata <- data$class

    idx <- which(ydata != "None")
   
    ydata <- ydata[idx]

    
    ydata <- as.numeric(factor(as.character(ydata)))-1
   
    #z <- as.data.frame(data_test)
    label_y <- t(ydata)
    #print(label_y)
          
    # z <- as.matrix(data_test)
    # print(z)
    #print(str(z))
    
    dtrain = xgb.DMatrix(as.matrix(data_test), label=label_y)
    #print(dtrain)
    m <- xgboost(dtrain, booster = "gbtree", objective = "binary:logistic", nthread = 10, silent = 0, max_depth = 6, eta = 0.1, colsample_bytree = 0.7, subsample = 0.7, nrounds =500, save_period = NULL)
   
    # m$xnames <- colnames(data_test)
    # print(m)
    imp_mat <- xgb.importance(colnames(data_test), m)
    
    # Feature       Gain     Cover Frequency
    # 1: Step4_Step.length 0.96055744 0.6614243  0.637931
    # 2: Step7_Step.length 0.03944256 0.3385757  0.362069
    
    importance <- imp_mat$Frequency
    #[1] 0.625 0.375
    
    names(importance) <- imp_mat$Feature
    # Step4_Step.length Step7_Step.length 
    # 0.625             0.375
    
    idx <- sort.int(importance, decreasing = T, index.return = T)$ix
    #[1] 1 2
    
    # Importance
    fimp0 <- importance[idx]
                        
    names(fimp0) <- imp_mat$Feature[idx]
    fimp0 <- data.table(name = names(fimp0), value = fimp0)
   
    Features <- imp_mat$Feature
    
    # dataset <- PCA$Combine_AandB_data()
    # 
    # # Create a list of 80% of the rows in the original dataset we can use for training
    # Training_index <- createDataPartition(dataset$Y_data, p = 0.80, list = FALSE)
    # # select 20% of the data for validation
    # test_dataset <- dataset[-Training_index, ]
    # # # use the remaining 80% of data to training and testing the models
    # training_dataset <- dataset[Training_index, ]
    # 
    # 
    # # # ` ` `+ XGboost +` ` ` # # #
    # training <- training_dataset[, -which(names(training_dataset) == "Y_data")]
    # train_label <- training_dataset[, "Y_data"]
    # 
    # test <- test_dataset[, -which(names(test_dataset) == "Y_data")]
    # test_label <- test_dataset[, "Y_data"]
    # 
    # 
    # xgb_train <- xgb.DMatrix(data = data.matrix(training), label = train_label)
    # xgb_test <- xgb.DMatrix(data = data.matrix(test), label = test_label)
    # 
    # 
    # fit.xbst <- xgboost(
    #   data = xgb_train,
    #   max.depth = 2,
    #   nround = 2 # max number of boosting iterations
    # )
    # 
    # predictions_xbst <- predict(object = fit.xbst, xgb_test)
    
    # algo_btn3_result$fit.xbst <- fit.xbst
    # algo_btn3_result$predictions_xbst <- predictions_xbst
    # algo_btn3_result$test_dataset <-  test_dataset
    
    algo_btn3_result$fimp0 <- fimp0
    algo_btn3_result$All_data <- All_data
    algo_btn3_result$feature <- Features
    algo_btn3_result$classifier <- m
  })
  
  
  output$classifier <- renderPrint({
    print(algo_btn3_result$classifier)
  })
  
  
  
  output$xgb_variable_plot <- renderPlot({
    req(algo_btn3_result$fimp0)
    fimp0 <- algo_btn3_result$fimp0
    calculate_importance(fimp0)
    
    p <- ggplot(fimp, aes(x = reorder(name, value), y = value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("") +
      ylab("") +
      coord_flip() +
      ggtitle("Feature importance")
    theme(legend.position = "bottom", legend.box = "horizontal")
    p
  })
  
  
  
  
  vals <- reactiveValues(
    var_df = NULL,
    number_facet = NULL,
    all_data = NULL,
    ggAni = NULL,
  )
  
  observeEvent(input$xgb_variableToShow_btn, {
    
    All <- algo_btn3_result$All_data

    All <- All %>% dplyr::select(-c(System.Recipe, MD, ln.MD))
    
    # Convert data to a "tall" format
    d <- data.table::melt(All, id.vars = c("Start.Time", "Tool.Chamber", "class"), variable.name = "variable")
    numberfacets <- length(unique(d$variable))
    
    
    All_data <- na.omit(updated()) %>% dplyr::select(-c(System.Recipe))
    class <- "None"
    All_data$class <- class
    # Convert data to a "tall" format
    All_data <- data.table::melt(All_data, id.vars = c("Start.Time", "Tool.Chamber", "class"), variable.name = "variable")
    
    
    # All_data_ggAni <- na.omit(updated()) %>% select(-c(System.Recipe))
    # y <- "None"
    # All_data_ggAni$y <- y
    # # Convert data to a "tall" format
    # All_data_ggAni <- data.table::melt(All_data_ggAni, id.vars = c("Start.Time", "Tool.Chamber", "y"), variable.name = 'variable')
    
    
    vals$var_df <- d
    vals$number_facet <- numberfacets
    vals$all_data <- All_data
    # vals$ggAni <- All_data_ggAni
  })
  
  
  
  
  output$stats_variable_plot <- renderPlot({
    req(vals$var_df)
    All_data <- vals$all_data
    data <- vals$var_df
    classA <- data %>% filter(class == "class A")
    classB <- data %>% filter(class == "class B")
    
    All_data$Start.Time <- as.Date(All_data$Start.Time, format = "%Y/%m/%d %H:%M", tz = "GMT")
    p <- ggplot(All_data, aes(x = Start.Time, y = value, colour = Tool.Chamber)) +
      geom_point() +
      facet_wrap(~variable, ncol = 1, scales = "free_y") +
      xlab("process_datatime") +
      ylab("value") +
      geom_point(
        data = classA,
        color = "red",
        shape = 2,
        size = 2
      ) +
      geom_point(
        data = classB,
        color = "blue",
        shape = 1,
        size = 2
      ) +
      scale_fill_grey() +
      theme(
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    p
  })
  
  
  plotHeight <- reactive(300 * vals$number_facet)
  #
  #
  
  output$plot.ui <- renderUI({
    req(vals$number_facet)
    plotOutput(ns("stats_variable_plot"), height = plotHeight())
  })
  
  
  
  
  # output$predictions_XGboost <- renderPrint({
  #   
  #   # print(algo_btn3_result$fit.xbst)
  #   
  # })
    
  # output$XGboost_model_plot <- renderPlot({
    # req(algo_btn3_result$predictions_xbst)
    # predictions <- algo_btn3_result$predictions_xbst
    # test_dataset <- algo_btn3_result$test_dataset
    # 
    #   data_mod <- data.frame(
    #     Predicted = predictions,
    #     Observed = test_dataset$Y_data
    #   )
    # 
    #   lm_eqn <- function(data_mod) {
    #     m <- lm(Observed ~ Predicted, data_mod)
    #     eq <- substitute(
    #       italic(y) == a + b %.% italic(x) * "," ~ ~ italic(r)^2 ~ "=" ~ r2,
    #       list(
    #         a = format(unname(coef(m)[1]), digits = 2),
    #         b = format(unname(coef(m)[2]), digits = 2),
    #         r2 = format(summary(m)$r.squared, digits = 3)
    #       )
    #     )
    #     as.character(as.expression(eq))
    #   }
    # 
    #   annotations <- data.frame(
    #     xpos = c(Inf),
    #     ypos = c(Inf),
    #     annotateText = lm_eqn(data_mod),
    #     hjustvar = c(1),
    #     vjustvar = c(1)
    #   )
    # 
    #   p <- ggplot(data_mod, aes(x = Observed, y = Predicted)) +
    #     geom_point(size = 2) +
    #     geom_text(data = annotations, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText), parse = TRUE) +
    #     geom_smooth(method = "lm", se = FALSE, colour = "red", alpha = .9)
    #   p
    
  # })
    
}