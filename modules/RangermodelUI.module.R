#' @title Ranger_model_UI
#' #' # In UI :
#' Ranger_model_UI(id = "mod2")
#' # In Server
#' callModule(module = show_Ranger_model,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }
#' 
#' ranger: A Fast Implementation of Random Forests

Ranger_model_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      plotOutput(ns("Ranger_variable_plot"), height = "450px", click = "click_varPlot") %>% withSpinner(color = "#78BE20"),
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
            ns("variableToShow_btn"),
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
    # plotOutput(ns("rf_model_plot"), height = "600px") %>% withSpinner(color = "#78BE20"),
    # verbatimTextOutput(ns("PR_predictions_rf")) %>% withSpinner(color = "#78BE20"),
  )
}




#' @title Ranger_model_UI
#' #' # In UI :
#' Ranger_model_UI(id = "mod2")
#' # In Server
#' callModule(module = show_Ranger_model,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }
#' 
#' ranger: A Fast Implementation of Random Forests
#' 
show_Ranger_model <- function(input, output, session, PCA, Ranger_btn, updated) {
  ns <- session$ns
  
  algo_btn3_result <- reactiveValues()
  
  observeEvent(Ranger_btn(), {
    
    
    #

    
  
    # Make class A,B
    ## ` ` ` ClassA
    classA <- PCA$selected_df()$df_classA
    # add column called 'y'
    y <- "class A"
    classA$y <- y

    ## ` ` ` ClassA
    classB <- PCA$selected_df()$df_classB
    # add column called 'y'
    y <- "class B"
    classB$y <- y


    #class <- factor(class,levels = c("None","Class A","Class B"))
    #print(class)
    All_data <- rbind(classA, classB)
    data <- rbind(classA, classB) %>%
      dplyr::select(-c(Tool.Chamber, Start.Time, System.Recipe, MD, ln.MD))
    
    data <- data[data$y != "None", ]
    print(data)
    data$y <- factor(as.character(data$y))
    
    
    m0 <- ranger(dependent.variable.name = "y", data = data, num.trees = 1000, mtry = 2, importance = "impurity", probability = T)

    ## Importance    
    fimp0 <- sort(m0$variable.importance, decreasing = T)

    #fimp0 <- data.table(name = names(fimp0), value = fimp0)
    # print(fimp0)
    
    
    
    
    #######################################################################
    
    # dataset <- PCA$Combine_AandB_data()
    #
    # # Create a list of 80% of the rows in the original dataset we can use for training
    # Training_index <- createDataPartition(dataset$Tool.Chamber, p = 0.80, list = FALSE)
    # # select 20% of the data for validation
    # test_dataset <- dataset[-Training_index, ]
    #
    # # # use the remaining 80% of data to training and testing the models
    # training_dataset <- dataset[Training_index, ]
    #
    # # # # #` ` ` Note : If a dependent variable is a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.
    # # # # # ` ` `+ Random Forest +` ` ` # # #
    # rf_classifier <- randomForest(Tool.Chamber ~ .,
    #                               data = training_dataset,
    #                               ntree = 1000,
    #                               mtry = 2)
    #
    # impo <- rf_classifier$importance
    # idx <- sort.int(rf_classifier$importance, decreasing = T, index.return = T)$ix
    #
    # fimp0 <- rf_classifier$importance[idx]
    #
    # names(fimp0) <- rownames(rf_classifier$importance)[idx]
    # fimp0 <- data.table(name = names(fimp0), value = fimp0)
    
    
    
    
    ## ` ` ` Importance
    #         name                            value    sensor                  step  statistic
    # class data
    # xdata <- env4$model$x
    # dataframe looks like below
    # TuningBallast_Flow_AI_Step2_Min TuningBallast_Flow_AI_Step2_Max
    
    # class <- data.frame(y = class, xdata)
    # class <- factor(class, levels = c("None","Class A","Class B"))
    # [1] 0.004 0.004 0.004 0.003 0.003
    ### BiasMatchImpedance_AI_step2_ SD     0.0002167  BiasMatchImpedance_AI     2      SD
    # mo <- randomForest(Tool.Chamber ~ .,
    #                               data = training_dataset,
    #                               ntree = 1000,
    #                               mtry = 2,
    # )
    # test <- mo$importance
    #                            MeanDecreaseGini
    ## CenterTap_AI_Step2_Avg         0.001
    
    # idx <- sort.int(m0$importance,decreasing = T, index.return = T)$ix
    # fimp0 <- m0$importance[idx]
    #
    # fimpo <- importance(rf_classifier)
    
    
    
    
    # # # # ` ` ` data format should look likes
    # name, value, sensor, step, statistic
    # TuningBallast_Flow_AI_Step2_Min 0.034 TuningBallast_Flow_AI 2 Min
    
    
    # p <- ggplot() +
    #   gome_bar(position = "stack", stat = "identity") +
    #   xlab("") + ylab("") + coord_flip()
    # # # # ` ` `
    
    
    
    # predictions_rf <- predict(rf_classifier, test_dataset)
    
    
    algo_btn3_result$classifier <- m0
    algo_btn3_result$fimp0 <- fimp0
    algo_btn3_result$All_data <- All_data
  })
  
  
  output$classifier <- renderPrint({
    print(algo_btn3_result$classifier)
  })
  
  
  output$Ranger_variable_plot <- renderPlot({
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
  
  observeEvent(input$variableToShow_btn, {
    All <- algo_btn3_result$All_data
    All <- All %>% dplyr::select(-c(System.Recipe, MD, ln.MD))
    
    # Convert data to a "tall" format
    d <- data.table::melt(All, id.vars = c("Start.Time", "Tool.Chamber", "y"), variable.name = "variable")
    numberfacets <- length(unique(d$variable))
    
    
    
    All_data <- na.omit(updated()) %>% dplyr::select(-c(System.Recipe))
    y <- "None"
    All_data$y <- y
    # Convert data to a "tall" format
    All_data <- data.table::melt(All_data, id.vars = c("Start.Time", "Tool.Chamber", "y"), variable.name = "variable")
    
    
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
    classA <- data %>% filter(y == "class A")
    classB <- data %>% filter(y == "class B")
    
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
  
  
  
  # output$gg_animate_plot <- renderImage({
  #
  #   data <- vals$ggAni
  #
  #   outfile <- tempfile(fileext='.gif')
  #
  #   p <- ggplot(data, aes(Start.Time, value, colour = Tool.Chamber)) +
  #     geom_point(size = 5, alpha = 0.7) +
  #     scale_fill_grey() +
  #     facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  #     theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
  #           axis.ticks.x = element_blank(), panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank()) +
  #     labs(y = "value", x = "process_datatime") +
  #     transition_states(Start.Time) +
  #     shadow_wake(wake_length = 5, alpha = FALSE) +
  #     ease_aes('cubic-in-out') +
  #     ggtitle('Process_datetime: {closest_state}',
  #             subtitle = 'Frame {frame} of {nframes}')
  #
  #     animate(p, duration = 15, fps = 10, width = 500, height = 500,  renderer = gifski_renderer("outfile.gif"))
  #     anim_save("outfile.gif", animate(p), renderer = gifski_renderer()) # New
  #
  #     list(src = "outfile.gif",
  #          contentType = 'image/gif'
  #          # width = 400,
  #          # height = 300,
  #          # alt = "This is alternate text"
  #     )}, deleteFile = TRUE)
  
  
  
  # output$PR_predictions_rf <- renderPrint({
  #
  #   print(algo_btn1_result$rf_classifier)
  #
  # })
  #
  #
  # output$rf_model_plot <- renderPlot({
  #   req(algo_btn1_result$predictions_rf)
  #   predictions <- algo_btn1_result$predictions_rf
  #   test_dataset <- algo_btn1_result$test_dataset
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
