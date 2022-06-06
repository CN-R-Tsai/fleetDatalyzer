#' @title PCA_UI
#' #' # In UI :
#' PCA_dataUI(id = "mod2")
#' # In Server
#' callModule(module = show_PCA,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }
PCA_dataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      12,
      # plotOutput(ns("PCA_plot"),
      #   dblclick = ns("PCAplot_dblclick"),
      #   brush = brushOpts(id = ns("PCA_plot_brush"), resetOnNew = FALSE),
      #   width = "100%",
      #   height = "800px"
      # ) %>% withSpinner(color = "#78BE20"),
      # plotOutput(ns("raw_plot"), height = "600px") %>% withSpinner(color = "#78BE20"),
      uiOutput(ns("plot_test.ui")),
      # plotOutput(ns("test_plot"),
      #   click = clickOpts(id = ns("testPlot_click")),
      #   dblclick = ns("test_PCAplot_dblclick"),
      #   brush = brushOpts(id = ns("test_plot_brush")),
      #   width = "100%",
      #   height = "800px"
      # ) %>% withSpinner(color = "#78BE20"),
      uiOutput(outputId = ns("Plot_ui_mainPlotToolTip")),
      uiOutput(ns("the_grouping_variable")),
      uiOutput(ns("the_pcs_to_plot_x")),
      uiOutput(ns("the_pcs_to_plot_y")),
      selectInput(
        inputId = ns(""),
        label = "Color by",
        choices = "",
        selected = ""
      ),
      column(
        6,
        radioButtons(
          inputId = ns("PCA_plotTypeOpt"),
          label = "Plot Type",
          choices = list(
            "Scatter" = "Scatter",
            "Boxplot" = "box"
          ),
          selected = "Scatter"
        ),
        conditionalPanel(
          condition = 'input.PCA_plotTypeOpt == "Scatter"',
          ns = ns,
          fluidRow(column(
            11,
            offset = 1,
            checkboxGroupInput(
              inputId = ns("PCA_plotTypeOpt_check_lineOption"),
              label = "Add Line",
              choices = list(
                "Line" = "line",
                "Curve" = "smooth",
                "Density" = "density"
              )
            )
          ))
        ),
        conditionalPanel(
          condition = 'input.PCA_plotTypeOpt == "box"',
          ns = ns,
          fluidRow(column(
            11,
            offset = 1,
            checkboxGroupInput(
              inputId = ns("PCA_plotTypeOpt_check_boxplotStat"),
              label = "Add Statistics",
              choices = list(
                "Min" = 1,
                "25%" = 2,
                "Med" = 3,
                "75%" = 4,
                "Max" = 5
              )
            )
          ))
        )
      ),
      column(
        6,
        checkboxGroupInput(
          inputId = ns("PCA_plotTypeOpt_check_plotOption"),
          label = "Plot Option",
          choices = c(
            "Add ellipse" = "ellipse",
            "Facet" = "facet",
            "Hide Legend" = "noLegend"
          )
        )
      )
    ),


    # br(),
    # h4("Class A Dataframe"),
    # DT::dataTableOutput(ns("bp_tbl_class_A")) %>% withSpinner(color = "#78BE20"),
    # # # verbatimTextOutput(ns("df_classA")) %>% withSpinner(color = "#78BE20"),
    # # br(),
    # h4("Class B Dataframe"),
    # DT::dataTableOutput(ns("bp_tbl_class_B")) %>% withSpinner(color = "#78BE20"),
    # br(),
    # h4("Class AB Dataframe"),
    # DT::dataTableOutput(ns("bp_tbl_AandB")) %>% withSpinner(color = "#78BE20"),
    # br(),
    # h4("All_data_frame"),
    # DT::dataTableOutput(ns("tbl_all_data")) %>% withSpinner(color = "#78BE20"),
    # br(),
    # verbatimTextOutput(ns("PR_predictions_rf")) %>% withSpinner(color = "#78BE20"),
    # plotOutput(ns("rf_model_plot"), height = "600px") %>% withSpinner(color = "#78BE20"),
  )



  # h4("Class B Dataframe"),
  # DT::dataTableOutput(ns("bp_tbl_class_B")) %>% withSpinner(color = "#78BE20"),
  # h4("Class A Dataframe"),



  # h4("RF model plot"),
  # plotOutput(ns("rf_model_plot"), height = "600px") %>% withSpinner(color = "#6CE3C6"),
  # tags$hr(),

  # verbatimTextOutput(ns("scree_summary_var")) %>% withSpinner(color = "#78BE20"),
  # h3("Scree plot"),
  # plotOutput(ns("plot2")) %>% withSpinner(color = "#78BE20"),
  # fluidRow(
  #   column(
  #     width = 6,
  #     box(
  #       id = "box1",
  #       title = actionLink("titleId", "Data parameters", icon = icon("arrow-circle-up")), width = NULL, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "danger",
  #       uiOutput(ns("the_grouping_variable")),
  #       uiOutput(ns("the_pcs_to_plot_x")),
  #       uiOutput(ns("the_pcs_to_plot_y"))
  #     )
  #   ),
  #   column(
  #     width = 6,
  #     box(
  #       id = "box2",
  #       title = actionLink("titleId", "Settings", icon = icon("arrow-circle-up")), width = NULL, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "danger",
  #       sliderInput("width", "y-axis", min = 100, max = 500, value = 250),
  #     )
  #   ),
  # ),
}




#' # In UI :
#' PCA_dataUI(id = "mod2")
#' # In Server
#' data_module1 <- callModule(module = show_PCA, id = "mod4", variable = variable, variable_name = variable_name)
#' }


show_PCA <- function(input, output, session, selected_df = NULL, variable = NULL, variable_name = NULL, getfile, updated, Add_classA_btn, Add_classB_btn, Reset_btn, RF_btn, Ranger_btn, XGboost_btn, Remove_data_btn, PCA_btn) {
  ns <- session$ns


  cat("\n > Enter in show_PCA.R \n")

  # choose a grouping variable
  
  output$the_grouping_variable <- renderUI({

    the_data <- updated()
    # for grouping we want to see only cols where the number of unique values are less than
    # 10% the number of observations
    grouping_cols <- sapply(seq(1, ncol(the_data)), function(i) length(unique(the_data[, i])) < nrow(the_data) / 10)

    the_data_group_cols <- the_data[, grouping_cols, drop = FALSE]
    
    # drop down selection
    selectInput(
      inputId = ns("the_grouping_variable"),
      label = "Grouping variable:",
      choices = c("None", names(the_data_group_cols))
    )
  })

  

  #   raw_data <- na.omit(updated())
  #   makePlot_raw(raw_data)
  # })
  #
  # output$raw_plot <- renderPlot({
  #   raw_data_plot()
  # })
  #
  # output$raw_data <- DT::renderDataTable({
  #   raw_data <- na.omit(updated())
  #   datatable(raw_data,
  #             options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
  #   )
  # })
  #

  ################# 02/13 add PCA_btn #################

  pca_objects <- reactiveValues()


  observeEvent(PCA_btn(), {
    the_data <- na.omit(updated()) %>% calculate_pca()
    
    #pca_objects$upload <- upload_data
    pca_objects$the_data <- the_data$the_data
    pca_objects$pca_output <- the_data$pca_output
    pca_objects$pcdata <- the_data$pcdata
  })


  output$the_pcs_to_plot_x <- renderUI({

    # 03/30 add MD
    pca_output <- pca_objects$pcdata

    # pca_output <- pca_objects$pca_output$x

    # # # drop down selection
    selectInput(
      inputId = ns("the_pcs_to_plot_x"),
      label = "X axis:",
      choices = colnames(pca_output),
      selected = "PC1"
    )
  })


  output$the_pcs_to_plot_y <- renderUI({

    # 03/30 add MD
    pca_output <- pca_objects$pcdata

    # pca_output <- pca_objects$pca_output$x
    # # # drop down selection
    selectInput(
      inputId = ns("the_pcs_to_plot_y"),
      label = "Y axis:",
      choices = colnames(pca_output),
      selected = "PC2"
    )
  })


  the_data <- reactive({
    the_data <- na.omit(updated())
  })







  ############################################################################

  # pca_objects <- reactive({
  #     the_data <- na.omit(updated()) %>%
  #       calculate_pca()
  # })
  # # #
  # the_data <- reactive({
  #     the_data <- na.omit(updated())
  # })
  # # #
  # # #
  # # #
  # output$the_pcs_to_plot_x <- renderUI({
  #    pca_output <- pca_objects()$pca_output$x
  # # #
  # # #   # drop down selection
  #    selectInput(
  #      inputId = ns("the_pcs_to_plot_x"),
  #      label = "X axis:",
  #      choices = colnames(pca_output),
  #      selected = "PC1"
  #    )
  # })
  # # #
  # # #
  # output$the_pcs_to_plot_y <- renderUI({
  #     pca_output <- pca_objects()$pca_output$x
  # #    # drop down selection
  #     selectInput(
  #       inputId = ns("the_pcs_to_plot_y"),
  #       label = "Y axis:",
  #       choices = colnames(pca_output),
  #       selected = "PC2"
  #     )
  # })






  ################################################################################


  brushedpoint_data <- reactive({
    req(pca_objects$pcdata, input$test_plot_brush)
    brushedPoints(pca_objects$pcdata, input$test_plot_brush) %>% dplyr::select(-contains("PC"))
  })

  # BP <- reactive({
  #   brushedPoints(pca_objects()$pcdata, input$PCA_plot_brush)
  # }) # allRows = TRUE is important for selecting the brushed pts


  BP_test <- reactive({
    brushedPoints(pca_objects$pcdata, input$test_plot_brush, allRows = TRUE)
  }) # allRows = TRUE is important for selecting the brushed pts


  ######### ------------- Delete the brushed point ------------- ##############


  observeEvent(Remove_data_btn(), {
    Var1 <- brushedPoints(pca_objects$pcdata, input$test_plot_brush, allRows = TRUE)
    # pca_objects$pcdata(Var1[!Var1$selected_, names(Var1) != "selected_", drop = FALSE])
  })


  #############################################################################

  vals <- reactiveValues(
    df_data_A = NULL,
    df_tata_B = NULL
  )
  # #
  # #
  observeEvent(Add_classA_btn(), {
    vals$df_data_A <- brush
  })
  # #
  # #
  observeEvent(Add_classB_btn(), {
    vals$df_data_B <- brush
  })


  #############################################################

  brush <- NULL
  makeReactiveBinding("brush")

  observeEvent(input$test_plot_brush, {
    brush <<- input$test_plot_brush
  })

  observeEvent(Reset_btn(), {
    session$resetBrush("test_plot_brush")
    brush <<- NULL
    vals$df_data_A <<- NULL
    vals$df_data_B <<- NULL
  })



  ######### -----Reset-------------####################

  # observeEvent(Add_classA_btn(), {
  #   env4$classA <<- NULL
  #   env4$classB <<- NULL
  #   nev4$grabPoint <<- NULL
  #   if(!is.null(env4$plot_data) || (is.null(env4$bp_data))
  #     restAllBrush(input,session)
  #     draw_main_plot()
  #   )
  # })

  #####################################################



  selected_df <- reactive({
    df_classA <- brushedPoints(pca_objects$pcdata, vals$df_data_A) %>%
      dplyr::select(-contains("PC")) # %>%
    # select (-c(Start.Time, System.Recipe))
    df_classA_factor <- df_classA %>%
      mutate_if(sapply(df_classA, is.character), as.factor)


    df_classB <- brushedPoints(pca_objects$pcdata, vals$df_data_B) %>%
      dplyr::select(-contains("PC")) # %>%
    # select (-c(Start.Time, System.Recipe))
    df_classB_factor <- df_classB %>%
      mutate_if(sapply(df_classB, is.character), as.factor)

    list(
      df_classA = df_classA_factor,
      df_classB = df_classB_factor
    )
  })


  output$bp_tbl_class_A <- DT::renderDataTable({
    datatable(selected_df()$df_classA,
      options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
    )
  })



  output$bp_tbl_class_B <- DT::renderDataTable({
    datatable(selected_df()$df_classB,
      options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
    )
  })

  output$tbl_all_data <- DT::renderDataTable({
    the_data <- na.omit(updated())
    datatable(the_data,
      options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
    )
  })


  ## ` ` ` Reversed engineering


  # xdata <- env4$model$x
  # TuningBallast_Flow_AI_Step2_Min TuningBallast_Flow_AI_Step2_Max

  # xdata <- env4$model
  # $x
  #  TuningBallast_Flow_AI_Step2_Min TuningBallast_Flow_AI_Step2_Max

  # $pcdata
  #   wid   PC1     PC2
  #    4  -32.54 0.500586
  #    3   32.64 0.4999

  # $md
  #   wid   MD    ln.MD
  #    4     1 -0.00000586
  #    3     1  0.00000000

  # $covx
  #      PC1        PC2
  # PC1  2129.500   0.000000
  # PC2  0.000000   0.499882
  # [1]  0.004 0.004 0.004 0.004 0.004 0.004

  # class <- rep("None", nrow(xdata))
  # [1]"None" "None"
  # [1] 0.004 0.004 0.004 0.004





  Combine_AandB_data <- reactive({
    dataset <- bind_rows(selected_df()$df_classA, selected_df()$df_classB)


    # Remove columns with zero & NA values
    dataset <- dataset[, colSums(dataset != 0, na.rm = TRUE) > 0]
    ## Go through each row and determine if a value is zero
    row_sub <- apply(dataset, 1, function(row) all(row != 0))
    ## Subset as usual
    dataset <- dataset[row_sub, ]

    # # # # ` ` `+ Adding dummy Y-data to dataframe  +` ` ` # # #
    dataset$Y_data <- sample(100, size = nrow(dataset), replace = TRUE)
    return(dataset)
  })


  output$bp_tbl_AandB <- DT::renderDataTable({
    datatable(Combine_AandB_data(),
      options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
    )
  })

  # add control for algo_btn ================================================== ----

  # algo_btn1_result <- reactiveValues()
  #
  # observeEvent(RF_btn(), {
  #   dataset <- Combine_AandB_data()
  #
  #   # Create a list of 80% of the rows in the original dataset we can use for training
  #   Training_index <- createDataPartition(dataset$Y_data, p = 0.80, list = FALSE)
  #   # select 20% of the data for validation
  #   test_dataset <- dataset[-Training_index, ]
  #
  #   # # use the remaining 80% of data to training and testing the models
  #   training_dataset <- dataset[Training_index, ]
  #
  #   # # # # ` ` `+ Random Forest +` ` ` # # #
  #   rf_classifier <- randomForest(Y_data ~ .,
  #     data = training_dataset,
  #     ntree = 500,
  #     mtry = 3,
  #     importance = TRUE,
  #     replace = FALSE
  #   )
  #
  #   predictions_rf <- predict(rf_classifier, test_dataset)
  #   algo_btn1_result$rf_classifier <- rf_classifier
  #   algo_btn1_result$predictions_rf <- predictions_rf
  #   algo_btn1_result$test_dataset <- test_dataset
  # })
  #
  # output$PR_predictions_rf <- renderPrint({
  #   print(algo_btn1_result$rf_classifier)
  # })


  # output$rf_model_plot <- renderPlot({
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


  ########################################################################################



  # model_building_data <- reactive({
  #
  #   dataset <- Combine_AandB_data()
  #
  #   # Create a list of 80% of the rows in the original dataset we can use for training
  #   Training_index <- createDataPartition(dataset$Y_data, p = 0.80, list = FALSE)
  #   # select 20% of the data for validation
  #   test_dataset <- dataset[-Training_index, ]
  #   # use the remaining 80% of data to training and testing the models
  #   training_dataset <- dataset[Training_index, ]
  #
  #   return(list(
  #     training_dataset = training_dataset,
  #     test_dataset = test_dataset
  #   ))
  #
  # })
  #
  #
  #
  # model_result <- reactive({
  #   train_dataset <- model_building_data()$training_dataset
  #   test_dataset <- model_building_data()$test_dataset
  #
  #
  #
  #
  #   # # # ` ` `+ Random Forest +` ` ` # # #
  #   rf_classifier <- randomForest(Y_data ~ .,
  #     data = train_dataset,
  #     ntree = 500,
  #     mtry = 3,
  #     importance = TRUE,
  #     replace = FALSE
  #   )
  #
  #   predictions_rf <- predict(rf_classifier, test_dataset)
  #
  #
  #
  #   # # # ` ` `+ Multivariate Adaptive Regression Splines +` ` ` # # #
  #   # ?earth
  #   # X, Y, degree, nprune
  #   # Ex: earth(x, y, degree = 2, nprune = 10)
  #
  #   # # Create a parameter tuning "grid"
  #   # parameter_grid <- floor(expand.grid(degree = 1:4, nprune = seq(5, 50, by = 5)))
  #   # #make this reproducible
  #   # set.seed(1)
  #   # #fit MARS model using k-fold cross-validation
  #
  #   fit.mars <- earth(
  #     Y_data ~ .,
  #     data = train_dataset,
  #     degree = 2
  #   )
  #
  #
  #   predictions_mars <- predict(fit.mars, test_dataset)
  #
  #   # results <- resamples(list(rf = fit.rf, mars = fit.mars))
  #   # summary_tbl <- summary(results)
  #
  #   return(list(
  #     predictions_rf = predictions_rf,
  #     rf_classifier = rf_classifier,
  #     fit.mars = fit.mars,
  #     predictions_mars = predictions_mars
  #   ))
  # })


  # output$PR_predictions_rf <- renderPrint({
  #   rf_classifier <- model_result()$rf_classifier$predicted
  #   print(rf_classifier)
  # })


  # output$rf_model_plot <- renderPlot({
  #   predictions <- model_result()$predictions_rf
  #   test_dataset <- model_building_data()$test_dataset
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


  ###############################################################################


  # zoom ranges
  test_zooming <- reactiveValues(x = NULL, y = NULL)

  observeEvent(input$test_PCAplot_dblclick, {
    brush <- input$test_plot_brush
    if (!is.null(brush)) {
      test_zooming$x <- c(brush$xmin, brush$xmax)
      test_zooming$y <- c(brush$ymin, brush$ymax)
    } else {
      test_zooming$x <- NULL
      test_zooming$y <- NULL
    }
  })


  test_biplot <- reactive({
    req(updated())
    makePlot_pca(
      pca_objects$pcdata,
      pca_objects$pca_output,
      input$the_pcs_to_plot_x,
      input$the_pcs_to_plot_y,
      input$the_grouping_variable,
      vals$df_data_A,
      vals$df_data_B,
      test_zooming,
      input$PCA_plotTypeOpt,
      input$PCA_plotTypeOpt_check_boxplotStat,
      input$PCA_plotTypeOpt_check_plotOption,
      input$PCA_plotTypeOpt_check_lineOption
    )
    
    
  })


  output$test_plot <- renderPlot({
    test_biplot()
  })


  #' 03/31 swtich to dynamic PCA plot
  #' for small mulitple
  cal_plotHeight <- reactive({
    grouping <- input$the_grouping_variable
    pcdata <- pca_objects$pcdata
    numberOfGrouping <- pcdata %>%
      dplyr::select(grouping) %>%
      group_by_(grouping) %>%
      summarise(n = n()) %>%
      nrow()
  })
  # 
  # 
  plotHeight <- reactive(150 * cal_plotHeight())
  # 
  # 
  output$plot_test.ui <- renderUI({

    if ("facet" %in% input$PCA_plotTypeOpt_check_plotOption) {
      plotOutput(ns("test_plot"),
                 click = clickOpts(id = ns("testPlot_click")),
                 dblclick = ns("test_PCAplot_dblclick"),
                 brush = brushOpts(id = ns("test_plot_brush")),
                 width = "100%",
                 height = plotHeight()
      ) %>% withSpinner(color = "#78BE20")
    } else {
      plotOutput(ns("test_plot"),
                 click = clickOpts(id = ns("testPlot_click")),
                 dblclick = ns("test_PCAplot_dblclick"),
                 brush = brushOpts(id = ns("test_plot_brush")),
                 width = "100%",
                 height = "750px"
      ) %>% withSpinner(color = "#78BE20")
    }
  })


  # MainPlotToolTip
  output$Plot_ui_mainPlotToolTip <- renderUI({
    req(input$testPlot_click)
    click <- input$testPlot_click
    point <- nearPoints(pca_objects$pcdata, click, threshold = 5, maxpoints = 1, addDist = TRUE)

    if (nrow(point) == 0) {
      return(NULL)
    }

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (click$x - click$domain$left) / (click$domain$right - click$domain$left)
    top_pct <- (click$domain$top - click$y) / (click$domain$top - click$domain$bottom)
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- click$coords_css$x
    top_px <- click$coords_css$y
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 35, "px; top:", top_px + 35, "px;"
    )
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      (HTML(str_c("<b> Tool/Chamber :", point$Tool.Chamber)))
    )
  })


  return(list(
    pca_objects = pca_objects,
    brushedpoint_data = brushedpoint_data,
    BP_test = BP_test,
    the_data = the_data,
    Combine_AandB_data = Combine_AandB_data,
    selected_df = selected_df
  ))
}
