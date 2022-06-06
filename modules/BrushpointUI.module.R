#' @title Brushedpoint_dataUI
#' #' # In UI :
#' Brushedpoint_dataUI(id = "mod5")
#' # In Server
#' callModule(module = show_PCA,
#'     id = "mod2",
#'     variable = reactive(dataset$data_var_x))
#' }


Brushedpoint_dataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = "Realtime Data Table", width = 12, collapsible = TRUE, status = "danger", solidHeader = FALSE,
      #h4(class = "h4", "Data Table"),
      DT::dataTableOutput(ns("brushed_point_classA"), height = "354px") %>% withSpinner(color = "#78BE20"),
      # plotOutput(ns("statistic_bar_plot")),
      # verbatimTextOutput(ns("br_tbl")),
      # DT::dataTableOutput(ns("brushed_point_tbl")) %>% withSpinner(color = "#78BE20"),
      # DT::dataTableOutput(ns("br_tbl_btnA")) %>% withSpinner(color = "#78BE20"),
      # verbatimTextOutput(ns("br_tbl_btnA")),
    ),
    fluidRow(
      column(
        12,
        box(
          title = "Analysis", width = 12, collapsible = TRUE, status = "danger", solidHeader = FALSE,
          #h4(class = "h4", "Analysis"),
          column(
            4,
            br(),
            actionButton(
              ns("analy_btn_PCA"),
              "PCA",
              width = "100%",
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            ),
            checkboxInput("analy_btn_autoRank", "Auto Rank", value = TRUE),
            actionButton(
              ns("analy_btn_Corr"),
              "Correlation",
              width = "100%",
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            )
          ),
          column(
            4,
            br(),
            actionButton(
              ns("analy_btn_classA"),
              "Add to Class A",
              width = "100%",
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            ),
            actionButton(
              ns("analy_btn_classB"),
              "Add to Class B",
              width = "100%",
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            ),
            actionButton(
              ns("analy_btn_resetClass"),
              "Reset Class",
              width = "100%",
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            ),
            actionButton(
              ns("analy_btn_removeData"),
              "Remove Data",
              width = "100%",
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            )
          ),
          column(
            4,
            br(),
            actionButton(
              ns("analy_btn_RF"),
              "Analyze 1",
              width = "100%",
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            ),
            actionButton(
              ns("analy_btn_XGB"),
              "Analyze 2",
              width = "100%",
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            ),
            actionButton(
              ns("analy_btn_Ranger"),
              "Analyze 3",
              width = "100%",
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            ),
          ),
          column(
            8,
            br(),
            actionButton(
              ns("analy_btn_Preprocessingresult"),
              "Preprocessing results",
              width = "100%",
              #icon = icon("play-circle"),
              #class = "btn btn-danger action-button",
              style = "font-size: 0.9em;"
            ),
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        box(
          title = "Virtual Metrology", width = 12, collapsible = TRUE, status = "danger", solidHeader = FALSE,
          #h4(class = "h4", "Regression"),
          radioButtons(
            inputId = ns("radio_regressionType"),
            label = "Model Type",
            choices = list(
              "Reg 1" = "MARS",
              "Reg 2" = "GBMReg",
              "Reg 3" = "mRFReg",
              "Reg 4" = "LARS",
              "Reg 5" = "EML"
            ),
            selected = "MARS",
            inline = T,
            width = "100%"
          ),
          fluidRow(
            column(
              width = 12,
              HTML("<b><font size='2em' color='black'> Threshold for Preprocessing </font></b>"),
              br(),
              sliderInput(inputId = "slider_regressionThres", label = NULL, min = 0, max = 1, value = 0.6, step = 0.01, width = "100%"),
              HTML("<b><font size='2em' color='black'> Training data Percentage </font></b>"),
              br(),
              sliderInput(inputId = "slider_regressionTrainRate", label = NULL, min = 1, max = 100, value = 95, width = "100%"),
            ),
          ),
          column(
            width = 4,
            actionButton(
              ns("analy_btn_regress"),
              HTML("Regression Analysis"),
              #icon = icon("play-circle"),
              class = "btn btn-danger action-button",
              style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
            )     
          )
        ),
        box(
          title = "Set Virtual Feature", width = 12, collapsible = TRUE, status = "danger", solidHeader = FALSE,
          #h4(class = "h4", "Set Virtual Feature"),
          # materialSwitch(inputId = "analySide_check_virtualParam",
          #                label = "Set Virtual Feature",
          #                status = "info",
          #                inline = T),
          #conditionalPanel('input.analySide_check_virtualParam',
                           HTML("<b>Select Feature &nbsp;&nbsp;</b>"),
                           actionLink(inputId = "analyVirtualM_btn_refresh_new", label = NULL),
                           selectInput("analyVirtualM_included_sensors", label = NULL, choices = "Not Loaded!",width = "100%",multiple = F),
                           actionButton(
                             ns("analyVirtualM_user_sensors"),
                             "Set in Equation",
                             #width = "100%",
                             #icon = icon("play-circle"),
                             class = "btn btn-danger action-button",
                             style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
                           ),
                           br(),
                           HTML("<b>Mathematical Expression &nbsp;&nbsp;</b>"),
                           actionLink(inputId = "analyVirtualM_btn_info_new", label = NULL,
                                      tags$i(
                                        class = "glyphicon glyphicon-info-sign", 
                                        #class = "fas fa-info-circle",
                                        style = "color:#0072B2;",
                                        title = "Note: Use operators for Addition: +, Subtraction: -, Multiplication: *, Division: /, Absolute: abs(feature_name), Delta function: delta(\"feature_name\",USE_PERCENT=F,n_avg=1,filter_range=c(0,0,0))"
                                      )),
                           textAreaInput("analyVirtualM_mathExp", label = NULL ,width="100%", rows = 5),
                           actionButton(
                             ns("analyVirtualM_mathExp_lock"),
                             "Get the equation",
                             class = "btn btn-danger action-button",
                             style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
                           ),
                           actionButton(
                             ns("analyVirtualM_clear"),
                             "Reset",
                             class = "btn btn-danger action-button",
                             style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
                           ),
                           #END
                           radioButtons(inputId = "analyVirtualM_radio_option",
                                        label = "Option",
                                        choices = list("Skip NA" = 1,
                                                       "Use all value" = 2),
                                        inline = T),
                           textInput(inputId = "analyVirtualM_txtIn_varName",
                                     label = NULL,
                                     placeholder = "Type Name for New Feature",
                                     width = "100%"),
                           fluidRow(column(width = 6,
                                           textOutput(outputId = "analyVirtualM_txtOut_varFullName")),
                                    column(width = 12,
                                           actionButton(
                                             ns("analyVirtualM_btn_addVirtualParam"),
                                             "Add Feature",
                                             class = "btn btn-danger action-button",
                                             style = "color: #00a9e0; background-color: #FFFFFF; border-color: #00a9e0; font-size: 0.9em;"
                                           ))
            #)
          )
        )
      )
    )
  )
}


#' @title Brushedpoint_dataUI
#' #' # In UI :
#' Brushedpoint_dataUI(id = "mod5")
#' # In Server
#' data_module1 <- callModule(module = show_PCA, id = "mod4", variable = variable, variable_name = variable_name)
#' }


show_Brushedpoint <- function(input, output, session, PCA) {
  ns <- session$ns


  output$brushed_point_classA <- DT::renderDataTable({
    df <- PCA$brushedpoint_data()
    datatable(df,
      options = list(searching = FALSE, pageLength = 15, lengthMenu = c(5, 10, 15, 20), scrollX = T, scrollY = "300px")
    )
  })



  output$statistic_bar_plot <- renderPlot({
    df <- PCA$brushedpoint_data()
    df <- data.frame(
      type = names(df[, ]),
      count = colSums(!is.na(df))
    )


    ggplot(df, aes(fill = type, y = count, x = type)) +
      geom_bar(stat = "identity") +
      labs(x = "Feature", y = "Count") +
      coord_flip() +
      theme(legend.position = "buttom", legend.box = "horizontal")
    #   theme_minimal()
  })

  output$br_tbl <- renderPrint({
    # df <- PCA$brushedpoint_data()
    df <- data.frame(t(colSums(!is.na(PCA$brushedpoint_data()))))
    # df <- data.frame(count = colSums(!is.na(PCA$brushedpoint_data())))
    print(str(df))
  })

  output$brushed_point_tbl <- DT::renderDataTable({

    # df <- PCA$brushedpoint_data()

    # df <- data.frame(colSums(!is.na(PCA$brushedpoint_data())))
    # df <- data.frame(t(colSums(!is.na(df))))

    df <- PCA$brushedpoint_data() %>% add_count(Tool.Chamber)

    # df <- data.frame(type = names(df[, ]),
    # count = colSums(!is.na(df)))
    # df <- data.frame(t(colSums(df))))
    # group_by(Tool.Chamber) %>%
    # tally()
    # df <- PCA$brushedpoint_data()
    # df <- data.frame(t(colSums(!is.na(PCA$brushedpoint_data()))))
    datatable(df,
      options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
    )
  })


  # output$br_tbl_btnA  <- DT::renderDataTable({
  #
  #   datatable(df_classA(),
  #             options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
  #   )
  # })
  #
  # # df_classA <- reactive({Warning: Error in UseMethod: no applicable method for 'select' applied to an object of class "NULL"v
  # #   df_classA <- PCA$BP()
  # # })
  # #
  #
  # df_classA <- reactiveVal(NULL)
  #


  # df_classA <- eventReactive(
  #   input$analy_btn_classA,
  #   {
  #     req(PCA$BP())
  #     df_classA <- PCA$BP()
  #   }
  # )




  return(
    list(
      analy_btn_classA = reactive(input$analy_btn_classA),
      analy_btn_classB = reactive(input$analy_btn_classB),
      analy_btn_resetclass = reactive(input$analy_btn_resetClass),
      analy_btn_RF = reactive(input$analy_btn_RF),
      analy_btn_Ranger = reactive(input$analy_btn_Ranger),
      analy_btn_XGboost = reactive(input$analy_btn_XGB),
      analy_btn_removeData = reactive(input$analy_btn_removeData),
      analy_btn_PCA = reactive(input$analy_btn_PCA),
      analy_btn_Corr = reactive(input$analy_btn_Corr),
      analy_btn_RF_regress = reactive(input$analy_btn_regress),
      analy_btn__radioRegre = reactive(input$radio_regressionType)
    )
  )



  #
  #
  # df_classB <- eventReactive(
  #   input$analy_btn_classB,
  #   {
  #     req(PCA$BP())
  #     # req(PCA$BP())
  #     df_classB <- PCA$BP()
  #   }
  # )



  # observeEvent(input$analy_btn_resetClass, {
  #   env4$ClassA <<- NULL
  #   env4$ClassB <<- NULL
  #   if(!is.null(en4$plot_data) || !is.null(en4$bp_data)){
  #     resetALLBrush(input,session)
  #
  #   }
  #
  # })
}
