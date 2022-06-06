#' fleet_insight_UI
#' @title fleet_insight_UI.module
#' @return UI page
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' # In Server
#' data_mod1 <- callModule(module = load_data, id = "mod1")
#' }

fleet_insight_UI <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "fleetinsightTab",

    #   ----------------------------------------------------------------
    # ` ` `+ Select Process Data ----
    #   ----------------------------------------------------------------
    fluidRow(
      box(
        title = NULL, collapsible = FALSE, width = 3, status = "primary", solidHeader = TRUE,
        dateInput(
          inputId = ns("fleet_insight_date"),
          label = "Date input:",
          value = Sys.Date()
        )
      ),
      box(
        title = NULL, collapsible = FALSE, width = 3, status = "primary", solidHeader = TRUE,
        numericInput(
          inputId = ns("fleet_insight_number"),
          label = "Show top N chambers:",
          value = 2
        )
      ),
      # box(
      #   title = "Load CSV data", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
      #   load_CSVdataUI(id = "mod0.5")
      # ),
      # box(
      #   title = "Feature Selection", collapsible = TRUE, width = 12, status = "danger", solidHeader = FALSE,
      #   load_dataUI(id = "mod1")
      # ),
      # h2(id = "fleet-insight-h2", "The dashboard I built to explore this data, fleetinsight, let us examine the behavior of the top productive chamber for any given day, at varying levels of detail."),
      br(),
    ),
    fluidRow(
      tabBox(
        title = NULL, id = ns("fleet_insight"), width = 12,
        tabPanel(
          "Fleet Traffic Overview",
          fluidRow(
            valueBoxOutput(ns("total_file"), width = 3),
            valueBoxOutput(ns("total_PM"), width = 3),
          ),
          # DT::dataTableOutput(ns("fleetoverview_data")) %>% withSpinner(color = "#78BE20"),
          br(),
          fluidRow(column(
            width = 6,
            imageOutput(ns("gg_animate_plot"), height = "650px" , width = "100%") %>% withSpinner(color = "#78BE20"),
          ),
          column(
            width = 6,
            plotOutput(ns("fleetoverview_plot"), height = "650px") %>% withSpinner(color = "#78BE20")
          ))
        ),
        tabPanel(
          "Chamber Ranking",
          fluidRow(
            tags$style(".small-box.bg-yellow { background-color: #538cc6 !important; color: #FFFFFF !important; font-family: Roboto;}"),
            valueBoxOutput(ns("Golden_Chamber"), width = 3),
            tags$style(".small-box.bg-red { background-color: #E75480 !important; color: #FFFFFF !important; font-family: Roboto;}"),
            valueBoxOutput(ns("Unproductivity_chamber"), width = 3)
          ),
          plotOutput(ns("Golden_Chamber_plot"), height = "650px") %>% withSpinner(color = "#78BE20"),
        ),
        tabPanel(
          "Traffic by Hours",
          #plotOutput(ns("test_plot"), height = "650px") %>% withSpinner(color = "#78BE20"),
        ),
        tabPanel(
          "Detail View",
          uiOutput(ns("tool")),
          plotOutput(ns("Detail_view_plot"), height = "650px") %>% withSpinner(color = "#78BE20"),
          materialSwitch(
            inputId = ns("detail_view_plot_waferMapOption"),
            label = "Show Wafer Map Option",
            status = "info",
            inline = T
          ),
          conditionalPanel(
            "input.detail_view_plot_waferMapOption",
            fluidRow(column(
              width = 4,
              radioButtons(
                inputId = "radio_colorWaferMap",
                label = "Color Option",
                choices = list(
                  "Chamber" = "TOOL_PM",
                  "Recipe" = "recipe"
                )
              )
            ))
          )
        )
      )
    )
  )
}



#' fleet_insight_UI
#' @title fleet_insight_UI.module
#' @return fleet_insight_UI
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' # In Server
#' data_mod1 <- callModule(module = load_data, id = "mod1")
#' }

show_fleet_insight <- function(input, output, session, updated, getfile) {
  ns <- session$ns

  cat("\n > Enter in fleet_insight_UI module \n")





  # output$fleetoverview_data <- DT::renderDataTable({
  #   data <- getfile()
  #   datatable(data,
  #     options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20), scrollX = T)
  #   )
  # })


  # Fleet Traffic Overview ================================================== ----
  output$fleetoverview_plot <- renderPlot({
    req(getfile())
    file_counts_eachPM <- getfile() %>%
      group_by(Tool.Chamber) %>%
      summarise(n = n())

    p <- ggplot(file_counts_eachPM, aes(x = Tool.Chamber, y = n)) +
      geom_bar(position = "stack", stat = "identity", alpha = .6, width = .4) +
      theme(
        plot.title = element_text(size = 28),
        axis.text = element_text(angle = 90, hjust = 1, size = 12),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
      ) +
      aes(fill = Tool.Chamber) +
      scale_fill_viridis_d(
        option = "plasma",
        direction = -1
      ) +
      xlab("Tool_PM") +
      ylab("Total # of files") +
      labs(title = "Fleet Traffic Overview")
    p
  })


  output$total_file <- renderValueBox({
    req(getfile())
    total_fileCounts <- getfile() %>%
      nrow() %>%
      format(big.mark = ",") %>%
      valueBox(tags$p("Total accumulation files", style = "font-size: 125%;"), icon("fas fa-file-alt"), color = "aqua")
  })


  output$total_PM <- renderValueBox({
    req(getfile())
    file_counts_eachPM <- getfile() %>%
      group_by(Tool.Chamber) %>%
      summarise(n = n())
    number_of_PM <- nrow(file_counts_eachPM)
    Show_PM_count <- number_of_PM %>%
      format(big.mark = ",") %>%
      valueBox(tags$p("Total connected PMs", style = "font-size: 125%;"), color = "aqua", icon("fas fa-industry"))
  })


  # Chamber Ranking ========================================================= ----

  dateInput <- reactive({
    input$fleet_insight_date
    print(input$fleet_insight_date)
  })

  NumberInput <- reactive({
    input$fleet_insight_number
    print(input$fleet_insight_number)
  })

  output$Golden_Chamber <- renderValueBox({
    Traffic_new(dateInput(), NumberInput())$Golden_Chamber %>%
      valueBox(tags$p("Most productive chamber", style = "font-size: 120%;"), color = "aqua", icon("fas fa-trophy"))
  })

  output$Unproductivity_chamber <- renderValueBox({
    Traffic_new(dateInput(), NumberInput())$Bad_Chamber %>%
      valueBox(tags$p("Low productivity chamber", style = "font-size: 120%;"), color = "red", icon("fas fa-exclamation-triangle"))
  })


  Traffic_new <- function(date, numb) {
    req(getfile())
    df <- getfile() %>%
      filter(Start.Time == dateInput()) %>%
      group_by(Tool.Chamber) %>%
      # Group by same tool_pm
      tally() %>%
      arrange(-n)

    Most_productivity_chamber <- head(df, 1) %>%
      top_n(1, n) %>%
      dplyr::select(Tool.Chamber)

    Unproductivity_chamber <- tail(df, 1) %>%
      top_n(-1, n) %>%
      dplyr::select(Tool.Chamber)

    Group_by_time <- getfile() %>%
      filter(Start.Time == dateInput()) %>%
      group_by(Start.Time, Tool.Chamber) %>%
      summarise(count = n())


    Group_by_time$Start.Time <- as.POSIXct(Group_by_time$Start.Time, format = "%Y/%m/%d %H:%M", tz = "GMT")


    P <- ggplot(Group_by_time, aes(x = Start.Time, y = count))
    Plot <- P + geom_bar(position = "stack", stat = "identity", alpha = .6, width = 1, color = "black") +
      scale_x_datetime(
        date_breaks = "3 hours",
        labels = date_format(
          format = "%Y/%m/%d %H:%M:%S",
          tz = "Asia/Tokyo"
        )
      ) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(hjust = 1, size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      ) +
      xlab("Date") +
      ylab(paste0("# of files on ", dateInput(), " ")) +
      facet_wrap(~Tool.Chamber) #+ geom_text(aes(label=count),size=12)


    newlist <- list(
      "Golden_Chamber" = Most_productivity_chamber,
      "Bad_Chamber" = Unproductivity_chamber,
      "Plot" = Plot
    )

    return(newlist)
  }


  observe({
    output$Golden_Chamber_plot <- renderPlot({
      Traffic_new(dateInput(), NumberInput())$Plot
    })
  })


  # For presentation gganimate ============================================== ----


  dateInput <- reactive({
    input$fleet_insight_date
    print(input$fleet_insight_date)
  })

  NumberInput <- reactive({
    input$fleet_insight_number
    print(input$fleet_insight_number)
  })


  Traffic_gganimate <- function(date, numb) {
    df <- getfile()
    df$Start.Time <- as.POSIXct(df$Start.Time, format = "%Y/%m/%d", tz = "GMT")

    df <- df %>%
      # calculate the occurances for each day and group
      group_by(Start.Time, Tool.Chamber) %>%
      dplyr::summarise(COUNT = n()) %>%
      ungroup()
    # Compute the rank by count for each day
    df_rank <- df %>%
      group_by(Start.Time) %>%
      mutate(rank = rank(-COUNT, ties.method = "min"))
    print(df_rank)

    # ranked by day

    myPlot <- df_rank %>% ggplot() +
      aes(
        xmin = 0,
        xmax = COUNT
      ) +
      aes(
        ymin = rank - .45,
        ymax = rank + .45,
        y = rank
      ) +
      facet_wrap(~ as.Date(Start.Time)) +
      geom_rect(alpha = .7) +
      aes(fill = Tool.Chamber) +
      scale_fill_viridis_d(
        option = "plasma",
        direction = -1
      ) +
      scale_x_continuous(
        limits = c(-50, 150),
        breaks = c(0, 25, 50, 75, 100)
      ) +
      geom_text(
        col = "gray13",
        hjust = "right",
        aes(label = Tool.Chamber),
        x = 10
      ) +
      geom_text(
        x = 85, y = -6,
        aes(label = as.character(Start.Time)),
        size = 10, col = "grey18"
      ) +
      #scale_y_reverse() +
      xlab("Number of file") +
      ylab("Ranking") +
      labs(title = "Fleet Traffic Overview")
    ### Now set up the animation

    Plot <- myPlot + facet_null() +
      gganimate::transition_time(Start.Time) +
      ease_aes("cubic-in-out")

    animate(Plot, fps = 0.5, duration = 100, width = 800, height = 600)

    newlist <- list("Plot" = Plot)

    return(newlist)
  }


  observe({
    output$test_plot <- renderPlot({
      Traffic_gganimate(dateInput(), NumberInput())$Plot
    })
  })

  
  
  output$gg_animate_plot <- renderImage({
    df <- getfile()
    df$Start.Time <- as.POSIXct(df$Start.Time, format = "%Y/%m/%d", tz = "GMT")
    
    outfile <- tempfile(fileext='.gif')
    df <- df %>%
      # calculate the occurances for each day and group
      group_by(Start.Time, Tool.Chamber) %>%
      dplyr::summarise(COUNT = n()) %>%
      ungroup()
    # Compute the rank by count for each day
    df_rank <- df %>%
      group_by(Start.Time) %>%
      mutate(rank = rank(-COUNT, ties.method = "min"))
    print(df_rank)
    
    # ranked by day
    
    myPlot <- df_rank %>% ggplot() +
      aes(
        xmin = 0,
        xmax = COUNT
      ) +
      aes(
        ymin = rank - .45,
        ymax = rank + .45,
        y = rank
      ) +
      facet_wrap(~ as.Date(Start.Time)) +
      geom_rect(alpha = .7) +
      aes(fill = Tool.Chamber) +
      scale_fill_viridis_d(
        option = "plasma",
        direction = -1
      ) +
      scale_x_continuous(
        limits = c(-50, 150),
        breaks = c(0, 25, 50, 75, 100)
      ) +
      geom_text(
        col = "gray13",
        hjust = "right",
        aes(label = Tool.Chamber),
        x = 10
      ) +
      geom_text(
        x = 85, y = -6,
        aes(label = as.character(Start.Time)),
        size = 10, col = "grey18"
      ) +
      scale_y_reverse() +
      xlab("Number of file") +
      ylab("Ranking") +
      labs(title = "Fleet Traffic Overview")
    ### Now set up the animation

  
    Plot <- myPlot + facet_null() +
      gganimate::transition_time(Start.Time) +
      ease_aes("cubic-in-out")
    
    animate(Plot, width = 800, height = 650,  renderer = gifski_renderer("outfile.gif"))
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  
  # Detail View ============================================================= ----

  ## create selectInput based on the result of Detailview
  output$tool <- renderUI({
    selectInput(
      inputId = ns("tool_pm"),
      label = "Select Tool_PM from the list below:",
      choices = Detailview(dateInput())$A1
    )
  })

  output$Detail_view_plot <- renderPlot({
    req(input$tool_pm)
    TP <- input$tool_pm
    p <- Detailview(dateInput())$B1 %>% filter(Tool.Chamber == TP)

    p %>% ggplot(aes(x = Start.Time, y = System.Recipe)) +
      geom_point(aes(colour = factor(System.Recipe)), shape = 1, size = 6) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(hjust = 1, size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      ) +
      scale_color_discrete(name = "System Recipe") +
      xlab("process_datetime") +
      ylab("System Recipe")
  })

  #----------------------------------------------------
  # Use: Detail_View
  # Arguments:
  # Return:
  #----------------------------------------------------

  Detailview <- function(date) {
    req(getfile())
    opt <- getfile()
    a1 <- count(opt, Tool.Chamber) %>% dplyr::select(Tool.Chamber)
    b1 <- opt %>% filter(Start.Time == dateInput())
    testlist <- list("A1" = a1, "B1" = b1)
    return(testlist)
  }
}
