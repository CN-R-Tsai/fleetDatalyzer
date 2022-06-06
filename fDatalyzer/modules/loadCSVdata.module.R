#' Variable selection UI
#' @title loadCSVdata.module
#' @return UI page
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' # In Server
#' data_mod1 <- callModule(module = load_data, id = "mod1")
#' }


load_CSVdataUI <- function(id) {
  
  ns <- NS(id)

  fluidRow(
    column(
      12, 
      fileInput(ns("file1"), "Select a CSV file to upload:",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values",
                      "text/tab-separated-values",
                      "text/plain",
                      ".csv",
                      ".tsv"
                    )
      ),
      tagList(
        p(style = "color:red;", "**Please select the options that match your CSV format, then upload your file:**"),
      ),
      # radioButtons(
      #   inputId = ns("header"),
      #   label = "Header",
      #   choices = c(
      #     "Columns have headers" = "Yes",
      #     "Columns do not have headers" = "No"
      #   ),
      #   selected = "Yes",
      #   inline = T
      # ),
      # radioButtons(
      #   ns("sep"), "Separator",
      #   c(
      #     Comma = ",",
      #     Semicolon = ";",
      #     Tab = "\t"
      #   ),
      #   ",",
      #   inline = T
      # ),
      radioButtons(ns('quote'), 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"',
                   inline = T
      )
    )
  )
}



#' Variable selection module server-side processing
#' @title show_data
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = load_data, id = "mod1")


show_CSV <- function(input, output, session) {

  ns <- session$ns
  
  #read in the CSV
  the_data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
  
    the_data <- read.csv(inFile$datapath, quote = input$quote, stringsAsFactors = FALSE)
    
    # different csv format selection
    # the_data <- read.csv(inFile$datapath, header = (input$header == "Yes"),
    #                      sep = input$sep, quote = input$quote, stringsAsFactors=FALSE)
    
    return(the_data)
  })
  

  
  
  
}




