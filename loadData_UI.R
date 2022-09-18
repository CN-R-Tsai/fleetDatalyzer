#' load data user interface
#' @title loadData_UI.R
#' @return loadData_UI page
#' 
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' 
#' # In Server
#' data_mod1 <- callModule(module = load_data, id = "mod1")


loadData_UI <- function(id) {
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



#' load data module server-side processing
#' @title loadData_UI.R
#' @return loadData_UI page
#' 
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = load_data, id = "mod1")


showData <- function(input, output, session) {
  ns <- session$ns
  
  cat("\n > Enter in loadData_UI module \n")
  
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




