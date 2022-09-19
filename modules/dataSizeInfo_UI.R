#' display dataSizeInfo widgets
#' dataSize_info
#' @title dataSizeInfo_UI.R
#' @return dataSizeInfo_UI
#' 
#' # In UI :
#' dataSizeInfo_UI(id = "dataSizeInfo")
#' 
#' # In Server
#' callModule(module = showDataSizeInfo, id = "dataSizeInfo", updated, getData)

dataSizeInfo_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    valueBoxOutput(ns("inputDataSize"), width = 6),
    valueBoxOutput(ns("featureDataSize"), width = 6)
  )

}

#' display dataSizeInfo widgets server-side processing
#' dataSize_info
#' @title dataSizeInfo_UI.R
#' @return dataSizeInfo_UI
#' 
#' # In UI :
#' dataSizeInfo_UI(id = "dataSizeInfo")
#' 
#' # In Server
#' callModule(module = showDataSizeInfo, id = "dataSizeInfo", updated, getData)

showDataSizeInfo <- function(input, output, session, updated, getData){
  ns <- session$ns
  
  cat("\n > Enter in dataSizeInfo_UI module \n")

  output$inputDataSize <- renderValueBox({
    #req(getData())
    if (is.null(getData())) {
      valueBox(paste0("?"),
               tags$p("Uploaded data dim", style = "font-size: 150%;"), 
               color = "teal", 
               icon = icon("comment-dots"), 
               width = "12 col-lg-6")
    } else {
      inputData <- getData()
      inputData_NofRow <- nrow(inputData)
      inputData_NofCol <-  ncol(inputData)
      inputDataSize <- paste0(inputData_NofRow, "x", inputData_NofCol, "\n") %>% 
        valueBox(tags$p("Uploaded data dim", style = "font-size: 150%;"), 
                 color = "teal", 
                 icon = icon("comment-dots"), 
                 width = "12 col-lg-6")
    }
  })
  
  output$featureDataSize <- renderValueBox({
    
    if (is.null(getData())) {
      valueBox(paste0("?"),
               tags$p("Selected feature data dim", style = "font-size: 150%;"),  
               color = "red",
               icon = icon("hourglass-half"),
               width = "12 col-lg-6")
    } else {
      inputData <- updated()
      inputData_NofRow <- nrow(inputData)
      inputData_NofCol <-  ncol(inputData)
      inputDataSize <- paste0(inputData_NofRow, "x", inputData_NofCol, "\n") %>% 
        valueBox(tags$p("Selected feature data dim", style = "font-size: 150%;"),  
                 color = "red",
                 icon = icon("hourglass-half"),
                 width = "12 col-lg-6")
    }
  })
  
  
}