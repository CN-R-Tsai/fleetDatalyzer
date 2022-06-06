#' dataSize_info
#' @title dataSizeInfo.module
#' @return UI page
#' # In UI :
#' dataSizeInfo_UI(id = "robotArmTracking")
#' # In Server
#' callModule(module = show_dataSizeInfo, id = "dataSizeInfo")
#' }

dataSize_info_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    valueBoxOutput(ns("inputDataSize"), width = 6),
    valueBoxOutput(ns("featureDataSize"), width = 6)
  )

}

#' dataSize_info
#' @title Robot_Arm_Tracking.module
#' @return UI page
#' # In UI :
#' dataSizeInfo_UI(id = "robotArmTracking")
#' # In Server
#' callModule(module = show_dataSizeInfo, id = "dataSizeInfo")
#' }


show_dataSizeInfo <- function(input, output, session, updated, getfile){
  ns <- session$ns
  
  cat("\n > Enter in dataSize_info_UI module \n")

  output$inputDataSize <- renderValueBox({
      req(getfile())
      inputData <- getfile()
      inputData_NofRow <- nrow(inputData)
      inputData_NofCol <-  ncol(inputData)
      inputDataSize <- paste0(inputData_NofRow, "x", inputData_NofCol, "\n") %>% 
        valueBox(tags$p("Input data dim", style = "font-size: 150%;"), 
                 color = "teal", 
                 icon = icon("comment-dots"), 
                 width = "12 col-lg-6")
  })
  
  output$featureDataSize <- renderValueBox({
    req(updated())
    inputData <- updated()
    inputData_NofRow <- nrow(inputData)
    inputData_NofCol <-  ncol(inputData)
    inputDataSize <- paste0(inputData_NofRow, "x", inputData_NofCol, "\n") %>% 
      valueBox(tags$p("Selected feature data dim", style = "font-size: 150%;"),  
               color = "fuchsia", 
               icon = icon("comment-dots"), 
               width = "12 col-lg-6")
  })
  
  
}