#' Robot Arm Tracking
#' @title Robot_Arm_Tracking.module
#' @return UI page
#' # In UI :
#' robot_arm_tracking_UI(id = "robotArmTracking")
#' # In Server
#' callModule(module = show_robotArm_tracking, id = "robotArmTracking")
#' }

robot_arm_tracking_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "robotArmTab",
    fluidRow(
      box(
        title = NULL, collapsible = FALSE, width = 3, status = "primary", solidHeader = TRUE,
        dateInput(
          inputId = ns("robot_arm_date"),
          label = "Date input:",
          value = Sys.Date()
        )
      )
    )
  )
}


#' Robot Arm Tracking
#' @title Robot_Arm_Tracking.module
#' @return UI page
#' # In UI :
#' robot_arm_tracking_UI(id = "mod1")
#' # In Server
#' callModule(module = show_robotArm_tracking, id = "robotArmTracking")
#' }


show_robotArm_tracking <- function(inuput, ouput, session){
  ns <- session$ns
  
  cat("\n > Enter in robot_arm_tracking_UI module \n")
  
}