#' About_UI
#' @title Analytics_UI.module
#' @return UI page
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' # In Server
#' data_mod1 <- callModule(module = load_data, id = "mod1")
#' }

About_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
  # About - About Me - start ------------------------------------------------
  box(
    title = "About me",
    status = "danger",
    width = "6 col-lg-4",
    tags$p(
      class = "text-center",
      tags$img(class = "img-responsive img-rounded center-block", src = "images/Roger.jpg", style = "max-width: 350px;")
    ),
    tags$p(
      class = "text-center",
      emo::ji("hand"),
      tags$strong("Hello! I'm Roger.")
    ),
    tags$p(
      "I'm a Sr. data scientist from Miyagi, Japan, where I focus on",
      "data visualization, interactive reporting,",
      "and machine learning. I enjoy building data tools,",
      "like this dashboard."
      
    ),
    tags$p(
      "I use data science to support intelligent system project with",
      tags$a(href = "https://www.tel.co.jp/", "Tokyo Electron", target = "_blank"),
      "at", tags$a("Miyagi Technology Innovation Center", target = "_blank"),
      "in beautiful Sendai, Miyagi.",
      "I develop interative and user-friendly web applications using R/Shiny to visualize metrics of interest",
      "allowing stakeholders to draw actionable insights like this one.",
    ),
    tags$p(
      "Get in touch with me by email at",
      HTML(paste0(tags$a(href = "mailto:chung_nan.tsai@tel.com", "chung_nan.tsai@tel.com"), "."))
    )
  ),
  # About - About Me - end --------------------------------------------------
  # About - About Dashboard - start -----------------------------------------
  box(
    title = "About this Dashboard",
    # status = "primary",
    width = "6 col-lg-4",
    tags$p(
      class = "text-center",
      tags$a(
        href = "https://www.r-project.org",
        target = "_blank",
        tags$img(class = "image-responsive",
                 src = "https://www.r-project.org/logo/Rlogo.svg",
                 style = "max-width: 150px;"
        )
      )
    ),
    tags$p(
      "This dashboard was built in",
      tags$a("R"),"with",
      tags$strong("shiny,"),
      tags$strong("shinydashboard,"),
      tags$strong("ggplot2,"),
      "the", tags$strong("tidyverse,"),
      "and many more packages."
    )
  )
  )
  
  
}


#' About_UI
#' @title Analytics_UI.module
#' @return UI page
#' # In UI :
#' load_CSVdataUI(id = "mod1")
#' # In Server
#' data_mod1 <- callModule(module = load_data, id = "mod1")
#' }

show_About <- function(input, output, session) {
  ns <- session$ns
  
  cat("\n > Enter in About_UI module \n")
  
}