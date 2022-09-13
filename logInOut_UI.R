#' logInOut_UI
#' @title logInOut_UI.module
#' @return logInOut UI
#'
#' # In UI :
#' logIn_UI(id = "logIn_module"),
#'
#' # In Server
#' validate_password_module <- callModule(module   = validate_pwd,
#' id = "logIn_module", data = user_base_module_tbl, user_col = username,
#' pwd_col  = password)


logIn_UI <- function(id, title) {
  ns <- NS(id) # namespaced id

  span(
    id = ns("logIn"),
    style = "width: 500px; max-width: 100%; margin: 0 auto;",
    box(
      width = NULL, height = "1024px", status = "danger", align = "center",
      div(style = "height:150px"),
      fluidRow(column(
        width = 4, offset = 4,
        img(
          src = "images/TEL_tm_rgb_Large.png",
          width = "100%"
        )
      )),
      div(style = "height:30px"),
      fluidRow(column(
        width = 4, offset = 4,
        textInput(
          inputId = ns("logInusername"),
          label = tagList(icon("user"), "User Name"),
          placeholder = "Username",
          width = "100%"
        )
      )),
      fluidRow(column(
        width = 4, offset = 4,
        passwordInput(
          inputId = ns("logInpassword"),
          label = tagList(icon("unlock-alt"), "Password"),
          placeholder = "Password",
          width = "100%"
        )
      )),
      fluidRow(column(
        width = 4, offset = 4,
        actionButton(
          inputId = ns("logInsignin"),
          label = "Log in",
          width = "100%"
        )
      )),
      em("f"), HTML("-Datalyzer (v2.0.0)"),
      br(),
      p(em("Developed by Chung-nan(Roger) Tsai"), style = "text-align:center; font-family: times"),
      p()
    )
  )
}


#' logInOut_UI
#' @title logInOut_UI.module
#' @return logInOut UI
#'
#' # In UI :
#' logIn_UI(id = "logIn_module"),
#'
#' # In Server
#' validate_password_module <- callModule(module   = validate_pwd,
#' id = "logIn_module", data = user_base_module_tbl, user_col = username,
#' pwd_col = password)


cat("\n > Enter in logInOut_UI module \n")

validate_pwd <- function(input, output, session, data, user_col, pwd_col) {

  # get user and pwd from data/ user_col/ pwd_col information

  user <- data %>% pull({{ user_col }})
  pwd <- data %>% pull({{ pwd_col }})

  # ` > check correctness ----
  eventReactive(input$logInsignin, {
    if (is.null(input$logInusername) || input$logInusername == "" || is.null(input$logInpassword) || input$logInpassword == "") {
      showModal(modalDialog(
        title = "Important message",
        "Username or Password cannot be blank",
        easyClose = TRUE
      ))
      # showNotification("Username or Password cannot be blank")
    } else if (input$logInusername != user || input$logInpassword != pwd) {
      showModal(modalDialog(
        title = "Important message",
        "Your username or password is incorrect - please try again.",
        easyClose = TRUE
      ))
      # showNotification("Your username or password is incorrect - please try again.")
    } else {
      if (input$logInusername == user &&
        input$logInpassword == pwd) {
        validate <- TRUE
        
      }
      # hide login form when user is confirmed
      if (validate) {
        shinyjs::hide(id = "logIn")
      }
      validate
    }
  })
}
