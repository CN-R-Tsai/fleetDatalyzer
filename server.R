

cat("\n > Enter in server. R \n")


server <- function(input, output, session) {


  # print("Initialize Start")
  ## ** For solving Zombi Process

  # code <- 'int wstat; while (waitpid(-1, &wstat, WNOHANG > 0) {};'
  # wait <- cfunction(body = code, includes = includes, convention = '.C')

  # setup_font(
  #   id = "roboto",
  #   output_dir = "www",
  #   variants = "regular"
  # )


   user_base_module_tbl <- data.frame(
     get.userInfo(username, password)
   )


  # # # check credentials
   validate_password_module <- callModule(
     module   = validate_pwd,
     id       = "module_login",
     data     = user_base_module_tbl,
     user_col = username,
     pwd_col  = password
  )



  output$sidebarmenu <- renderMenu({
    req(validate_password_module())
    updateTabItems(session, "sidebarmenu","analysisTab")
    sidebarMenu(
      id = "sidebarmenu",
      tags$style(HTML(".sidebar-menu li a { font-size: 16px; }")),
      menuItem("Analytics", tabName = "analysisTab", icon = icon("far fa-chart-bar"), selected = T),
      menuItem("Fleet Insight", tabName = "fleetinsightTab", icon = icon("fas fa-database"), selected = T),
      #menuItem("Robot Arm Tracking", tabName = "robotArmTab", icon = icon("fa-solid fa-route"), selected = T),
      #menuItem("Quick Trace", tabName = "quicktraceTab", icon = icon("fas fa-rocket"), selected = T),
      #menuItem("User File Upload", tabName = "userUpTab", icon = icon("file-excel-o")),
      menuItem("About", tabName = "aboutTab", icon = icon("info"))
    )
  })

  
  observeEvent(input$sidebarmenu,
    {
      if (input$sidebarmenu == "fleetinsightTab") {
        
        # callModule(
        #   module = show_Analytics, id = "analytics_tab1"
        # )
        # callModule(
        #   module = show_fleet_insight, id = "fleet_insight_UI_1", updated
        # )
      }
      
      # if (input$sidebarmenu == "analysisTab") {
      #   callModule(module = show_Analytics, id = "analytics_tab1")
      # }
      
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )



  # req(validate_password_module())




  ############################################# +
  ## Module analytics : Load analyticsUI     ###
  ##     id call = "analytics_1"          ###+
  ############################################# +
  {
    callModule(module = show_Analytics, id = "analytics_1")
  }


  # 
  #ReactiveValue that "belongs" to Application and updated through all modules
  rv <- reactiveValues(variable = NULL, selected_df = NULL)


  ################################## +
  ## Module 0 : Load test Data  #####
  ##     id call = "mod0"       ####+
  ################################## +
  {
    # Call module load_testdata
    data_mod1 <- callModule(module = load_testdata, id = "mod0")

    observeEvent(data_mod1$trigger, {
      req(data_mod1$trigger > 0)
      rv$variable <- data_mod1$variable
      rv$selected_df <- data_mod1$selected_df
    })
    
  }



  ############################################# +
  ## Module 0.5&X : Load & show CSV table   ####
  ##     id call = "mod0.5"                 ###+
  ############################################# +

  {
    # Call module load_CSVdataUI
    getfile <- callModule(module = show_CSV, id = "mod0.5")
    callModule(module = dataInspec_table, id = "modX", getfile, updated)
  }

  ################################### +
  ## Module X : Load Sensor Data  ####
  ##     id call = "mod1"         ###+
  ################################### +
  {
    updated <- callModule(module = load_data, id = "mod1", getfile)
  }

  
  ################################### +
  ## Module X : Load Y Data  ####
  ##     id call = "modY"         ###+
  ################################### +
  {
    upload_Y <- callModule(module = show_Y, id = "modY")
  }
  
  

  ################################# +
  ## Module 2 : Show PCA data   ####
  ##     id call = "mod2"       ###+
  ################################# +
  {
    # Call module show_data
    PCA <- callModule(
      module = show_PCA, 
      id = "mod2", 
      getfile, 
      updated,
      Add_classA_btn = classA$analy_btn_classA,
      Add_classB_btn = classA$analy_btn_classB,
      Reset_btn = classA$analy_btn_resetclass,
      RF_btn = classA$analy_btn_RF,
      Ranger_btn = classA$analy_btn_Ranger,
      XGboost_btn = classA$analy_btn_XGboost,
      Remove_data_btn = classA$analy_btn_removeData,
      PCA_btn = classA$analy_btn_PCA,
      variable = reactive(rv$variable),
      variable_name = reactive(data_mod1$variable_name),
      selected_df = reactive(rv$selected_df)
    )
  }


  ############################ +
  ## Module 2 : Show data  ####
  ##     id call = "mod3"  ###+
  ############################ +
  {
    # Call module show_data
    callModule(
      module = show_data, id = "mod3", getfile, updated,
      selected_df = reactive(rv$selected_df),
      variable = reactive(rv$variable),
      variable_name = reactive(data_mod1$variable_name),
      useggplot = TRUE
    )
  }


  ################################# +
  ## Module 3 : Show datatable  ####
  ##     id call = "mod3"       ###+
  ################################# +
  {
    # Call module show_data
    callModule(
      module = show_table, id = "mod3",
      selected_df = reactive(rv$selected_df),
    )
  }


  ################################# +
  ## Module 4 : Show cor plot   ####
  ##     id call = "mod4"       ###+
  ################################# +
  {
    # Call module show_data
    callModule(
      module = show_corr, id = "mod4",
      variable = reactive(rv$variable),
      variable_name = reactive(data_mod1$variable_name),
      selected_df = reactive(rv$selected_df), 
      getfile, 
      updated, 
      PCA,
      Corr_btn = classA$analy_btn_Corr
    )
  }


  ################################### +
  ## Module 4 : Show Scree plot   ####
  ##     id call = "mod5"         ###+
  ################################### +
  {
    # Call module show_data
    callModule(module = show_Scree, id = "mod5", updated, PCA)
  }


  ###################################### +
  ## Module  : Show brushed point   #####
  ##     id call = "mod6"           ####+
  ###################################### +
  {
    # Call module show_data
    classA <- callModule(module = show_Brushedpoint, id = "mod6", PCA)
  }


  ############################################# +
  ## Module analytics : About_UI            ### +
  ##     id call = "About_UI_1"             ### +
  ############################################# +
  {
    callModule(module = show_About, id = "About_UI_1")
  }


  ############################################# +
  ## Module analytics : RF_model_UI            ### +
  ##     id call = "RF_model_UI_1"             ### +
  ############################################# +
  {
    callModule(
      module = show_RF_model, id = "RF_model_UI_1",
      PCA,
      RF_btn = classA$analy_btn_RF,
      updated
    )
  }

  
  ############################################# +
  ## Module  : MARS_model_UI            ### +
  ##     id call = "RF_model_UI_1"             ### +
  ############################################# +
  # {
  #   callModule(
  #     module = show_MARS_model, id = "MARS_model_UI_1",
  #     PCA,
  #     MARS_btn = classA$analy_btn_MARS
  #   )
  # }

  
  ############################################# +
  ## Module  : MARS_model_UI            ### +
  ##     id call = "RF_model_UI_1"             ### +
  ############################################# +
  {
    callModule(
      module = show_XGboost_model, id = "XGboost_model_UI_1",
      PCA,
      XGboost_btn = classA$analy_btn_XGboost,
      updated
    )
  }

  
  ############################################# +
  ## Module  : Ranger_model_UI            ### +
  ##     id call = "Ranger_model_UI_1"             ### +
  ############################################# +
  {
    callModule(
      module = show_Ranger_model, id = "Ranger_model_UI_1",
      PCA,
      Ranger_btn = classA$analy_btn_Ranger,
      updated
    )
  }
  

  ############################################# +
  ## Module  : fleet_insight_UI             ### +
  ##     id call =  fleet_insight_UI_1      ### +
  ############################################# +
  {
    callModule(
      module = show_fleet_insight, id = "fleet_insight_UI_1", updated, getfile
    )
  }


  ############################################# +
  ## Module: robot_arm_tracking_UI          ### +
  ##     id call = "About_UI_1"             ### +
  ############################################# +
  {
    callModule(module = show_robotArm_tracking, id = "robotArmTracking")
  }
  
  
  ############################################# +
  ## Module: dataSize_info_UI               ### +
  ##        id call = "dataSizeInfo         ### +
  ############################################# +
  {
    callModule(module = show_dataSizeInfo, id = "dataSizeInfo", updated, getfile)
  }
  
  
  ############################################# +
  ## Module: RF_Regression_UI               ### +
  ##        id call = "show_RF_Regre        ### +
  ## add: 04/06                             ### +
  ############################################# +
  {
    callModule(module = show_RF_Regre, id = "rfGress", 
               PCA,
               updated,
               rf_regress_btn = classA$analy_btn_RF_regress,
               radioRegre_btn = classA$analy_btn__radioRegre
               )
  }
  
  

  ## ** For solving Zombi Process
  # session$onSessionEnded(function(){
  #   wait()
  # })
}
