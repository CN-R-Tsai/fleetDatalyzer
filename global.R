#' updating the max upload size to 1GB ----
options(shiny.maxRequestSize = 1024 * 1024^2)
options(scipen = 999) ## to avoid large integer id as scientific expression

#' global items 
#' Library ----
list.of.packages <- c(
  "ggplot2",
  "shinydashboard",
  "GGally",
  "DT",
  "shinycssloaders",
  "tidyverse",
  "dplyr",
  "inline",
  "shinyWidgets",
  "reshape2",
  "shiny",
  "caret", # automating the tuning process
  "randomForest",
  "earth", # fitting MARS models
  "shinyjs",
  "emo", # emoji,
  "gfonts",
  "xgboost",
  "scales",
  "RSQLite",
  "data.table",
  "reshape2",
  "gridExtra",
  "gganimate",
  "gifski",
  "ranger",
  "MASS", # Calculates the Moore-Penrose generalized inverse of a matrix X
  "moments" # calculate skewness
)


#' load all of these packages ----
lapply(list.of.packages, require, character.only = TRUE)

#' A visual insight into that black box of Shiny reactivity ----
# library(reactlog)
# tell shiny to log all reactivity
# reactlog_enable()
# reactlogShow()


#' load modules ----
source("modules/loginout.module.R")
source("modules/loadtestdata.module.R")
source("modules/loadCSVdata.module.R")
source("modules/loaddata.module.R")
source("modules/showdataUI.module.R")
source("modules/showtableUI.module.R")
source("modules/CorrdataUI.module.R")
source("modules/PCAdataUI.module.R")
source("helper_funs.R")
source("modules/datainspecttableUI.module.R")
source("modules/ScreedataUI.module.R")
source("modules/BrushpointUI.module.R")
source("modules/fleet_insight_UI.module.R")
source("modules/Analytics_UI.module.R")
source("modules/About_UI.module.R")
source("modules/RFmodelUI.module.R")
source("modules/MARSmodelUI.module.R")
source("modules/XGboostmodelUI.module.R")
source("modules/loadYdata.module.R")
source("modules/RangermodelUI.module.R")
#source("modules/robotArmTrackingUI.module.R")
source("modules/dataSizeInfo.module.R") 
source("modules/RF_Regression.module.R") 

# -----------------------------------------------------------------------------
# Use: connect to SQLite database
# Argument: instance of db connection
# Return: TRUE
# -----------------------------------------------------------------------------

connectDB <- function() {
  return(
    dbConnect(RSQLite::SQLite(), "datalab.db")
  )
}

# -----------------------------------------------------------------------------
# Use: disconnect from SQLite database
# Argument: instance of db connection
# Return: TRUE
# -----------------------------------------------------------------------------

disconnectDB <- function(con) {
  dbDisconnect(con)
}

# -----------------------------------------------------------------------------
# Use: get userName & password from db
# Argument: username & password
# Return: TRUE
# -----------------------------------------------------------------------------

get.userInfo <- function(username, password) {
  qry <- paste0("Select username, password from user_list;")
  con <- connectDB()
  userInfo <- dbGetQuery(con, qry)
  disconnectDB(con)
  return(userInfo)
}

# -----------------------------------------------------------------------------
# Use: get fleet information
# Argument:
# Return:
# -----------------------------------------------------------------------------

get.fleetInfo <- function() {
  qry <- paste0("Select fleet_name from fleet_list;")
  con <- connectDB()
  fleetInfo <- dbGetQuery(con, qry)
  disconnectDB(con)

  list.fleet <- sapply(fleetInfo, function(x) {
    as.list(x)
  })
}


# -----------------------------------------------------------------------------
# Use: calculate PCA
# Arguments:
# Return: the_data, pca_output, pcdata
# -----------------------------------------------------------------------------

calculate_pca <- function(the_data) {
  cat(paste0("Included Sensors: \t", nrow(the_data), "x", ncol(the_data), "\n"))
  print(dim(the_data))
  
  # Rows contain one or more NAs will be excluded from the PCA.
  # Columns that contain a mixture of numbers and text will not be included in the computation of the PCA results.
  the_data_num <- na.omit(the_data[, sapply(the_data, is.numeric)])
  # exclude cols with zero variance
  the_data_subset <- the_data_num[, !apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]

  ## --- Pr <- prcomp(x, scale. = TRUE)
  pca_output <- prcomp(na.omit(the_data_subset), center = FALSE, scale. = TRUE)
  
  md <- calculate_mahalanobis(pca_output)

  auto_rank(pca_output)

  # data.frame of PCs
  #pcdata <- cbind(the_data$Start.Time, pca_output$x, md)

  pcdata <- cbind(the_data, pca_output$x, md)
  pcdata$Start.Time <- as.Date(pcdata$Start.Time, format = "%Y/%m/%d %H:%M", tz = "GMT")
  return(list(
    the_data = the_data,
    pca_output = pca_output,
    pcdata = pcdata
  ))
}


# -----------------------------------------------------------------------------
# Use: create PCA plot
# Arguments:
#    pcdata, pca_output, the_pcs_to_plot_x, the_pcs_to_plot_y, the_grouping_variable,
#    df_classA, df_classB, zooming, plotType, boxStat
# Return: pc_plot_no_groups, pc_plot_groups
# -----------------------------------------------------------------------------

makePlot_pca <- function(pcdata, 
                         pca_output,
                         the_pcs_to_plot_x,
                         the_pcs_to_plot_y,
                         the_grouping_variable,
                         df_classA,
                         df_classB,
                         zooming,
                         plotType, 
                         boxStats,
                         plotOption,
                         lineOption) {
  pcdata <- pcdata
  pca_output <- pca_output
  var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", the_pcs_to_plot_x))]^2 / sum(pca_output$sdev^2), 1)
  var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", the_pcs_to_plot_y))]^2 / sum(pca_output$sdev^2), 1)
  labels <- rownames(pca_output$x)
  grouping <- the_grouping_variable
  
  #pcdata$Start.Time <- as.POSIXct(pcdata$Start.Time, format = "%Y/%m/%d %H:%M", tz = "GMT")
  
  #' Draw plot -----
  if (plotType == "Scatter") {
    if (grouping == "None") {
      print("\n > Enter in scatter plot \n")
      #  #plot without grouping variable
      #
      pc_plot_no_groups <- ggplot(data = pcdata, aes_string(the_pcs_to_plot_x, the_pcs_to_plot_y)) +
        geom_point(size = 2) +
        geom_point(
          data = brushedPoints(pcdata, df_classA),
          color = "red",
          shape = 2,
          size = 3
        ) +
        geom_point(
          data = brushedPoints(pcdata, df_classB),
          color = "blue",
          shape = 1,
          size = 3
        ) +
        theme_bw(base_size = 14) +
        #xlab(paste0(the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        #ylab(paste0(the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) +
        coord_cartesian(xlim = zooming$x, ylim = zooming$y)
      pc_plot_no_groups
      
    } else { # daisy-chained
      #  # 'plot with grouping variable
      #
      print("\n > Enter in grouping variable plot \n")
      pcdata$fill_ <- as.character(pcdata[, grouping, drop = TRUE])
      pc_plot_groups <- ggplot(pcdata, 
                               mapping = aes_string(the_pcs_to_plot_x, 
                                                    the_pcs_to_plot_y,
                                                    fill = "fill_",
                                                    colour = "fill_")) +
        geom_point(size = 3) +
        geom_point(
        data = brushedPoints(pcdata, df_classA),
        color = "red",
        shape = 2,
        size = 3
        ) +
        geom_point(
          data = brushedPoints(pcdata, df_classB),
          color = "blue",
          shape = 1,
          size = 3
        ) +
        theme_bw(base_size = 14) +
        scale_colour_discrete(guide = FALSE) +
        guides(fill = guide_legend(title = "groups")) +
        theme(legend.position = "top") +
        #xlab(paste0(the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        #ylab(paste0(the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) +
        coord_cartesian(xlim = zooming$x, ylim = zooming$y) 
      
      if ("line" %in% lineOption) {
        pc_plot_groups <- pc_plot_groups + geom_smooth(method = "rlm", se = F, colour = "red")
        pc_plot_groups
      }
      if ("smooth" %in% lineOption) {
        pc_plot_groups <- pc_plot_groups + geom_smooth(method = "lm", se = F, colour = "red")
        pc_plot_groups
      }
      if ("density" %in% lineOption) {
        pc_plot_groups <- pc_plot_groups + geom_density2d(colour = "red")
        pc_plot_groups
      }
      
    　if ("ellipse" %in% plotOption) {
        pc_plot_groups <- pc_plot_groups + stat_ellipse(geom = "polygon", alpha = 0.1)
        pc_plot_groups
      }
      if ("facet" %in% plotOption) {
        pc_plot_groups <- pc_plot_groups + facet_wrap(as.formula(paste("~", grouping)), ncol = 3) # Passing string variable to facet_wrap
        pc_plot_groups
      }
      if ("noLegend" %in% plotOption) {
        pc_plot_groups <- pc_plot_groups + theme(legend.position = "none")
        pc_plot_groups
      } else {
        return(pc_plot_groups)
      }
      
    }
    
  } else {
    #  # boxplot
    #
    if (grouping == "None") {
      #  #plot without grouping variable
      #
      pc_plot_no_groups <- ggplot(data = pcdata, aes_string(the_pcs_to_plot_x, the_pcs_to_plot_y)) +
        geom_boxplot(position = "dodge") +
        xlab("")
      pc_plot_no_groups
    } else { # nested
      #  # plot with grouping variable
      #
      pcdata$fill_ <- as.character(pcdata[, grouping, drop = TRUE])

      pc_plot_groups <- ggplot(pcdata, mapping = aes_string(
        x = "fill_",
        y = the_pcs_to_plot_y,
        colour = "fill_"
      )) +
        geom_boxplot(position = "dodge") +
        xlab("") +
        scale_color_discrete(name = "Grouping Variable")
      if (1 %in% boxStats) {
        pc_plot_groups <- pc_plot_groups + stat_summary(
          fun = "min",
          aes(label = round(..y.., digits = 2)), vjust = 1.5, geom = "text", show.legend = F
        )
        pc_plot_groups
      }
      if (2 %in% boxStats) {
        pc_plot_groups <- pc_plot_groups + stat_summary(
          fun = function(z) {
            quantile(z, 0.25)
          },
          aes(label = round(..y.., digits = 2)), hjust = -0.1, vjust = 1.5, geom = "text", show.legend = F
        )
        pc_plot_groups
      }
      if (3 %in% boxStats) {
        pc_plot_groups <- pc_plot_groups + stat_summary(
          fun = "median",
          aes(label = round(..y.., digits = 2)), vjust = -0.5, geom = "text", show.legend = F
        )
        pc_plot_groups
      }
      if (4 %in% boxStats) {
        pc_plot_groups <- pc_plot_groups + stat_summary(
          fun = function(z) {
            quantile(z, 0.75)
          },
          aes(label = round(..y.., digits = 2)), hjust = -0.1, vjust = -0.6, geom = "text", show.legend = F
        )
        pc_plot_groups
      }
      if (5 %in% boxStats) {
        pc_plot_groups <- pc_plot_groups + stat_summary(
          fun = "max",
          aes(label = round(..y.., digits = 2)), vjust = -0.6, geom = "text", show.legend = F
        )
        pc_plot_groups
      } else {
        print("\n > No stats selected \n")
        return(pc_plot_groups)
      }
    }
  }
}

#-------------------------------------------------------------------------------
# Use: create fleet overview plot
# Arguments:
# Return: figure & total_count
#-------------------------------------------------------------------------------

makePlot_overview <- function(All_data) {
  Total_file_count <- All_data %>%
    summarise(total_file = sum(count))
  Number_of_PM <- nrow(All_data)
  
  Figure <- All_data %>%
    ggplot(aes(x = reorder(tool_pm, -count), y = count)) +
    geom_bar(position = "stack", stat = "identity", alpha = .6, width = .4) +
    theme(
      axis.text = element_text(angle = 90, hjust = 1, size = 12),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      legend.position = "none"
    ) +
    xlab("Tool_PM") +
    ylab("Total # of files")

  wafer_data_tbl_size <- opt1

  newlist <- list("Pic" = Figure, "TC" = Total_file_count, "NP" = Number_of_PM, "waferDataTblSize" = wafer_data_tbl_size)

  return(newlist)
}
