#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ns <- session$ns
  vals <- reactiveValues()

  data(gps_fixrate)
  data(gps_tradeoffs)
  data(movmods)
  data(df_sims)

  # DYNAMIC UI ELEMENTS ---------------------------------------------------
  # Render sidebar menu: --------------------------------------------------

  output$side_menu <- shinydashboard::renderMenu({

    # Workflows for data type path:
    if(is.null(vals$which_data)) {

      data_title <- "Upload or select data"
      data_icon <- shiny::icon("file-upload")

      sims_title <- "Simulate data"
      sims_icon <- shiny::icon("file-signature")

      regs_title <- "Tracking regime"
      regs_icon <- shiny::icon("stopwatch")

    } else {

      selected_datatype <- vals$which_data

      if(selected_datatype == "Upload" ||
         selected_datatype == "Select") {
        data_title <- highlight_title("Upload or select data")
        data_icon <- highlight_icon(name = "file-upload")

        sims_title <- "Simulate data"
        sims_icon <- shiny::icon("file-signature")

        regs_title <- highlight_title("Tracking regime")
        regs_icon <- highlight_icon(name = "stopwatch")

      }

      if(selected_datatype == "Simulate") {
        data_title <- "Upload or select data"
        data_icon <- shiny::icon("file-upload")

        sims_title <- highlight_title("Simulate data")
        sims_icon <- highlight_icon(name = "file-signature")
        regs_title <- highlight_title("Tracking regime")
        regs_icon <- highlight_icon(name = "stopwatch")
      }
    }

    # Workflow for research question path:
    if(is.null(vals$which_question)) {

      hr_title <- "Home range estimation"
      hr_icon <- shiny::icon("map-marked-alt")

      ctsd_title <- "CTSD estimation"
      ctsd_icon <- shiny::icon("tachometer-alt")

    } else {

      selected_question <- vals$which_question

      if("Home range" %in% selected_question) {
        hr_title <- highlight_title("Home range estimation")
        hr_icon <- highlight_icon(name = "map-marked-alt")
      } else {
        hr_title <- "Home range estimation"
        hr_icon <- shiny::icon("map-marked-alt")
      }

      if("Distance/speed" %in% selected_question) {
        ctsd_title <- highlight_title("CTSD estimation")
        ctsd_icon <- highlight_icon(name = "tachometer-alt")
      } else {
        ctsd_title <- "CTSD estimation"
        ctsd_icon <- shiny::icon("tachometer-alt")
      }

    }

    shinydashboard::sidebarMenu(
      id = "tabs",

      # Tab 1: Home
      shinydashboard::menuItem(
        text = "Home",
        tabName = "about",
        icon = shiny::icon("home")
      ),

      # Tab 2 and 3: Upload or simulate data
      shinydashboard::menuItem(
        id = "group_data",
        tabname = "data",
        text = "Data",
        icon = shiny::icon("paw"),
        startExpanded = TRUE,

        shinydashboard::menuSubItem(
          tabName = "real",
          text = data_title,
          icon = data_icon),

        shinydashboard::menuSubItem(
          text = sims_title,
          tabName = "sims",
          icon = sims_icon)
      ),

      # Tab 4: Device tradeoffs
      shinydashboard::menuItem(
        tabname = "group_device",
        text = "Device",
        icon = shiny::icon("map-marker-alt"),
        startExpanded = TRUE,

        shinydashboard::menuSubItem(
          tabName = "device",
          text = regs_title,
          icon = regs_icon)
      ),

      # Tab 5 and 6: Home range or CTSD estimation
      shinydashboard::menuItem(
        id = "group_design",
        tabname = "design",
        text = "Analyses", # "Study design",
        icon = shiny::icon("drafting-compass"),
        startExpanded = TRUE,

        shinydashboard::menuSubItem(
          tabName = "hr",
          text = hr_title,
          icon = hr_icon),
        shinydashboard::menuSubItem(
          tabName = "ctsd",
          text = ctsd_title,
          icon = ctsd_icon)
      ),

      # Tab 7: Report
      shinydashboard::menuItem(
        text = "Summary",
        tabName = "report",
        icon = shiny::icon("archive")
      ), tags$hr(),

      # Source code:
      shinydashboard::menuItem(
        text = "Source code",
        icon = icon("code"),
        href = "https://github.com/ecoisilva/movedesign")

    ) # end of sidebarMenu
  }) # end of renderMenu

  observe({
    vals$active_tab <- input$tabs
  }) %>% bindEvent(input$tabs)

  # Tabs: -----------------------------------------------------------------

  # About this app:
  mod_tab_about_server("tab_about_1", vals = vals)

  # Data tabs:
  mod_tab_data_server("tab_data_1", vals = vals)
  mod_tab_sims_server("tab_sims_1", vals = vals)

  # Device tab:
  mod_tab_device_server("tab_device_1", vals = vals)

  # Analyses tabs:
  mod_tab_hrange_server("tab_hrange_1", vals = vals)
  mod_tab_ctsd_server("tab_ctsd_1", vals = vals)

  # Report tab:
  mod_tab_report_server("tab_report_ui_1", vals = vals)

  # Misc: -----------------------------------------------------------------

  # Header and control tabs:
  mod_comp_settings_server("comp_settings_1", vals = vals)

  # Interactive tour:
  mod_comp_tour_server("tour_1", vals = vals)

}
