#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ns <- session$ns
  vals <- reactiveValues()

  data(gps_fixrate, package = "movedesign")
  data(gps_tradeoffs, package = "movedesign")
  data(output_sims, package = "movedesign")
  data(movmods, package = "movedesign")

  # DYNAMIC UI ELEMENTS ---------------------------------------------------

  keep_expanded <- function(x) {
    x$children[[1]]$attribs$onclick = "event.stopPropagation()"
    x
  }

  # Render sidebar menu: --------------------------------------------------

  output$side_menu <- shinydashboard::renderMenu({

    # title, icon
    info_upload <- c("Import data", "file-upload")
    info_select <- c("Select data", "file")
    info_sims <- c("Simulate data", "file-signature")
    info_regs <- c("Tracking regime", "stopwatch")
    info_hr <- c("Home range estimation", "map-marked-alt")
    info_ctsd <- c("CTSD estimation", "tachometer-alt")

    upload_title <- info_upload[1]
    upload_icon <- shiny::icon(info_upload[2])

    select_title <- info_select[1]
    select_icon <- shiny::icon(info_select[2])

    sims_title <- info_sims[1]
    sims_icon <- shiny::icon(info_sims[2])

    regs_title <- info_regs[1]
    regs_icon <- shiny::icon(info_regs[2])

    hr_title <- info_hr[1]
    hr_icon <- shiny::icon(info_hr[2])

    ctsd_title <- info_ctsd[1]
    ctsd_icon <- shiny::icon(info_ctsd[2])

    # Workflows for data type path:

    if(!is.null(vals$which_data)) {

      regs_title <- highlight_title(info_regs[1])
      regs_icon <- highlight_icon(info_regs[2])

      if(vals$which_data == "Upload") {
        upload_title <- highlight_title(info_upload[1])
        upload_icon <- highlight_icon(name = info_upload[2])
      }

      if(vals$which_data == "Select") {
        select_title <- highlight_title(info_select[1])
        select_icon <- highlight_icon(name = info_select[2])
      }

      if(vals$which_data == "Simulate") {
        sims_title <- highlight_title(info_sims[1])
        sims_icon <- highlight_icon(name = info_sims[2])
      }
    }

    # Workflow for research question path:

    if(!is.null(vals$which_question)) {

      if("Home range" %in% vals$which_question) {
        hr_title <- highlight_title(info_hr[1])
        hr_icon <- highlight_icon(name = info_hr[2])
      } else {
        hr_title <- info_hr[1]
        hr_icon <- shiny::icon(info_hr[2])
      }

      if("Speed & distance" %in% vals$which_question) {
        ctsd_title <- highlight_title(info_ctsd[1])
        ctsd_icon <- highlight_icon(name = info_ctsd[2])
      } else {
        ctsd_title <- info_ctsd[1]
        ctsd_icon <- shiny::icon(info_ctsd[2])
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
      keep_expanded(
        shinydashboard::menuItem(
          id = "group_data",
          tabname = "data",
          text = "Data",
          icon = shiny::icon("paw"),
          startExpanded = TRUE,

          # shinydashboard::menuSubItem(
          #   tabName = "real",
          #   text = data_title,
          #   icon = data_icon),

          shinydashboard::menuSubItem(
            tabName = "data_upload",
            text = upload_title,
            icon = upload_icon),

          shinydashboard::menuSubItem(
            tabName = "data_select",
            text = select_title,
            icon = select_icon),

          shinydashboard::menuSubItem(
            tabName = "sims",
            text = sims_title,
            icon = sims_icon)
        )),

      # Tab 4: Device tradeoffs
      keep_expanded(
        shinydashboard::menuItem(
          id = "group_device",
          tabname = "device",
          text = "Device",
          icon = shiny::icon("map-marker-alt"),
          startExpanded = TRUE,

          shinydashboard::menuSubItem(
            tabName = "regime",
            text = regs_title,
            icon = regs_icon)
        )),

      # Tab 5 and 6: Home range or CTSD estimation
      keep_expanded(
        shinydashboard::menuItem(
          id = "group_design",
          tabname = "design",
          text = "Analyses",
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
        )),

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
  # mod_tab_data_server("tab_data_1", vals = vals)
  mod_tab_data_upload_server("tab_data_upload_1", vals = vals)
  mod_tab_data_select_server("tab_data_select_1", vals = vals)
  mod_tab_sims_server("tab_sims_1", vals = vals)

  # Device tab:
  mod_tab_device_server("tab_device_1", vals = vals)

  # Analyses tabs:
  mod_tab_hrange_server("tab_hrange_1", vals = vals)
  mod_tab_ctsd_server("tab_ctsd_1", vals = vals)

  # Report tab:
  mod_tab_report_server("tab_report_ui_1", vals = vals)

  # Misc: -----------------------------------------------------------------

  # Viz:
  mod_comp_viz_server("comp_viz_uploaded", vals = vals) # data_upload
  mod_comp_viz_server("comp_viz_selected", vals = vals) # data_select

  # Header and control tabs:
  mod_comp_settings_server("comp_settings_1", vals = vals)

  # Interactive tour:
  mod_comp_tour_server("tour_1", vals = vals)

  # -----------------------------------------------------------------------

  onStop(function() {

    message("Session stopped")

    if(!is.null(output_sims)) {
      rm("output_sims", envir = .GlobalEnv) }
    if(!is.null(gps_fixrate)) {
      rm("gps_fixrate", envir = .GlobalEnv) }
    if(!is.null(gps_tradeoffs)) {
      rm("gps_tradeoffs", envir = .GlobalEnv) }
    if(!is.null(movmods)) {
      rm("movmods", envir = .GlobalEnv) }

    # print(ls(envir = .GlobalEnv))

  }) # end of onStop

}
