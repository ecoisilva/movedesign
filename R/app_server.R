#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ns <- session$ns
  vals <- reactiveValues()

  # DYNAMIC UI ELEMENTS ---------------------------------------------------

  keep_expanded <- function(x) {
    x$children[[1]]$attribs$onclick = "event.stopPropagation()"
    x
  }

  ## Render sidebar menu: -------------------------------------------------

  output$side_menu <- shinydashboard::renderMenu({

    # title, icon
    info_upload <- c("Import data", "file-arrow-up")
    info_select <- c("Select data", "file")
    info_sims <- c("Simulate data", "file-signature")
    info_regs <- c("Tracking regime", "stopwatch")
    info_hr <- c("Home range", "map-location-dot")
    info_ctsd <- c("Speed & distance", "gauge-high")

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

    shinydashboard::sidebarMenu(
      id = "tabs",

      # Tab 1: Home
      shinydashboard::menuItem(
        text = "Home",
        tabName = "about",
        icon = icon("house")
      ),

      # Tab 2 and 3: Upload or simulate data
      keep_expanded(
        shinydashboard::menuItem(
          id = "group_data",
          tabname = "data",
          text = "Species",
          icon = shiny::icon("paw"),
          startExpanded = TRUE,

          if(is.null(vals$which_data) ||
             vals$which_data == "Upload") {
            shinydashboard::menuSubItem(
              tabName = "data_upload",
              text = upload_title,
              icon = upload_icon) },

          if(is.null(vals$which_data) ||
             vals$which_data == "Select") {
            shinydashboard::menuSubItem(
              tabName = "data_select",
              text = select_title,
              icon = select_icon) },

          if(is.null(vals$which_data) ||
             vals$which_data == "Simulate") {
            shinydashboard::menuSubItem(
              tabName = "sims",
              text = sims_title,
              icon = sims_icon) }
        )),

      # Tab 4: Device
      keep_expanded(
        shinydashboard::menuItem(
          id = "group_device",
          tabname = "device",
          text = "Device",
          icon = shiny::icon("location-dot"),
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
          icon = shiny::icon("compass-drafting"),
          startExpanded = TRUE,

          if (is.null(vals$which_question) ||
              "Home range" %in% vals$which_question) {
            shinydashboard::menuSubItem(
              tabName = "hr",
              text = hr_title,
              icon = hr_icon) },

          if (is.null(vals$which_question) ||
              "Speed & distance" %in% vals$which_question) {
            shinydashboard::menuSubItem(
              tabName = "ctsd",
              text = ctsd_title,
              icon = ctsd_icon) }
        )),

      # Tab 7: Report
      shinydashboard::menuItem(
        text = "Report",
        tabName = "report",
        icon = icon("box-archive")
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
  mod_tab_data_upload_server("tab_data_upload_1", vals = vals)
  mod_tab_data_select_server("tab_data_select_1", vals = vals)
  mod_tab_sims_server("tab_sims_1", vals = vals)

  # Device tab:
  mod_tab_device_server("tab_device_1", vals = vals)

  # Analyses tabs:
  mod_tab_hrange_server("tab_hrange_1", vals = vals)
  mod_tab_ctsd_server("tab_ctsd_1", vals = vals)

  # Report tab:
  mod_tab_report_server("tab_report_1", vals = vals)

  # Misc: -----------------------------------------------------------------

  # Data viz:
  mod_comp_viz_server("comp_viz_uploaded", vals = vals) # Uploaded
  mod_comp_viz_server("comp_viz_selected", vals = vals) # Selected

  # Header and control tabs:
  mod_comp_settings_server("comp_settings_1", vals = vals)

  # Interactive tour:
  mod_comp_tour_server("tour_1", vals = vals)

  # -----------------------------------------------------------------------

  options(reactable.theme = reactable::reactableTheme(
    borderColor = "#f2f2f2",
    rowSelectedStyle = list(
      backgroundColor = "#eee",
      boxShadow = "inset 2px 0 0 0 #009da0")
  ))


  onStop(function() {

    message("Session stopped")

    if(!is.null(gps_fixrate)) rm("gps_fixrate", envir = .GlobalEnv)
    if(!is.null(sims_hrange)) rm("sims_hrange", envir = .GlobalEnv)
    if(!is.null(sims_speed)) rm("sims_speed", envir = .GlobalEnv)
    print(ls(envir = .GlobalEnv))

  }) # end of onStop

}
