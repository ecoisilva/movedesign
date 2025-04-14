#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  ns <- session$ns
  
  species <- c(
    "buffalo" = "African Buffalo",
    "pelican" = "Brown Pelican",
    "coati" = "Coati",
    "jaguar" = "Jaguar",
    "wolf" = "Maned Wolf",
    "gazelle" = "Mongolian Gazelle",
    "turtle" = "Wood turtle")
  
  species_binom <- c(
    "buffalo" = "Syncerus caffer",
    "pelican" = "Pelecanus occidentalis",
    "coati" = "Nasua narica",
    "jaguar" = "Panthera onca",
    "wolf" = "Chrysocyon brachyurus",
    "gazelle" = "Procapra gutturosa",
    "turtle" = "Glyptemys insculpta")
  
  rv <- reactiveValues(
    ctmm = data.frame(cbind(species, species_binom)),
    data_type = NULL,
    
    nsims = NULL,
    species = NULL,
    id = NULL,
    
    which_m = "none",
    which_meta = "none",
    
    groups = list(intro = list(A = c(), B = c()),
                  final = list(A = c(), B = c())),
    grouped = FALSE,
    
    truth = list(hr = list(area = list(),
                           data = list()),
                 ctsd = list()),
    
    dev = NULL,
    pars = NULL,
    hr = NULL,
    sd = NULL,
    report = NULL,
    tmp = NULL,
    status = FALSE,
    set_analysis = NULL,
    
    seedList = list(),
    ctsdList = list(),
    akdeList = list(),
    pathList = list(),
    
    meanfit = NULL, 
    is_emulate = FALSE,
    random = FALSE,
    
    is_isotropic = NULL,
    is_analyses = FALSE,
    is_report = FALSE,
    is_meta = FALSE,
    is_font = FALSE,
    add_note = FALSE,
    
    err_prev = list("hr" = rep(1, 5), "ctsd" = rep(1, 5)),
    dev_failed = c(),
    
    tour_active = FALSE,
    alert_active = TRUE,
    overwrite_active = FALSE,
    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
    time = list(
      "upload" = c(0, 0),
      "sims" = c(0, 0),
      "hr" = c(0, 0),
      "ctsd" = c(0, 0),
      "total" = c(0, 0)),
    
    highlight_dur = "",
    highlight_dti = "",
    var_fraction = .5,
    
    restored_rv = NULL,
    restored = NULL)
  
  # DYNAMIC UI ELEMENTS ---------------------------------------------------
  
  keep_expanded <- function(x) {
    x$children[[1]]$attribs$onclick <- "event.stopPropagation()"
    x
  }
  
  ## Render sidebar menu: -------------------------------------------------
  
  output$side_menu <- shinydashboard::renderMenu({
    
    info <- list(
      upload = c(title = "Import data", icon = "file-csv"),
      select = c(title = "Select data", icon = "file-circle-plus"),
      sims = c(title = "Simulate data", icon = "file-pen"),
      
      design = c(title = "Sampling design", icon = "stopwatch"),
      
      hr = c(title = "Home range", icon = "map-location-dot"),
      ctsd = c(title = "Speed & distance", icon = "gauge-high"),
      meta = c(title = "Meta-analyses", icon = "layer-group")
    )
    
    shinydashboard::sidebarMenu(
      id = "tabs",
      
      # Tab 1: Home
      shinydashboard::menuItem(
        text = " Home",
        tabName = "about",
        icon = icon("house")
      ),
      
      # Tab 2 and 3: Data
      keep_expanded(
        shinydashboard::menuItem(
          id = "group_data",
          tabname = "data",
          text = "Species",
          icon = shiny::icon("paw"),
          startExpanded = TRUE,
          
          if (is.null(rv$which_data) ||
              rv$which_data == "Upload") {
            shinydashboard::menuSubItem(
              tabName = "data_upload",
              text = info$upload[["title"]],
              icon = shiny::icon(info$upload[["icon"]])) },
          
          if (is.null(rv$which_data) ||
              rv$which_data == "Select") {
            shinydashboard::menuSubItem(
              tabName = "data_select",
              text = info$select[["title"]],
              icon = shiny::icon(info$select[["icon"]])) },
          
          if (is.null(rv$which_data) ||
              rv$which_data == "Simulate") {
            shinydashboard::menuSubItem(
              tabName = "simulate",
              text = info$sims[["title"]],
              icon = shiny::icon(info$sims[["icon"]])) }
        )),
      
      # Tab 4: Device
      keep_expanded(
        shinydashboard::menuItem(
          id = "group_design",
          tabname = "design",
          text = "Device",
          icon = shiny::icon("location-dot"),
          startExpanded = TRUE,
          
          shinydashboard::menuSubItem(
            tabName = "device",
            text = info$design[["title"]],
            icon = shiny::icon(info$design[["icon"]]))
        )),
      
      # Tab 5 and 6: Analyses
      keep_expanded(
        shinydashboard::menuItem(
          id = "group_analyses",
          tabname = "analyses",
          text = "Analyses",
          icon = shiny::icon("compass-drafting"),
          startExpanded = TRUE,
          
          if (is.null(rv$which_question) ||
              "Home range" %in% rv$which_question) {
            shinydashboard::menuSubItem(
              tabName = "hr",
              text = info$hr[["title"]],
              icon = shiny::icon(info$hr[["icon"]])) },
          
          if (is.null(rv$which_question) ||
              "Speed & distance" %in% rv$which_question) {
            shinydashboard::menuSubItem(
              tabName = "ctsd",
              text = info$ctsd[["title"]],
              icon = shiny::icon(info$ctsd[["icon"]])) },
          
          if (is.null(rv$which_meta) ||
              req(rv$which_meta) != "none") {
            shinydashboard::menuSubItem(
              tabName = "meta",
              text = info$meta[["title"]],
              icon = shiny::icon(info$meta[["icon"]])) }
        )),
      
      # Tab 8: Report
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
    rv$active_tab <- input$tabs
  }) %>% bindEvent(input$tabs)
  
  # Tabs: -----------------------------------------------------------------
  
  # About/workflow:
  mod_tab_about_server("tab_about_1", rv = rv)
  
  # Data tabs:
  mod_tab_data_upload_server("tab_data_upload_1", rv = rv)
  mod_tab_data_select_server("tab_data_select_1", rv = rv)
  mod_tab_sims_server("tab_sims_1", rv = rv)
  
  # Device tab:
  mod_tab_design_server("tab_design_1", rv = rv)
  
  # Analyses tabs:
  mod_tab_hrange_server("tab_hrange_1", rv = rv)
  mod_tab_ctsd_server("tab_ctsd_1", rv = rv)
  mod_tab_meta_server("tab_meta_1", rv = rv)
  
  # Report tab:
  mod_tab_report_server("tab_report_1", rv = rv)
  
  # Misc: -----------------------------------------------------------------
  
  # Number of tags to deploy:
  mod_comp_m_server("comp_m_in_hr", rv = rv, set_analysis = "hr")
  mod_comp_m_server("comp_m_in_ctsd", rv = rv, set_analysis = "ctsd")
  
  # Parameters:
  mod_comp_pars_server("comp_pars_uploaded", rv = rv, set_type = "upload")
  mod_comp_pars_server("comp_pars_selected", rv = rv, set_type = "select")
  
  # Data viz:
  mod_viz_server("comp_viz_uploaded", rv = rv)
  mod_viz_server("comp_viz_selected", rv = rv)
  
  # Meta viz:
  mod_viz_meta_server("viz_meta_1", rv = rv)
  mod_viz_meta_server("viz_meta_2", rv = rv)
  
  # Header and control tabs:
  mod_comp_settings_server("comp_settings_1", rv = rv)
  
  # Interactive tour:
  mod_comp_tour_server("tour_1", rv = rv)
  
  # Alerts:
  mod_comp_alerts_server("comp_alerts_1", rv = rv)
  
  # -----------------------------------------------------------------------
  
  # Table theme:
  options(reactable.theme = reactable::reactableTheme(
    rowSelectedStyle = list(
      backgroundColor = "#eee",
      boxShadow = "inset 2px 0 0 0 #009da0")
  ))
  
  # onStop(function() {
  #   message("Session stopped")
  # 
  #   # if (!is.null(fixrates)) rm("fixrates", envir = .GlobalEnv)
  #   # if (!is.null(sims_hrange)) rm("sims_hrange", envir = .GlobalEnv)
  #   # if (!is.null(sims_speed)) rm("sims_speed", envir = .GlobalEnv)
  #   # # print(ls(envir = .GlobalEnv))
  # 
  # }) # end of onStop
  
}
