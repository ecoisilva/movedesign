#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # UI ELEMENTS ---------------------------------------------------------

    tags$html(lang = "en"),
    shinydashboardPlus::dashboardPage(

      options = list(sidebarExpandOnHover = TRUE),
      title = "Study design of movement ecology studies",
      skin = "black",

      # HEADER ------------------------------------------------------------

      header = shinydashboardPlus::dashboardHeader(
        titleWidth = 220,
        title = tagList(
          tags$span(
            class = "logo-mini",
            tags$img(title = "title",
                     src = "www/favicon.ico",
                     height = "30px")),

          tags$span(
            class = "logo-lg",
            tags$img(title = "title",
                     src = "www/logo.png",
                     height = "50px"))
        ),

        controlbarIcon = icon("gears"),
        shinydashboard::dropdownMenu(

          shinydashboardPlus::messageItem(
            from = "GitHub",
            message = "Documentation, source, & citation",
            icon = tags$i(class = "fa fa-github",
                          style = "color: #000000"),
            href = "https://github.com/ecoisilva/movedesign"),

          shinydashboardPlus::messageItem(
            from = "Issues",
            message = "Report Issues",
            icon = icon("circle-exclamation"),
            href = "https://github.com/ecoisilva/movedesign/issues"),
          icon = icon("circle-info"),

          type = "messages",
          badgeStatus = NULL,
          headerText = "Information:")

      ), # end of dashboardHeader

      # SIDEBAR -----------------------------------------------------------

      sidebar = shinydashboardPlus::dashboardSidebar(
        width = 220,

        shinydashboard::sidebarMenuOutput("side_menu")

      ), # end of dashboardSidebar

      # CONTROL BAR -------------------------------------------------------

      controlbar = shinydashboardPlus::dashboardControlbar(
        id = "controlbar",
        skin = "dark",
        width = 300,

        shinydashboardPlus::controlbarMenu(
          id = "controlbarMenu",

          shinydashboardPlus::controlbarItem(
            title = "Settings", icon = icon("sliders"),

            mod_comp_settings_ui("comp_settings_1")

          ), # end of controlbar item (settings)

          shinydashboardPlus::controlbarItem(
            title = "Share", icon = icon("share-nodes"),

            fluidRow(
              tags$div(
                style = paste("font-size: 30px;",
                              "letter-spacing: .5rem;",
                              "text-align: center"),
                tags$a(href = "https://github.com/",
                       icon("github")),
                tags$a(href = "https://twitter.com/",
                       icon("twitter")),
                tags$a(href = "https://www.linkedin.com/",
                       icon("linkedin")))
            )
          ) # end of controlbar item (share)

        ) # end of controlbarMenu
      ), # end of dashboardControlbar

      # BODY --------------------------------------------------------------

      body = shinydashboard::dashboardBody(

        shinydashboard::tabItems(

          # Tab 1 'Home'
          shinydashboard::tabItem(
            tabName = "about",
            mod_tab_about_ui("tab_about_1")),

          # Tab 2.1 'Upload data'
          shinydashboard::tabItem(
            tabName = "data_upload",
            mod_tab_data_upload_ui("tab_data_upload_1")),

          # Tab 2.2 'Select data'
          shinydashboard::tabItem(
            tabName = "data_select",
            mod_tab_data_select_ui("tab_data_select_1")),

          # Tab 3 'Simulate data'
          shinydashboard::tabItem(
            tabName = "sims",
            mod_tab_sims_ui("tab_sims_1")),

          # Tab 4 'Device'
          shinydashboard::tabItem(
            tabName = "regime",
            mod_tab_device_ui("tab_device_1")),

          # Tab 5 'Home range estimation'
          shinydashboard::tabItem(
            tabName = "hr",
            mod_tab_hrange_ui("tab_hrange_1")),

          # Tab 6 'CTSD estimation'
          shinydashboard::tabItem(
            tabName = "ctsd",
            mod_tab_ctsd_ui("tab_ctsd_1")),

          # Tab 7 'Report'
          shinydashboard::tabItem(
            tabName = "report",
            mod_tab_report_ui("tab_report_1"))

        ) # end of tabItems
      ), # end of dashboardBody

      footer = shinydashboardPlus::dashboardFooter(

        left = tags$div(
          HTML(paste0('<i class="fa fa-code-branch"',
                      'style = "color:#222d32;"> </i>',
                      HTML('&nbsp;'), packageVersion("movedesign")))),
        right = tags$div(
          HTML(paste('<i class="fa fa-copyright"',
                     'style = "color:#222d32;"> </i>',
                     "2022")))

      ) # end of dashboardFooter
    ) # end of dashboardPage

  ) # end of tagList
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),

    bundle_resources(
      path = app_sys("app/www"),
      app_title = "movedesign"
    ),

    shinyFeedback::useShinyFeedback(),
    shinybrowser::detect(),

    # Activate tooltips and popovers:
    bsplus::use_bs_tooltip(),
    bsplus::use_bs_popover(),

    # Font types:
    tags$link(href = paste0("https://fonts.googleapis.com/css?",
                            "family=",
                            "Plus+Jakarta+Sans", "|",
                            "Roboto+Condensed", "|",
                            "Roboto+Condensed:wght@700", "|",
                            "Fira+Mono", "|",
                            "Fira+Sans+Condensed",
                            "&display=fallback"),
              rel = "stylesheet"),

    # Misc:
    rintrojs::introjsUI(), # set up introjs
    shinyjs::useShinyjs(), # set up shinyjs
    shinyjs::extendShinyjs(script = "www/fullscreen.js",
                           functions = c("toggleFullScreen"))

  )
}
