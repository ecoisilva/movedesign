#' tab_about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_tab_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(

      # Introduction: -----------------------------------------------------

      shinydashboardPlus::box(
        title = NULL,
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = FALSE, closable = FALSE,

        column(
          align = "center", width = 12,
          style = "padding: 0 20px 0 20px;",

          img(src = "www/logo.png", height = "140px"), p(),

          p(style = "max-width: 685px;",

            "This", a(href = 'http://shiny.rstudio.com', 'Shiny'),
            "application will assist researchers in designing",
            span("movement ecology", class = "cl-grn"),
            "studies related to two",
            "main research questions: the estimation of",
            span("home ranges", class = "cl-sea-d"), "and of",
            HTML(paste0(span("speed and distance traveled",
                             class = "cl-sea-d"), ".")))

          ) # end of column (text)
      ), # end of box // intro

      # Tour/tutorial section: --------------------------------------------

      # Section currently in progress:
      shinydashboardPlus::box(
        id = "about_tour",
        title = NULL,
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = FALSE, closable = FALSE,

        column(
          align = "center", width = 12,

          br(),
          h2("How does this",
             span("application", class = "cl-sea"), "work?"),
          p(),
          mod_comp_tour_ui("tour_1"), p()

        ) # end of column (text)
      ), # end of box // tour

      # Workflows section: ------------------------------------------------

      shinydashboardPlus::box(
        id = "about_workflow",
        title = NULL,
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = FALSE, closable = FALSE,

        column(
          align = "center", width = 12,

          fluidRow(
            align = "center",
            div(id = "content-workflow",

                br(),
                h2("What is your",
                   wrap_none(span("workflow",
                                    class = "cl-sea"), "?")),
                p(style = "padding: none;"),

                ### Data source -------------------------------------------

                shinyWidgets::radioGroupButtons(
                  inputId = ns("which_data"),
                  label = span("Data source:", style = "font-size: 16px;"),
                  choices = c("Upload" = "Upload",
                              "Select" = "Select",
                              "Simulate" = "Simulate"),
                  selected = character(0),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square",
                                 style = "color: var(--sea);"),
                    no = tags$i(class = "fa fa-square-o",
                                style = "color: var(--danger);"))),

                ### Research question(s) ----------------------------------

                # shinyWidgets::checkboxGroupButtons()
                shinyWidgets::radioGroupButtons(
                  inputId = ns("which_question"),
                  label = span("Research question:",
                               style = "font-size: 16px;"),
                  choices = c("Home range",
                              "Speed & distance"),
                  selected = NULL,
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square",
                                 style = "color: var(--sea);"),
                    no = tags$i(class = "fa fa-square-o",
                                style = "color: var(--danger);"))),
                br()

            ) # end of div
          ) # end of fluidRow

        ) # end of column
      ), # end of box // tour

      # Miscellaneous: ----------------------------------------------------

      shinydashboardPlus::box(
        id = "about_misc",
        title = NULL,
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = FALSE, closable = FALSE,

        column(
          align = "center", width = 6,
          style = "border-right: 1px solid #ececec;",

          ## Restore application settings: --------------------------------

          h5("Restore settings:"),

          p("After using this app for the first time, you can save",
            "your progress (and any previously defined values) to",
            "your local environment, by accessing the",
            icon("gears", class = "cl-mdn"),
            "symbol on the",
            HTML(paste0(span("upper right corner", class = "cl-blk"), "."))
          ),

          uiOutput(ns("saving_vals")),

          fileInput(ns("restore_state"),
                    label = "Upload file:",
                    accept = ".rds",
                    placeholder = ".rds file"),

          p("Click", span("Browse...", class = "cl-blk"),
            "and select a previously saved",
            span(".rds", class = "cl-sea"),
            "file to load stored values.")

        ), # end of column (right)

        ## Contact details: ---------------------------------------------

        column(
          align = "center", width = 6,

          h5("Project contact:"),
          p("In\u00EAs Silva,",
            span("i.simoes-silva\u0040hzdr.de", class = "cl-sea")),
          tags$hr(),

          h5("Institutional contact:"),
          p(style = paste("font-size: 14px; text-align: justify;"),
            paste(
              "Center for Advanced Systems Understanding (CASUS),",
              "Helmholtz-Zentrum Dresden-Rossendorf e.V. (HZDR),",
              "Untermarkt 20, 02826, G\u00F6rlitz — Germany")),
          p(),
          img(src = "www/logo_casus.png", height = "70px"), p(),
          a(href = "https://www.casus.science/",
            "https://www.casus.science/"), p()

        ) # end of column (right)
      ) # end of box

    ) # end of fluidRow
  ) # end to tagList
}

#' tab_about Server Functions
#'
#' @noRd
mod_tab_about_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store values: -------------------------------------------------------

    observe({
      vals$which_data <- input$which_data
      vals$which_question <- input$which_question

    })

    observe({
      # shinyWidgets::updateCheckboxGroupButtons()
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "which_question",
        selected = vals$which_question)
    })

    # SETTINGS ------------------------------------------------------------
    ## Restore settings/values (from previous session): -------------------

    observe({

      validate(need(input$restore_state, message = FALSE))
      restored_vals <- readRDS(
        file = paste(input$restore_state$datapath))
      vals$restored_vals <- restored_vals

      shiny::showModal(
        shiny::modalDialog(
          title = "Previous parameters restored!",

          p("Data status:"),
          p("Type:", vals$restored_vals$"data_type"),

          p("Parameters:"),
          p("Seed:", vals$restored_vals$"seed0"),


          footer = tagList(
            modalButton("Dismiss")
          ),
          size = "s"))


    }) %>% # end of observe, then:
      bindEvent(input$restore_state)

  })
}

## To be copied in the UI
# mod_tab_about_ui("tab_about_1")

## To be copied in the server
# mod_tab_about_server("tab_about_1")
