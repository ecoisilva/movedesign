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
          img(src = "www/logo.png", height = "140px"),
          p(),

          p(span("Movement ecology", style = txt_key),
            "studies frequently make use of data collected from",
            HTML(paste0(span(
              "animal tracking projects", style = txt_key), ".")),
            "Planning a successful animal tracking project requires",
            "careful consideration and clear objectives. It is crucial",
            "to plan ahead and understand how much data is required",
            "to accurately answer your chosen research questions,",
            "and choose the optimal",
            HTML(paste0(span(
              "tracking regime or schedule", style = txt_key), ".")),
            br(),
            "This", a(href = 'http://shiny.rstudio.com', 'Shiny'),
            "application will assist researchers in designing",
            "movement ecology studies related to two",
            "main research questions: the estimation of",
            span("home range", style = txt_key), "and of",
            HTML(paste0(span("speed and distance traveled",
                             style = txt_key), "."))),

          # verbatimTextOutput(outputId = ns("warning_about")),
          tags$hr(),

          ## Workflow section: --------------------------------------------

          h3("What is your workflow?"),
          p(style = "padding: none;"),

          fluidRow(
            align = "center",

            ### Data source -----------------------------------------------

            shinyWidgets::radioGroupButtons(
              inputId = ns("which_data"),
              label = span("Data source",
                           style = paste("font-size: 16px;",
                                         "font-weight: 500;")),
              choices = c("Upload your own" = "Upload",
                          "Select from available species" = "Select",
                          "Simulate from scratch" = "Simulate"),

              selected = character(0),
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon"))),
              # checkIcon = list(
              #   yes = tags$i(class = "fa fa-check-square",
              #                style = "color: #009da0"),
              #   no = tags$i(class = "fa fa-square-o",
              #               style = "color: #dd4b39"))),

            ### Research question(s) --------------------------------------

            shinyWidgets::checkboxGroupButtons(
              inputId = ns("which_question"),
              label = span("Research question",
                           style = paste("font-size: 16px;",
                                         "font-weight: 500;")),
              choices = c("Home range",
                          "Distance/speed"),
              selected = NULL,
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square",
                             style = "color: #009da0"),
                no = tags$i(class = "fa fa-square-o",
                            style = "color: #dd4b39")))

          ), # end of fluidRow

          ## Tour/tutorial section: ---------------------------------------

          tags$hr(),

          h3("How does this application work?"),
          p(),
          mod_comp_tour_ui("tour_1"),
          p(),

          p(
            "To facilitate study design, we refer to the",
            a(href = mainlink_ctmm, 'ctmm'), 'R package.',

            "Animal movement is inherently",
            span("autocorrelated", style = txt_key),
            "(locations are similar as a function of space and",
            "distance), and the",
            a(href = mainlink_ctmm, 'ctmm'),
            "package allows us to model these data as",
            span("continuous-time stochastic processes",
                 style = txt_key), "and to deal",
            "with other known biases (such as small sample sizes,",
            "or irregular sampling schedules)."
          )

        ) # end of column (text)
      ), # end of box // intro

      # Miscellaneous: ----------------------------------------------------

      shinydashboardPlus::box(
        id = "about_misc",
        title = NULL,
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = FALSE, closable = FALSE,

        column(
          align = "center", width = 5,

          ## Restore application settings: --------------------------------

          p("Restore settings:", style =
              paste(ttl_sub, "text-align: left!important;")),

          p(
            "After using this app for the first time, you can save",
            "your progress (and any previously defined values) to",
            "your local environment, by accessing the",
            fontawesome::fa(name = "cogs", fill = hex_main),
            "symbol on the",
            HTML(paste0(span("upper right corner",
                             style = "color: black;"), "."))
          ),

          uiOutput(ns("saving_vals")),

          fileInput(ns("restore_state"),
                    label = "Upload file:",
                    accept = ".rds",
                    placeholder = ".rds file"),
          p(
            "Click", span("Browse...", style = "color: black;"),
            "and select a previously saved",
            span(".rds", style = col_border),
            "file to load stored values.")

        ), # end of column (right)

        ## Contact details: ---------------------------------------------

        column(
          align = "center", width = 7,

          p("Project contact:", style =
              paste(ttl_sub, "text-align: left!important;")),

          p(
            "Inês Silva,",
            span(contact_email, style = col_border),
            tags$hr()
          ),

          p("Institutional contact:", style =
              paste(ttl_sub, "text-align: left!important;")),

          p(
            style = "font-size: 14px",
            address_casus
          ),

          img(src = "www/logo_casus.png", height = "70px"),
          p(),
          a(href = mainlink_casus, mainlink_casus),
          p()

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

    # Values: -----------------------------------------------------------

    observe({ vals$which_data <- input$which_data }) %>%
      bindEvent(input$which_data)
    observe({ vals$which_question <- input$which_question }) %>%
      bindEvent(input$which_question)

    # Restore settings: -------------------------------------------------

    observe({

      validate(need(input$restore_state, message = FALSE))
      restored_vals <- readRDS(
        file = paste(input$restore_state$datapath))
      vals$restored_vals <- restored_vals

      output$tab1_console <- renderPrint({

        list(vals$restored_vals$"tau_p0"[1],
             vals$restored_vals$"tau_p0_units"[1])

      }) # end of renderPrint // tab1_console

    }) %>% # end of observe, then:
      bindEvent(input$restore_state)

    # ---------------------------------------------------------------------

    # output$warning_about <- renderText({
    #   paste(
    #     "All results or suggestions within this app are based on",
    #     "simulations only, and may not reflect real species or outputs.",
    #     "Please use any recommended tracking regimes with caution.")
    # })


  })
}

## To be copied in the UI
# mod_tab_about_ui("tab_about_1")

## To be copied in the server
# mod_tab_about_server("tab_about_1")
