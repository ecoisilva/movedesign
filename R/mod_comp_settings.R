#' comp_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comp_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12, align = "center",
      shiny::p(),

      # tags$div(htmltools::tagAppendAttributes(
      #   actionButton(
      #     inputId = "fullscreen",
      #     label = "Fullscreen",
      #     width = "100%",
      #     icon = icon("expand")
      #   ),
      #   onclick = "shinyjs.toggleFullScreen();")),
      # p(),
      # p(style = "text-align: justify; color: #ffffff;",
      #   "(Only works in browser)."),
      #
      # tags$hr(style = "border-color: #2c3b41;"),

      shiny::downloadButton(
        outputId = ns("download_settings"),
        label = "Save settings",
        icon = shiny::icon("download"),
        style = "width: 100%"
      ),
      p(),
      uiOutput(ns("text_save")),

      # tags$hr(style = "border-color: #2c3b41;"),
      # h4("Language:"),
      # uiOutput(ns("menu_language")),

      # selectInput(inputId = ns("lang_select"),
      #             label = NULL,
      #             choices = languages,
      #             selected = "EN",
      #             width = "80px",
      #             selectize = T),

      tags$hr(style = "border-color: #2c3b41;"),
      shinyWidgets::prettyCheckbox(
        inputId = ns("parallel"),
        label = span("Parallel mode",
          style = paste0(
            "color: #ffffff;",
            "font-size: 15px;",
            "letter-spacing: 0.5px;"
          )
        ),
        value = TRUE
      ),
      br()
    ) # end of column
  ) # end of tagList
}

#' comp_settings Server Functions
#'
#' @noRd
mod_comp_settings_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      vals$parallel <- input$parallel
    }) %>% bindEvent(input$parallel)

    observe({


    }) %>% bindEvent(input$lang_select)

    # DYNAMIC UI ELEMENTS -------------------------------------------------

    output$text_save <- renderUI({
      if (is.null(vals$data0)) {
        p(
          style = "text-align: justify; color: #ffffff;",
          "Return here after running any analyses to save",
          "all stored values to your local environment."
        )
      } else {
        p(
          style = "text-align: justify",
          "Save all stored values",
          "to your local environment."
        )
      }
    }) # end of output$text_save

    # SETTINGS ------------------------------------------------------------
    ## Save settings/values (for next session): ---------------------------

    output$download_settings <- downloadHandler(
      filename = function() {
        paste0("movedesign-settings_", Sys.Date(), ".rds")
      },
      content = function(file) {
        if (is.null(vals$data0)) {
          shiny::showNotification("No data to save",
            type = "error", duration = 8
          )
        } else {
          saveRDS(reactiveValuesToList(vals), file = file)
        }
      }
    ) # end of output$downloadHandler
  }) # end of moduleServer
}


## To be copied in the UI
# mod_comp_settings_ui("comp_settings_1")

## To be copied in the server
# mod_comp_settings_server("comp_settings_1")
