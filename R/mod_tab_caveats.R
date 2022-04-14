#' tab_caveats UI Function
#'
#' @description A shiny Module for device and design limitations.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_caveats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(

      # Introduction: -----------------------------------------------------

      div(class = div_column_main,

          shinydashboardPlus::box(

            title = span("Caveats \u0026 limitations:",
                         style = paste("padding-top: 14px;", ttl_main)),
            icon = fontawesome::fa(name = "bomb",
                                   height = "22px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "#e3e3e3"),
            id = ns("caveats_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = TRUE, closable = FALSE,

            column(
              align = "center", width = 12,

              p("Weight limits imposed on tracking devices",
                "(e.g., 5% rule) also impact battery life."),

              p(style = txt_label_bold,
                "Do you want to consider weight limitations?"),

              shinyWidgets::switchInput(
                inputId = ns("weight"),
                onLabel = "Yes",
                offLabel = "No",
                labelWidth = "25px"),
              br(),

              p(style = txt_label_bold,
                "Do you want to consider a maximum",
                "number of stored locations?"),

              shinyWidgets::switchInput(
                inputId = ns("maxlocs"),
                onLabel = "Yes",
                offLabel = "No",
                labelWidth = "25px"),
              br()


            ) # end of column (text)
          ) # end of box // select_intro
      ), # end of div (top row)

      # [right column] ----------------------------------------------------

      div(class = div1_column_right,

          shinydashboardPlus::box(
            id = ns("caveatBox_loss"),
            width = NULL,
            headerBorder = FALSE,

            shinyWidgets::sliderTextInput(
              inputId = ns("caveatInput_loss"),
              label = "Simulate % data loss:",
              choices = c(0, 5, seq(10, 100, by = 10)),
              from_min = 0, from_max = 90, selected = 0,
              grid = FALSE,
              post = "%",
              width = "100%") %>%
              help_modal(file = "modal_dataloss"),

            br(),
            uiOutput(ns("caveatBlock_loss"))

          ), # end of box // regBox_loss

      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(class = div1_column_left,

          shinydashboardPlus::box(
            title = span("More settings", style = ttl_box.solid),
            id = ns("caveatBox_device"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            shiny::numericInput(
              inputId = ns("device_error"),
              label = "Measurement error (in meters):",
              min = 1, value = 15)

          ) # end of box // caveatBox_device

      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          # Additional information: ---------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", style = ttl_box),
            id = ns("caveatBox_misc"),
            width = NULL, solidHeader = FALSE,

            NULL

          ) # end of box // caveatBox_misc
      ) # end of column (bottom)

    ) # end of fluidRow
  ) # end of tagList
}

#' tab_caveats Server Functions
#'
#' @noRd
mod_tab_caveats_server <-  function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_caveats_ui("tab_caveats_1")

## To be copied in the server
# mod_tab_caveats_server("tab_caveats_1")
