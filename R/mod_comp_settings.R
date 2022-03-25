#' comp_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comp_settings_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' comp_settings Server Functions
#'
#' @noRd
mod_comp_settings_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_comp_settings_ui("comp_settings_1")

## To be copied in the server
# mod_comp_settings_server("comp_settings_1")
