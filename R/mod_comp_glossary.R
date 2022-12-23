#' comp_glossary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comp_glossary_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' comp_glossary Server Functions
#'
#' @noRd
mod_comp_glossary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_comp_glossary_ui("comp_glossary_1")

## To be copied in the server
# mod_comp_glossary_server("comp_glossary_1")
