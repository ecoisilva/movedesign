#' comp_out UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comp_out_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' comp_out Server Functions
#'
#' @noRd
mod_comp_out_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_comp_out_ui("comp_out_1")

## To be copied in the server
# mod_comp_out_server("comp_out_1")
