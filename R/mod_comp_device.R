#' comp_device UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_comp_device_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  ) # end of tagList
}
    
#' comp_device Server Functions
#'
#' @noRd 
mod_comp_device_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_comp_device_ui("comp_device_1")
    
## To be copied in the server
# mod_comp_device_server("comp_device_1")
