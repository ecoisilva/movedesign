#' comp_alerts UI Function
#'
#' @description Module for all the main alert messages.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_comp_alerts_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' comp_alerts Server Functions
#'
#' @noRd
mod_comp_alerts_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # ALERTS --------------------------------------------------------------
    
    ## If no groups available after sims tab:
    
    observe({
      req(rv$datList,
          rv$active_tab == 'device',
          rv$which_meta == "compare",
          rv$data_type == "simulated")
      
      if (length(rv$tau_p) == 1)
        shinyalert::shinyalert(
          type = "error",
          title = "No groups found",
          text = tagList(span(
            "No groups were set, or one of the groups is blank.",
            "Go to the", fontawesome::fa("file-pen", fill = pal$sea),
            span("Simulate data", class = "cl-sea"),
            "tab to fix, by selecting two sets of parameters from",
            "the table."
          )),
          html = TRUE,
          size = "xs")
      
    }) # end of observe
    
    ## If no initial data uploaded, selected or simulated:
    
    observe({
      req(rv$active_tab == 'hr' || rv$active_tab == 'ctsd')
      
      if (is.null(rv$datList) && is.null(rv$species))
        shinyalert::shinyalert(
          type = "error",
          title = "No data found",
          text = tagList(span(
            "Please upload, select or simulate a",
            span("movement dataset", class = "cl-dgr"),
            "first in the",
            icon("paw", class = "cl-mdn"),
            span("Species", class = "cl-mdn"), "tabs."
          )),
          html = TRUE,
          size = "xs")
      
    }) # end of observe
    
    # If sampling design and analyses don't match:
    
    observe({
      req(rv$active_tab == 'report')
      
      if (!is.null(rv$is_analyses)) {
        if (!rv$is_analyses) 
          shinyalert::shinyalert(
            type = "error",
            title = "Regime does not match analyses",
            text = tagList(span(
              "You have changed the regime without re-running",
              "estimations. Please go back to the",
              icon("compass-drafting", class = "cl-mdn"),
              span("Analyses", class = "cl-mdn"), "tab",
              "and make sure to click the",
              icon("paper-plane", class = "cl-mdn"),
              span("'Run estimation'", class = "cl-mdn"), "button."
            )),
            html = TRUE,
            size = "s")
      }
      
    }) # end of observer
 
    
    # If deployment type not set:
    
    observe({
      req(rv$active_tab == 'hr' || rv$active_tab == 'ctsd',
          rv$which_question, rv$which_meta, rv$simList)
      
      if (is.null(rv$which_m) && rv$which_meta != "none") {
        shinyalert::shinyalert(
          type = "error",
          title = "Deployment type unavailable",
          text = tagList(span(
            "You have not specified the deployment type",
            "in the", icon("house", class = "cl-blk"),
            span("Home", class = "cl-blk"), "tab.",
            " Please set either a fixed or",
            "minimum number of tags before clicking the", 
            icon("bolt", class = "cl-sea"),
            span("'Simulate'", class = "cl-sea"), "button."
          )),
          html = TRUE,
          size = "s")
      }
      
    }) # end of observer
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_comp_alerts_ui("comp_alerts_1")
    
## To be copied in the server
# mod_comp_alerts_server("comp_alerts_1")
