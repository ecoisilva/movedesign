#' viz_meta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_viz_meta_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
        ggiraph::girafeOutput(
          outputId = ns("metaPlot_m_optimal"),
          width = "100%", height = "100%"))
    
  ) # end of tagList
}
    
#' viz_meta Server Functions
#'
#' @noRd
mod_viz_meta_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    output$metaPlot_m_optimal <- ggiraph::renderGirafe({
      req(rv$meta_tbl, 
          rv$which_m, 
          rv$which_meta,
          rv$which_question,
          rv$set_analysis)
      req(length(rv$simList) > 1)
      if (rv$random) {
        req(rv$error_threshold, 
            rv$meta_tbl_resample) }
      if (rv$which_meta == "compare") {
        req(rv$metaList_groups)
        req(rv$metaList[[rv$set_analysis]]) }
      
      p.optimal <- plot_meta_resamples(
        rv,
        set_target = rv$set_analysis,
        random = rv$random, 
        subpop = rv$grouped, 
        colors = c(pal$sea, 
                   # pal$grn,
                   pal$dgr))
      
      p.optimal <- p.optimal +
        theme_movedesign(font_available = rv$is_font) +
        ggplot2::theme(
          legend.position = "bottom",
          plot.title = ggtext::element_markdown(
            size = 14, hjust = 1, margin = ggplot2::margin(b = 15)))
      
      if (rv$which_meta == "mean") {
        p.optimal <- p.optimal +
          ggplot2::guides(shape = "none")
      }
      
      if (rv$which_meta == "mean") {
        p.optimal <- p.optimal +
          ggplot2::guides(shape = "none")
      }
      
      ggiraph::girafe(
        ggobj = p.optimal,
        width_svg = 5.5, height_svg = 4,
        options = list(
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_tooltip(
            opacity = 1,
            use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill: #1279BF;",
                        "stroke: #1279BF;",
                        "cursor: pointer;")))) %>%
        suppressWarnings()
      
    }) %>% # end of renderGirafe, "metaPlot_m_optimal"
      bindEvent(list(rv$run_meta,
                     rv$run_meta_resample,
                     rv$set_analysis,
                     rv$meta_nresample))
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_viz_meta_ui("viz_meta_1")
    
## To be copied in the server
# mod_viz_meta_server("viz_meta_1")
