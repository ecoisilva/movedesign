#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    title = "movedesign",
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      # gps_fixrate = movedesign::gps_fixrate,
      # sims_hrange = movedesign::sims_hrange,
      # sims_speed = movedesign::sims_speed
      gps_fixrate = data(gps_fixrate, package = "movedesign"),
      sims_hrange = data(sims_hrange, package = "movedesign"),
      sims_speed = data(sims_speed, package = "movedesign")
    ) # end of list
  )
}
