
#' Run movedesign R Shiny application
#'
#' @param onStart A function called before the app runs. Only relevant for
#'   programmatic usage.
#' @param options A named list passed to `shiny::runApp`.
#' @param enableBookmarking One of `url`, `server`, or `disable`. The
#'   default, `NULL`, respects any previous call to `enableBookmarking`.
#' @param uiPattern A regular expression used to match request paths. The
#' request path must match the expression in full to be handled by the UI.
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#'
#' @return No return value. This function is called for its side effects.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' 
run_app <- function(
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
      fixrates = movedesign::fixrates
    )
  )
}

