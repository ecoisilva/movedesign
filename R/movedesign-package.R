#' @keywords internal
"_PACKAGE"

utils::globalVariables(
  c(
    ".data", "group",
    "x", "y",
    "long", "lat",
    "longitude", "latitude",
    "time", "timestamp", "lag",
    "duration", "dur",
    "interval", "dti", "dti_notes",
    "error", "error_lci", "error_uci",
    "CI_low", "CI_high",
    "buffalo",
    "coati",
    "pelican",
    "jaguar",
    "wolf",
    "gazelle",
    "turtle"
  )
)

## usethis namespace: start
#' @importFrom ctmm %#%
#' @importFrom dplyr %>%
#' @importFrom ggplot2 %+replace%
#' @importFrom stats median
#' @importFrom utils data
#' @importFrom utils packageVersion
## usethis namespace: end
NULL
