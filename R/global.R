
.onLoad <- function(libname, pkgname) {
  rlang::run_on_load()
}

rlang::on_load(rlang::local_use_cli(inline = TRUE))

.onAttach <- function(libname, pkgname) {
  version <- utils::packageDescription(pkgname, fields = "Version")

  website <- "https://github.com/ecoisilva/movedesign/issues"
  if (interactive())
    website <- "{.url https://github.com/ecoisilva/movedesign/issues}"

  rlang::inform(
    class = "packageStartupMessage",
    message = c(
      "!" = paste("Issues?", website),
      "x" = "Silence? {.code suppressPackageStartupMessages(library(movedesign))}",
      "i" = 'For citation details, run: {.code citation("movedesign")}'
    )
  )
}

# Global variables:
utils::globalVariables(
  c(".data", "group", "seed",
    
    "x", "y",
    "long", "lat",
    "longitude", "latitude",
    "time", "timestamp", "lag",
    
    "duration", "dur",
    "interval", "dti", "dti_notes",
    
    "error", "error_lci", "error_uci",
    "CI_low", "CI_high",
    
    "par_modal",
    
    "buffalo",
    "coati",
    "pelican",
    "jaguar",
    "wolf",
    "gazelle",
    "turtle")
)
