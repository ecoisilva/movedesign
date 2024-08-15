options(shiny.maxRequestSize = 30 * 1024^2) # 30 MB

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
    
    "id",
    "x", "y",
    "x0", "y0", "x1", "y1",
    "long", "lat",
    "longitude", "latitude",
    "time", "timestamp", "lag",
    
    "device",
    "duration", "dur",
    "interval", "dti", "dti_notes",
    
    "value", "low", "high",
    "est", "lci", "uci",
    "error", "error_lci", "error_uci",
    "CI_low", "CI_high",
    
    "svf",
    "svf_lower",
    "svf_upper",
    "svf_low50",
    "svf_upp50",
    
    "par_modal",
    
    "type",
    "variable",
    
    "n",
    "m",
    "subpop",
    "overlaps",
    
    "var_color",
    
    "buffalo",
    "coati",
    "pelican",
    "jaguar",
    "wolf",
    "gazelle",
    "turtle")
)
