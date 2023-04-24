
# .onLoad <- function(libname, pkgname) {
#   rlang::run_on_load()
# }
# 
# rlang::on_load(rlang::local_use_cli(inline = TRUE))
# 
# .onAttach <- function(libname, pkgname) {
#   version <- utils::packageDescription(pkgname, fields = "Version")
# 
#   website <- "https://github.com/ecoisilva/movedesign/issues"
#   if (interactive())
#     website <- "{.url https://github.com/ecoisilva/movedesign/issues}"
#   
#   rlang::inform(
#     class = "packageStartupMessage",
#     message = c(
#       "!" = paste("Issues?", website),
#       "i" = 'Citation: \tFor citation details, run: {.code citation("movedesign")}',
#       "x" = "Silence? {.code suppressPackageStartupMessages(library(movedesign))}"
#     )
#   )
# }

# Global variables:
utils::globalVariables(
  c(".data", "group",
    
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

# Application layout:
div_column_main <- "col-xs-12 col-sm-12 col-md-12 col-lg-12"
div_column_half <- "col-xs-6 col-sm-6 col-md-6 col-lg-6"

div_column_left <- "col-xs-12 col-sm-4 col-md-4 col-lg-3"
div_column_right <- "col-xs-12 col-sm-8 col-md-8 col-lg-9"

# Message types:
msg_main <- crayon::make_style("dimgray")
msg_success <- crayon::make_style("#009da0")
msg_danger <- crayon::make_style("#dd4b39")
msg_warning <- crayon::make_style("#ffbf00")