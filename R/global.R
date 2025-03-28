
options(shiny.maxRequestSize = 500 * 1024^2) # 500 MB

.onLoad <- function(libname, pkgname) {
  rlang::run_on_load()
}

rlang::on_load(rlang::local_use_cli(inline = TRUE))

.onAttach <- function(libname, pkgname) {
  # version <- utils::packageDescription(pkgname, fields = "Version")
  website <- "{.url https://github.com/ecoisilva/movedesign/issues}"
  
  rlang::inform(
    class = "packageStartupMessage",
    message = c(
      "i" = 'To cite, run: {.code citation("movedesign")}',
      "!" = paste("Bugs?", website),
      "x" = "Silence? {.code suppressPackageStartupMessages(library(movedesign))}"
    )
  )
}


