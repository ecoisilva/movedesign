
# Main links and variables: -----------------------------------------------

tm_now <- Sys.time()
tm_today <- Sys.Date()
tm_today_full <- format(Sys.time(), "%d-%B-%Y")

link_github_package <- "https://github.com/ecoisilva/movedesign"
link_issues <- "https://github.com/ecoisilva/movedesign/issues"
mainlink_ctmm <- "https://ctmm-initiative.github.io/ctmm/"

# Application layout: -----------------------------------------------------

div_column_main <- "col-xs-12 col-sm-12 col-md-12 col-lg-12"
div_column_half <- "col-xs-6 col-sm-6 col-md-6 col-lg-6"

div_column_left <- "col-xs-12 col-sm-4 col-md-4 col-lg-3"
div_column_right <- "col-xs-12 col-sm-8 col-md-8 col-lg-9"

# Colors: -----------------------------------------------------------------

msg_main <- crayon::make_style("dimgray")
msg_success <- crayon::make_style("#009da0")
msg_danger <- crayon::make_style("#dd4b39")
msg_warning <- crayon::make_style("#ffbf00")

# 'ctmm' R package, available species: ------------------------------------

ctmm_species <- c(
  "African Buffalo" = "buffalo",
  "Brown Pelican" = "pelican",
  "Coati" = "coati",
  "Jaguar" = "jaguar",
  "Maned Wolf" = "wolf",
  "Mongolian Gazelle" = "gazelle",
  "Wood turtle" = "turtle"
)

ctmm_species_binom <- c(
  "Syncerus caffer" = "buffalo",
  "Pelecanus occidentalis" = "pelican",
  "Nasua narica" = "coati",
  "Panthera onca" = "jaguar",
  "Chrysocyon brachyurus" = "wolf",
  "Procapra gutturosa" = "gazelle",
  "Glyptemys insculpta" = "turtle"
)
