
utils::globalVariables(unique(c(

  # as_tele_dt:
  ".I", "id", "row_name", "row_no",
  # mod_tab_data_select_server : <anonymous>:
  "dat0",
  # mod_tab_device_server : <anonymous>:
  "dur_new", "gps_fixrate", "lm", "ppm", "ppm_notes",
  # mod_tab_report_server : <anonymous>:
  "error_lci", "error_uci", "sims_hrange", "taup", "var",

  # mod_comp_viz_server :
  "x", "y", "latitude", "longitude",
  "time",
  # mod_tab_ctsd_server :
  "freq",
  "high",
  "x",
  "y",
  "timestamp",
  "t_new",
  "low",
  # mod_tab_data_select_server :
  "name_short",
  "buffalo",
  "coati",
  "gazelle",
  "jaguar",
  "pelican",
  "turtle",
  "wolf",
  # mod_tab_device_server :
  "choices",
  "color",
  "DOF_area",
  "DOF_speed",
  "duration",
  "freq_hrs",
  "frequency",
  "nu_notes",
  "timestamp",
  "x",
  "y",
  # mod_tab_hrange_server :
  "bias",
  "duration",
  "error",
  "freq",
  "mean.error",
  "method",
  "n.error",
  "sd.error",
  "se.error",
  # mod_tab_sims_server :
  "timestamp",
  "x",
  "y",
  # plotting_hr:
  "group",
  "lat",
  "long",
  "x",
  "y",
  # plotting_hrsim:
  "group",
  "lat",
  "long",
  "x",
  "y",
  # plotting_svf:
  "lag_days",
  "SVF",
  "var_low50",
  "var_low95",
  "var_upp50",
  "var_upp95",
  # prepare_svf:
  "lag",
  # simulate_gpsdecay:
  "freq_hrs",
  "highlight",
  "nu",
  "nu_notes"
)))

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
  "Wood turtle" = "turtle")

ctmm_species_binom <- c(
  "Syncerus caffer" = "buffalo",
  "Pelecanus occidentalis" = "pelican",
  "Nasua narica" = "coati",
  "Panthera onca" = "jaguar",
  "Chrysocyon brachyurus" = "wolf",
  "Procapra gutturosa" = "gazelle",
  "Glyptemys insculpta" = "turtle")
