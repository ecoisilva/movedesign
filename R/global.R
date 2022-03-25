
# Main links and variables: -----------------------------------------------

tm_now <- Sys.time()
tm_today <- Sys.Date()
tm_today_full <- format(Sys.time(), "%d-%B-%Y")

app_version <- "Version 1.0.0"
contact_email <- "i.simoes-silva@hzdr.de"

link_github_package <- "https://github.com/ecoisilva/movedesign"
link_issues <- "https://github.com/ecoisilva/movedesign/issues"
mainlink_ctmm <- "https://ctmm-initiative.github.io/ctmm/"
mainlink_casus <- "https://www.casus.science/"
address_casus <- paste(
  "Center for Advanced Systems Understanding (CASUS),",
  "Helmholtz-Zentrum Dresden-Rossendorf e.V. (HZDR),",
  "Untermarkt 20, 02826, Görlitz — Germany")

# Font: -------------------------------------------------------------------

mainfont <- "Roboto Condensed"
grDevices::windowsFonts(mainfont = grDevices::windowsFont(mainfont))

# Application layout: -----------------------------------------------------

div_column_main <- "col-xs-12 col-sm-12 col-md-12 col-lg-12"
div_column_half <- "col-xs-6 col-sm-6 col-md-6 col-lg-6"

div1_column_right <- "col-xs-12 col-sm-4 col-md-4 col-lg-3"
div1_column_left <- "col-xs-12 col-sm-8 col-md-8 col-lg-9"

div2_column_right <- "col-xs-12 col-sm-5 col-md-4 col-lg-3"
div2_column_left <- "col-xs-12 col-sm-7 col-md-8 col-lg-9"

# Colors: -----------------------------------------------------------------

hex_main <- "#222d32"
hex_subitem <- "#2c3b41"
hex_border <- "#009da0"
hex_key <- "#006466"
hex_caution <- "#dd4b39"
hex_gold <- "#ffbf00"

msg_main <- crayon::make_style("dimgray")
msg_success <- crayon::make_style(hex_border)
msg_danger <- crayon::make_style(hex_caution)
msg_warning <- crayon::make_style(hex_gold)

col_main <- paste0("color: ", hex_main, ";")
col_border <- paste0("color: ", hex_border, ";")
col_key <- paste0("color: ", hex_key, ";")
col_caution <- paste0("color: ", hex_caution, "!important;")
col_gold <- paste0("color: ", hex_gold, ";")
col_grey <- paste0("color: #8aa4af;")
col_black <- paste0("color: black;")

# Titles and text formatting: ---------------------------------------------

ft <- "font-family: Roboto Condensed;"
ft_thin <- "font-weight: 300;"
ft_bold <- "font-weight: 500;"
ft_extrabold <- "font-weight: 700;"
ft_italic <- "font-style: italic;"
ft_center <- "text-align: center;"
ft_right <- "text-align: right;"

ttl_main <- paste0(ft, ft_bold, "font-size: 24px;")
ttl_box <- paste0(ft, col_key, "font-size: 18px;")
ttl_box.solid <- paste0(ft, "color: #fff;", "font-size: 18px;")

ttl_innerbox <- paste0(ft,
                       "letter-spacing: 0.5px;",
                       "font-size: 14px;")

ttl_sub <- paste0(ft, col_main, "font-size: 18px;")
ttl_panel <- paste0(ft, col_border, "font-size: 16px;")

txt_key <- paste0(ft_bold, col_key)
txt_caution <- paste0(ft_bold, col_caution)
txt_border <- paste0(ft_bold, col_border)
txt_gold <- paste0(ft_bold, col_gold)

txt_label <- paste0(ft, col_main,
                    "font-size: 14px;",
                    "letter-spacing: 0.5px;",
                    "margin: 0 0 5px -7px;")

txt_label_bold <- paste0(ft,
                         ft_center,
                         ft_extrabold,
                         col_main, "font-size: 14px;")

# Button colors for text formatting: --------------------------------------

btn_primary <- paste(ft, col_main)
btn_danger <- paste(ft, col_caution)

# Tour: -------------------------------------------------------------------

ttl_tour <- paste(ft, ft_bold, ft_center, "font-size: 28px;")
txt_tour <- paste0(ft,
                   "font-size: 15px;",
                   "line-height: 1.5;",
                   "letter-spacing: 0.2px;")
txt_action <- paste0(ft, col_caution,
                     "padding: .2rem .2rem;",
                     "background: #2c3b41;",
                     "font-size: 13px;",
                     "text-align: justify;",
                     "line-height: 1.5;",
                     "letter-spacing: 0.5px;",
                     "text-transform: uppercase;")

txt_step <- paste0(ft, "color: black;",
                   "padding: .2rem .2rem .2rem .4rem;",
                   "background: #ffbf00;",
                   "font-size: 13px;",
                   "font-weight: 500;",
                   "text-align: justify;",
                   "line-height: 1.5;",
                   "letter-spacing: 0.5px;",
                   "text-transform: uppercase;")

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
