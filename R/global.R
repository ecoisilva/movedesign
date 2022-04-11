
utils::globalVariables(unique(c(
    # mod_comp_viz_server :
    "latitude",
    "longitude",
    "time",
    "x",
    "y",
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

app_version <- "Version 0.0.1"
contact_email <- "i.simoes-silva@hzdr.de"

link_github_package <- "https://github.com/ecoisilva/movedesign"
link_issues <- "https://github.com/ecoisilva/movedesign/issues"
mainlink_ctmm <- "https://ctmm-initiative.github.io/ctmm/"
mainlink_casus <- "https://www.casus.science/"
address_casus <- paste(
  "Center for Advanced Systems Understanding (CASUS),",
  "Helmholtz-Zentrum Dresden-Rossendorf e.V. (HZDR),",
  "Untermarkt 20, 02826, G\u00F6rlitz — Germany")

# Application layout: -----------------------------------------------------

div_column_main <- "col-xs-12 col-sm-12 col-md-12 col-lg-12"
div_column_half <- "col-xs-6 col-sm-6 col-md-6 col-lg-6"

div1_column_right <- "col-xs-12 col-sm-4 col-md-4 col-lg-3"
div1_column_left <- "col-xs-12 col-sm-8 col-md-8 col-lg-9"

div2_column_right <- "col-xs-12 col-sm-5 col-md-4 col-lg-3"
div2_column_left <- "col-xs-12 col-sm-7 col-md-8 col-lg-9"

# Colors: -----------------------------------------------------------------

hex_main <- "#222d32"
hex_item <- "#008d90"
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
col_item <- paste0("color: ", hex_item, ";")
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
                         col_main, "font-size: 16px;")

# Button colors for text formatting: --------------------------------------

btn_primary <- paste(ft, col_main)
btn_danger <- paste(ft, col_caution)

# Tour: -------------------------------------------------------------------

ttl_tour <- paste(ft, ft_bold, ft_center, "font-size: 32px;")

txt_tour <- paste0("font-family: Anek Tamil;",
                   "font-size: 17px;",
                   "line-height: 1.5;",
                   "letter-spacing: 0.2px;")

txt_tour_italic <- paste0(txt_tour, "font-style: italic;")
txt_tour_border <- paste0(txt_tour, col_border)
txt_tour_caution <- paste0(txt_tour, col_caution)
txt_tour_white <- paste0(txt_tour, "color: white;")
txt_tour_grey <- paste0(txt_tour, col_grey)
txt_tour_item <- paste0(txt_tour, col_item)

txt_action <- paste0(txt_tour,
                     "text-align: justify;",
                     "color: black;",
                     "background: #fff;") # "#0f5254"

txt_step <- paste0("font-family: Roboto Condensed, sans-serif;")

txt_output <- paste0("font-family: Fira Mono;",
                     "font-size: 15px;")

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


# Modals: -----------------------------------------------------------------

modal_tau_p0 <- bsplus::bs_modal(
  id = "modal_tau_p0",
  title = shiny::h4(span("Position autocorrelation",
                  style = col_border),
             "timescale:"),

  body = fluidRow(
    style = paste("margin-right: 20px;",
                  "margin-left: 20px;"),

    p("The", span("position autocorrelation", style = txt_border),
      "timescale", HTML(paste0("(\u03C4", tags$sub("p"), ")")),
      "is the", HTML(paste0(span("home range crossing time",
                                 style = txt_border), "."))),
    p(span("What does this mean?",
           style = paste(ft, ft_bold, ft_center, col_main)),
      "The", span("home range crossing time", style = txt_border),
      "is the time is takes (on average) for an animal to cross",
      "the linear extent of its home range. As",
      HTML(paste0("\u03C4", tags$sub("p"))),
      "increases, we can expect an animal to take longer to travel",
      "this linear extent. For example:"
    ),

    column(
      width = 12,
      shiny::img(src = "www/explain_taup.gif",
                 width = "100%", align = "center")),
    p(HTML('&nbsp;')),

    p("Typically, the",
      span("sampling duration",  style = txt_caution),
      "needs to be at least as long as the home range crossing time",
      "(if not many times longer) for",
      span("home range", style = txt_key), "estimation."
    )

  ), size = "medium")

modal_tau_v0 <- bsplus::bs_modal(
  id = "modal_tau_v0",
  title = shiny::h4(span("Velocity autocorrelation",
                         style = col_border),
                    "timescale:"),

  body = fluidRow(
    style = paste("margin-right: 20px;",
                  "margin-left: 20px;"),

    p("The", span("velocity autocorrelation", style = txt_border),
      "timescale", HTML(paste0("(\u03C4", tags$sub("v"), ")")),
      "is the", HTML(paste0(span("directional persistence",
                                 style = txt_border), "."))),
    p("Animals with strong", span("directional persistence",
                                  style = txt_border),
      "(ballistic or more linear movement bursts), will tend to have",
      "a", span("long", style = col_main),
      HTML(paste0("\u03C4", tags$sub("v"))), "parameter.",
      "On the other hand, animals with more tortuous",
      "movement (less linear), will tend to have a much",
      span("shorter", style = col_main),
      HTML(paste0("\u03C4", tags$sub("v"), " parameter.")),
      "For example:"
    ),

    p(HTML('&nbsp;')),
    column(
      width = 12,
      shiny::img(src = "www/explain_tauv.gif",
                 width = "100%", align = "center")),
    p(HTML('&nbsp;')),

    p("Typically, the",
      span("sampling interval", HTML("(\u0394t)"),
           style = txt_caution),
      "needs to be at least as long as the",
      span("velocity autocorrelation", style = txt_border),
      "timescale for", span("distance/speed traveled",
                            style = txt_key), "estimation.",
      "If", span(HTML("\u0394t"), style = txt_caution), ">",
      HTML(paste0("3\u03C4", tags$sub("v"))), "then no",
      "statistically significant signature of the animal's",
      "velocity will remain in the tracking dataset."
    )

  ), size = "medium")

modal_sigma0 <- bsplus::bs_modal(
  id = "modal_sigma0",
  title = shiny::h4(span("Semi-variance", style = col_border),
                    "parameter:"),

  body = fluidRow(
    style = paste("margin-right: 20px;",
                  "margin-left: 20px;"),

    p("The", span("semi-variance", style = txt_border),
      "parameter", HTML("(\u03C3)"), "is the",
      "the average square distance observed",
      "at two different times,",
      "and ultimately measures the spatial variability",
      "between any two locations."
    ),

    p("We are simulating an",
      span("isotropic", style = txt_key), "movement process,",
      "so", HTML("\u03C3"),
      "is the same in both the x and the y directions,",
      "resulting in a circular", span("home range", style = txt_key),
      "area."
    ),

    p("As we are also modeling",
      span("range resident", style = txt_key),
      "individuals (with a tendency to remain within their",
      "home range),", HTML("\u03C3"), "is asymptotic:",
      "if the", span("sampling duration", style = txt_caution),
      "is sufficient, the average square distance between any two",
      "locations will be equal to the chosen",
      HTML("\u03C3"), "value."
    )

  ), size = "medium")

modal_dataloss <- bsplus::bs_modal(
  id = "modal_dataloss",
  title = shiny::h4(span("Missing data", style = col_border),
                    "bias:"),

  body = fluidRow(
    style = paste("margin-right: 20px;",
                  "margin-left: 20px;"),

    p("Many real-world issues can lead to animal locations",
      "being sampled", span("irregularly", style = col_caution),
      "in time: duty-cycling tags to avoid wasting battery",
      "during periods of inactivity, device malfunctions,",
      "habitat-related signal loss, and many others.",
      "Ultimately, missing data equate to",
      "a loss of", span("information.", style = txt_key)),

  ), size = "medium")
