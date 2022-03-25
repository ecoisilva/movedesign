
#' Highlight tab titles
#'
#' @description Highlight tab titles for workflows.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
highlight_title <- function(title) {

  span(HTML("&nbsp;"), title,
       style = paste("font-size: 15px;",
                     "font-family: Fira Sans Extra Condensed;",
                     "font-weight: 700;"))
}

#' Highlight tab icons
#'
#' @description Highlight tab icons for workflows.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
highlight_icon <- function(name) {

  fontawesome::fa(name = name,
                  width = "15px")
}

#' Parameter blocks
#'
#' @description Display parameters.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
parBlock <- function(number = NULL,
                     header = NULL,
                     text = NULL,
                     icon = NULL) {

  cl <- "parblock"

  shiny::tags$div(
    class = cl,

    if (!is.null(icon)) {
      shiny::tags$span(
        class = "parblock-icon", icon(icon), br()) },
    shiny::tags$span(class = "parblock-text", text, br()),
    shiny::tags$span(class = "parblock-header", header),
    if(is.null(number)) { NULL } else {
      shiny::tags$span(class = "parblock-percentage",
                       br(), number) }
  )
}

#' Sample size blocks
#'
#' @description Display sample sizes.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
sampleBlock <- function(number = NULL,
                        numberIcon = FALSE,
                        header = NULL,
                        line1 = NULL,
                        line2 = NULL,
                        rightBorder = TRUE,
                        marginBottom = FALSE,
                        alt = FALSE) {

  cl <- "sampleblock"
  if (isTRUE(rightBorder))
    cl <- paste0(cl, " border-right")
  if (isTRUE(marginBottom))
    cl <- paste0(cl, " margin-bottom")
  numcl <- "samplebox-percentage"

  if (!is.null(header)) {

    if(header <= 5) {
      numberColor <- paste0("color: ", hex_caution, "!important;")
      if(numberIcon) { numberIcon <- icon("angle-double-down")
      } else { numberIcon <- HTML("&nbsp;") }
    } else { if(header >= 30) {
      numberColor <- paste0("color: ", hex_border, "!important;")
      header <- scales::label_comma(accuracy = 1)(header)
      if(numberIcon) { numberIcon <- icon("angle-down")
      } else { numberIcon <- HTML("&nbsp;") }
    } else {
      numberColor <- paste0("color: ", hex_gold, "!important;")
      if(numberIcon) { numberIcon <- icon("angle-down")
      } else { numberIcon <- HTML("&nbsp;") }
    }}
  }

  shiny::tags$div(
    class = cl,

    shiny::tags$span(
      class = numcl, number,
      if (!is.null(numberIcon)) numberIcon, br(),
      style = numberColor),
    shiny::tags$span(class = "sampleblock-header", header,
                     style = numberColor),
    shiny::tags$span(class = "sampleblock-text", br(), line1),
    shiny::tags$span(class = "sampleblock-text", br(), line2))
}

#' Relative error blocks
#'
#' @description Display relative errors.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
errorBlock <- function(icon = NULL,
                       text = NULL,
                       value = NULL,
                       min = NULL,
                       max = NULL,
                       rightBorder = FALSE) {

  cl <- "errorblock"
  if (isTRUE(rightBorder)) cl <- paste0(cl, " border-right")

  min <- ifelse((min * 100) %% 1 == 0,
                scales::label_comma(accuracy = 1)(min * 100),
                scales::label_comma(accuracy = .1)(min * 100))

  max <- ifelse((max * 100) %% 1 == 0,
                scales::label_comma(accuracy = 1)(max * 100),
                scales::label_comma(accuracy = .1)(max * 100))

  range <- paste0(min, "% — ", max, "%")

  if (!is.null(value)) {
    if(value > 0) {
      tmptext <- span("Overestimation", icon("angle-up"))
    } else {
      tmptext <- span("Underestimation", icon("angle-down"))
    }

    if(value >= 0.8) {
      numberColor <- paste0("color: ", hex_caution, "!important;") }
    if(value < 0.2 || value < 0.8) {
      numberColor <- paste0("color: ", hex_gold, "!important;") }
    if(value <= 0.2) {
      numberColor <- paste0("color: ", hex_border, "!important;")
    }
  }

  value <- round(value * 100, 1)
  shiny::tags$div(
    class = "errorblock",

    if (!is.null(icon)) {
      shiny::tags$span(
        class = "errorblock-icon", icon(icon), br())},
    shiny::tags$span(class = "errorblock-text", text, br()),
    shiny::tags$span(class = "errorblock-percentage",
                     tmptext, br(), style = numberColor),
    shiny::tags$span(class = "errorblock-header",
                     span(HTML(paste0(
                       HTML("&nbsp;"), value, "%"))),
                     style = numberColor),
    shiny::tags$span(class = "errorblock-percentage", br(), range,
                     style = numberColor)
  )
}

#' Extract units.
#'
#' @description Extracting units from ctmm summaries.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
extract_units <- function(input) {
  gsub("\\(([^()]+)\\)", "\\1",
       stringr::str_extract_all(input, "\\(([^()]+)\\)")[[1]])
}

#' Add helper text.
#'
#' @description Add helper text to inputs.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
help_text <- function(title, subtitle, content) {
  shiny::fluidRow(
    title, style = "margin-bottom: -14px;",

    bsplus::shiny_iconlink(
      name = "info-circle",
      class = "icon_help") %>%
      bsplus::bs_embed_popover(
        title = subtitle,
        content = content,
        placement = "bottom")
  )
}

#' Add helper tip.
#'
#' @description Add helper tip to inputs.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
help_tip <- function(input, text, placement = "bottom") {
  bsplus::shinyInput_label_embed(
    input,
    bsplus::shiny_iconlink(
      name = "info-circle",
      class = "icon_help") %>%
      bsplus::bs_embed_tooltip(
        title = text, placement = placement))
}

#' Create message logs
#'
#' @description Create message logs to show throughout app run.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
msg_log <- function(message, detail, style) {

  time_stamp <- stringr::str_c(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "]")

  if(style == "success") {
    line1 <- msg_success("\u2713")
    line2 <- crayon::bold(msg_success("Success:")) }

  if(style == "warning") {
    line1 <- msg_warning("!")
    line2 <- NULL }

  if(style == "danger") {
    line1 <- msg_danger("!")
    line2 <- crayon::bold(msg_danger("Warning:")) }

  if(style == "error") {
    line1 <- crayon::bold(msg_danger("\u2716"))
    line2 <- crayon::bold(msg_danger("Error:")) }

  cat(msg_main(time_stamp), "\n",
      ' ', line1,
      line2, message, "\n",
      ' ', msg_main(detail), "\n")
}

#' Create message steps
#'
#' @description Create message steps
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
msg_step <- function(current, total, style) {

  if(style == "success") {
    text_current <- msg_success(current) }

  if(style == "warning") {
    text_current <- msg_warning(current) }

  if(style == "danger") {
    text_current <- msg_danger(current)  }

  if(style == "error") {
    text_current <- crayon::bold(msg_danger(current)) }

  return(paste0(" (step ", text_current, " out of ", total, ")."))
}

#' Reset reactive values
#'
#' @description Reset reactive values
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
reset_data_values <- function(vals) {

  vals$id <- NULL
  vals$data_type <- NULL
  vals$data <- vals$dataList <- NULL
  vals$fit <- vals$sum <- NULL

}

#' Coerce telemetry object to list
#'
#' @description Coerce telemetry object to list from ctmmweb
#'
#' @noRd
as_tele_list <- function(tele) {
  if (class(tele) != "list") {
    # use same name so we can return same name if no change made
    tele_list <- list(tele)
    names(tele_list) <- attr(tele_list[[1]],"info")$identity
    return(tele_list)
  } else {
    return(tele)
  }
}

#' Add help modal
#'
#' @description Add help modal to inputs
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
help_modal <- function(input, file) {
  bsplus::shinyInput_label_embed(
    input,
    bsplus::shiny_iconlink(
      name = "question-circle",
      class = "icon_help") %>%
      bsplus::bs_attach_modal(
        id_modal = file))
}


#' Modal for tau p
#'
#' @noRd
#
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

#' Modal for tau v
#'
#' @noRd
#
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

#' Modal for sigma
#'
#' @noRd
#
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

#' Modal for data loss
#'
#' @noRd
#
modal_dataloss <- bsplus::bs_modal(
  id = "modal_dataloss",
  title = h4(span("Missing data", style = col_border),
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


