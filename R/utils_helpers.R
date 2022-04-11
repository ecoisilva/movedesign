
#' Highlight tab titles
#'
#' @description Highlight tab titles for workflows.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
highlight_title <- function(title) {

  span(title,
       style = paste("font-size: 16px;",
                     "font-family: Anek Tamil;",
                     "font-weight: 700;",
                     "padding-left: 5px;"))
}

#' Highlight tab icons
#'
#' @description Highlight tab icons for workflows.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
highlight_icon <- function(name) {

  fontawesome::fa(name = name, height = "17px")
}

#' Parameter blocks
#'
#' @description Display parameters.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
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
#' @keywords internal
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

    if(as.numeric(header) <= 5) {
      numberColor <- paste0("color: ", hex_caution, "!important;")
      if(numberIcon) { numberIcon <- icon("angle-double-down")
      } else { numberIcon <- HTML("&nbsp;") }
    } else { if(as.numeric(header) >= 30) {
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
#'
#' @noRd
reset_data_values <- function(vals) {

  vals$is_valid <- FALSE

  if(!is.null(vals$id)) vals$id <- NULL
  if(!is.null(vals$data_type)) vals$data_type <- NULL
  if(!is.null(vals$data0)) vals$data0 <- NULL
  if(!is.null(vals$dataList)) vals$dataList <- NULL

  if(!is.null(vals$tmpid)) vals$tmpid <- NULL
  if(!is.null(vals$fit0)) vals$fit0 <- NULL

}

#' Coerce telemetry object to list
#'
#' @description Coerce telemetry object to list from ctmmweb
#' @keywords internal
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
#' @keywords internal
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


#' @title movedesign ggplot2 custom theme
#' @encoding UTF-8
#'
#' @description Custom ggplot2 theme for movedesign plot outputs.
#' @author Inu00EAs Silva \email{i.simoes-silva@@hzdr.de}
#' @keywords internal
#'
#' @param ft_size Base font size.
#' @noRd
theme_movedesign <- function(ft_size = 14) {
  font <- "Roboto Condensed" # assign font family

  ggplot2::theme_minimal() %+replace% # replace elements
    ggplot2::theme(

      text = ggplot2::element_text(family = font, size = ft_size),

      plot.title = ggplot2::element_text(
        face = "bold", size = ft_size + 2, vjust = 1.2, hjust = .5),
      plot.subtitle = ggplot2::element_text(
        color = "black", hjust = .5),
      plot.margin = ggplot2::unit(c(0.3, 0.3, 1, 0.3), "cm"),

      panel.grid.minor = ggplot2::element_line(colour = "#f7f7f7"),
      panel.grid.major = ggplot2::element_line(colour = "#f7f7f7"),

      axis.text.x = ggplot2::element_text(colour = "#878787"),
      axis.text.y = ggplot2::element_text(colour = "#878787"),
      axis.title.x = ggplot2::element_text(
        family = font, face = "bold", hjust = 1, vjust = -1),
      axis.title.y = ggplot2::element_text(
        family = font, face = "bold", angle = 90, vjust = 2))
}



#' Plot home range
#'
#' @description Plotting home range output from ctmm
#' @keywords internal
#'
#' @noRd
plotting_hr <- function(dat, ud, levels) {

  pol_ud_high <- ctmm::SpatialPolygonsDataFrame.UD(
    ud, level.UD = .95)@polygons[[3]] # upper

  if("95% high CI" %in% levels) {

    p1 <- ggiraph::geom_polygon_interactive(
      data = pol_ud_high,
      mapping = ggplot2::aes(x = long, y = lat,
                             group = group),
      fill = hex_caution,
      alpha = .3)
  }

  if("Estimate" %in% levels) {
    pol_ud <- ctmm::SpatialPolygonsDataFrame.UD(
      ud, level.UD = .95)@polygons[[2]] # estimate

    p2 <- ggiraph::geom_polygon_interactive(
      data = pol_ud,
      mapping = ggplot2::aes(x = long, y = lat,
                             group = group),
      fill = "#2c3b41",
      alpha = .3)
  }

  if("95% low CI" %in% levels) {
    pol_ud_low <- ctmm::SpatialPolygonsDataFrame.UD(
      ud, level.UD = .95)@polygons[[1]] # low

    p3 <- ggiraph::geom_polygon_interactive(
      data = pol_ud_low,
      mapping = ggplot2::aes(x = long, y = lat,
                             group = group),
      fill = hex_caution,
      alpha = .3)
  }

  ymin <- min(pol_ud_high@Polygons[[1]]@coords[,2])
  yrange <- range(pol_ud_high@Polygons[[1]]@coords[,2])

  tmp <- ymin - diff(yrange) * .2
  p <- ggplot2::ggplot() +
    ggiraph::geom_polygon_interactive(
      data = pol_ud_high,
      mapping = ggplot2::aes(x = long, y = lat,
                             group = group),
      fill = NA, alpha = 1) +

    ggplot2::geom_path(dat,
                       mapping = ggplot2::aes(
                         x = x, y = y),
                       color = hex_border, size = 0.2,
                       alpha = .4) +
    ggplot2::geom_point(dat,
                        mapping = ggplot2::aes(
                          x = x, y = y),
                        color = hex_border, size = 1.5) +

    { if("95% high CI" %in% levels) p1 } +
    { if("Estimate" %in% levels) p2 } +
    { if("95% low CI" %in% levels) p3 } +

    ggplot2::labs(x = "X coordinate",
                  y = "Y coordinate") +

    ggplot2::scale_x_continuous(
      labels = scales::comma) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(tmp, NA)) +

    # viridis::scale_color_viridis(
    #   name = "Tracking time:",
    #   option = "D", trans = "time",
    #   breaks = c(min(dat$timestamp),
    #              max(dat$timestamp)),
    #   labels = c("Start", "End")) +

    theme_movedesign() +

    ggplot2::guides(
      color = ggplot2::guide_colorbar(
        title.vjust = 1.02)) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = "Roboto Condensed"),

      legend.position = c(0.76, 0.08),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(
        size = 11, face = "bold.italic"),
      legend.key.height = ggplot2::unit(0.3, "cm"),
      legend.key.width = ggplot2::unit(0.6, "cm")
    )
}

#' Plot home range with simulated data
#'
#' @description Plotting home range output from ctmm with simulation.
#' @keywords internal
#'
#' @noRd
plotting_hrsim <- function(dat, datsim, ud, levels, show) {

  pol_ud_high <- ctmm::SpatialPolygonsDataFrame.UD(
    ud, level.UD = .95)@polygons[[3]] # upper

  if("95% high CI" %in% levels) {

    p1 <- ggiraph::geom_polygon_interactive(
      data = pol_ud_high,
      mapping = ggplot2::aes(x = long, y = lat,
                             group = group),
      fill = hex_caution,
      alpha = .3)
  }

  if("Estimate" %in% levels) {
    pol_ud <- ctmm::SpatialPolygonsDataFrame.UD(
      ud, level.UD = .95)@polygons[[2]] # estimate

    p2 <- ggiraph::geom_polygon_interactive(
      data = pol_ud,
      mapping = ggplot2::aes(x = long, y = lat,
                             group = group),
      fill = "#617680", # "#2c3b41",
      alpha = .6)
  }

  if("95% low CI" %in% levels) {
    pol_ud_low <- ctmm::SpatialPolygonsDataFrame.UD(
      ud, level.UD = .95)@polygons[[1]] # low

    p3 <- ggiraph::geom_polygon_interactive(
      data = pol_ud_low,
      mapping = ggplot2::aes(x = long, y = lat,
                             group = group),
      fill = hex_caution,
      alpha = .3)
  }

  show_col <- ifelse(show, hex_border, "white")
  show_alpha <- ifelse(show, 1, 0)

  p <- ggplot2::ggplot() +
    ggiraph::geom_polygon_interactive(
      data = pol_ud_high,
      mapping = ggplot2::aes(x = long, y = lat,
                             group = group),
      fill = NA, alpha = 0) +

    ggplot2::geom_point(
      datsim, mapping = ggplot2::aes(x = x, y = y),
      color = hex_main, size = 1.5) +

    { if("95% high CI" %in% levels) p1 } +
    { if("Estimate" %in% levels) p2 } +
    { if("95% low CI" %in% levels) p3 } +

    ggplot2::geom_point(
      dat, mapping = ggplot2::aes(x = x, y = y),
      color = show_col, alpha = show_alpha, size = 2) +

    ggplot2::labs(x = "X coordinate",
                  y = "Y coordinate") +

    ggplot2::scale_x_continuous(
      labels = scales::comma) +
    ggplot2::scale_y_continuous(
      labels = scales::comma) +

    theme_movedesign() +
    ggplot2::theme(legend.position = "none")

}



#' Plot variogram
#'
#' @description Plot variogram from ctmm
#' @keywords internal
#'
#' @noRd
plotting_svf <- function(data) {

  p <- data %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = lag_days,
                   ymin = var_low95,
                   ymax = var_upp95),
      fill = "grey50",
      alpha = 0.25) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = lag_days,
                   ymin = var_low50,
                   ymax = var_upp50),
      fill = hex_caution,
      alpha = 0.25) +
    ggplot2::geom_line(
      ggplot2::aes(x = lag_days, y = SVF), size = 0.5) +
    ggplot2::labs(
      x = "Time lag (in days)",
      y = expression(bold("Semi-variance"~"("*km^{"2"}*")"))) +
    theme_movedesign()

  return(p)

}

