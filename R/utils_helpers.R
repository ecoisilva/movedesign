
#' Parameter blocks
#'
#' @description Display parameters.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
parBlock <- function(icon = NULL,
                     header = NULL,
                     value = NULL,
                     subtitle = NULL) {

  cl <- "parblock"

  shiny::tags$div(
    class = cl,

    if (!is.null(icon)) {
      shiny::tags$span(
        class = paste0(cl, "-icon"), icon(icon), br()) },
    shiny::tags$span(class = paste0(cl, "-text"), header, br()),
    shiny::tags$span(class = paste0(cl, "-value"), value),
    if (is.null(subtitle)) { NULL } else {
      shiny::tags$span(class = paste0(cl, "-subtitle"),
                       br(), subtitle) }
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

    if (as.numeric(header) <= 5) {
      numberColor <- "color: var(--danger) !important;"
      if (numberIcon) { numberIcon <- icon("angles-down")
      } else { numberIcon <- HTML("&nbsp;") }
    } else { if (as.numeric(header) >= 30) {
      numberColor <- "color: var(--sea) !important;"
      header <- scales::label_comma(accuracy = 1)(header)
      if (numberIcon) { numberIcon <- icon("angle-down")
      } else { numberIcon <- HTML("&nbsp;") }
    } else {
      numberColor <- "color: var(--gold) !important;"
      if (numberIcon) { numberIcon <- icon("angle-down")
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

  range <- paste0(min, "% \u2014 ", max, "%")

  if (!is.null(value)) {
    if (value > 0) {
      tmptext <- span("Overestimation", icon("angle-up"))
    } else {
      tmptext <- span("Underestimation", icon("angle-down"))
    }

    if (abs(value) >= 0.8) {
      numberColor <- "color: var(--danger) !important;" }
    if (abs(value) < 0.2 || abs(value) < 0.8) {
      numberColor <- "color: var(--gold) !important;" }
    if (abs(value) <= 0.2) {
      numberColor <- "color: var(--sea) !important;" }
  }

  value <- sigdigits(value * 100, 2)

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
    shiny::tags$span(class = "errorblock-percentage", br(), range)
  )
}

#' Parameter blocks
#'
#' @description Display parameters.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
staticBlock <- function(text,
                        type = "logical",
                        active = FALSE) {

  if (type == "logical") {
    icon_T <- "square-check"
    icon_F <- "circle-xmark"
  }

  if (type == "maximum" | type == "max") {
    icon_T <- "less-than-equal"
    icon_F <- "circle-xmark"
  }
  
  if (type == "none") {
    icon_T <- "square-check"
    icon_F <- "square-check"
  }

  if (active) {
    cl <- "staticblock_active"
    icon <- icon(name = icon_T)
  } else {
    cl <- "staticblock"
    icon <- icon(name = icon_F)
  }

  shiny::tags$div(
    class = cl,

    tagList(
      shiny::tags$span(class = "staticblock-icon",
                       shiny::HTML("&nbsp;"), icon),
      shiny::tags$span(class = "staticblock-text",
                       text)
    )
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
  
  tryCatch(
    expr = {
      string <- gsub(
        "\\(([^()]+)\\)", "\\1",
        stringr::str_extract_all(input,
                                 "\\(([^()]+)\\)")[[1]])
      return(string)
    },
    error = function(e) {
      print(
        sprintf("An error occurred in extract_units at %s : %s",
                Sys.time(), e))
    })
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
      name = "circle-info",
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
      name = "circle-info",
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

  if (style == "success") {
    line1 <- msg_success("\u2713")
    line2 <- crayon::bold(msg_success("Success:")) }

  if (style == "warning") {
    line1 <- msg_warning("!")
    line2 <- NULL }

  if (style == "danger") {
    line1 <- msg_danger("!")
    line2 <- crayon::bold(msg_danger("Warning:")) }

  if (style == "error") {
    line1 <- crayon::bold(msg_danger("\u2716"))
    line2 <- crayon::bold(msg_danger("Error:")) }

  
  if(missing(detail)) {
    out <- cat(msg_main(time_stamp), "\n",
               ' ', line1,
               line2, message, "\n")
  } else {
    out <- cat(msg_main(time_stamp), "\n",
               ' ', line1,
               line2, message, "\n",
               ' ', msg_main(detail), "\n")
  }
  
  return(out)
}


#' Create main log
#'
#' @description Create message logs to show throughout app run.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
msg_header <- function(header) {

  time_stamp <- stringr::str_c(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "]")

  cat(msg_main(time_stamp), header, "\n")
}

#' Create message steps
#'
#' @description Create message steps
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
msg_step <- function(current, total, style) {

  if (style == "success") {
    text_current <- msg_success(current) }

  if (style == "warning") {
    text_current <- msg_warning(current) }

  if (style == "danger") {
    text_current <- msg_danger(current)  }

  if (style == "error") {
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

  if (!is.null(vals$id)) vals$id <- NULL
  if (!is.null(vals$data_type)) vals$data_type <- NULL
  if (!is.null(vals$data0)) vals$data0 <- NULL
  if (!is.null(vals$dataList)) vals$dataList <- NULL
  
  if (!is.null(vals$tmpid)) vals$tmpid <- NULL
  if (!is.null(vals$fit0)) vals$fit0 <- NULL
  
  if (!is.null(vals$hr)) vals$hr <- NULL
  if (!is.null(vals$sd)) vals$sd <- NULL
  
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
      name = "circle-question",
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
#' @importFrom ggplot2 %+replace%
#'
#' @param ft_size Base font size.
#' @noRd
theme_movedesign <- function(ft_size = 13) {
  font <- "Roboto Condensed" # assign font family

  ggplot2::theme_minimal() %+replace% # replace elements
    ggplot2::theme(

      text = ggplot2::element_text(family = font, size = ft_size),

      plot.title = ggplot2::element_text(
        size = ft_size + 3, vjust = 1.2, hjust = .5),
      plot.subtitle = ggplot2::element_text(
        color = "#666666", hjust = .5),
      plot.margin = ggplot2::unit(c(0.2, 0.2, 0.3, 0.2), "cm"),

      panel.grid.minor = ggplot2::element_line(colour = "#f7f7f7"),
      panel.grid.major = ggplot2::element_line(colour = "#f7f7f7"),

      axis.text.x = ggplot2::element_text(colour = "#878787"),
      axis.text.y = ggplot2::element_text(colour = "#878787"),
      axis.title.x = ggplot2::element_text(
        family = font, hjust = 1, vjust = -1),
      axis.title.y = ggplot2::element_text(
        family = font, angle = 90, vjust = 2))
}

#' Plot home range
#'
#' @description Plotting home range output from ctmm
#' @keywords internal
#'
#' @noRd
plotting_hr <- function(data, 
                        sigma,
                        show_truth,
                        ud, levels,
                        color, fill) {
  
  id <- NULL
  radius_x <- radius_y <- sqrt(-2 * log(0.05) * sigma)
  truth <- data.frame(
    id = rep(1, each = 100),
    angle = seq(0, 2 * pi,length.out = 100))
  
  truth$long <- unlist(lapply(
    mean(data$x), 
    function(x) x + radius_x * cos(truth$angle)))
  truth$lat <- unlist(lapply(
    mean(data$y), 
    function(x) x + radius_y * sin(truth$angle)))
  
  pol_ud_high <- ctmm::SpatialPolygonsDataFrame.UD(
    ud, level.UD = .95)@polygons[[3]] # upper
  
  if ("95% high CI" %in% levels) {
    
    p1 <- ggplot2::geom_polygon(
      data = pol_ud_high,
      mapping = ggplot2::aes(x = long,
                             y = lat,
                             group = group),
      fill = color, col = color,
      linetype = "dotted",
      alpha = .2)
  }
  
  if ("Estimate" %in% levels) {
    pol_ud <- ctmm::SpatialPolygonsDataFrame.UD(
      ud, level.UD = .95)@polygons[[2]] # estimate
    
    p2 <- ggplot2::geom_polygon(
      data = pol_ud,
      mapping = ggplot2::aes(x = long,
                             y = lat,
                             group = group),
      fill = "#007d80", color = color,  
      alpha = .2)
  }
  
  if ("95% low CI" %in% levels) {
    pol_ud_low <- ctmm::SpatialPolygonsDataFrame.UD(
      ud, level.UD = .95)@polygons[[1]] # low
    
    p3 <- ggplot2::geom_polygon(
      data = pol_ud_low,
      mapping = ggplot2::aes(x = long,
                             y = lat,
                             group = group),
      col = color, fill = color,
      linetype = "dotted",
      alpha = .2)
  }
  
  ymin <- min(pol_ud_high@Polygons[[1]]@coords[,2])
  yrange <- range(pol_ud_high@Polygons[[1]]@coords[,2])
  
  tmp <- ymin - diff(yrange) * .2
  p <- ggplot2::ggplot() +
    
    ggplot2::geom_polygon(
      data = pol_ud_high,
      mapping = ggplot2::aes(x = long,
                             y = lat,
                             group = group),
      fill = NA, alpha = 1) +
    
    { if (show_truth)
      ggplot2::geom_polygon(
        data = truth,
        mapping = ggplot2::aes(x = long,
                               y = lat,
                               group = id),
        fill = "#353c42",
        alpha = .2)
    } +
    
    ggplot2::geom_path(data,
                       mapping = ggplot2::aes(x = x,
                                              y = y),
                       color = "black", size = 0.4,
                       alpha = .4) +
    ggplot2::geom_point(data,
                        mapping = ggplot2::aes(x = x,
                                               y = y),
                        color = "black", size = 1) +
    
    { if ("95% high CI" %in% levels) p1 } +
    { if ("Estimate" %in% levels) p2 } +
    { if ("95% low CI" %in% levels) p3 } +
    
    ggplot2::labs(x = "X coordinate",
                  y = "Y coordinate") +
    
    ggplot2::scale_x_continuous(
      labels = scales::comma) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(tmp, NA)) +
    
    ggplot2::coord_fixed() +
    
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
      legend.key.width = ggplot2::unit(0.6, "cm"))
}




#' Plot home range with simulated data
#'
#' @description Plotting home range output from ctmm with simulation.
#' @keywords internal
#'
#' @noRd
plotting_hr_new <- function(data1, data2,
                            sigma,
                            ud, levels,
                            show_data,
                            show_truth,
                            # bbox,
                            color, sim_color, fill) {

  id <- NULL
  radius_x <- radius_y <- sqrt(-2 * log(0.05) * sigma)
  truth <- data.frame(
    id = rep(1, each = 100),
    angle = seq(0, 2 * pi,length.out = 100))

  truth$long <- unlist(lapply(
    mean(data2$x),
    function(x) x + radius_x * cos(truth$angle)))
  truth$lat <- unlist(lapply(
    mean(data2$y),
    function(x) x + radius_y * sin(truth$angle)))

  pol_ud_high <- ctmm::SpatialPolygonsDataFrame.UD(
    ud, level.UD = .95)@polygons[[3]] # upper

  if ("95% high CI" %in% levels) {

    p1 <- ggplot2::geom_polygon(
      data = pol_ud_high,
      mapping = ggplot2::aes(x = long,
                             y = lat,
                             group = group),
      fill = fill, col = fill,
      linetype = "dotted",
      alpha = .2)
  }

  if ("Estimate" %in% levels) {
    pol_ud <- ctmm::SpatialPolygonsDataFrame.UD(
      ud, level.UD = .95)@polygons[[2]] # estimate
    
    p2 <- ggplot2::geom_polygon(
      data = pol_ud,
      mapping = ggplot2::aes(x = long,
                             y = lat,
                             group = group),
      fill = fill, color = fill,  
      alpha = .2)
  }

  if ("95% low CI" %in% levels) {
    pol_ud_low <- ctmm::SpatialPolygonsDataFrame.UD(
      ud, level.UD = .95)@polygons[[1]] # low
    
    p3 <- ggplot2::geom_polygon(
      data = pol_ud_low,
      mapping = ggplot2::aes(x = long,
                             y = lat,
                             group = group),
      col = fill, fill = fill,
      linetype = "dotted",
      alpha = .2)
  }
  
  show_col <- ifelse(show_data, "black", "white")
  show_alpha <- ifelse(show_data, 1, 0)

  ymin <- min(pol_ud_high@Polygons[[1]]@coords[,2])
  yrange <- range(pol_ud_high@Polygons[[1]]@coords[,2])
  
  tmp <- ymin - diff(yrange) * .2
  p <- ggplot2::ggplot() +
    
    ggplot2::geom_polygon(
      data = pol_ud_high,
      mapping = ggplot2::aes(x = long,
                             y = lat,
                             group = group),
      fill = NA, alpha = 1) +

    { if (show_truth)
      ggplot2::geom_polygon(
        data = truth,
        mapping = ggplot2::aes(x = long,
                               y = lat,
                               group = id),
        fill = "#353c42",
        alpha = .2)
    } +
    
    ggplot2::geom_path(data2,
                       mapping = ggplot2::aes(x = x,
                                              y = y),
                       color = fill, size = 0.4,
                       alpha = .4) +
    ggplot2::geom_point(data2,
                        mapping = ggplot2::aes(x = x,
                                               y = y),
                        color = fill, size = 1) +

    { if ("95% high CI" %in% levels) p1 } +
    { if ("Estimate" %in% levels) p2 } +
    { if ("95% low CI" %in% levels) p3 } +

    ggplot2::geom_point(
      data1, mapping = ggplot2::aes(x = x, y = y),
      color = show_col, alpha = show_alpha, size = 2) +

    ggplot2::labs(x = "X coordinate",
                  y = "Y coordinate") +

    ggplot2::scale_x_continuous(
      labels = scales::comma) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(tmp, NA)) +

    ggplot2::coord_fixed() +
    
    theme_movedesign() +
    ggplot2::theme(legend.position = "none")
}

#' Plot variogram
#'
#' @description Plot variogram from ctmm
#' @keywords internal
#'
#' @noRd
plotting_svf <- function(data, fill) {

  p <- data %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      ggplot2::aes_string(x = "lag_days",
                          ymin = "var_low95",
                          ymax = "var_upp95"),
      fill = "grey50",
      alpha = 0.25) +
    ggplot2::geom_ribbon(
      ggplot2::aes_string(x = "lag_days",
                          ymin = "var_low50",
                          ymax = "var_upp50"),
      fill = fill,
      alpha = 0.25) +
    ggplot2::geom_line(
      ggplot2::aes_string(x = "lag_days",
                          y = "SVF"), size = 0.5) +
    ggplot2::labs(
      x = "Time lag (in days)",
      y = expression("Semi-variance"~"("*km^{"2"}*")")) +
    theme_movedesign()

  return(p)

}



#' To significant digits
#'
#' @description WIP
#' @keywords internal
#'
#' @importFrom stringr str_pad
#' @noRd
#'
sigdigits <- function(x, digits = 2) {

  z <- format(x, digits = digits)
  if (!grepl("[.]",z)) return(z)
  
  out <- stringr::str_pad(z, digits + 1, "right" , "0")
  out <- as.numeric(out)

  return(out)
}


#' Subset time frame
#'
#' @description Subset time frame
#' @keywords internal
#'
#' @noRd
#'
subset_timeframe <- function(var, value) {
  as.data.frame(var) %>% dplyr::top_frac(value)
}

#' add_spinner
#'
#' @description WIP
#' @keywords internal
#'
#' @noRd
#'
add_spinner <- function(ui, type = 4, height = "300px") {
  shinycssloaders::withSpinner(
    ui, proxy.height = height,
    type = getOption("spinner.type", default = type),
    size = getOption("spinner.size", default = 1.5),
    color = getOption("spinner.color",
                      default = "#f4f4f4"))
}



#' wrap_none
#'
#' @description Wrap text without spaces
#' @keywords internal
#'
#' @noRd
#'
wrap_none <- function(text, ...,
                      end = "",
                      color = NULL,
                      css = NULL) {

  out <- shiny::HTML(paste0(text, ...))

  if (!is.null(css)) {
    out <- shiny::HTML(paste0(shiny::span(
      paste0(text, ...), class = css), end))
  }

  if (!is.null(color)) {

    out <- shiny::HTML(paste0(
      shiny::span(
        paste0(text, ...),
        style = paste0("color:", color, "!important;")),
      end))
  }

  return(out)

}

#' format_num
#'
#' @noRd
format_num <- function(value) {
  if (value < 5) {
    color <- "var(--danger)"
  } else if (value > 5 & value < 30) {
    color <- "var(--gold)"
  } else {
    color <- "var(--midnight)"
  }
}

#' format_perc
#'
#' @noRd
format_perc <- function(value) {
  if (abs(value) > .8) {
    color <- "var(--danger)"
  } else if (abs(value) > .2 & abs(value) < .8) {
    color <- "#f5b700"
  } else {
    color <- "var(--sea)"
  }
}

#' Calculate limits for plots.
#'
#' @noRd
calc_limit <- function(data1, data2, data3 = NULL, scale = .1) {
  
  xmin <- min(
    min(data1$x) - diff(range(data1$x)) * scale,
    min(data2$x) - diff(range(data2$x)) * scale)
  if (!is.null(data3)) {
    xmin <- min(xmin, min(data3$x) - diff(range(data3$x)) * scale)
  }
  
  xmax <- max(
    max(data1$x) + diff(range(data1$x)) * scale,
    max(data2$x) + diff(range(data2$x)) * scale)
  if (!is.null(data3)) {
    xmax <- max(xmax, max(data3$x) + diff(range(data3$x)) * scale)
  }
  
  ymin <- min(
    min(data1$y) - diff(range(data1$y)) * scale,
    min(data2$y) - diff(range(data2$y)) * scale)
  if (!is.null(data3)) {
    ymin <- min(ymin, min(data3$y) - diff(range(data3$y)) * scale)
  }
  ymax <- max(
    max(data1$y) + diff(range(data1$y)) * scale,
    max(data2$y) + diff(range(data2$y)) * scale)
  if (!is.null(data3)) {
    ymax <- max(ymax, max(data3$y) + diff(range(data3$y)) * scale)
  }
  
  out <- data.frame("xmin" = xmin, 
                    "xmax" = xmax, 
                    "ymin" = ymin, 
                    "ymax" = ymax)
  
  return(out)
}


#' create_pal
#'
#' @noRd
load_pal <- function() {

  # Palette:
  out <- list(mdn = "#222d32",
              sea = "#009da0",
              sea_m = "#007d80",
              sea_d = "#006669",
              grn = "#77b131",
              grn_d = "#508016",
              dgr = "#dd4b39",
              gld = "#ffbf00")

  return(out)
}


#' create_modal
#'
#' @noRd
create_modal <- function(var, id) {

  if (var == "taup") {
    out_title <- shiny::h4(
      span("Position autocorrelation", class = "cl-sea"),
      "parameter:")

    out_body <- fluidRow(
      style = paste("margin-right: 20px;",
                    "margin-left: 20px;"),

      p("The", span("position autocorrelation", class = "cl-sea"),
        "timescale", HTML(paste0("(\u03C4", tags$sub("p"), ")")),
        "is the", HTML(paste0(span("home range crossing time",
                                   class = "cl-sea"), "."))),
      p(span("What does this mean?",
             class = "cl-mdn", style = "text-align: center;"),
        "The", span("home range crossing time", class = "cl-sea"),
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
        span("sampling duration",  class = "cl-dgr"),
        "needs to be at least as long as the home range crossing time",
        "(if not many times longer) for",
        span("home range", class = "cl-sea-d"), "estimation.")

    ) # end of fluidRow
  } # end of taup

  if (var == "tauv") {
    out_title <- shiny::h4(
      span("Velocity autocorrelation", class = "cl-sea"),
      "parameter:")

    out_body <- fluidRow(
      style = paste("margin-right: 20px;",
                    "margin-left: 20px;"),

      p("The", span("velocity autocorrelation", class = "cl-sea"),
        "timescale", HTML(paste0("(\u03C4", tags$sub("v"), ")")),
        "is the", HTML(paste0(span("directional persistence",
                                   class = "cl-sea"), "."))),
      p("Animals with strong", span("directional persistence",
                                    class = "cl-sea"),
        "(ballistic or more linear movement bursts), will tend to have",
        "a", span("long", class = "cl-mdn"),
        HTML(paste0("\u03C4", tags$sub("v"))), "parameter.",
        "On the other hand, animals with more tortuous",
        "movement (less linear), will tend to have a much",
        span("shorter", class = "cl-mdn"),
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
             class = "cl-dgr"),
        "needs to be at least as long as the",
        span("velocity autocorrelation", class = "cl-sea"),
        "timescale for", span("distance/speed traveled",
                              class = "cl-sea-d"), "estimation.",
        "If", span(HTML("\u0394t"), class = "cl-dgr"), ">",
        HTML(paste0("3\u03C4", tags$sub("v"))), "then no",
        "statistically significant signature of the animal's",
        "velocity will remain in the tracking dataset.")

    ) # end of fluidRow
  } # end of tauv

  if (var == "sigma") {
    out_title <- shiny::h4(
      span("Semi-variance", class = "cl-sea"), "parameter:")

    out_body <- fluidRow(
      style = paste("margin-right: 20px;",
                    "margin-left: 20px;"),

      p("The", span("semi-variance", class = "cl-sea"),
        "parameter", HTML("(\u03C3)"), "is the",
        "average square distance observed",
        "at two different times,",
        "and ultimately measures the spatial variability",
        "between any two locations."
      ),

      p("We are simulating an",
        span("isotropic", class = "cl-sea-d"), "movement process,",
        "so", HTML("\u03C3"),
        "is the same in both the x and the y directions,",
        "resulting in a circular", span("home range", class = "cl-sea-d"),
        "area."
      ),

      p("As we are also modeling",
        span("range resident", class = "cl-sea-d"),
        "individuals (with a tendency to remain within their",
        "home range),", HTML("\u03C3"), "is asymptotic:",
        "if the", span("sampling duration", class = "cl-dgr"),
        "is sufficient, the average square distance between any two",
        "locations will be equal to the chosen",
        HTML("\u03C3"), "value.")

    ) # end of fluidRow
  } # end of tauv

  if (var == "loss") {
    out_title <- shiny::h4(
      span("Missing data", class = "cl-sea"), "bias:")

    out_body <- fluidRow(
      style = paste("margin-right: 20px;",
                    "margin-left: 20px;"),

      p("Many real-world issues can lead to animal locations",
        "being sampled", span("irregularly", class = "cl-dgr"),
        "in time: duty-cycling tags to avoid wasting battery",
        "during periods of inactivity, device malfunctions,",
        "habitat-related signal loss, and many others.",
        "Ultimately, missing data equate to",
        "a loss of", wrap_none(
          span("information", class = "cl-sea-d"), "."))

    ) # end of fluidRow
  } # end of loss


  if (var == "error") {
    out_title <- shiny::h4(
      span("Location error", class = "cl-sea"), "bias:")

    out_body <- fluidRow(
      style = paste("margin-right: 20px;",
                    "margin-left: 20px;"),

      p("TBA")

    ) # end of fluidRow

  } # end of error

  out <- bsplus::bs_modal(
    id = paste0("modal_", var, "_", id),
    title = out_title,
    body = out_body, size = "medium")

  return(out)
}


#' One tab to put inside a tab items container
#'
#' @description shinydashboard function, but with data values to fix rintrojs issue.
#' @keywords internal
#'
#' @noRd
newTabItem <- function(tabName = NULL, ...) {
  if (is.null(tabName))
    stop("Need tabName")
  
  if (grepl(".", tabName, fixed = TRUE)) {
    stop("tabName must not have a '.' in it.")
  }
  
  div(
    role = "tabpanel",
    class = "tab-pane",
    id = paste0("shiny-tab-", tabName),
    `data-value` = tabName,
    ...
  )
}


#' Convert as.telemetry to data.frame.
#'
#' @description Convert as.telemetry to data.frame
#' @keywords internal
#'
#' @noRd
as_tele_df <- function(object) {
  
  data_df <- list()
  for(i in 1:length(object)) {
    tempdf <- object[[i]]
    tempdf$id <- names(object)[i]
    data_df[[i]] <- tempdf
  }
  data_df <- do.call(rbind.data.frame, data_df)
  
  return(data_df)
}


#' round_any from plyr
#'
#' @description WIP
#' @keywords internal
#'
#' @noRd
#'
round_any <- function(x, accuracy, f = round) {
  f(x/accuracy) * accuracy
}


# ctmm and ctmmweb functions: ---------------------------------------------

#' Give false origin, orientation, dispatch epoch from ctmm.
#'
#' @description Give false origin, orientation, dispatch epoch
#' @keywords internal
#'
#' @noRd
pseudonymize <- function(data, 
                         center = c(0, 0), 
                         datum = "WGS84", 
                         origin = "1111-11-11 11:11.11 UTC", 
                         tz = "GMT", proj = NULL) {
  
  if(is.null(data)) { stop("No data selected.") }
  
  DROP <- class(data)[1] == "telemetry"
  if(class(data)[1] != "list") {
    data <- list(data)
    names(data) <- attr(data[[1]],'info')$identity
  }
  
  if (is.null(proj)) {
    proj <- paste0("+proj=aeqd +lon_0=", center[1], " +lat_0=", 
                   center[2], " +datum=", datum)
  }
  for (i in 1:length(data)) {
    
    axes <- c("x", "y")
    if (all(axes %in% names(data[[i]]))) {
      xy <- as.matrix(data.frame(data[[i]])[, axes], dimnames = axes)
    } else {
      xy <- numeric(0)
    }
    
    xy <- rgdal::project(xy, proj, inv = TRUE)
    data[[i]]$longitude <- xy[, 1]
    data[[i]]$latitude <- xy[, 2]
    attr(data[[i]], "info")$projection <- proj
    data[[i]]$timestamp <- as.POSIXct(data[[i]]$t, tz = tz, 
                                      origin = origin)
    attr(data[[i]], "info")$timezone <- tz
  }
  if (DROP) {
    data <- data[[1]]
  }
  return(data)
}

#' Extract total variance or average variance from ctmm.
#'
#' @description Extract total variance or average variance
#' @keywords internal
#'
#' @noRd
#' 
var.covm <- function(sigma, ave = FALSE) {
 
  if (ncol(sigma) == 1) {
    sigma <- return(sigma@par["major"])
  }
  sigma <- attr(sigma, "par")[c("major", "minor")]
  sigma <- sort(sigma, decreasing = TRUE)
  
  if (ave) {
    sigma <- mean(sigma, na.rm = TRUE)
  } else {
    sigma <- sum(sigma, na.rm = TRUE)
  }
  return(sigma)
}




#' Fall back function from ctmmweb
#'
#' @description General fall back function to deal with errors
#' @keywords internal
#'
#' @noRd
#'
fall_back <- function(f1, f1_args_list, f2, f2_args_list, msg) {
  res <- try(do.call(f1, f1_args_list))
  if (inherits(res, "try-error")) {
    cat(crayon::white$bgBlack(msg), "\n")
    res <- do.call(f2, f2_args_list)
  }
  return(res)
}


#' Check if error function from ctmmweb
#'
#' @noRd
#'
has_error <- function(result) {
  if (inherits(result, "try-error")) {
    TRUE
  } else {
    sapply(result, function(x) {
      inherits(x, "try-error")
    })
  }
}

#' Coerce telemetry object to list
#'
#' @description Coerce telemetry object to list from ctmmweb
#' @keywords internal
#'
#' @noRd
as_tele_list <- function(object) { 
  
  if (!inherits(object, "list")) { 
    tele_list <- list(object) 
    names(tele_list) <- attr(tele_list[[1]],"info")$identity 
    return(tele_list) 
  } else { 
    return(object) 
  } 
} 

#' Convert as.telemetry to data.table
#'
#' @description Convert as.telemetry to data.table
#' @keywords internal
#'
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom rlang :=
#' @importFrom plyr .
#'
#' @noRd
#'
as_tele_dt <- function(object) {
  
  .I <- id <- row_name <- row_no <- NULL
  
  if (!inherits(object, "list")) {
    stop("Requires list")
  }
  
  animal_count <- length(object)
  animal_data_list <- vector(mode = "list", length = animal_count)
  
  for (i in 1:animal_count) {
    animal_data_list[[i]] <- data.table::data.table(data.frame(object[[i]]))
    animal_data_list[[i]][, `:=`(id, object[[i]]@info$identity)]
    animal_data_list[[i]][, `:=`(row_name, row.names(object[[i]]))]
  }
  data_dt <- data.table::rbindlist(animal_data_list, fill = TRUE)
  data_dt[, `:=`(id, factor(id))]
  data_dt[, `:=`(row_no, .I)]
  data.table::setkey(data_dt, row_no)
  any_dup <- anyDuplicated(data_dt, by = c("id", "row_name"))
  if (any_dup != 0) {
    message("duplicated row name found within same individual:\n")
    print(data_dt[any_dup, .(id, row_name)])
  }
  return(data_dt)
}



# ctmmweb functions: ------------------------------------------------------

#' Parallel lapply
#'
#' @description Parallel lapply from ctmmweb.
#'
#' @param input Input list, with two sub-items: telemetry object and CTMM object.
#' @param parallel True/false. Uses a single core when FALSE.
#' @keywords internal
#'
#' @noRd
#'
par.lapply <- function(lst,
                       fun, 
                       cores = NULL,
                       parallel = TRUE,
                       win_init = expression({
                         requireNamespace("ctmm", quietly = TRUE)})) {
  if (parallel) {
    if (!is.null(cores) && cores > 0) {
      cluster_size <- cores
    }
    if (!is.null(cores) && cores < 0) {
      cluster_size <- max(parallel::detectCores(logical = FALSE) + 
                            cores, 1)
    }
    sysinfo <- Sys.info()
    tryCatch({
      if (sysinfo["sysname"] == "Windows") {
        if (is.null(cores)) {
          cluster_size <- min(length(lst), parallel::detectCores(logical = FALSE) * 
                                2)
        }
        cat(crayon::inverse("running parallel in SOCKET cluster of", 
                            cluster_size, "\n"))
        cl <- parallel::makeCluster(cluster_size, outfile = "")
        parallel::clusterExport(cl, c("win_init"), envir = environment())
        parallel::clusterEvalQ(cl, eval(win_init))
        res <- parallel::parLapplyLB(cl, lst, fun)
        parallel::stopCluster(cl)
      }
      else {
        if (is.null(cores)) {
          cluster_size <- min(length(lst), 
                              parallel::detectCores(logical = FALSE) * 4)
        }
        cat(crayon::inverse("running parallel with mclapply in cluster of", 
                            cluster_size, "\n"))
        res <- parallel::mclapply(lst, fun, mc.cores = cluster_size)
      }
    }, error = function(e) {
      cat(crayon::bgRed$white("Parallel Error, try restart R session\n"))
      cat(e)
    })
  } else {
    res <- lapply(lst, fun)
  }
  return(res)
}

#' Parallel model selection
#'
#' @description Parallel model selection, ctmm.select(), from ctmmweb.
#'
#' @param input Input list, with two sub-items: telemetry object and CTMM object.
#' @param parallel True/false. Uses a single core when FALSE.
#' @keywords internal
#'
#' @noRd
#'
par.ctmm.select <- function(input, cores = NULL,
                            trace = TRUE,
                            parallel = TRUE) {
  
  try_models <- function(input, trace) {
    fall_back(ctmm::ctmm.select,
              list(input[[1]],
                   CTMM = input[[2]],
                   control = list(method = "pNewton",
                                  cores = internal_cores),
                   trace = trace),
              ctmm::ctmm.select,
              list(input[[1]],
                   CTMM = input[[2]],
                   control = list(cores = internal_cores),
                   trace = trace),
              paste0("ctmm.select() failed with pNewton,",
                     "switching to Nelder-Mead."))
  }
  
  if (length(input) == 1) {
    # Process one individual on multiple cores:
    
    # message("No. of cores: ", parallel::detectCores(logical = FALSE))
    
    internal_cores <- if (parallel) -1 else 1
    res <- try(try_models(input[[1]],
                          trace = trace))
    
  } else {
    internal_cores <- 1
    res <- try(par.lapply(input,
                          try_models,
                          cores,
                          parallel))
  }
  
  if (any(has_error(res))) {
    cat(crayon::bgYellow$red("Error in model selection\n"))
  }
  
  return(res)
}


#' Parallel model fit
#'
#' @description Parallel model selection, ctmm.fit().
#'
#' @param input Input list, with two sub-items: telemetry object and CTMM object.
#' @param parallel True/false. Uses a single core when FALSE.
#' @keywords internal
#'
#' @noRd
#'
par.ctmm.fit <- function(input,
                         cores = NULL,
                         parallel = TRUE) {
  
  try_models <- function(input) {
    ctmm::ctmm.fit(input[[1]],
                   CTMM = input[[2]],
                   method = "pHREML",
                   control = list(cores = internal_cores))
  }
  
  if (length(input) == 1) {
    # Process one individual on multiple cores:
    internal_cores <- if (parallel) -1 else 1
    res <- try(try_models(input[[1]]))
    
  } else {
    # Process multiple animals on multiple cores:
    internal_cores <- 1
    res <- try(par.lapply(input,
                          try_models,
                          cores,
                          parallel))
  }
  
  if (any(has_error(res))) {
    cat(crayon::bgYellow$red("Error in model fit\n"))
  }
  return(res)
}


#' Calculate speed in parallel
#'
#' @param input Telemetry and model list.
#' @inheritParams par_lapply
#'
#' @noRd
#'
par.speed <- function(input,
                      cores = NULL,
                      trace = TRUE,
                      parallel = TRUE) {
  
  speed_calc <- function(input) {
    
    if (trace) message("Calculating:")
    
    ctmm::speed(input[[1]],
                input[[2]],
                cores = internal_cores,
                trace = trace)
  }
  
  if (length(input) == 1) {
    
    internal_cores <- if (parallel) -1 else 1
    res <- try(speed_calc(input[[1]]))
    
  } else {
    
    internal_cores <- 1
    res <- par.lapply(input, speed_calc, cores, parallel)
    
  }
  
  if (any(has_error(res))) {
    cat(crayon::bgYellow$red("Error in speed calculation\n"))
  }
  
  return(res)
}


#' Align lists
#'
#' @noRd
#'
align_lists <- function(...) {
  list_lst <- list(...)
  len_vec <- sapply(list_lst, length)
  stopifnot(length(unique(len_vec)) == 1)
  res <- lapply(seq_along(list_lst[[1]]), function(i) {
    lapply(list_lst, getElement, i)
  })
  if (length(res) == 0) 
    res <- NULL
  return(res)
}
