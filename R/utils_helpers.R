
#' Quiet functions
#' 
#' @noRd
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' Negate %in%
#' 
#' @noRd
`%!in%` <- Negate(`%in%`) 

#' Unicode names
#' 
#' @noRd
txt.tau_p <- "\u03C4\u209A"
txt.tau_v <- "\u03C4\u1D65"
txt.sig_p <- "\u03C3\u209A"
txt.sig_v <- "\u03C3\u1D65"

#' Message types
#' 
#' @noRd
msg_main <- crayon::make_style("dimgray")
msg_success <- crayon::make_style("#009da0")
msg_danger <- crayon::make_style("#dd4b39")
msg_warning <- crayon::make_style("#ffbf00")

#' Generate seed
#' 
#' @noRd
generate_seed <- function(seed_list = NULL) {
  
  set.seed(NULL)
  get_random <- function(n) {
    round(stats::runif(n, min = 1, max = 999999), 0)
  }
  
  out <- get_random(1)
  if (!is.null(seed_list))
    while ((out %in% seed_list) && ((out + 1) %in% seed_list)) 
      out <- get_random(1)
  return(out)
}

#' Parameter blocks
#'
#' @description Display parameters.
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
#' @keywords internal
#'
#' @noRd
sizeBlock <- function(type = c("n", "N_area", "N_speed"),
                      percentage = NULL,
                      icon = FALSE,
                      value = NULL,
                      intervals = NULL,
                      rightBorder = TRUE,
                      marginBottom = FALSE) {
  
  type <- match.arg(type)
  
  if (is.null(intervals)) {
    line1 <- dplyr::case_when(
      type == "n" ~ "Absolute sample size",
      TRUE ~ "Effective sample size")
    line2 <- dplyr::case_when(
      type == "n" ~ "n",
      type == "N_area" ~ "area",
      type == "N_speed" ~ "speed")
    if (startsWith(type, "N"))
      line2 <- HTML(paste0("(N", tags$sub(line2), ")"))
  } else {
    line1 <- intervals
    line2 <- dplyr::case_when(
      type == "n" ~ "Absolute sample size (n)",
      type == "N_area" ~ "area",
      type == "N_speed" ~ "speed")
    if (startsWith(type, "N"))
      line2 <- HTML(paste0("Effective sample size (N", 
                           tags$sub(line2), ")"))
  }
  
  cl <- "sizeblock"
  if (isTRUE(rightBorder))
    cl <- paste0(cl, " border-right")
  if (isTRUE(marginBottom))
    cl <- paste0(cl, " margin-bottom")
  
  if (!is.null(value)) {
    
    if (as.numeric(value) <= 5) {
      percentageColor <- "color: var(--danger) !important;"
      if (icon) { icon <- icon("angles-down")
      } else { icon <- HTML("&nbsp;") }
    } else { if (as.numeric(value) >= 30) {
      percentageColor <- "color: var(--sea) !important;"
      value <- scales::label_comma(accuracy = 1)(value)
      if (icon) { icon <- icon("angle-down")
      } else { icon <- HTML("&nbsp;") }
    } else {
      percentageColor <- "color: var(--gold) !important;"
      if (icon) { icon <- icon("angle-down")
      } else { icon <- HTML("&nbsp;") }
    }}
  }
  
  if (is.null(intervals)) {
    shiny::tags$div(
      class = cl,
      
      shiny::tags$span(
        class = "sizeblock-percentage", percentage,
        if (!is.null(icon)) icon, br(), style = percentageColor),
      shiny::tags$span(class = "sizeblock-header", value,
                       style = percentageColor),
      shiny::tags$span(class = "sizeblock-text", br(), line1),
      shiny::tags$span(class = "sizeblock-text", br(), line2))
  } else {
    shiny::tags$div(
      class = cl,
      
      shiny::tags$span(class = "sizeblock-text", br(), line2),
      br(),
      shiny::tags$span(
        class = "sizeblock-percentage", percentage,
        if (!is.null(icon)) icon, br(), style = percentageColor),
      shiny::tags$span(class = "sizeblock-header", value,
                       style = percentageColor),
      shiny::tags$span(class = "sizeblock-text", br(), line1))
  }
}

#' Relative error blocks
#'
#' @description Display relative errors.
#' @keywords internal
#'
#' @noRd
errorBlock <- function(icon = NULL,
                       text = NULL,
                       value = NULL,
                       min = NULL,
                       max = NULL,
                       nsims = NULL,
                       rightBorder = FALSE) {
  
  cl <- "errorblock"
  if (isTRUE(rightBorder)) cl <- paste0(cl, " border-right")
  
  if (value > 0) {
    tmptext <- span("Overestimation", icon("angle-up"))
  } else {
    tmptext <- span("Underestimation", icon("angle-down"))
  }
  
  getColor <- function(v) {
    
    if (is.na(v)) return( "#808080" )
    if (abs(v) >= 0.8) {
      "#dd4b39"
    } else if (abs(v) > 0.1 && abs(v) < 0.8) {
      "#ffa700"
    } else if (abs(v) <= 0.1) {
      "#009da0"
    }
  }
  
  color_err <- paste("color:", getColor(value), "!important;")
  color_err_min <- getColor(min)
  color_err_max <- getColor(max)
  
  value <- sigdigits(value * 100, 2)
  min <- ifelse((min * 100) %% 1 == 0,
                scales::label_comma(accuracy = 1)(min * 100),
                scales::label_comma(accuracy = .1)(min * 100))
  
  max <- ifelse((max * 100) %% 1 == 0,
                scales::label_comma(accuracy = 1)(max * 100),
                scales::label_comma(accuracy = .1)(max * 100))
  
  range <- wrap_none(
    "[", wrap_none(min, color = color_err_min),
    ", ", wrap_none(max, "%", color = color_err_max), "]")
  
  if (abs(value) < .01) value <- paste0("< 0.01")
  else if (abs(value) < .1) value <- round(value, 2)
  
  out_nsims <- NULL
  if (!is.null(nsims)) {
    out_nsims <- tagList(
      p(),
      shiny::tags$span(class = "errorblock-text", "Based on:", br()),
      shiny::tags$span(class = "errorblock-header",
                       nsims, ifelse(nsims == 1,
                                     "simulation", "simulations")))
  }
  
  shiny::tags$div(
    class = "errorblock",
    
    if (!is.null(icon)) { shiny::tags$span(
      class = "errorblock-icon", icon(icon), br()) },
    shiny::tags$span(class = "errorblock-text", text, br()),
    shiny::tags$span(class = "errorblock-header",
                     tmptext, br(), style = color_err),
    shiny::tags$span(class = "errorblock-value",
                     span(HTML(paste0(value, "%")), style = color_err)),
    shiny::tags$span(class = "errorblock-header", br(),
                     range), out_nsims
  ) # end of div
  
}

#' Parameter blocks
#'
#' @description Display parameters.
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
#' @keywords internal
#'
#' @noRd
extract_units <- function(input, name = NULL) {
  
  if (length(input) == 0) return(NULL)
  
  # if (class(input)[1] != "list" && class(input[[1]])[1] != "ctmm") {
  #   input <- summary(input)
  # } else {
  #   input <- summary(input[[1]])
  # }
  #   
  # if (inherits(input, "data.frame")) {
  #   tmp <- rownames(input)
  #   input <- tmp[grep(name, tmp)]
  # }
  
  tryCatch(
    expr = {
      string <- gsub(
        "\\(([^()]+)\\)", "\\1",
        stringr::str_extract_all(input,
                                 "\\(([^()]+)\\)")[[1]])
      return(string)
      
    }, error = function(e) return(NULL))
}

#' Add helper text.
#'
#' @description Add helper text to inputs.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @importFrom dplyr %>%
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
#' @keywords internal
#'
#' @importFrom dplyr %>%
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
#' @keywords internal
#'
#' @importFrom crayon make_style
#' @importFrom ctmm %#%
#' 
#' @noRd
msg_log <- function(..., detail, 
                    exp_time = NULL,
                    run_time = NULL,
                    style = NULL) {
  
  if (!is.null(run_time)) {
    total_time <- fix_unit(run_time[[1]], "seconds", convert = TRUE)
    
    if (round(run_time, 0) <= 1 %#% "minute") {
      detail <- "This step took less than one minute."
    } else {
      detail <- paste0("This step took approximately ",
                       round(total_time$value, 1), " ",
                       total_time$unit, ".")
    }
  } # end of run_time
  
  if (is.null(style)) {
    out <- cat(' ', HTML(...), "\n")
  } else {
    
    time_stamp <- stringr::str_c(
      "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "]")
    
    switch(
      style,
      "success" = { 
        line1 <- msg_success("\u2713")
        line2 <- crayon::bold(msg_success("Success:")) },
      "warning" = { 
        line1 <- msg_warning("!")
        line2 <- NULL },
      "danger" = { 
        line1 <- msg_danger("!")
        line2 <- crayon::bold(msg_danger("Warning:")) },
      "error" = { 
        line1 <- crayon::bold(msg_danger("\u2716"))
        line2 <- crayon::bold(msg_danger("Error:")) }
    )
    
    if (missing(detail)) {
      out <- cat(msg_main(time_stamp), "\n",
                 ' ', line1,
                 line2, ..., "\n")
    } else {
      out <- cat(msg_main(time_stamp), "\n",
                 ' ', line1,
                 line2, ..., "\n",
                 ' ', msg_main(detail), "\n")  }
  }
  
  return(out)
}


#' Create message steps
#'
#' @description Create message logs
#' @keywords internal
#'
#' @noRd
msg_step <- function(current, total, style) {
  
  if (style == "success") 
    txt <- msg_success(current)
  if (style == "warning") 
    txt <- msg_warning(current)
  if (style == "danger") 
    txt <- msg_danger(current)
  if (style == "error") 
    txt <- crayon::bold(msg_danger(current))
  
  return(paste0(" (step ", txt, " out of ", total, ")."))
}

#' Reset reactive values
#'
#' @description Reset reactive values
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
reset_reactiveValues <- function(rv) {
  
  rv$is_valid <- FALSE
  rv$is_analyses <- FALSE
  
  if (!is.null(isolate(rv$species))) rv$species <- NULL
  if (!is.null(isolate(rv$id))) rv$id <- NULL
  
  if (!is.null(isolate(rv$sigma))) rv$sigma <- NULL
  if (!is.null(isolate(rv$tau_p))) rv$tau_p <- NULL
  if (!is.null(isolate(rv$tau_v))) rv$tau_v <- NULL
  if (!is.null(isolate(rv$speed))) rv$speed <- NULL
  if (!is.null(isolate(rv$mu))) rv$mu <- NULL
  
  if (!is.null(isolate(rv$tmp$id))) rv$tmp$id <- NULL
  if (!is.null(isolate(rv$tmp$sp))) rv$tmp$sp <- NULL
  if (!is.null(isolate(rv$tmp$sp_common))) rv$tmp$sp_common <- NULL
  
  if (!is.null(isolate(rv$data_type))) rv$data_type <- NULL
  if (!is.null(isolate(rv$datList))) rv$datList <- NULL
  if (!is.null(isolate(rv$fitList))) rv$fitList <- NULL
  if (!is.null(isolate(rv$simdatList))) rv$simdatList <- NULL
  if (!is.null(isolate(rv$simfitList))) rv$simfitList <- NULL
  
  if (!is.null(isolate(rv$hr$datList))) rv$hr$datList <- NULL
  if (!is.null(isolate(rv$hr$fitList))) rv$hr$fitList <- NULL
  if (!is.null(isolate(rv$sd$datList))) rv$sd$datList <- NULL
  if (!is.null(isolate(rv$sd$fitList))) rv$sd$fitList <- NULL
  
  if (!is.null(isolate(rv$hr))) rv$hr <- NULL
  if (!is.null(isolate(rv$sd))) rv$sd <- NULL
  if (!is.null(isolate(rv$nsims))) rv$nsims <- NULL
  
}

#' Add help modal
#'
#' @description Add help modal to inputs
#' @keywords internal
#'
#' @importFrom dplyr %>%
#' @noRd
help_modal <- function(input, file) {
  bsplus::shinyInput_label_embed(
    input, bsplus::shiny_iconlink(
      name = "circle-question", class = "icon_help") %>%
      bsplus::bs_attach_modal(id_modal = file))
}


#' @title movedesign ggplot2 custom theme
#' @encoding UTF-8
#'
#' @description Custom ggplot2 theme for movedesign plot outputs.
#' @author Inu00EAs Silva \email{i.simoes-silva@@hzdr.de}
#' @keywords internal
#' 
#' @importFrom ggplot2 %+replace%
#' @importFrom dplyr %>%
#'
#' @param ft_size Base font size.
#' @noRd
theme_movedesign <- function(ft_size = 13,
                             font = "Roboto Condensed",
                             font_available = TRUE,
                             title_y = TRUE) {
  
  if (!font_available) {
    
    ggplot2::theme_minimal() %+replace%
      ggplot2::theme(
        
        text = ggplot2::element_text(size = ft_size),
        
        plot.title = ggtext::element_markdown(
          size = ft_size + 3, vjust = 1.2, hjust = .5),
        plot.subtitle = ggtext::element_markdown(
          color = "#666666", hjust = .5),
        plot.margin = ggplot2::unit(c(0.2, 0.2, 0.3, 0.2), "cm"),
        
        panel.grid.minor = ggplot2::element_line(colour = "#f7f7f7"),
        panel.grid.major = ggplot2::element_line(colour = "#f7f7f7"),
        
        axis.text.x = ggplot2::element_text(colour = "#878787"),
        axis.text.y = ggplot2::element_text(colour = "#878787"),
        axis.title.x = ggtext::element_markdown(
          hjust = 1, margin = ggplot2::margin(t = 2.5)),
        axis.title.y = ggtext::element_markdown(
          angle = 90, margin = ggplot2::margin(r = 2.5))) %>% 
      suppressWarnings()
    
  } else {
    
    ggplot2::theme_minimal() %+replace%
      ggplot2::theme(
        
        text = ggplot2::element_text(family = font, size = ft_size),
        
        plot.title = ggtext::element_markdown(
          size = ft_size + 3, vjust = 1.2, hjust = .5),
        
        plot.subtitle = ggtext::element_markdown(
          color = "#666666", hjust = .5),
        plot.margin = ggplot2::unit(c(0.2, 0.2, 0.3, 0.2), "cm"),
        
        panel.grid.minor = ggplot2::element_line(colour = "#f7f7f7"),
        panel.grid.major = ggplot2::element_line(colour = "#f7f7f7"),
        
        axis.text.x = ggplot2::element_text(colour = "#878787"),
        axis.text.y = ggplot2::element_text(colour = "#878787"),
        axis.title.x = ggtext::element_markdown(
          hjust = 1, margin = ggplot2::margin(t = 2.5)),
        
        if (title_y) {
          axis.title.y = ggtext::element_markdown(
            angle = 90, margin = ggplot2::margin(r = 2.5)) }
        
      ) %>%
      suppressWarnings()
  }
}


#' Plot home range
#'
#' @description Plotting home range output from ctmm
#' @keywords internal
#'
#' @noRd
plotting_hr <- function(input1,
                        input2 = NULL,
                        show_both = FALSE,
                        truth,
                        show_truth,
                        show_locations,
                        contours,
                        color,
                        extent,
                        font_available = TRUE) {
  
  id <- NULL
  if (!is.list(input1)) stop("Input is not a list.")
  data <- data1 <- input1[["data"]]
  to_plot <- "initial"
  
  if (!is.null(input2)) {
    if (!is.list(input2)) stop("Input is not a list.")
    data <- data2 <- input2[["data"]]
    to_plot <- "modified"
  }
  
  if (to_plot == "initial") {
    ud <- input1[["ud"]]
    if (!inherits(input1[["ud"]], "UD"))
      stop("'ud' element is not UD class.")
    pal <- c("#007d80", "#00484a")
  } else if (to_plot == "modified") {
    ud <- input2[["ud"]]
    if (!inherits(input2[["ud"]], "UD"))
      stop("'ud' element is not UD class.")
    pal <- c("#dd4b39", "#cc1b34")
  }
  
  show_col <- ifelse(show_both, "#00484a", "white")
  show_alpha <- ifelse(show_both, 0.3, 0)
  
  extent[1,"x"] <- min(extent[1,"x"], min(truth$x), min(data$x))
  extent[2,"x"] <- max(extent[2,"x"], max(truth$x), max(data$x))
  extent[1,"y"] <- min(extent[1,"y"], min(truth$y), min(data$y))
  extent[2,"y"] <- max(extent[2,"y"], max(truth$y), max(data$y))
  
  extent[,"x"] <- extent[,"x"] + 
    diff(range(extent[,"x"])) *
    c(-.01, .01)
  extent[,"y"] <- extent[,"y"] + 
    diff(range(extent[,"y"])) *
    c(-.01, .01)
  
  ud <- ctmm::as.sf(ud, level = .95, level.UD = .95)
  
  if ("uci" %in% contours) {
    p1 <- ggplot2::geom_sf(
      data = ud[3, ],
      fill = color, color = color, 
      linetype = "dotted", alpha = .2)
  }
  
  p2 <- ggplot2::geom_sf(
    data = ud[2, ],
    fill = pal[1], color = NA, alpha = .1)
  if ("est" %in% contours) {
    p2 <- ggplot2::geom_sf(
      data = ud[2, ],
      fill = pal[1], color = color, alpha = .1)
  }
  
  if ("lci" %in% contours) {
    p3 <- ggplot2::geom_sf(
      data = ud[1, ],
      fill = color, color = pal[2], 
      linetype = "dotted", alpha = .2)
  }
  
  p <- ggplot2::ggplot() +
    
    { if (show_truth)
      ggplot2::geom_polygon(
        data = truth,
        mapping = ggplot2::aes(x = x, y = y, group = id),
        fill = "#353c42", alpha = .2)
    } +
    
    { if (show_locations)
      ggplot2::geom_path(
        data = data,
        mapping = ggplot2::aes(x = x, y = y),
        color = pal[2], linewidth = 0.4, alpha = .4)
    } +
    { if (show_locations)
      ggplot2::geom_point(
        data = data,
        mapping = ggplot2::aes(x = x, y = y),
        color = pal[2], size = 1, alpha = .3)
    } +
    
    { if ("uci" %in% contours) p1 } +
    p2 +
    { if ("lci" %in% contours) p3 } +
    
    { if (show_both)
      ggplot2::geom_point(
        data = data1,
        mapping = ggplot2::aes(x = x, y = y),
        color = show_col, alpha = show_alpha, 
        size = 1)
    } +
    
    ggplot2::scale_x_continuous(
      labels = scales::comma,
      limits = c(
        extent$x[1] - abs(diff(range(extent$x))) * .01,
        extent$x[2] + abs(diff(range(extent$x))) * .01)) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(
        extent$y[1] - abs(diff(range(extent$y))) * .01,
        extent$y[2] + abs(diff(range(extent$y))) * .01)) +
    
    ggplot2::labs(x = "X coordinate", y = "Y coordinate") +
    theme_movedesign(font_available = font_available) +
    ggplot2::theme(legend.position = "none")
  
  return(p)
}



#' Plot variogram
#'
#' @description Plot variogram from ctmm
#' @keywords internal
#'
#' @importFrom dplyr %>%
#' @noRd
plotting_svf <- function(data, fill,
                         fraction = .5,
                         add_fit = FALSE,
                         x_unit = "days",
                         y_unit = "km^2",
                         font_available = TRUE) {
  out <- list()
  if (class(data[[1]])[1] != "list") data <- list(data)
  m <- length(data)
  
  if (y_unit == "km^2") y_lab <- 
    expression("Semi-variance"~"("*km^{"2"}*")")
  if (y_unit == "m^2") y_lab <- 
    expression("Semi-variance"~"("*m^{"2"}*")")
  if (y_unit == "hectares") y_lab <- "Semi-variance (ha)"
  
  data[sapply(data, is.null)] <- NULL
  
  out <- lapply(seq_along(data), function(x) {
    if (is.null(data[[x]]$fit)) {
      svf <- data[[x]]$data %>% 
        dplyr::slice_min(lag, prop = fraction)
      add_fit <- FALSE
    } else {
      fit <- data[[x]]$fit %>% 
        dplyr::slice_min(lag, prop = fraction)
      svf <- data[[x]]$data[data[[x]]$data$lag <= max(fit$lag), ]
    }
    
    ft_size <- ifelse(m == 1, 13, ifelse(m >= 10, 6, 11))
    
    p <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = svf,
        mapping = ggplot2::aes(x = lag,
                               ymin = svf_lower,
                               ymax = svf_upper),
        fill = "grey50",
        alpha = 0.25) +
      ggplot2::geom_ribbon(
        data = svf,
        mapping = ggplot2::aes(x = lag,
                               ymin = svf_low50,
                               ymax = svf_upp50),
        fill = "grey40",
        alpha = 0.25) +
      ggplot2::geom_line(
        data = svf,
        mapping = ggplot2::aes(x = lag,
                               y = svf),
        linewidth = 0.5) +
      
      { if (add_fit) 
        ggplot2::geom_line(
          data = fit,
          mapping = ggplot2::aes(x = lag,
                                 y = svf),
          color = fill[[x]], linetype = "dashed") 
      } +
      
      { if (add_fit) 
        ggplot2::geom_ribbon(
          data = fit, 
          mapping = ggplot2::aes(x = lag,
                                 ymin = svf_lower,
                                 ymax = svf_upper),
          fill = fill[[x]], alpha = 0.2)
      } +
      
      ggplot2::labs(
        title = names(data)[[x]],
        x = "Time lag (in days)",
        y = y_lab) +
      theme_movedesign(font_available = font_available,
                       ft_size = ft_size)
    
    return(p)
    
  })
  
  return(out)
  
}

#' Plot outlier
#'
#' @description Plot outliers
#' @keywords internal
#'
#' @importFrom dplyr %>%
#' @noRd
plotting_outlier <- function(data,
                             font_available = TRUE) {
  
  m <- length(data)
  out_data <- quiet(ctmm::outlie(data, plot = FALSE)) %>% 
    suppressMessages() %>%
    suppressWarnings()
  
  out_plot <- lapply(seq_along(data), function(x) {
    
    ind <- data[[x]]
    UERE <- ctmm::uere(ind)
    
    if ("VAR.xy" %!in% names(ind)) {
      ctmm::uere(ind) <- UERE
    }
    
    error <- UERE$UERE[, "horizontal"]
    names(error) <- rownames(UERE$UERE)
    error <- ctmm::ctmm(error = error, axes = c("x", "y"))
    error <- get.error(ind, error, calibrate = TRUE)
    
    DT <- diff(data[[x]]$t)
    time.res <- time_res(DT)
    ZERO <- DT == 0
    if (any(ZERO)) {
      DT[ZERO] <- time.res[2]
    }
    
    Vs <- assign_speeds(data[[x]],
                        UERE = error, 
                        DT = DT, axes = c("x", "y"))
    
    v <- Vs$v.t
    VAR.v <- Vs$VAR.t
    mu <- ctmm::median(data[[x]])
    d <- get.telemetry(data[[x]], axes = c("x", "y"))
    mu <- get.telemetry(mu, axes = c("x", "y"))
    mu <- c(mu)
    d <- t(d) - mu
    if (length(dim(error)) == 3) {
      d <- t(d)
    } else {
      d <- colSums(d^2)
      d <- sqrt(d)
    }
    D <- distanceMLE(d, error, return.VAR = TRUE)
    d <- D[, 1]
    VAR.d <- D[, 2]
    rm(D)
    
    # if ("z" %in% names(data[[x]])) {
    #   error <- UERE$UERE[, "vertical"]
    #   names(error) <- rownames(UERE$UERE)
    #   error <- ctmm::ctmm(error = error, axes = c("z"))
    #   error <- get.error(data[[x]], error, calibrate = TRUE)
    #   Vz <- assign_speeds(data[[x]], UERE = error, DT = DT,
    #                       axes = "z")
    #   vz <- Vz$v.t
    #   VAR.vz <- Vz$VAR.t
    #   dz <- get.telemetry(data[[x]], axes = c("z"))
    #   dz <- dz - stats::median(data[[x]]$z)
    #   dz <- abs(dz)
    #   DZ <- distanceMLE(dz, error, axes = "z", 
    #                     return.VAR = TRUE)
    #   dz <- DZ[, 1]
    #   VAR.dz <- DZ[, 2]
    #   rm(DZ)
    # }
    
    lwd <- Vs$v.dt
    if (diff(range(lwd))) lwd <- lwd / max(lwd) else lwd <- 0
    if (diff(range(d))) cex <- d/max(d) * 4 else cex <- 0
    
    # col <- grDevices::rgb(cex, 0, 0, cex)
    palette <- grDevices::colorRampPalette(
      c("white", "#dd4b39"))(length(cex))
    
    ft_size <- ifelse(m == 1, 13, ifelse(m >= 10, 6, 11))
    
    p <- ggplot2::ggplot(data[[x]]) +
      ggplot2::geom_segment(
        data = data.frame(
          x0 = data[[x]]$x[-length(data[[x]]$x)],
          y0 = data[[x]]$y[-length(data[[x]]$y)],
          x1 = data[[x]]$x[-1],
          y1 = data[[x]]$y[-1],
          lwd = lwd),
        ggplot2::aes(x = x0, y = y0, 
                     xend = x1, yend = y1, 
                     color = lwd),
        linewidth = lwd) +
      
      viridis::scale_color_viridis(option = "mako",
                                   direction = -1) +
      
      ggplot2::geom_point(
        ggplot2::aes(x = x, y = y),
        color = palette, size = cex, shape = 20) +
      
      ggplot2::scale_size_identity() +
      ggplot2::labs(x = NULL, y = NULL) +
      theme_movedesign(font_available = font_available,
                       ft_size = ft_size) +
      ggplot2::theme(legend.position = "none")
    
    return(p)
  })
  
  return(list(data = out_data, plot = out_plot))
}


#' To significant digits
#'
#' @description WIP
#' @keywords internal
#'
#' @importFrom stringr str_pad
#' @noRd
#'
sigdigits <- function(x, digits) {
  
  new_x <- format(x, digits = digits)
  out <- ifelse(
    grepl("[.]", new_x),
    stringr::str_pad(new_x, digits + 1, "right", "0"),
    new_x)
  
  return(as.numeric(out))
}


#' Subset time frame
#'
#' @description Subset time frame
#' @keywords internal
#'
#' @importFrom dplyr %>%
#' @noRd
#'
subset_timeframe <- function(var, value) {
  as.data.frame(var) %>% dplyr::top_frac(value)
}

#' Show loading spinner
#'
#' @description WIP
#' @keywords internal
#'
#' @importFrom ctmm %#%
#' @importFrom dplyr %>%
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

#' Show loading modal
#'
#' @description WIP
#' @keywords internal
#'
#' @importFrom dplyr %>%
#' @importFrom ctmm %#%
#' @noRd
#'
loading_modal <- function(x, 
                          exp_time = NULL,
                          parallel = FALSE,
                          n = NULL, type = "speed") {
  
  if (missing(x))
    stop("`x` argument not provided.")
  if (!is.character(x))
    stop("`unit` argument must be a character string.")
  
  note_parallel <- ifelse(parallel, "core", "simulation")
  
  x <- stringr::str_split(x, " ")[[1]]
  num_words <- length(x)
  if (num_words > 2) x[2] <- paste(x[2:num_words], collapse = " ")
  
  n <- ifelse(is.null(n), 1, n)
  
  if (is.null(exp_time)) {
    out_txt <- p()
  } else {
    
    if (!("mean" %in% names(exp_time)) || 
        !("unit" %in% names(exp_time)) ||
        !("range" %in% names(exp_time)))
      stop(paste0("input must contain named columns 'mean'",
                  "'range', and 'unit'."))
    
    header_css <- paste("background-color: #eaeaea;",
                        "color: #797979;",
                        "font-size: 16px;",
                        "text-align: center;")
    time_css <- paste("background-color: #eaeaea;",
                      "color: #009da0;",
                      "font-size: 15px;",
                      "text-align: center;",
                      "margin-top: -40px;")
    
    mean_time <- fix_unit(exp_time$mean * n, exp_time$unit,
                          convert = TRUE)
    max_time <- fix_unit(exp_time$max * n, exp_time$unit, 
                         convert = TRUE)
    
    tmp <- max_time$unit %#% (
      ifelse(exp_time$min == 0, .001, exp_time$min) * n) %#%
      exp_time$unit
    min_time <- fix_unit(ifelse(tmp <= 1, 2, tmp),
                         max_time$unit)
    
    out_txt_range <- paste0(min_time$value, 
                            "\u2013", max_time$value, 
                            " ", max_time$unit)
    
    out_txt_parallel <- span("")
    if (type == "fit") {
      if (parallel) {
        n_cores <- parallel::detectCores(logical = FALSE)/2
        tmp_time <- fix_unit(mean_time$value / n_cores,
                             mean_time$unit, convert = TRUE)
        out_txt_range <- paste(tmp_time$value, tmp_time$unit)
        out_txt_parallel <- p(
          style = paste("font-size: 14px;",
                        "line-height: 1;",
                        "text-align: center;"),
          span("[Running in parallel!].", class = "cl-sea"))
        
      } else {
        out_txt_range <- paste(mean_time$value, mean_time$unit)
      }
    }
    
    if (!is.null(n)) {
      if (!is.numeric(n))
        stop("`n` argument must be numeric.")
      
      out_txt <- tagList(
        p(),
        p("Expected run time:",
          style = paste("background-color: #eaeaea;",
                        "color: #797979;",
                        "font-size: 16px;",
                        "text-align: center;")), br(),
        p(exp_time$range, style = time_css), p())
      
      out_txt_total <- tagList(
        p("Total run time:", 
          style = header_css), br(),
        p("\u2248", out_txt_range,
          style = time_css), p())
      
      if (n > 1) {
        out_txt <- tagList(
          p(),
          p("Expected run time:",
            style = header_css), br(),
          p(exp_time$range, paste0("(per ", note_parallel, ")"), 
            style = time_css), p(),
          if (!parallel) out_txt_total,
          out_txt_parallel
        )
      }
    }
  }
  
  shinybusy::show_modal_spinner(
    spin = "fading-circle",
    color = "var(--sea)",
    text = tagList(
      span(x[1], style = "color: #797979;"),
      HTML(paste0(span(x[2], class = "cl-sea"),
                  span("...", style = "color: #797979;"))),
      out_txt)
  ) # end of modal
  
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
    out <- shiny::HTML(
      paste0(
        shiny::span(
          paste0(text, ...), class = css), end))
  }
  
  if (!is.null(color)) {
    
    out <- shiny::HTML(paste0(
      shiny::span(
        shiny::HTML(paste0(text, ...)),
        style = paste0("color:", color, "!important;")),
      end))
  }
  
  return(out)
  
}

#' format_num
#'
#' @noRd
format_num <- function(value) {
  list(color = case_when(
    value < 5 ~ "#dd4b39",
    value > 5 & value < 30 ~ "#ffa600",
    TRUE ~ "#222d32")) #, fontWeight = "bold")
}

#' format_perc
#'
#' @noRd
format_perc <- function(value) {
  list(color = case_when(
    abs(value) > .5 ~ "#dd4b39",
    abs(value) > .1 & abs(value) < .5 ~ "#ffa600",
    TRUE ~ "#006669"))
}

#' Calculate limits for plots.
#'
#' @noRd
extract_limits <- function(data1, data2, data3 = NULL, scale = .1) {
  
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
              sea_d = "#00585A",
              grn = "#77b131",
              grn_d = "#385c13",
              dgr = "#dd4b39",
              dgr_d = "#A12C3B",
              gld = "#ffb300",
              gld_d = "#D47800")
  
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
      span("Location variance", class = "cl-sea"), "parameter:")
    
    out_body <- fluidRow(
      style = paste("margin-right: 20px;",
                    "margin-left: 20px;"),
      
      p("The", span("location variance", class = "cl-sea"),
        "parameter", wrap_none("(\u03C3", tags$sub("p"), ")"), "is the",
        "average square distance observed",
        "at two different times,",
        "and ultimately measures the spatial variability",
        "between any two locations."
      ),
      
      p("We are simulating an",
        span("isotropic", class = "cl-sea-d"), "movement process,",
        "so", wrap_none("\u03C3", tags$sub("p")),
        "is the same in both the x and the y directions,",
        "resulting in a circular", span("home range", class = "cl-sea-d"),
        "area."
      ),
      
      p("As we are also modeling",
        span("range resident", class = "cl-sea-d"),
        "individuals (with a tendency to remain within their",
        "home range),", HTML("\u03C3\u209A"), "is asymptotic:",
        "if the", span("sampling duration", class = "cl-dgr"),
        "is sufficient, the average square distance between any two",
        "locations will be equal to the chosen",
        HTML("\u03C3\u209A"), "value.")
      
    ) # end of fluidRow
  } # end of tauv
  
  # if (var == "speed") {
  #   out_title <- shiny::h4(
  #     span("Movement speed", class = "cl-sea"), "parameter:")
  #   
  #   out_body <- fluidRow(
  #     style = paste("margin-right: 20px;",
  #                   "margin-left: 20px;"),
  #     
  #     
  #     p("WIP")
  #     
  #   ) # end of fluidRow
  # } # end of speed
  
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
  
  if (var == "failure") {
    out_title <- shiny::h4(
      span("Transmitter or tag failure", class = "cl-sea"), ":")
    
    out_body <- fluidRow(
      style = paste("margin-right: 20px;",
                    "margin-left: 20px;"),
      
      p("Some devices stop collecting information in the field.",
        "This could be due to a myriad of factors:",
        wrap_none(span("signal loss", class = "cl-dgr"), ","),
        "animal mortality or premature detachment, exhaustion",
        "of batteries, antenna breakage, among others.", br(),
        "Setting this input to 5%, for example, means that there",
        "is a 5% chance that the simulated tags stop recording",
        span("locations", class = "cl-dgr"),
        "at some point during the simulation.",
        "For illustrative purposes, the initial tags/simulations",
        "created in the", 
        fontawesome::fa("stopwatch", fill = "#009da0"),
        span("Sampling design", class = "cl-sea-l"), "tab",
        "will never fail, this will only be applicable to any",
        "subsequent simulation in the",
        fontawesome::fa("compass-drafting", fill = "#009da0"),
        span("Analyses", class = "cl-sea-l"), "tabs."
      )
      
    ) # end of fluidRow
  } # end of failure
  
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
telemetry_as_df <- function(object) {
  if (class(object)[1] != "list" && class(object)[1] != "ctmm") 
    stop("Object must be a telemetry object.")
  
  out_df <- lapply(seq_along(object), function(x) {
    df <- cbind(object[[x]], id = names(object)[x])
    df[, c("timestamp", "longitude", "latitude", "t", "x", "y", "id")]
  })
  
  out_df <- do.call(rbind.data.frame, out_df)
  head(out_df)
  return(out_df)
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
  
  if (is.null(data)) { stop("No data selected.") }
  
  DROP <- class(data)[1] == "telemetry"
  if (class(data)[1] != "list") {
    data <- list(data)
    names(data) <- attr(data[[1]],'info')$identity
  }
  
  if (is.null(proj))
    proj <- paste0("+proj=aeqd +lon_0=", center[1],
                   " +lat_0=", center[2], 
                   " +datum=", datum)
  
  for (i in seq_along(data)) {
    
    axes <- c("x", "y")
    if (all(axes %in% names(data[[i]]))) {
      xy <- as.matrix(data.frame(data[[i]])[, axes], dimnames = axes)
    } else {
      xy <- numeric(0)
    }
    
    xy <- terra::project(xy, from = proj,to = "+proj=longlat +datum=WGS84")
    data[[i]]$longitude <- xy[, 1]
    data[[i]]$latitude <- xy[, 2]
    attr(data[[i]], "info")$projection <- proj
    
    data[[i]]$timestamp <- as.POSIXct(data[[i]]$t, tz = tz, origin = origin)
    attr(data[[i]], "info")$timezone <- tz
  }
  
  if (DROP) data <- data[[1]]
  return(data)
}


#' Extract location variance from ctmm.
#'
#' @description Extract total variance or average variance
#' @keywords internal
#'
#' @noRd
#' 
var.covm <- function(sigma, average = FALSE) {
  
  if (ncol(sigma) == 1) return(sigma@par["major"])
  
  sigma <- attr(sigma, "par")[c("major", "minor")]
  sigma <- sort(sigma, decreasing = TRUE)
  
  sigma <- ifelse(average,
                  mean(sigma, na.rm = TRUE),
                  sum(sigma, na.rm = TRUE))
  
  return(sigma)
}


#' Check if error function from ctmmweb
#'
#' @noRd
#'
has_error <- function(result) {
  if (inherits(result, "try-error")) return(TRUE)
  else return(sapply(result, function(x) {
    inherits(x, "try-error")
  }))
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
tele_to_dt <- function(object) {
  
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
    message("Duplicated row name found within same individual:")
    print(data_dt[any_dup, .(id, row_name)])
    return(NULL)
  }
  return(data_dt)
}

#' @title Build tables row-by-row
#' @description Build tables row-by-row
#' @keywords internal
#' 
#' @noRd
#' 
.build_tbl <- function(data_type = "Initial",
                       target = NULL,
                       group = NULL,
                       device = NULL,
                       data,
                       seed,
                       obj,
                       tau_p,
                       tau_v,
                       sigma,
                       area,
                       area_error,
                       speed,
                       speed_error,
                       distance, 
                       distance_error) {
  
  if (is.null(group)) group <- NA
  if (is.null(device)) device <- NA
  if (is.null(target)) target <- ""
  
  out <- data.frame(
    data = data_type,
    device = device,
    seed = as.numeric(seed),
    group = group,
    taup = NA,
    tauv = NA,
    sigma = NA,
    dur = NA,
    dti = NA,
    n = nrow(data),
    N1 = NA,
    N2 = NA,
    area = NA,
    area_err = NA,
    area_err_min = NA,
    area_err_max = NA,
    ctsd = NA,
    ctsd_err = NA,
    ctsd_err_min = NA,
    ctsd_err_max = NA,
    dist = NA,
    dist_err = NA)
  
  out$taup <- paste(
    scales::label_comma(.1)(tau_p$value[2]), abbrv_unit(tau_p$unit[2]))
  out$tauv <- paste(
    scales::label_comma(.1)(tau_v$value[2]), abbrv_unit(tau_v$unit[2]))
  out$sigma <- paste(
    scales::label_comma(.1)(sigma$value[2]), abbrv_unit(sigma$unit[2]))
  
  dur <- extract_sampling(data, name = "period")[[1]]
  dur <- fix_unit(dur$value, dur$unit, convert = TRUE)
  out$dur <- paste(dur$value, abbrv_unit(dur$unit))
  
  dti <- extract_sampling(data, name = "interval")[[1]]
  dti <- fix_unit(dti$value, dti$unit)
  out$dti <- paste(dti$value, abbrv_unit(dti$unit))
  
  if (target == "") {
    out$N1 <- extract_dof(obj, name = "area")[[1]]
    out$N2 <- extract_dof(obj, name = "speed")[[1]]
  }
  
  if (target == "hr") {
    out$N1 <- extract_dof(obj, name = "area")[[1]]
    
    if (is.na(area$est)) out$area <- NA
    else out$area <- paste(
      scales::label_comma(.1)(area$est), abbrv_unit(area$unit))
    
    out$area_err <- area_error$est
    out$area_err_min <- area_error$lci
    out$area_err_max <- area_error$uci
    
  } # end of if (target == "hr")
  
  if (target == "ctsd") {
    out$N2 <- extract_dof(obj, name = "speed")[[1]]
    
    if (is.na(speed$est) || is.infinite(speed$est)) {
      out$ctsd <- NA
    } else {
      out_ctsd <- fix_unit(speed$est, speed$unit)
      out$ctsd <- paste(
        scales::label_comma(.1)(out_ctsd$value), 
        abbrv_unit(out_ctsd$unit))
    }
    
    if (is.na(distance$est)) {
      out$dist <- NA
    } else {
      out_dist <- fix_unit(distance$est, distance$unit, convert = TRUE)
      out$dist <- paste(
        scales::label_comma(.1)(out_dist$value), 
        abbrv_unit(out_dist$unit))
    }
    
    out$ctsd_err <- speed_error$est
    out$ctsd_err_min <- speed_error$lci
    out$ctsd_err_max <- speed_error$uci
    out$dist_err <- distance_error$est

  } # end of if (target == "ctsd")
  
  return(out)
  
} # end of function, .build_tbl()


#' @title Chooser input
#'
#' @noRd
#'
chooserInput <- function(inputId, 
                         leftLabel, rightLabel,
                         leftChoices, rightChoices,
                         size = 5, multiple = FALSE,
                         width = 100) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple) multiple <- "multiple"
  else multiple <- NULL
  
  class <- paste("shiny-input-select form-control")
  
  tagList(
    singleton(tags$head(
      tags$script(src = "chooser-binding.js"))),
    
    div(id = inputId, # class = "chooser",
        class = "chooser form-group shiny-input-container",
        fluidRow(
          style = paste("display: flex;",
                        "justify-content: space-evenly;",
                        "align-items: center !important;",
                        "padding: 10px;"),
          column(
            width = 5, align = "center",
            div(class = "chooser-container chooser-left-container",
                tags$label(leftLabel,
                           style = "font-size: 17px;",
                           class = "cl-jgl"),
                tags$br(),
                tags$select(class = paste("left", class),
                            size = size, 
                            multiple = multiple,
                            leftChoices))
          ),
          column(
            width = 2, align = "center",
            fluidRow(
              style = "display: inherit;",
              p(style = "margin-top: 20px;"),
              style = paste("display: inline;",
                            "position: relative;",
                            "top: 50%;"),
              icon("circle-right", class = "right-arrow fa-2x cl-jgl"),
              p(),
              icon("circle-left", class = "left-arrow fa-2x cl-jgl"))
          ),
          column(
            width = 5, align = "center",
            div(class = "chooser-container chooser-right-container",
                tags$label(rightLabel,
                           style = "font-size: 17px;",
                           class = "cl-jgl"),
                tags$br(),
                tags$select(class = paste("right", class),
                            size = size, 
                            multiple = multiple,
                            rightChoices))
          )
        )) # end of div
  ) # end of tagList
}

shiny::registerInputHandler("shinyjsexamples.chooser", 
                            function(data, ...) {
                              if (is.null(data)) NULL
                              else list(A = as.character(data$left),
                                        B = as.character(data$right))
                            }, force = TRUE)


#' Parallel lapply
#'
#' @description Parallel lapply adapted from ctmmweb.
#'
#' @param obj Input list of two lists (telemetry and CTMM objects).
#' @param fun the function to be applied to each element of `obj`.
#' @param cores integer. Number of cores.
#' @param parallel logical. Uses a single core when FALSE.
#' @keywords internal
#'
#' @noRd
#'
par.lapply <- function(obj,
                       fun, 
                       cores = NULL,
                       parallel = TRUE,
                       win_init = expression({
                         requireNamespace("ctmm", quietly = TRUE)})) {
  
  num_cores <- parallel::detectCores(logical = FALSE)
  
  if (parallel) {
    if (!is.null(cores) && cores > 0)
      cluster_size <- cores
    
    if (!is.null(cores) && cores < 0)
      cluster_size <- max(num_cores + cores, 1)
    
    sysinfo <- Sys.info()
    tryCatch({
      if (sysinfo["sysname"] == "Windows") {
        if (is.null(cores))
          cluster_size <- min(length(obj), num_cores * 2)
        
        message(
          " Running in parallel SOCKET cluster of ",
          cluster_size, "...")
        
        cl <- parallel::makeCluster(cluster_size, outfile = "")
        parallel::clusterExport(cl, c("win_init"), envir = environment())
        parallel::clusterEvalQ(cl, eval(win_init))
        out <- parallel::parLapplyLB(cl = cl, X = obj, fun = fun)
        parallel::stopCluster(cl)
        # message(" ... done!")
        
      } else {
        if (is.null(cores))
          cluster_size <- min(length(obj), num_cores * 4)
        
        message(
          " Running in parallel with mclapply cluster of ",
          cluster_size, "...")
        
        out <- parallel::mclapply(obj, fun, mc.cores = cluster_size)
        # message(" ... done!")
      }
      
    }, error = function(e) {
      cat(crayon::bgRed$white(
        "Parallel error, try restarting R session.\n"))
      print(e)
      
    }) # end of tryCatch
    
  } else {
    out <- lapply(obj, fun)
  }
  
  return(out)
}

#' Parallel model selection
#'
#' @description Parallel model selection, adapted from ctmmweb.
#'
#' @param data telemetry object from as.telemetry().
#' @param guess ctmm object from ctmm.guess().
#' @param parallel True/false. Uses a single core when FALSE.
#' @keywords internal
#'
#' @noRd
#'
par.ctmm.select <- function(data,
                            guess,
                            trace = TRUE,
                            cores = NULL,
                            parallel = TRUE) {
  
  if (class(data)[1] != "list" && class(data[[1]])[1] != "ctmm") {
    stop("'input' must be a list of ctmm objects.")
  } else {
    if (length(data) != length(guess)) 
      stop("'data' and 'guess' must be same length.")
    
    input <- lapply(seq_along(data),
                    function(x) list(data[[x]], 
                                     guess[[x]],
                                     trace))
  }
  
  # if (parallel && length(data) > 1)
  #   message("No. of cores detected: ",
  #           parallel::detectCores(logical = FALSE))
  
  fall_back <- function(f1, f1_args_list, f2, f2_args_list, msg) {
    out <- try(do.call(f1, f1_args_list))
    if (inherits(out, "try-error")) {
      cat(crayon::white$bgBlack(msg), "\n")
      out <- do.call(f2, f2_args_list)
    }
    return(out)
  }
  
  try_select <- function(input) {
    
    fall_back(ctmm::ctmm.select,
              list(input[[1]],
                   CTMM = input[[2]],
                   control = list(method = "pNewton",
                                  cores = internal_cores),
                   trace = input[[3]]),
              ctmm::ctmm.select,
              list(input[[1]],
                   CTMM = input[[2]],
                   control = list(cores = internal_cores),
                   trace = input[[3]]),
              paste0("ctmm.select() failed with pNewton,",
                     "switching to Nelder-Mead."))
  }
  
  if (length(input) == 1) {
    # Process one individual on multiple cores:
    internal_cores <- if (parallel) -1 else 1
    out <- try(try_select(input[[1]]))
    
  } else {
    # Process multiple individuals:
    internal_cores <- 1
    out <- try(par.lapply(input,
                          try_select,
                          cores = cores,
                          parallel = parallel))
  }
  
  if (any(has_error(out))) {
    message("Error in model selection")
    return(NULL)
  }
  
  return(out)
}


#' Parallel model fit
#'
#' @description Parallel model fit, adapted from ctmmweb.
#'
#' @param data telemetry object from as.telemetry().
#' @param guess ctmm object from ctmm.guess().
#' @param parallel True/false. Uses a single core when FALSE.
#' @keywords internal
#'
#' @noRd
#'
par.ctmm.fit <- function(data,
                         guess,
                         cores = NULL,
                         parallel = TRUE) {
  
  # if (parallel && length(data) > 1)
  #   message("No. of cores detected: ",
  #           parallel::detectCores(logical = FALSE))
  
  input <- lapply(seq_along(data),
                  function(x) list(data[[x]], guess[[x]]))
  
  fall_back <- function(f1, f1_args_list, f2, f2_args_list, msg) {
    out <- try(do.call(f1, f1_args_list))
    if (inherits(out, "try-error")) {
      cat(crayon::white$bgBlack(msg), "\n")
      out <- do.call(f2, f2_args_list)
    }
    return(out)
  }
  
  try_fit <- function(input) {
    fall_back(ctmm::ctmm.fit,
              list(input[[1]],
                   CTMM = input[[2]],
                   method = "pHREML",
                   control = list(cores = internal_cores)),
              ctmm::ctmm.fit,
              list(input[[1]],
                   CTMM = input[[2]],
                   method = "ML",
                   control = list(cores = internal_cores)),
              paste0("ctmm.fit() failed with pHREML,",
                     "switching to ML."))
  }
  
  if (length(input) == 1) {
    # Process one individual on multiple cores:
    internal_cores <- if (parallel) -1 else 1
    out <- try(try_fit(input[[1]]))
    
  } else {
    # Process multiple animals on multiple cores:
    internal_cores <- 1
    out <- try(par.lapply(input,
                          try_fit,
                          cores = cores,
                          parallel = parallel))
  }
  
  if (any(has_error(out))) {
    message("Error in model fit")
    return(NULL)
  }
  
  return(out)
}


#' Parallel home range estimation
#'
#' @param input Telemetry (data) and model (fit) lists.
#' @inheritParams par_lapply
#'
#' @noRd
#'
par.akde <- function(data,
                     fit,
                     cores = NULL,
                     trace = TRUE,
                     parallel = TRUE) {
  
  if (class(fit)[1] != "list" && class(fit[[1]])[1] != "ctmm") {
    stop("'input' must be a list of ctmm objects.")
  } else {
    if (length(data) != length(fit)) 
      stop("'data' and 'fit' must be same length.")
    input <- lapply(seq_along(data),
                    function(x) list(data[[x]], 
                                     fit[[x]]))
  }
  
  try_akde <- function(input) {
    out <- tryCatch({
      ctmm::akde(input[[1]], input[[2]])
    }, error = function(e) return(NULL))
    return(out)
  }
  
  if (length(input) == 1) {
    # Process one individual on multiple cores:
    internal_cores <- if (parallel) -1 else 1
    out_akde <- try(try_akde(input[[1]]))
    
  } else {
    # Process multiple animals on multiple cores:
    internal_cores <- 1
    out_akde <- par.lapply(input,
                           try_akde, 
                           cores = cores,
                           parallel = parallel)
  }
  
  if (any(has_error(out_akde))) {
    msg_log(
      style = "danger",
      message = paste0("Home range estimation ",
                       msg_danger("failed"), "."))
  }
  
  set.seed(NULL)
  return(out_akde)
}


#' Parallel speed estimation
#'
#' @param input Telemetry and model list, adapted from ctmmweb.
#' @inheritParams par_lapply
#'
#' @noRd
#'
par.speed <- function(data,
                      fit,
                      cores = NULL,
                      trace = TRUE,
                      parallel = TRUE,
                      seed = NULL) {
  
  if (class(fit)[1] != "list" && class(fit[[1]])[1] != "ctmm")
    stop("'input' must be a list of ctmm objects.")
  
  is_one <- length(data) == 1
  if (length(data) != length(fit)) 
    stop("'data' and 'fit' must be same length.")
  input <- lapply(seq_along(data),
                  function(x) list(data[[x]], 
                                   fit[[x]],
                                   seed[[x]]))
  
  try_speed <- function(input) {
    set.seed(input[[3]])
    out <- tryCatch({
      ctmm::speed(input[[1]],
                  input[[2]],
                  cores = internal_cores,
                  trace = trace)
    }, error = function(e) return(NULL))
    return(out)
  }
  
  if (length(input) == 1) {
    
    # Process one individual on multiple cores:
    internal_cores <- if (parallel) -1 else 1
    out_speed <- try(try_speed(input[[1]]))
    
  } else {
    
    # Process multiple animals on multiple cores:
    internal_cores <- 1
    out_speed <- par.lapply(input,
                            try_speed, 
                            cores = cores,
                            parallel = parallel)
  }
  
  if (any(has_error(out_speed))) {
    msg_log(
      style = "danger",
      message = paste0("Speed estimation ",
                       msg_danger("failed"), "."))
  }
  
  if (is_one) out_speed <- list(out_speed)
  
  set.seed(NULL)
  return(out_speed)
}


#' Align lists
#'
#' @noRd
#'
align_lists <- function(...) {
  list_of_lists <- list(...)
  if (dplyr::n_distinct(lengths(list_of_lists)) != 1) 
    stop("Input lists must be of the same length.")
  
  out_lists <- lapply(
    seq_along(list_of_lists[[1]]), function(i) 
      lapply(list_of_lists, "[[", i))
  
  if (length(out_lists) == 0) out_lists <- NULL
  return(out_lists)
}

CI.upper <- Vectorize(function(k, level) {
  stats::qchisq((1 - level)/2, k, lower.tail = FALSE) / k} )

CI.lower <- Vectorize(function(k, level) {
  stats::qchisq((1 - level)/2, k, lower.tail = TRUE) / k} )
