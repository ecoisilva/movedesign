#' @title movedesign ggplot2 custom theme
#'
#' @description Custom ggplot2 theme for movedesign plot outputs.
#'
#' @author Ines Silva \email{i.simoes-silva@@hzdr.de}
#'
#' @param ft_size Base font size.
#'
#' @export
#'
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


#' Abbreviate time units
#'
#' @description Create abbreviations of time units for info blocks.
#' @param unit Character. A character vector for a time units.
#'
#' @return Returns a character vector with one element.
#' @export
#'
#' @examples
#' movedesign::abbreviate_time("years")
#'
abbreviate_time <- function(unit) {

  if(unit == "years" || unit == "year" ) out_short <- "yr"
  if(unit == "months" || unit == "month" ) out_short <- "mth"
  if(unit == "days" || unit == "day" ) out_short <- "d"
  if(unit == "hours" || unit == "hour" ) out_short <- "hr"
  if(unit == "minutes" || unit == "minute" ) out_short <- "min"
  if(unit == "seconds" || unit == "seconds" ) out_short <- "sec"

  return(out_short)
}


#' Fix values and units of time
#'
#' @description Correctly convert time values and units from ctmm outputs for blocks.
#'
#' @param value numeric, integer. For example, 1.
#' @param unit character vector of time units. For example, "hours".
#' @return A list with the corrected value and the corrected unit.
#'
#' @examples
#' \dontrun{
#' movedesign::fix_timeunits(1, "hours")
#' }
#'
#' @export
fix_timeunits <- function(value, unit)  {

  # Check if value is equal to 1 (e.g. 1 hour):
  if(value == 1 && (unit == "years" || unit == "year" ))
    unit <- "year"
  if(value == 1 && (unit == "months" || unit == "month" ))
    unit <- "month"
  if(value == 1 && (unit == "days" || unit == "day" ))
    unit <- "day"
  if(value == 1 && (unit == "hours" || unit == "hour" ))
    unit <- "hour"
  if(value == 1 && (unit == "minutes" || unit == "minute" ))
    unit <- "minute"
  if(value == 1 && (unit == "seconds" || unit == "second" ))
    unit <- "second"

  out_value <- ifelse(
    value %% 1 == 0,
    scales::label_comma(accuracy = 1)(value),
    scales::label_comma(accuracy = .1)(value))

  out <- c(out_value, unit)
  return(out)

}

#' Fix values and spatial values and units
#'
#' @description Correctly convert spatial units from ctmm outputs for blocks.
#'
#' @export
fix_spUnits <- function(value, units) {

  # type = "speed"
  value <- value %#% units

  if(value >= 1e6) {
    out_value <- "km^2" %#% value
    units <- "km^2"
    units_html <- HTML(paste0("km", tags$sup(2)))
  }

  if(1e4 > value || value < 1e6) {
    out_value <- "ha" %#% value
    units <- "ha"
    units_html <- "ha"
  }

  if(value <= 1e4) {
    out_value <- value
    units <- "m^2"
    units_html <- HTML(paste0("km", tags$sup(2)))
  }

  out_value <- ifelse(out_value %% 1 == 0,
                      scales::label_comma(accuracy = 1)(out_value),
                      scales::label_comma(accuracy = .1)(out_value))

  return(c(out_value, units, units_html))

}

#' Prepare movement model
#'
#' @description Prepare parameters for movement data simulation
#'
prepare_pars <- function(tau_p0,
                         tau_p0_units,
                         tau_v0,
                         tau_v0_units,
                         sigma0,
                         sigma0_units) {

  # characteristic timescales
  tau_p <- tau_p0 %#% tau_p0_units # position autocorrelation
  tau_v <- tau_v0 %#% tau_v0_units # velocity autocorrelation
  # tau_p dictates the animal’s home-range crossing time

  sig <- sigma0 %#% sigma0_units # spatial variance

  # generate the mod0
  mod <- ctmm::ctmm(tau = c(tau_p, tau_v),
                    isotropic = TRUE,
                    sigma = sig,
                    mu = c(0,0))

  return(mod)
}

#' Simulate movement data
#'
#' @description Simulate movement data through ctmm
#'
simulate_data <- function(mod0,
                          dur0,
                          dur0_units,
                          dti0,
                          dti0_units,
                          seed0) {

  dur <- dur0 %#% dur0_units # duration
  dti <- round(dti0 %#% dti0_units, 0) # sampling interval

  t0 <- seq(0, dur, by = dti)
  dat <- ctmm::simulate(mod0, t = t0, seed = seed0)
  dat <- ctmm:::pseudonymize(dat)
  dat$index <- 1:nrow(dat)

  return(dat)
}

CI.upper <- Vectorize(function(k, level) {
  stats::qchisq((1 - level)/2, k, lower.tail = FALSE) / k} )

CI.lower <- Vectorize(function(k, level) {
  stats::qchisq((1 - level)/2, k, lower.tail = TRUE) / k} )

#' Plot home range
#'
#' @description Plotting home range output from ctmm
#'
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

    { if("95% high CI" %in% levels) p1 } +
    { if("Estimate" %in% levels) p2 } +
    { if("95% low CI" %in% levels) p3 } +

    ggplot2::geom_path(dat,
                       mapping = ggplot2::aes(
                         x = x, y = y,
                         color = timestamp), size = 0.5,
                       alpha = .6) +
    ggplot2::geom_point(dat,
                        mapping = ggplot2::aes(
                          x = x, y = y,
                          color = timestamp), size = 2) +

    ggplot2::labs(x = "X coordinate",
                  y = "Y coordinate") +

    ggplot2::scale_x_continuous(
      labels = scales::comma) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(tmp, NA)) +
    viridis::scale_color_viridis(
      name = "Tracking time:",
      option = "D", trans = "time",
      breaks = c(min(dat$timestamp),
                 max(dat$timestamp)),
      labels = c("Start", "End")) +

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
#'
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

  show_col <- ifelse(show, hex_border, "white")
  show_alpha <- ifelse(show, 1, 0)

  p <- ggplot2::ggplot() +
    ggiraph::geom_polygon_interactive(
      data = pol_ud_high,
      mapping = ggplot2::aes(x = long, y = lat,
                             group = group),
      fill = NA, alpha = 0) +

    { if("95% high CI" %in% levels) p1 } +
    { if("Estimate" %in% levels) p2 } +
    { if("95% low CI" %in% levels) p3 } +

    ggplot2::geom_point(
      dat, mapping = ggplot2::aes(x = x, y = y),
      color = show_col, alpha = show_alpha) +

    ggplot2::geom_point(
      datsim, mapping = ggplot2::aes(x = x, y = y),
      color = hex_main, alpha = 0.7, size = 2) +

    ggplot2::labs(x = "X coordinate",
                  y = "Y coordinate") +

    ggplot2::scale_x_continuous(
      labels = scales::comma) +
    ggplot2::scale_y_continuous(
      labels = scales::comma) +

    theme_movedesign() +
    ggplot2::theme(legend.position = "none")

}

#' Prepare semi-variance data
#'
#' @description Prepare semi-variance data
#'
prepare_svf <- function(data, fraction = .65) {

  level <- 0.95
  SVF <- ctmm::variogram(data = data) # CI = "Gauss"
  vardat <- data.frame(SVF = SVF$SVF,
                       DOF = SVF$DOF,
                       lag = SVF$lag) %>%
    dplyr::slice_min(lag, prop = fraction) %>%
    dplyr::mutate(lag_days = lag/60/60/24)

  vardat$lag_days <- (vardat$lag)/60/60/24
  vardat$var_low95 <- "square kilometers" %#%
    ( vardat$SVF * CI.lower(vardat$DOF, level) )
  vardat$var_upp95 <- "square kilometers" %#%
    ( vardat$SVF * CI.upper(vardat$DOF, level) )
  vardat$var_low50 <- "square kilometers" %#%
    ( vardat$SVF * CI.lower(vardat$DOF, .5) )
  vardat$var_upp50 <- "square kilometers" %#%
    ( vardat$SVF * CI.upper(vardat$DOF, .5) )
  vardat$SVF <-"square kilometers" %#% vardat$SVF

  return(vardat)
}

#' Plot variogram
#'
#' @description Plot variogram from ctmm
#'
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

#' Simulate GPS battery life decay
#'
#' @description Simulate GPS battery life decay
#'
#' @param data data.frame. A dataset with frequencies.
#' @param k0 Numeric. The rate constant.
#' @param yrange Numeric. Value for the range of y, in y units.
#' @param subset Character. Cut-off for frequencies
#' @param minrate Minimum frequency.
#'
simulate_gpsdecay <- function(data,
                              k0,
                              yrange0,
                              subset,
                              minrate) {

  tmp <- data$freq_hrs[
    match(minrate, data$nu_notes)]

  newdata <- data %>%
    dplyr::filter(highlight == "Y") %>%
    dplyr::filter(freq_hrs >= tmp) %>%
    dplyr::select(nu_notes, nu, freq_hrs)

  ylow <- 0

  x <- newdata$freq_hrs - newdata$freq_hrs[1]
  newdata$dur <- yrange0 * exp(-k0 * x) + ylow
  newdata$color <- as.factor(dplyr::case_when(
    newdata$dur < subset ~ "red",
    newdata$dur >= subset ~ "blue"))
  newdata$id <- 1:nrow(newdata)

  newdata$n <- NA
  for(i in 1:nrow(newdata)) {
    if(newdata$dur[i] <= 0.0328766) {
      newdata$n[i] <- 0
      newdata$dur[i] <- 0
    } else {
      newdata$n[i] <- length(
        seq(1, round((newdata$dur[i] %#% "months"), 0),
            by = newdata$nu[i]))
    }
  }

  return(newdata)
}


#' Convert as.telemetry to data.frame
#'
#' @description Convert as.telemetry to data.frame
#'
as_tele_df <- function(data) {

  data_df <- list()
  for(i in 1:length(data)) {
    tempdf <- data[[i]]
    tempdf$id <- names(data)[i]
    data_df[[i]] <- tempdf
  }
  data_df <- do.call(rbind.data.frame, data_df)

  return(data_df)
}


#' Subset time frame
#'
#' @description Subset time frame
#'
subset_timeframe <- function(var, value) {
  as.data.frame(var) %>% dplyr::top_frac(value)
}
