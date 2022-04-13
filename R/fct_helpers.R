#' Abbreviate time units
#'
#' @description Create abbreviations of time units for info blocks.
#' @param unit Character. A character vector for a time units.
#'
#' @return Returns a character vector with one element.
#'
#' @examples
#' \dontrun{
#' movedesign::abbreviate_time("years")
#' }
#'
#' @export
abbreviate_time <- function(unit) {

  if (!is.character(unit)) {
    stop("`unit` argument must be a character string.")
  }

  all_units <- c("year", "month", "day", "hour", "minute", "second")
  x <- gsub("(.)s$", "\\1", unit)

  val <- all_units[pmatch(x, all_units, duplicates.ok = TRUE)]
  if (any(is.na(val))) {
    stop("Invalid time unit: ", paste(x[is.na(val)], collapse = ", "),
         call. = FALSE)
  }

  if (x == "year" ) y <- "yr"
  if (x == "month" ) y <- "mth"
  if (x == "day" ) y <- "d"
  if (x == "hour" ) y <- "hr"
  if (x == "minute" ) y <- "min"
  if (x == "seconds" ) y <- "sec"

  return(y)

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
#' movedesign::fix_time(1, "hours")
#' }
#' @keywords internal
#'
#' @importFrom dplyr case_when
#' @importFrom ctmm `%#%`
#' @noRd
fix_time <- function(value, unit, adjust = FALSE)  {

  if (!is.character(unit)) {
    stop("`unit` argument must be a character string.")
  }

  all_units <- c("year", "month", "day", "hour", "minute", "second")
  y <- gsub("(.)s$", "\\1", unit)

  var <- all_units[pmatch(y, all_units, duplicates.ok = TRUE)]
  if (any(is.na(var))) {
    stop("Invalid time unit: ", paste(y[is.na(var)], collapse = ", "),
         call. = FALSE)
  }

  x <- value
  secs <- round(x %#% y, 0)

  if (adjust) {
    if (any(secs < 60)) {
      new.unit <- "second"
    } else if (any(secs < 3600)) {
      new.unit <- "minute"
    } else if (any(secs < 86400)) {
      new.unit <- "hour"
    } else if (any(secs < 1 %#% "month")) {
      new.unit <- "day"
    } else if (any(secs < 1 %#% "year")) {
      new.unit <- "month"
    } else {
      new.unit <- "year"
    }
    x <- new.unit %#% value %#% y
    y <- new.unit
  }

  x <- dplyr::case_when(
    x %% 1 == 0 ~ scales::label_comma(accuracy = 1)(x),
    x %% 1 != 0 ~ scales::label_comma(accuracy = .1)(x))

  # Check if value is equal to 1 (e.g. 1 hour):
  y <- dplyr::case_when(x < 1 || x > 1 ~ paste0(y, "s"),
                 x == 1 ~ y)

  out <- c(x, y)
  return(out)

}

#' Fix spatial values and units
#'
#' @description Correctly convert spatial units from ctmm outputs for blocks.
#' @keywords internal
#'
#' @param value numeric, integer. For example, 1.
#' @param unit character vector of spatial units. For example, "square/kilometers".
#' @return A list with the corrected value and the corrected unit.
#'
#' @noRd
fix_spatial <- function(value, unit) {

  if (!is.character(unit)) {
    stop("`unit` argument must be a character string.")
  }

  all_units <- c("square kilometers",
                 "square meters",
                 "km^2",
                 "m^2",
                 "ha")
  x <- gsub("(.)s$", "\\1", unit)

  val <- all_units[pmatch(x, all_units, duplicates.ok = TRUE)]
  if (any(is.na(val))) {
    stop("Invalid time unit: ", paste(x[is.na(val)], collapse = ", "),
         call. = FALSE)
  }

  value <- value %#% x

  if(value >= 1e6) {
    out_value <- "km^2" %#% value
    unit <- "km^2"
    unit_html <- HTML(paste0("km", tags$sup(2)))
  }

  if(1e4 > value || value < 1e6) {
    out_value <- "ha" %#% value
    unit <- "ha"
    unit_html <- "ha"
  }

  if(value <= 1e4) {
    out_value <- value
    unit <- "m^2"
    unit_html <- HTML(paste0("km", tags$sup(2)))
  }

  out_value <- ifelse(
    out_value %% 1 == 0,
    scales::label_comma(accuracy = 1)(out_value),
    scales::label_comma(accuracy = .1)(out_value))

  return(c(out_value, unit, unit_html))

}

#' Prepare movement model
#'
#' @description Prepare parameters for movement data simulation
#' @keywords internal
#'
#' @param tau_p0 numeric, integer. position autocorrelation timescale.
#' @param tau_p0_units character vector of tau p units.
#' @param tau_v0 numeric, integer. velocity autocorrelation timescale.
#' @param tau_v0_units character vector of tau v units.
#' @param sigma0 numeric, integer. semi-variance or sigma.
#' @param tau_p0_units character vector of sigma units.
#'
#' @noRd
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
#' @keywords internal
#'
#' @param mod0 movement model
#' @param dur0 numeric, integer. sampling duration.
#' @param dur0_units character vector of sampling duration units.
#' @param tau_v0 numeric, integer. sampling interval.
#' @param tau_v0_units character vector of sampling interval units.
#' @param seed0 random seed value for simulation.
#'
#' @noRd
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

#' Prepare semi-variance data
#'
#' @description Prepare semi-variance data
#' @keywords internal
#'
#' @noRd
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

#' Simulate GPS battery life decay
#'
#' @description Simulate GPS battery life decay
#'
#' @param data data.frame. A dataset with frequencies.
#' @param k0 Numeric. The rate constant.
#' @param yrange Numeric. Value for the range of y, in y units.
#' @param subset Character. Cut-off for frequencies
#' @param minrate Minimum frequency.
#' @keywords internal
#'
#' @noRd
simulate_gpsdecay <- function(data,
                              k0,
                              yrange0,
                              subset,
                              minrate) {

  tmp <- data$freq_hrs[match(minrate, data$nu_notes)]

  newdata <- data %>%
    dplyr::filter(highlight == "Y") %>%
    dplyr::filter(freq_hrs >= tmp) %>%
    dplyr::select(nu_notes, nu, freq_hrs)

  ylow <- 0
  yrange <- "months" %#% yrange0

  x <- newdata$freq_hrs - newdata$freq_hrs[1]
  newdata$dur_mth <- yrange * exp(-k0 * x) + ylow
  newdata$color <- as.factor(dplyr::case_when(
    newdata$dur_mth < subset ~ "red",
    newdata$dur_mth >= subset ~ "blue"))
  newdata$id <- 1:nrow(newdata)

  newdata$n <- NA
  for(i in 1:nrow(newdata)) {
    if(newdata$dur_mth[i] <= 0.033) {
      newdata$n[i] <- 0
      newdata$dur_mth[i] <- 0
    } else {
      newdata$n[i] <- length(
        seq(1, round((newdata$dur_mth[i] %#% "months"), 0),
            by = newdata$nu[i]))
    }
  }

  return(newdata)
}


#' Convert as.telemetry to data.frame
#'
#' @description Convert as.telemetry to data.frame
#' @keywords internal
#'
#' @noRd
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
#' @keywords internal
#'
#' @noRd
subset_timeframe <- function(var, value) {
  as.data.frame(var) %>% dplyr::top_frac(value)
}
