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
#' @noRd
fix_time <- function(value, unit)  {

  if (!is.numeric(value) || !is.integer(value)) {
    stop("`value` argument must be numeric.")
  }
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

  # Check if value is equal to 1 (e.g. 1 hour):
  if(value == 1 && x == "year") y <- "year"
  if(value == 1 && x == "month") y <- "month"
  if(value == 1 && x == "day") y <- "day"
  if(value == 1 && x == "hour") y <- "hour"
  if(value == 1 && x == "minute") y <- "minute"
  if(value == 1 && x == "second") y <- "second"

  out_value <- ifelse(
    value %% 1 == 0,
    scales::label_comma(accuracy = 1)(value),
    scales::label_comma(accuracy = .1)(value))

  out <- c(out_value, unit)
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
fix_spatial <- function(value, units) {

  if (!is.numeric(value) || !is.integer(value)) {
    stop("`value` argument must be numeric.")
  }
  if (!is.character(unit)) {
    stop("`unit` argument must be a character string.")
  }

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
#' @keywords internal
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
