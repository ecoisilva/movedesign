#' Abbreviate units
#'
#' @description Create abbreviations of units.
#' @param unit Character. A character vector for a unit.
#'
#' @return Returns a character vector with one element.
#'
#' @examples
#' \dontrun{
#' movedesign::abbrv_unit("square kilometers")
#' }
#'
#' @export
abbrv_unit <- function(unit) {

  if (missing(unit)) {
    stop("`unit` argument not provided.")
  }

  if (!is.character(unit)) {
    stop("`unit` argument must be a character string.")
  }

  all_units <- c("year", "month", "week",
                 "day", "hour", "minute", "second",
                 "kilometer", "meter", "km", "m",
                 "km^2", "m^2", "ha",
                 "square kilometer", "square meter", "hectare",
                 "kilometers/hour", "meters/second",
                 "kilometers/day" , "meters/day")


  x <- gsub("(.)s$", "\\1", unit)
  var <- all_units[pmatch(x, all_units, duplicates.ok = TRUE)]

  if (any(is.na(var))) {
    stop("Invalid unit: ", paste(x[is.na(var)], collapse = ", "),
         call. = FALSE)
  }

  out <- NULL
  if (x == "year" ) out <- "yr"
  if (x == "month" ) out <- "mth"
  if (x == "week" ) out <- "wk"
  if (x == "day" ) out <- "d"
  if (x == "hour" ) out <- "hr"
  if (x == "minute" ) out <- "min"
  if (x == "second" ) out <- "sec"

  if (x == "kilometer" ) out <- "km"
  if (x == "meter" ) out <- "m"

  if (x == "square kilometer" | x == "km^2") out <- "km\u00B2"
  if (x == "square meter" | x == "m^2") out <- "m\u00B2"
  if (x == "hectare" | "ha") out <- "ha"

  if (x == "kilometers/hour" ) out <- "km/h"
  if (x == "meters/second" ) out <- "m/s"
  if (x == "kilometers/day" ) out <- "km/day"
  if (x == "meters/day" ) out <- "m/day"

  return(out)

}

#' Fix values and units of space and time
#'
#' @description Correctly convert values and units from ctmm outputs for blocks.
#'
#' @param value numeric, integer. For example, 1.
#' @param unit character vector of time units. For example, "hours" or "meters".
#' @return A list with the corrected value and the corrected unit.
#'
#' @examples
#' \dontrun{
#' movedesign::fix_unit(1, "hours")
#' }
#' @keywords internal
#'
#' @importFrom dplyr case_when
#' @importFrom ctmm `%#%`
#' @noRd
fix_unit <- function(value, unit,
                     ui = FALSE,
                     convert = FALSE)  {

  if (!is.character(unit)) {
    stop("`unit` argument must be a character string.")
  }

  all_units <- c("year", "month", "week",
                 "day", "hour", "minute", "second",
                 "kilometer", "meter", "km", "m",
                 "square kilometer", "square meter", "hectare",
                 "km^2", "m^2", "ha",
                 "kilometers/day", "kilometer/day",
                 "meters/second", "meter/second")

  units_tm <- all_units[1:7]
  units_sp <- all_units[8:11]
  units_ar <- all_units[12:17]
  units_vl <- all_units[18:21]

  x <- gsub("(.)s$", "\\1", unit)
  var <- all_units[pmatch(x, all_units, duplicates.ok = TRUE)]

  if (any(is.na(var))) {
    stop("Invalid unit: ", paste(x[is.na(var)], collapse = ", "),
         call. = FALSE)
  }

  # Convert value:

  y <- ifelse(convert, value %#% x, value)

  if ((x %in% units_tm) & convert) {
    if (any(y < 60)) {
      x_new <- "second"
    } else if (any(y < 3600)) {
      x_new <- "minute"
    } else if (any(y < 86400)) {
      x_new <- "hour"
    } else if (any(y < 1 %#% "month")) {
      x_new <- "day"
    } else if (any(y < 1 %#% "year")) {
      x_new <- "month"
    } else {
      x_new <- "year"
    }
    y <- x_new %#% y
    x <- x_new
  }

  if ((x %in% units_sp) & convert) {
    x_new <- ifelse(y >= 1000, "km", "m")
    y <- x_new %#% y
    x <- x_new
  }

  if ((x %in% units_ar) & convert) {
    if(y >= 1e6) {
      x_new <- "km^2"
    } else if (1e4 > y || y < 1e6) {
      x_new <- "ha"
    } else if (y <= 1e4) {
      x_new <- "m^2"
    }
    y <- x_new %#% y
    x <- x_new
  }

  if ((x %in% units_ar)) {
    if (x == "square kilometer" | x == "km^2") {
      x_html <- HTML(paste0("km", tags$sup(2)))
    } else if (x == "square meter" | x == "m^2") {
      x_html <- HTML(paste0("m", tags$sup(2)))
    } else if (x == "hectare" | x == "ha") {
      x_html <- "ha"
    }
  }

  if ((x %in% units_vl) & convert) {
   #TODO
  }

  # Round value:

  if (ui) {
    y <- dplyr::case_when(
      y %% 1 == 0 ~ scales::label_comma(accuracy = 1)(y),
      y %% 1 != 0 ~ scales::label_comma(accuracy = .1)(y))
  } else {
    y <- dplyr::case_when(
      y %% 1 == 0 ~ round(y, 0),
      y %% 1 != 0 ~ round(y, 1))
  }

  # Check if value is equal to 1 (e.g. 1 hour), adjust unit:

  if (x %in% units_tm) {
    x <- dplyr::case_when(y < 1 || y > 1 ~ paste0(x, "s"),
                          y == 1 ~ x)
  }

  # Show units as HTML:
  x <- ifelse(ui, x_html, x)

  out <- data.frame(value = y, unit = x)
  return(out)

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
prepare_mod <- function(tau_p,
                        tau_p_units,
                        tau_v,
                        tau_v_units,
                        sigma,
                        sigma_units) {

  # characteristic timescales
  taup <- tau_p %#% tau_p_units # position autocorrelation
  tauv <- tau_v %#% tau_v_units # velocity autocorrelation

  sig <- sigma %#% sigma_units # spatial variance

  # generate the mod0
  mod <- ctmm::ctmm(tau = c(taup, tauv),
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
#' @param k Numeric. The rate constant.
#' @param yrange Numeric. Value for the range of duration (y).
#' @param yunits Numeric. Value for the range of duration (y).
#' @param subset Character. Cut-off for frequencies
#' @param max_interval Minimum frequency.
#' @keywords internal
#'
#' @noRd
simulate_gps <- function(data,
                         k,
                         yrange,
                         yunits,
                         cutoff,
                         max_x,
                         min_x,
                         simplified = FALSE) {

  x_max <- data$freq_hrs[match(max_x, data$nu_notes)]
  x_min <- data$freq_hrs[match(min_x, data$nu_notes)]

  if(simplified) {

    # k <- 8.046066
    newdata <- data %>%
      dplyr::select(nu_notes, nu, freq_hrs) %>%
      dplyr::filter(freq_hrs >= x_max) %>%
      subset(freq_hrs <= x_min)

    y0 <- yrange

    x <- newdata$freq_hrs
    y <- y0 / (1 + exp(log(x) - log(k)))
    newdata$dur <- y %#% yunits
    newdata$dur_mth <- "months" %#% newdata$dur

    if (max(newdata$dur) > cutoff) {
      newdata$color <- as.factor(dplyr::case_when(
        newdata$dur < cutoff ~ "red",
        newdata$dur >= cutoff ~ "blue"))
    } else { newdata$color <- "red" }

    newdata$id <- 1:nrow(newdata)

  } else {
    newdata <- data %>%
      dplyr::filter(highlight == "Y") %>%
      dplyr::filter(freq_hrs >= x_max) %>%
      dplyr::select(nu_notes, nu, freq_hrs)

    ylow <- 0
    y0 <- "months" %#% yrange %#% yunits

    x <- newdata$freq_hrs - newdata$freq_hrs[1]
    newdata$dur_mth <- y0 * exp(-k * x) + ylow
    newdata$dur <- newdata$dur_mth %#% "months"
    newdata$color <- as.factor(dplyr::case_when(
      newdata$dur < cutoff ~ "red",
      newdata$dur >= cutoff ~ "blue"))
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
      }}
  }

  return(newdata)
}

#' Estimate computation time
#'
#' @description Calculate computation time of ctmm functions.
#' @keywords internal
#'
#' @noRd
#'
estimate_time <- function(data, parallel = TRUE) {

  start_test <- Sys.time()
  units <- "minute"
  n <- 150

  dti <- data[[2,"t"]] - data[[1,"t"]]

  if (nrow(data) < 150) {

    expt_min <- 0
    expt <- expt_max <- 1
    expt_units <- units

  } else {

    tmpdat <- data[1:n, ]
    guess <- ctmm::ctmm.guess(tmpdat, interactive = FALSE)
    inputList <- list(list(tmpdat, guess))
    fit <- par_ctmm.select(inputList, trace = FALSE, parallel = TRUE)
    total_time <- difftime(Sys.time(), start_test,
                           units = "secs") %>%
      as.numeric()

    expt <- ((total_time/n) * nrow(data))
    expt <- ceiling(units %#% expt)

    if(expt >= 15) {
      expt_max <- round_any(expt, 5, f = ceiling)
      expt_min <- expt - 5
    } else if(expt < 15 & expt > 5) {
      expt_max <- round_any(expt, 1, f = ceiling)
      expt_min <- expt - 3
    } else {
      expt_max <- round_any(expt, 1, f = ceiling)
      expt_min <- expt_max
    }
    expt_units <- ifelse(expt_max == 1, units, "minutes")
  }

  outputs <- data.frame(expt, expt_min, expt_max, expt_units)
  names(outputs) <- c("mean", "min", "max", "units")
  return(outputs)

}


#' Calculate distance
#'
#' @description Calculate distance traveled
#' @keywords internal
#'
#' @noRd
#'
calc_dist <- function(data) {

  tmpdat <- data.frame(
    x = data$x,
    y = data$y)

  tmpdist <- list()
  for(i in 2:nrow(data)) {
    tmpdist[[i]] <-
      sqrt((tmpdat$x[i] - tmpdat$x[i-1])^2 +
             (tmpdat$y[i] - tmpdat$y[i-1])^2)
  }
  dist <- c(0, do.call("rbind", tmpdist))
  return(dist)

  # dist <- c(0, sqrt((data$x - lag(data$x))^2 +
  #                     (data$y - lag(data$y))^2)[-1])
  # return(dist)

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
par_ctmm.select <- function(input, cores = NULL,
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
    # Process multiple animals on multiple cores: #TODO

    internal_cores <- 1
    res <- try(ctmmweb::par_lapply(input,
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
par_ctmm.fit <- function(input,
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
    # Process multiple animals on multiple cores: #TODO
    internal_cores <- 1
    res <- try(ctmmweb::par_lapply(input,
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
par_speed <- function(input,
                      cores = NULL,
                      parallel = TRUE) {

  speed_calc <- function(input) {

    message("Calculating speed:")
    ctmm::speed(input[[1]],
                input[[2]],
                cores = internal_cores,
                trace = TRUE)
  }

  if (length(input) == 1) {
    # Process one individual on multiple cores:

    internal_cores <- if (parallel) -1 else 1
    res <- try(speed_calc(input[[1]]))

  } else {

    internal_cores <- 1
    res <- ctmmweb::par_lapply(input, speed_calc, cores, parallel)

  }

  if (any(has_error(res))) {
    cat(crayon::bgYellow$red("Error in speed calculation\n"))
  }

  return(res)
}

