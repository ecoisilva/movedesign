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
#' @noRd
abbrv_unit <- function(unit, ui_only = TRUE) {

  if (missing(unit))
    stop("`unit` argument not provided.")

  if (!is.character(unit))
    stop("`unit` argument must be a character string.")
  
  all_units <- c("year", "month", "week",
                 "day", "hour", "minute", "second",
                 "kilometer", "meter", "km", "m",
                 "km^2", "m^2", "ha",
                 "square kilometer", "square meter", "hectare",
                 "kilometers/hour", "meters/second",
                 "kilometers/day" , "meters/day",
                 "km/h", "m/s",
                 "km/day", "m/day")

  x <- gsub("(.)s$", "\\1", unit)
  var <- all_units[pmatch(x, all_units, duplicates.ok = TRUE)]

  if (any(is.na(var)))
    stop("Invalid unit: ", paste(x[is.na(var)], collapse = ", "),
         call. = FALSE)
  
  out <- x
  if (x == "year") out <- "yr"
  if (x == "month") out <- "mth"
  if (x == "week") out <- "wk"
  if (x == "day") out <- "d"
  if (x == "hour") out <- "hr"
  if (x == "minute") out <- "min"
  if (x == "second") out <- "sec"

  if (x == "kilometer") out <- "km"
  if (x == "meter") out <- "m"

  if (x == "square kilometer" || x == "km^2") 
    out <- ifelse(ui_only, "km\u00B2", "km^2")
  if (x == "square meter" || x == "m^2") 
    out <- ifelse(ui_only, "m\u00B2", "m^2")
  if (x == "hectare") out <- "ha"

  if (x == "kilometers/hour") out <- "km/h"
  if (x == "meters/second") out <- "m/s"
  if (x == "kilometers/day") out <- "km/day"
  if (x == "meters/day") out <- "m/day"

  return(out)

}

#' Fix values and units of space and time
#'
#' @description Correctly convert values and units for reporting.
#'
#' @param value numeric, integer. For example, 1.
#' @param unit character vector of time units. For example, "hours" or "meters".
#' @return A list with the corrected value and the corrected unit.
#'
#' @examples
#' \dontrun{
#' movedesign:::fix_unit(1, "hours")
#' }
#' @keywords internal
#'
#' @importFrom dplyr case_when
#' @importFrom dplyr add_row
#' @importFrom ctmm `%#%`
#' @noRd
fix_unit <- function(input,
                     unit,
                     digits = 3,
                     ui = FALSE,
                     match_all = TRUE,
                     convert = FALSE)  {
  
  if (missing(unit)) {
    if (!("value" %in% names(input)) || !("unit" %in% names(input)))
      stop("input must contain named columns 'value' and 'unit'.")
    
    value <- input$value
    unit <- x_html <- input$unit
  } else { 
    value <- input
    x_html <- unit
  }
  
  if (!is.character(unit)) {
    stop("`unit` argument must be a character string.")
  }
  
  units_tm <- c("year", "month", "week",
                "day", "hour", "minute", "second")
  units_sp <- c("kilometer", "meter", "km", "m")
  units_ar <- c("square kilometer", "square meter", "hectare",
                "km^2", "m^2", "ha")
  units_vl <- c("kilometers/day", "kilometer/day", "km/day",
                "kilometers/hour", "kilometer/hour", "km/h",
                "meters/second", "meter/second", "m/s")
  
  all_units <- c(units_tm, 
                 units_sp,
                 units_ar, 
                 units_vl)
  
  # Create empty vectors to store results
  x <- y <- x_html <- NULL
  
  for (i in seq_along(value)) {
    
    if (!unit[i] %in% units_vl) {
      x <- c(x, gsub("(.)s$", "\\1", unit[i]))
    } else { 
      unit[i] <- case_when(
        (unit[i] == "kilometers/hour" | unit[i] == "km/h") ~ "km/h",
        (unit[i] == "kilometers/day" | unit[i] == "km/day") ~ "km/day",
        (unit[i] == "meters/second" | unit[i] == "m/s") ~ "m/s",
        TRUE ~ unit[i])
      x <- c(x, unit[i]) 
    }
    
    var <- all_units[pmatch(x[i], all_units, duplicates.ok = TRUE)]
    if (any(is.na(var))) {
      stop("Invalid unit: ", paste(x[i][is.na(var)], collapse = ", "),
           call. = FALSE)
    }
    
    # Convert value to standard format:
    y[i] <- ifelse(convert, value[i] %#% x[i], value[i])
    
    # Convert time units:
    if ((x[i] %in% units_tm) & convert) {
      x_new <- dplyr::case_when(
        y[i] < 60 ~ "second",
        y[i] < 3600 ~ "minute",
        y[i] < 86400 ~ "hour",
        y[i] < (1 %#% "month") ~ "day",
        y[i] < (1 %#% "year") ~ "month",
        TRUE ~ "year")
      if (ui) x_html[i] <- x_new
    }
    
    # Convert spatial units:
    if ((x[i] %in% units_sp) & convert) {
      x_new <- ifelse(y[i] >= 1000, "km", "m")
      if (ui) x_html[i] <- x_new
    }
    
    # Convert area units:
    if ((x[i] %in% units_ar) & convert) {
      x_new <- dplyr::case_when(
        y[i] < 1e4 ~ "m^2",
        y[i] < 1e6 ~ "ha",
        TRUE ~ "km^2")
      
      if (ui) x_html[i] <- dplyr::case_when(
        (x_new == "square kilometer" | x_new == "km^2") ~ "km\u00B2",
        (x_new == "square meter" | x_new == "m^2") ~ "m\u00B2",
        (x_new == "hectare" | x_new == "ha") ~ "ha")
    }
    
    # Convert speed units:
    if ((x[i] %in% units_vl) & convert) {
      x_new <- dplyr::case_when(
        y[i] < 0.01 ~ "m/s",
        y[i] < 0.25 ~ "km/day",
        TRUE ~ "km/h")
      
      if (ui) x_html[i] <- case_when(
        (x_new == "km/h") ~ "kilometers/hour",
        (x_new == "km/day") ~ "kilometers/day",
        (x_new == "m/s") ~ "meters/second")
    }
    
    if (convert) {
      y[i] <- x_new %#% y[i]
      x[i] <- x_new
    }   

    # Round value:
    y[i] <- ifelse((y[i] %% 1) * 10 == 0, 
                   round(y[i], 0),
                   round(y[i], 1))
    
    # Check if value is equal to 1 (e.g. 1 hour), adjust unit:
    if (x[i] %in% units_tm) 
      x[i] <- dplyr::case_when(
        y[i] < 1 || y[i] > 1 ~ paste0(x[i], "s"),
        y[i] == 1 ~ x[i])
    
  } # end of loop
  
  # Show units as HTML:
  if (ui) x <- x_html
  
  # Create data.frame with outputs:
  out <- data.frame(value = y, unit = x)
  
  # Match all units to estimate unit (if length > 1):
  if (match_all & length(value) > 1) {
    out_unit <- out[2,2]
    for (i in nrow(out)) {
      out[i,1] <- out_unit %#% out[i,1] %#% out[i,2] 
    }
    out[,2] <- rep(out_unit, nrow(out))
  }
  
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
#' @importFrom ctmm `%#%`
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
simulate_data <- function(mod,
                          dur,
                          dur_units,
                          dti,
                          dti_units,
                          seed) {

  dur <- round(dur %#% dur_units, 0) # duration
  dti <- round(dti %#% dti_units, 0) # interval

  t0 <- seq(0, dur, by = dti)
  dat <- ctmm::simulate(mod, t = t0, seed = seed)
  dat <- pseudonymize(dat)
  dat$index <- 1:nrow(dat)

  return(dat)
}


CI.upper <- Vectorize(function(k, level) {
  stats::qchisq((1 - level)/2, k, lower.tail = FALSE) / k} )

CI.lower <- Vectorize(function(k, level) {
  stats::qchisq((1 - level)/2, k, lower.tail = TRUE) / k} )

calculate_ci <- function(variable, level) {
  
  out <- data.frame(
    CI = level,
    CI_low = CI.lower(variable, level),
    CI_high = CI.upper(variable, level))
  
  return(out)
}


#' Extract parameters.
#'
#' @description Extracting parameter values and units from ctmm summaries.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
extract_pars <- function(obj = NULL, 
                         data = NULL,
                         name, fraction = .65) {
  
  out <- unit <- NULL
  if (name == "sigma") { 
    if (missing(data)) stop("`data` argument not provided.")
    
    svf <- extract_svf(data, fraction = fraction)
    out <- c("low" = mean(svf$var_low95) %#% "km^2",
             "est" = var.covm(obj$sigma, average = T),
             "high" = mean(svf$var_upp95) %#% "km^2")
    
    out <- data.frame(value = out, "unit" = "m^2")
    return(out)
  }
  
  if (inherits(obj, "telemetry")) {
    nms.dat <- suppressWarnings(names(summary(obj)))
    
    unit <- extract_units(nms.dat[grep(name, nms.dat)])
    value <- suppressWarnings(as.numeric(
      summary(obj)[grep(name, nms.dat)]))
    
    out <- data.frame(value = value, "unit" = unit)
    return(out)
  }
  
  if (inherits(obj, "ctmm")) {
    sum.fit <- summary(obj)
    nms.fit <- rownames(sum.fit$CI)
    
    # Special cases of movement processes:
    if (length(obj$tau) == 2 && (obj$tau[1] == obj$tau[2])) {
      # OUf:
      name <- "\u03C4"
      out <- sum.fit$CI[grep(name, nms.fit), ]
      unit <- extract_units(nms.fit[grep(name, nms.fit)])
      
      if (length(grep("decay", nms.fit)) != 0) {
        # OUΩ:
        out <- out[grep("decay", rownames(out)), ]
        unit <- extract_units(nms.fit[grep("decay", nms.fit)])
      }
      
      out <- data.frame(value = out, "unit" = unit)
      return(out)
    }
    
    # All other cases:
    if (length(grep(name, nms.fit)) != 0) {
      unit <- extract_units(nms.fit[grep(name, nms.fit)])
      out <- sum.fit$CI[grep(name, nms.fit), ]
      out <- data.frame(value = out, "unit" = unit)
      return(out)
    }
  }
  
  return(out)
}


#' Extract DOF
#'
#' @description Extracting DOF values and units from ctmm summaries.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
extract_dof <- function(obj, par) {
  
  par_list <- c("mean", "speed", "area", "diffusion")
  
  if (!(par %in% par_list)) {
    stop("`par` argument is not valid.")
  }
  
  if (inherits(obj, "ctmm")) {
    sum.fit <- summary(obj)
    nms.fit <- names(sum.fit$DOF)
    
    out <- sum.fit$DOF[grep(par, nms.fit)][[1]]
    if (is.na(out)) out <- NULL
    
  } else {
    stop("`object` argument is not a `ctmm` movement model.")
  }
  
  return(out)
}

#' Extract semi-variance data
#'
#' @description Extract semi-variance data
#' @keywords internal
#'
#' @noRd
extract_svf <- function(data, fraction = .65) {

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
  vardat$SVF <- "square kilometers" %#% vardat$SVF

  return(vardat)
}

#' Simulate GPS battery life decay
#'
#' @description Simulate GPS battery life decay
#'
#' @param data data.frame. A dataset with frequencies.
#' @param b_max Numeric. Maximum duration (y) for the GPS device.
#' @param b_unit Character. Unit for the maximum duration (y).
#' @param cutoff Character. Cut-off for for minimum duration required.
#' @param dti_max Maximum sampling interval (or minimum frequency) for the maximum duration.
#' @keywords internal
#'
#' @importFrom ctmm `%#%`
#' @importFrom dplyr `%>%`
#' 
#' @noRd
simulate_gps <- function(data,
                         b_max,
                         b_unit,
                         cutoff,
                         dti_max,
                         seed = NULL,
                         set_seed = FALSE) {
  
  stopifnot(!is.null(data))
  stopifnot(is.numeric(b_max) || is.null(b_max))
  stopifnot(is.numeric(cutoff) || is.null(cutoff))
  stopifnot(is.character(b_unit) || is.null(b_unit))
  stopifnot(is.character(dti_max) || is.null(dti_max))
  if (b_max == 0) stop("Duration (b_max) cannot be 0.")
  if (b_max < 2 && b_unit == "days") 
    stop("Duration (b_max) cannot be less than 2 days.")
  if (set_seed) set.seed(seed)
  
  trace <- FALSE
  dti <- dti_notes <- dti_scale <- dti_yn <- frq_hrs <- NULL
  add_noise <- function(max) stats::runif(1, min = 0, max = max)
  
  # Initialize parameters:
  
  unit <- "days"
  params <- data.frame(
    id = ifelse(dti_max == "1 fix every day", TRUE, FALSE),
    b_max = round(unit %#% (b_max %#% b_unit), 1),
    x_min = data$frq_hrs[match(dti_max, data$dti_notes)],
    scale = 0)
  
  params[["scale"]] <- dplyr::case_when(
    params[["b_max"]] < 31 ~ 1,
    params[["b_max"]] < 365 ~ params[["b_max"]] * 0.01,
    TRUE ~ params[["b_max"]] * 0.02)
  
  newdata <- data %>%
    dplyr::select(dti_notes, dti, frq_hrs) %>%
    dplyr::filter(frq_hrs >= params[["x_min"]])
  
  # Initialize log-logistic function:
  
  init <- init0 <- c(-16.913, params[["b_max"]])
  f <- update_f(x = newdata$frq_hrs, init)
  y <- f$y
  
  err <- 100 - (max(y) * 100) / params[["b_max"]]
  
  # Iterate until the maximum value is equal to b_max:
  
  i <- 0
  max_attempts <- 150
  start_time <- Sys.time()
  threshold <- ifelse(params[["b_max"]] > 31, 0.01, 1)
  
  params[["scale"]] <- dplyr::case_when(
    params[["b_max"]] < 31 ~ 1,
    params[["b_max"]] < 365 ~ params[["b_max"]] * 0.01,
    TRUE ~ params[["b_max"]] * 0.02)
  
  while (abs(err) > threshold && i < max_attempts) {

    # Update the log-logistic function:
    i <- i + 1
    f <- update_f(x = newdata$frq_hrs, init)
    y <- f$y
    
    # Check error against threshold:
    
    err <- 100 - (max(y) * 100) / params[["b_max"]]
    if (trace) print(paste0(i, ", Error: ", round(err, 2), "%"))
    if (abs(err) < threshold) break
    
    # prev_val <- ifelse(i == 1, 0, curr_val)
    # curr_val <- f$pars[["b_max"]]
    
    # Adjust initial parameters:
    
    if (params[["id"]] && # Adjust for small values when 1/day
        params[["b_max"]] <= 24 && i == 1) init[2] <- init[2] + 1
    
    if (abs(err) > 5) mult <- 0.2
    else if (abs(err) >= 0.1) mult <- 0.1
    else mult <- 0.01
    
    init[1] <- ifelse(
      sign(err) == 1,
      init[1] - max(y) * mult + add_noise(0.01),
      init[1] + max(y) * mult + add_noise(0.01)
    )
    
    # Update the log-logistic function:
    
    i <- i + 1
    f <- update_f(x = newdata$frq_hrs, init)
    y <- f$y
    
    err <- 100 - (max(y) * 100) / params[["b_max"]]
    if (trace) print(paste0(i, ", Error: ", round(err, 2), "%"))
    
    if (abs(err) < threshold) break
    
    if (!params[["id"]]) {
      
      mult <- dplyr::case_when(
        abs(err) <= .5 ~ abs(err) * .5,
        abs(err) <= 1 ~ abs(err),
        TRUE ~ abs(err))
      mult <- mult + add_noise(abs(err) * .1)
      if (params[["b_max"]] < 31) mult <- mult * .1
      
      init[2] <- ifelse(err > threshold,
                        init[2] + params[["scale"]] * mult,
                        init[2] - params[["scale"]] * mult)
      
      f <- update_f(newdata$frq_hrs, init)
      y <- f$y
      
    } # !params[["id"]]
  } # end of while
  
  if (trace) {
    message("number of attempts: ", i)
    cat("max(b):", round(max(y), 1),
        "\n", "b_max:", round(params[["b_max"]], 1))
    cat(", error:", round(abs(err), 2), "%", "\n")
    
    message("Elapsed time since start:")
    print(Sys.time() - start_time)
  }
  
  if (abs(err) > threshold) {
    msg_log(
      style = "error", 
      message = "Something went wrong!")
  }
  
  newdata$dur_sec <- y %#% unit
  newdata$dur_mth <- "months" %#% newdata$dur_sec
  
  if (max(newdata$dur_sec) > cutoff) {
    newdata$cutoff <- as.factor(dplyr::case_when(
      newdata$dur_sec < cutoff ~ "Y",
      newdata$dur_sec >= cutoff ~ "N"))
  } else { newdata$cutoff <- "Y" }
  
  newdata$id <- 1:nrow(newdata)
  newdata <- dplyr::left_join(
    newdata,
    data %>% dplyr::select(dti, dti_scale, dti_yn),
    by = "dti")
  
  if (set_seed) set.seed(NULL)
  return(newdata)
}


#' Calculate initial parameters
#'
#' @description Calculate initial parameters for log-logistic function
#' @keywords internal
#'
#' @noRd
#'
update_f <- function(x, init) {
  
  d <- init[1] + 6.756 * init[2]
  if (!sign(d/init[2]) == 1) {
    return(list(y = rep(0, length(x)), 
                pars = c("b_max" = 0,
                         "b_50" = 0,
                         "beta" = 0)))
  }
  
  e <- 1.005511 / 
    ( 1 + exp(1.490650 *
                (log(d/init[2]) - log(0.202345))) )
  b <- 0.847 + (0.985 - 0.847) * exp(-(init[2]) / 14.297)
  y <- d / ( 1 + exp(b * (log(x) - log(e))) )
  
  return(list(y = y, pars = c("b_max" = d,
                              "b_50" = e,
                              "beta" = b)))
}


#' Rough estimation of computation time
#'
#' @description Calculate computation time of ctmm functions.
#' @keywords internal
#'
#' @noRd
#'
guess_time <- function(data, 
                       seed,
                       type = "fit",
                       fit = NULL,
                       with_truth = FALSE,
                       trace = FALSE,
                       parallel = TRUE) {
  
  if (!type %in% c("fit", "speed"))
    stop("type =", type, " is not supported.", call. = FALSE)
  
  if (missing(data)) stop("`data` not provided.")
  expt_unit <- "minute"
  
  expt <- expt_max <- expt_min <- expt_rng <- 0
  outputs <- data.frame(expt, expt_min, expt_max, expt_unit, expt_rng)
  names(outputs) <- c("mean", "min", "max", "unit", "range")
  
  if (type == "fit") {
    n <- 2500
    if (nrow(data) < n) {
      outputs$mean <- ifelse(nrow(data) < 1000, 1, 2)
      outputs$range <- paste(
        "\u2264", outputs$mean,
        ifelse(nrow(data) < 1000, expt_unit, "minutes"))
      outputs$max <- 5
      return(outputs)
    }
    
    start <- Sys.time()
    guess <- ctmm::ctmm.guess(data[1:200, ], interactive = FALSE)
    fit <- par.ctmm.select(list(list(data[1:200, ], guess)),
                           trace = FALSE, parallel = TRUE)
    total_time <- difftime(Sys.time(), start, units = "sec")[[1]]
    expt <- expt_unit %#% (total_time * nrow(data) / 200)
    
    expt <- round_any(expt, 1, f = floor)
    expt_min <- max(round_any(expt, 1, f = floor) - 2, 0)
    expt_max <- round_any(expt, 2, f = ceiling)
    if (expt >= 15) expt_max <- round_any(expt, 5, f = ceiling)
    
  } # end of if (type == "fit")
  
  if (type == "speed") {
    if (missing(fit)) stop("ctmm `fit` object not provided.")
    if (is.null(summary(fit)$DOF["speed"])) return(outputs)
    tauv <- extract_pars(fit, name = "velocity")
    if (is.null(tauv)) return(outputs)
    if (tauv$value[2] == 0) return(outputs)
    
    dti <- data[[2,"t"]] - data[[1,"t"]]
    dur <- extract_pars(data, name = "period")
    tauv <- tauv$value[2] %#% tauv$unit[2]
    N <- summary(fit)$DOF["speed"]
    
    x1 <- log(N)
    x2 <- tauv/dti
    x3 <- "days" %#% dur$value %#% dur$unit
    
    if (tauv/dti < 1) {
      y_min <- max(exp(1.3915 + 0.1195 * x1), 1)
      y <- exp(3.4924 - 0.1978 * x1) 
      if (N < 30) {
        y_max <- exp(4.15038 - 0.3159 * x1 + 0.01912 * x3)
        if (N <= 5) y_max <- y_max * 3
      } else { y_max <- y }
      
      expt_min <- round_any(expt_unit %#% y, 1, f = ceiling)
      expt <- round_any(expt_unit %#% y, 2, f = ceiling)
      expt_max <- round_any(expt_unit %#% y_max, 5, f = ceiling)
      
    } else {
      if (tauv/dti < 10 && tauv/dti >= 1)
        y <- exp(-3.28912 + 1.01494 * x1 + 0.01953 * x1 * x2)
      if (tauv/dti >= 10)
        y <- exp(-2.0056285 + 0.9462089 * x1 + 0.0023285 * x1 * x2)
      
      y <- expt_unit %#% y
      expt_min <- ceiling(y * 2) / 2 
      expt <- round_any(y, 1, f = ceiling)
      expt_max <- round_any(y, 5, f = ceiling)
    }
  } # end of if (type == "speed")
  
  if (with_truth) expt <- expt + 3
  if (with_truth) expt_min <- expt_min + 3
  if (with_truth) expt_max <- expt_max + 5
  
  if (expt <= 1) {
    range <- paste("\u2264", "1", expt_unit)
  } else {
    expt_unit <- "minutes"
    range <- ifelse(
      expt_min == expt_max,
      paste("\u2264", expt_max, expt_unit),
      paste0(expt_min, "\u2013", expt_max, " ", expt_unit))
  }
  
  outputs <- data.frame("mean" = expt, 
                        "min" = expt_min,
                        "max" = expt_max, 
                        "unit" = expt_unit,
                        "range" = range)
  return(outputs)
}


#' Calculate distance
#'
#' @description Calculate distance traveled
#' @keywords internal
#'
#' @noRd
#'
measure_distance <- function(data) {

  tmpdat <- data.frame(
    x = data$x,
    y = data$y)

  tmpdist <- list()
  for (i in 2:nrow(data)) {
    tmpdist[[i]] <-
      sqrt((tmpdat$x[i] - tmpdat$x[i - 1])^2 +
             (tmpdat$y[i] - tmpdat$y[i - 1])^2)
  }
  dist <- c(0, do.call("rbind", tmpdist))

  # dist <- c(0, sqrt((data$x - lag(data$x))^2 +
  #                     (data$y - lag(data$y))^2)[-1])
  
  return(dist)

}

#' Convert to a different unit
#'
#' @description Convert to a different unit.
#' @keywords internal
#'
#' @importFrom ctmm `%#%`
#' @importFrom dplyr `%>%`
#' @noRd
#'
convert_to <- function(x, unit, new_unit = NULL, to_text = FALSE) {
  if (is.data.frame(x) &&
      "value" %in% names(x) && "unit" %in% names(x)) {
    unit <- x$unit
    x <- x$value
  } else if (missing(unit)) {
    stop("'unit' must be specified when passing a single value.")
  }
  
  out <- (new_unit %#% (x %#% unit)) %>% 
    fix_unit(., new_unit)
  
  if (to_text) out <- paste0(out[1], " ", out[2])
  
  return(out)
}


