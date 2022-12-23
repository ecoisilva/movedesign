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
  if (missing(unit)) {
    stop("`unit` argument not provided.")
  }

  if (!is.character(unit)) {
    stop("`unit` argument must be a character string.")
  }

  all_units <- c(
    "year", "month", "week",
    "day", "hour", "minute", "second",
    "kilometer", "meter", "km", "m",
    "km^2", "m^2", "ha",
    "square kilometer", "square meter", "hectare",
    "kilometers/hour", "meters/second",
    "kilometers/day", "meters/day",
    "km/h", "m/s",
    "km/day", "m/day"
  )

  x <- gsub("(.)s$", "\\1", unit)
  var <- all_units[pmatch(x, all_units, duplicates.ok = TRUE)]

  if (any(is.na(var))) {
    stop("Invalid unit: ", paste(x[is.na(var)], collapse = ", "),
      call. = FALSE
    )
  }

  if (x == "square kilometer") x <- "km^2"
  if (x == "square meter") x <- "m^2"

  out <- x
  switch(x,
    "year" = out <- "yr",
    "month" = out <- "mth",
    "week" = out <- "wk",
    "day" = out <- "d",
    "hour" = out <- "hr",
    "minute" = out <- "min",
    "second" = out <- "sec",
    "kilometer" = out <- "km",
    "meter" = out <- "m",
    "km^2" = out <- ifelse(ui_only, "km\u00B2", "km^2"),
    "m^2" = out <- ifelse(ui_only, "m\u00B2", "m^2"),
    "hectare" = out <- "ha",
    "kilometers/hour" = out <- "km/h",
    "meters/second" = out <- "m/s",
    "kilometers/day" = out <- "km/day",
    "meters/day" = out <- "m/day"
  )
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
fix_unit <- function(value, unit,
                     digits = 2,
                     ui = FALSE,
                     convert = FALSE) {
  if (!is.character(unit)) {
    stop("`unit` argument must be a character string.")
  }

  all_units <- c(
    "year", "month", "week",
    "day", "hour", "minute", "second",
    "kilometer", "meter", "km", "m",
    "square kilometer", "square meter", "hectare",
    "km^2", "m^2", "ha",
    "kilometers/day", "kilometer/day", "km/day",
    "kilometers/hour", "kilometer/hour", "km/hour",
    "meters/second", "meter/second", "m/s"
  )

  units_tm <- all_units[1:7]
  units_sp <- all_units[8:11]
  units_ar <- all_units[12:17]
  units_vl <- all_units[18:26]

  if (!unit %in% units_vl) {
    x <- gsub("(.)s$", "\\1", unit)
  } else {
    x <- unit
  }

  var <- all_units[pmatch(x, all_units, duplicates.ok = TRUE)]
  if (any(is.na(var))) {
    stop("Invalid unit: ", paste(x[is.na(var)], collapse = ", "),
      call. = FALSE
    )
  }

  # Convert value:

  y <- ifelse(convert, value %#% x, value)

  if ((x %in% units_tm) & convert) {
    x_new <- dplyr::case_when(
      y < 60 ~ "second",
      y < 3600 ~ "minute",
      y < 86400 ~ "hour",
      y < (1 %#% "month") ~ "day",
      y < (1 %#% "year") ~ "month",
      TRUE ~ "year"
    )

    y <- x_new %#% y
    x <- x_new
  }

  if ((x %in% units_sp) & convert) {
    x_new <- ifelse(y >= 1000, "km", "m")
    y <- x_new %#% y
    x <- x_new
  }

  if ((x %in% units_ar) & convert) {
    x_new <- dplyr::case_when(
      y < 1e4 ~ "m^2",
      y < 1e6 ~ "ha",
      TRUE ~ "km^2"
    )

    y <- x_new %#% y
    x <- x_new
  }

  if (x %in% units_ar) {
    x_html <- dplyr::case_when(
      (x == "square kilometer" | x == "km^2") ~ "km\u00B2",
      (x == "square meter" | x == "m^2") ~ "m\u00B2",
      (x == "hectare" | x == "ha") ~ "ha"
    )
  }

  if ((x %in% units_vl) & convert) {
    x_new <- dplyr::case_when(
      y < 0.01 ~ "m/s",
      y < 0.25 ~ "km/day",
      TRUE ~ "km/hour"
    )

    y <- x_new %#% y
    x <- x_new
  }

  if (x %in% units_vl) {
    if (x == "kilometers/hour" || x == "km/h") {
      x <- "km/h"
      x_html <- "kilometers/day"
    } else if (x == "kilometers/day" || x == "km/day") {
      x <- "km/day"
      x_html <- "kilometers/day"
    } else if (x == "meters/second" || x == "m/s") {
      x <- "m/s"
      x_html <- "meters/second"
    }
  }

  # Round value:
  y <- sigdigits(y, digits)

  # Check if value is equal to 1 (e.g. 1 hour), adjust unit:
  if (x %in% units_tm) {
    x <- dplyr::case_when(
      y < 1 || y > 1 ~ paste0(x, "s"),
      y == 1 ~ x
    )
  }

  # Show units as HTML:
  x <- ifelse(ui, x_html, x)

  out <- data.frame(value = numeric(0), unit = character(0))
  out <- out %>% dplyr::add_row(value = as.numeric(y), unit = x)
  return(out)
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
        stringr::str_extract_all(
          input,
          "\\(([^()]+)\\)"
        )[[1]]
      )
      return(string)
    },
    error = function(e) {
      print(
        sprintf(
          "An error occurred in extract_units at %s : %s",
          Sys.time(), e
        )
      )
    }
  )
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
  mod <- ctmm::ctmm(
    tau = c(taup, tauv),
    isotropic = TRUE,
    sigma = sig,
    mu = c(0, 0)
  )
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
  dur <- dur %#% dur_units # duration
  dti <- round(dti %#% dti_units, 0) # sampling interval

  t0 <- seq(0, dur, by = dti)
  dat <- ctmm::simulate(mod, t = t0, seed = seed)
  dat <- pseudonymize(dat)
  dat$index <- 1:nrow(dat)

  return(dat)
}


CI.upper <- Vectorize(function(k, level) {
  stats::qchisq((1 - level) / 2, k, lower.tail = FALSE) / k
})

CI.lower <- Vectorize(function(k, level) {
  stats::qchisq((1 - level) / 2, k, lower.tail = TRUE) / k
})

calculate_ci <- function(variable, level) {
  out <- data.frame(
    CI = level,
    CI_low = CI.lower(variable, level),
    CI_high = CI.upper(variable, level)
  )

  return(out)
}


#' Extract parameters.
#'
#' @description Extracting parameter values and units from ctmm summaries.
#' @return The return value, if any, from executing the utility.
#' @keywords internal
#'
#' @noRd
extract_pars <- function(obj, par,
                         fraction = .65,
                         data = NULL) {
  if (par == "sigma") {
    if (missing(data)) {
      stop("`data` argument not provided.")
    }

    svf <- extract_svf(data, fraction = fraction)
    out <- c(
      "low" = mean(svf$var_low95) %#% "km^2",
      "est" = var.covm(obj$sigma, ave = T),
      "high" = mean(svf$var_upp95) %#% "km^2"
    )

    out <- data.frame(value = out, "unit" = "m^2")
  } else if (inherits(obj, "telemetry")) {
    nms.dat <- suppressWarnings(names(summary(obj)))

    unit <- extract_units(nms.dat[grep(par, nms.dat)])
    value <- suppressWarnings(as.numeric(
      summary(obj)[grep(par, nms.dat)]
    ))

    out <- data.frame(value = value, "unit" = unit)
  } else if (inherits(obj, "ctmm")) {
    sum.fit <- summary(obj)
    nms.fit <- rownames(sum.fit$CI)

    out <- sum.fit$CI[grep(par, nms.fit), ]

    if (all(is.na(out))) {
      out <- NULL
    } else {
      unit <- extract_units(nms.fit[grep(par, nms.fit)])
      out <- data.frame(value = out, "unit" = unit)
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
  vardat <- data.frame(
    SVF = SVF$SVF,
    DOF = SVF$DOF,
    lag = SVF$lag
  ) %>%
    dplyr::slice_min(lag, prop = fraction) %>%
    dplyr::mutate(lag_days = lag / 60 / 60 / 24)

  vardat$lag_days <- (vardat$lag) / 60 / 60 / 24
  vardat$var_low95 <- "square kilometers" %#%
    (vardat$SVF * CI.lower(vardat$DOF, level))
  vardat$var_upp95 <- "square kilometers" %#%
    (vardat$SVF * CI.upper(vardat$DOF, level))
  vardat$var_low50 <- "square kilometers" %#%
    (vardat$SVF * CI.lower(vardat$DOF, .5))
  vardat$var_upp50 <- "square kilometers" %#%
    (vardat$SVF * CI.upper(vardat$DOF, .5))
  vardat$SVF <- "square kilometers" %#% vardat$SVF

  return(vardat)
}

#' Simulate GPS battery life decay
#'
#' @description Simulate GPS battery life decay
#'
#' @param data data.frame. A dataset with frequencies.
#' @param yrange Numeric. Value for the range of duration (y).
#' @param yunits Character. Unit for the range of duration (y).
#' @param cutoff Character. Cut-off for for minimum duration required.
#' @param max_dti Maximum sampling interval (or minimum) frequency for the maximum duration.
#' @param trace Logical. Display messages as function runs.
#' @keywords internal
#'
#' @importFrom ctmm `%#%`
#' @importFrom dplyr `%>%`
#'
#' @noRd
simulate_gps <- function(data,
                         b_max,
                         b_unit,
                         dti_max,
                         dti_min,
                         trace = FALSE) {
  
  stopifnot("Error: data required." = !is.null(data))
  
  stopifnot(is.numeric(b_max) || is.null(b_max))
  stopifnot(is.character(b_unit) || is.null(b_unit))
  stopifnot(is.numeric(dti_min) || is.null(dti_min))
  stopifnot(is.character(dti_max) || is.null(dti_max))
  if (b_max == 0) stop("Duration cannot be 0.", call. = FALSE)
  
  start_time <- Sys.time()
  dti <- dti_notes <- frq_hrs <- NULL
  
  unit <- "days"
  params <- data.frame(
    id = ifelse(dti_max == "1 fix every day", "main", "other"),
    b_max = round(unit %#% (b_max %#% b_unit), 1),
    x_min = data$frq_hrs[match(dti_max, data$dti_notes)],
    shift = NA
  )
  if (params[["b_max"]] <= 2.5) 
    stop("Duration cannot be less than 2.5 days.", call. = FALSE)
  
  newdata <- data %>%
    dplyr::select(dti_notes, dti, frq_hrs) %>%
    dplyr::filter(frq_hrs >= params[["x_min"]])
  
  threshold <- 0.01
  
  init <- init0 <- data.frame(
    "b_max" = params[["b_max"]],
    "b_intercept" = -16.913, 
    "b_scale" = dplyr::case_when(
      params[["b_max"]] < 31 ~ 1,
      params[["b_max"]] < unit %#% 6 %#% "months" ~ 3,
      params[["b_max"]] < unit %#% 1 %#% "year" ~ 10,
      params[["b_max"]] < unit %#% 3 %#% "years" ~ 20,
      TRUE ~ 30),
    "b_steps" = dplyr::case_when(
      params[["b_max"]] < 31 ~ b_unit %#% 1 %#% "day",
      params[["b_max"]] < unit %#% 6 %#% "months" ~ b_unit %#% 5 %#% "days",
      params[["b_max"]] < unit %#% 1 %#% "year" ~ b_unit %#% 1 %#% "months",
      params[["b_max"]] < unit %#% 3 %#% "years" ~ b_unit %#% 1 %#% "months",
      params[["b_max"]] < unit %#% 5 %#% "years" ~ b_unit %#% 1.4 %#% "months",
      TRUE ~ b_unit %#% 6 %#% "months")
  )
  
  f <- update_f(newdata$frq_hrs, init)
  y <- f$y
  err <- diff(c(params[["b_max"]], max(y)))
  
  i <- 0
  while (abs(err) > threshold) {
    i <- i + 1
    if (all(y == 0)) {
      init <- init0
      break
    }
    
    # Iterate through b_intercept until max(y) is similar to b_max:
    while (abs(err) > threshold) {
      
      if (params[["x_min"]] >= 30) break
      err <- diff(c(params[["b_max"]], max(y)))
      if (trace) print(paste0("Error: ", round(err, 2), "%"))
      if (abs(err) <= threshold) break
      
      var <- dplyr::case_when(
        abs(err) <= 30 ~ abs(err),
        TRUE ~ 30
      )
      
      if (sign(err) == 1) {
        init[["b_intercept"]] <- 
          init[["b_intercept"]] + init[["b_scale"]] * var
      } else {
        init[["b_intercept"]] <- 
          init[["b_intercept"]] - init[["b_scale"]] * var
      }
      
      f <- update_f(newdata$frq_hrs, init)
      y <- f$y
      
      i <- i + 1
      if (all(y == 0)) {
        init <- init0
        break
      }
    }
    if (abs(err) <= threshold) break
    
    # Match maximum duration with b_max:
    f <- update_f(newdata$frq_hrs, init)
    y <- f$y
    
    # Adjust initial parameters:
    
    var <- dplyr::case_when(
      abs(err) <= 1 ~ 1,
      abs(err) <= 5 ~ 2,
      TRUE ~ 3)
    if (params[["x_min"]] >= 30) var <- abs(err) 
    
    if (err > threshold) {
      b_max <- b_max - init[["b_steps"]] * var
    } else {
      b_max <- b_max + init[["b_steps"]] * var
    }
    
    init[["b_max"]] <- unit %#% b_max %#% b_unit
    f <- update_f(newdata$frq_hrs, init)
    y <- f$y
    
    err <- diff(c(params[["b_max"]], max(y)))
    if (trace) print(paste0("Error: ", round(err, 2), "%, ..."))
    if (abs(err) <= threshold) break
    i <- i + 1
    
  }
  
  newdata$dur_sec <- y %#% unit
  newdata$dur_mth <- "months" %#% newdata$dur_sec
  
  if (max(newdata$dur_sec) > dti_min) {
    newdata$color <- as.factor(dplyr::case_when(
      newdata$dur_sec < dti_min ~ "red",
      newdata$dur_sec >= dti_min ~ "black"
    ))
  } else {
    newdata$color <- "red"
  }
  
  if (trace) {
    print(paste("-- number of attempts:", i))
    print(end_time <- Sys.time() - start_time)
    
    if (abs(err) > threshold)
      cat(crayon::red("Failed \n"))
    else cat(crayon::green("Success! \n"))
  }
  
  newdata$id <- 1:nrow(newdata)
  return(newdata)
}


#' Calculate initial parameters
#'
#' @description Calculate initial parameters for log-logistic function
#' @keywords internal
#'
#' @noRd
update_f <- function(x, init) {
  
  b_max <- init[["b_intercept"]] + 6.756 * init[["b_max"]]
  if (!sign(b_max/init[["b_max"]]) == 1) {
    return(list(y = rep(0, length(x)), pars = c("b_max" = 0,
                                                "b_50" = 0,
                                                "beta" = 0)))
  }
  
  b_50 <- 1.005511 /
    ( 1 + exp(1.490650 *
                (log(b_max/init[["b_max"]]) - log(0.202345))) )
  beta <- 0.847 + (0.985 - 0.847) * exp(-(init[["b_max"]]) / 14.297)
  
  # Compute the log-logistic function
  # for the given values of x, b_max, b_50, and beta
  y <- b_max / ( 1 + exp(beta * (log(x) - log(b_50))) )
  
  return(list(y = y, pars = c("b_max" = b_max,
                              "b_50" = b_50,
                              "beta" = beta)))
}


#' Estimate computation time
#'
#' @description Calculate computation time of ctmm functions.
#' @keywords internal
#'
#' @noRd
#'
guesstimate_time <- function(data,
                             fit = NULL,
                             type = "fit",
                             trace = FALSE,
                             parallel = TRUE) {
  if (!type %in% c("fit", "speed")) {
    stop("type =", type, " is not supported.", call. = FALSE)
  }

  if (missing(data)) {
    stop("`data` argument not provided.")
  }

  units <- "minute"

  n <- 500
  if (nrow(data) < n) {
    # Does not need to run for smaller datasets:

    expt <- expt_max <- 1
    expt_min <- 0
    expt_units <- units
  } else {
    if (type == "fit") {
      start_test <- Sys.time()
      tmpdat <- data[1:n, ]

      guess <- ctmm::ctmm.guess(tmpdat, interactive = FALSE)
      inputList <- list(list(tmpdat, guess))
      fit <- par.ctmm.select(inputList, trace = FALSE, parallel = TRUE)

      total_time <- difftime(Sys.time(), start_test,
        units = "secs"
      ) %>%
        as.numeric()
    } # end of if (type == "fit")

    if (type == "speed") {
      if (missing(fit)) {
        stop("ctmm `fit` object argument not provided.")
      }

      n <- 10
      start_test <- Sys.time()
      units <- "minute"

      dti <- data[[2, "t"]] - data[[1, "t"]]
      tauv <- extract_pars(fit, par = "velocity")
      m <- ifelse(dti >= 4 * (tauv$value[2] %#% tauv$unit[2]),
        1, 90
      )

      tmpdat <- data[1:n, ]
      inputList <- align_lists(list(fit), list(tmpdat))
      speed <- par.speed(inputList, trace = trace, parallel = parallel)

      total_time <- difftime(Sys.time(), start_test,
        units = "secs"
      ) %>%
        as.numeric() / m
    } # end of if (type == "speed")

    expt <- ((total_time / n) * nrow(data))
    expt <- ceiling(units %#% expt)

    if (expt >= 15) {
      expt_max <- round_any(expt, 5, f = ceiling)
      expt_min <- expt - 5
    } else if (expt < 15 & expt > 5) {
      expt_max <- round_any(expt, 1, f = ceiling)
      expt_min <- expt - 3
    } else {
      expt_max <- round_any(expt, 1, f = ceiling)
      expt_min <- expt_max
    }
  }

  expt_units <- ifelse(expt_max == 1, units, "minutes")

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
    y = data$y
  )

  tmpdist <- list()
  for (i in 2:nrow(data)) {
    tmpdist[[i]] <-
      sqrt((tmpdat$x[i] - tmpdat$x[i - 1])^2 +
        (tmpdat$y[i] - tmpdat$y[i - 1])^2)
  }
  dist <- c(0, do.call("rbind", tmpdist))
  return(dist)

  # dist <- c(0, sqrt((data$x - lag(data$x))^2 +
  #                     (data$y - lag(data$y))^2)[-1])
  # return(dist)
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
                         requireNamespace("ctmm", quietly = TRUE)
                       })) {
  if (parallel) {
    if (!is.null(cores) && cores > 0) {
      cluster_size <- cores
    }
    if (!is.null(cores) && cores < 0) {
      cluster_size <- max(parallel::detectCores(logical = FALSE) +
        cores, 1)
    }
    sysinfo <- Sys.info()
    tryCatch(
      {
        if (sysinfo["sysname"] == "Windows") {
          if (is.null(cores)) {
            cluster_size <- min(length(lst), parallel::detectCores(logical = FALSE) *
              2)
          }
          cat(crayon::inverse(
            "running parallel in SOCKET cluster of",
            cluster_size, "\n"
          ))
          cl <- parallel::makeCluster(cluster_size, outfile = "")
          parallel::clusterExport(cl, c("win_init"), envir = environment())
          parallel::clusterEvalQ(cl, eval(win_init))
          res <- parallel::parLapplyLB(cl, lst, fun)
          parallel::stopCluster(cl)
        } else {
          if (is.null(cores)) {
            cluster_size <- min(
              length(lst),
              parallel::detectCores(logical = FALSE) * 4
            )
          }
          cat(crayon::inverse(
            "running parallel with mclapply in cluster of",
            cluster_size, "\n"
          ))
          res <- parallel::mclapply(lst, fun, mc.cores = cluster_size)
        }
      },
      error = function(e) {
        cat(crayon::bgRed$white("Parallel Error, try restart R session\n"))
        cat(e)
      }
    )
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
    fall_back(
      ctmm::ctmm.select,
      list(input[[1]],
        CTMM = input[[2]],
        control = list(
          method = "pNewton",
          cores = internal_cores
        ),
        trace = trace
      ),
      ctmm::ctmm.select,
      list(input[[1]],
        CTMM = input[[2]],
        control = list(cores = internal_cores),
        trace = trace
      ),
      paste0(
        "ctmm.select() failed with pNewton,",
        "switching to Nelder-Mead."
      )
    )
  }

  if (length(input) == 1) {
    # Process one individual on multiple cores:

    # message("No. of cores: ", parallel::detectCores(logical = FALSE))

    internal_cores <- if (parallel) -1 else 1
    res <- try(try_models(input[[1]],
      trace = trace
    ))
  } else {
    internal_cores <- 1
    res <- try(par.lapply(
      input,
      try_models,
      cores,
      parallel
    ))
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
      control = list(cores = internal_cores)
    )
  }

  if (length(input) == 1) {
    # Process one individual on multiple cores:
    internal_cores <- if (parallel) -1 else 1
    res <- try(try_models(input[[1]]))
  } else {
    # Process multiple animals on multiple cores:
    internal_cores <- 1
    res <- try(par.lapply(
      input,
      try_models,
      cores,
      parallel
    ))
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
      trace = trace
    )
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
  if (length(res) == 0) {
    res <- NULL
  }
  return(res)
}
