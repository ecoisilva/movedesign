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

  if (x == "square kilometer" || x == "km^2") out <- "km\u00B2"
  if (x == "square meter" || x == "m^2") out <- "m\u00B2"
  if (x == "hectare" || x == "ha") out <- "ha"

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
                     digits = 2,
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
                 "kilometers/day", "kilometer/day", "km/day",
                 "kilometers/hour", "kilometer/hour", "km/hour",
                 "meters/second", "meter/second", "m/s")

  units_tm <- all_units[1:7]
  units_sp <- all_units[8:11]
  units_ar <- all_units[12:17]
  units_vl <- all_units[18:26]

  if (!unit %in% units_vl) {
    x <- gsub("(.)s$", "\\1", unit)
  } else { x <- unit }
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
    } else if (any(y < 1 %#% "years")) {
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
    if (y >= 1e6) {
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
    if (x == "square kilometer" || x == "km^2") {
      x_html <- HTML(paste0("km", tags$sup(2)))
    } else if (x == "square meter" || x == "m^2") {
      x_html <- HTML(paste0("m", tags$sup(2)))
    } else if (x == "hectare" || x == "ha") {
      x_html <- "ha"
    }
  }

  if ((x %in% units_vl) & convert) {
    if (y >= 0.25) {
      x_new <- "km/hour"
    } else if (0.01 > y || y < 0.25) {
      x_new <- "km/day"
    } else {
      x_new <- "m/s"
    }
    y <- x_new %#% y
    x <- x_new
  }
  
  if ((x %in% units_vl) & convert) {
    if (x == "kilometers/day" || x == "km/day") {
      x_html <- "kilometers/day"
    } else if (x == "meters/second" || x == "m/s") {
      x_html <- "meters/second"
    }
  }

  # Round value:

  if (ui) {
    y <- dplyr::case_when(
      y %% 1 == 0 ~ scales::label_comma(accuracy = 1)(y),
      y %% 1 != 0 ~ scales::label_comma(accuracy = .1)(y))
  } else {
    y <- sigdigits(y, digits)
  }

  # Check if value is equal to 1 (e.g. 1 hour), adjust unit:

  if (x %in% units_tm) {
    x <- dplyr::case_when(y < 1 || y > 1 ~ paste0(x, "s"),
                          y == 1 ~ x)
  }

  # Show units as HTML:
  x <- ifelse(ui, x_html, x)

  out <- data.frame(value = as.numeric(y), unit = x)
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
  dat <- pseudonymize(dat)
  dat$index <- 1:nrow(dat)

  return(dat)
}

CI.upper <- Vectorize(function(k, level) {
  stats::qchisq((1 - level)/2, k, lower.tail = FALSE) / k} )

CI.lower <- Vectorize(function(k, level) {
  stats::qchisq((1 - level)/2, k, lower.tail = TRUE) / k} )



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
    out <- c("low" = mean(svf$var_low95) %#% "km^2",
             "est" = var.covm(obj$sigma, ave = T),
             "high" = mean(svf$var_upp95) %#% "km^2")
    
    out <- data.frame(value = out, "unit" = "m^2")
    
  } else if (inherits(obj, "telemetry")) {
    nms.dat <- suppressWarnings(names(summary(obj)))
    
    unit <- extract_units(nms.dat[grep(par, nms.dat)])
    value <- suppressWarnings(as.numeric(
      summary(obj)[grep(par, nms.dat)]))
    
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
#' @importFrom ctmm `%#%`
#' 
#' @noRd
simulate_gps <- function(data,
                         k,
                         yrange,
                         yunits,
                         cutoff,
                         max_dti,
                         weight_g,
                         simplified = FALSE) {
  
  dti <- dti_notes <- frq_hrs <- highlight <- NULL
  
  x_min <- data$frq_hrs[match(max_dti, data$dti_notes)]
  
  if (simplified) {
    
    newdata <- data %>%
      dplyr::select(dti_notes, dti, frq_hrs) %>%
      dplyr::filter(frq_hrs >= x_min)
    
    max_dur <- "days" %#% yrange %#% yunits
    
    init <- c(-16.913, max_dur)
    d <- init[1] + 6.756 * init[2]
    e <- 1.005511 / 
      ( 1 + exp(1.490650 *
                  (log(d/init[2]) - log(0.202345))) )
    b <- 0.847 + (0.985 - 0.847) * exp(-(init[2]) / 14.297)
    
    x <- newdata$frq_hrs
    y <- d / ( 1 + exp(b * (log(x)-log(e))) )
    
    n_tries <- 40
    goal_dur <- "months" %#% max_dur %#% "days"
    for (i in 1:n_tries) {
      
      dur <- "months" %#% max(y) %#% "days"
      diff_dur <- diff(c(goal_dur, dur))
      
      if (yunits == "days") {
        threshold <- .1
        x <- c(10, 1)
      } else if (yunits == "months") {
        threshold <- .05
        x <- c(100, 10)
      } else if (yunits == "years") {
        threshold <- .02
        x <- c(120, 100)
      }
      
      if (abs(diff_dur) > threshold) {
        
        if (!sign(diff_dur == -1)) {
          init[1] <- init[1] + x[1]
        } else { 
          init[1] <- init[1] - x[1] 
        }
        
        d <- init[1] + 6.756 * init[2]
        e <- 1.005511 / 
          ( 1 + exp(1.490650 *
                      (log(d/init[2]) - log(0.202345))) )
        b <- 0.847 + (0.985 - 0.847) * exp(-(init[2]) / 14.297)
        
        x <- newdata$frq_hrs
        y <- d / ( 1 + exp(b * (log(x)-log(e))) )
        
      } else { 
        break
      }
    }
    
    newdata$dur_sec <- y %#% "days"
    newdata$dur_mth <- "months" %#% newdata$dur_sec
    
    if (max(newdata$dur_sec) > cutoff) {
      newdata$color <- as.factor(dplyr::case_when(
        newdata$dur_sec < cutoff ~ "red",
        newdata$dur_sec >= cutoff ~ "blue"))
    } else { newdata$color <- "red" }
    
  } else {
    
    # k <- 8.046066
    newdata <- data %>%
      dplyr::select(dti_notes, dti, frq_hrs) %>%
      dplyr::filter(frq_hrs >= x_min)
    
    y0 <- yrange
    
    x <- newdata$frq_hrs
    y <- y0 / ( 1 + exp(log(x) - log(k)) )
    newdata$dur <- y %#% yunits
    newdata$dur_mth <- "months" %#% newdata$dur
    
    if (max(newdata$dur) > cutoff) {
      newdata$color <- as.factor(dplyr::case_when(
        newdata$dur < cutoff ~ "red",
        newdata$dur >= cutoff ~ "blue"))
    } else { newdata$color <- "red" }
  }
  
  # pri <- t(paste("----- number of attempts i:", i))
  newdata$id <- 1:nrow(newdata)
  return(newdata)
}

#' Estimate computation time
#'
#' @description Calculate computation time of ctmm functions.
#' @keywords internal
#'
#' @noRd
#'
estimate_time <- function(data, 
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
  
  n <- 200
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
                             units = "secs") %>%
        as.numeric()
      
    } # end of if (type == "fit")
    
    if (type == "speed") {
      
      if (missing(fit)) {
        stop("ctmm `fit` object argument not provided.")
      }
      
      n <- 10
      start_test <- Sys.time()
      units <- "minute"
      
      tmpdat <- data[1:n, ]
      inputList <- align_lists(list(fit), list(tmpdat))
      speed <- par.speed(inputList, trace = trace, parallel = parallel)
      
      total_time <- difftime(Sys.time(), start_test,
                             units = "secs") %>%
        as.numeric() / 100
      
    } # end of if (type == "speed")
    
    expt <- ((total_time/n) * nrow(data))
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
    # Process multiple animals on multiple cores: #TODO
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
