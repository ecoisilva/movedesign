
#' @title Download home range simulations file
#' 
#' @importFrom ctmm %#%
#' @importFrom dplyr group_by summarize select mutate
#' 
#' @noRd
get_hrange_file <- function() {
  
  url <- paste0(
    "https://github.com/ecoisilva/ecoisilva.github.io/",
    "raw/refs/heads/main/static/data/sims_hrange.csv")
  
  read_remote_csv <- function(url) {
    tryCatch({
      utils::read.csv(url, stringsAsFactors = FALSE)
    },
    error = function(e) {
      message("Remote resource not available or has changed.")
      invisible(NULL)
    })
  }
  
  out <- read_remote_csv(url)
  
  if (is.null(out)) {
    return(invisible(NULL))
  }
  
  out_sum <- out %>%
    dplyr::group_by(.data$duration, .data$tau_p) %>%
    dplyr::summarise(
      error = mean(.data$error, na.rm = TRUE), 
      error_lci = mean(.data$error_lci, na.rm = TRUE),
      error_uci = mean(.data$error_uci, na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::select(dplyr::all_of(c("duration",
                                  "tau_p",
                                  "error",
                                  "error_lci",
                                  "error_uci")))
  
  sims_hrange <- list(data = out, summary = out_sum)
  return(sims_hrange)
} 


#' @title Download speed simulations file
#' 
#' @importFrom ctmm %#%
#' @importFrom dplyr group_by summarize select mutate left_join
#' 
#' @noRd
get_speed_file <- function() { 
  
  url <- paste0(
    "https://github.com/ecoisilva/ecoisilva.github.io/",
    "raw/refs/heads/main/static/data/sims_speed.csv")
  
  read_remote_csv <- function(url) {
    tryCatch({
      utils::read.csv(url, stringsAsFactors = FALSE)
    },
    error = function(e) {
      message("Remote resource not available or has changed.")
      invisible(NULL)
    })
  }
  
  out <- read_remote_csv(url)
  
  if (is.null(out)) {
    return(invisible(NULL))
  }
  
  out_summary <- out %>%
    dplyr::group_by(.data$tau_v, .data$dur, .data$dti) %>%
    dplyr::summarise(
      error = mean(.data$error, na.rm = TRUE),
      error_lci = mean(.data$error_lci, na.rm = TRUE),
      error_uci = mean(.data$error_uci, na.rm = TRUE),
      .groups = "drop")
  
  fixes_per_day <- c(1, 2^seq(1, 11, by = 1)) # Number of fixes per day
  dti_notes <- data.frame(
    dti = as.numeric(c((round((1 %#% "day") /
                                fixes_per_day, 0)), 20)),
    dti_notes = c(
      "1 fix every 24 hours",
      "1 fix every 12 hours",
      "1 fix every 6 hours",
      "1 fix every 3 hours",
      "1 fix every 1.5 hours",
      "1 fix every 45 minutes",
      "1 fix every 22.5 minutes",
      "1 fix every 11.3 minutes",
      "1 fix every 5.6 minutes",
      "1 fix every 2.8 minutes",
      "1 fix every 1.4 minutes",
      "1 fix every 42 seconds",
      "1 fix every 20 seconds") 
  )
  
  out <- dplyr::left_join(out, dti_notes, by = "dti")
  out_summary <- dplyr::left_join(out_summary, dti_notes, by = "dti")
  
  sims_speed <- list(data = out, summary = out_summary)
  return(sims_speed)
}


#' @title Abbreviate units
#' 
#' @description create abbreviations of units
#' @param unit character. a character vector of units
#' @param ui_only logical. if TRUE, squared units use the superscript
#'   character; if FALSE, they use the caret form
#' 
#' @return a character vector of the same length as `unit`
#' 
#' @noRd
abbrv_unit <- function(unit, ui_only = TRUE) {
  
  if (missing(unit))
    stop("`unit` argument not provided.")
  
  if (!is.character(unit))
    stop("`unit` argument must be a character string.")
  
  key <- c(
    "year" = "yr", "month" = "mth", "week" = "wk", "day" = "d",
    "hour" = "hr", "minute" = "min", "second" = "sec",
    "yr" = "yr", "mth" = "mth", "wk" = "wk", "d" = "d",
    "hr" = "hr", "min" = "min", "sec" = "sec",
    
    "kilometer" = "km", "meter" = "m",
    "km" = "km", "m" = "m",
    
    "square kilometer" = "km^2", "km^2" = "km^2",
    "square meter" = "m^2", "m^2" = "m^2",
    "hectare" = "ha", "ha" = "ha",
    
    "kilometers/hour" = "km/h", "meters/second" = "m/s",
    "kilometers/day" = "km/day", "meters/day" = "m/day",
    "km/h" = "km/h", "m/s" = "m/s",
    "km/day" = "km/day", "m/day" = "m/day")
  
  x <- sub("\u00B2$", "^2", unit)
  x[x == "hm^2"] <- "hectare"
  x <- ifelse(x == "m/s", "m/s", sub("(.)s$", "\\1", x))
  
  idx <- pmatch(x, names(key), duplicates.ok = TRUE)
  
  if (any(is.na(idx)))
    stop("Invalid or ambiguous unit: ",
         paste(x[is.na(idx)], collapse = ", "),
         call. = FALSE)
  
  out <- unname(key[idx])
  if (ui_only) out <- sub("\\^2$", "\u00B2", out)
  
  return(out)
}


#' @title Fix values and units of space and time
#' 
#' @description correctly convert values and units for reporting
#' @param value numeric, integer
#' @param unit character vector of time units
#' @return a list with the corrected value and the corrected unit
#' 
#' @importFrom dplyr case_when
#' @importFrom dplyr add_row
#' @importFrom ctmm %#%
#' 
#' @noRd
fix_unit <- function(input,
                     unit,
                     digits = 1,
                     ui = FALSE,
                     match_all = TRUE,
                     convert = FALSE) {
  
  if (missing(unit)) {
    if (!("value" %in% names(input)) || !("unit" %in% names(input)))
      stop("input must contain named columns 'value' and 'unit'.")
    
    value <- input$value
    unit <- input$unit
  } else {
    value <- input
  }
  
  if (!is.numeric(value)) stop("'value' must be numeric.")
  if (!is.character(unit)) stop("`unit` must be a character string.")
  if (length(unit) == 0) stop("`unit` must not be empty.")
  
  if (length(unit) != length(value))
    unit <- rep(unit, length.out = length(value))
  
  unit <- sub("\u00B2$", "^2", unit)
  
  units_tm <- c("year", "month", "week",
                "yr", "mon", "wk",
                "day", "hour", "minute", "second",
                "d", "hr", "min", "sec")
  units_sp <- c("kilometer", "meter", "km", "m")
  units_ar <- c("square kilometer", "square meter", "hectare",
                "km^2", "m^2", "hm^2", "ha")
  units_vl <- c("kilometers/day", "kilometer/day", "km/day",
                "meters/day", "meter/day", "m/day",
                "kilometers/hour", "kilometer/hour", "km/h",
                "meters/second", "meter/second", "m/s")
  
  all_units <- c(units_tm, units_sp, units_ar, units_vl)
  
  .display <- function(u, val) {
    if (u %in% units_ar) {
      return(switch(u,
                    "km^2" = "km\u00B2",
                    "m^2" = "m\u00B2",
                    "hm^2" = "ha",
                    "ha" = "ha",
                    u))
    }
    if (u %in% units_vl) {
      return(switch(u,
                    "km/h" = "kilometers/hour",
                    "km/day" = "kilometers/day",
                    "m/s" = "meters/second",
                    u))
    }
    return(u)
  }
  
  .pluralize <- function(u, val) {
    if (!(u %in% units_tm)) return(u)
    if (is.na(val)) return(u)
    if (val == 1) return(u)
    return(paste0(u, "s"))
  }
  
  n <- length(value)
  x_conv <- character(n)
  y <- numeric(n)
  
  for (i in seq_len(n)) {
    
    u_i <- if (unit[i] %in% units_vl) unit[i] else
      gsub("(.)s$", "\\1", unit[i])
    
    idx <- pmatch(u_i, all_units, duplicates.ok = TRUE)
    if (is.na(idx))
      stop("Invalid or ambiguous unit: ", u_i, call. = FALSE)
    
    u_i <- all_units[idx]
    y[i] <- ifelse(convert, value[i] %#% u_i, value[i])
    u_new <- u_i
    
    if (convert) {
      if (u_i %in% units_tm) {
        u_new <- dplyr::case_when(
          y[i] < 60 ~ "second",
          y[i] < 3600 ~ "minute",
          y[i] < 86400 ~ "hour",
          y[i] < (1 %#% "month") ~ "day",
          y[i] < (1 %#% "year") ~ "month",
          TRUE ~ "year")
        
      } else if (u_i %in% units_sp) {
        u_new <- ifelse(y[i] >= 1000, "km", "m")
        
      } else if (u_i %in% units_ar) {
        u_new <- dplyr::case_when(
          y[i] < 1e4 ~ "m^2",
          y[i] < 1e6 ~ "ha",
          TRUE ~ "km^2")
        
      } else if (u_i %in% units_vl) {
        u_new <- dplyr::case_when(
          y[i] < 0.01 ~ "m/s",
          y[i] < 0.25 ~ "km/day",
          TRUE ~ "km/h")
      }
      
      y[i] <- u_new %#% y[i]
    }
    
    x_conv[i] <- u_new
    
    if (digits == 1) {
      y[i] <- ifelse(
        isTRUE(all.equal(round(y[i], 1), round(y[i], 0))),
        round(y[i], 0),
        round(y[i], 1))
    } else {
      y[i] <- round(y[i], digits)
    }
    
  } # end of loop
  
  if (match_all && n > 1) {
    si <- vapply(seq_len(n), function(i) y[i] %#% x_conv[i],
                 numeric(1))
    out_unit <- x_conv[which.max(si)]
    
    y <- vapply(si, function(v) out_unit %#% v, numeric(1))
    y <- if (digits == 1) round(y, 1) else round(y, digits)
    x_conv <- rep(out_unit, n)
  }
  
  x_out <- vapply(seq_len(n), function(i) {
    u <- if (ui) .display(x_conv[i], y[i]) else x_conv[i]
    .pluralize(u, y[i])
  }, character(1))
  
  return(data.frame(value = y, unit = x_out))
}


#' @title Prepare movement model
#' 
#' @description prepare model for movement data simulation
#' @param tau_p numeric, integer. position autocorrelation timescale
#' @param tau_p_units character vector of tau p units
#' @param tau_v numeric, integer. velocity autocorrelation timescale
#' @param tau_v_units character vector of tau v units
#' @param sigma numeric, integer. location variance
#' @param tau_p_units character vector of sigma units
#' @param mu numeric vector of length 2 in the format c(x, y)
#' 
#' @importFrom ctmm %#%
#' 
#' @noRd
prepare_mod <- function(tau_p, tau_p_unit = NULL,
                        tau_v, tau_v_unit = NULL,
                        sigma, sigma_unit = NULL,
                        mu = NULL,
                        isotropic = TRUE) { 
  
  if (missing(tau_p)) stop("tau_p is required.")
  if (missing(tau_p_unit)) {
    
    if (!("unit" %in% names(tau_p)))
      stop("tau must contain named columns 'value' and 'unit'.")
    taup <- tau_p$value %#% tau_p$unit
  } else { taup <- tau_p %#% tau_p_unit }
  
  if (missing(sigma)) stop("sigma is required.")
  if (missing(sigma_unit)) {
    
    if (!("unit" %in% names(sigma)))
      stop("sigma must contain named columns 'value' and 'unit'.")
    sig <- sigma$value %#% sigma$unit
  } else { sig <- sigma %#% sigma_unit }
  
  if (is.null(mu)) mu <- c(0, 0) else mu <- c(mu[1], mu[2])
  
  if (is.null(tau_v)) {
    mod <- ctmm::ctmm(tau = taup,
                      isotropic = isotropic,
                      sigma = sig,
                      mu = mu)
    return(mod)
  }
  
  if (missing(tau_v)) stop("tau_v is required.")
  if (missing(tau_v_unit)) {
    
    if (!("unit" %in% names(tau_v)))
      stop("tau must contain named columns 'value' and 'unit'.")
    tauv <- tau_v$value %#% tau_v$unit
  } else { tauv <- tau_v %#% tau_v_unit }
  
  # Generate movement model:
  mod <- ctmm::ctmm(tau = c(taup, tauv),
                    isotropic = isotropic,
                    sigma = sig,
                    mu = mu)
  return(mod)
}


#' @title ctmm::mean() but seeded
#' 
#' @noRd
mean_seeded <- function(obj, seed) {
  set.seed(seed) 
  return(quiet(suppressMessages(mean(obj))))
} 


#' @title ctmm::emulate() but seeded
#' 
#' @noRd
emulate_seeded <- function(obj, seed) { 
  set.seed(seed) 
  return(ctmm::emulate(obj, fast = TRUE)) 
} 


#' @title ctmm::simulate() but seeded
#' 
#' @noRd
simulate_seeded <- function(obj, seed) {
  set.seed(seed) 
  return(suppressWarnings(ctmm::simulate(obj)))
} 


#' @title Get true home range area
#' 
#' @importFrom ctmm %#%
#' 
#' @noRd
get_true_hr <- function(data = NULL,
                        seed = NULL,
                        sigma,
                        
                        ind_var = TRUE,
                        fit = NULL,
                        
                        grouped = FALSE,
                        groups = NULL,
                        
                        summarized = FALSE,
                        recenter = TRUE,
                        level = 0.95) {
  
  if (!summarized && length(data) == 0)
    stop("`data` is required when `summarized = FALSE`.",
         call. = FALSE)
  
  k <- -2 * log(1 - level)
  
  .hr_na <- function()
    list(area = NA, data = NULL)
  
  .is_range_resident <- function(fit_i) {
    if (is.null(fit_i)) return(TRUE)
    return(isTRUE(fit_i$range))
  }
  
  .get_center <- function(fit_i) {
    if (recenter || is.null(fit_i$mu)) return(c(0, 0))
    return(as.numeric(fit_i$mu[1, ])[1:2])
  }
  
  .covm_decomp <- function(x) {
    
    e <- eigen(.as_covm_matrix(x$sigma), symmetric = TRUE)
    
    return(list(values = .clamp(e$values, min = 0, max = Inf),
                vectors = e$vectors))
  }
  
  .hr_from_eigen <- function(lambda,
                             vectors = NULL,
                             center = c(0, 0),
                             n = 100) {
    
    if (length(lambda) != 2L ||
        any(!is.finite(lambda)) || min(lambda) <= 0) {
      warning("Degenerate covariance: home range area is undefined.",
              call. = FALSE)
      return(.hr_na())
    }
    
    radius <- sqrt(k * lambda)
    angle <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
    xy <- cbind(radius[1] * cos(angle), radius[2] * sin(angle))
    
    if (!is.null(vectors)) xy <- xy %*% t(vectors)
    xy[, 1] <- xy[, 1] + center[1]
    xy[, 2] <- xy[, 2] + center[2]
    
    outline <- data.frame(id = rep(1L, n),
                          angle = angle,
                          x = xy[, 1],
                          y = xy[, 2])
    
    return(list(area = pi * radius[1] * radius[2],
                data = outline))
  }
  
  .get_hr <- function(fit_i, sigma_i) {
    
    if (!.is_range_resident(fit_i)) {
      warning("Model is not range-resident (BM/IOU): home range area ",
              "is undefined.", call. = FALSE)
      return(.hr_na())
    }
    
    if (ind_var) {
      
      e <- .covm_decomp(fit_i)
      return(.hr_from_eigen(e$values,
                            vectors = e$vectors,
                            center = .get_center(fit_i)))
      
    } else {
      
      sigma_g <- sigma_i$value[2] %#% sigma_i$unit[2]
      
      if (!is.finite(sigma_g) || sigma_g <= 0) {
        warning("Non-positive variance: home range area is undefined.",
                call. = FALSE)
        return(.hr_na())
      }
      
      if (is.null(fit_i))
        return(.hr_from_eigen(.eigen_from_scalar(sigma_g, ratio = 1)))
      
      e <- .covm_decomp(fit_i)
      
      if (!is.finite(e$values[1]) || e$values[1] <= 0)
        return(.hr_from_eigen(.eigen_from_scalar(sigma_g, ratio = 1)))
      
      ratio <- .clamp(e$values[2] / e$values[1], min = 0, max = 1)
      if (!is.finite(ratio) || ratio <= 0) {
        warning("Degenerate covariance: home range area is undefined.",
                call. = FALSE)
        return(.hr_na())
      }
      
      return(.hr_from_eigen(.eigen_from_scalar(sigma_g, ratio),
                            vectors = e$vectors,
                            center = .get_center(fit_i)))
    }
  }
  
  if (summarized) {
    
    nms <- names(sigma)
    
    out <- lapply(seq_along(nms), function(i) {
      x <- nms[[i]]
      .get_hr(fit[[x]], sigma[[x]])
      
    }) # end of lapply
    
    names(out) <- nms
    return(out)
    
  } else {
    
    out <- lapply(seq_along(data), function(i) {
      nm <- names(data)[[i]]
      
      group <- if (grouped) {
        if (nm %in% groups[["A"]]) "A" else "B"
      } else "All"
      
      if (ind_var) {
        seed_i <- if (is.null(seed)) NULL else as.integer(seed[[i]])
        fit_i <- simulate_seeded(fit[[group]], seed_i)
        .get_hr(fit_i, NULL)
      } else {
        .get_hr(fit[[group]], sigma[[group]])
      }
      
    }) # end of lapply
    
    names(out) <- names(data)
    return(out)
    
  } # end of if (summarized)
  
} # end of function, get_true_hr()


#' @title Get true movement speed
#' 
#' @importFrom ctmm %#%
#' 
#' @noRd
get_true_speed <- function(data,
                           seed = NULL,
                           
                           tau_p,
                           tau_v,
                           sigma,
                           
                           ind_var = TRUE,
                           fit = NULL,
                           
                           grouped = FALSE,
                           groups = NULL,
                           
                           summarized = FALSE) {
  
  speed_from_fit <- function(fit_i, seed_i = NULL) {
    
    if (!.has_velocity(fit_i))
      stop("Model has no velocity process (BM/OU/IID): mean speed ",
           "is undefined.", call. = FALSE)
    
    is_stationary <- is.character(fit_i$mean) &&
      length(fit_i$mean) == 1L && fit_i$mean == "stationary"
    if (is_stationary)
      return(.gaussian_mean_speed(.velocity_covm(fit_i)))
    
    tv <- if ("velocity" %in% names(fit_i$tau)) {
      fit_i$tau[["velocity"]]
    } else {
      pars <- extract_pars(fit_i, "velocity")[[1]]
      pars$value[[2]] %#% pars$unit[[2]]
    }
    
    return(.weighted_average_speed(tv, fit_i, seed_i))
  }
  
  speed_from_pars <- function(sigma_i, tau_p_i, tau_v_i,
                              fit_i = NULL) {
    
    s <- sigma_i$value[2] %#% sigma_i$unit[2]
    tp <- tau_p_i$value[2] %#% tau_p_i$unit[2]
    tv <- tau_v_i$value[2] %#% tau_v_i$unit[2]
    
    lambda <- .eigen_from_scalar(s / (tp * tv), .covm_ratio(fit_i))
    
    return(.gaussian_mean_speed(diag(lambda)))
  }
  
  .get_speed <- function(fit_i, sigma_i, tau_p_i, tau_v_i,
                         seed_i = NULL) {
    if (ind_var) {
      speed_from_fit(fit_i, seed_i)
    } else {
      speed_from_pars(sigma_i, tau_p_i, tau_v_i, fit_i)
    }
  }
  
  if (summarized) {
    
    nms <- names(tau_p)
    
    out <- lapply(seq_along(nms), function(i) {
      x <- nms[[i]]
      
      seed_i <- if (is.null(seed)) NULL else as.integer(seed[[i]])
      
      .get_speed(fit[[x]],
                 sigma[[x]],
                 tau_p[[x]],
                 tau_v[[x]],
                 seed_i)
      
    }) # end of lapply
    
    names(out) <- nms
    return(out)
    
  } else {
    
    out <- lapply(seq_along(data), function(i) {
      
      nm <- names(data)[[i]]
      
      group <- if (grouped) {
        if (nm %in% groups[["A"]]) "A" else "B"
      } else "All"
      
      if (ind_var) {
        
        seed_i <- if (is.null(seed)) NULL else as.integer(seed[[i]])
        fit_i <- simulate_seeded(fit[[group]], seed_i)
        
        .get_speed(fit_i, NULL, NULL, NULL, seed_i)
        
      } else {
        
        .get_speed(fit[[group]],
                   sigma[[group]],
                   tau_p[[group]],
                   tau_v[[group]])
      }
      
    }) # end of lapply
    
    names(out) <- names(data)
    return(out)
    
  } # end of if (summarized)
  
} # end of function, get_true_speed()


#' @title Calculate confidence intervals
#' 
#' @noRd
calculate_ci <- function(data, level = 0.95) { 
  
  data <- data[is.finite(data)]
  n <- length(data)
  
  if (n < 2) return(data.frame(
    CI = level, CI_low = NA, CI_high = NA))
  
  alpha <- 1 - (1 - level)/2
  
  margin <- stats::qt(alpha, df = n - 1) * stats::sd(data)/sqrt(n)
  lci <- mean(data) - margin
  uci <- mean(data) + margin
  
  return(data.frame(CI = level, CI_low = lci, CI_high = uci))
}


#' @title Extract parameters
#' 
#' @description Extracting values and units from ctmm summaries
#' 
#' @importFrom ctmm %#%
#' 
#' @noRd
extract_pars <- function(
    obj, # data = NULL,
    name = c("position", "velocity", "sigma", "speed"),
    si_units = FALSE,
    meta = FALSE) {
  
  name <- match.arg(name) 
  
  unit <- NA
  out <- NULL
  if (missing(obj))
    stop("`obj` argument not provided.")
  # if (name == "sigma" && missing(data))
  #   stop("`data` argument not provided.")
  if (class(obj)[1] != "list" && class(obj[[1]])[1] != "ctmm") {
    # if (!is.null(data)) data <- list(data)
    obj <- list(obj)
  }
  
  if (name == "position" || name == "velocity")
    var <- paste("tau", name) else
      if (name == "sigma") var <- "area" else
        if (name == "speed") var <- name
  
  if (meta && length(obj) > 1) {
    .capture_meta(obj,
                  variable = var,
                  units = !si_units,
                  verbose = FALSE,
                  plot = FALSE) -> out
    if (is.null(out)) return(NULL)
    
    unit <- extract_units(rownames(out$meta)[1])
    tmp <- c(out$meta[1, 1],
             out$meta[1, 2],
             out$meta[1, 3])
    if (name == "sigma") tmp <- tmp / -2 / log(0.05) / pi
    
    return(list(data.frame(value = tmp, unit = unit,
                           row.names = c("low", "est", "high"))))
  }
  
  out <- list()
  out <- lapply(seq_along(obj), function(x) {
    
    sum.obj <- summary(obj[[x]], units = !si_units)
    nms.obj <- rownames(sum.obj$CI)
    
    if (var == "area") {
      
      tmp <- sum.obj$CI[grep(var, nms.obj), ]
      unit <- extract_units(nms.obj[grep(var, nms.obj)])
      
      if (!is.null(nrow(tmp)))
        if (nrow(tmp) > 1)
          tmp <- subset(tmp, !grepl("^CoV", row.names(tmp)))[1,]
      
      tmp <- data.frame(value = tmp / -2 / log(0.05) / pi,
                        unit = unit)
      
      if (!si_units) tmp <- fix_unit(tmp, convert = TRUE)
      
      return(data.frame(tmp,
                        row.names = c("low", "est", "high")))
    }
    
    # Special cases of movement processes:
    tmp_name <- name
    tmp <- sum.obj$CI[grep(name, nms.obj), ]
    unit <- extract_units(nms.obj[grep(name, nms.obj)])
    
    if (length(obj[[x]]$tau) == 2 &&
        all(obj[[x]]$tau[1] == obj[[x]]$tau[2])) {
      
      # (OUOmega and OUf):
      tmp_name <- ifelse(any(grepl("decay", nms.obj)),
                         "decay", "\u03C4")
      tmp <- sum.obj$CI[grep(tmp_name, nms.obj), ]
      unit <- extract_units(nms.obj[grep(tmp_name, nms.obj)])
    }
    
    if (!is.null(nrow(tmp)))
      if (nrow(tmp) > 1)
        tmp <- subset(tmp, !grepl("^CoV", row.names(tmp)))[1,]
    unit <- extract_units(nms.obj[grep(tmp_name, nms.obj)])
    
    if (length(tmp) == 0) return(NULL)
    if (si_units && !all(is.na(tmp))) tmp <- unit %#% tmp
    
    return(data.frame(value = tmp, unit = unit,
                      row.names = c("low", "est", "high")))
  })
  
  names(out) <- names(obj)
  out[sapply(out, is.null)] <- NULL
  if (length(out) == 0) return(NULL)
  return(out)
}


#' @title Extract sampling parameters
#' 
#' @description extracting sampling parameters from ctmm summaries
#' 
#' @importFrom ctmm %#%
#' 
#' @noRd
extract_sampling <- function(obj, name,
                             units = FALSE,
                             average = FALSE) { 
  
  if (missing(obj)) {
    stop("`obj` argument not provided.", call. = FALSE)
  }
  
  if (name == "duration") name <- "period"
  
  out <- unit <- NULL
  if (missing(obj)) stop("`obj` argument not provided.") 
  if (class(obj)[1] != "list" && class(obj[[1]])[1] != "ctmm") 
    obj <- list(obj) 
  
  if (!all(vapply(obj, inherits, logical(1), "telemetry"))) {
    stop("`obj` must contain `telemetry` objects.", call. = FALSE)
  }
  
  out <- lapply(obj, function(x) {
    
    sum.obj <- summary(x) 
    nms.obj <- suppressWarnings(names(sum.obj)) 
    
    unit <- extract_units(nms.obj[grep(name, nms.obj)]) 
    tmp <- suppressWarnings(as.numeric(sum.obj[grep(name, nms.obj)])) 
    
    if (units) { 
      tmp <- tmp %#% unit 
      unit <- "seconds" 
    } else { 
      unit <- extract_units(nms.obj[grep(name, nms.obj)]) 
    } 
    
    return(data.frame(value = tmp, unit = unit))
    
  })
  
  if (average) {
    out <- data.frame(
      value = mean(vapply(out, `[[`, numeric(1), "value")),
      unit = out[[1L]]$unit)
  }
  
  return(out) 
}


#' @title Extract DOF values
#' 
#' @description extracting DOF values and units from ctmm summaries
#' 
#' @noRd
extract_dof <- function(
    obj,
    name = c("mean", "speed", "area", "diffusion")) {
  
  name <- match.arg(name)
  
  out <- NULL
  if (missing(obj)) stop("`obj` argument not provided.")
  if (class(obj)[1] != "list" && class(obj[[1]])[1] != "ctmm") {
    obj <- list(obj)
  }
  
  out <- list()
  out <- lapply(seq_along(obj), function(x) {
    
    if (is.null(obj[[x]])) return(NULL)
    if (inherits(obj[[x]], "speed")) {
      sum.obj <- obj[[x]]
    } else {
      sum.obj <- summary(obj[[x]])
    }
    
    if (is.null(sum.obj)) return(NULL)
    if (is.null(sum.obj$DOF)) return(NULL)
    if (length(sum.obj$DOF) == 0) return(NULL)
    
    nms.obj <- names(sum.obj$DOF)
    idx <- grep(name, nms.obj)
    if (length(idx) == 0) return(NULL)
    
    out_tmp <- sum.obj$DOF[[idx[1]]]
    if (length(out_tmp) != 1 || !is.finite(out_tmp)) return(NULL)
    return(out_tmp)
    
  }) # end of lapply
  
  return(out)
}


#' @title Extract semi-variance data for ggplot2
#' 
#' @description extract semivariance data
#' 
#' @importFrom ctmm %#%
#' 
#' @noRd
extract_svf <- function(data, fit = NULL, 
                        fraction = 1, level = .95, 
                        x_unit = "days", y_unit = "km^2") { 
  
  single <- class(data)[1] != "list" && class(data[[1]])[1] != "ctmm" 
  
  out <- list() 
  nms <- names(data) 
  if (single) { 
    data <- list(data) 
    fit <- list(fit) 
  } 
  
  out <- lapply(seq_along(data), function(x) { 
    
    VAR <- NULL 
    if (is.null(fit[[x]])) { 
      VAR <- ctmm::variogram(data = data[[x]]) 
    } else { 
      VAR <- ctmm::variogram(data = data[[x]], axes = fit[[x]]$axes) 
    } 
    
    V <- list(VAR) 
    max.lag <- sapply(V, function(v) dplyr::last(v$lag)) 
    max.lag <- fraction * max(max.lag) 
    V <- lapply(V, function(y) { y[y$DOF >= 1, ] }) 
    if (fraction < 1) { 
      V <- lapply(V, function(y) { y[y$lag <= max.lag, ] }) 
    } 
    xlim <- c(0, max.lag) 
    ylim <- ctmm::extent(V, level = max(level))$y 
    lag <- V[[1]]$lag 
    lag[1] <- lag[2]/1000 
    
    if (length(lag) == 1) { 
      out <- NULL 
    } else { 
      if (!is.null(fit[[x]])) { 
        fit[[x]]$tau <- fit[[x]]$tau[fit[[x]]$tau > 0] 
        
        SVF <- svf.func(fit[[x]], moment = TRUE) 
        svf <- SVF$svf 
        DOF <- SVF$DOF 
        
        if (any(diag(fit[[x]]$COV) > 0)) { 
          SVF <- Vectorize(function(t) svf(t))(lag) 
          dof <- Vectorize(function(t) { DOF(t) })(lag) 
          svf.lower <- Vectorize(function(dof)
            CI.lower(dof, level) )(dof) 
          svf.upper <- Vectorize(function(dof)
            CI.upper(dof, level) )(dof) 
        } 
      } 
      
      VAR <- data.frame(svf = VAR$SVF, 
                        dof = VAR$DOF, 
                        lag = VAR$lag) %>% 
        dplyr::slice_min(.data$lag, prop = fraction) %>% 
        dplyr::mutate(lag = x_unit %#% .data$lag) 
      
      VAR$svf_lower <- y_unit %#% ( VAR$svf * CI.lower(VAR$dof, level) ) 
      VAR$svf_upper <- y_unit %#% ( VAR$svf * CI.upper(VAR$dof, level) ) 
      VAR$svf_low50 <- y_unit %#% ( VAR$svf * CI.lower(VAR$dof, .5) ) 
      VAR$svf_upp50 <- y_unit %#% ( VAR$svf * CI.upper(VAR$dof, .5) ) 
      VAR$svf <- y_unit %#% VAR$svf 
      
      FIT <- NULL 
      if (!is.null(fit[[x]])) { 
        FIT <- data.frame( 
          svf = y_unit %#%  
            sapply(lag, Vectorize(function(t) { svf(t) })), 
          lag = x_unit %#% lag, 
          svf_lower = SVF * (y_unit %#% svf.lower), 
          svf_upper = SVF * (y_unit %#% svf.upper)) 
      } 
      
      out <- list(data = VAR, 
                  fit = FIT, 
                  x_unit = x_unit, 
                  y_unit = y_unit) 
    } 
    
    return(out) 
    
  }) # end of lapply
  
  if (!single) names(out) <- nms 
  return(out) 
  
} 

#' @title Extract outputs
#' 
#' @description extracting output values and units from ctmm summaries
#' 
#' @importFrom ctmm %#%
#' 
#' @noRd
extract_outputs <- function(obj,
                            name = c("hr", "ctsd"),
                            groups = NULL,
                            si_units = TRUE,
                            meta = TRUE) {
  
  name <- match.arg(name)
  
  unit <- NA
  out <- NULL
  if (missing(obj)) stop("`obj` argument not provided.")
  if (class(obj)[1] != "list" &&
      class(obj[[1]])[1] != "ctmm") obj <- list(obj)
  
  name <- ifelse(name == "hr", "area", "speed")
  
  if (meta && length(obj) > 1) {
    .capture_meta(obj,
                  variable = name,
                  units = !si_units,
                  verbose = FALSE,
                  plot = FALSE) -> out_meta
    if (is.null(out_meta)) return(NULL)
    unit <- extract_units(rownames(out_meta$meta)[1])
    tmp <- data.frame("lci" = out_meta$meta[1, 1], 
                      "est" = out_meta$meta[1, 2], 
                      "uci" = out_meta$meta[1, 3], 
                      "unit" = unit)
    out_meta <- cbind(
      data.frame(id = "All",
                 subject = "All",
                 group = "All"), tmp)
  }
  
  out <- lapply(seq_along(obj), function(x) {
    
    if (name == "area") sum.obj <- summary(obj[[x]])
    if (name == "speed") sum.obj <- obj[[x]]
    tmpname <- rownames(sum.obj$CI)
    tmpunit <- extract_units(
      tmpname[grep(paste0("^", name), tmpname)])
    
    if (si_units) {
      return(c("lci" = sum.obj$CI[1] %#% tmpunit,
               "est" = sum.obj$CI[2] %#% tmpunit,
               "uci" = sum.obj$CI[3] %#% tmpunit,
               "unit" = abbrv_unit("m^2", ui_only = TRUE)))
    } else {
      return(c("lci" = sum.obj$CI[1],
               "est" = sum.obj$CI[2],
               "uci" = sum.obj$CI[3],
               "unit" = abbrv_unit(tmpunit, ui_only = TRUE)))
    }
    
  }) # end of lapply
  
  if (!is.null(groups))
    obj_groups <- sapply(seq_along(obj), function(x) {
      nm <- names(obj)[[x]]
      return(ifelse(nm %in% groups[["A"]], "A", "B"))
    })
  
  out <- cbind(
    data.frame(id = names(obj),
               subject = "Individuals",
               group = if (is.null(groups)) NA else obj_groups,
               do.call(rbind, out)))
  
  out <- dplyr::arrange(out, as.numeric(.data$id))
  out <- out %>%
    dplyr::mutate(lci = as.numeric(.data$lci),
                  est = as.numeric(.data$est),
                  uci = as.numeric(.data$uci))
  if (meta && length(obj) > 1) out <- rbind(out, out_meta)
  if (!is.null(groups)) out <- out %>%
    dplyr::mutate(group = as.factor(.data$group))
  else out$group <- out$subject
  
  return(out)
} 


#' @title Simulate GPS battery life decay
#' 
#' @description Simulate GPS battery life decay
#' 
#' @param data data.frame. a dataframe with frequencies
#' @param b_max numeric. maximum duration (y) for the GPS device
#' @param b_unit character. unit for the maximum duration (y)
#' @param cutoff character. cut-off for for minimum duration required
#' @param dti_max character. maximum sampling interval (or minimum
#'   frequency) for the maximum duration
#' @param method character. `"loglogistic"` (default) applies a
#'   log-logistic model. `"power"` applies a power law
#' @param k_dti numeric. exponent of the power law
#' @param anchor_adj numeric. multiplicative correction
#' 
#' @importFrom ctmm %#%
#' @importFrom dplyr %>%
#' 
#' @noRd
simulate_gps <- function(data,
                         b_max,
                         b_unit,
                         cutoff,
                         dti_max,
                         method = c("loglogistic", "power"),
                         k_dti = 0.686,
                         anchor_adj = 1) {
  
  method <- match.arg(method)
  
  stopifnot(!is.null(data))
  if (is.null(b_max)) stop("Duration (b_max) is required.")
  if (is.null(b_unit)) stop("Unit (b_unit) is required.")
  if (is.null(cutoff)) stop("Cut-off (cutoff) is required.")
  stopifnot(is.numeric(b_max))
  stopifnot(is.numeric(cutoff))
  stopifnot(is.character(b_unit))
  stopifnot(is.character(dti_max) || is.null(dti_max))
  
  if (("days" %#% (b_max %#% b_unit)) < 2)
    stop("Duration (b_max) cannot be less than 2 days.", call. = FALSE)
  
  trace <- FALSE
  
  # Initialize parameters:
  
  unit <- "days"
  params <- data.frame(
    b_max = round(unit %#% (b_max %#% b_unit), 1),
    x_min = data$frq_hrs[match(dti_max, data$dti_notes)],
    dti_ref = data$dti[match(dti_max, data$dti_notes)])
  
  if (is.na(params[["x_min"]]))
    stop("`dti_max` does not match any entry in `data$dti_notes`.",
         call. = FALSE)
  
  newdata <- data %>%
    dplyr::select(dplyr::all_of(c("dti_notes", "dti", "frq_hrs"))) %>%
    dplyr::filter(.data$frq_hrs >= params[["x_min"]])
  
  if (nrow(newdata) == 0)
    stop("No fix rates at or above `dti_max`.", call. = FALSE)
  
  if (method == "power") {
    
    newdata$dur_sec <- anchor_adj * (params[["b_max"]] %#% unit) *
      (newdata$dti / params[["dti_ref"]])^k_dti
    
  } else {
    
    # Log-logistic model:
    
    start_time <- Sys.time()
    threshold <- ifelse(params[["b_max"]] > 31, 0.01, 1)
    
    .max_y <- function(a, b)
      max(update_f(x = newdata$frq_hrs, c(a, b))$y)
    
    .peak <- function(b) {
      lo <- -6.756 * b
      hi <- 6.756 * b + 1000
      phi <- (sqrt(5) - 1) / 2
      
      for (k in seq_len(60)) {
        m1 <- hi - phi * (hi - lo)
        m2 <- lo + phi * (hi - lo)
        if (.max_y(m1, b) < .max_y(m2, b)) lo <- m1 else hi <- m2
      }
      
      a <- (lo + hi) / 2
      return(list(a = a, value = .max_y(a, b)))
    }
    
    b_par <- params[["b_max"]]
    pk <- .peak(b_par)
    n_grow <- 0
    
    while (pk$value < params[["b_max"]] && n_grow < 100) {
      b_par <- b_par * 1.1
      pk <- .peak(b_par)
      n_grow <- n_grow + 1
    }
    
    lo <- pk$a
    hi <- pk$a + max(b_par, 1)
    k <- 0
    
    while (.max_y(hi, b_par) > params[["b_max"]] && k < 200) {
      hi <- hi + max(b_par, 1) * 2^k
      k <- k + 1
    }
    
    for (k in seq_len(100)) {
      mid <- (lo + hi) / 2
      if (.max_y(mid, b_par) > params[["b_max"]])
        lo <- mid else hi <- mid
    }
    
    init <- c((lo + hi) / 2, b_par)
    y <- update_f(x = newdata$frq_hrs, init)$y
    err <- 100 - (max(y) * 100) / params[["b_max"]]
    
    if (trace) {
      cat("max(b):", round(max(y), 1),
          "\n", "b_max:", round(params[["b_max"]], 1))
      cat(", error:", round(abs(err), 2), "%", "\n")
      
      message("Elapsed time since start:")
      elapsed <- Sys.time() - start_time
      cat(format(elapsed), "\n")
    }
    
    if (abs(err) > threshold) {
      msg_log(
        style = "error",
        message = paste0("Solver did not converge (error: ",
                         round(abs(err), 2), "%)."))
    }
    
    newdata$dur_sec <- y %#% unit
  }
  
  newdata$dur_mth <- "months" %#% newdata$dur_sec
  
  if (max(newdata$dur_sec) > cutoff) {
    newdata$cutoff <- as.factor(dplyr::case_when(
      newdata$dur_sec < cutoff ~ "Y",
      newdata$dur_sec >= cutoff ~ "N"))
  } else {
    newdata$cutoff <- as.factor(rep("Y", nrow(newdata)))
  }
  
  newdata$id <- seq_len(nrow(newdata))
  newdata <- dplyr::left_join(
    newdata,
    data %>% dplyr::select(.data$dti, .data$dti_scale, .data$dti_yn),
    by = "dti")
  
  return(newdata)
}


#' @title Calculate initial parameters
#' 
#' @description calculate initial parameters for log-logistic function
#' 
#' @param x numeric. sampling frequency, in fixes per hour
#' @param init numeric vector of length 2, c(offset, b_max)
#' 
#' @noRd
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


#' @title Rough estimation of computation time
#' 
#' @description estimate computation time of ctmm functions
#' 
#' @importFrom ctmm %#%
#' @importFrom dplyr %>%
#' 
#' @noRd
guess_time <- function(type = "fit", 
                       data = NULL, 
                       fit = NULL, 
                       dti = NULL, 
                       dur = NULL, 
                       error = NULL, 
                       seed = NULL, 
                       trace = FALSE, 
                       parallel = TRUE) { 
  
  error <- ifelse(is.null(error), FALSE, error)
  
  cal <- 1
  set_id <- 1 
  
  if (!type %in% c("fit", "speed")) 
    stop("type =", type, " is not supported.", call. = FALSE) 
  
  expt_unit <- "minute"
  expt <- expt_max <- expt_min <- 0
  
  outputs <- data.frame("mean" = 0,
                        "min" = 0,
                        "max" = 0,
                        "unit" = expt_unit,
                        "range" = "unknown")
  
  if (type == "fit") {
    if (is.null(data) || !length(data)) stop("`data` not provided.")
    
    data <- data[[set_id]]
    
    n <- 2500
    if (nrow(data) < n) {
      outputs$mean <- ifelse(nrow(data) < 1000, 1, 2)
      if (error) outputs$mean <- outputs$mean * 6
      outputs$mean <- outputs$mean * cal
      outputs$min <- outputs$mean
      outputs$max <- max(5, outputs$mean)
      outputs$unit <- ifelse(outputs$mean <= 1, "minute", "minutes")
      outputs$range <- paste("\u2264", round(outputs$mean, 0),
                             outputs$unit)
      return(outputs)
    }
    
    start <- Sys.time()
    guess <- ctmm::ctmm.guess(data[1:200, ], interactive = FALSE)
    tmp_fit <- par.ctmm.select(list(data[1:200, ]),
                               list(guess),
                               trace = trace,
                               parallel = parallel)
    total_time <- difftime(Sys.time(), start, units = "sec")[[1]]
    
    expt <- expt_unit %#% (total_time * nrow(data) / 200)
    if (error) expt <- expt * 6
    
    expt <- round_any(expt, 1, f = floor)
    expt_min <- max(round_any(expt, 1, f = floor) - 2, 0)
    expt_max <- round_any(expt, 2, f = ceiling)
    if (expt >= 15) expt_max <- round_any(expt, 5, f = ceiling)
    
  } # end of if (type == "fit")
  
  if (type == "speed") {
    
    if (is.null(fit) || !length(fit))
      stop("ctmm `fit` object not provided.")
    if (is.null(dti) || !all(c("value", "unit") %in% names(dti)))
      stop("`dti` must be a list with elements 'value' and 'unit'.")
    if (is.null(dur) || !all(c("value", "unit") %in% names(dur)))
      stop("`dur` must be a list with elements 'value' and 'unit'.")
    
    fit <- fit[[set_id]]
    sum_fit <- summary(fit)
    
    if (!("speed" %in% names(sum_fit$DOF))) return(outputs)
    if (!is.finite(sum_fit$DOF[["speed"]])) return(outputs)
    
    tauv <- extract_pars(fit, name = "velocity")[[1]]
    if (is.null(tauv)) return(outputs)
    if (tauv$value[2] == 0) return(outputs)
    tauv <- tauv$value[2] %#% tauv$unit[2]
    
    dti <- dti$value %#% dti$unit
    dur <- "days" %#% dur$value %#% dur$unit
    N <- sum_fit$DOF[["speed"]]
    
    x1 <- log(N)
    x2 <- tauv/dti
    x3 <- dur
    
    if (tauv/dti < 1) {
      y <- exp(3.4924 - 0.1978 * x1)
      
      if (N < 15) {
        x3_capped <- min(x3, 30)
        y_max <- exp(4.15038 - 0.3159 * x1 + 0.01912 * x3_capped)
        if (N <= 5) y_max <- y_max * 2
        y_max <- min(y_max, y * 20)
      } else {
        y_max <- y
      }
      
      y <- y * cal
      y_max <- y_max * cal
      
      expt <- expt_min <- ceiling(expt_unit %#% y)
      expt_max <- ifelse(
        N > 30,
        round_any(expt_unit %#% y_max, 2, f = ceiling),
        round_any(expt_unit %#% y_max, 3, f = ceiling))
      
    } else {
      if (tauv/dti < 10)
        y <- y_max <- exp(-3.28912 + 1.01494 *
                            x1 + 0.01953 * x1 * x2)
      if (tauv/dti >= 10)
        y <- y_max <- exp(-2.0056285 + 0.9462089 *
                            x1 + 0.0023285 * x1 * x2)
      if (N < 15) y_max <- y_max + y_max * 2
      
      y <- y * cal
      y_max <- y_max * cal
      
      y <- expt_unit %#% y
      y_max <- expt_unit %#% y_max
      expt_min <- ceiling(y * 2) / 2
      expt <- round_any(y, 1, f = ceiling)
      expt_max <- round_any(y_max, 1, f = ceiling)
    }
    
  } # end of if (type == "speed")
  
  if (expt <= 1) {
    expt_min <- min(expt_min, 1)
    expt_max <- min(expt_max, 1)
    range <- paste("\u2264", "1", expt_unit)
  } else {
    expt_unit <- "minutes"
    expt_min <- min(expt_min, expt)
    expt_max <- max(expt_max, expt)
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


#' @title Measure straight-line distance
#' 
#' @description measure distance
#' 
#' @noRd
measure_distance <- function(data) {
  
  n <- nrow(data)
  if (is.null(n) || n < 2) return(rep(0, max(c(n, 0)))) 
  
  dx <- diff(data$x)
  dy <- diff(data$y)
  
  return(c(0, sqrt(dx^2 + dy^2)))
}


#' @title Estimate distance from trajectory
#' 
#' @description estimate distance from ctmm::speed()
#' 
#' @noRd
estimate_trajectory <- function(data, 
                                fit, 
                                groups = NULL, 
                                dur, 
                                tau_v, 
                                seed) { 
  
  grouped <- ifelse(is.null(groups), FALSE, TRUE) 
  
  if (class(data)[1] != "list" && 
      class(data[[1]])[1] != "ctmm")  
    stop("data argument needs to be a named list.") 
  
  if (class(fit)[1] != "list" && 
      class(fit[[1]])[1] != "ctmm") 
    stop("fit argument needs to be a named list.") 
  
  if (!is.list(seed) || length(seed) != length(data)) 
    stop("seed argument needs to be a list matching `data`.", 
         call. = FALSE)
  
  nms <- names(data) 
  out <- lapply(seq_along(data), function(x) { 
    
    group <- 1 
    if (grouped) { 
      nm <- names(data)[[x]] 
      group <- ifelse(nm %in% groups$A, "A", "B") 
    } 
    
    tau_v <- tau_v[[group]]$value[2] %#% tau_v[[group]]$unit[2] 
    dti <- ifelse(tau_v <= 1 %#% "min", 1 %#% "min", tau_v/10) 
    dur <- dur$value %#% dur$unit 
    
    t_new <- seq(0, round(dur, 0), by = dti)[-1] 
    path <- ctmm::simulate(data[[x]],  
                           fit[[x]],  
                           seed = seed[[x]], 
                           t = t_new) 
    path$dist <- measure_distance(path) 
    return(path) 
    
  }) # end of lapply
  
  names(out) <- nms 
  return(out) 
} 


#' @title Convert to a different unit
#' 
#' @description convert to a different unit
#' 
#' @importFrom ctmm %#%
#' 
#' @noRd
convert_to <- function(x,
                       unit,
                       new_unit = NULL,
                       to_text = FALSE) {
  
  if (is.data.frame(x) &&
      "value" %in% names(x) && "unit" %in% names(x)) {
    unit <- x$unit
    x <- x$value
  } else if (missing(unit)) {
    stop("'unit' must be specified when passing a single value.")
  }
  
  if (is.null(new_unit))
    stop("'new_unit' must be specified.", call. = FALSE)
  
  out <- fix_unit((new_unit %#% (x %#% unit)), new_unit)
  if (to_text) out <- paste0(out[1, 1], " ", out[1, 2])
  
  return(out)
}
