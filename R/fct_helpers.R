#' Download home range simulations file 
#' 
#' Downloads and reads a dataset hosted externally. 
#' @return Returns a data.frame with the dataset. 
#'  
#' @keywords internal 
#' 
#' @importFrom ctmm %#% 
#' @importFrom dplyr group_by summarize select mutate 
#' @noRd 
get_hrange_file <- function() { 
  
  url <- paste0( 
    "https://github.com/ecoisilva/ecoisilva.github.io/", 
    "raw/refs/heads/main/static/data/sims_hrange.csv" 
  ) 
  
  out <- read.csv(url) 
  
  out_sum <- out %>% 
    dplyr::group_by(.data$duration, .data$tau_p) %>% 
    dplyr::summarise( 
      error = mean(.data$error, na.rm = TRUE), 
      error_lci = mean(.data$error_lci, na.rm = TRUE), 
      error_uci = mean(.data$error_uci, na.rm = TRUE), 
      .groups = "drop" 
    ) %>% 
    dplyr::select(dplyr::all_of(c("duration",  
                                  "tau_p", 
                                  "error", 
                                  "error_lci",  
                                  "error_uci"))) 
  
  sims_hrange <- list(data = out, summary = out_sum) 
  return(sims_hrange) 
} 

#' Download speed simulations file 
#' 
#' Downloads and reads a dataset hosted externally. 
#' @return Returns a data.frame with the dataset. 
#'  
#' @keywords internal 
#' 
#' @importFrom ctmm %#% 
#' @importFrom dplyr group_by summarize select mutate left_join 
#' @noRd 
get_speed_file <- function() { 
  
  url <- paste0( 
    "https://github.com/ecoisilva/ecoisilva.github.io/", 
    "raw/refs/heads/main/static/data/sims_speed.csv" 
  ) 
  
  out <- read.csv(url) 
  
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
      "1 fix every 20 seconds" 
    ) 
  ) 
  
  out <- dplyr::left_join(out, dti_notes, by = "dti") 
  out_summary <- dplyr::left_join(out_summary, dti_notes, by = "dti") 
  
  sims_speed <- list(data = out, summary = out_summary) 
  return(sims_speed) 
} 

#' Abbreviate units 
#' 
#' @description Create abbreviations of units. 
#' @param unit Character. A character vector for a unit. 
#' 
#' @return Returns a character vector with one element. 
#' 
#' @examples 
#' movedesign::abbrv_unit("square kilometers") 
#' 
#' @noRd 
abbrv_unit <- function(unit, ui_only = TRUE) { 
  
  if (missing(unit)) 
    stop("`unit` argument not provided.") 
  
  if (!is.character(unit)) 
    stop("`unit` argument must be a character string.") 
  
  all_units <- c("year", "month", "week", 
                 "day", "hour", "minute", "second", 
                 "hr", "min", "sec", 
                 "kilometer", "meter", "km", "m", 
                 "km^2", "m^2","hm^2", "ha",  
                 "km\u00B2", "m\u00B2", "hm\u00B2", 
                 "square kilometer", "square meter", "hectare", 
                 "kilometers/hour", "meters/second", 
                 "kilometers/day" , "meters/day", 
                 "km/h", "m/s", 
                 "km/day", "m/day") 
  
  if (unit == "km\u00B2") unit <- "km^2" 
  if (unit == "m\u00B2") unit <- "m^2" 
  if (unit == "hm\u00B2" || unit == "hm^2") unit <- "hectare" 
  
  x <- ifelse(unit == "m/s", "m/s", gsub("(.)s$", "\\1", unit)) 
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
  if (x == "hectare" || x == "hm^2") out <- "ha" 
  
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
#' @keywords internal 
#' 
#' @importFrom dplyr case_when 
#' @importFrom dplyr add_row 
#' @importFrom ctmm %#% 
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
  
  if (!is.numeric(value)) stop("'value' must be numeric.")  
  if (!is.character(unit)) stop("`unit` must be a character string.") 
  
  for (x in seq_along(unit)) { 
    if (unit[x] == "km\u00B2") unit[x] <- "km^2" 
    if (unit[x] == "m\u00B2") unit[x] <- "m^2" 
    if (unit[x] == "hm\u00B2") unit[x] <- "hm^2" 
  } 
  
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
    if ((x[i] %in% units_tm) && convert) { 
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
    if ((x[i] %in% units_sp) && convert) { 
      x_new <- ifelse(y[i] >= 1000, "km", "m") 
      if (ui) x_html[i] <- x_new 
    } 
    
    # Convert area units: 
    if ((x[i] %in% units_ar) && convert) { 
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
    if ((x[i] %in% units_vl) && convert) { 
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
  
  # Match all units to one unit (if length > 1): 
  if (match_all && length(value) > 1) { 
    
    max_y <- sapply(seq_along(value), 
                    function(i) value[i] %#% unit[i]) 
    max_index <- which.max(max_y) 
    
    out_unit <- out[max_index, 2] 
    for (i in seq_len(nrow(out))) 
      out[i,1] <- out_unit %#% out[i,1] %#% out[i,2]  
    out[, 2] <- rep(out_unit, nrow(out)) 
  } 
  
  return(out) 
} 


#' Prepare movement model 
#' 
#' @description Prepare model for movement data simulation 
#' @keywords internal 
#' 
#' @param tau_p numeric, integer. position autocorrelation timescale. 
#' @param tau_p_units character vector of tau p units. 
#' @param tau_v numeric, integer. velocity autocorrelation timescale. 
#' @param tau_v_units character vector of tau v units. 
#' @param sigma numeric, integer. semi-variance or sigma. 
#' @param tau_p_units character vector of sigma units. 
#' @param mu numeric vector of length 2 in the format c(x, y). 
#' 
#' @importFrom ctmm %#% 
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


#' ctmm::emulate() but seeded 
#'  
#' @noRd 
emulate_seeded <- function(obj, seed) { 
  set.seed(seed) 
  return(ctmm::emulate(obj, fast = TRUE)) 
} 

#' ctmm::simulate() but seeded 
#'  
#' @noRd 
simulate_seeded <- function(obj, seed) { 
  set.seed(seed) 
  return(ctmm::simulate(obj)) 
} 


#' Get true home ranges 
#'  
#' @importFrom ctmm %#% 
#' @noRd 
get_true_hr <- function(data = NULL, 
                        seed = NULL, 
                        sigma, 
                        
                        emulated = TRUE, 
                        fit = NULL, 
                        
                        grouped = FALSE, 
                        groups = NULL, 
                        
                        summarized = FALSE) { 
  
  get_circle <- function(radius_x, radius_y) { 
    
    mean_x <- 0 # fit$mu[[1]][1] 
    mean_y <- 0 # fit$mu[[1]][2] 
    
    truth <- data.frame( 
      id = rep(1, each = 100), 
      angle = seq(0, 2 * pi, length.out = 100)) 
    truth$x <- unlist(lapply( 
      mean_x, function(x) x + radius_x * cos(truth$angle))) 
    truth$y <- unlist(lapply( 
      mean_y, function(x) x + radius_y * sin(truth$angle))) 
    return(truth) 
  } 
  
  if (summarized) { 
    
    out <- lapply(names(sigma), function(x) { 
      
      if (emulated) { 
        
        if (fit[[x]]$isotropic[["sigma"]]) { 
          
          sig <- var.covm(fit[[x]]$sigma, average = TRUE) 
          radius_x <- radius_y <- sqrt(-2 * log(0.05) * sig) 
          area <- -2 * log(0.05) * pi * sig 
          
        } else { 
          
          sigma <- fit[[x]]$sigma 
          if (ncol(sigma) == 1) { 
            sigma <- sigma@par["major"] 
          } else { 
            sigma <- attr(sigma, "par")[c("major", "minor")] 
            sigma <- sort(sigma, decreasing = TRUE) 
          } 
          
          radius_x <- sqrt(-2 * log(0.05) * sigma[["major"]]) 
          radius_y <- sqrt(-2 * log(0.05) * sigma[["minor"]]) 
          area <- pi * radius_x * radius_y 
          
        } # end of if (is_isotropic) 
        
      } else { 
        
        sig <- sigma[[x]]$value[2] %#% sigma[[x]]$unit[2] 
        radius_x <- radius_y <- sqrt(-2 * log(0.05) * sig) 
        area <- -2 * log(0.05) * pi * sig 
        
      } # end of if (is_emulate) 
      
      truth <- get_circle(radius_x, radius_y) 
      return(list(area = area, data = truth)) 
      
    }) # end of lapply (x) 
    
    names(out) <- names(sigma) 
    return(out) 
    
  } else { 
    
    out <- lapply(seq_along(data), function(x) { 
      
      if (grouped) { 
        nm <- names(data)[[x]] 
        group <- ifelse(nm %in% groups[["A"]], "A", "B") 
      } else group <- "All" 
      
      if (emulated) { 
        fit <- emulate_seeded(fit[[group]], names(data)[[x]]) 
        
        if (fit$isotropic[["sigma"]]) { 
          sig <- var.covm(fit$sigma, average = TRUE) 
          radius_x <- radius_y <- sqrt(-2 * log(0.05) * sig) 
          area <- -2 * log(0.05) * pi * sig 
          
        } else { 
          sig1 <- fit$sigma@par["major"][[1]] 
          sig2 <- fit$sigma@par["minor"][[1]] 
          radius_x <- sqrt(-2 * log(0.05) * sig1) 
          radius_y <- sqrt(-2 * log(0.05) * sig2) 
          
          fit_ellipse <- ellipse(fit$sigma, level = 0.95) 
          semi_axis_1 <- max(fit_ellipse[,1])/2 - min(fit_ellipse[,1])/2 
          semi_axis_2 <- max(fit_ellipse[,2])/2 - min(fit_ellipse[,2])/2 
          area <- pi * semi_axis_1 * semi_axis_2 
          
        } # end of if (is_isotropic) 
        
      } else { 
        
        sig <- sigma[[group]]$value[2] %#% sigma[[group]]$unit[2] 
        radius_x <- radius_y <- sqrt(-2 * log(0.05) * sig) 
        area <- -2 * log(0.05) * pi * sig 
        
      } # end of if (is_emulate) 
      
      truth <- get_circle(radius_x, radius_y) 
      return(list(area = area, data = truth)) 
      
    }) # end of lapply (x) 
    
    names(out) <- names(data) 
    return(out) 
    
  } # end of if (summarized) 
  
} 

#' Get weighted average speed (approximate only) 
#'  
#' @noRd 
weighted_average_speed <- function(tau_v, fit, seed,  
                                   err = 0.01, cor.min = 0.5) { 
  
  dt.max <- -log(cor.min) * tau_v 
  
  dt <- tau_v * (err/10)^(1/3) # O(error/10) inst error 
  t <- seq(0, tau_v/err^2, dt) # O(error) est error 
  dat <- ctmm::simulate(fit, t = t, 
                        seed = seed, 
                        precompute = FALSE) 
  v <- sqrt(dat$vx^2 + dat$vy^2) 
  
  w <- diff(t) 
  w <- w * (w <= dt.max) 
  w <- c(0,w) + c(w,0) 
  w <- w * (t >= range(dat$t)[1] & t <= range(dat$t)[2]) 
  v <- sum(w * v)/sum(w) # weighted average speed 
  return(v) 
} 

#' Get true movement speed 
#'  
#' @importFrom ctmm %#% 
#' @noRd 
get_true_speed <- function(data, 
                           seed = NULL, 
                           
                           tau_p, 
                           tau_v, 
                           sigma, 
                           
                           emulated = TRUE, 
                           fit = NULL, 
                           
                           grouped = FALSE, 
                           groups = NULL, 
                           
                           summarized = FALSE) { 
  
  clamp <- function (num, min = 0, max = 1) { 
    ifelse(num < 0, 0, ifelse(num > 1, 1, num)) 
  } 
  
  if (summarized) { 
    
    out <- lapply(names(tau_p), function(x) { 
      
      if (emulated) { 
        fit <- fit[[x]] 
        sigma <- fit$sigma 
        tau <- fit$tau 
        
        if (fit$range) { 
          sigma <- sigma/prod(fit$tau) # OUF 
        } else { sigma <- sigma/fit$tau[2] } # IOU 
        
        if (fit$mean == "stationary") { 
          sigma <- eigen(sigma)$values 
          if (fit$isotropic[["sigma"]] || sigma[1] == sigma[2]) { 
            truth <- sqrt(sigma[1] * pi/2) 
          } else { 
            truth <- sqrt(2/pi) * sqrt(sigma[1]) * 
              ellipke(1 - clamp(sigma[2]/sigma[1]))$e  
          } 
          
        } else { 
          if (is.null(tau[["velocity"]])) { 
            tau_v <- extract_pars(fit, "velocity")[[1]] 
            tau_v <- tau_v$value[[2]] %#% tau_v$unit[[2]] 
          } else { 
            tau_v <- tau[["velocity"]] 
          } 
          truth <- weighted_average_speed(tau_v, fit, seed[[1]]) 
        } 
        
      } else { 
        sigma <- sigma[[x]]$value[2] %#% sigma[[x]]$unit[2] 
        tau_p <- tau_p[[x]]$value[2] %#% tau_p[[x]]$unit[2] 
        tau_v <- tau_v[[x]]$value[2] %#% tau_v[[x]]$unit[2] 
        
        truth <- sqrt(sigma * pi/2) 
        truth <- truth/sqrt(prod(tau_p, tau_v)) # error ~ 0.01 
      } 
      
    }) # end of lapply (x) 
    
    names(out) <- names(tau_p) 
    return(out) 
    
  } else { 
    
    out <- lapply(seq_along(data), function(x) { 
      nm <- names(data)[[x]] 
      if (grouped) { 
        group <- ifelse(nm %in% groups[["A"]], "A", "B") 
      } else group <- "All" 
      
      if (emulated) { 
        fit <- emulate_seeded(fit[[group]], nm) 
        sigma <- var.covm(fit$sigma, average = TRUE) 
        
        sigma <- fit$sigma 
        tau <- fit$tau 
        
        if (fit$range) { 
          sigma <- sigma/prod(fit$tau) # OUF 
        } else { sigma <- sigma/fit$tau[2] } # IOU 
        
        if (fit$mean == "stationary") { 
          sigma <- eigen(sigma)$values 
          if (fit$isotropic[["sigma"]] || sigma[1] == sigma[2]) { 
            truth <- sqrt(sigma[1] * pi/2) 
          } else { 
            truth <- sqrt(2/pi) * sqrt(sigma[1]) * 
              ellipke(1 - clamp(sigma[2]/sigma[1]))$e  
          } 
          
        } else { 
          if (is.null(tau[["velocity"]])) { 
            tau_v <- extract_pars(fit, "velocity")[[1]] 
            tau_v <- tau_v$value[[2]] %#% tau_v$unit[[2]] 
          } else { 
            tau_v <- tau[["velocity"]] 
          } 
          truth <- weighted_average_speed(tau_v, fit, nm) 
        } 
        
      } else { 
        sigma <- sigma[[group]]$value[2] %#% sigma[[group]]$unit[2] 
        tau_p <- tau_p[[group]]$value[2] %#% tau_p[[group]]$unit[2] 
        tau_v <- tau_v[[group]]$value[2] %#% tau_v[[group]]$unit[2] 
        
        truth <- sqrt(sigma * pi/2) 
        truth <- truth/sqrt(prod(tau_p, tau_v)) # error ~ 0.01 
      } 
      
      return(truth) 
      
    }) # end of lapply (x) 
    
    names(out) <- names(data) 
    return(out) 
    
  } # end of if (summarized) 
  
} # end of function, get_true_speed() 


#' Calculate confidence intervals 
#' @noRd 
calculate_ci <- function(data, level = 0.95) { 
  
  alpha <- 1 - (1 - level)/2 # two-tailed 
  
  dof <- length(data) - 1 
  margin <- qt(alpha, df = dof) * sd(data)/sqrt(length(data)) 
  lci <- mean(data) - margin 
  uci <- mean(data) + margin 
  
  out <- data.frame( 
    CI = level, 
    CI_low = lci, 
    CI_high = uci) 
  
  return(out) 
} 


#' Extract sigma 
#' 
#' @description Extracting sigma from ctmm fit. 
#' @keywords internal 
#' 
#' @importFrom ctmm %#% 
#' @noRd 
get_sigma <- function(x, level = 0.95) { 
  
  nant <- function(x, to) { 
    NAN <- is.na(x) 
    if (any(NAN)) { 
      to <- array(to, length(x)) 
      x[NAN] <- to[NAN] 
    } 
    return(x) 
  } 
  
  get_area_covm <- function(sigma, ave = TRUE) { 
    # sigma <- x$sigma 
    
    if (ncol(sigma) == 1) { 
      sigma <- sigma@par["major"] 
    } else { 
      sigma <- attr(sigma, "par")[c("major", "minor")] 
      sigma <- sort(sigma, decreasing = TRUE) 
    } 
    DIM <- length(sigma) 
    sigma <- prod(sigma) 
    if (ave) sigma <- sigma^(1/DIM) 
    return(sigma) 
  } 
  
  
  if ("sigma" %!in% names(x)) { 
    return(NULL) 
  } 
  
  alpha <- 1 - level 
  sigma <- x$sigma@par 
  AREA <- get_area_covm(x) 
  AREA 
  
  if ("major" %!in% dimnames(x$COV)[[1]]) { 
    VAR <- Inf 
  } else if (x$isotropic[1]) { 
    VAR <- x$COV["major", "major"] 
  } else { 
    P <- c("major", "minor") 
    GRAD <- AREA/sigma[P]/2 
    VAR <- c(GRAD %*% x$COV[P, P] %*% GRAD) 
  } 
  
  if (x$range) { 
    DOF <- nant(AREA^2/abs(VAR), 0) 
  } else { 
    DOF <- 0 
  }  
  
  DOF <- 2 * DOF 
  # VAR <- 2*AREA^2/DOF 
  AREA <- chisq.ci(AREA, DOF = DOF, alpha = alpha) 
  return(AREA) 
} 


#' Extract parameters. 
#' 
#' @description Extracting values and units from ctmm summaries. 
#' @keywords internal 
#' 
#' @importFrom ctmm %#% 
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
      
      # tmp <- get_sigma(obj[[x]]) 
      # tmp <- fix_unit(data.frame(value = tmp, unit = "m^2"), 
      #                 convert = TRUE) 
      #  
      # return(data.frame(tmp, row.names = c("low", "est", "high"))) 
    } 
    
    # Special cases of movement processes: 
    tmp_name <- name 
    tmp <- sum.obj$CI[grep(name, nms.obj), ] 
    unit <- extract_units(nms.obj[grep(name, nms.obj)]) 
    
    if (length(obj[[x]]$tau) == 2 && 
        all(obj[[x]]$tau[1] == obj[[x]]$tau[2])) { 
      
      # (OUΩ and OUf): 
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


#' Extract sampling parameters. 
#' 
#' @description Extracting sampling parameters from ctmm summaries. 
#' @return The return value, if any, from executing the utility. 
#' @keywords internal 
#' 
#' @importFrom ctmm %#% 
#' @noRd 
extract_sampling <- function(obj, name, units = FALSE) { 
  
  out <- unit <- NULL 
  if (missing(obj)) stop("`obj` argument not provided.") 
  if (class(obj)[1] != "list" && class(obj[[1]])[1] != "ctmm") 
    obj <- list(obj) 
  
  out <- list() 
  if (inherits(obj[[1]], "telemetry")) { 
    i <- 1 
    for (i in seq_along(obj)) { 
      sum.obj <- summary(obj[[i]]) 
      nms.obj <- suppressWarnings(names(sum.obj)) 
      
      unit <- extract_units(nms.obj[grep(name, nms.obj)]) 
      tmp <- suppressWarnings(as.numeric(sum.obj[grep(name, nms.obj)])) 
      
      if (units) { 
        tmp <- tmp %#% unit 
        unit <- "seconds" 
      } else { 
        unit <- extract_units(nms.obj[grep(name, nms.obj)]) 
      } 
      out[[i]] <- data.frame(value = tmp, unit = unit) 
    } 
    
  } else stop("as.telemetry() obj required.") 
  
  return(out) 
} 


#' @title Extract DOF values 
#' @description Extracting DOF values and units from ctmm summaries. 
#' @keywords internal 
#' @noRd 
extract_dof <- function( 
    obj, 
    name = c("mean", "speed", "area", "diffusion")) { 
  
  name <- match.arg(name) 
  
  out <- NULL 
  if (missing(obj)) stop("`obj` argument not provided.") 
  if (class(obj)[1] != "list" && class(obj[[1]])[1] != "ctmm") { 
    if (!is.null(data)) data <- list(data) 
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
    out_tmp <- sum.obj$DOF[grep(name, nms.obj)][[1]] 
    if (is.na(out_tmp)) out_tmp <- NULL 
    return(out_tmp) 
  }) 
  
  return(out) 
}


#' Extract semi-variance data for ggplot2 
#' 
#' @description Extract semi-variance data 
#' @keywords internal 
#' 
#' @importFrom ctmm %#% 
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
          svf.lower <- Vectorize(function(dof) CI.lower(dof, level) )(dof) 
          svf.upper <- Vectorize(function(dof) CI.upper(dof, level) )(dof) 
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

#' Extract parameters. 
#' 
#' @description Extracting values and units from ctmm summaries. 
#' @keywords internal 
#' 
#' @importFrom ctmm %#% 
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
    tmpunit <- extract_units(tmpname[grep(paste0("^", name), tmpname)]) 
    
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
      return(ifelse(nm %in% groups[["A"]], "A", "B")) }) 
  
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
#' @importFrom ctmm %#% 
#' @importFrom dplyr %>% 
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
    if (trace) cat(paste0(i, ", Error: ", round(err, 2), "%\n")) 
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
    if (trace) cat(paste0(i, ", Error: ", round(err, 2), "%\n")) 
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
    elapsed <- Sys.time() - start_time 
    cat(format(elapsed), "\n") 
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
  
  newdata$id <- seq_len(nrow(newdata)) 
  newdata <- dplyr::left_join( 
    newdata, 
    data %>% dplyr::select(.data$dti, .data$dti_scale, .data$dti_yn), 
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
#' @description Estimate computation time of ctmm functions. 
#' @keywords internal 
#' 
#' @importFrom ctmm %#% 
#' @importFrom dplyr %>% 
#' @noRd 
#'  
guess_time <- function(type = "fit", 
                       data = NULL, 
                       fit = NULL, 
                       dti = NULL, 
                       dur = NULL, 
                       error = NULL, 
                       seed = NULL, 
                       trace = FALSE, 
                       parallel = TRUE) { 
  
  error <- ifelse(!is.null(error), TRUE, FALSE) 
  
  set_id <- 1 
  
  if (!type %in% c("fit", "speed")) 
    stop("type =", type, " is not supported.", call. = FALSE) 
  
  expt_unit <- "minute" 
  expt <- expt_max <- expt_min <- expt_rng <- 0 
  outputs <- data.frame(expt, expt_min, expt_max, expt_unit, expt_rng) 
  names(outputs) <- c("mean", "min", "max", "unit", "range") 
  
  if (type == "fit") { 
    if (missing(data)) stop("`data` not provided.") 
    
    data <- data[[set_id]]  
    
    n <- 2500 
    if (nrow(data) < n) { 
      outputs$mean <- ifelse(nrow(data) < 1000, 1, 2) 
      if (error) outputs$mean <- outputs$mean * 6 
      outputs$range <- paste( 
        "\u2264", outputs$mean, 
        ifelse(nrow(data) < 1000, expt_unit, "minutes")) 
      outputs$max <- 5 
      return(outputs) 
    } 
    
    start <- Sys.time() 
    guess <- ctmm::ctmm.guess(data[1:200, ], interactive = FALSE) 
    fit <- par.ctmm.select(list(data[1:200, ]),  
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
    
    if (!is.null(seed)) set.seed(seed[[set_id]]) 
    if (missing(fit)) stop("ctmm `fit` object not provided.") 
    
    fit <- fit[[set_id]] 
    if (is.null(summary(fit)$DOF["speed"])) return(outputs) 
    
    tauv <- extract_pars(fit, name = "velocity")[[1]] 
    if (is.null(tauv)) return(outputs) 
    if (tauv$value[2] == 0) return(outputs) 
    tauv <- tauv$value[2] %#% tauv$unit[2] 
    
    dti <- dti$value %#% dti$unit 
    dur <- "days" %#% dur$value %#% dur$unit 
    N <- summary(fit)$DOF[["speed"]] 
    
    x1 <- log(N) 
    x2 <- tauv/dti 
    x3 <- dur 
    
    if (tauv/dti < 1) { 
      y_min <- max(exp(1.3915 + 0.1195 * x1), 1) 
      y <- exp(3.4924 - 0.1978 * x1)  
      
      if (N < 15) { 
        y_max <- exp(4.15038 - 0.3159 * x1 + 0.01912 * x3) 
        if (N <= 5) y_max <- y_max * 2 
      } else { 
        y_max <- y 
      } 
      
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
      
      y <- expt_unit %#% y 
      expt_min <- ceiling(y * 2) / 2 
      expt <- round_any(y, 1, f = ceiling) 
      expt_max <- round_any(y_max, 1, f = ceiling) 
    } 
    
    set.seed(NULL) 
  } # end of if (type == "speed") 
  
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


#' Measure straight-line distance 
#' 
#' @description Measure distance 
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


#' Estimate distance from trajectory 
#' 
#' @description Estimate distance from ctmm::speed(). 
#' @keywords internal 
#' 
#' @noRd 
#' 
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
  
  if (class(seed)[1] != "list" && 
      class(seed[[1]])[1] != "ctmm") 
    stop("seed argument needs to be a named list.") 
  
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


#' Convert to a different unit 
#' 
#' @description Convert to a different unit. 
#' @keywords internal 
#' 
#' @importFrom ctmm %#% 
#' @noRd 
#' 
convert_to <- function(x, unit,  
                       new_unit = NULL, 
                       to_text = FALSE) { 
  if (is.data.frame(x) && 
      "value" %in% names(x) && "unit" %in% names(x)) { 
    unit <- x$unit 
    x <- x$value 
  } else if (missing(unit)) { 
    stop("'unit' must be specified when passing a single value.") 
  } 
  
  out <- fix_unit((new_unit %#% (x %#% unit)), new_unit) 
  if (to_text) out <- paste0(out[1], " ", out[2]) 
  
  return(out) 
} 
