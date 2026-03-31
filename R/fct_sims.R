
#' Simulate movement data from continuous-time movement models
#'
#' @description
#' Generates simulated animal movement tracks based on continuous-time
#' movement models using [`ctmm::simulate()`]. Supports both single-group
#' and grouped simulations, as determined by study design and data
#' parameters. Used within the `movedesign` application workflows to
#' create synthetic data for simulation studies and to evaluate study
#' design.
#'
#' @param rv A `reactiveValues` object with all simulation inputs:
#'   \itemize{
#'     \item dur A list with elements `value` and `unit` (e.g.,
#'        `list(value = 2, unit = "months")`), for the study's maximum
#'        duration. `unit` must be either `"second"`, `"minute"`,
#'        `"hour"`, `"day"`, `"month"`, or `"year"`.
#'     \item dti A list with elements `value` and `unit` (e.g.,
#'        `list(value = 1, unit = "day")`), specifying the intended
#'        sampling interval between relocations. `unit` must be either
#'        `"second"`, `"minute"`, `"hour"`, `"day"`, `"month"`, 
#'        or `"year"`.
#'     \item `data_type`: Character, data source that informs the 
#'       simulations.
#'     \item `add_ind_var`: Logical; if `TRUE`, draws parameters from
#'       population distribution for each new individual.
#'     \item `modList`: List of fitted models.
#'     \item `meanfitList`: List of mean models for individual variation.
#'     \item `grouped`: Logical; if `TRUE`, simulates from two groups.
#'     \item `which_meta`: Character vector; analytical target.
#'     \item `tau_p`, `tau_v`, `sigma`, `mu`: Lists of
#'       movement parameters.
#'   }
#' @param seed Integer for random number generator,
#'  ensuring reproducibility.
#'
#' @return
#' A list of simulated movement datasets:
#'   \itemize{
#'     \item If `grouped = FALSE`, a list with a single simulated track.
#'     \item If `grouped = TRUE`, 
#'        a list with two tracks (from groups A and B).
#'   }
#'
#' @details
#' This function simulates animal movement tracks based on the selected
#' mode and design settings. It first constructs a time sequence using
#' the specified duration and interval. Depending on the simulation mode
#' (`data_type`), it either retrieves movement models from `modList`
#' (for simulated data) or uses `meanfitList` or raw movement parameters
#' to build models (for uploaded or selected data). If a group
#' comparison is requested, models are prepared for both groups.
#' Tracks are then simulated using `ctmm::simulate()` and subsequently
#' pseudonymized.
#' 
#' @note
#' This function is intended for internal use and may assume inputs
#' follow specific structure and constraints not referenced explicitly.
#' 
#' @keywords internal
#' @importFrom ctmm simulate %#%
simulating_data <- function(rv, seed) {
  
  # Helper for recentering mean location:
  .recenter_mu <- function(fit) {
    fit$mu[["x"]] <- 0
    fit$mu[["y"]] <- 0
    return(fit)
  }
  
  dur <- rv$dur$value %#% rv$dur$unit
  dti <- rv$dti$value %#% rv$dti$unit
  t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]

  if (rv$data_type == "simulated") {
    
    fit <- fitA <- rv$modList[[1]]
    if (rv$grouped) fitB <- rv$modList[[2]]
    
  } else {
    
    if (rv$add_ind_var) {
      fit <- simulate_seeded(rv$meanfitList[[1]], seed)
      fit <- .recenter_mu(fit)
      
    } else {
      fit <- prepare_mod(
        tau_p = rv$tau_p[[1]][2, ],
        tau_v = rv$tau_v[[1]][2, ],
        sigma = rv$sigma[[1]][2, ],
        mu = rv$mu[[1]])
    }

    if ("compare" %in% rv$which_meta) {
      
      if (rv$add_ind_var) {
        
        fitA <- simulate_seeded(rv$meanfitList[["A"]], seed)
        fitB <- simulate_seeded(rv$meanfitList[["B"]], seed + 1)
        
        fitA <- .recenter_mu(fitA)
        fitB <- .recenter_mu(fitB)
        
      } else {
        fitA <- prepare_mod(
          tau_p = rv$tau_p[[2]][2, ],
          tau_v = rv$tau_v[[2]][2, ],
          sigma = rv$sigma[[2]][2, ],
          mu = rv$mu[[2]])

        fitB <- prepare_mod(
          tau_p = rv$tau_p[[3]][2, ],
          tau_v = rv$tau_v[[3]][2, ],
          sigma = rv$sigma[[3]][2, ],
          mu = rv$mu[[3]])
      }
    }
  }
  
  if (rv$grouped) {
    
    simA <- ctmm::simulate(fitA, t = t_new, seed = seed)
    simB <- ctmm::simulate(fitB, t = t_new, seed = seed + 1)
    
    failed_groups <- c()
    if (is.null(simA)) failed_groups <- c(failed_groups, "A")
    if (is.null(simB)) failed_groups <- c(failed_groups, "B")
    
    if (length(failed_groups)) {
      msg_log(
        style = "danger",
        message = paste0(
          "Simulation ", msg_danger("failed"),
          " for group(s): ", msg_danger(failed_groups)),
        detail = "Try again with different groupings.")
      shinybusy::remove_modal_spinner()
    }
    
    stopifnot(!is.null(simA), !is.null(simB))
    sim <- list(pseudonymize(simA), pseudonymize(simB))
    return(sim)

  } else {
    sim <- ctmm::simulate(fit, t = t_new, seed = seed)
    sim <- list(pseudonymize(sim))
    return(sim)
  }
  
} # end of function, simulating_data()


#' Fit continuous-time movement models
#'
#' @description
#' This function fits continuous-time movement models to simulated location
#' data using the `ctmm` package. It estimates movement parameters for each
#' simulated trajectory, allowing for parallel execution. It currently
#' supports both home range and speed estimation workflows.
#'
#' @param obj A list of simulated movement datasets, each a `telemetry`
#'   object compatible with `ctmm` `R` package.
#' @param set_target A character vector specifying the research targets.
#'   Current options:
#'   \describe{
#'     \item{"hr"}{Home range estimation.}
#'     \item{"ctsd"}{Speed & distance estimation.}
#'   }
#' @param parallel Logical. If `TRUE`, enables parallel processing.
#' @param ncores Integer. Number of CPU cores to use for parallel
#'   processing. Defaults to all available cores minus one.
#' @param trace Logical. If `TRUE` (default), prints progress and
#'   timing messages to the console.
#' @param ... Additional arguments used internally.
#' 
#' @return
#' A list of fitted movement models, all recentered to the origin.
#' 
#' @details
#' The function generates initial parameter estimates for each dataset
#' using `ctmm::ctmm.guess()`. If the data includes simulated location
#' error, it adds an error model accordingly. Models are fitted using
#' `ctmm::ctmm.select()`, which performs model selection to find the
#' best-fit movement process. Finally, all fitted models are recentered
#' to (`0, 0`) for downstream consistency.
#'
#' @note
#' This function is intended for internal use and may assume inputs
#' follow specific structure and constraints not referenced explicitly.
#' 
#' @seealso
#' `ctmm::ctmm.guess()`, `ctmm::ctmm.select()`
#' 
#' @importFrom ctmm ctmm.guess ctmm.select
#' @export
fitting_models <- function(obj,
                           set_target = c("hr", "ctsd"),
                           parallel = FALSE,
                           trace = FALSE,
                           ncores = parallel::detectCores(),
                           ...) {
  
  stopifnot(is.list(obj), length(obj) >= 1)
  
  if (class(obj)[1] != "list" && class(obj[[1]])[1] != "ctmm")
    obj <- list(obj)
  
  if (!inherits(obj[[1]], "telemetry")) {
    stop(paste("'obj' must be a list of 'telemetry' objects."))
  }
  
  dots <- list(...)
  
  .dur <- dots[[".dur"]] %||% NULL
  .dti <- dots[[".dti"]] %||% NULL
  .tau_p <- dots[[".tau_p"]] %||% NULL
  .tau_v <- dots[[".tau_v"]] %||% NULL
  .rerun <- dots[[".rerun"]] %||% FALSE
  .error_m <- dots[[".error_m"]] %||% NULL
  .check_sampling <- dots[[".check_sampling"]] %||% FALSE
  
  n_obj <- length(obj)
  has_error <- any(grepl("error", names(obj[[1]])))
  
  if (.check_sampling) {
    stopifnot(!is.null(.dur), !is.null(.dti),
              !is.null(.tau_p), !is.null(.tau_v))
    
    N <- character()
    if ("hr" %in% set_target) N <- c(N, "area")
    if ("ctsd" %in% set_target) N <- c(N, "speed")
  }
  
  if (has_error && is.null(.error_m)) 
    stop("No location error provided!")
  
  guessList <- lapply(obj, function(x) {
    if (has_error) {
      ctmm::ctmm.guess(
        x, ctmm::ctmm(error = TRUE), interactive = FALSE)
    } else { ctmm::ctmm.guess(x, interactive = FALSE) }
  })
  
  out <- tryCatch(
    par.ctmm.select(obj, guessList, parallel = parallel, trace = trace),
    error = function(e) e)
  
  if (inherits(out, "error")) return(NULL)
  if (n_obj == 1) out <- list(out)
  
  if (.rerun) {
    N <- setNames(lapply(N, function(x) extract_dof(out, x)), set_target)
    
    lapply(set_target, function(target) {
      to_rerun <- which(N[[target]] < 0.1)
      
      if (any(N[[target]] < 0.1)) {
        for (z in seq_along(to_rerun)) {
          out[[z]] <- par.ctmm.select(
            obj[to_rerun[[z]]], 
            guessList[to_rerun[[z]]],
            parallel = parallel,
            cores = ncores,
            trace = trace)
        }
      }
      
    }) # end of lapply(set_target)
    
  } # end of if (.rerun)
  
  # Recenter to 0,0:
  out <- lapply(out, function(m) {
    m$mu[[1, "x"]] <- 0
    m$mu[[1, "y"]] <- 0
    return(m)
  })
  
  return(out)
}


#' @title Add m individuals (for batch simulation)
#' @noRd
.md_add_m <- function(rv,
                      m,
                      init_m,
                      has_groups = FALSE,
                      trace = FALSE) {
  
  # Simulate data:
  
  if (!is.null(init_m)) {
    
    if (!has_groups) {
      simList <- lapply(seq_len(init_m), function(x) {
        rv$seed0 <- generate_seed(rv$seedList)
        out <- simulating_data(rv, rv$seed0)[[1]]
        rv$seedList <- c(rv$seedList, rv$seed0)
        return(out) 
      })
      
      seedList <- utils::tail(rv$seedList, init_m)
      names(simList) <- seedList
      
    } else {
      
      simList <- list()
      seedList <- list()
      for (j in seq_len(init_m)) {
        
        rv$seed0 <- generate_seed(rv$seedList)
        tmpList <- simulating_data(rv, rv$seed0)
        rv$groups[[2]][["A"]] <- c(
          as.character(rv$groups[[2]]$A),
          as.character(rv$seed0))
        rv$groups[[2]][["B"]] <- c(
          as.character(rv$groups[[2]]$B),
          as.character(rv$seed0 + 1))
        
        names(tmpList) <- c(rv$seed0, rv$seed0 + 1)
        seedList <- list(seedList, rv$seed0, rv$seed0 + 1)
        rv$seedList <- c(rv$seedList, rv$seed0, rv$seed0 + 1)
        simList <- c(simList, tmpList)
      }
      
      names(simList) <- unlist(seedList)
    }
    
  } else {
    
    if (!has_groups) {
      
      simList <- lapply(seq_len(m), function(x) {
        rv$seed0 <- generate_seed(rv$seedList)
        out <- simulating_data(rv, rv$seed0)[[1]]
        rv$seedList <- c(rv$seedList, rv$seed0)
        return(out) 
      })
      seedList <- utils::tail(rv$seedList, m)
      names(simList) <- seedList
      
    } else {
      
      extra_m <- ceiling(m / 2)
      
      simList <- list()
      seedList <- list()
      for (j in seq_len(extra_m)) {
        
        rv$seed0 <- generate_seed(rv$seedList)
        tmpList <- simulating_data(rv, rv$seed0)
        rv$groups[[2]][["A"]] <- c(
          as.character(rv$groups[[2]]$A),
          as.character(rv$seed0))
        rv$groups[[2]][["B"]] <- c(
          as.character(rv$groups[[2]]$B),
          as.character(rv$seed0 + 1))
        
        names(tmpList) <- c(rv$seed0, rv$seed0 + 1)
        seedList <- list(seedList, rv$seed0, rv$seed0 + 1)
        rv$seedList <- c(rv$seedList, rv$seed0, rv$seed0 + 1)
        simList <- c(simList, tmpList)
      }
      
      names(simList) <- unlist(seedList)
      
    }
  }
  
  new_tmpnames <- names(simList)
  
  # If there is tag failure:
  
  failure_occurred <- FALSE
  if (!is.null(rv$fail_prob)) {
    if (req(rv$fail_prob) > 0) {
      
      fail_prob <- rv$fail_prob
      simList <- lapply(simList, function(x) {
        
        failure_occurred <- sample(
          c(FALSE, TRUE), size = 1, 
          prob = c(1 - fail_prob, fail_prob))
        
        to_keep_vec <- rep(1, nrow(x))
        if (failure_occurred) {
          
          to_keep_vec <- c(rep(1, 10), cumprod(
            1 - stats::rbinom(nrow(x) - 10, 1, prob = 0.01)))
          if (!any(to_keep_vec == 0)) failure_occurred <- FALSE
          
          rv$dev_failed <- c(rv$dev_failed, failure_occurred)
          return(x[to_keep_vec == 1, ])
          
        } else return(x)
        
      }) # end of lapply
      
    } # end of if (rv$fail_prob > 0)
  } else rv$dev_failed <- c(rv$dev_failed, failure_occurred)
  
  # If there is data loss:
  
  if (!is.null(rv$lost))
    if (rv$lost$perc > 0) {
      
      simList <- lapply(simList, function(x) {
        to_keep <- round(nrow(x) * (1 - rv$lost$perc), 0)
        to_keep_vec <- sort(
          sample(seq_len(nrow(x)), to_keep, replace = FALSE))
        x[to_keep_vec, ] })
      
    } # end of input$device_fixsuccess
  
  # If there are errors associated with each location:
  
  if (!is.null(rv$error))
    if (req(rv$error) > 0) {
      
      simList <- lapply(simList, function(x) {
        
        x$error_x <- x$error_y <- stats::rnorm(
          nrow(x), mean = 0, sd = rv$error)
        
        x$HDOP <- sqrt(2) * sqrt(x$error_x^2 + x$error_y^2) /
          sqrt(-2 * log(0.05))
        
        x$original_x <- x$x
        x$original_y <- x$y
        x[c("x", "y")] <- x[c("x", "y")] + c(x$error_x,
                                             x$error_y)
        ctmm::uere(x) <- 1
        
        return(x) })
      
    } # end of input$device_error
  
  tmpnames <- names(rv$simList)
  rv$simList <- c(rv$simList, simList)
  
  current_dur <- rv$dur$value %#% rv$dur$unit
  optimal_dur <- (rv$tau_p[[1]]$value[2] %#%
                    rv$tau_p[[1]]$unit[2]) * 30
  
  current_dti <- rv$dti$value %#% rv$dti$unit
  optimal_dti <- (rv$tau_v[[1]]$value[2] %#%
                    rv$tau_v[[1]]$unit[2]) / 3
  
  # optimal_dur <= current_dur && current_dti <= optimal_dti
  if (rv$set_analysis == "hr") {
    check_ok <- optimal_dur <= current_dur
  }
  if (rv$set_analysis == "ctsd") {
    check_ok <- current_dti <= optimal_dti
  }
  
  # Fit movement models:
  
  fitList <- lapply(seq_along(simList), function(x) {
    
    guess <- ctmm::ctmm.guess(simList[[x]], interactive = F)
    if (check_ok)
      out <- ctmm::ctmm.fit(simList[[x]], guess, trace = F)
    else out <- ctmm::ctmm.select(simList[[x]], guess, trace = F)
    rv$simfitList <- c(rv$simfitList, list(out))
    return(out)
    
  })
  names(rv$simfitList) <- names(rv$simList)
  req(length(rv$simList) == length(rv$simfitList))
  
  # Estimate home range area:
  
  if ("Home range" %in% rv$which_question) {
    
    akdeList <- lapply(seq_along(simList), function(x) {
      out <- tryCatch(
        ctmm::akde(simList[[x]], fitList[[x]]),
        warning = function(w) NULL,
        error = function(e) NULL)
      rv$akdeList <- c(rv$akdeList, list(out))
      return(out)
    })
    names(rv$akdeList) <- names(rv$simList)
    
  } # end of if (hr)
  
  # Estimate speed & distance traveled:
  
  if ("Speed & distance" %in% rv$which_question) {
    
    ctsdList <- par.speed(
      simList,
      fitList,
      seed = seedList,
      parallel = rv$parallel)
    rv$ctsdList <- c(rv$ctsdList, ctsdList)
    names(rv$ctsdList) <- names(rv$simList)
    
    speedDatList <- lapply(seq_along(simList), function(x) {
      ctmm::speeds(simList[[x]], fitList[[x]], units = FALSE)
    })
    rv$speedDatList <- c(rv$speedDatList, speedDatList)
    names(rv$speedDatList) <- names(rv$simList)
    
    pathList <- estimate_trajectory(
      data = simList,
      fit = fitList,
      groups = if (has_groups) rv$groups[[2]] else NULL,
      dur = rv$dur,
      tau_v = rv$tau_v,
      seed = seedList)
    rv$pathList <<- c(rv$pathList, pathList)
    names(rv$pathList) <- names(rv$simList)
    
  } # end of if (ctsd)
  
  return(invisible(NULL))
  
}

