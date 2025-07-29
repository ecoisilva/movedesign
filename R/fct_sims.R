
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
#' simulated trajectory, optionally running in parallel for efficiency. It
#' supports both home range and speed estimation workflows.
#'
#' @param obj A list of simulated movement datasets, each formatted as a 
#'   `telemetry` object compatible with `ctmm`.
#' @param set_target A character vector specifying the research goals.
#'   Options include:
#'   \itemize{
#'     \item `"hr"` — Home range estimation.
#'     \item `"ctsd"` — Speed and distance estimation.
#'   }
#' @param ... Optional control parameters passed via `...`. These include
#'   `.dur`, `.dti`, `.tau_p`, `.tau_v`, `.error_m`, `.check_sampling`, 
#'   `.rerun`, `.parallel`, and `.trace`. See **Details** for their
#'   descriptions.
#' 
#' @return
#' A list of fitted movement models (class `ctmm`), one per simulation. 
#' Each model is recentered to the origin (`x = 0`, `y = 0`).
#' 
#' @details
#' The function generates initial parameter estimates for each dataset
#' using `ctmm::ctmm.guess()`. If the data includes simulated location
#' error, it uses an error model accordingly. When `.check_sampling` is
#' `TRUE`, it compares the sampling duration and interval against optimal
#' thresholds derived from the provided autocorrelation timescales.
#' Models are fitted using `ctmm::ctmm.select()`, which performs model
#' selection to find the best-fit movement process. If `.rerun` is
#' enabled, the function identifies simulations with effective 
#' sample sizes below 0.1 and attempts to reselect models for those.
#' Finally, all fitted models are recentered to (`0, 0`) for downstream
#' consistency.
#' 
#' The following arguments can be supplied via `...`:
#'
#' - `.dur`: A list with elements `value` (numeric) and `unit` (string),
#'   specifying the maximum study duration. Example:
#'   `list(value = 2, unit = "months")`.
#'
#' - `.dti`: A list with elements `value` (numeric) and `unit` (string),
#'   specifying the intended sampling interval. Example:
#'   `list(value = 1, unit = "day")`.
#'
#' - `.tau_p`: A list of position autocorrelation timescales. Optional,
#'   but required if `.check_sampling = TRUE`.
#'
#' - `.tau_v`: A list of velocity autocorrelation timescales. Optional,
#'   but required if `.check_sampling = TRUE`.
#'
#' - `.error_m`: A numeric value specifying location error in meters
#'   (used for simulation).
#'
#' - `.check_sampling`: Logical; if `TRUE`, checks whether the sampling
#'   schedule meets minimum requirements for reliable model fitting via
#'   `ctmm::ctmm.fit()`. This feature is experimental and may change
#'    in future versions.
#'
#' - `.rerun`: Logical; if `TRUE`, re-runs model selection when
#'   simulations result in very low effective sample sizes, to
#'   avoid convergence issues.
#'
#' - `.parallel`: Logical; if `TRUE`, enables parallel computation.
#'
#' - `.trace`: Logical; if `TRUE`, print progress and timing
#'   messages to the console.
#'
#' @note
#' This function is intended for internal use and may assume inputs
#' follow specific structure and constraints not referenced explicitly.
#' 
#' @seealso
#' `ctmm::ctmm.guess()`, `ctmm::ctmm.select()`
#' 
#' @keywords internal
#' @importFrom ctmm ctmm.guess
fitting_model <- function(obj,
                          set_target = c("hr", "ctsd"),
                          ...) {
  
  dots <- list(...)
  
  .dur <- dots[[".dur"]] %||% NULL
  .dti <- dots[[".dti"]] %||% NULL
  .tau_p <- dots[[".tau_p"]] %||% NULL
  .tau_v <- dots[[".tau_v"]] %||% NULL
  .error_m <- dots[[".error_m"]] %||% NULL
  
  .check_sampling <- dots[[".check_sampling"]] %||% FALSE
  .rerun <- dots[[".rerun"]] %||% FALSE
  .parallel <- dots[[".parallel"]] %||% TRUE
  .trace <- dots[[".trace"]] %||% FALSE
  
  n_obj <- length(obj)
  error <- any(grepl("error", names(obj[[1]])))
  
  if (.check_sampling) {
    stopifnot(!is.null(.dur), !is.null(.dti),
              !is.null(.tau_p), !is.null(.tau_v))
    
    dur <- .dur$value %#% .dur$unit
    optimal_dur <- (.tau_p[[1]]$value[2] %#% .tau_p[[1]]$unit[2]) * 30
    
    dti <- .dti$value %#% .dti$unit
    optimal_dti <- (.tau_v[[1]]$value[2] %#% .tau_v[[1]]$unit[2]) / 3
    
    is_fit <- logical()
    N <- character()
    if ("hr" %in% set_target) {
      is_fit <- c(is_fit, optimal_dur <= dur)
      N <- c(N, "area")
    }
    if ("ctsd" %in% set_target) {
      is_fit <- c(is_fit, dti <= optimal_dti)
      N <- c(N, "speed")
    }
  } else {
    stopifnot(is.null(.dur), is.null(.dti))
    is_fit <- rep(TRUE, length(set_target))
  }
  
  if (error && is.null(.error_m)) 
    stop("No location error provided!")
  
  guessList <- lapply(obj, function(x) {
    if (error) {
      ctmm::ctmm.guess(x, CTMM = ctmm::ctmm(error = TRUE),
                       interactive = FALSE)
    } else {
      ctmm::ctmm.guess(x, interactive = FALSE)
    }
  })
  
  # out <- tryCatch(
  #   if (all(to_fit))
  #     par.ctmm.fit(obj, guessList, parallel = .parallel)
  #   else par.ctmm.select(obj, guessList, parallel = .parallel)
  #   , error = function(e) e)
  
  out <- tryCatch(
    par.ctmm.select(obj, guessList, parallel = .parallel),
    error = function(e) e)
  
  if (inherits(out, "error")) return(NULL)
  if (n_obj == 1) out <- list(out)
  
  if (.rerun) {
    N <- setNames(lapply(N, function(x) 
      extract_dof(out, x)), set_target)
    
    lapply(set_target, function(target) {
      to_rerun <- which(N[[target]] < 0.1)
      
      if (any(N[[target]] < 0.1)) {
        for (z in seq_along(to_rerun)) {
          out[[z]] <- par.ctmm.select(
            obj[to_rerun[[z]]], 
            guessList[to_rerun[[z]]],
            parallel = .parallel)
        }
      }
      
    }) # end of lapply(set_target)
    
  } # end of if (.rerun)
  
  # Recenter to 0,0:
  
  lapply(seq_along(out), function (x) {
    out[[x]]$mu[[1, "x"]] <- 0
    out[[x]]$mu[[1, "y"]] <- 0 
  })
  
  return(out)
}
