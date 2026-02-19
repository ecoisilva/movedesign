
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
            cores = cores,
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
