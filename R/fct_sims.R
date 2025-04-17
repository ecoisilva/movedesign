#' Simulate Movement Data from Fitted Models
#'
#' @description This function generates simulated location data using movement
#' models. The function supports both single and grouped simulations based on 
#' whether the data is simulated or derived from an emulated or fitted model.
#'
#' @param rv A reactive values list containing:
#'   \itemize{
#'     \item \code{dur} - A list specifying the sampling duration (\code{value} and \code{unit}).
#'     \item \code{dti} - A list specifying the time interval between locations (\code{value} and \code{unit}).
#'     \item \code{data_type} - A character string indicating data type.
#'     \item \code{is_emulate} - Logical; if \code{TRUE}, the function generates an emulated model.
#'     \item \code{modList} - A list of fitted movement models for simulation.
#'     \item \code{meanfitList} - A list of a mean model for emulation.
#'     \item \code{grouped} - Logical; if \code{TRUE}, the simulation considers grouped movement models.
#'     \item \code{which_meta} - A character vector indicating whether to compare models.
#'     \item \code{tau_p}, \code{tau_v}, \code{sigma}, \code{mu} - Lists of movement model parameters.
#'     \item \code{seed0} - An integer used for random seed initialization.
#'   }
#'
#' @return A list containing one or two simulated movement datasets (depending on grouping):
#'   \itemize{
#'     \item If \code{grouped = FALSE}, returns a list with a single simulated dataset.
#'     \item If \code{grouped = TRUE}, returns a list with two simulated datasets (for groups \code{A} and \code{B}).
#'   }
#'
#' @details
#' The function first constructs a time sequence based on the provided duration and interval.
#' If the data is fully simulated from scratch (not conditioned on existing data), it retrieves
#' movement model(s) from \code{rv$modList}. 
#' Otherwise, it either emulates a model using \code{rv$meanfitList} and a random seed 
#' or constructs a model from movement parameters.
#'
#' If estimate comparisons are enabled (via \code{which_meta}), two models are prepared. 
#' The function then runs \code{ctmm::simulate()} to generate simulated movement data.
#' The resulting trajectories are pseudonymized before returning.
#'
#' @importFrom ctmm simulate
#' @export
simulating_data <- function(rv) {

  dur <- rv$dur$value %#% rv$dur$unit
  dti <- rv$dti$value %#% rv$dti$unit
  t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]

  if (rv$data_type == "simulated") {
    fit <- fitA <- rv$modList[[1]]
    if (rv$grouped) fitB <- rv$modList[[2]]
  }

  if (rv$data_type != "simulated") {

    if (rv$is_emulate) {
      
      fit <- emulate_seeded(rv$meanfitList[[1]], rv$seed0)
      if (length(fit$isotropic) > 1)
        fit$isotropic <- fit$isotropic[["sigma"]]

      # Recenter to 0,0:
      fit$mu[["x"]] <- 0
      fit$mu[["y"]] <- 0

    } else {
      fit <- prepare_mod(
        tau_p = rv$tau_p[[1]][2, ],
        tau_v = rv$tau_v[[1]][2, ],
        sigma = rv$sigma[[1]][2, ],
        mu = rv$mu[[1]])
    }

    if ("compare" %in% rv$which_meta) {
      
      if (rv$is_emulate) {
        fitA <- emulate_seeded(rv$meanfitList[["A"]], rv$seed0)
        fitB <- emulate_seeded(rv$meanfitList[["B"]], rv$seed0 + 1)
        if (length(fitA$isotropic) > 1)
          fitA$isotropic <- fitA$isotropic[["sigma"]]
        if (length(fitB$isotropic) > 1)
          fitB$isotropic <- fitB$isotropic[["sigma"]]

        # Recenter to 0,0:
        fitA$mu[["x"]] <- 0
        fitA$mu[["y"]] <- 0
        fitB$mu[["x"]] <- 0
        fitB$mu[["y"]] <- 0

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
    # rv$modList <- list(fit)
  }

  if (rv$grouped) {
    
    # rv$modList_groups <- list(A = fitA, B = fitB)
    simA <- ctmm::simulate(fitA, t = t_new, seed = rv$seed0)
    simB <- ctmm::simulate(fitB, t = t_new, seed = rv$seed0 + 1)
    
    if (is.null(simA) || is.null(simB)) {
      bug_group <- c()
      if (is.null(simA)) bug_group <- c(bug_group, "A")
      if (is.null(simB)) bug_group <- c(bug_group, "B")
      
      msg_log(
        style = "danger",
        message = paste0(
          "Simulation ", msg_danger("failed"),
          " for group(s): ", msg_danger(bug_group)),
        detail = "Try again with different groupings.")
      shinybusy::remove_modal_spinner()
    }
    
    stopifnot(!is.null(simA), !is.null(simB))
    
    simA <- pseudonymize(simA)
    simB <- pseudonymize(simB)
    sim <- list(simA, simB)
    return(sim)

  } else {
    sim <- ctmm::simulate(fit, t = t_new, seed = rv$seed0)
    sim <- pseudonymize(sim)
    return(list(sim))
  }

} # end of function, simulating_data()

#' Fit continuous-time movement models
#'
#' @description This function fits continuous-time movement models to simulated location
#' data using the \code{ctmm} package. It estimates movement parameters for each
#' simulated trajectory, optionally running in parallel for efficiency.
#'
#' @param obj A list of simulated movement datasets.
#' @param set_target A character vector indicating the research target(s). Options:
#'   \itemize{
#'     \item \code{"hr"} - Home range estimation.
#'     \item \code{"ctsd"} - Speed and distance estimation.
#'   }
#' @param .dur Numeric, sampling duration of the simulated data (required if \code{.check_sampling = TRUE}).
#' @param .dti Numeric, sampling interval of simulated data (required if \code{.check_sampling = TRUE}).
#' @param .tau_p List, position autocorrelation timescale (optional).
#' @param .tau_v List, velocity autocorrelation timescale (optional).
#' @param .error_m Numeric, if simulating a dataset with location error (in meters).
#' @param .check_sampling Logical; if \code{TRUE}, checks if the sampling schedule is optimal for ctmm.fit().
#' @param .rerun Logical; if \code{TRUE}, re-runs model selection if effective sample sizes fall below threshold.
#' @param .parallel Logical; if \code{TRUE}, enables parallel computation for efficiency. Default is \code{TRUE}.
#' @param .trace Logical; if \code{TRUE}, prints additional information.
#'
#' @return A list of fitted movement models, one per simulation.
#'
#' @details
#' The function first generates initial parameter estimates using \code{ctmm::ctmm.guess()}.
#' It then selects the best movement model for each simulation using \code{par.ctmm.select()}.
#' The function ensures that each fitted model is centered at the origin (\code{x = 0, y = 0}) before returning.
#'
#' @importFrom ctmm ctmm.guess
#' @export
#' 
fitting_model <- function(obj,
                          set_target = c("hr", "ctsd"),
                          .dur = NULL,
                          .dti = NULL,
                          .tau_p = NULL,
                          .tau_v = NULL,
                          .error_m = NULL,
                          .check_sampling = FALSE,
                          .rerun = FALSE,
                          .parallel = TRUE,
                          .trace = FALSE) {
  
  .error <- any(grepl("error", names(obj[[1]])))
  
  to_fit <- FALSE
  if (.check_sampling) {
    stopifnot(!is.null(.dur), !is.null(.dti),
              !is.null(.tau_p), !is.null(.tau_v))
    
    dur <- .dur$value %#% .dur$unit
    optimal_dur <- (.tau_p[[1]]$value[2] %#% .tau_p[[1]]$unit[2]) * 10
    
    dti <- .dti$value %#% .dti$unit
    optimal_dti <- (.tau_v[[1]]$value[2] %#% .tau_v[[1]]$unit[2]) / 3
    
    to_fit <- N <- c()
    if ("hr" %in% set_target) {
      to_fit <- c(to_fit, optimal_dur <= dur)
      N <- c(N, "area")
    }
    if ("ctsd" %in% set_target) {
      to_fit <- c(to_fit, dti <= optimal_dti)
      N <- c(N, "speed")
    }
    
  } else {
    stopifnot(is.null(.dur), is.null(.dti))
  }
  
  n_obj <- length(obj)
  
  if (!.error) {
    guessList <- lapply(seq_along(obj), function (x)
      ctmm::ctmm.guess(obj[[x]], interactive = FALSE))
    
  } else {
    if (is.null(.error_m)) stop("No location error provided!")
    
    guessList <- lapply(seq_along(obj), function(x)
      ctmm::ctmm.guess(obj[[x]],
                       CTMM = ctmm::ctmm(error = TRUE),
                       interactive = FALSE))
  }
  
  out <- tryCatch(
    if (all(to_fit)) par.ctmm.fit(obj, guessList, parallel = .parallel)
    else par.ctmm.select(obj, guessList, parallel = .parallel)
    , error = function(e) e)
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
