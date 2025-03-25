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
