
#' @title Estimate home range from simulated movement data
#'
#' @description
#' Estimates home range areas for each simulated movement dataset using
#' the Autocorrelated Kernel Density Estimator (AKDE) via
#' [`ctmm::akde()`]. This function is intended for use within simulation
#' workflows where home range calculations are needed for each simulated
#' individual.
#'
#' @param rv A `reactiveValues` list containing, at a minimum:
#'   \itemize{
#'     \item `simList`: A list of simulated movement datasets
#'       (e.g., telemetry tracks).
#'     \item `simfitList`: A list of fitted movement models, each
#'       corresponding to an entry in `simList`.
#'   }
#'   Each movement dataset in `simList` should be compatible with
#'   [`ctmm::akde()`], and each fitted model in `simfitList` should
#'   correspond to its respective simulated dataset.
#'
#' @return
#' A named list of `ctmm` objects, each representing an AKDE home range
#' estimate for the corresponding simulation. If AKDE estimation fails
#' for a simulation (e.g., due to poor model fit or data issues), the
#' result for that simulation will be `NULL`.
#'
#' @seealso
#' [`ctmm::akde()`] for details on home range estimation.
#'
#' @note
#' This function is intended for internal use and may assume inputs
#' follow specific structure and constraints not referenced explicitly.
#' 
#' @importFrom ctmm akde
#' 
#' @keywords internal
estimate_hr <- function(rv) {
  
  hrList <- lapply(seq_along(rv$simList), function(x) {
    tryCatch(
      suppressMessages(ctmm::akde(rv$simList[[x]],
                                  rv$simfitList[[x]])),
      warning = function(w) {
        w; return(NULL) 
      },
      error = function(e) {
        warning(paste("ctmm::akde() failed for simulation no.", x))
        NULL
      }
    )
  }) # end of lapply
  
  names(hrList) <- names(rv$simList)
  return(hrList)
  
} # end of function, estimate_hr()


#' @title Estimate movement speed for simulated movement data
#'
#' @description
#' Calculates continuous-time speed and distance (CTSD) for each simulated
#' movement dataset using its corresponding fitted movement model with
#' [`ctmm::speed()`]. This function is designed for simulation workflows
#' where speed metrics are required for each simulated individual.
#'
#' @param rv A `reactiveValues` list containing, at a minimum:
#'   \itemize{
#'     \item `simList`: A list of simulated movement datasets
#'       (e.g., telemetry tracks).
#'     \item `simfitList`: A list of fitted movement models, each
#'       corresponding to an entry in `simList`.
#'   }
#'   Each element in `simList` should be compatible with
#'   [`ctmm::speed()`], and each model in `simfitList` should
#'   correspond to its respective simulated dataset.
#'
#' @return
#' A named list of speed estimates (`ctmm` objects), with one entry per
#' simulation.For any simulation where speed estimation fails (e.g., 
#' due to model fitting issues or incompatible data), `NULL` is returned
#' for that entry and omitted from the final output.
#'
#' @seealso
#' [`ctmm::speed()`] for details on speed estimation.
#'
#' @note
#' This function is intended for internal use and may assume inputs
#' follow specific structure and constraints not referenced explicitly.
#' 
#' @importFrom ctmm speed
#' 
#' @keywords internal
estimate_speed <- function(rv) {
  
  ctsdList <- lapply(seq_along(rv$simList), function(x) {
    tryCatch(
      ctmm::speed(rv$simList[[x]],
                  rv$simfitList[[x]],
                  units = FALSE,
                  trace = 0),
      warning = function(w) {
        msg <- "Movement model is fractal"
        if (grepl(msg, w)) warning(msg)
        return(NULL)
      },
      error = function(e) {
        warning(paste("ctmm::speed() failed for simulation no.", x))
        return(NULL)
      }
    )
  }) # end of lapply
  
  names(ctsdList) <- names(rv$simList)
  ctsdList[sapply(ctsdList, is.null)] <- NULL
  return(ctsdList)
  
} # end of function, estimate_speed()
