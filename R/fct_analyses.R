#' Estimate home range
#'
#' @description Estimates home range areas with the Autocorrelated Kernel Density Estimator (AKDE)
#' for each simulated movement dataset using the fitted movement models.
#'
#' @param rv A reactive values list containing:
#'   \itemize{
#'     \item \code{simList} - A list of simulated movement datasets.
#'     \item \code{simfitList} - A list of fitted movement models corresponding to \code{simList}.
#'   }
#'
#' @return A named list of \code{ctmm} objects, one per simulation.
#'
#' @details
#' The function applies \code{ctmm::akde()} to estimate home range areas while handling 
#' potential warnings and errors gracefully. Any failed computations return \code{NULL}.
#'
#' @importFrom ctmm akde
#' @export
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

#' Estimate movement speed
#'
#' @description Estimates movement speed using continuous-time speed and distance (CTSD)
#'  for each simulated movement dataset using the corresponding fitted movement model.
#'
#' @param rv A reactive values list containing:
#'   \itemize{
#'     \item \code{simList} - A list of simulated movement datasets.
#'     \item \code{simfitList} - A list of fitted movement models corresponding to \code{simList}.
#'   }
#'
#' @return A named list of \code{ctmm} objects, one per simulation.
#'
#' @details
#' The function applies \code{ctmm::speed()} to estimate movement speed while handling 
#' potential warnings and errors gracefully.
#'
#' @importFrom ctmm speed
#' @export
estimate_speed <- function(rv) {
  
  ctsdList <- lapply(seq_along(rv$simList), function(x) {
    tryCatch(
      ctmm::speed(rv$simList[[x]],
                  rv$simfitList[[x]],
                  units = FALSE,
                  trace = 0),
      warning = function(w) {
        w; return(NULL) 
      },
      error = function(e) {
        warning(paste("ctmm::speed() failed for simulation no.", x))
        NULL
      }
    )
  }) # end of lapply
  
  names(ctsdList) <- names(rv$simList)
  ctsdList[sapply(ctsdList, is.null)] <- NULL
  return(ctsdList)
  
} # end of function, estimate_speed()
