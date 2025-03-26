
#' @title Trigger tag failure
#' @description
#' Simulates the failure of tagging devices within a dataset by introducing
#' a given probability of failure. If a failure occurs, data collection is
#' halted at that point, meaning the absolute sample size of each
#' simulation may vary. The function returns a list of modified datasets
#' and a logical vector indicating which devices failed.
#' @noRd
.trigger_tag_failure <- function(obj, fail_prob = 0) {
  
  if (!inherits(obj, "movedesign")) {
    stop("The 'obj' argument must be of class 'movedesign'.")
  }
  
  devices_failed <- logical(length(obj)) 
  failure_occurred <- FALSE
  
  obj <- lapply(obj, function(x) {
    
    failure_occurred <- sample(
      c(FALSE, TRUE), size = 1, 
      prob = c(1 - fail_prob, fail_prob))
    
    to_keep_vec <- rep(1, nrow(x))
    
    if (failure_occurred) {
      # If failure occurred, simulate data truncation:
      to_keep_vec <- c(rep(1, 10), cumprod(
        1 - stats::rbinom(nrow(x) - 10, 1, prob = 0.005)))
      if (!any(to_keep_vec == 0)) failure_occurred <- FALSE
      
      x <- x[to_keep_vec == 1, ]
    }
    
    devices_failed[x] <- failure_occurred
    return(x)
    
  }) # end of lapply
  
  return(list(obj, devices_failed))
  
} # end of function, .trigger_tag_failure()

#' @title Trigger fix success rate
#' @description
#' Simulates fix success rate based on a given  probability. For each
#' dataset, a number of rows (randomly) corresponding to the success rate
#' are retained, while the remaining locations are discarded.
#' 
#' @noRd
.trigger_fix_success <- function(obj, prob = 0) {
  
  if (!inherits(obj, "movedesign")) {
    stop("The 'obj' argument must be of class 'movedesign'.")
  }
  
  obj <- lapply(obj, function(x) {
    to_keep <- round(nrow(x) * (1 - prob), 0)
    to_keep_vec <- sort(sample(1:nrow(x), to_keep, replace = FALSE))
    return(x[to_keep_vec, ])
    
  }) # end of lapply
  
  return(obj)
  
} # end of function, .trigger_data_loss()

#' @title Trigger location error
#' @description
#' Simulates location error in tracking data (x, y coordinates) by adding
#' random noise drawn from a normal distribution. The noise is scaled by
#' the specified error parameter.
#' 
#' @noRd
.trigger_error <- function(obj, error = 0) {
  
  if (!inherits(obj, "movedesign")) {
    stop("The 'obj' argument must be of class 'movedesign'.")
  }
  
  obj <- lapply(obj, function(x) {
    error_x <- stats::rnorm(nrow(x), mean = 0, sd = error)
    error_y <- stats::rnorm(nrow(x), mean = 0, sd = error)
    x[c("x", "y")] <- x[c("x", "y")] + c(error_x, error_y)
    return(x) })
  
  return(obj)
  
} # end of function, .trigger_error()
