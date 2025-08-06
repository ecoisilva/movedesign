
#' @title Generate seed
#' 
#' @noRd
generate_seed <- function(seed_list = NULL) {
  
  set.seed(NULL)
  get_random <- function(n) {
    round(stats::runif(n, min = 1, max = 9999999), 0)
  }
  
  out <- get_random(1)
  if (!is.null(seed_list))
    while ((out %in% seed_list) && ((out + 1) %in% seed_list)) 
      out <- get_random(1)
  return(out)
}


#' @title Trigger tag failure
#' @description
#' Simulates the failure of tagging devices within a dataset by introducing
#' a given probability of failure. If a failure occurs, data collection is
#' halted at that point, meaning the absolute sample size of each
#' simulation may vary. The function returns a list of modified datasets
#' and a logical vector indicating which devices failed.
#' @noRd
.trigger_tag_failure <- function(obj, fail_prob = 0) {
  
  if (is.null(fail_prob)) return(list(obj = obj,
                                      dev_failed = logical(length(obj))))
  if (fail_prob == 0) return(list(obj = obj,
                                  dev_failed = logical(length(obj))))
  
  # if (!inherits(obj, "movedesign")) {
  #   stop("The 'obj' argument must be of class 'movedesign'.")
  # }
  
  devices_failed <- logical(length(obj))
  
  obj <- lapply(obj, function(x) {
    
    failure_occurred <- sample(
      c(FALSE, TRUE), size = 1, 
      prob = c(1 - fail_prob, fail_prob))
    
    to_keep_vec <- rep(1, nrow(x))
    
    if (failure_occurred) {
      # If failure occurred, simulate data truncation:
      to_keep_vec <- c(rep(1, 10), cumprod(
        1 - stats::rbinom(nrow(x) - 10, 1, prob = 0.01)))
      if (!any(to_keep_vec == 0)) failure_occurred <- FALSE
      
      x <- x[to_keep_vec == 1, ]
    }
    
    devices_failed[x] <- failure_occurred
    return(x)
    
  }) # end of lapply
  
  return(list(obj = obj, dev_failed = devices_failed))
  
} # end of function, .trigger_tag_failure()

#' @title Trigger fix success rate
#' @description
#' Simulates fix success rate based on a given  probability. For each
#' dataset, a number of rows (randomly) corresponding to the success rate
#' are retained, while the remaining locations are discarded.
#' 
#' @noRd
.trigger_fix_success <- function(obj, prob = 0) {
  
  if (is.null(prob)) return(obj)
  if (prob == 0) return(obj)
  
  # if (!inherits(obj, "movedesign")) {
  #   stop("The 'obj' argument must be of class 'movedesign'.")
  # }
  
  obj <- lapply(obj, function(x) {
    to_keep <- round(nrow(x) * (1 - prob), 0)
    to_keep_vec <- sort(sample(seq_len(nrow(x)), to_keep, replace = FALSE))
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
  
  if (is.null(error)) return(obj)
  if (error == 0) return(obj)
  
  # if (!inherits(obj, "movedesign")) {
  #   stop("The 'obj' argument must be of class 'movedesign'.")
  # }
  
  obj <- lapply(obj, function(x) {
    
    x$error_x <- x$error_y <- stats::rnorm(
      nrow(x), mean = 0, sd = error)
    
    x$HDOP <- sqrt(2) * sqrt(x$error_x^2 + x$error_y^2) /
      sqrt(-2 * log(0.05))
    
    x$original_x <- x$x
    x$original_y <- x$y
    x[c("x", "y")] <- x[c("x", "y")] + c(x$error_x, x$error_y)
    
    ctmm::uere(x) <- 1
    return(x)
    
  }) # end of lapply
  
  return(obj)
  
} # end of function, .trigger_error()


#' Generate an irregular time sequence based on day/night intervals
#'
#' This function generates a sequence of timestamps with different step sizes
#' for day and night periods.
#'
#' @param start_day The start time of the daytime period (as "HH:MM:SS").
#' @param start_night The start time of the nighttime period (as "HH:MM:SS").
#' @param dti_day The step size during the daytime period (in seconds, must be positive).
#' @param dti_night The step size during the nighttime period (in seconds, must be positive).
#' @param start_time The start timestamp (default: "00:00:00").
#' @param end_time The end timestamp (default: "23:59:59").
#' 
#' @keywords internal
#' 
#' @return A sorted vector of POSIXct timestamps.
#' 
#' @examples
#' .generate_day_night_t("06:00:00", "18:00:00", 3600, 4 * 3600)
#' 
#' @noRd
.generate_day_night_t <- function(start_day, 
                                  start_night,
                                  dti_day,
                                  dti_night) {
  
  start_time = "00:00:00"
  end_time = "23:59:59"
  
  if (start_time >= end_time)
    stop("'start_time' must be before 'end_time'!")
  
  # Validate time format function
  validate_time_format <- function(time_str) {
    if (!grepl("^\\d{2}:\\d{2}:\\d{2}$", time_str)) {
      stop(paste("Invalid time format for", 
                 time_str, "Expected format: HH:MM:SS"))
    }
  }
  
  # Validate the time format for each input time
  lapply(c(start_day, start_night,
           start_time, end_time), validate_time_format)
  
  if (!is.null(dti_day))
    stopifnot(is.numeric(dti_day) & dti_day > 0)
  if (!is.null(dti_night))
    stopifnot(is.numeric(dti_night) & dti_night > 0)
  
  # Convert times to numeric seconds (total seconds since midnight)
  start_time <- as.numeric(lubridate::hms(start_time))
  end_time <- as.numeric(lubridate::hms(end_time))
  start_day <- as.numeric(lubridate::hms(start_day))
  start_night <- as.numeric(lubridate::hms(start_night))
  
  if (start_day >= start_night) 
    stop("'start_day' must be before 'start_night'!")
  if (start_time >= end_time) 
    stop("'start_time' must be before 'end_time'!")
  
  # Create intervals for day and night periods
  intervals <- data.frame(
    start = c(start_time, start_day, start_night),
    end = c(start_day, start_night, end_time),
    step = c(dti_night, dti_day, dti_night),
    cycle = c("night", "day", "night"))
  
  if (is.null(dti_day)) {
  }
  
  if (is.null(dti_night)) {
    intervals[1, "step"] <- start_day - start_time
    intervals[3, "step"] <- start_night - start_day
  }
  
  out <- .generate_irregular_t(intervals, 
                               start_time = start_time, 
                               end_time = end_time)
  
  if (!is.null(dti_day) && is.null(dti_night))
    out <- out[!out == "0"]
  
  return(out)
  
} # end of function, .generate_day_night_t()


#' Generate an irregular time sequence based on custom intervals
#'
#' This function generates a sequence of timestamps with different step sizes
#' for different time intervals.
#'
#' @param intervals A data frame with columns 'start', 'end', and 'step',
#'   defining time intervals and their respective step sizes (in seconds).
#' @param start_time The start timestamp (default: "00:00:00").
#' @param end_time The end timestamp (default: "23:59:59").
#' 
#' @keywords internal
#' 
#' @return A sorted vector of POSIXct timestamps.
#' 
#' @examples
#' intervals <- data.frame(
#'   start = hms("00:00:00"),
#'   end = hms("06:00:00"),
#'   step = 14400)
#' 
#' .generate_irregular_t(intervals)
#' 
#' @noRd
.generate_irregular_t <- function(intervals, 
                                  start_time = start_time, 
                                  end_time = end_time) {
  
  # Validate the intervals dataframe
  if (!is.data.frame(intervals) ||
      !all(c("start", "end", "step") %in% names(intervals))) {
    stop(paste("'intervals' must be a data frame with 'start',",
               "'end', and 'step' columns"))
  }
  
  if (any(intervals$step <= 0))
    stop("All step values must be positive!")
  
  # Generate the time sequence for each interval:
  
  out <- unique(sort(unlist(
    mapply(seq,
           intervals$start,
           intervals$end,
           intervals$step, SIMPLIFY = FALSE))))
  
  return(out)
  
} # end of function, .generate_irregular_t()

