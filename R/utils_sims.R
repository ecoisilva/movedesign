

#' @title Generate a unique random seed
#'
#' @description
#' Generates a single random integer seed in the range `[1, 9999999]`. If
#' a `seed_list` is provided, the function ensures the generated seed is
#' not already present in the list, and avoids generating a value
#' immediately followed by another in the list.
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


#' @title Trigger random deployment disruption events
#' 
#' @description
#' Introduces probabilistic failure events into a list of telemetry
#' datasets, simulating real-world tag malfunction or animal mortality.
#' Each dataset is truncated stochastically if a failure occurs,
#' resulting in variable sampling durations across individuals.
#' 
#' Failure is applied per individual, with each device having a
#' user-defined probability (`fail_prob`) of failing. If a failure occurs,
#'  the dataset is truncated after a random time point.
#' 
#' @param obj A list of data frames, where each element corresponds to
#'   a single individual’s telemetry data.
#' 
#' @return
#' A list with two components:
#' - `obj`: A list of modified data frames, where some may have been
#'   truncated due to simulated device failure.
#' - `dev_failed`: A logical vector indicating which individuals
#'   experienced tag failure (`TRUE`) and which did not (`FALSE`).
#' 
#' @param fail_prob A numeric value between `0` and `1` (default = `0`),
#'   representing the probability that any given device fails. If `0` or
#'   `NULL`, all devices are assumed to remain active.
#'
#' @noRd
.trigger_disruption <- function(obj, fail_prob = 0) {
  
  # if (!inherits(obj, "movedesign")) {
  #   stop("The 'obj' argument must be of class 'movedesign'.")
  # }
  
  if (is.null(fail_prob)) 
    return(list(obj = obj,
                dev_failed = logical(length(obj))))
  if (fail_prob == 0) 
    return(list(obj = obj,
                dev_failed = logical(length(obj))))
  
  devices_failed <- logical(length(obj))
  
  obj <- lapply(obj, function(x) {
    
    failure_occurred <- sample(
      c(FALSE, TRUE), size = 1, 
      prob = c(1 - fail_prob, fail_prob))
    
    to_keep_vec <- rep(1, nrow(x))
    
    if (failure_occurred) {
      # If failure occurred, truncate data:
      to_keep_vec <- c(rep(1, 10), cumprod(
        1 - stats::rbinom(nrow(x) - 10, 1, prob = 0.01)))
      if (!any(to_keep_vec == 0)) failure_occurred <- FALSE
      
      x <- x[to_keep_vec == 1, ]
    }
    
    devices_failed[x] <- failure_occurred
    return(x)
    
  }) # end of lapply
  
  return(list(obj = obj, dev_failed = devices_failed))
  
} # end of function, .trigger_disruption()

#' @title Trigger fix success rate
#' 
#' @description
#' Simulates fix success rate based on a given probability. For each
#' dataset, a number of rows (randomly) corresponding to the success rate
#' are retained, while the remaining locations are discarded.
#' 
#' @noRd
.trigger_fix_success <- function(obj, prob = 0) {
  
  # if (!inherits(obj, "movedesign")) {
  #   stop("The 'obj' argument must be of class 'movedesign'.")
  # }
  
  if (is.null(prob)) return(obj)
  if (!is.numeric(prob) ||
      length(prob) != 1 ||
      is.na(prob) || prob < 0 || prob > 1) {
    stop("'prob' must be a numeric value between 0 and 1 inclusive.")
  }
  
  obj <- lapply(obj, function(x) {
    to_keep <- round(nrow(x) * (prob), 0)
    to_keep_vec <- sort(sample(seq_len(nrow(x)), to_keep, replace = FALSE))
    return(x[to_keep_vec, ])
    
  }) # end of lapply
  
  return(obj)
  
} # end of function, .trigger_fix_success()

#' @title Trigger location error
#' 
#' @description
#' Simulates location error in tracking data (x, y coordinates) by adding
#' random noise drawn from a normal distribution. The noise is scaled by
#' the specified error parameter.
#' 
#' @noRd
.trigger_error <- function(obj, error = 0) {
  
  # if (!inherits(obj, "movedesign")) {
  #   stop("The 'obj' argument must be of class 'movedesign'.")
  # }
  
  if (is.null(error)) return(obj)
  if (error == 0) return(obj)
  
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


#' @title Generate irregular day/night intervals
#' 
#' @description
#' This function generates a sequence of timestamps with different
#' sampling intervals for day and night periods.
#'
#' @param fail_prob A numeric value between `0` and `1` (default = `0`),
#'   representing the probability that any given device fails. If `0` or
#'   `NULL`, all devices are assumed to remain active.
#'   
#' @param start_day The start time of the daytime period.
#' @param start_night The start time of the nighttime period.
#' @param dti_day The sampling interval during the daytime period
#'   (in seconds).
#' @param dti_night The sampling interval during the nighttime period
#'   (in seconds).
#' 
#' @keywords internal
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


#' @title Generate an irregular day/night time sequence
#' 
#' @description
#' Generate an irregular time sequence based on custom intervals.
#'
#' @param intervals A data frame with columns 'start', 'end', and 'step',
#'   defining time intervals and their respective step sizes (in seconds).
#' @param start_time The start timestamp (default: "00:00:00").
#' @param end_time The end timestamp (default: "23:59:59").
#' 
#' @keywords internal
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


#' @title Generate an irregular day/night sampling schedule
#' 
#' @description
#' Creates a sequence of POSIXct timestamps over a date range, using
#' separate time steps for day and night periods within each day. Day and
#' night periods are defined by user-supplied start times, and sampling
#' intervals can differ for day and night.
#'
#' @param date_start Character or Date.  Start date (inclusive) for
#'  tracking schedule.
#' @param date_end Character or Date. End date (inclusive) for
#'  tracking schedule.
#' @param daytime_start Character. Time of day (HH:MM:SS) marking the
#'   start of the daytime period.
#' @param nighttime_start Character. Time of day (HH:MM:SS) marking the
#'   start of the nighttime period.
#' @param dti_day The sampling interval during the daytime period
#'   (in seconds).
#' @param dti_night The sampling interval during the nighttime period
#'   (in seconds).
#' @param tz Character. Time zone for output timestamps (default: "UTC").
#'
#' @return A sorted POSIXct vector of timestamps for the entire schedule.
#'
#' @examples
#' # Generate timestamps for 2 days,
#' # with sampling interval of 1 hour for daytime, 2 hours for nighttime
#' ts <- .generate_schedule(
#'   date_start = "2023-01-01",
#'   date_end = "2023-01-02",
#'   daytime_start = "06:00:00",
#'   nighttime_start = "18:00:00",
#'   dti_day = 3600,
#'   dti_night = 7200,
#'   tz = "UTC"
#' )
#' 
#' @keywords internal
#' 
#' @noRd
.generate_schedule <- function(date_start,
                               date_end,
                               daytime_start,
                               nighttime_start,
                               dti_day,
                               dti_night,
                               tz = "UTC") {
  
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)
  
  if (date_start > date_end) {
    stop("'date_start' must be on or before 'date_end'.")
  }
  
  # Generate sequence of dates from start to end:
  date_seq <- seq.Date(date_start, date_end, by = "1 day")
  
  # Preallocate output list:
  out_list <- vector("list", length(date_seq))
  
  for (i in seq_along(date_seq)) {
    # Generate time offsets (in seconds) for this day
    seconds_since_midnight <- movedesign:::.generate_day_night_t(
      start_day = daytime_start,
      start_night = nighttime_start,
      dti_day = dti_day,
      dti_night = dti_night
    )
    
    # Combine with date origin in the correct time zone
    date_origin <- as.POSIXct(date_seq[i], tz = tz)
    out_list[[i]] <- date_origin + seconds_since_midnight
  }
  
  # Flatten the list and sort timestamps
  result <- sort(do.call(c, out_list))
  return(result)
}

