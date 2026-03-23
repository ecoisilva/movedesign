
#' @title Simulate movement data from species parameters
#'
#' @description
#' Simulates continuous-time movement trajectories based on
#' user-specified movement parameters. The function is designed to
#' support study design workflows by generating synthetic tracking data
#' under specified the movement, sampling, and analytical assumptions.
#' 
#' Use this function when you do not have empirical data available.
#' For workflows grounded in real data, use [md_prepare()] instead,
#' which extracts movement parameters directly from fitted models.
#' 
#' @param n_individuals Integer. Number of tracked individuals (tags)
#'   to simulate. Defines the target population sample size. When
#'   `grouped = TRUE`, this number is currently split evenly between
#'   the two groups, so it must be even.
#'   
#' @param tau_p Position autocorrelation timescale, corresponding to
#'   the average *home range crossing time*.
#'   
#'   Provide a list with two elements:
#'   \itemize{
#'     \item `value` — a numeric value (e.g. `6`)
#'     \item `unit`  — a character string: one of `"second"`,
#'       `"minute"`, `"hour"`, `"day"`, `"month"`, or `"year"`
#'   }
#' 
#'   **Example:** `list(value = 6, unit = "hours")`
#' 
#'   When `grouped = TRUE`, provide a named list with two entries,
#'   one per group:
#' 
#'   ```r
#'   list(
#'     A = list(value = 6,  unit = "hours"),
#'     B = list(value = 12, unit = "hours")
#'   )
#'   ```
#' 
#' @param tau_v Velocity autocorrelation timescale, corresponding to
#'   directional persistence (how long does an animal maintains a
#'   consistent direction and speed before changing course).
#'
#'   Same format as `tau_p`.
#'
#' @param sigma Location variance parameter. Captures the overall
#'   spatial extent of movement.
#'
#'   Same format as `tau_p`.
#'
#' @param dur Sampling duration. A list with elements `value`
#'   (numeric) and `unit` (character).
#'
#'   **Example:** `list(value = 3, unit = "months")`
#'
#' @param dti Sampling interval between relocations.
#'   A list with elements `value` (numeric) and `unit` (character).
#'
#'   **Example:** `list(value = 2, unit = "hours")`
#'
#' @param set_target Character vector specifying the target metrics
#'   to be evaluated in the study design workflow. Choose one or
#'   both:
#'   \itemize{
#'     \item `"hr"`   — home range area
#'     \item `"ctsd"` — continuous-time speed and distance
#'   }
#'   Defaults to `c("hr", "ctsd")`.
#'
#' @param which_meta Character specifying the population-level
#'   analytical target. Choose one:
#'   \itemize{
#'     \item `"mean"` (default) — estimates the average value
#'       across all individuals.
#'     \item `"ratio"` — compares the mean between groups A and B.
#'       Requires `grouped = TRUE`.
#'   }
#'   
#' @param grouped Logical. Set to `TRUE` to simulate two distinct
#'   groups (e.g. males and females) with different movement
#'   parameters. When `TRUE`, all three parameter arguments
#'   (`tau_p`, `tau_v`, `sigma`) must be named lists for both groups,
#'   and `n_individuals` must be even. Defaults to `FALSE`.
#'   
#' @param parallel Logical. Whether to use parallel processing
#'   during model fitting. Defaults to `FALSE`.
#'   
#' @param seed Optional integer. Random seed for reproducibility.
#'   If `NULL` (default), a seed is chosen automatically and stored
#'   in the returned object so results can be reproduced later.
#'
#' @details
#' 
#' Each simulated trajectory represents a single continuously-tracked
#' animal, generated from a continuous-time movement model with the
#' parameters you supply. The time vector is constructed from `dur`
#' and `dti`, and then one trajectory per individual is drawn from
#' that model.
#' 
#' Simulated data are immediately passed through the full
#' `movedesign` workflow — model fitting, aggregation, and
#' estimation of the target metrics — so the returned object is
#' ready for study design evaluation without further steps.
#' 
#' When `grouped = TRUE`, simulations are generated independently for
#' each using their own movement parameters but currently share the
#' same sampling parameters (`dur` and `dti`).  Group structure
#' only affects downstream inference when `which_meta = "ratio"`.
#'
#' @note
#' Results are only as informative as the parameters you provide.
#' Where possible, derive parameters from real tracking data using
#' [md_prepare()]. Simulations based on arbitrary or weakly justified
#' parametersvalues may be useful for exploration purposes, but should
#' be interpreted with caution in any design or inference context.
#' 
#' @return
#' An object of class `movedesign_input`. This is the standard input
#' object for the `movedesign` workflow and can be passed directly
#' to downstream functions such as `md_run()` or `md_replicate()`.
#' It contains the simulated trajectories, fitted movement models,
#' and all metadata needed for study design evaluation.
#'
#' @examples
#' if(interactive()) {
#' 
#' # Single group:
#' # (simulate 10 individuals over 3 months with fixes every 2 hours)
#' 
#' input <- md_simulate(
#'   n_individuals = 4,
#'   tau_p = list(value = 6, unit = "hours"),
#'   tau_v = list(value = 30, unit = "minutes"),
#'   sigma = list(value = 1, unit = "km^2"),
#'   dur = list(value = 1, unit = "month"),
#'   dti = list(value = 2, unit = "hours"))
#'
#' # Two groups with different parameters:
#' 
#' input_grouped <- md_simulate(
#'   n_individuals = 10,
#'   tau_p  = list(
#'     A = list(value = 6,  unit = "hours"),
#'     B = list(value = 12, unit = "hours")
#'   ),
#'   tau_v  = list(
#'     A = list(value = 0.5, unit = "hours"),
#'     B = list(value = 1,   unit = "hours")
#'   ),
#'   sigma  = list(
#'     A = list(value = 1, unit = "km^2"),
#'     B = list(value = 2, unit = "km^2")
#'   ),
#'   dur = list(value = 3, unit = "months"),
#'   dti = list(value = 2, unit = "hours"),
#'   grouped = TRUE,
#'   which_meta = "ratio")
#'   
#' }
#'
#' @seealso [md_prepare()] to derive parameters from real tracking
#'   data.
#'
#' @importFrom ctmm %#%
#' 
#' @family workflow_steps
#' @export
md_simulate <- function(n_individuals = NULL,
                        tau_p,
                        tau_v,
                        sigma,
                        dur = NULL,
                        dti = NULL,
                        set_target = c("hr", "ctsd"),
                        which_meta = "mean",
                        grouped = FALSE,
                        parallel = FALSE,
                        seed = NULL) {
  
  if (grouped && which_meta == "mean") {
    warning(paste0(
      "Groups were specified, but the analytical target is set",
      " to `mean`. Therefore, group structure will be ignored.",
      " If this is not the intended behavior, set",
      " `which_meta = \"ratio\"` to compare group means."))
  }
  
  if (which_meta == "ratio") which_meta <- "compare"
  
  if (missing(n_individuals) || 
      !is.numeric(n_individuals) ||
      length(n_individuals) != 1)
    stop("'n_individuals' must be a single integer.", call. = FALSE)
  
  if (missing(dur) || 
      !is.list(dur) ||
      !all(c("value", "unit") %in% names(dur)))
    stop("'dur' must be a list with elements 'value' and 'unit'.",
         call. = FALSE)
  if (missing(dti) || 
      !is.list(dti) || 
      !all(c("value", "unit") %in% names(dti)))
    stop("'dti' must be a list with elements 'value' and 'unit'.",
         call. = FALSE)
  
  if (grouped && n_individuals %% 2 != 0)
    stop("'n_individuals' must be even when 'grouped' is TRUE.",
         call. = FALSE)
  
  if (!is.logical(parallel) ||
      length(parallel) != 1L ||
      is.na(parallel)) {
    stop("'parallel' must be TRUE or FALSE.", call. = FALSE)
  }
  
  species <- "simulated"
  add_individual_variation <- FALSE
  
  .stop_if <- function(cond, msg) {
    if (cond) stop(msg, call. = FALSE)
  }
  
  .validate_parameters <- function(x, grouped, name) {
    
    .make_df <- function(value, unit) {
      data.frame(value = c(NA, value, NA),
                 unit  = rep(unit, 3),
                 row.names = c("low", "est", "high"))
    }
    
    if (!grouped) {
      .stop_if(!is.list(x),
               paste0(name,
                      " must be a list when grouped = FALSE"))
      .stop_if(!all(c("value", "unit") %in% names(x)),
               paste0(name,
                      " must contain 'value' and 'unit'"))
      
      return(list(All = .make_df(x$value[[1]], x$unit[[1]])))
    }
    
    .stop_if(!is.list(x) || is.null(names(x)),
             paste0(name, 
                    " must be a named list when grouped = TRUE"))
    
    out <- lapply(names(x), function(g) {
      xi <- x[[g]]
      .stop_if(!is.list(xi),
               paste0(name, "[[", g, "]] must be a list"))
      .stop_if(!all(c("value", "unit") %in% names(xi)),
               paste0(name, "[[", g,
                      "]] must contain 'value' and 'unit'"))
      
      .make_df(xi$value[[1]], xi$unit[[1]])
    })
    
    names(out) <- names(x)
    return(out)
  }
  
  .validate_target <- function(set_target) {
    if (!is.character(set_target) || anyDuplicated(set_target) || 
        !all(set_target %in% c("hr", "ctsd"))) {
      stop("`set_target` must be 'hr', 'ctsd', or both.")
    }
    return(set_target)
  }
  
  .validate_sampling <- function(param, key = NULL) {
    
    .check_entry <- function(x) {
      is.list(x) &&
        all(c("value", "unit") %in% names(x)) &&
        is.numeric(x$value) &&
        is.character(x$unit) &&
        length(x$value) == 1 &&
        length(x$unit) == 1
    }
    
    is_simple <- is.list(param) && .check_entry(param)
    
    is_list_of_simple <- is.list(param) &&
      length(param) > 0 &&
      all(vapply(param, .check_entry, logical(1)))
    
    if (!(is_simple || is_list_of_simple)) {
      stop(paste0(
        "Invalid '", key, "':",
        "must be either a simple list with numeric 'value' and",
        "character 'unit', or a list of such lists."
      ))
    }
  }
  
  .validate_sampling(dur, "dur")
  .validate_sampling(dti, "dti")
  set_target <- .validate_target(set_target)
  
  tau_p <- .validate_parameters(tau_p, grouped, "tau_p")
  tau_v <- .validate_parameters(tau_v, grouped, "tau_v")
  sigma <- .validate_parameters(sigma, grouped, "sigma")
  
  if (is.null(dur) || is.null(dti)) {
    stop("Both dur and dti must be provided", call. = FALSE)
  }
  
  seed0 <- if (is.null(seed)) generate_seed() else seed
  
  dur0 <- round(dur$value %#% dur$unit, 0)
  dti0 <- round(dti$value %#% dti$unit, 0)
  t0 <- seq(0, dur0, by = dti0)[-1]
  
  data <- vector("list", length(tau_p))
  modList <- vector("list", length(tau_p))
  names(data) <- names(tau_p)
  names(modList) <- names(tau_p)
  
  for (gr in names(tau_p)) {
    
    modList[[gr]] <- prepare_mod(
      tau_p = tau_p[[gr]]$value[[2]],
      tau_p_unit = tau_p[[gr]]$unit[[2]],
      tau_v = tau_v[[gr]]$value[[2]],
      tau_v_unit = tau_v[[gr]]$unit[[2]],
      sigma = sigma[[gr]]$value[[2]],
      sigma_unit = sigma[[gr]]$unit[[2]])
    
    tmp_seed <- seed0
    if (gr == "B") tmp_seed <- seed0 + 1
    dat <- ctmm::simulate(modList[[gr]], t = t0, seed = tmp_seed)
    
    dat <- pseudonymize(dat)
    dat$index <- seq_len(nrow(dat))
    dat$id <- as.character(seed0)
    dat$group <- gr
    data[[gr]] <- dat
    
  }
  
  groups <- NULL
  if (!grouped) {
    names(data) <- as.character(seed0)
    names(modList) <- as.character(seed0)
    seedList <- list(seed0)
    
  } else {
    names(data) <- c(as.character(seed0),
                     as.character(seed0 + 1))
    names(modList) <- c(as.character(seed0),
                        as.character(seed0 + 1))
    seedList <- list(seed0, seed0 + 1)
    
    groups <- vector("list", 2)
    groups[[1]] <- list(A = seed0, B = seed0 + 1)
    groups[[2]] <- list(A = c(), B = c())
    names(groups) <- NULL
  }
  
  #  Fit movement models:
  
  fitList <- fitting_models(data, parallel = parallel)
  names(fitList) <- names(data)
  
  meanfitList <- list(NULL)
  names(meanfitList) <- "All"
  
  mu <- list(array(0, dim = 2, dimnames = list(c("x", "y"))))
  names(mu) <- "All"
  
  use_global_parameters <- is.list(dur) &&
    all(c("value", "unit") %in% names(dur)) &&
    !any(sapply(dur, is.list))
  
  design <- movedesign_input(list(
    data = data,
    data_type = "simulated",
    get_species = species,
    n_individuals = as.numeric(n_individuals),
    dur = dur,
    dti = dti,
    use_global_parameters = use_global_parameters,
    add_ind_var = add_individual_variation,
    grouped = grouped,
    groups = groups,
    set_target = set_target,
    which_meta = which_meta,
    which_m = "set_m",
    parallel = parallel,
    modList = modList,
    fitList = fitList,
    meanfitList = meanfitList,
    sigma = sigma,
    tau_p = tau_p,
    tau_v = tau_v,
    mu = mu,
    seed = seed0))
  
  return(design)
  
}


#' @title Prepare movement study design inputs
#'
#' @description
#' Prepares and validates all inputs needed to evaluate the study design
#' of animal movement projects using parameters derived from empirical
#' tracking data. The function checks data integrity, fits or verifies
#' movement models, extracts key parameters, and consolidates all
#' settings into a structured object for reproducible and streamlined
#' downstream analyses.
#' 
#' If you do not have empirical data, use [md_simulate()]
#' instead, which builds inputs from user-specified parameters.
#' 
#' @param species Character. A label for the focal species
#'   (scientific or common name). Used for display and
#'   bookkeeping only; does not affect results.
#'   
#' @param data A named list of telemetry objects, created with
#'   [ctmm::as.telemetry()], to be used as the empirical basis for the
#'   simulations. Each telemetry object must contain valid metadata
#'   and timestamped locations.
#'   
#' @param models (Optional) Named list of fitted movement
#'   models, one per individual, created with `ctmm::ctmm.select()`.
#'   Names must match those in `data`. If not supplied, models are
#'   fitted automatically.
#'   
#' @param n_individuals A single positive integer. The target number
#'   of animals in the study design (equivalent to number of tags to
#'   be deployed in the field). This defines the *population* sample
#'   size used in downstream analyses, and does not need to match the
#'   number of individuals in `data`.
#'   
#' @param dur Study duration. A list with elements `value`
#'   (numeric) and `unit` (character).
#'   
#'   **Example:** `list(value = 2, unit = "months")`
#'   
#'   Valid units: `"second"`, `"minute"`, `"hour"`,
#'   `"day"`, `"month"`, `"year"`.
#'
#' @param dti Sampling interval between consecutive GPS fixes.
#'   A list with elements `value` (numeric) and `unit` (character).
#'   Same valid units as `dur`.
#'   
#'   **Example:** `list(value = 1, unit = "day")`
#'   
#' @param set_target Character vector specifying the target metrics
#'   to be evaluated in the study design workflow. Choose one or
#'   both:
#'   \itemize{
#'     \item `"hr"`   — home range area
#'     \item `"ctsd"` — continuous-time speed and distance
#'   }
#'   Defaults to `c("hr", "ctsd")`.
#'
#' @param which_meta Character. Specifies the analytical target for
#'   population-level inference. Choose one:
#'   \itemize{
#'     \item `"mean"` (default) — estimates the average
#'       across all individuals.
#'     \item `"ratio"` — compares the mean between groups
#'       `"A"` and `"B"`. Requires `groups` to be specified.
#'     \item `NULL` — single-individual inference. Requires
#'       `data` to be a single telemetry object rather than
#'       a list.
#'   }
#'   
#' @param add_individual_variation Logical. If `TRUE`, simulates
#'   variation by drawing movement parameters from the population
#'   distribution. This produces more realistic
#'   between-individual variability. Defaults to `FALSE`.
#'   
#' @param groups (Optional) A named list assigning individuals
#'   to two groups, required when `which_meta = "ratio"`.
#'   Each element is a character vector of individual names
#'   (matching `data`).
#'   
#'   **Example:**
#'   ```r
#'   list(
#'     A = c("Animal_01", "Animal_02"),
#'     B = c("Animal_03", "Animal_04")
#'   )
#'   ```
#'   
#' @param parallel Logical. Whether to use parallel processing
#'   during model fitting. Defaults to `FALSE`.
#'   
#' @param .seed (Optional) Integer. Random seed for reproducibility.
#'   If `NULL` (default), a seed is chosen automatically and stored
#'   in the returned object so results can be reproduced later.
#'   Only needed to be specifiedwhen reproducing Shiny app analyses
#'   in the R console.
#'   
#' @details
#' This function is designed to streamline and standardize the
#' preparation of input data and study design parameters for
#' simulation-based movement ecology analyses. It performs the
#' following key steps:
#' 
#' \itemize{
#'   \item Validates that `data` is a non-empty list of telemetry
#'     objects.
#'   \item Fits movement models to each individual if not supplied.
#'   \item Checks supplied movement models for validity.
#'   \item Extracts parameters (`tau_p`, `tau_v`, `sigma`) from fitted
#'     models for use in downstream simulations.
#'   \item Consolidates all settings, parameters, and model objects
#'     into a single structured object.
#' }
#' 
#' By default (`add_individual_variation = FALSE`), all
#' simulated animals share the same movement parameters,
#' estimated from the population mean. Setting
#' `add_individual_variation = TRUE` instead randomly draws parameters
#' from the population distribution for each individual, which
#' better reflects natural variability but increases
#' uncertainty in downstream estimates.
#' 
#' @return
#' An object of class `movedesign_input`, accepted by all
#' downstream functions such as [md_run()] and
#' [md_replicate()]. Contains the validated inputs, fitted
#' models, extracted movement parameters, and all metadata
#' needed for study design evaluation.
#' 
#' @examples
#' if(interactive()) {
#'   
#'   data(buffalo)
#'   input <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 1, unit = "month"),
#'     dti = list(value = 1, unit = "day"),
#'     set_target = "hr",
#'     which_meta = "mean")
#'     
#'  summary(input)
#' }
#' 
#' @seealso
#'   [md_simulate()] to build inputs from directly specified
#'   parameters rather than empirical data,
#'   [md_run()],
#'   [md_replicate()].
#'   
#' @importFrom ctmm %#%
#' 
#' @family workflow_steps
#' @export
md_prepare <- function(species = NULL,
                       data,
                       models = NULL,
                       n_individuals = NULL,
                       dur = NULL,
                       dti = NULL,
                       set_target = c("hr", "ctsd"),
                       which_meta = "mean",
                       add_individual_variation = FALSE,
                       groups = NULL,
                       parallel = FALSE,
                       .seed = NULL) {
  
  if (is.null(species) ||
      !is.character(species) ||
      nchar(trimws(species)) == 0L) {
    stop("'species' must be a non-empty string.",
         call. = FALSE)
  }
  
  if (!is.logical(parallel) ||
      length(parallel) != 1L ||
      is.na(parallel)) {
    stop("'parallel' must be TRUE or FALSE.",
         call. = FALSE)
  }
  
  .validate_target <- function(set_target) {
    if (!is.character(set_target) || anyDuplicated(set_target) || 
        !all(set_target %in% c("hr", "ctsd"))) {
      stop("`set_target` must be 'hr', 'ctsd', or both.")
    }
    return(set_target)
  }
  
  .validate_meta <- function(which_meta, data) {
    valid_meta <- c("mean", "ratio")
    if (!is.null(which_meta) && !(which_meta %in% valid_meta)) {
      stop("`which_meta` must be either NULL, 'mean', or 'ratio'.")
    }
    if (is.null(which_meta)) {
      if (!inherits(data, "telemetry")) {
        stop(paste("If `which_meta` is NULL, 'data' must be a",
                   "single 'telemetry' object."))
      }
      if (is.null(data$identity)) {
        stop("If `which_meta` is NULL,",
             "'data$identity' must not be NULL.")
      }
    } else {
      if (!is.list(data)) {
        stop(paste("If `which_meta` is 'mean' or 'ratio',",
                   "'data' must be a list of telemetry objects."))
      }
      if (length(data) == 0 || !inherits(data[[1]], "telemetry")) {
        stop(paste("If `which_meta` is 'mean' or 'ratio',",
                   "'data' must be a list of telemetry objects."))
      }
    }
    invisible(TRUE)
  }
  
  .validate_sampling <- function(param, key = NULL) {
    .check_entry <- function(x) {
      is.list(x) &&
        all(c("value", "unit") %in% names(x)) &&
        is.numeric(x$value) &&
        is.character(x$unit) &&
        length(x$value) == 1 &&
        length(x$unit) == 1
    }
    
    is_simple <- is.list(param) && .check_entry(param)
    
    is_list_of_simple <- is.list(param) &&
      length(param) > 0 &&
      all(vapply(param, .check_entry, logical(1)))
    
    if (!(is_simple || is_list_of_simple)) {
      stop(paste0(
        "Invalid '", key, "':",
        "must be either a simple list with numeric 'value' and",
        "character 'unit', or a list of such lists."
      ))
    }
  }
  
  stopifnot(is.list(data))
  if (length(data) == 0) stop("Input 'data' cannot be empty.")
  
  set_target <- .validate_target(set_target)
  .validate_meta(which_meta, data)
  
  if (!is.null(dur)) {
    if (missing(dur) || 
        !is.list(dur) ||
        !all(c("value", "unit") %in% names(dur)))
      stop("'dur' must be a list with elements 'value' and 'unit'.")
    if (missing(dti) || 
        !is.list(dti) || 
        !all(c("value", "unit") %in% names(dti)))
      stop("'dti' must be a list with elements 'value' and 'unit'.")
    .validate_sampling(dur, "dur")
  }
  
  if (!is.null(dti)) {
    if (missing(dur) || 
        !is.list(dur) ||
        !all(c("value", "unit") %in% names(dur)))
      stop("'dur' must be a list with elements 'value' and 'unit'.")
    if (missing(dti) || 
        !is.list(dti) || 
        !all(c("value", "unit") %in% names(dti)))
      stop("'dti' must be a list with elements 'value' and 'unit'.")
    .validate_sampling(dti, "dti")
  }
  
  which_m <- ifelse(is.null(dur) && is.null(dti),
                    "get_all", "set_m")
  
  if (!is.null(groups)) {
    if (!is.list(groups) || length(groups) < 2) {
      stop(paste("'groups' must be a named list with",
                 "at least two groups."))
    }
    group_names <- names(groups)
    if (any(group_names == "")) {
      stop("All groups must be named in the 'groups' list.")
    }
    
    # All individuals in groups must exist in data:
    data_names <- names(data)
    for (g in group_names) {
      if (length(groups[[g]]) == 0) {
        stop(sprintf(
          "Group '%s' must contain at least one individual.", g))
      }
      absent <- setdiff(groups[[g]], data_names)
      if (length(absent) > 0) {
        stop(sprintf(
          "Group '%s' contains individual not present in 'data': %s",
          g, paste(absent, collapse = ", ")))
      }
    }
  }
  
  if (!is.null(groups) && which_meta == "mean") {
    warning(paste0(
      "Groups were specified, but the analytical target is set",
      " to `mean`. Therefore, group structure will be ignored.",
      " If this is not the intended behavior, set",
      " `which_meta = \"ratio\"` to compare group means."
    ))
    groups <- NULL
  }
  
  if (which_meta == "ratio") {
    if (is.null(groups)) {
      stop(paste("To use `which_meta = \"ratio\"`, you must",
                 "provide a valid `groups` argument."))
    }
    which_meta <- "compare"
  }
  
  info_ok <- all(sapply(data, function(x) !is.null(x@info)))
  if (!info_ok) stop(
    "Each telemetry object in 'data' must have @info metadata.")
  
  if (missing(n_individuals) || 
      !is.numeric(n_individuals) ||
      length(n_individuals) != 1)
    stop("'n_individuals' must be a single integer.")
  
  if (!is.null(groups) && n_individuals %% 2 != 0)
    stop("'n_individuals' must be even when 'groups' is not NULL.")
  
  # Fit or validate models:
  
  fitList <- if (is.null(models)) {
    fitting_models(data, parallel = parallel)
  } else {
    if (!all(sapply(models, function(m) 
      inherits(m, c("ctmm", "ctmm.select")))))
      stop("Models must be from ctmm.select().")
    models
  }
  names(fitList) <- names(data)
  
  if (is.null(.seed)) {
    seedInit <- generate_seed()
  } else {
    seedInit <- .seed
  }
  
  meanfitList <- list(mean_seeded(fitList, seedInit))
  names(meanfitList) <- "All"
  
  if (add_individual_variation) {
    sigma <- suppressWarnings(extract_pars(meanfitList, "sigma"))
    tau_p <- suppressWarnings(extract_pars(meanfitList, "position"))
    tau_v <- suppressWarnings(extract_pars(meanfitList, "velocity"))
  } else {
    sigma <- extract_pars(fitList, "sigma", meta = TRUE)
    tau_p <- extract_pars(fitList, "position", meta = TRUE)
    tau_v <- extract_pars(fitList, "velocity", meta = TRUE)
  }
  
  if (is.null(tau_p)) {
    if ("hr" %in% set_target)
      stop("Position autocorrelation timescale (tau_p) ",
           "is required for home range estimation.")
  } else {
    names(tau_p) <- "All"
  }
  
  if (is.null(tau_v)) {
    if ("ctsd" %in% set_target)
      stop("Velocity autocorrelation timescale (tau_v) ",
           "is required for speed & distance estimation.")
    
  } else {
    names(tau_v) <- "All"
  }
  
  mu <- list(array(0, dim = 2, dimnames = list(c("x", "y"))))
  names(sigma) <- names(mu) <- "All"
  
  if (!is.null(groups)) {
    
    groups[[1]] <- groups
    groups[[2]] <- list(A = c(), B = c())
    names(groups) <- NULL
    
    fitA <- fitList[groups[[1]][["A"]]]
    fitB <- fitList[groups[[1]][["B"]]]
    
    meanfitA <- tryCatch(
      mean_seeded(fitA, seedInit) %>% 
        suppressMessages() %>% 
        suppressWarnings() %>% 
        quiet(),
      error = function(e) e)
    meanfitB <- tryCatch(
      mean_seeded(fitB, seedInit) %>% 
        suppressMessages() %>% 
        suppressWarnings() %>% 
        quiet(),
      error = function(e) e)
    
    if (inherits(meanfitA, "error") ||
        inherits(meanfitB, "error")) {
      stop(paste0(
        "Extraction ", .msg("failed", "danger"), 
        " for one or both groups."))
    } else {
      meanfitList <- list(
        "All" = meanfitList[[1]], 
        "A" = meanfitA, 
        "B" = meanfitB)
    }
    
    mu <- list(mu[[1]], mu[[1]], mu[[1]])
    
    fitA <- tryCatch({
      simulate_seeded(meanfitList[["A"]], seedInit)
    }, error = function(e) {
      message("A warning occurred:", conditionMessage(e), "\n")
    })
    
    fitB <- tryCatch({
      simulate_seeded(meanfitList[["B"]], seedInit + 1)
    }, error = function(e) {
      message("A warning occurred:", conditionMessage(e), "\n")
    })
    
    validate_A <- tryCatch({
      ctmm::simulate(fitA, t = seq(0, 100, by = 1),
                     seed = seedInit)
    }, error = function(e) {
      return(NULL)
    })
    
    validate_B <- tryCatch({
      ctmm::simulate(fitB, t = seq(0, 100, by = 1),
                     seed = seedInit + 1)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(validate_A) || is.null(validate_B)) {
      bug_group <- c()
      if (is.null(validate_A)) bug_group <- c(bug_group, "A")
      if (is.null(validate_B)) bug_group <- c(bug_group, "B")
      
      stop("Validation ", .msg("failed", "danger"),
           " of group(s): ", .msg(toString(bug_group), "danger"))
    }
    
    fit <- list(A = fitA, B = fitB)
    
    sigma <- c(sigma, lapply(1:2, function(x) {
      suppressWarnings(extract_pars(
        obj = fit[[x]],
        name = "sigma", meta = TRUE)[[1]])
    }))
    names(sigma) <- c("All", "A", "B") 
    
    tau_p <- c(tau_p, lapply(1:2, function(x) {
      suppressWarnings(extract_pars(
        obj = fit[[x]], 
        name = "position", meta = TRUE)[[1]])
    }))
    names(tau_p) <- c("All", "A", "B") 
    
    if (!is.null(tau_v)) {
      tau_v <- c(tau_v, lapply(1:2, function(x) {
        suppressWarnings(extract_pars(
          obj = fit[[x]], 
          name = "velocity", meta = TRUE)[[1]])
      }))
      names(tau_v) <- c("All", "A", "B") 
    }
    
    mu <- list(array(0, dim = 2, 
                     dimnames = list(c("x", "y"))),
               array(0, dim = 2, 
                     dimnames = list(c("x", "y"))),
               array(0, dim = 2, 
                     dimnames = list(c("x", "y"))))
    names(mu) <- c("All", "A", "B")
    
  } # end of if (!is.null(groups))
  
  use_global_parameters <- is.list(dur) &&
    all(c("value", "unit") %in% names(dur)) &&
    !any(sapply(dur, is.list))
  
  design <- movedesign_input(list(
    data = data,
    data_type = "selected",
    get_species = species,
    n_individuals = as.numeric(n_individuals),
    dur = dur,
    dti = dti,
    use_global_parameters = use_global_parameters,
    add_ind_var = add_individual_variation,
    grouped = ifelse(!is.null(groups), TRUE, FALSE),
    groups = groups,
    set_target = set_target,
    which_meta = which_meta,
    which_m = which_m,
    parallel = parallel,
    fitList = fitList,
    meanfitList = meanfitList,
    sigma = sigma,
    tau_p = tau_p,
    tau_v = tau_v,
    mu = mu,
    seed = seedInit))
  
  return(design)
}


#' @title Run study design workflow
#'
#' @description
#' The main workhorse of the `movedesign` workflow. Runs one full round
#' of simulation and analyses to evaluate whether the design meets its
#' estimation targets. #' Call this function once your design has been
#' built using [md_prepare()] for empirical data, or [md_simulate()]
#' for user-specified parameters.
#' 
#' Because a single run is subject to stochastic
#' variation, treat outputs from [md_run()] as exploratory, and use
#' [md_replicate()] for more robust inferences (as it aggregates
#' results across multiple replicates).
#' 
#' @param design An object of class `movedesign_input`, as returned by
#'   [md_prepare()] or [md_simulate()].
#'   
#' @param trace Logical. If `TRUE` (default), prints progress messages
#'   and elapsed time for each step. Set to `FALSE` for silent execution.
#'   
#' @param .seeds (Optional) List of integer seeds, one per individual,
#'   used to reproduce a previous run exactly.
#'   Seeds from a prior run are stored in the `$seedList` slot of the
#'   object returned by this function. Leave as `NULL` (default) for
#'   a fresh run with automatically generated seeds.
#'   
#' @return
#' An object of class `movedesign_processed`, accepted by
#' downstream functions such as [md_plot_preview()],
#' or [md_compare_preview()].
#' 
#' @details
#' Progress messages are printed by default.
#' Every individual simulation is assigned a unique random
#' seed, stored in `$seedList` of the returned object.
#' Passing that list to `.seeds` in a subsequent call
#' reproduces the run exactly. This is particularly useful
#' when replicating a result first produced in the Shiny
#' app.
#'
#' Typical workflow:
#' \itemize{
#'   \item Prepare a study design with [md_prepare()].
#'   \item Run all simulations and analyses with [md_run()].
#'   \item Summarize or plot outputs from the returned object.
#' }
#'
#' @seealso
#' [md_prepare()] and [md_simulate()] to build the input
#' object. [md_replicate()] to run the workflow multiple
#' times and aggregate results, which is recommended over
#' a single [md_run()] call for any final design
#' evaluation.
#' [md_plot_preview()] and [md_compare_preview()] to inspect
#' or compare these preliminary outputs.
#'
#' @examples
#' if(interactive()) {
#' 
#' input <- md_prepare(
#'   data = buffalo,
#'   models = models,
#'   species = "buffalo",
#'   n_individuals = 5,
#'   dur = list(value = 1, unit = "month"),
#'   dti = list(value = 1, unit = "day"),
#'   add_individual_variation = FALSE,
#'   set_target = "hr",
#'   which_meta = "mean")
#'   
#' output <- md_run(input)
#' }
#' 
#' @family workflow_steps
#' @export
md_run <- function(design,
                   trace = TRUE,
                   .seeds = NULL) {
  
  if (!inherits(design, "movedesign")) {
    stop(paste("The object must be of class 'movedesign'.",
               "Run md_prepare() first."))
  }
  
  if (!is.logical(trace) ||
      length(trace) != 1L ||
      is.na(trace)) {
    stop("'trace' must be TRUE or FALSE.",
         call. = FALSE)
  }
  
  start_total <- Sys.time()
  
  if (trace) writeLines(paste0(
    crayon::yellow("  \u2015\u2015\u2015\u2015\u2015\u2015"),
    " Simulating ", crayon::yellow("data"), "..."))
  if (trace) start <- Sys.time()
  
  m <- as.integer(design$n_individuals)
  design$simList <- vector("list", m)
  design$seedList <- vector("list", m)
  design$simfitList <- vector("list", m)
  design$akdeList <- vector("list", m)
  design$ctsdList <- vector("list", m)
  
  nms <- list()
  for (i in seq_len(m)) {
    
    if (is.null(.seeds)) {
      seed0 <- generate_seed(nms)
    } else {
      seed0 <- .seeds[[i]]
    }
    
    if (design$grouped) {
      if (i %% 2 == 0) next
      
      tmp <- simulating_data(design, seed0)
      
      design$simList[[i]] <- tmp[[1]]
      design$simList[[i + 1]] <- tmp[[2]]
      
      design$groups[[2]][["A"]] <- c(
        as.character(design$groups[[2]]$A),
        as.character(seed0))
      design$groups[[2]][["B"]] <- c(
        as.character(design$groups[[2]]$B),
        as.character(seed0 + 1))
      nms[[i]] <- seed0
      nms[[i + 1]] <- seed0 + 1
    } else {
      design$simList[[i]] <- simulating_data(design, seed0)[[1]]
      nms[[i]] <- seed0
    }
  }
  
  design$seedList <- nms
  names(design$simList) <- names(design$seedList) <- nms
  # if (trace) .msg_time(start, "        Run time: ")
  
  if (trace) writeLines(paste0(
    crayon::yellow("  \u2015\u2015\u2015\u2015\u2015\u2015"),
    " Fitting ", crayon::yellow("movement models"), "..."))
  if (trace) start <- Sys.time()
  design$simfitList <- fitting_models(
    design$simList, parallel = design$parallel)
  names(design$simfitList) <- names(design$simList)
  if (trace) .msg_time(start, "        Run time: ")
  gc()
  
  if ("hr" %in% design$set_target) {
    
    if (trace) writeLines(paste0(
      crayon::yellow("  \u2015\u2015\u2015\u2015\u2015\u2015"),
      " Estimating ", crayon::yellow("home range"), "..."))
    if (trace) start <- Sys.time()
    design$akdeList <- estimate_hr(design)
    if (trace) .msg_time(start, "        Run time: ")
  }
  
  if ("ctsd" %in% design$set_target) {
    if (trace) writeLines(paste0(
      crayon::yellow("  \u2015\u2015\u2015\u2015\u2015\u2015"),
      " Estimating ", crayon::yellow("speed"), "..."))
    if (trace) start <- Sys.time()
    design$ctsdList <- estimate_speed(design)
    if (trace) .msg_time(start, "        Run time: ")
  }
  
  if ("ctsd" %in% design$set_target) {
    
    n_total <- design$n_individuals
    is_finite <- !.check_for_inf_speed(design$ctsdList)
    n_valid <- sum(is_finite)
    
    if (n_valid == 0)
      stop("The movement model is fractal in all simulations.")
    
    n_fractal <- n_total - n_valid
    if (n_fractal > 0) {
      warning(paste0(
        "The movement model is fractal in ",
        n_fractal, " out of ", n_total, " simulations."),
        call. = FALSE)
    }
  }
  
  # message("\nExecution ended: ", 
  #         crayon::yellow(crayon::bold(
  #           format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))))
  
  # .msg_time(start_total, " Elapsed time: ")
  
  return(movedesign_processed(list(
    data = design$data,
    get_species = design$get_species,
    data_type = design$data_type,
    n_individuals = design$n_individuals,
    dur = design$dur,
    dti = design$dti,
    use_global_parameters = design$use_global_parameters,
    add_ind_var = design$add_ind_var,
    grouped = design$grouped,
    groups = design$groups,
    set_target = design$set_target,
    which_meta = design$which_meta,
    parallel = design$parallel,
    fitList = design$fitList,
    meanfitList = design$meanfitList,
    sigma = design$sigma,
    tau_p = design$tau_p,
    tau_v = design$tau_v,
    mu = design$mu,
    simList = design$simList,
    seedInit = design$seed,
    seedList = design$seedList,
    simfitList = design$simfitList,
    akdeList = design$akdeList,
    ctsdList = design$ctsdList)))
  
}


#' @title Merge multiple simulation outputs
#'
#' @description
#' Pools two or more [md_run()] outputs into a single
#' `movedesign_processed` object by concatenating all simulated
#' individuals, fitted models, and seeds. The merged object behaves
#' exactly as if all individuals had been simulated in one call:
#' if each input contains 5 individuals, the merged output contains
#' 10.
#' 
#' The distinction from [md_stack()] is important. `md_merge()`
#' treats all inputs as parts of one larger dataset; replicate
#' identity is lost and individual counts accumulate. [md_stack()]
#' instead assigns a replicate ID to each [md_run()] output and
#' aggregates population-level inference across them, keeping each
#' run as a separate replicate.
#'
#' Call `md_merge()` directly only when you have run [md_run()]
#' separately and need to pool the raw outputs before downstream
#' analyses.
#' 
#' @param x Either a list of `movedesign_processed` objects, or the
#'   first of multiple objects passed individually. All objects must
#'   share the same `set_target` and sampling parameters.
#'   
#' @param ... Reserved for internal use.
#' 
#' @details
#' Before merging, all inputs are checked for consistent metadata
#' (e.g. `dur`, `dti`, `set_target`). Movement timescale parameters
#' (`tau_p`, `tau_v`) are compared after rounding to one decimal
#' place, to tolerate minor numerical differences arising from
#' separate model fitting runs. If any field mismatches are found,
#' the function stops with an informative message listing the
#' affected fields.
#' 
#' @return
#' A single `movedesign_output` object that contains all merged
#' simulation outputs and inherits metadata from the first input
#' object.
#' 
#' @examples
#' if (interactive()) {
#' 
#'   data(buffalo)
#'   input <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 1, unit = "month"),
#'     dti = list(value = 1, unit = "day"),
#'     add_individual_variation = FALSE,
#'     set_target = "hr",
#'     which_meta = "mean")
#'
#'   output1 <- md_run(input)
#'   output2 <- md_run(input)
#'
#'   # Both of the following are equivalent:
#'
#'   md_merge(output1, output2)
#'   md_merge(list(output1, output2))
#'   
#' }
#'
#' @seealso
#'   [md_prepare],
#'   [md_run]
#'
#' @export
md_merge <- function(x, ...) {
  
  dots <- list(...)
  .ignore_mismatch <- dots[[".ignore_mismatch"]] %||% FALSE
  extra <- if (is.null(names(dots))) {
    dots } else {  dots[is.na(names(dots)) | names(dots) == ""] }
  
  outs <- if (
    is.list(x) &&
    length(x) > 0L &&
    inherits(x[[1L]], c("movedesign_processed",
                        "movedesign_output"))) {
    x
  } else {
    c(list(x), extra)
  }
  
  if (length(outs) == 1 &&
      is.list(outs[[1]]) &&
      all(sapply(outs[[1]], is.list))) {
    outs <- outs[[1]]
  }
  
  data_type <- outs[[1]]$data_type
  
  class_list <- vapply(outs, function(x)
    if (is.list(x) && length(class(x)) > 0)
      class(x)[1] else NA_character_, character(1))
  
  allowed_classes <- c("movedesign_processed",
                       "movedesign_output")
  if (!all(class_list %in% allowed_classes)) {
    stop(
      "All inputs to md_merge() must be of class ",
      "'movedesign_processed' or 'movedesign_output'.\n",
      "Offending element(s): ",
      paste(which(!class_list %in% allowed_classes),
            collapse = ", "))
  }
  
  if (!all(sapply(outs, is.list))) {
    stop("All elements must be outputs from 'movedesign' functions.")
  }
  
  required_fields <- c("simList", "simfitList", "seedList")
  for (f in required_fields) {
    if (!all(sapply(outs, function(x) f %in% names(x)))) {
      stop(sprintf("Missing field '%s' in one or more inputs.", f))
    }
  }
  
  set_target_all <- unique(unlist(lapply(outs, `[[`, "set_target")))
  if (!all(set_target_all %in% c("hr", "ctsd", "none"))) {
    stop("Unknown target(s) in `set_target`.")
  }
  
  # Pull shared metadata from first entry:
  metadata_fields <- c(
    "data", "data_type",
    "get_species", "n_individuals",
    "dur", "dti", "add_ind_var",
    "grouped", "groups",
    "set_target", "which_meta", "parallel",
    "sigma", "tau_p", "tau_v", "mu"
  )
  
  # Helper function to compare metadata element-wise:
  find_mismatches <- function(metadatas) {
    ref <- metadatas[[1]]
    n <- length(metadatas)
    mismatches <- vector("list", length(metadata_fields))
    names(mismatches) <- metadata_fields
    
    for (field in metadata_fields) {
      vals <- lapply(metadatas, `[[`, field)
      
      if (field %in% c("tau_p", "tau_v")) {
        ref_val <- round(ref[[field]]$All$value, 1)
        
        if (data_type != "simulated") {
          identicals <- vapply(vals[-1], function(v) {
            if (is.null(v)) return(FALSE)
            all(round(v$All$value, 1) == ref_val)
          }, logical(1))
        } else {
          identicals <- vapply(vals[-1], function(v) {
            if (is.null(v)) return(FALSE)
            all(round(v$All$value, 1) == ref_val, na.rm = TRUE)
          }, logical(1))
          
        }
       
      } else {
        identicals <- vapply(vals[-1], function(v)
          identical(v, vals[[1]]), logical(1))
      }
      
      if (!all(identicals)) {
        mismatches[[field]] <- vals
        if (field %in% c("tau_p", "tau_v")) {
          stop(sprintf(
            "Metadata field '%s' does not match at precision 0.1", 
            field))
        }
      } else {
        mismatches[[field]] <- NULL
      }
    }
    
    Filter(Negate(is.null), mismatches)
  }
  
  # Extract metadata:
  metadatas <- lapply(outs, function(x) x[metadata_fields])
  
  # Check and stop if mismatches exist:
  mismatched_elements <- NULL
  if (!.ignore_mismatch) {
    mismatched_elements <- find_mismatches(metadatas)
  }
  
  if (length(mismatched_elements) > 0 && !.ignore_mismatch) {
    mismatched_names <- names(mismatched_elements)
    stop(
      "Metadata mismatch across simulation outputs. Merge aborted.\n",
      "Mismatched fields: ",
      paste(mismatched_names, collapse = ", "), "\n",
      "Inspect elements in `mismatched_elements` for more detail."
    )
  }
  
  # Helper to merge and sequentially rename list elements:
  .merge_named_lists <- function(outs, field) {
    lists <- lapply(outs, `[[`, field)
    merged <- do.call(c, lists)
    
    names(merged) <- unlist(Map(function(x, i) {
      nm <- names(x)
      n <- length(x)
      
      if (is.null(nm) || all(nm == "") || all(is.na(nm))) {
        paste0(seq_len(n))
      } else {
        nm[is.na(nm) | nm == ""] <- seq_len(n)[is.na(nm) | nm == ""]
        paste0(nm)
      }
    }, lists, seq_along(lists)))
    
    merged
  }
  
  # Merge core simulation outputs
  combined_simList <- .merge_named_lists(outs, "simList")
  combined_seedList <- .merge_named_lists(outs, "seedList")
  combined_simfitList <- .merge_named_lists(outs, "simfitList")
  
  # Merge optional outputs
  combined_akdeList <- if (
    "hr" %in% set_target_all && 
    all(sapply(outs, function(x) "akdeList" %in% names(x)))) {
    .merge_named_lists(outs, "akdeList")
  } else NULL
  
  combined_ctsdList <- if (
    "ctsd" %in% set_target_all &&
    all(sapply(outs, function(x) "ctsdList" %in% names(x)))) {
    .merge_named_lists(outs, "ctsdList")
  } else NULL
  
  meta <- outs[[1]][metadata_fields]
  meta$meanfitList <- outs[[1]]$meanfitList
  meta$n_individuals <- length(combined_simList)
  out <- c(meta, list(
    simList = combined_simList,
    simfitList = combined_simfitList,
    seedList = combined_seedList,
    akdeList = combined_akdeList,
    ctsdList = combined_ctsdList
  ))
  
  out[sapply(out, is.null)] <- NULL
  class(out) <- "movedesign_processed"
  
  return(out)
}


#' @title Stack simulation outputs as replicates
#'
#' @description
#' Assigns a replicate ID to each [md_run()] output, re-runs
#' population-level resampling for each, and aggregates inference
#' results into a unified output. Calling `md_stack()` on
#' a list of `n` [md_run()] outputs produces the same result as
#' calling [md_replicate()] with `n_replicates = n`.
#' 
#' Use this function when the [md_run()] calls have already been
#' made — for example, when runs were executed in parallel outside
#' the standard workflow, or recovered after an interruption.
#' 
#' The distinction from [md_merge()] is important. [md_merge()]
#' pools all inputs into one larger dataset (*e.g.*, if each run has
#' `5` individuals, the output has `10`, and the design corresponds to
#' a single replicate.
#' `md_stack()` assigns each run a separate replicate ID:
#' number of individuals does not accumulate, and population-level
#' inference is aggregated across replicates.
#' 
#' @param obj A list of `movedesign_processed` objects, each
#'   returned by [md_run()]. All objects must share the same
#'   `set_target`, `dur`, and `dti`.
#'   
#' @param error_threshold Numeric. The acceptable error
#'   threshold used when summarising estimation performance across
#'   replicates (e.g. `0.05` for 5%).
#'   
#' @param ... Reserved for internal use.
#' 
#' @return
#' A list of class `movedesign_output`.
#' 
#' @export
md_stack <- function(obj, error_threshold = 0.05, ...) {
  
  dots <- list(...)
  
  if (!is.list(obj)) {
    stop("Object must be a list.")
  }
  
  if (is.null(dots[[".ignore_mismatch"]])) {
    .ignore_mismatch <- FALSE
  } else {
    .ignore_mismatch <- dots[[".ignore_mismatch"]]
  }
  
  n_replicates <- length(obj)
  
  outputs <- list()
  for (i in seq_along(obj)) {
    
    outputs[[i]] <- run_meta_resamples(
      obj[[i]], set_target = obj[[i]]$set_target,
      iter_step = 1,
      subpop = obj[[i]]$grouped,
      randomize = FALSE, 
      trace = FALSE,
      .automate_seq = FALSE)
    outputs[[i]]$sample <- i
    
  }
  
  if (length(outputs) > 0) {
    summary <- data.table::rbindlist(
      outputs, fill = TRUE, idcol = "replicate")
  } else {
    summary <- data.table::data.table()
  }
  
  if (length(obj) > 0) {
    
    if (obj[[1]]$grouped) {
      group_keys <- c("A", "B")
      common_names <- obj[[1]]$groups[[1]]
      merged_ids <- lapply(obj, function(x) x$groups[[2]])
      merged_ids <- Reduce(
        function(x, y) Map(c, x, y), merged_ids)
      
      for (x in seq_along(obj)) {
        obj[[x]]$groups <- list(common_names, merged_ids)
      }
    }
    
    merged <- md_merge(obj, .ignore_mismatch = .ignore_mismatch)
    class(merged) <- unique(c("movedesign_output", class(merged)))
  } else {
    merged <- NULL
  }
  
  merged$n_replicates <- n_replicates
  
  out <- structure(
    list(data = merged,
         summary = summary,
         error_threshold = error_threshold), class = "movedesign")
  class(out) <- unique(c("movedesign_output", class(out)))
  return(out)
  
}


#' @title Replicate study design workflow and aggregate outputs
#' 
#' @description
#' Runs the full `movedesign` workflow multiple times and aggregates
#' outputs across independent replicates. Use this function after
#' [md_run()] to quantify how stochasticity and design choices affect
#' estimation performance, and to produce the robust, replicated
#' results needed for a reliable design evaluation. Use [md_check()]
#' afterwards to assess whether enough replicates have been run for
#' stable inference.
#' 
#' Can also extend a previous run of [md_replicate()]: passing an
#' existing `movedesign_output` object appends new replicates to the
#' existing outputs rather than starting over.
#' 
#' @param obj An object of class `movedesign_input`, as returned by
#'   [md_prepare()] or [md_simulate()], or a `movedesign_output`
#'   object from a previous call of this function. Passing a
#'   `movedesign_output` appends `n_replicates` to
#'   the existing results.
#'   
#' @param n_replicates A single positive integer. The number of
#'   independent replicates to run. Must be at least `5`. Start with
#'   a modest number (*e.g.*, `20`), then use [md_check()] to assess
#'   convergence. If convergence has not been reached, pass the
#'   output back to this function to append more replicates.
#'   
#' @param verbose Logical. If `TRUE` (default), evaluates
#'   population-level inference at every *population* sample size
#'   up to `n_individuals`, saving results at each step. This shows how
#'   estimation performance changes as sample size grows. If `FALSE`,
#'   inference is run only once at the maximum sample size defined
#'   by `n_individuals` in [`md_prepare()`].
#'   
#' @param trace Logical. If `TRUE` (default), prints progress and
#'   timing messages to the console for each replicate. Set to `FALSE`
#'   for silent execution.
#'   
#' @param parallel Logical. If `TRUE`, runs replicates in parallel.
#'   Defaults to `FALSE`. Not supported on Windows, where execution
#'   falls back to sequential automatically.
#'   
#' @param error_threshold Numeric. The acceptable error
#'   threshold used when summarising estimation performance across
#'   replicates (e.g. `0.05` for 5%).
#'   
#' @param ncores Integer. Number of CPU cores to use when
#'   `parallel = TRUE`. Defaults to all available cores via
#'   [parallel::detectCores()]. Ignored when `parallel = FALSE` or
#'   on Windows.
#'   
#' @param ... Reserved for internal use.
#'
#' @return
#' An object of class `movedesign_output`, accepted by [md_check()],
#' [md_plot()], and [md_plot_replicates()].
#'
#' @details
#' Each replicate calls [md_run()] with a unique random seed,
#' ensuring results are statistically independent. If the function is
#' interrupted, it returns all results completed up to that point
#' rather than discarding them. This makes it safe to stop a long run
#' early and still retrieve partial results.
#' 
#' ## Parallel processing
#' 
#' Setting `parallel = TRUE` can substantially reduce runtime for
#' large replication runs. Parallelisation relies on
#' [parallel::mclapply()] and is not available on Windows; in that
#' case, execution falls back to sequential with no error.
#' 
#' ## Appending replicates
#' 
#' Passing a `movedesign_output` object as `obj` adds new
#' replicates to the existing results. This is useful when an
#' initial run needs more replicates for stable inference without
#' discarding completed work.
#' 
#' ## Assessing convergence
#'
#' There is no universal rule for how many replicates are sufficient.
#' After an initial run, use [md_check()] to evaluate whether the
#' cumulative mean of the tracked error metric has stabilised across
#' replicates. If convergence has not been reached, pass the returned
#' `movedesign_output` object back to [md_replicate()] to append
#' more replicates without discarding completed work. Repeat until
#' [md_check()] confirms convergence.
#'
#' @seealso
#'   [md_prepare()] and [md_simulate()] to build the input object.
#'   [md_run()] for a single exploratory run before committing to
#'   full replication.
#'   [md_check()] to assess whether cumulative estimation error has
#'   stabilised across replicates (the recommended criterion for
#'   deciding when enough replicates have been run).
#'   [md_plot()] and [md_plot_replicates()] to visualize outputs.
#' 
#' @examples
#' if (interactive()) {
#' 
#'   data(buffalo)
#'   input <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 1, unit = "month"),
#'     dti = list(value = 1, unit = "day"),
#'     add_individual_variation = TRUE,
#'     grouped = FALSE,
#'     set_target = "hr",
#'     which_meta = "mean")
#'   
#'   output <- md_replicate(input, n_replicates = 5)
#'   md_check(output)
#'   
#'   # Append more replicates to an existing result:
#'   output <- md_replicate(output, n_replicates = 10)
#' }
#' 
#' @family workflow_steps
#' @export
md_replicate <- function(obj,
                         n_replicates,
                         verbose = TRUE,
                         trace = TRUE,
                         parallel = FALSE,
                         error_threshold = 0.05,
                         ncores = parallel::detectCores(),
                         ...) {
  
  stopifnot(is.numeric(n_replicates) && n_replicates > 0)
  
  is_input <- inherits(obj, "movedesign_input")
  is_output <- inherits(obj, "movedesign_output")
  
  dots <- list(...)
  .override <- dots[[".override"]] %||% FALSE
  
  if (!is_input && !is_output) {
    stop("`obj` must be a `movedesign_input` ",
         "or `movedesign_output` object.")
  }
  
  if (is_output) {
    base_input <- obj$input
    existing_out <- obj$data
    existing_meta <- obj$summary
    offset <- obj$data$n_replicates
    
  } else {
    if (!.override && n_replicates < 5)
      stop("`n_replicates` must be set to at least 5.")
    
    base_input <- obj
    existing_out <- NULL
    existing_meta <- NULL
    offset <- 0
  }
  
  .worker <- function(i) {
    rep_id <- offset + i
    
    if (trace) writeLines(paste0(
      crayon::yellow("  \u2015\u2015\u2015"),
      " Replicate ", crayon::yellow(rep_id), " out of ",
      crayon::yellow(offset + n_replicates)))
    
    out <- md_run(base_input, trace = trace)
    
    set_m <- if (verbose) NULL else base_input$n_individuals
    meta <- run_meta_resamples(out,
                               set_target = base_input$set_target,
                               subpop = base_input$grouped,
                               .m = set_m)
    
    return(list(out = out, meta = meta))
  }
  
  outList <- vector("list", n_replicates)
  metaList <- vector("list", n_replicates)
  completed <- 0
  
  start_total <- Sys.time()
  message("Execution started: ", 
          crayon::yellow(crayon::bold(
            format(start_total, "%Y-%m-%d %H:%M:%S %Z"))))
  
  tryCatch({
    
    sysname <- Sys.info()[["sysname"]]
    if (sysname != "Windows" && parallel) {
      
      tmp <- parallel::mclapply(
        seq_len(n_replicates), 
        .worker, mc.cores = ncores)
      
      for (i in seq_len(n_replicates)) {
        outList[[i]] <- tmp[[i]]$out
        metaList[[i]] <- tmp[[i]]$meta
        completed <- i
      }
      
    } else {
      
      if (!trace)
        pb <- txtProgressBar(min = 0, max = n_replicates, style = 3)
      
      for (i in seq_len(n_replicates)) {
        out <- .worker(i)
        outList[[i]] <- out$out
        metaList[[i]] <- out$meta
        completed <- i
        if (!trace) setTxtProgressBar(pb, i)
      }
      
      if (!trace) close(pb)
      
    }
    
    TRUE
    
  }, interrupt = function(e) {
    
    if (!is_output) {
      message(.msg(sprintf(
        "\nUser interrupt detected. Returning %s out of %s results.",
        completed, n_replicates), "error"))
    } else {
      message(.msg(sprintf(
        "User interrupt detected. Returning %s new replicates.",
        completed), "error"))
    }
    
    FALSE
    
  })
  
  # Remove NULL elements (if interrupted):
  outList <- outList[seq_len(completed)]
  metaList <- metaList[seq_len(completed)]
  
  if (length(metaList) > 0) {
    new_meta <- data.table::rbindlist(
      metaList, fill = TRUE, idcol = "replicate")
  } else {
    new_meta <- data.table::data.table()
  }
  
  if (!is.null(existing_meta)) {
    new_meta$replicate <- new_meta$replicate +
      max(existing_meta$replicate)
    summary <- data.table::rbindlist(
      list(existing_meta, new_meta), fill = TRUE)
  } else {
    summary <- new_meta
  }
  
  if (length(outList) > 0) {
    
    if (base_input$grouped) {
      
      group_keys <- c("A", "B")
      init_names <- outList[[1]]$groups[[1]]
      merged_ids <- lapply(outList, function(x) x$groups[[2]])
      merged_ids <- Reduce(
        function(x, y) Map(c, x, y), merged_ids)
      
      for (x in seq_along(outList)) {
        outList[[x]]$groups <- list(init_names, merged_ids)
      }
    }
    
    if (!is.null(existing_out)) {
      merged <- md_merge(c(list(existing_out), outList),
                         .ignore_mismatch = TRUE)
    } else {
      merged <- md_merge(outList)
    }
    
    class(merged) <- unique(c("movedesign_output", class(merged)))
    
  } else {
    
    merged <- NULL
    
  }
  
  if (trace) {
    
    message(.msg(sprintf(
      "Replication completed! %s replicates merged.", 
      completed), "success"))
    
    if (completed < n_replicates) {
      message(.msg(sprintf(
        "Warning: Only %s out of %s replicates were run.", 
        completed, n_replicates), "warning"))
    }
    
    message(.msg(
      sprintf("Total number of simulations: %s", 
              if (!is.null(merged))
                length(merged$simList) else 0), "success"))
  }
  
  if (!is.null(merged))
    merged$n_replicates <- offset + completed
  
  message("\nExecution ended: ",
          crayon::yellow(crayon::bold(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))))
  
  .msg_time(start_total, " Elapsed time: ")
  
  out <- structure(
    list(input = base_input,
         data = merged,
         summary = summary,
         error_threshold = error_threshold,
         verbose = verbose), class = "movedesign")
  class(out) <- unique(c("movedesign_output", class(out)))
  return(out)
}


#' @title Preview plot for movedesign workflow outputs (single replicate)
#'
#' @description
#' Generates a quick visualization of relative error for home range or
#' movement speed estimation from a single replicate of a movedesign
#' workflow.
#' The plot can display either the estimates from that replicate for a
#' random combination of individuals, or, when resampling is enabled,
#' summaries derived from repeated draws of individuals at each population
#' sample size (based on the specified number of resamples).
#' 
#' This functions shows preliminary outputs for a single stochastic run
#' from [md_run()] (a `movedesign_processed` object) and should not be
#' used to evaluate study design by itself. Instead, users should run
#' [md_replicate()] and check for convergence with [md_check()].
#' 
#' @param obj An object of class `movedesign_processed`, as returned
#'   by [md_run()].
#'   
#' @param n_resamples A single positive integer. The number of
#'   random combinations of individuals generated at each population
#'   sample size. Each combination produces one population-level
#'   estimate. Set to `NULL` to plot raw estimates without
#'   resampling.
#'   
#' @param error_threshold Numeric. Relative error threshold shown as
#'   a reference line in the plot (e.g. `0.05` for 5%).
#'   
#' @param pal A character vector of two colours, used for estimates
#'   within and outside the error threshold respectively.
#'   Defaults to `c("#007d80", "#A12C3B")`.
#'   
#' @param ... Reserved for internal use.
#' 
#' @return
#' A `ggplot` object. Displays relative error as a function of
#' population sample size, with point estimates, confidence
#' intervals, and a horizontal reference line at `error_threshold`.
#' 
#' @details
#' This plot summarizes a single replicate, so it is subject to
#' stochastic variation. The plot shown here may look very
#' different with another run of the same design. Use [md_replicate()]
#' to aggregate results across many independent runs, and [md_check()]
#' to confirm that estimates have stabilised before drawing conclusions.
#' 
#' @examples
#' if (interactive()) {
#'   
#'   data(buffalo)
#'   input <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 1, unit = "month"),
#'     dti = list(value = 1, unit = "day"),
#'     add_individual_variation = FALSE,
#'     grouped = TRUE,
#'     set_target = "hr",
#'     which_meta = "mean")
#'   
#'   output <- md_run(input)
#'   md_plot_preview(output, error_threshold = 0.05)
#' }
#' 
#' @seealso
#'   [md_run()] to generate the input object.
#'   [md_replicate()] for robust outputs based on multiple replicates.
#'   [md_check()] to assess convergence across replicates.
#'   
#' @export
md_plot_preview <- function(obj,
                            n_resamples = NULL,
                            error_threshold = 0.05,
                            pal = c("#007d80", "#A12C3B"),
                            ...) {
  
  dots <- list(...)
  .seed <- dots[[".seed"]] %||% NULL
  resampled <- ifelse(is.null(n_resamples), FALSE, TRUE)
  
  n <- x <- y <- id <- NULL
  type <- group <- error <- error_sd <- NULL
  
  if (!inherits(obj, "movedesign_processed")) {
    stop(paste("Input must be a 'movedesign_processed'",
               "object from `md_run()`."))
  }
  
  if ("ctsd" %in% obj$set_target) {
    iter_step <- ifelse(
      sum(!.check_for_inf_speed(obj$ctsdList)) <= 16, 2, 4)
  } else {
    iter_step <- ifelse(length(obj$simList) <= 16, 2, 4)
  }
  
  # Run meta-analyses:
  
  if (resampled) {
    out <- run_meta_resamples(obj,
                              set_target = obj$set_target,
                              subpop = obj$grouped,
                              randomize = resampled, 
                              max_draws = n_resamples,
                              trace = TRUE,
                              .automate_seq = TRUE,
                              .seed = .seed)
  } else {
    out <- run_meta(obj, 
                    set_target = obj$set_target,
                    subpop = obj$grouped, 
                    iter_step = iter_step,
                    trace = TRUE,
                    .seed = .seed)
  }
  
  out <- out %>%
    dplyr::mutate(
      dplyr::across(
        "type", ~factor(., levels = c("hr", "ctsd")))) %>% 
    dplyr::mutate(
      overlaps = factor(
        abs(.data$error) <= error_threshold,
        levels = c(TRUE, FALSE)))
  
  max_draws <- max(unique(out$sample))
  
  max_m <- max(unique(out$m))
  only_one_m <- ifelse(length(unique(out$m)) == 1, TRUE, FALSE)
  if (only_one_m) { 
    out$m <- factor(out$m)
    warning(paste0(
      "Only ", .msg("one", "danger"),
      " unique value of `m` detected per group.\n",
      "No resampling possible for this design.\n"),
      call. = FALSE)
  }
  
  dti <- fix_unit(obj$dti$value, obj$dti$unit)
  txt_tdi <- ifelse(dti$value == 1,
                    dti$unit, paste(dti$value, dti$unit))
  
  set_subtitle <- paste0(
    max_m, " tags, tracked for ",
    paste(obj$dur$value, obj$dur$unit), " every ",
    txt_tdi)
  
  grouped <- unique(out$is_grouped)
  set_target <- unique(out$type)
  
  label_threshold <- sprintf("%s%%", error_threshold * 100)
  labels_target <- c(
    "hr" = "Home range area estimation",
    "ctsd" = "Speed & distance estimation")
  
  if (resampled) {
    
    max_draws <- max(unique(out$sample))
    if (grouped) out <- dplyr::filter(out, group != "All")
    
    out_mean <- .summarize_error(out, error_threshold = error_threshold)
    label_ci <- sprintf("%g%%", unique(out_mean$ci) * 100)
    
    m_resampled <- out_mean$m[out_mean$n > 1L]
    
    p <- out_mean %>%
      ggplot2::ggplot(
        ggplot2::aes(x = .data$m,
                     y = .data$error,
                     group = .data$group,
                     shape = .data$group,
                     color = .data$overlaps)) +
      
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.3,
        linetype = "solid") +
      ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = -error_threshold,
        ymax = error_threshold,
        alpha = 0.05) +
      ggplot2::geom_hline(
        yintercept = c(-error_threshold, error_threshold),
        linetype = "dashed",
        linewidth = 0.35, alpha = 0.8) +
      ggplot2::annotate(
        "text",
        x = Inf, y = error_threshold,
        label = paste0("+", label_threshold),
        hjust = 1.08, vjust = -0.5,
        size = 3.2) +
      ggplot2::annotate(
        "text",
        x = Inf, y = -error_threshold,
        label = paste0("\u2212", label_threshold),
        hjust = 1.08, vjust = 1.5,
        size = 3.2) +
      
      { if (!only_one_m)
        ggplot2::geom_jitter(
          data = dplyr::filter(out, .data$m %in% m_resampled),
          mapping = ggplot2::aes(x = .data$m,
                                 y = .data$error,
                                 group = .data$group,
                                 shape = .data$group,
                                 color = .data$overlaps),
          position = ggplot2::position_jitterdodge(
            dodge.width = 0.4),
          color = "grey80", size = 3, alpha = 0.9) } +
      
      # Prediction intervals:
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$pred_lci,
                     ymax = .data$pred_uci),
        position = ggplot2::position_dodge(width = 0.5),
        linewidth = 3,
        alpha = 0.18,
        show.legend = FALSE) +
      
      # Confidence intervals:
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$error_lci,
                     ymax = .data$error_uci),
        position = ggplot2::position_dodge(width = 0.5),
        linewidth = 1.1,
        alpha = 0.55,
        show.legend = FALSE) +
      
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.5),
        size = 4,
        show.legend = TRUE) +
      
      { if (length(set_target) > 1)
        ggplot2::facet_wrap(
          . ~ .data$type, scales = "free_y",
          labeller = ggplot2::labeller(type = labels_target))
      } +
      
      ggplot2::labs(
        title = "Design",
        subtitle = set_subtitle,
        x = "Population sample size",
        y = "Relative error (%)",
        color = sprintf(
          "Within error threshold (\u00B1%g%%)?",
          error_threshold * 100),
        caption = sprintf(
          paste0(
            "Wide bars: %s prediction interval  \u2022  ",
            "Narrow bars: %s confidence interval\n",
            "Based on %d resamples"),
          label_ci, label_ci, max_draws)) +
      
      { if (length(unique(out$m)) == 2) {
        ggplot2::scale_x_continuous(
          breaks = unique(out$m))
      } else if (!only_one_m) {
        ggplot2::scale_x_continuous(
          breaks = scales::breaks_pretty())
      }
      } +
      
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        breaks = scales::breaks_pretty(),
        expand = ggplot2::expansion(mult = c(0.05, 0.10))) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = pal[1], "FALSE" = pal[2]),
        # labels = c("TRUE" = "Yes", "FALSE" = "No"),
        drop = FALSE) +
      
      { if (grouped)
        ggplot2::scale_shape_manual(
          "Groups:", values = c(16, 17)) } +
      
      { if (grouped)
        ggplot2::guides(
          color = ggplot2::guide_legend(
            override.aes = list(
              shape = 22, size = 4,
              fill = c(pal[1], pal[2])))) } +
      
      .theme_movedesign_report()
    
    if (!grouped) p <- p + ggplot2::guides(shape = "none")
    
  } else {
    
    if (grouped) out <- dplyr::filter(out, group != "All")
    
    label_ci <- "95%"
    
    p <- out %>%
      ggplot2::ggplot(
        ggplot2::aes(x = .data$m,
                     y = .data$error,
                     shape = .data$group,
                     group = .data$group,
                     color = .data$overlaps)) +
      
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.3,
        linetype = "solid") +
      ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = -error_threshold,
        ymax = error_threshold,
        alpha = 0.05) +
      ggplot2::geom_hline(
        yintercept = c(-error_threshold, error_threshold),
        linetype = "dashed",
        linewidth = 0.35, alpha = 0.8) +
      ggplot2::annotate(
        "text",
        x = Inf, y = error_threshold,
        label = paste0("+", label_threshold),
        hjust = 1.08, vjust = -0.5,
        size = 3.2) +
      ggplot2::annotate(
        "text",
        x = Inf, y = -error_threshold,
        label = paste0("\u2212", label_threshold),
        hjust = 1.08, vjust = 1.5,
        size = 3.2) +
      
      # Confidence intervals:
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$error_lci,
                     ymax = .data$error_uci),
        position = if (grouped)
          ggplot2::position_dodge(width = 0.5) else 
            ggplot2::position_identity(),
        linewidth = 1.1,
        alpha = 0.55,
        show.legend = FALSE) +
      
      ggplot2::geom_point(
        position = if (grouped)
          ggplot2::position_dodge(width = 0.5) else 
            ggplot2::position_identity(),
        size = 4,
        show.legend = TRUE) +
      
      { if (length(set_target) > 1)
        ggplot2::facet_wrap(
          . ~ .data$type, scales = "free_y",
          labeller = ggplot2::labeller(type = labels_target))
      } +
      
      ggplot2::labs(
        title = "Design",
        subtitle = set_subtitle,
        x = "Population sample size",
        y = "Relative error (%)",
        color = paste0("Within error threshold (\u00B1",
                       error_threshold * 100, "%)?"),
        caption = sprintf(
          "Narrow bars: %s confidence interval", label_ci)) +
      
      { if (length(unique(out$m)) == 2) {
        ggplot2::scale_x_continuous(
          breaks = unique(out$m),
          expand = ggplot2::expansion(mult = c(0.10, 0.10)))
      } else if (!only_one_m) {
        ggplot2::scale_x_continuous(
          breaks = scales::breaks_pretty(),
          expand = ggplot2::expansion(mult = c(0.10, 0.10)))
      }
      } +
      
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        breaks = scales::breaks_pretty(),
        expand = ggplot2::expansion(mult = c(0.10, 0.10))) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = pal[1], "FALSE" = pal[2]),
        drop = FALSE) +
      
      { if (grouped)
        ggplot2::scale_shape_manual(
          "Groups:", values = c(16, 17),
        ) } +
      
      { if (grouped)
        ggplot2::guides(
          color = ggplot2::guide_legend(
            override.aes = list(
              shape = 22, size = 4,
              fill = c(pal[1], pal[2])))) } +
      { if (grouped)
        ggplot2::guides(
          shape = ggplot2::guide_legend(
            override.aes = list(size = 4))) } +
      
      .theme_movedesign_report()
    
    if (!grouped) p <- p + ggplot2::guides(shape = "none")
    
  }
  
  if (resampled && obj$n_individuals < 15) {
    warning(
      crayon::bold("! PRELIMINARY RESULTS ONLY\n"),
      " This plot displays outputs from ",
      crayon::yellow(paste0(
        "1 replicate + ",
        length(unique(out$sample)), " resample(s)")),
      ".\n Results may change substantially",
      " with additional replicates or resamples.\n\n",
      crayon::bold("  Run `md_replicate()`"),
      crayon::bold(" for more robust inferences.\n"),
      call. = FALSE)
    
  } else if (!resampled && obj$n_individuals < 15) {
    warning(
      crayon::bold("! PRELIMINARY RESULTS ONLY\n"),
      " This plot displays outputs from ",
      crayon::yellow("a single replicate"),
      " and no resampling.\n",
      "  Results may change substantially",
      " with additional replicates or resamples.\n\n",
      crayon::bold("  Run `md_replicate()`"),
      crayon::bold(" for more robust inferences.\n"),
      call. = FALSE)
  } else if (!resampled && obj$n_individuals >= 15) {
    warning(
      crayon::bold("! PRELIMINARY RESULTS ONLY\n"),
      " This plot displays outputs with no ",
      crayon::yellow("resampling"), ".\n",
      crayon::bold("  Run `md_plot_preview()`"),
      crayon::bold(" with resampling.\n"),
      call. = FALSE)
  }
  
  return(suppressWarnings(print(p)))
  
}


#' @title Visualize study design outputs
#'
#' @description
#' Produces a publication-ready density plot showing the distribution of
#' relative error estimates from study design simulations. The plot
#' highlights the mean and a shaded credible interval (CI) region,
#' following the computation of credible intervals as implemented in
#' `bayestestR::ci()`. If groups are present, density curves for each
#' group are overlaid for comparison, using customizable colors.
#'
#' This function is typically used after running [`md_replicate()`],
#' providing a visual diagnostic of simulation results.
#'
#' @param x A `movedesign_output` object, as returned by
#'   [`md_replicate()`]. The object must contain a
#'   `summary` data frame with, at a minimum, the following columns:
#'   \describe{
#'     \item{error}{Relative error values for each replicate.}
#'     \item{error_lci}{Lower credible interval bound for error.}
#'     \item{error_uci}{Upper credible interval bound for error.}
#'     \item{group}{(Optional) Group label for comparing densities.}
#'   }
#' @param stat Character string specifying which summary statistic to
#'   display. Must be `"mean"` or `"median"`. Defaults to `"mean"`.
#' @param ci Numeric scalar between 0 and 1. The probability of the
#'   credible interval (CI) to be estimated. Default to `0.80` (80%).
#' @param method Character. Credible interval estimation method (passed
#'   to `bayestestR::ci()`; default: `"HDI"`). See `?bayestestR::ci()`
#'   for more details.
#' @param pal Character vector of color(s) for the density, CI shading,
#'   and mean line. If a single group, supply one color (default:
#'   `"#007d80"`). If groups are present, supply two colors (default:
#'   `c("#007d80", "#A12C3B")`).
#' @param m Numeric (Optional). If provided, restricts the results to a
#'   specific population sample size (`m`). Defaults to `NULL`, which
#'   checks up to the maximum population sample size.
#' @param show_text Logical, whether to display text annotations in
#'   the plots. Default is `TRUE`.
#' @param ... Reserved for internal use.
#'
#' @return
#' A `ggplot` object showing:
#'   \itemize{
#'     \item Density curve(s) of the relative error distribution.
#'     \item Shaded region for the central credible interval.
#'     \item Vertical dashed lines at mean(s).
#'     \item Overlaid densities if multiple groups are present.
#'     \item Percent-formatted x-axis for interpretation.
#'   }
#'
#' This object can be further customized with additional `ggplot2`
#' layers if needed.
#'
#' @details
#' This plot helps users assess the reliability of simulation outputs
#' by visualizing the distribution of relative errors. When multiple
#' groups are simulated, the plot enables direct visual comparison
#' of performance across groups. If credible intervals cannot be
#' calculated, a warning is issued and only the density curves
#' are displayed.
#' 
#' **It is strongly recommended to use [`md_check()`] to assess whether
#' the distributions shown here have stabilized.** Checking for
#' convergence ensures that the summary statistics and uncertainty
#' estimates depicted in the plot are reliable and not unduly
#' influenced by too few replicates or ongoing variability.
#' Running [`md_check()`] helps you determine if additional simulation
#' replicates are needed to achieve stable inference in your design
#' evaluation.
#' 
#' @seealso
#' [`md_replicate()`],
#' [`md_check()`] for convergence diagnostics,
#' and refer to `bayestestR::ci()` for details on credible interval
#' computation and interpretation.
#'
#' @examples
#' if (interactive()) {
#'   input <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 1, unit = "month"),
#'     dti = list(value = 1, unit = "day"),
#'     add_individual_variation = TRUE,
#'     grouped = TRUE,
#'     set_target = "hr",
#'     which_meta = "mean"
#'   )
#'
#'   output <- md_replicate(input, n_replicates = 20)
#'
#'   # Plot with 80% credible intervals:
#'   md_plot(output, ci = 0.80, method = "HDI")
#' }
#'
#' @export
md_plot <- function(x,
                    stat = c("mean", "median"),
                    ci = 0.8,
                    method = "HDI",
                    pal = c("#007d80", "#A12C3B"),
                    m = NULL,
                    show_text = TRUE,
                    ...) {
  
  dots <- list(...)
  .verbose <- dots[[".verbose"]] %||% FALSE
  .override <- dots[[".override"]] %||% FALSE
  
  # Input validation:
  stat <- match.arg(stat)
  stat_text <- switch(stat,
                      mean = "x\u0305",
                      median = "x\u0303",
                      tools::toTitleCase(stat))
  
  warnings_list <- character(0L)
  
  if (!is.numeric(ci) || length(ci) != 1 || ci <= 0 || ci >= 1) 
    stop("`ci` must be a single numeric value strictly between ",
         " 0 and 1 (e.g. `ci = 0.80` for an 80% credible interval).")
  
  if (!(inherits(x, "movedesign_output") ||
        inherits(x, "movedesign_optimized"))) {
    if (!.override) stop("`x` must be 'movedesign_output' object.")
  } else {
    x <- list(x)
  }
  
  if (!is.null(m) && (!is.numeric(m) ||
                      length(m) != 1L ||
                      is.na(m) || m < 1L)) {
    stop("`m` must be a single positive integer ",
         "specifying a population sample ",
         "size, but received: ",
         paste(m, collapse = ", "), ".")
  }
  
  
  # Helper functions:
  
  .get_stat <- function(x) {
    switch(stat,
           mean = mean(x, na.rm = TRUE),
           median = median(x, na.rm = TRUE))
  }
  
  .safe_extract_cri <- function(x, method = method, ci = ci) {
    
    warnings_list <- character(0L)
    cri <- withCallingHandlers(
      suppressWarnings(
        .extract_cri(x, method = method, ci = ci)),
      warning = function(w) {
        warnings_list <<- unique(c(
          warnings_list, conditionMessage(w)))
        invokeRestart("muffleWarning") })
    
    if (anyNA(c(cri$lci, cri$uci))) {
      warnings_list <<- unique(c(
        warnings_list,
        "CI could not be computed (returned NA). ",
        "Consider reducing `ci` or increasing `n_replicates`."))
    }
    
    return(invisible(cri))
  }
  
  .validate_design <- function(input) {
    
    if ( !(inherits(input, "movedesign_output") ||
           inherits(input, "movedesign_optimized")) ||
        !("summary" %in% names(input))) {
      stop("Each element of `x` must be a `movedesign_output` ",
           "object produced by `md_replicate()`.")
    }
    
    data <- input$summary
    if (!is.data.frame(data)) {
      stop("`summary` slot must be a data frame, ",
           "but found: <", class(data)[[1L]], ">.")
    }
    
    required_cols <- c("error", "error_lci", "error_uci")
    missing_cols <- setdiff(required_cols, names(data))
    
    if (length(missing_cols) > 0L) {
      stop("`summary` is missing required column(s): ",
           paste(missing_cols, collapse = ", "), ".\n",
           "  - Expected: ", paste(required_cols,
                                   collapse = ", "), ".")
    }
    
    return(invisible(data))
  }
  
  .filter_m <- function(data, input) {
    
    max_m <- max(data$m, na.rm = TRUE)
    target_m <- if (!is.null(m)) m else max_m
    
    if (!is.null(m)) {
      if (!input$verbose) {
        stop("`md_replicate()` must be run with ",
             "`verbose = TRUE` to use the `m` argument.")
      }
      
      valid_m <- sort(unique(data$m))
      if (!target_m %in% valid_m) {
        stop(paste0("Population sample size '", m,
                    "' not found in design. Valid `m` values: ",
                    paste(sort(unique(data$m)), collapse = ", ")))
      }
    }
    return(data[data$m == target_m, , drop = FALSE])
  }
  
  .build_density <- function(d, tp, gr = "All", shaded = TRUE) {
    
    if (nrow(d) == 0) return(NULL)
    
    dens <- stats::density(d$error, na.rm = TRUE)
    df <- data.frame(x = dens$x,
                     y = dens$y,
                     type = tp,
                     group = gr,
                     stringsAsFactors = FALSE)
    
    if (!shaded) return(df)
    
    cri <- suppressMessages(
      .safe_extract_cri(d$error, method = method, ci = ci))
    if (anyNA(c(cri$lci, cri$uci))) return(NULL)
    
    df <- df[df$x >= cri$lci & df$x <= cri$uci, , drop = FALSE]
    if (nrow(df) == 0L) return(NULL)
    df$shaded <- TRUE
    
    return(df)
  }
  
  .build_text <- function(d, tp, gr = "All", x_range) {
    
    if (nrow(d) == 0L) return(NULL)
    
    cri <- suppressMessages(
      .safe_extract_cri(d$error, method = method, ci = ci))
    dens <- stats::density(d$error, na.rm = TRUE)
    stat_value <- .get_stat(d$error)
    
    local_y <- stats::approx(dens$x, dens$y, xout = stat_value)$y
    local_y <- if (is.na(local_y)) max(dens$y) else local_y
    
    x_adj <- diff(x_range) * 0.012
    direction <- ifelse(stat_value > mean(x_range), -x_adj, x_adj)
    
    data.frame(group = gr,
               type = tp,
               stat_value = stat_value,
               lci = cri$lci,
               uci = cri$uci,
               max_y = max(dens$y),
               min_y = min(dens$y),
               local_y = local_y,
               label_y = NA_real_,
               x_adjust = direction,
               hjust = NA_real_,
               stringsAsFactors = FALSE)
  }
  
  .resolve_label_positions <- function(text_data,
                                       pad = 0.08,
                                       x_tol = NULL,
                                       y_tol = NULL,
                                       y_step = 0.05) {
    
    if (is.null(text_data) || nrow(text_data) == 0L)
      return(text_data)
    
    n <- nrow(text_data)
    sv <- text_data$stat_value
    lci <- text_data$lci
    uci <- text_data$uci
    max_y <- text_data$max_y
    max_y_g <- max(max_y)
    
    # Tolerances
    x_range_vals <- range(c(sv, lci, uci), na.rm = TRUE)
    x_span <- diff(x_range_vals)
    if (is.null(x_tol)) x_tol <- max(x_span * 0.08, 0.02)
    if (is.null(y_tol)) y_tol <- max_y_g * 0.15
    
    # Label y anchor
    text_data$label_y <- max_y * (1 + pad)
    
    # x nudge and adjust
    x_mid <- mean(x_range_vals)
    x_nudge <- x_span * 0.012
    
    if (is.null(text_data$x_adjust))
      text_data$x_adjust <- NA_real_
    
    na_idx <- is.na(text_data$x_adjust)
    text_data$x_adjust[na_idx] <- ifelse(
      sv[na_idx] > x_mid, -x_nudge, x_nudge)
    text_data$hjust <- ifelse(text_data$x_adjust < 0, 1, 0)
    
    # Force horizontal separation for x-close pairs
    if (n >= 2L) {
      ord <- order(sv)
      text_data <- text_data[ord, ]
      sv <- text_data$stat_value
      
      for (i in seq_len(n - 1L)) {
        for (j in (i + 1L):n) {
          if (!is.na(sv[i]) && !is.na(sv[j]) && 
              abs(sv[i] - sv[j]) < x_tol) {
            text_data$x_adjust[i] <- -abs(x_nudge)
            text_data$x_adjust[j] <-  abs(x_nudge)
          }
        }
      }
    }
    
    # Vertical stagger for y-close pairs
    if (n >= 2L) {
      
      ord <- order(text_data$label_y)
      text_data <- text_data[ord, ]
      sv <- text_data$stat_value
      ly <- text_data$label_y
      
      for (i in seq_len(n - 1L)) {
        for (j in (i + 1L):n) {
          if (!is.na(sv[i]) && !is.na(sv[j]) &&
              abs(sv[i] - sv[j]) < x_tol &&
              abs(ly[j] - ly[i]) < y_tol) {
            ly[j] <- ly[i] + y_step * max_y_g
          }
        }
      }
      
      text_data$label_y <- ly
    }
    
    return(text_data)
  }
  
  .build_layers <- function(data, types, groups, FUN) {
    dplyr::bind_rows(
      lapply(groups, function(gr) {
        lapply(types, function(tp) {
          FUN(data[data$type == tp &
                     data$group == gr, ], tp, gr)
        })
      }))
  }
  
  # Preprocess designs:
  
  processed <- lapply(x, function(input) {
    
    data <- .validate_design(input)
    data <- .filter_m(data, input)
    data$type <- factor(data$type, levels = c("hr", "ctsd"))
    has_groups <- "group" %in% names(data) &&
      all(c("A", "B") %in% unique(as.character(data$group)))
    facet_by_type <- length(input$input$set_target) == 2L
    
    return(list(data = data,
                input = input,
                has_groups = has_groups,
                facet_by_type = facet_by_type))
  })
  
  global_x_range <- local({
    
    all_x <- unlist(lapply(processed, function(p) {
      
      data <- p$data
      types <- unique(data$type)
      groups <- if (p$has_groups) c("A", "B") else "All"
      
      unlist(lapply(groups, function(gr) {
        lapply(types, function(tp) {
          d <- data[data$type  == tp &
                      data$group == gr, , drop = FALSE]
          if (nrow(d) == 0L) return(NULL)
          return(stats::density(d$error, na.rm = TRUE)$x)
        })
      }))
      
    }))
    
    r <- range(all_x, na.rm = TRUE)
    if (!all(is.finite(r))) c(-Inf, Inf) else r
    
  })
  
  # Build plots:
  
  label_target <- c(
    hr = "Home range estimation",
    ctsd = "Speed \u0026 distance estimation")
  
  plots <- list()
  for (i in seq_along(processed)) {
    
    data <- processed[[i]]$data
    input <- processed[[i]]$input
    has_groups <- processed[[i]]$has_groups
    facet_by_type <- processed[[i]]$facet_by_type
    
    types <- unique(data$type)
    groups <- if (has_groups) c("A", "B") else "All"
    
    if (has_groups) {
      data <- data[data$group != "All", , drop = FALSE]
    } else {
      data <- data[data$group == "All", , drop = FALSE]
    }
    
    .apply_builder <- function(FUN) {
      dplyr::bind_rows(
        lapply(groups, function(gr) {
          lapply(types, function(tp) {
            FUN(data[data$type  == tp &
                       data$group == gr, , 
                     drop = FALSE], tp, gr) })
        }))
    }
    
    dens_full <- .apply_builder(
      function(d, tp, gr) 
        .build_density(d, tp, gr, shaded = FALSE))
    
    dens_shaded <- .apply_builder(
      function(d, tp, gr) 
        .build_density(d, tp, gr, shaded = TRUE))
    
    text_data <- .apply_builder(
      function(d, tp, gr) 
        .build_text(d, tp, gr, global_x_range))
    text_data <- .resolve_label_positions(text_data)
    
    if (length(processed) > 1) {
      set_title <- paste("Design", i)
    } else { set_title <- "Design" }
    
    n_tags <- max(data$m, na.rm = TRUE)
    set_n_tags <- if (has_groups) {
      paste0(n_tags * 2L, " tags (", n_tags, " per group)")
    } else {
      paste0(n_tags, " tags")
    }
    
    dti <- fix_unit(input$data$dti$value, input$data$dti$unit)
    txt_tdi <- ifelse(dti$value == 1,
                      dti$unit, paste(dti$value, dti$unit))
    
    set_subtitle <- paste0(
      set_n_tags, ", tracked for ",
      input$data$dur$value, " ", input$data$dur$unit,
      " every ", txt_tdi, "\n", input$data$n_replicates, " replicates")
    
    set_caption <- paste0(
      "Shaded region: ", as.integer(ci * 100L),
      "% credible interval; dotted line = ", stat)
    
    .stat_label <- function(row) {
      value <- scales::percent(row$stat_value, accuracy = 0.1)
      if (has_groups) {
        sprintf("%s = %s", row$group, value)
      } else {
        sprintf("%s", value)
      }
    }
    
    text_data$label <- vapply(
      seq_len(nrow(text_data)),
      function(i) .stat_label(text_data[i, ]),
      character(1L))
    
    # Assemble plot:
    
    glow_widths <- c(5.5, 3.2, 1.8, 0.65)
    glow_alphas <- c(0.04, 0.10, 0.22, 0.90)
    ci_area_alphas <- c(0.04, 0.08, 0.13, 0.18)
    
    overlaps_zero <- (global_x_range[1] <= 0 &
                        global_x_range[2] >= 0)
    
    p <- ggplot2::ggplot(data) +
      
      # Zero reference:
      { if (overlaps_zero) 
        ggplot2::geom_vline(
        xintercept = 0,
        linetype = "solid", linewidth = 0.7)
      } +
      
      # Replicates as ticks:
      ggplot2::geom_jitter(
        ggplot2::aes(
          x = .data$error, y = 0,
          color = .data$group),
        height = 0, width = 0.001,
        shape = "|", size = 8, alpha = 0.8) +
      
      # Distribution (full, shade):
      ggplot2::geom_area(
        data = dens_full,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          fill = .data$group),
        alpha = 0.05, position = "identity")
    
    # Layered CI shading:
    if (nrow(dens_shaded) > 0) {
      for (i_glow in seq_along(ci_area_alphas)) {
        p <- p + ggplot2::geom_area(
          data = dens_shaded,
          ggplot2::aes(x = .data$x,
                       y = .data$y,
                       fill = .data$group),
          alpha = ci_area_alphas[i_glow],
          position = "identity")
      }
    }
    
    # Central tendency:
    p <- p + ggplot2::geom_vline(
      data = text_data,
      ggplot2::aes(
        xintercept = .data$stat_value,
        color = .data$group),
      linetype = "dotted",
      linewidth = 0.5)
  
    # Density outline:
    for (i_glow in seq_along(glow_widths)) {
      p <- p + ggplot2::geom_line(
        data = dens_full,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          color = .data$group),
        linewidth = glow_widths[i_glow],
        alpha = glow_alphas[i_glow])
    }
    
    # Scales:
    p <- p +
      ggplot2::scale_color_manual(values = pal, drop = TRUE) +
      ggplot2::scale_fill_manual(values = pal, drop = TRUE) +
      ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = scales::breaks_pretty(n = 5),
        limits = global_x_range) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.22)),
        limits = c(0, NA))
    
    # Labels:
    p <- p + ggplot2::labs(
      title = set_title,
      subtitle = set_subtitle,
      caption = paste0(
        "Shaded region: ", as.integer(ci * 100L),
        "% credible interval \u2022 ",
        "Dotted vertical line(s): ", stat,
        "\nEach tick mark represents one replicate"),
      x = "Relative error (%)",
      y = NULL) +
      ggplot2::guides(color = "none", fill = "none") +
      .theme_movedesign_density()
    
    if (facet_by_type) {
      p <- p + ggplot2::facet_wrap(
        . ~ .data$type, scales = "free",
        labeller = ggplot2::labeller(type = label_target))
    }
    
    if (show_text && nrow(text_data) > 0L) {
      
      p <- p +
        ggplot2::geom_text(
          data = text_data,
          ggplot2::aes(
            x = .data$stat_value + .data$x_adjust,
            y = .data$label_y,
            label = .data$label),
          color = "white",
          size = 3.9,
          hjust = text_data$hjust,
          # fontface = "bold",
          # family = "mono",
          lineheight = 0.9,
          show.legend = FALSE) +
        ggplot2::geom_text(
          data = text_data,
          ggplot2::aes(
            x = .data$stat_value + .data$x_adjust,
            y = .data$label_y,
            label = .data$label,
            color = .data$group),
          size = 3.6,
          hjust = text_data$hjust,
          # fontface = "bold",
          # family = "mono",
          show.legend = FALSE)
    }
    
    p <- p + ggplot2::guides(color = "none", fill = "none")
    plots[[i]] <- p
  }
  
  if (length(warnings_list) > 0) {
    warning(
      "\n",
      "  The requested credible interval could not be computed.\n",
      "  This can occur if `ci` is too high or `n_replicates` low.\n",
      "  ", crayon::yellow$bold("Reduce `ci`"), 
      " or ", crayon::yellow$bold("increase `n_replicates`"),
      " for more reliable results.\n",
      call. = FALSE)
  }
  
  if (.verbose) {
    
    out <- suppressMessages(
      lapply(processed, function(x) {
        
        data <- x$data
        cri <- dplyr::bind_rows(
          lapply(groups, function(gr) {
            lapply(types, function(tp) {
              d <- data[data$type == tp & data$group == gr, ]
              dens <- stats::density(d$error, na.rm = TRUE)
              df <- data.frame(x = dens$x, y = dens$y,
                               type = tp, group = gr)
              
              tmp <- .safe_extract_cri(
                d$error, method = method, ci = ci)
              tmp$type <- tp
              tmp$group <- gr
              
              return(tmp)
              
            })}))
      })
    )
    
    return(list(
      outputs = out,
      plots = patchwork::wrap_plots(plots, nrow = length(plots))))
    
  }
  
  return(patchwork::wrap_plots(plots, nrow = length(plots)))
  
}


#' @title Assess output convergence in simulation outputs
#'
#' @description
#' Evaluates whether the cumulative mean of a tracked error metric in
#' simulation outputs has stabilized, indicating convergence. This
#' function helps determine if repeated simulations or resampling have
#' produced stable estimates, which is critical for reliable inference
#' in animal movement projects.
#'
#' Use this function after running `md_run()` or
#' `md_replicate()` to check the reliability of outputs
#' before further interpretation or reporting.
#'
#' @param obj A `movedesign` or related object returned by
#'   [md_run()] or [md_replicate()].
#' @param m Numeric (optional). If provided, restricts the convergence
#'   check to results for a specific population sample size (`m`).
#'   Defaults to `NULL`, which checks up to the maximum population
#'   sample size.
#' @param tol Numeric. The tolerance threshold for absolute change in
#'   the cumulative mean to declare convergence. Defaults to
#'   `0.05`.
#' @param n_converge Integer. Number of consecutive steps within
#'   tolerance required to confirm convergence.
#' @param plot Logical. If `TRUE` (default), generates a plot of
#'   stepwise changes in the cumulative mean, highlighting when
#'   convergence is achieved.
#' @param pal Character vector of color(s) of the plot, such as
#'   `c("#007d80", "#A12C3B")`) (default).
#' 
#' @return An object of class `"movedesign_check"` with the
#'   following elements:
#' \describe{
#'   \item{`has_converged`}{
#'     Logical scalar indicating whether convergence was achieved.}
#'   \item{`recent_deltas`}{
#'     Numeric vector of absolute changes in cumulative mean
#'     over the last `n_converge` steps.}
#'   \item{`max_delta`}{
#'     Maximum absolute change among the last steps.}
#'   \item{`tolerance`}{
#'     Numeric, the input tolerance `tol`.}
#'   \item{`n_converge`}{
#'     Integer, the input `n_converge`.}
#'   \item{variable}{
#'     Character. Name of the variable checked.}
#'   \item{recent_cummean}{Numeric vector.
#'     The last cumulative means checked.}
#' }
#'
#' @details
#' The cumulative mean of error is calculated, and the absolute changes
#' over the last `n_converge` steps are inspected. If all are
#' below the specified tolerance, convergence is declared.
#'
#' If `plot = TRUE`, a plot is shown of absolute stepwise change
#' in the cumulative mean, with a shaded region indicating the
#' convergence threshold, aiding visual assessment.
#'
#' @seealso
#'   [md_run()],
#'   [md_replicate()]
#'
#' @examples
#' if(interactive()) {
#'  
#'   output <- md_replicate(input, n_replicates = 20)
#'   md_check(output, tol = 0.05, n_converge = 10)
#'  
#' }
#' 
#' @export
md_check <- function(obj,
                     m = NULL,
                     tol = 0.05,
                     n_converge = 9,
                     plot = TRUE,
                     pal = c("#007d80", "#A12C3B")) {
  
  if (!inherits(obj, "movedesign") ||
      !("summary" %in% names(obj))) {
    stop(paste(
      "Input does not appear to be a 'movedesign_output' object.",
      "\nPlease provide the output of md_run()."))
  }
  
  if (!is.null(m) && !obj$verbose) {
    stop(paste("`md_replicate()` must be run with",
               "`verbose = TRUE` to use the `m` argument."))
  }
  
  stopifnot(
    is.numeric(tol) && tol > 0 && tol < 1,
    is.numeric(n_converge) && n_converge >= 0)
  
  set_caption <- NULL
  if (n_converge < 5 || obj$data$n_replicates <= 5) {
    set_caption <- paste(
      "Using a small number of replicates may",
      "give unreliable convergence diagnostics.")
    set_caption <- paste("Warning:", set_caption)
  }
  
  index <- NULL
  type <- group <- has_converged <- NULL
  cummean <- delta_cummean <- last_cummean <- NULL
  idx_stable_start <- last_roll_sd <- NULL
  
  variable <- "error"
  data <- obj$summary
  
  set_target <- obj$data$set_target
  has_groups <- ("group" %in% names(data)) &&
    any(c("A", "B") %in% unique(as.character(data$group)))
  
  if (has_groups) {
    data <- data[data$group != "All", ]
  } else {
    data <- data[data$group == "All", ]
  }
  
  if (!is.null(m)) {
    if (m %!in% unique(data$m)) {
      stop(paste0("Population sample size '", m, 
                  "' not found in data. Valid values: ", 
                  paste(sort(unique(data$m)), collapse = ", ")))
    }
    data <- dplyr::filter(data, m == !!m)
  } else {
    data <- subset(data, m == max(m))
  }
  
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(tol) && tol > 0)
  stopifnot(is.numeric(n_converge) && n_converge >= 2)
  stopifnot(variable %in% names(data))
  
  # Rolling SD (vectorized):
  
  .roll_sd <- function(x, n) {
    sd_vec <- rep(NA_real_, length(x))
    if(length(x) >= n) {
      for(i in n:length(x)) {
        sd_vec[i] <- stats::sd(x[(i - n + 1):i])
      }
    }
    return(sd_vec)
  }
  
  # Stable detection:
  
  .find_stable <- function(delta_vec, tol, n_converge) {
    
    valid <- !is.na(delta_vec) & (abs(delta_vec) <= tol)
    if (sum(valid) < n_converge) return(NA_integer_)
    
    # Find first run of n_converge consecutive TRUEs:
    run_len <- 0L
    for (i in seq_along(valid)) {
      run_len <- if (valid[i]) run_len + 1L else 0L
      if (run_len >= n_converge)
        return(i - n_converge + 1L)
    }
    
    return(NA_integer_)
  }
  
  # Compute cumulative mean and stepwise changes:
  
  data_subset <- data %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::mutate(
      index = dplyr::row_number(),
      cummean = cumsum(.data[[variable]]) / index,
      cumvar = cumsum((.data[[variable]] - cummean)^2) / index,
      delta_cummean = c(NA, diff(cummean)),
      has_converged = abs(delta_cummean) < tol,
      roll_sd = .roll_sd(cummean, n = n_converge)) %>%
    dplyr::mutate(
      color_state = dplyr::case_when(
        .data$index <= max(.data$index) - n_converge ~ "early",
        .data$has_converged ~ "converged",
        TRUE ~ "diverged")) %>%
    dplyr::ungroup()
  
  get_r <- data_subset %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::summarise(
      n_steps = sum(!is.na(.data$delta_cummean)),
      .groups = "drop") %>%
    dplyr::pull(.data$n_steps) %>%
    min()
  
  if (get_r < n_converge) {
    stop(sprintf(
      "Not enough replicates (n = %d) to check %d steps.",
      get_r, n_converge))
  }

  # Compute convergence diagnostics:
  
  stable_idx <- data_subset %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::summarise(
      idx_stable_start = .find_stable(
        delta_cummean, tol, n_converge),
      .groups = "drop")
  
  diag <- data_subset %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::summarise(
      last_cummean = tail(.data$cummean, 1),
      recent_cummean = list(tail(.data$cummean, n_converge)),
      recent_delta_cummean = list(
        tail(.data$delta_cummean, n_converge)),
      recent_roll_sd = list(tail(.data$roll_sd, n_converge)),
      last_roll_sd = max(
        tail(.data$roll_sd, 1), na.rm = TRUE),
      idx_stable_start = .find_stable(
        delta_cummean, tol, n_converge),
      .groups = "drop") %>%
    dplyr::mutate(
      has_converged = !is.na(.data$idx_stable_start) &
        .data$last_roll_sd <= tol) %>%
    dplyr::arrange(dplyr::desc(.data$type))
  
  dt_plot <- data_subset %>%
    dplyr::left_join(
      diag %>% dplyr::select(
        -"has_converged", -"idx_stable_start"),
      by = c("type", "group")) %>%
    dplyr::left_join(stable_idx, by = c("type", "group"))
  
  dt_rect <- suppressWarnings(
    dt_plot %>%
      dplyr::group_by(.data$type, .data$group) %>%
      dplyr::filter(!is.na(.data$idx_stable_start)) %>%
      dplyr::summarise(
        xmin = .data$idx_stable_start[1],
        xmax = max(.data$index, na.rm = TRUE),
        .groups = "drop"))
  
  failed_groups <- diag %>%
    dplyr::filter(!.data$has_converged) %>%
    dplyr::mutate(
      group_label = if (has_groups) {
        paste0(.data$type, " / group ", .data$group)
      } else {
        dplyr::case_when(
          .data$type == "hr" ~ "mean home range",
          .data$type == "ctsd" ~ "mean speed",
          TRUE ~ as.character(.data$type)
        )
      }) %>%
    dplyr::pull(.data$group_label)
  
  all_groups <- diag %>%
    dplyr::mutate(
      group_label = if (has_groups) {
        paste0(.data$type, " / group ", .data$group)
      } else {
        dplyr::case_when(
          .data$type == "hr" ~ "mean home range",
          .data$type == "ctsd" ~ "mean speed",
          TRUE ~ as.character(.data$type)
        )
      }) %>%
    dplyr::pull(.data$group_label)
  
  if (has_groups) {
    all_groups <- gsub("^hr", "mean home range", all_groups)
    all_groups <- gsub("^ctsd", "mean speed", all_groups)
    failed_groups <- gsub("^hr", "mean home range", failed_groups)
    failed_groups <- gsub("^ctsd", "mean speed", failed_groups)
  }
  
  converged_groups <- setdiff(all_groups, failed_groups)
  set_subtitle <- paste0(
    if (length(converged_groups)) {
      paste0("Converged: ",
             paste(converged_groups,
                   collapse = "\n                  "))
    } else "",
    
    if (length(converged_groups) && 
        length(failed_groups)) "\n" else "",
    
    if (length(failed_groups)) {
      paste0("Did not converge: ",
             paste(failed_groups,
                   collapse = "\n                           "))
    } else "",
    
    sprintf("\n(tolerance = \u00B1 %g%%)", tol * 100)
  )
  
  dt_plot$type_f <- factor(dt_plot$type, levels = c("hr", "ctsd"))
  dt_rect$type_f <- factor(dt_rect$type, levels = c("hr", "ctsd"))
  
  col_converged <- col_tolerance <- pal[1]
  col_diverged <- pal[2]
  
  p <- NULL
  p <- dt_plot %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$replicate, 
                   y = .data$delta_cummean,
                   group = .data$group,
                   shape = .data$group,
                   linetype = .data$group)) +
    
    # Convergence zone:
    ggplot2::annotate(
      "rect",
      xmin = -Inf, xmax = Inf,
      ymin = -tol, ymax = tol,
      fill = col_tolerance,
      alpha = 0.05) +
    
    # Stable region:
    # { if (nrow(dt_rect) > 0L)
    #   ggplot2::geom_rect(
    #     data = dt_rect,
    #     ggplot2::aes(
    #       xmin = .data$xmin, # - 0.08,
    #       xmax = .data$xmax, # + 0.08,
    #       ymin = -Inf,
    #       ymax = Inf),
    #     fill = col_converged, alpha = 0.06,
    #     inherit.aes = FALSE)
    # } +
    
    # Vertical entry marker:
    { if (nrow(dt_rect) > 0L)
      ggplot2::geom_vline(
        data = dt_rect,
        ggplot2::aes(xintercept = .data$xmin),
        color = col_converged,
        linetype = "dotted", linewidth = 0.45,
        inherit.aes = FALSE)
    } +
    
    # Zero reference:
    ggplot2::geom_hline(
      yintercept = 0,
      color = "black",
      # color = "#1a1a1a",
      linewidth = 0.35,
      alpha = 0.35) +
    
    # Tolerance bounds:
    ggplot2::geom_hline(
      yintercept = tol,
      color = col_tolerance,
      linewidth = 0.55,
      linetype = "dashed") +
    ggplot2::geom_hline(
      yintercept = -tol,
      color = col_tolerance,
      linewidth = 0.55,
      linetype = "dashed") +
    
    # Tolerance labels:
    ggplot2::annotate(
      "text",
      x = Inf, y = tol,
      label = sprintf("+%g%%", tol * 100),
      color = col_tolerance,
      # fontface = "bold",
      hjust = 1.08, vjust = -0.5,
      size = 3.2) +
    ggplot2::annotate(
      "text",
      x = Inf, y = -tol,
      label = sprintf("\u2212%g%%", tol * 100),
      color = col_tolerance,
      # fontface = "bold",
      hjust = 1.08, vjust = 1.5,
      size = 3.2) +
    
    # Facets:
    
    { if (length(set_target) > 1)
      ggplot2::facet_wrap(
        . ~ .data$type_f,
        labeller = ggplot2::labeller(
          type_f = c(
            "hr" = "Home range estimation", 
            "ctsd" = "Speed \u0026 distance estimation"))) } +
    
    ggplot2::geom_line(
      color = "#CCCCCC",
      position = if (has_groups)
        ggplot2::position_dodge(width = 0.1) else 
          ggplot2::position_identity(),
      linewidth = 0.7,
      lineend = "round",
      show.legend = FALSE) +
    
    ggplot2::geom_point(
      ggplot2::aes(color = .data$color_state),
      position = if (has_groups)
        ggplot2::position_dodge(width = 0.1) else 
          ggplot2::position_identity(),
      size = 8,
      alpha = 0.10) +
    
    ggplot2::geom_point(
      ggplot2::aes(color = .data$color_state),
      position = if (has_groups)
        ggplot2::position_dodge(width = 0.1) else 
          ggplot2::position_identity(),
      size = 5,
      alpha = 0.25) +
    
    ggplot2::geom_point(
      ggplot2::aes(color = .data$color_state),
      position = if (has_groups)
        ggplot2::position_dodge(width = 0.1) else 
          ggplot2::position_identity(),
      size = 1.8,
      alpha = 1) +
    
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(),
      expand = ggplot2::expansion(mult = 0.10)) +
    
    ggplot2::scale_color_manual(
      name = NULL,
      values = c(
        early = "#C8C8C8",
        converged = col_converged,
        diverged = col_diverged),
      labels = c(
        early = paste0(
          "Burn-in (\u2264 ", n_converge, " replicates)"),
        converged = "Converged",
        diverged = "Not yet converged"),
      drop = FALSE) +
    
    { if (has_groups)
      ggplot2::scale_shape_manual(
        "Groups:", values = c(16, 17), drop = FALSE)
    } +
    { if (has_groups)
      ggplot2::scale_linetype_manual(
        "Groups:", values = c("solid", "dashed"),
        drop = FALSE)
    } +
    
    # Labels:
    ggplot2::labs(
      title = "Convergence diagnostics",
      subtitle = set_subtitle,
      caption = set_caption,
      # caption = paste0(
      #   if (!is.null(caption))
      #     paste0(caption, " \u2022 ") else NULL,
      #   "\u25cf\u2009Converged ",
      #   "\u25cf\u2009Not converged"),
      x = "Replicate index",
      y = "Stepwise \u0394 cumulative mean") +
    
    # Theme:
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      
      # Canvas
      plot.background = ggplot2::element_rect(
        fill = "#FFFFFF", color = NA),
      panel.background = ggplot2::element_rect(
        fill = "#F7F7F5", color = NA),
      panel.spacing = ggplot2::unit(0.8, "cm"),
      
      # Grid
      panel.grid.major.y = ggplot2::element_line(
        color = "#E8E8E8", linewidth = 0.35),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      
      # Axes
      axis.title = ggplot2::element_text(
        size = 11),
      axis.title.x = ggplot2::element_text(
        face = "bold", hjust = 1),
      axis.title.y = ggplot2::element_text(
        face = "bold", hjust = 1),
      axis.text = ggplot2::element_text(
        color = "grey40", size = 9),
      axis.ticks = ggplot2::element_line(
        color = "grey80", linewidth = 0.3),
      
      # Facet strips
      strip.text = ggplot2::element_text(
        color = "grey20", size = 14, face = "bold",
        margin = ggplot2::margin(b = 6)),
      strip.background.x = ggplot2::element_rect(
        color = NA, fill = NA),
      strip.background.y = ggplot2::element_rect(
        color = NA, fill = NA),
      strip.clip = "off",
      
      # Title block
      plot.title = ggplot2::element_text(
        face = "bold", size = 16,
        margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(
        color = "grey45", size = 10,
        margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(
        color = "grey60", size = 9, hjust = 0.5,
        margin = ggplot2::margin(t = 8)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      # Margins
      plot.margin = ggplot2::unit(
        c(1, 1, 1, 1), "cm"))
  
  if (has_groups) {
    p <- p +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(color = "none", fill = "none")
    
  } else {
    p <- p + 
      ggplot2::theme(legend.position = "none") +
      ggplot2::guides(
        color = "none", fill = "none", linetype = "none")
  }
   
  if (plot) {
    suppressWarnings(print(p))
  }
  
  return(structure(list(
    grouped = has_groups,
    diagnostics_table = diag,
    tolerance = tol,
    n_converge = n_converge,
    has_converged = diag$has_converged,
    stabilized_at = stable_idx,
    error_threshold = obj$error_threshold,
    plot = p,
    warning = set_caption
  ), class = "movedesign_check"))
  
}


#' @title Interactively configure movement design setup
#'
#' @description
#' Guides the user to assign each argument required for a movement design
#' workflow, including species label and key simulation settings. Users
#' may choose to set a specific population sample size (number of animals
#' tagged/to be tagged) or optimize the population sample size considering
#' a specific analytical target.
#'
#' @details
#' The argument `data` is **required** and must be supplied directly (as
#' a list of telemetry objects, obtained from `ctmm::as.telemetry()`).
#' The argument `models` is optional, and if omitted, models will be
#' fitted automatically.
#'
#' @param data A named list of simulated movement datasets, each a
#'   `telemetry` object compatible with `ctmm` `R` package. Each object
#'   must contain valid metadata and timestamped locations.
#' @param models (Optional) Named list of fitted ctmm models (from
#'   `ctmm::ctmm.fit()` or `ctmm::ctmm.select()`). If not supplied,
#'   models are fitted automatically.
#' @param parallel Logical. If `TRUE`, enables parallel processing.
#' 
#' @return
#' An object of class `movedesign_input` (and `movedesign`). This is
#' a structured S3 list containing all validated inputs, model fits,
#' and derived parameters for the study design workflow.
#'
#' @examples
#' if(interactive()) {
#'   data(buffalo)
#'   md_params <- md_configure(data = buffalo)
#' }
#'
#' @seealso [`md_prepare()`]
#' 
#' @importFrom ctmm %#%
#' 
#' @export
md_configure <- function(data, models = NULL, parallel = FALSE) {
  
  if (is.null(data)) stop(
    "'data' is a required argument and must not be NULL.")
  
  valid_units <- c("day", "days",
                   "hour", "hours",
                   "week", "weeks",
                   "year", "years")
  
  # Maps any valid unit input to its plural form for consistency:
  normalize_unit <- function(unit) {
    unit <- tolower(unit)
    if (unit %in% c("day", "days")) return("days")
    if (unit %in% c("hour", "hours")) return("hours")
    if (unit %in% c("week", "weeks")) return("weeks")
    if (unit %in% c("year", "years")) return("years")
    return(NULL)
  }
  
  # Prompt for value with optional validation:
  .ask_input <- function(prompt,
                         default = NULL,
                         validate = NULL,
                         details = "") {
    
    repeat {
      cat("\n", .msg(prompt, "success"))
      if (nzchar(details)) cat(.msg(paste("", details), "success"))
      if (!is.null(default)) cat(paste0(" [", default, "]"))
      cat("\n")
      
      input <- readline(.msg("  Enter value: ", "main"))
      input <- if (input == "" && !is.null(default)) 
        default else input
      
      result <- tryCatch(
        if (is.null(validate)) TRUE else validate(input),
        error = function(e) "Validation failed."
      )
      
      if (isTRUE(result)) return(input)
      
      # Show specific error message if returned
      cat(.msg(paste0("\n  ", if (is.character(result)) 
        result else "Invalid input. Try again."), "danger"))
    }
  }
  
  
  # Prompt for choice from options:
  .ask_choice <- function(prompt, choices) {
    
    repeat {
      cat("\n", .msg(prompt, "success"), "\n")
      for (i in seq_along(choices)) {
        cat(.msg(paste0("  [", i, "] "), "success"),
            choices[i], "\n", sep = "")
      }
      
      value <- readline(.msg("  Select option number: ", "main"))
      if (value %in% as.character(seq_along(choices)))
        return(as.integer(value))
      cat(.msg("\n  Invalid input. Please enter a valid number.\n", 
               "danger"))
    }
  }
  
  cat(.header("Movement design interactive setup"))
  
  # Prompt for species label:
  species <- .ask_input(
    "Species label", NULL, function(x) nzchar(x),
    "(scientific or common name)?"
  )
  
  # Fit models automatically (if not provided):
  if (is.null(models)) {
    cat("\nNo models provided. Fitting models automatically...\n")
    models <- fitting_models(data, parallel = FALSE)
    cat("Models fitted successfully!\n")
  } else {
    if (!all(sapply(models, function(m) 
      inherits(m, c("ctmm")))))
      stop("Models must be from ctmm.fit() or ctmm.select().")
  }
  names(models) <- names(data)
  
  # Workflows:
  
  sample_mode <- .ask_choice(
    "Do you want to:",
    c("Verify study design for a specific population sample size",
      "Find the optimal study design required for a target")
  )
  # sample_mode <- .ask_choice(
  #   "Do you want to:",
  #   c("Verify study design for a specific population sample size",
  #     "Find the minimum population sample size required for a target")
  # )
  
  which_m <- NULL
  n_individuals <- NULL
  dur <- dti <- NULL
  objective <- NULL
  target_param <- NULL
  
  # Research target:
  
  # set_target <- .ask_choice(
  #   "Which research target:",
  #   c("Home range estimation",
  #     "Speed & distance estimation",
  #     "Both"))
  set_target <- .ask_choice(
    "Which research target:",
    c("Home range estimation",
      "Speed & distance estimation"))
  set_target <- switch(set_target,
                       "hr", "ctsd", c("hr", "ctsd"))
  
  # Analytical target:
  
  which_meta <- .ask_choice(
    "Which analytical target:",
    c(paste0("Mean estimate of ",
             .msg("sampled population", "success")),
      paste0("Compare estimates of ", .msg("two", "success"), 
             " sampled groups")))
  
  if (which_meta == 1) which_meta <- "mean"
  if (which_meta == 2) which_meta <- "ratio"
  
  # Population sample size:
  
  if (sample_mode == 1) {
    n_individuals <- as.integer(.ask_input(
      "Enter population sample size (number of individuals):",
      NULL, function(x) {
        val <- suppressWarnings(as.integer(x))
        if (is.na(val) || val <= 0) 
          return("Must be a positive integer.")
        if (!is.null(groups) && val %% 2 != 0)
          return("Must be even when 'groups' is defined.")
        TRUE  } ))
    which_m <- "set_m"
  } else {
    n_individuals <- as.integer(.ask_input(
      "Enter maximum population sample size (number of individuals):",
      NULL, function(x) {
        val <- suppressWarnings(as.integer(x))
        if (is.na(val) || val <= 0) 
          return("Must be a positive integer.")
        if (!is.null(groups) && val %% 2 != 0)
          return("Must be even when 'groups' is defined.")
        TRUE  } ))
    which_m <- "get_m"
    
    # opt_choices <- c("Duration", "Interval")
    # opt_idx <- .ask_choice(
    #   "Which sampling parameter to optimize?", opt_choices)
    # objective <- "sampling_parameters"
    # target_param <- opt_choices[opt_idx]
  }
  
  # Groups (optional):
  
  groups <- NULL
  grouped <- FALSE
  if (which_meta == "ratio") {
    
    groups_var <- .ask_input(
      "Groups object:", NULL,
      function(x) x == "" || exists(x, .GlobalEnv),
      "(named list of groups in environment)")
    groups <- if (groups_var != "") 
      get(groups_var, .GlobalEnv) else NULL
    grouped <- TRUE    
  }
  
  # Sampling duration:
  
  if (sample_mode == 1) {
    .header("Sampling parameters")
    
    dur_unit <- .ask_input(
      "Sampling duration unit", "days",
      function(x) !is.null(normalize_unit(x)),
      "(day[s], hour[s], week[s], year[s]):")
    dur_value <- as.numeric(.ask_input(
      "Sampling duration value:", NULL, function(x) 
        suppressWarnings(!is.na(as.numeric(x))),
      paste0("(e.g. '10' for 10 ", dur_unit, ")")))
    
    # dur_unit <- normalize_unit(dur_unit)
    dur <- list(value = dur_value, unit = dur_unit)
  }
  
  # Sampling interval:
  
  if (sample_mode == 1) {
    dti_unit <- .ask_input(
      "Sampling interval unit", "hours",
      function(x) !is.null(normalize_unit(x)),
      "(second[s], minute[s], hour[s]):")
    dti_value <- as.numeric(.ask_input(
      "Sampling interval value", NULL, function(x)
        suppressWarnings(!is.na(as.numeric(x))),
      paste0("(e.g. '1' for 1 ", dti_unit, ")")))
    
    # dti_unit <- normalize_unit(dti_unit)
    dti <- list(value = dti_value, unit = dti_unit)
  }
  
  # Individual variation:
  
  add_individual_variation <- as.logical(.ask_input(
    "Add individual variation?", "FALSE",
    function(x) x %in% c("TRUE", "FALSE"),
    "(TRUE or FALSE)"
  ))
  
  # Parallel execution:
  
  parallel <- as.logical(.ask_input(
    "Run in parallel?", "FALSE",
    function(x) x %in% c("TRUE", "FALSE"),
    "(TRUE or FALSE)"
  ))
  
  cat(.msg("\nAll arguments assigned.\n", "success"))
  
  meanfitList <- list(mean(models))
  names(meanfitList) <- "All"
  
  if (add_individual_variation) {
    sigma <- suppressWarnings(extract_pars(meanfitList, "sigma"))
    tau_p <- suppressWarnings(extract_pars(meanfitList, "position"))
    tau_v <- suppressWarnings(extract_pars(meanfitList, "velocity"))
  } else {
    sigma <- extract_pars(models, "sigma", meta = TRUE)
    tau_p <- extract_pars(models, "position", meta = TRUE)
    tau_v <- extract_pars(models, "velocity", meta = TRUE)
  }
  
  mu <- list(array(0, dim = 2, dimnames = list(c("x", "y"))))
  names(sigma) <- names(tau_p) <- names(tau_v) <- "All"
  names(mu) <- "All"
  
  if (!is.null(groups)) {
    
    groups[[1]] <- groups
    groups[[2]] <- list(A = c(), B = c())
    names(groups) <- NULL
    
    fitA <- models[groups[[1]][["A"]]]
    fitB <- models[groups[[1]][["B"]]]
    
    meanfitA <- tryCatch(
      mean(fitA) %>% 
        suppressMessages() %>% 
        suppressWarnings() %>% 
        quiet(),
      error = function(e) e)
    
    meanfitB <- tryCatch(
      mean(fitB) %>% 
        suppressMessages() %>% 
        suppressWarnings() %>% 
        quiet(),
      error = function(e) e)
    
    if (inherits(meanfitA, "error") ||
        inherits(meanfitB, "error")) {
      stop(paste0(
        "Extraction ", .msg("failed", "danger"), 
        " for one or both groups."))
      
    } else {
      meanfitList <- list(meanfitList[[1]], meanfitA, meanfitB)
      names(meanfitList) <- c("All", "A", "B")
    }
    
    mu <- list(mu[[1]], mu[[1]], mu[[1]])
    
    seed0 <- generate_seed()
    
    fitA <- tryCatch({
      simulate_seeded(meanfitList[["A"]], seed0)
    }, error = function(e) {
      message("A warning occurred:", conditionMessage(e), "\n")
    })
    
    fitB <- tryCatch({
      simulate_seeded(meanfitList[["B"]], seed0)
    }, error = function(e) {
      message("A warning occurred:", conditionMessage(e), "\n")
    })
    
    validate_A <- tryCatch({
      ctmm::simulate(fitA, t = seq(0, 100, by = 1), seed = seed0)
    }, error = function(e) {
      return(NULL)
    })
    
    validate_B <- tryCatch({
      ctmm::simulate(fitB, t = seq(0, 100, by = 1), seed = seed0)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(validate_A) || is.null(validate_B)) {
      bug_group <- c()
      if (is.null(validate_A)) bug_group <- c(bug_group, "A")
      if (is.null(validate_B)) bug_group <- c(bug_group, "B")
      
      stop("Validation ", .msg("failed", "danger"),
           " of group(s): ", .msg(toString(bug_group), "danger"))
    }
    
    fit <- list(A = fitA, B = fitB)
    
    sigma <- c(sigma, lapply(1:2, function(x) {
      suppressWarnings(extract_pars(
        obj = fit[[x]],
        name = "sigma", meta = TRUE)[[1]])
    }))
    names(sigma) <- c("All", "A", "B") 
    
    tau_p <- c(tau_p, lapply(1:2, function(x) {
      suppressWarnings(extract_pars(
        obj = fit[[x]], 
        name = "position", meta = TRUE)[[1]])
    }))
    names(tau_p) <- c("All", "A", "B") 
    
    tau_v <- c(tau_v, lapply(1:2, function(x) {
      suppressWarnings(extract_pars(
        obj = fit[[x]], 
        name = "velocity", meta = TRUE)[[1]])
    }))
    names(tau_v) <- c("All", "A", "B") 
    
    mu <- list(array(0, dim = 2, 
                     dimnames = list(c("x", "y"))),
               array(0, dim = 2, 
                     dimnames = list(c("x", "y"))),
               array(0, dim = 2, 
                     dimnames = list(c("x", "y"))))
    names(mu) <- c("All", "A", "B")
    
  } # end of if (!is.null(groups))
  
  if (which_meta == "ratio") which_meta <- "compare"
  
  design <- movedesign_input(list(
    data = data,
    data_type = "selected",
    get_species = species,
    n_individuals = as.numeric(n_individuals),
    dur = dur,
    dti = dti,
    add_ind_var = add_individual_variation,
    grouped = ifelse(!is.null(groups), TRUE, FALSE),
    groups = groups,
    set_target = set_target,
    which_meta = which_meta,
    which_m = which_m,
    parallel = parallel,
    fitList = models,
    meanfitList = meanfitList,
    sigma = sigma,
    tau_p = tau_p,
    tau_v = tau_v,
    mu = mu))
  
  return(design)
}


#' @title Plot meta-analyses outputs
#'
#' @description
#' Computes summary statistics and generates a ggplot object visualizing
#' relative error across population sample sizes for one or more
#' metrics. Supports both one or two groups.
#' 
#' @keywords internal
#' @noRd
.md_plot_meta <- function(x,
                          error_threshold = NULL,
                          set_target = c("hr", "ctsd"),
                          has_groups = FALSE) {
  
  ci <- 0.95
  pal <- list("TRUE" = "#007d80", "FALSE" = "#A12C3B")
  
  if (has_groups) {
    out <- x[x$group != "All", ]
    set_shapes <- c(21, 24)
    set_shapes_manual <- c(16, 17)
  } else {
    out <- x[x$group == "All", ]
    set_shapes <- c(21, 21)
    set_shapes_manual <- c(16, 16)
  }
  
  if (!is.null(error_threshold)) {
    out <- out %>%
      dplyr::mutate(
        overlaps = factor(
          abs(.data$error) <= error_threshold,
          levels = c(TRUE, FALSE)),
        highlight = FALSE)
  } else {
    out <- out %>%
      dplyr::mutate(overlaps = TRUE)
  }
  
  out_mean <- .summarize_error(out, conf_level = ci,
                               error_threshold = error_threshold)
  
  if (!is.null(error_threshold)) {
    out_mean <- out_mean %>%
      dplyr::mutate(top_facet = .data$type == "hr") %>%
      dplyr::ungroup()
  } else {
    out_mean <- out_mean %>%
      dplyr::mutate(top_facet = .data$type == "hr",
                    overlaps = TRUE) %>%
      dplyr::ungroup()
  }
  
  label_ci <- paste0(ci * 100, "%")
  label_threshold <- sprintf("%s%%", error_threshold * 100)
  labels_target <- c("hr" = "Home range area estimation",
                     "ctsd" = "Speed \u0026 distance estimation")
  
  only_one_m <- ifelse(length(unique(out$m)) == 1, TRUE, FALSE)
  if (only_one_m) out$m <- factor(out$m)
  
  m_resampled <- out_mean$m[out_mean$n > 1L]
  
  p <- out_mean %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = if (has_groups && length(unique(.data$m)) == 1) {
          as.factor(.data$m) 
        } else { 
          .data$m 
        },
        y = .data$error,
        group = .data$group,
        shape = .data$group,
        color = .data$overlaps)) +
    
    { !is.null(error_threshold)
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.3,
        linetype = "solid") } +
    { !is.null(error_threshold)
      ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = -error_threshold,
        ymax = error_threshold,
        alpha = 0.05)} +
    { !is.null(error_threshold)
      ggplot2::geom_hline(
        yintercept = c(-error_threshold, error_threshold),
        linetype = "dashed",
        linewidth = 0.35, alpha = 0.8) } +
    { !is.null(error_threshold)
      ggplot2::annotate(
        "text",
        x = Inf, y = error_threshold,
        label = paste0("+", label_threshold),
        hjust = 1.08, vjust = -0.5,
        size = 3.2) } +
    { !is.null(error_threshold)
      ggplot2::annotate(
        "text",
        x = Inf, y = -error_threshold,
        label = paste0("\u2212", label_threshold),
        hjust = 1.08, vjust = 1.5,
        size = 3.2) } +
    
    { if (!only_one_m)
      ggplot2::geom_jitter(
        data = dplyr::filter(out, .data$m %in% m_resampled),
        mapping = ggplot2::aes(
          .data$m,
          y = .data$error,
          group = .data$group,
          shape = .data$group,
          color = .data$overlaps,
          fill = .data$overlaps),
        position = ggplot2::position_jitterdodge(
        dodge.width = 1.5),
      size = 3, alpha = 0.25) } +
    
    # Prediction intervals:
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$pred_lci,
                   ymax = .data$pred_uci),
      position = ggplot2::position_dodge(width = 0.5),
      color = "grey20",
      linewidth = 3,
      alpha = 0.18,
      show.legend = FALSE) +
    
    # Confidence intervals:
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$error_lci,
                   ymax = .data$error_uci),
      position = ggplot2::position_dodge(width = 0.5),
      linewidth = 1.1,
      alpha = 0.55,
      show.legend = FALSE) +
    
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.5),
      size = 6,
      show.legend = TRUE) +
    
    { if (length(set_target) > 1)
      ggplot2::facet_wrap(
        . ~ .data$type, scales = "free_y",
        labeller = ggplot2::labeller(labels_target))
    } +
    
    ggplot2::labs(
      x = "Population sample size",
      y = "Relative error (%)") +
    
    { if (!is.null(error_threshold)) {
      ggplot2::scale_fill_manual(
        name = paste0("Within error threshold (\u00B1",
                      error_threshold * 100, "%)?"),
        breaks = c("TRUE", "FALSE"),
        values = pal, drop = FALSE,
        guide = ggplot2::guide_legend(
          override.aes = list(color = pal, fill = pal, size = 3),
          order = 1,
          label.vjust = 0.4))
    } else {
      ggplot2::scale_fill_manual(values = pal[1])
    }} +
    
    { if (length(unique(out$m)) == 2) {
      ggplot2::scale_x_continuous(
        breaks = unique(out$m))
    } else if (!only_one_m) {
      ggplot2::scale_x_continuous(
        breaks = scales::breaks_pretty())
    }
    } +
    
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      breaks = scales::breaks_pretty(),
      expand = ggplot2::expansion(mult = c(0.05, 0.10))) +
    
    ggplot2::scale_color_manual(
      values = c(pal[1], pal[2]), drop = FALSE) +
    
    { if (has_groups)
      ggplot2::scale_shape_manual(
        "Groups:", values = c(16, 17)) } +
    
    ggplot2::scale_alpha_continuous(
        range = c(0.3, 0.3)) +
    
    { if (has_groups)
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = 22, size = 4,
            fill = c(pal[1], pal[2])))) } +
    
    .theme_movedesign_report()
  
  if (!has_groups) {
    p <- p + ggplot2::guides(shape = "none", color = "none")
  }
  
  if (is.null(error_threshold)) {
    p <- p + ggplot2::guides(fill = "none", color = "none")
  }
  
  return(list(plot = p,
              dt_plot = out,
              dt_plot_means = out_mean))
  
}


#' @title Prepare optimization setup
#'
#' @keywords internal
#' @noRd
.md_optimize_setup <- function(obj,
                               n_replicates,
                               error_threshold,
                               plot,
                               verbose,
                               parallel,
                               ncores,
                               trace,
                               ...) {
  
  dots <- list(...)
  .tol <- dots[[".tol"]] %||% 0.05
  .seeds <- dots[[".seeds"]] %||% NULL
  .n_converge <- dots[[".n_converge"]] %||% 5
  .console_alert <- dots[[".console_alert"]] %||% TRUE
  
  tmp_N_area <- suppressWarnings(
    extract_dof(obj$meanfitList, "area"))[[1]]
  tmp_N_speed <- suppressWarnings(
    extract_dof(obj$meanfitList, "speed"))[[1]]
  
  if ("hr" %in% obj$set_target && tmp_N_area < 30) {
    warning(paste0(
      "Effective sample size (for area) ",
      "in the original dataset is low (N = ", 
      round(tmp_N_area, 1), "). Evaluate outputs with caution."))
  }
  
  if ("ctsd" %in% obj$set_target && tmp_N_speed < 30) {
    warning(paste0(
      "Effective sample size (for speed) ",
      "in the original dataset is low (N = ", 
      round(tmp_N_speed, 1), "). Evaluate outputs with caution."))
  }
  
  tau_p <- obj$tau_p
  if (obj$add_ind_var) {
    taup <- tau_p[[1]][3, "value"] %#% tau_p[[1]][3, "unit"]
  } else {
    taup <- tau_p[[1]][2, "value"] %#% tau_p[[1]][2, "unit"]
  }
  
  error_label <- paste0(round(error_threshold * 100, 0), "%")
  
  .get_ess <- function(target_error) {
    
    if (abs(target_error) >= 0.6791068) return(1)
    target_error <- target_error * 0.5
    if (abs(target_error) <= 0.0125) return(1000)
    
    target_error <- -abs(target_error)
    a_hat <- -0.01084036
    b_hat <- -0.66826640
    c_hat <-  0.86579770
    
    N1 <- (b_hat / (target_error - a_hat))^(1 / c_hat)
    
    return(N1)
  }
  
  N1 <- .get_ess(error_threshold)
                    
  thresholdList <- list(
    "0.01" = c(350, 5000), "0.05" = c(200, 2000),
    "0.10" = c(80, 1000), "0.25" = c(30, 500),
    "0.50" = c(10, 200), "default" = c(5, 100))
  numeric_keys <- setdiff(names(thresholdList), "default")
  idx <- which(error_threshold <= as.numeric(numeric_keys))[1]
  threshold_key <- if (!is.na(idx)) numeric_keys[idx] else "default"
  
  N2 <- thresholdList[[threshold_key]][2]
  
  .warn <- function(total_years, error_threshold_label) {
    if (total_years > 5) {
      
      cat('', msg_warning("!"),
          paste0("Error threshold is set to ", 
                 msg_warning(error_threshold_label), ","), "\n",
          ' ', paste0(
            "Meeting it may require very long tracking durations."))
      
      # Ask user whether to proceed:
      proceed <- readline(
        prompt = "Do you wish to proceed? (y/n): ")
      while (!tolower(proceed) %in% c("y", "n")) {
        proceed <- readline(
          prompt = "Please type 'y' to continue or 'n' to stop: ")
      }
      
      if (tolower(proceed) == "n") stop("Execution stopped by user.")
    }
  }
  
  total_years <- "years" %#% (N1 * taup)
  
  proceed <- TRUE
  if ("hr" %in% obj$set_target) .warn(total_years, error_label)
  
  return(list(proceed = proceed,
              obj = obj,
              n_replicates = n_replicates,
              error_threshold = error_threshold,
              N1 = N1,
              N2 = N2,
              .tol = .tol,
              .n_converge = .n_converge,
              .console_alert = .console_alert,
              .seeds = .seeds,
              ncores = ncores,
              plot = plot,
              verbose = verbose,
              parallel = obj$parallel,
              trace = trace))
}

#' @title Execute optimization
#'
#' @keywords internal
#' @noRd
.md_optimize_execute <- function(prep, trace) {
  
  if (!prep$proceed) {
    message("Execution stopped by user.")
    return(invisible(NULL))
  }
  
  .adjust_percent <- function(x, percent) {
    x * (1 + percent / 100)
  }
  
  .configure_sampling <- function(obj,
                                  optimal_dur,
                                  optimal_dti,
                                  tauv = NULL,
                                  max_hr_obs = 2000,
                                  max_sd_obs = 1100,
                                  max_iter = 20) {
    
    set_target <- obj$set_target
    hr_dur <- hr_dti <- NULL
    ctsd_dur <- ctsd_dti <- NULL
    
    count <- 0
    for (target in set_target) {
      
      if (target == "hr") {
        
        dti <- 1 %#% "day"
        dur <- optimal_dur
        if (dur <= dti) dti <- 1 %#% "hour"
        if (floor(dur / dti) < 5) dti <- 15 %#% "minutes"
        
        repeat {
          n_obs <- floor(dur / dti)
          if (n_obs <= max_hr_obs || count > max_iter) break
          dti <- dti * 2
          count <- count + 1
        }
        
        hr_dur <- fix_unit(dur, "seconds", convert = TRUE)
        hr_dti <- fix_unit(dti, "seconds", convert = TRUE)
      }
      
      if (target == "ctsd") {
        
        stopifnot(!is.null(tauv))
        
        dur <- 8 %#% "days"
        r <- 3 * ((1 / (1 - error_threshold) - 1)^(1 / 1.1))
        dti <- r * tauv
        
        repeat {
          n_obs <- floor(dur / dti)
          if (n_obs <= max_sd_obs || count > max_iter) break
          dur <- dur / 2
          count <- count + 1
          if (dur < 2 %#% "days") break
        }
        
        ctsd_dur <- fix_unit(dur, "seconds", convert = TRUE)
        ctsd_dti <- fix_unit(dti, "seconds", convert = TRUE)
      }
    }
    
    has_hr <- "hr" %in% set_target
    has_ctsd <- "ctsd" %in% set_target
    
    if (all(c("hr", "ctsd") %in% set_target)) {
      obj$dur <- hr_dur
    } else if ("hr" %in% set_target) {
      obj$dur <- hr_dur
    } else if ("ctsd" %in% set_target) {
      obj$dur <- ctsd_dur
    }
    if (all(c("hr", "ctsd") %in% set_target)) {
      obj$dti <- ctsd_dti
    } else if ("hr" %in% set_target) {
      obj$dti <- hr_dti
    } else if ("ctsd" %in% set_target) {
      obj$dti <- ctsd_dti
    }
    
    return(obj)
  }
  
  .worker <- function(r, m, obj, seeds = NULL, trace = FALSE) {
    
    if (trace) writeLines(paste0(
      crayon::yellow("  \u2015\u2015\u2015\u2015\u2015\u2015"),
      " Replicate ", crayon::yellow(r), " out of ",
      crayon::yellow(n_replicates)))
    
    obj$n_individuals <- ifelse(obj$grouped, m * 2, m)
    
    if (is.null(.seeds)) {
      return(md_run(obj, trace = FALSE))
      
    } else {
      
      if (has_groups) {
        idx_start <- (r - 1) * (m * 2) + 1
        idx_end <- r * (m * 2)
      } else {
        idx_start <- (r - 1) * m + 1
        idx_end <- r * m
      }
      
      tmp <- seeds[idx_start:idx_end]
      return(md_run(obj, .seeds = tmp, trace = FALSE))
    }
  }
  
  obj <- prep$obj
  add_ind_var <- obj$add_ind_var
  n_replicates <- prep$n_replicates
  error_threshold <- prep$error_threshold
  
  N1 <- prep$N1
  N2 <- prep$N2
  
  .tol <- prep$.tol
  .n_converge <- prep$.n_converge
  .console_alert <- prep$.console_alert
  
  .seeds <- prep$.seeds
  
  ncores <- prep$ncores
  plot <- prep$plot
  verbose <- prep$verbose
  parallel <- prep$parallel
  trace <- prep$trace
  
  if (!is.null(.seeds) &&
      length(.seeds) != obj$n_individuals * n_replicates) {
    stop("Length of .seeds must equal no. of tags * replicates.")
  }
  
  . <- n <- m <- type <- NULL
  group <- groups <- top_facet <- NULL
  hr_dur <- hr_dti <- ctsd_dur <- ctsd_dti <- NULL
  error_mean <- error_sd <- NULL
  tmp_N_area <- tmp_N_speed <- NA
  
  set_target <- obj$set_target
  has_groups <- obj$grouped
  
  seedInit <- obj$seed
  tau_p <- obj$tau_p
  tau_v <- obj$tau_v
  
  .max_m <- obj$n_individuals
  if (.max_m <= 8) .iter_step <- 2 else .iter_step <- 4
  
  if (obj$add_ind_var) {
    taup <- tau_p[[1]][3, "value"] %#% tau_p[[1]][3, "unit"]
  } else {
    taup <- tau_p[[1]][2, "value"] %#% tau_p[[1]][2, "unit"]
  }
  
  m_seq <- .get_sequence(seq_len(.max_m),
                         .step = .iter_step,
                         .max_m = .max_m,
                         .automate_seq = TRUE,
                         grouped = obj$grouped)
  
  if (length(m_seq) == 0) 
    stop("Error generating sequence of sample sizes.")
  
  if (!is.null(.seeds)) {
    seeds_split <- split(
      .seeds, ceiling(seq_along(.seeds) / 
                        (length(.seeds) / length(m_seq))))
  }
  
  err_prev <- setNames(
    replicate(length(set_target), rep(1, 10), simplify = FALSE),
    set_target)
  
  if (set_target == "hr") 
    tmp_N_area <- suppressWarnings(
      extract_dof(obj$meanfitList, "area"))[[1]]
  if (set_target == "ctsd") 
    tmp_N_speed <- suppressWarnings(
      extract_dof(obj$meanfitList, "speed"))[[1]]
  
  if ("hr" %in% set_target && tmp_N_area < 30) {
    warning(paste0(
      "Effective sample size (for area) ",
      "in the original dataset is low (N = ", 
      round(tmp_N_area, 1), "). Evaluate outputs with caution."))
  }
  
  if ("ctsd" %in% set_target && tmp_N_speed < 30) {
    warning(paste0(
      "Effective sample size (for speed) ",
      "in the original dataset is low (N = ", 
      round(tmp_N_speed, 1), "). Evaluate outputs with caution."))
  }
  
  N1_adj <- N1
  N2_adj <- N2
  
  message(crayon::yellow("Selecting"), " initial parameters...")
  
  if ("hr" %in% set_target) {
    
    optimal_dur <- round(taup * N1_adj)
    obj_updated <- .configure_sampling(obj = obj,
                                       optimal_dur = optimal_dur,
                                       optimal_dti = optimal_dti)
    
  } # end of hr
  
  if ("ctsd" %in% set_target) {
    
    tauv <- tau_v[[1]][2, "value"] %#% tau_v[[1]][2, "unit"]
    optimal_dur <- round(taup * N1_adj)
    optimal_dti <- round(tauv / 3)
    
    obj_updated <- .configure_sampling(
      obj = obj,
      optimal_dur = optimal_dur,
      optimal_dti = optimal_dti,
      tauv = tauv)
    
  } # end of ctsd
  
  message("...Parameters ", crayon::yellow("selected"), ".")
  
  message(paste0(
    "Running ", crayon::yellow("simulations"), " ",
    "for the following ", crayon::cyan("population"), " ",
    "sample sizes:"))
  
  obj <- obj_updated
  obj$add_ind_var <- add_ind_var
  
  msg_seq_txt <- format(m_seq, big.mark = ",", trim = TRUE)
  msg_seq_txt <- paste(crayon::cyan(msg_seq_txt), collapse = ", ")
  msg_seq_txt <- sub(", ([^,]+)$", " and \\1", msg_seq_txt)
  suffix <- ifelse(has_groups, " individuals per group", " individuals")
  
  message(paste0(
    "  ", msg_seq_txt, suffix,
    " (total of ", crayon::yellow(length(m_seq)), " sets)\n"))

  broke <- FALSE
  outList <- vector("list", length(m_seq))
  summaryList <- vector("list", length(m_seq))
  
  for (i in seq_along(m_seq)) {
    
    m_current <- m_seq[[i]]
    m <- m_current - ifelse(i == 1, 0, m_seq[[i - 1]])
    
    writeLines(paste0(
      crayon::yellow("\u2015\u2015\u2015"),
      " Set ", crayon::yellow(i), " out of ",
      crayon::yellow(length(m_seq))))
    
    message(paste(
      crayon::cyan("\u2015\u2015\u2015"),
      "Current sampled population:", 
      if (!is.null(groups))
        crayon::cyan(m_current * 2) else crayon::cyan(m_current), 
      "individual(s)"))
    
    seeds <- NULL
    if (!is.null(.seeds)) seeds <- seeds_split[[i]]
    
    sysname <- Sys.info()[["sysname"]]
    if (sysname != "Windows" && parallel) {
      tmpList <- parallel::mclapply(
        seq_len(n_replicates), function(r) 
          .worker(r, m, obj, seeds, trace = TRUE), mc.cores = ncores)
    } else if (sysname != "Windows" && parallel) {
      cl <- parallel::makeCluster(ncores)
      tmpList <- tryCatch(parallel::parLapply(
        cl, seq_len(n_replicates), function(r)
          .worker(r, m, obj, seeds, trace = TRUE)),
        finally = parallel::stopCluster(cl))
    } else {
      tmpList <- lapply(seq_len(n_replicates), function(r)
        .worker(r, m, obj, seeds, trace = TRUE))
    }
    
    if (i == 1) {
      outList[[i]] <- tmpList
      class(outList[[i]]) <- "movedesign_processed"
    }
    
    nms <- list()
    if (i > 1) {
      
      if (has_groups) {
        
        group_keys <- c("A", "B")
        init_names <- tmpList[[1]]$groups[[1]]
        
        for (x in seq_along(tmpList)) {
          ids_tmp <- tmpList[[x]]$groups[[2]]
          ids_out <- outList[[i - 1]][[x]]$groups[[2]]
          
          merged_ids <- setNames(
            lapply(group_keys, function(gr) {
              c(ids_tmp[[gr]], ids_out[[gr]])
            }), group_keys)
          
          nms[[x]] <- list(init_names, merged_ids)
          tmpList[[x]]$groups <- nms[[x]]
          outList[[i - 1]][[x]]$groups <- nms[[x]]
        }
      }
      
      tmpList <- Map(function(prev, curr) {
        merged <- md_merge(prev, curr, .ignore_mismatch = TRUE)
        merged$n_individuals <- m_current
        return(merged)
      }, outList[[i - 1]], tmpList)
      
      outList[[i]] <- tmpList
      class(outList[[i]]) <- "movedesign_processed"
    }
    
    metaList <- lapply(tmpList, function(x) {
      run_meta_resamples(x,
                         set_target = obj$set_target,
                         subpop = obj$grouped,
                         .m = m_seq[[i]],
                         .only_max_m = TRUE)
    })
    
    if (length(metaList) > 0) {
      summary <- data.table::rbindlist(
        metaList, fill = TRUE, idcol = "replicate")
    } else {
      summary <- data.table::data.table()
    }
    
    summaryList[[i]] <- summary
    
    if (has_groups) {
      data <- summary[summary$group != "All", ]
    } else {
      data <- summary[summary$group == "All", ]
    }
    
    err_replicates <- setNames(
      lapply(set_target, function(target) {
        data[data$type == target, ]$error
      }), set_target)
    
    err_prev <- setNames(
      lapply(names(err_prev), function(target) {
        if (target %in% set_target) {
          tmp <- data[data$type == target, ]$error
          c(err_prev[[target]], abs(mean(tmp, na.rm = TRUE)))
        } else {
          err_prev[[target]]
        }
      }), names(err_prev))
    
    variable <- "error"
    data_subset <- data %>%
      dplyr::group_by(.data$type, .data$group) %>%
      dplyr::mutate(
        cummean = cumsum(.data[[variable]]) / 
          dplyr::row_number()) %>%
      dplyr::ungroup()
    
    # Compute convergence diagnostics:
    
    diag <- data_subset %>%
      dplyr::group_by(.data$type, .data$group) %>%
      dplyr::summarise(
        recent_cummean = list(tail(.data$cummean, .n_converge)),
        recent_deltas = list(abs(diff(.data$recent_cummean[[1]]))),
        max_delta = max(.data$recent_deltas[[1]], na.rm = TRUE),
        has_converged = all(.data$recent_deltas[[1]] < .tol),
        .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$type))
    
    # Break conditions:
    
    err_values <- lapply(err_prev, tail, n = 3)
    err_values <- unlist(err_values)
    
    tmp <- .md_plot_meta(summary,
                         error_threshold = error_threshold,
                         set_target = set_target,
                         has_groups = has_groups)
    
    all_broke_when <- tmp$dt_plot_means %>%
      dplyr::mutate(m = as.numeric(as.character(.data$m))) %>%
      dplyr::inner_join(
        tmp$dt_plot %>%
          dplyr::mutate(m = as.numeric(as.character(.data$m))) %>%
          dplyr::group_by(.data$type, .data$group, .data$m) %>%
          dplyr::summarize(
            all_within_threshold = all(
              abs(.data$error) <= error_threshold),
            .groups = "drop"),
        by = c("type", "group", "m")) %>%
      dplyr::filter(.data$overlaps == TRUE,
                    .data$all_within_threshold == TRUE) %>%
      dplyr::group_by(.data$type, .data$group) %>%
      dplyr::filter(.data$type == set_target) %>%
      # dplyr::filter(.data$group == groups[[g]]) %>%
      dplyr::slice_min(.data$m, n = 1) %>%
      dplyr::ungroup()
    
    broke_when <- tmp$dt_plot_means %>%
      dplyr::filter(abs(.data$error) <= error_threshold) %>%
      dplyr::group_by(.data$type, .data$group) %>%
      dplyr::arrange(.data$m, .by_group = TRUE) %>%
      dplyr::filter(.data$type == set_target) %>%
      # dplyr::filter(.data$group == groups[[g]]) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
    
    error <- tmp$dt_plot_means %>%
      dplyr::filter(.data$type == set_target) %>%
      # dplyr::filter(.data$group == groups[[g]]) %>%
      .summarize_error(error_threshold = error_threshold) %>%
      dplyr::slice_max(.data$m) %>%
      dplyr::pull(.data$error)
    
    error_ok <- abs(error) <= error_threshold
    all_error_ok <- !all(is.na(all_broke_when$m))
    if (length(all_error_ok) == 0) all_error_ok <- FALSE
    
    if (obj$which_meta == "mean") {
      
      # overlaps_with_truth <- dplyr::between(
      #   unique(data$truth),
      #   mean(data$lci, na.rm = TRUE),
      #   mean(data$uci, na.rm = TRUE))
      
      if (error_ok && all_error_ok && diag$has_converged) {
        broke <- TRUE
        break
      }
      
    } # end of if (which_meta == "mean")
    
    if (obj$which_meta == "compare") {
      
      cov <- Inf
      if (all(error_ok) && all(all_error_ok) &&
          all(diag$has_converged)) {
        
        input <- .get_groups(obj$fitList,
                             groups = obj$groups[[1]])
        
        out_cov <- list()
        overlaps_with_truth <- list()
        ratios <- list()
        for (target in set_target) {
          
          out_meta <- list()
          out_meta_truth <- list()
          if (target == "hr") variable <- "area"
          if (target == "ctsd") variable <- "speed"
          
          out_meta_truth[[target]] <- .capture_meta(
            input,
            variable = variable,
            units = FALSE,
            verbose = TRUE,
            plot = FALSE) %>%
            suppressMessages() %>%
            quiet()
          
          out_cov[[target]] <- c()
          overlaps_with_truth[[target]] <- c()
          ratios[[target]] <- c()
          
          for (tmp in tmpList) {
            
            if (target == "hr") tmp_input <- tmp$akdeList
            if (target == "ctsd") tmp_input <- tmp$ctsdList
            
            tmp_input <- list(
              tmp_input,
              .get_groups(tmp_input,
                          groups = tmp$groups[[2]]))
            names(tmp_input) <- c("All", "groups")
            
            out_meta[[target]] <- setNames(
              lapply(tmp_input, function(x) {
                return(.capture_meta(x,
                                     variable = variable,
                                     sort = TRUE,
                                     units = FALSE,
                                     verbose = TRUE,
                                     plot = FALSE) %>%
                         suppressMessages())
              }), names(tmp_input))
            
            tmp_data <- dplyr::filter(data, .data$type == target)
            if (!is.na(tmp_data[nrow(tmp_data), ]$est)) {
              cov <- out_meta[[target]][["All"]]$meta[
                grep("CoV", rownames(
                  out_meta[[target]][["All"]]$meta)), 2][[2]]
              out_cov[[target]] <- c(out_cov[[target]], cov)
            } else {
              out_cov[[target]] <- c(out_cov[[target]], 0)
            }
            
            if (!is.null(out_meta[[target]][["groups"]])) {
              
              ratios[[target]] <- c(
                ratios[[target]],
                .get_ratios(out_meta[[target]][["groups"]])$est)
              
              overlaps_with_truth[[target]] <- c(
                overlaps_with_truth[[target]],
                dplyr::between(
                  .get_ratios(out_meta[[target]][["groups"]])$est,
                  .get_ratios(out_meta_truth[[target]])$lci,
                  .get_ratios(out_meta_truth[[target]])$uci))
              
            } else {
              ratios[[target]] <- c(ratios[[target]], NA)
              overlaps_with_truth[[target]] <- c(
                overlaps_with_truth[[target]], FALSE)
            }
            
          } # end of [tmp] loop (tmList)
          
        } # end of [target] loop
        
        cov <- lapply(out_cov, tail, n = 1)
        
        # if cov -> infinity,
        # still sensitive to small changes in the mean.
        if (!all(is.infinite(unlist(cov)))) {
          broke <- TRUE
          break
        }
      }
      
    } # end of if (which_meta == "compare")
    
  } # end of [i] loop
  
  has_converged <- TRUE
  if (any(!diag$has_converged)) {
    
    has_converged <- FALSE
    warning(sprintf(
      "Failing to converge with %s replicates. %s",
      n_replicates, "Consider increasing 'n_replicates'."
    ), call. = FALSE)
    
  }
  
  if (obj$grouped) {
    group_keys <- c("A", "B")
    init_names <- tmpList[[1]]$groups[[1]]
    merged_ids <- lapply(tmpList, function(x) x$groups[[2]])
    merged_ids <- Reduce(function(x, y) Map(c, x, y), merged_ids)
    
    for (x in seq_along(tmpList)) {
      tmpList[[x]]$groups <- list(init_names, merged_ids)
    }
  }
  
  merged <- md_merge(tmpList)
  class(merged) <- unique(c("movedesign_output", class(merged)))
  
  names(merged$seedList) <- unlist(merged$seedList)
  merged$n_replicates <- n_replicates
  merged$seedInit <- seedInit
  
  if (!is.null(.seeds)) {
    target_names <- as.character(.seeds)
    
    if (!setequal(target_names, names(merged$seedList))) {
      stop("Seed mismatch detected.\n",
           "Only in merged: ",
           paste(setdiff(names(merged$seedList),
                         target_names), collapse = ", "),
           "\nOnly in target: ",
           paste(setdiff(target_names, 
                         names(merged$seedList)), collapse = ", "))
    }
    
    if (anyDuplicated(names(merged$seedList)) ||
        anyDuplicated(target_names)) {
      stop("Duplicate seeds detected")
    }
    
    merged$seedList <- merged$seedList[target_names]
    merged$simList <- merged$simList[target_names]
    merged$simfitList <- merged$simfitList[target_names]
    merged$akdeList <- merged$akdeList[target_names]
    merged$ctsdList <- merged$ctsdList[target_names]
  }
  
  message(" ")
  message(.msg(paste(
    "Running meta-analyses",
    "for the following population sample sizes:"), "success"))
  msg_seq <- format(m_seq, big.mark = ",", trim = TRUE)
  msg_seq <- paste(msg_seq, collapse = ", ")
  msg_seq <- sub(", ([^,]+)$", " and \\1", msg_seq)
  suffix <- ifelse(has_groups, " individuals per group", " individuals")
  message(.msg(sprintf("  %s%s (total of %d sets)\n",
                       msg_seq, suffix, length(m_seq)), "success"))
  
  start_meta_total <- Sys.time()
  print(sprintf("Start time: %s",
                format(start_meta_total, "%Y-%m-%d %H:%M:%S %Z")))
  
  metaList <- list()
  for (i in seq_along(m_seq)) {
    message(.msg(
      sprintf("Set %s out of %s...", i, length(m_seq)),
      "success"))
    
    tmp <- lapply(
      seq_len(n_replicates), function(x) {
        
        run_meta_resamples(merged,
                           set_target = merged$set_target,
                           subpop = merged$grouped,
                           .m = m_seq[[i]],
                           .seed = seedInit + x)
      })
    
    if (length(tmp) > 0) {
      metaList[[i]] <- data.table::rbindlist(
        tmp, fill = TRUE, idcol = "replicate")
    } else {
      metaList[[i]] <- data.table::data.table()
    }
    
    message(.msg("...done.", "success"))
    
    elapsed_meta <- as.numeric(difftime(
      Sys.time(), start_meta_total, units = "secs"))
    elapsed_meta_hms <- sprintf("%02d:%02d:%02d (hh:mm:ss)",
                                elapsed_meta %/% 3600,
                                (elapsed_meta %% 3600) %/% 60,
                                round(elapsed_meta %% 60))
    
    message("Elapsed time: ",
            .msg(crayon::bold(elapsed_meta_hms), "success"))
  }
  
  summary_full <- data.table::rbindlist(metaList, fill = TRUE)
  out_plot <- .md_plot_meta(summary_full,
                            error_threshold = error_threshold,
                            set_target = set_target,
                            has_groups = has_groups)
  if (plot) print(out_plot$plot)
  
  if (trace) {
    
    .make_header <- function(title, n_dash = 10) {
      header_line <- paste0(strrep("\u2500", n_dash), " ", title, ":")
      return(crayon::bold(header_line))
    }
    
    .make_line <- function(label, value, width = 45L) {
      pad <- width - nchar(crayon::strip_style(label))
      dots <- paste(rep(".", max(pad, 1L)), collapse = "")
      paste0("     ", label, dots, " ", value)
    }
    
    .color <- function(x, ok) {
      if (ok) crayon::yellow(x) else crayon::red(x)
    }
    
    groups <- if (has_groups) c("A", "B") else c("All")
    
    report <- c(
      "", .make_header("Study design performance", 4), "")
    
    for (g in seq_along(groups)) {
      
      all_broke_when <- out_plot$dt_plot_means %>%
        dplyr::inner_join(
          out_plot$dt_plot %>%
            dplyr::mutate(m = as.numeric(as.character(.data$m))) %>%
            dplyr::group_by(.data$type, .data$group, .data$m) %>%
            dplyr::summarize(
              all_within_threshold = all(
                abs(.data$error) <= error_threshold),
              .groups = "drop"),
          by = c("type", "group", "m")) %>%
        dplyr::filter(.data$overlaps == TRUE,
                      .data$all_within_threshold == TRUE) %>%
        dplyr::group_by(.data$type, .data$group) %>%
        dplyr::filter(.data$type == set_target) %>%
        dplyr::filter(.data$group == groups[[g]]) %>%
        dplyr::slice_min(.data$m, n = 1) %>%
        dplyr::ungroup()
      
      broke_when <- out_plot$dt_plot_means %>%
        dplyr::filter(abs(.data$error) <= error_threshold) %>%
        dplyr::group_by(.data$type, .data$group) %>%
        dplyr::arrange(.data$m, .by_group = TRUE) %>% 
        dplyr::filter(.data$type == set_target) %>%
        dplyr::filter(.data$group == groups[[g]]) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()
    
      # Population size that achieves mean error below threshold:
      error_ok <- !is.na(broke_when$m)
      if (length(error_ok) == 0) error_ok <- FALSE
      error_ok_m <- if (error_ok) broke_when$m else NA
      
      # Population size that achieves all replicates below threshold:
      all_error_ok <- !is.na(all_broke_when$m)
      if (length(all_error_ok) == 0) all_error_ok <- FALSE
      all_error_ok_m <- if (all_error_ok) all_broke_when$m else NA
      
      if (error_ok && all_error_ok) {
        start_text <- "Recommended sampling"
        if (g == 1) {
          report <- c(
            report,
            .make_line(
              paste("Minimum",
                    .color("population", all_error_ok), "sample size"),
              .color(all_error_ok_m, all_error_ok)))
        }
      } else if (error_ok && !all_error_ok) {
        start_text <- "Sampling"
        if (g == 1) {
          report <- c(
            report,
            .make_line(
              paste("Minimum",
                    .color("population", error_ok), "sample size"),
              .color(error_ok_m, error_ok)))
        }
      } else {
        start_text <- "Sampling"
        if (g == 1) {
          report <- c(
            report,
            .make_line(
              paste(crayon::red("Population"), "sample size"),
              paste(crayon::red(.max_m),
                    crayon::red("[insufficient]"))))
        }
      }
      
      if (g == 1) {
        report <- c(
          report,
          .make_line(
            paste(start_text,
                  .color("duration", error_ok)),
            .color(paste0(round(obj$dur$value, 1),
                          " ", obj$dur$unit), error_ok)),
          .make_line(
            paste(start_text, 
                  .color("interval", error_ok)),
            .color(paste0(round(obj$dti$value, 1),
                          " ", obj$dti$unit), error_ok)))
      }
      
      if (length(groups) > 1) {
        report <- c(
          report,
          c("",
            paste0("                               ",
                   "                                ", 
                   crayon::yellow("Group", groups[[g]])),
            paste0("     ", crayon::yellow(
              paste(rep("\u2500", 65L), collapse = "")))))
      }
      
      if (error_ok && all_error_ok) {
        report <- c(
          report,
          "",
          paste0("     ",
                 crayon::yellow("\u2713 "), "Mean relative error ",
                 crayon::yellow("within threshold"), " (\u00B1",
                 round(error_threshold * 100, 0), "%) achieved at m = ",
                 crayon::yellow(error_ok_m)))
        
        n_outside_threshold <- out_plot$dt_plot %>%
          dplyr::group_by(.data$type, .data$group) %>%
          dplyr::filter(.data$m == error_ok_m) %>%
          dplyr::summarise(
            n_outside_threshold = sum(
              abs(.data$error) > error_threshold, na.rm = TRUE),
            n_replicates = dplyr::n(),
            .groups = "keep") %>%
          dplyr::pull(n_outside_threshold)
        set_m <- error_ok_m
        
      } else {
        report <- c(
          report,
          if (!has_groups) "" else NULL,
          paste0("     ",
                 crayon::red("\u2717 "), "Mean relative error ",
                 crayon::red("outside threshold"), " (\u00B1",
                 round(error_threshold * 100, 0), "%)"),
          paste0("     ", "Increase ",
                 crayon::red("population"), " sample size!"))
        
        n_outside_threshold <- out_plot$dt_plot %>%
          dplyr::group_by(.data$type, .data$group) %>%
          dplyr::mutate(m = as.numeric(.data$m)) %>%
          dplyr::filter(.data$m == max(.data$m, na.rm = TRUE)) %>%
          dplyr::summarise(
            n_outside_threshold = sum(
              abs(.data$error) > error_threshold, na.rm = TRUE),
            n_replicates = dplyr::n(),
            .groups = "keep") %>%
          dplyr::pull(n_outside_threshold)
        set_m <- .max_m
      }
      
      if (all_error_ok) {
        extra_words <- ifelse(error_ok, "All", "However, all")
        report <- c(
          report,
          paste0("     ",
                 crayon::yellow("\u2713 "), extra_words, " replicates ",
                 crayon::yellow("within threshold"), " (\u00B1",
                 round(error_threshold * 100, 0), "%) achieved at m = ",
                 crayon::yellow(all_error_ok_m)))
        
      } else {
        
        set_m <- if (has_groups) set_m/2 else set_m
        extra_words <- ifelse(error_ok, "However, with", "With")
        report <- c(
          report,
          paste0("     ",
                 crayon::red("\u2717 "), extra_words, " ",
                 crayon::red(set_m), " individuals, ",
                 crayon::red(n_outside_threshold[[g]]),
                 " of ", crayon::red(n_replicates),
                 " replicates exceeded threshold."))
      }
    }
    
    writeLines(report)
  }
  
  if (has_groups) {
    out_summary <- summary_full %>%
      dplyr::select(
        "type", "m", "replicate",
        "error", "error_lci", "error_uci",
        "ratio_est", "ratio_lci", "error_uci",
        "is_grouped", "group", "subpop_detected")
  } else {
    out_summary <- summary_full %>%
      dplyr::select(
        "type", "m", "replicate",
        "is_grouped", "group", "error", "error_lci", "error_uci")
  }
  
  out <- structure(
    list(data = merged,
         summary = summary_full,
         verbose = verbose,
         diagnostics_table = diag,
         plot_data = out_plot$dt_plot,
         plot = out_plot$plot,
         error_threshold = error_threshold,
         sampling_duration = paste0(
           round(obj$dur$value, 1), " ", obj$dur$unit),
         sampling_interval = paste0(
           round(obj$dti$value, 1), " ", obj$dti$unit),
         sample_size_achieved = broke,
         minimum_population_sample_size = m_seq[[i]]
    ), class = "movedesign")
  class(out) <- unique(c("movedesign_optimized", class(out)))
  
  return(out)
  
}


#' @title Optimize sampling parameters and population sample size
#'
#' @description
#' Repeatedly simulates movement datasets across a range of
#' candidate population sample sizes to identify the minimal sample size
#' and associated sampling parameters (i.e., duration, sampling interval)
#' needed to achieve estimates for key movement and space-use metrics 
#' (e.g., home range area, speed) within the specified relative error
#' threshold.
#'
#' The function quantifies estimation error for each metric and sample
#' size, evaluating which population sample size reliably meet target
#' thresholds, and reports final recommendations.
#'
#' @note
#' Some biologgers inherently involve a trade-off between fix frequency
#' and battery life. Shorter intervals between location fixes offer higher
#' temporal resolution but reduce deployment duration due to increased
#' power consumption. In contrast, longer deployments require less
#' frequent sampling to conserve battery.
#' 
#' This trade-off makes it challenging to estimate multiple metrics with
#' differing data needs: high-resolution data (shorter intervals) improve
#' speed estimation, while extended deployments (longer durations) are
#' vital for accurate home range area estimates. A sampling design that
#' minimizes error for one metric may increase error for another.
#' 
#' Researchers facing these constraints should consider prioritizing a
#' single research target (e.g., either home range area *or* speed), or
#' use stratified designs to balance data needs across individuals.
#' 
#' @details
#' The function iteratively runs movement design simulations for
#' increasing population sample sizes (`m`), evaluating error for
#' each replicate and metric via meta-analyses. Convergence is checked
#' using the error threshold and stability of cumulative mean error.
#' The function stops when a sample size meets all criteria or the
#' maximum population sample size is reached. Results can be visualized
#' using if `plot = TRUE`.
#' 
#' @param obj A movement design input object (see [`md_prepare()`]
#'   or [`md_configure()`]).
#' @param n_replicates Integer. Number of simulation replicates at each
#'   candidate sample size.
#' @param error_threshold Numeric. Upper limit of the relative error
#'   in estimation (e.g., `0.05` for 5%) deemed acceptable by the user.
#'   The function will attempt to find sampling parameters and sample 
#'   sizes that keep errors below this threshold.
#' @param verbose Logical. If `TRUE` (default), prints a summary
#'   of the convergence check to the console.
#' @param trace Logical; if `TRUE` (default), prints progress and
#'   timing messages to the console.
#' @param parallel Logical; if `TRUE`, enables parallel processing.
#'   Default is `FALSE`.
#' @param ncores Integer; number of CPU cores to use for parallel
#'   processing. Defaults to all available cores detected by
#'   `parallel::detectCores()`.
#' @param plot Logical. If TRUE, displays a diagnostic plot of
#'   the final results.
#' @param ... Reserved for internal use.
#' 
#' @return A list of class `movedesign_report` containing:
#' \itemize{
#'   \item `summary`: Data frame of summary statistics for each
#'     replicate, sample size, and metric.
#'   \item `error_threshold`: Numeric. The error threshold used.
#'   \item `sampling_duration`: Character string. Final sampling duration.
#'   \item `sampling_interval`: Character string. Final sampling interval.
#'   \item `sample_size_achieved`: Logical. Indicates if convergence was
#'     achieved and the threshold met.
#'   \item `minimum_population_sample_size`: Integer. Minimum sample size
#'     achieving the threshold (or maximum evaluated if
#'     `sample_size_achieved` is `FALSE`).
#' }
#' 
#' @examples
#' if(interactive()) {
#'   obj <- md_configure(data = buffalo,
#'                       models = models)
#'                       
#'   out <- md_optimize(tmp,
#'                      error_threshold = 0.05,
#'                      plot = TRUE)
#' }
#' 
#' @seealso
#' [`md_prepare()`], [`md_configure()`]
#' 
#' @export
md_optimize <- function(obj,
                        n_replicates = 10,
                        error_threshold = 0.05,
                        plot = FALSE,
                        verbose = TRUE,
                        parallel = FALSE,
                        ncores = parallel::detectCores(),
                        trace = TRUE,
                        ...) {
  
  if (!inherits(obj, c("movedesign_input", "movedesign"))) {
    stop("Input must have class 'movedesign_input'.")
  }
  
  if (length(obj$set_target) != 1 ||
      !obj$set_target %in% c("hr", "ctsd")) {
    stop("Invalid `set_target`: expected only one of {'hr', 'ctsd'}",
      call. = FALSE)
  }
  
  .check_numeric(error_threshold, lower = 0.01, upper = 0.99)
  .check_integer(ncores)
  .check_logical(plot)
  .check_logical(verbose)
  .check_logical(parallel)
  .check_logical(trace)
  
  stopifnot(is.numeric(n_replicates) && n_replicates > 0)

  if (n_replicates < 10)
    stop("`n_replicates` must be set to at least 10.")
  
  start_total <- Sys.time()
  message("Execution started: ", 
          crayon::yellow(crayon::bold(
            format(start_total, "%Y-%m-%d %H:%M:%S %Z"))))
  
  init <- .md_optimize_setup(obj = obj,
                             n_replicates = n_replicates,
                             error_threshold = error_threshold,
                             plot,
                             verbose,
                             parallel,
                             ncores,
                             trace,
                             ...)
  
  if (!init$proceed) {
    message(
      "`md_optimize()` execution halted by user at warning stage.")
    return(invisible(NULL))
  }
  
  out <- .md_optimize_execute(init, trace = trace)
  
  message("\nExecution ended: ", 
          crayon::yellow(crayon::bold(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))))
  
  elapsed <- as.numeric(difftime(
    Sys.time(), start_total, units = "secs"))
  elapsed_hms <- sprintf("%02d:%02d:%02d (hh:mm:ss)",
                         elapsed %/% 3600,
                         (elapsed %% 3600) %/% 60,
                         round(elapsed %% 60))
  
  message("      Total elapsed time: ",
          crayon::yellow(crayon::bold(elapsed_hms)))
  
  
  return(out)
  
}


#' @title Plot estimation error, replicates and confidence intervals
#'
#' @description
#' This function produces two complementary visualizations of replicate
#' performance across a range of population sample sizes \eqn{m}, up to
#' the maximum number of individuals requested. It
#' summarizes and plots relative errors regarding the estimation of a
#' set target (*e.g.*, home range or speed estimation), and classified by
#' whether the error falls within a user-defined acceptable threshold.
#'
#' \describe{
#'   \item{"Estimation error (for a single random replicate)"}{
#'     Error for one randomly-selected replicate, rendered as
#'     point estimates with confidence interval bars. Reflects
#'     the outcome a researcher would observe from a *single*
#'     empirical study.
#'   }
#'   \item{"Mean estimation error across all replicates"}{
#'     Mean error collapsed across **all** replicates (with confidence
#'     intervals as a narrow bar, and prediction intervals as a wide
#'     shaded band), with per-replicate jitter in the background.
#'     Conveys the full distribution of outcomes with the set
#'     sampling parameters and population sample size.
#'   }
#' }
#'
#' Both panels share a common colour palette (within/outside the
#' acceptable error threshold) and, when two groups are present,
#' a common shape scale.
#' 
#' @param obj A movement design output object (returned by
#'   either [`md_replicate()`] or [`md_optimize()`]).
#' @param ci Confidence level for the intervals. Applied to both
#'   the narrow confidence bars and wide prediction bands. Must be
#'   between `0` and `1`. Default: `0.95` (95%).
#' @param view Layout selector. Indicate whether to return the
#'   complete two-panel layout (`"both"`) or only the aggregated
#'   summary plot with all replicates (`"summary_only"`).
#' @param pal Character vector of two valis hex color code for
#'   within and for outside the threshold
#'   (default: `c("#007d80", "#A12C3B"))`.
#' @param ... Reserved for internal use.
#' 
#' @return A `patchwork` / `ggplot` object containing:
#' \itemize{
#'   \item Top plot: A `ggplot` object displaying the results from a
#'     single randomly selected replicate, showing individual error
#'     estimates and their confidence intervals.
#'   \item Bottom plot: A `ggplot` object summarizing mean relative
#'     error across all replicates, with aggregated estimates in the
#'     foreground and individual replicates shown in lighter tones in
#'     the background.
#' }
#'
#' @examples
#' if(interactive()) {
#'   
#'   obj <- md_replicate(...)
#'   
#'   # Default: both panels
#'   md_plot_replicates(obj)
#'   
#'   # Summary panel, custom palette, 80% CI:
#'   md_plot_replicates(
#'     obj,
#'     ci = 0.90,
#'     view = "summary_only",
#'     pal = c("#1e2a38", "#9e3419"))
#'
#' }
#' 
#' @seealso
#' [md_plot()] for the density plot for the maximum \eqn{m}. \cr
#' [md_replicate()] to produce a `movedesign_output`. \cr
#' [md_optimize()] to produce a `movedesign_optimized`.
#' 
#' @importFrom dplyr %>%
#' 
#' @export
md_plot_replicates <- function(obj,
                               ci = 0.95,
                               view = "summary_only",
                               pal = c("#007d80", "#A12C3B"),
                               ...) {
  
  dots <- list(...)
  .font <- dots[[".font"]] %||% NULL
  
  view_opts <- c("both", "summary_only")
  if (!view %in% view_opts) {
    stop(sprintf(
        "Invalid 'view'. Must be one of: %s",
        paste(view_opts, collapse = ", ")), call. = FALSE)
  }
  
  if (!is.character(pal) || length(pal) != 2) {
    stop("`pal` must be a character vector of exactly two colors.", 
         call. = FALSE)
  }
  if (any(!grepl("^#(?:[0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})$", pal))) {
    stop("`pal` must contain valid hexadecimal color codes.",
         call. = FALSE)
  }
  
  if (!inherits(obj, "movedesign_optimized") &&
      !inherits(obj, "movedesign_output")) {
    
    stop("`obj` must be either a ",
         "'movedesign_output' (from `md_replicate()`) or a ",
         "'movedesign_optimized' object (from `md_optimize()`).",
         call. = FALSE)
  }
  
  top_facet <- NULL
  type <- overlaps <- NULL
  n <- error_mean <- error_sd <- NULL
  
  error_threshold <- obj$error_threshold
  max_replicates <- obj$data$n_replicates
  
  pal <- list("TRUE" = pal[1], "FALSE" = pal[2])
  
  label_ci <- paste0(ci * 100, "%")
  label_threshold <- sprintf("%s%%", error_threshold * 100)
  labels_target <- c("hr" = "Home range area estimation",
                     "ctsd" = "Speed \u0026 distance estimation")
  
  # Prepare data:
  
  out <- obj$summary %>%
    dplyr::mutate(
      overlaps = factor(
        abs(.data$error) <= error_threshold,
        levels = c(TRUE, FALSE)),
      highlight = FALSE)
  
  has_groups <- "group" %in% names(out) &&
    all(c("A", "B") %in% unique(as.character(out$group)))
  
  if (has_groups) {
    out <- out[out$group != "All", ]
    set_shapes <- c(21, 24)
    set_shapes_manual <- c(16, 17)
  } else {
    out <- out[out$group == "All", ]
    set_shapes <- c(21, 21)
    set_shapes_manual <- c(16, 16)
  }
  
  out_mean <- .summarize_error(out, conf_level = ci,
                               error_threshold = error_threshold)
  
  set_target <- unique(out$type)
  max_m <- max(unique(out$m))
  
  out_one <- dplyr::filter(
    out, .data$replicate == sample(max_replicates, 1))
  out_one$facet <- "original"
  out_mean$facet <- "resampled"
  
  if (view == "both") {
  out <- out %>%
    dplyr::mutate(
      highlight = .data$replicate %in% out_one$replicate &
        .data$m %in% out_one$m &
        .data$group %in% out_one$group)
  }
  
  if (!(max_m %% 2)) {
    out_one <- dplyr::filter(out_one, .data$m %% 2 == 0)
    out_mean <- dplyr::filter(out_mean, .data$m %% 2 == 0)
  }
  
  only_one_m <- ifelse(length(unique(out$m)) == 1, TRUE, FALSE)
  if (only_one_m) out$m <- factor(out$m)
  
  m_resampled <- out_mean$m[out_mean$n > 1L]
  
  # Plot with a single replicate:
  
  p <- out_one %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$m,
                   y = .data$error,
                   group = .data$group,
                   shape = .data$group,
                   color = .data$overlaps)) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    ggplot2::annotate(
      "rect",
      xmin = -Inf, xmax = Inf,
      ymin = -error_threshold,
      ymax = error_threshold,
      alpha = 0.05) +
    ggplot2::geom_hline(
      yintercept = c(-error_threshold, error_threshold),
      linetype = "dashed",
      linewidth = 0.35, alpha = 0.8) +
    ggplot2::annotate(
      "text",
      x = Inf, y = error_threshold,
      label = paste0("+", label_threshold),
      hjust = 1.08, vjust = -0.5,
      size = 3.2) +
    ggplot2::annotate(
      "text",
      x = Inf, y = -error_threshold,
      label = paste0("\u2212", label_threshold),
      hjust = 1.08, vjust = 1.5,
      size = 3.2) +
    
    # Confidence intervals:
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$error_lci,
                   ymax = .data$error_uci),
      position = if (has_groups)
        ggplot2::position_dodge(width = 0.5) else 
          ggplot2::position_identity(),
      linewidth = 1.1,
      alpha = 0.55,
      show.legend = FALSE) +
    
    ggplot2::geom_point(
      position = if (has_groups)
        ggplot2::position_dodge(width = 0.5) else 
          ggplot2::position_identity(),
      size = 4,
      show.legend = TRUE) +
    
    { if (length(set_target) > 1)
      ggplot2::facet_wrap(
        . ~ .data$type, scales = "free_y",
        labeller = ggplot2::labeller(type = labels_target))
    } +
    
    ggplot2::labs(
      title = expression("Estimation error (for a single " *
                           italic("random") * " replicate)"),
      x = "Population sample size",
      y = "Relative error (%)",
      color = paste0("Within error threshold (\u00B1",
                     error_threshold * 100, "%)?")) +
      # caption = sprintf(
      #   "Narrow bars: %s confidence interval", label_ci)) +
    
    { if (length(unique(out$m)) == 2) {
      ggplot2::scale_x_continuous(
        breaks = unique(out$m),
        expand = ggplot2::expansion(mult = c(0.10, 0.10)))
    } else if (!only_one_m) {
      ggplot2::scale_x_continuous(
        breaks = scales::breaks_pretty(),
        expand = ggplot2::expansion(mult = c(0.10, 0.10)))
    }
    } +
    
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      breaks = scales::breaks_pretty(),
      expand = ggplot2::expansion(mult = c(0.10, 0.10))) +
    ggplot2::scale_color_manual(
      values = c(pal[1], pal[2]),
      drop = FALSE) +
    
    { if (has_groups)
      ggplot2::scale_shape_manual(
        "Groups:", values = c(16, 17),
      ) } +
    
    { if (has_groups)
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = 22, size = 4,
            fill = c(pal[1], pal[2])))) } +
    { if (has_groups)
      ggplot2::guides(
        shape = ggplot2::guide_legend(
          override.aes = list(size = 4))) } +
    
    .theme_movedesign_report(font = .font)
  
  if (!has_groups) p <- p + ggplot2::guides(shape = "none")
  
  # Plot with all the replicates:
  
  p.replicates <- out_mean %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$m,
                   y = .data$error,
                   group = .data$group,
                   shape = .data$group,
                   color = .data$overlaps)) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    ggplot2::annotate(
      "rect",
      xmin = -Inf, xmax = Inf,
      ymin = -error_threshold,
      ymax = error_threshold,
      alpha = 0.05) +
    ggplot2::geom_hline(
      yintercept = c(-error_threshold, error_threshold),
      linetype = "dashed",
      linewidth = 0.35, alpha = 0.8) +
    ggplot2::annotate(
      "text",
      x = Inf, y = error_threshold,
      label = paste0("+", label_threshold),
      hjust = 1.08, vjust = -0.5,
      size = 3.2) +
    ggplot2::annotate(
      "text",
      x = Inf, y = -error_threshold,
      label = paste0("\u2212", label_threshold),
      hjust = 1.08, vjust = 1.5,
      size = 3.2) +
    
    { if (!only_one_m)
      ggplot2::geom_jitter(
        data = dplyr::filter(out, .data$m %in% m_resampled),
        mapping = ggplot2::aes(x = .data$m,
                               y = .data$error,
                               group = .data$group,
                               shape = .data$group,
                               color = .data$overlaps,
                               alpha = .data$highlight),
        position = ggplot2::position_jitterdodge(
          dodge.width = 0.8),
        color = "grey50", size = 3) } +
        # color = "grey80",  alpha = 0.9) } +
    
    # Prediction intervals:
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$pred_lci,
                   ymax = .data$pred_uci),
      position = ggplot2::position_dodge(width = 0.5),
      linewidth = 3,
      alpha = 0.18,
      show.legend = FALSE) +
    
    # Confidence intervals:
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$error_lci,
                   ymax = .data$error_uci),
      position = ggplot2::position_dodge(width = 0.5),
      linewidth = 1.1,
      alpha = 0.55,
      show.legend = FALSE) +
    
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.5),
      size = 4,
      show.legend = TRUE) +
    
    { if (length(set_target) > 1)
      ggplot2::facet_wrap(
        . ~ .data$type, scales = "free_y",
        labeller = ggplot2::labeller(type = labels_target))
    } +
    
    ggplot2::labs(
      title = expression(
        "Mean estimation error across " *
          italic("all") * " replicates"),
      x = "Population sample size",
      y = "Relative error (%)",
      color = sprintf(
        "Within error threshold (\u00B1%g%%)?",
        error_threshold * 100)) +
      # caption = sprintf(
      #   paste0(
      #     "Wide bars: %s prediction interval  \u2022  ",
      #     "Narrow bars: %s confidence interval\n",
      #     "Based on %d resamples"),
      #   label_ci, label_ci, max_draws)) +
    
    { if (length(unique(out$m)) == 2) {
      ggplot2::scale_x_continuous(
        breaks = unique(out$m))
    } else if (!only_one_m) {
      ggplot2::scale_x_continuous(
        breaks = scales::breaks_pretty())
    }
    } +
    
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      breaks = scales::breaks_pretty(),
      expand = ggplot2::expansion(mult = c(0.05, 0.10))) +
    ggplot2::scale_color_manual(
      values = c(pal[1], pal[2]),
      # labels = c("TRUE" = "Yes", "FALSE" = "No"),
      drop = FALSE) +
    
    { if (has_groups)
      ggplot2::scale_shape_manual(
        "Groups:", values = c(16, 17)) } +
    
    { if (view == "both") {
      ggplot2::scale_alpha_continuous(
        range = c(0.2, 0.9)) 
    } else {
      ggplot2::scale_alpha_continuous(
        range = c(0.3, 0.3)) 
    }} +
    
    { if (has_groups)
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = 22, size = 4,
            fill = c(pal[1], pal[2])))) } +
    
    .theme_movedesign_report(font = .font)
  
  p.replicates <- p.replicates + ggplot2::guides(alpha = "none")
  
  if (!has_groups) {
    p.replicates <- p.replicates + ggplot2::guides(shape = "none")
  } else {
    p <- p + ggplot2::guides(shape = "none")
  }
  
  if (length(obj$data$set_target) == 2)
    p <- p + ggplot2::guides(color = "none")
  
  if (view == "both") {
    
    p.replicates <- p.replicates +
      ggplot2::theme(legend.position = "none")
    
    plots <- list(p, p.replicates)
    
    return(suppressWarnings(print(
      patchwork::wrap_plots(
        plots,
        nrow = length(plots),
        guides = "collect") +
        patchwork::plot_annotation(
          theme = ggplot2::theme(legend.position = "bottom")))))
    
  } else {
    return(suppressWarnings(print(p.replicates)))
  }
  
}


#' @title Compare estimation error across designs (single replicate)
#'
#' @description
#' Plots estimation error of the chosen targets for two or more
#' study designs, each represented by a single [md_run()] output.
#' Use this function to quickly compare how design choices
#' affect estimation performance before committing to a full
#' replication with [md_replicate()].
#'
#' Because each design is represented by a single stochastic run,
#' results are preliminary. For robust, publication-ready
#' comparisons, run [md_replicate()] for each design and compare
#' with [md_compare()].
#'
#' @param x A list of at least two `movedesign_processed` objects,
#'   each returned by [md_run()]. Each element represents one study
#'   design to compare. Designs typically differ in sampling
#'   parameters such as `dur`, `dti`, or `n_individuals`, but any
#'   valid [md_prepare()] or [md_simulate()] inputs can be compared.
#'   
#' @param n_resamples A single positive integer. The number of
#'   random combinations of individuals generated at each population
#'   sample size per design. Each combination produces one
#'   population-level estimate. Set to `NULL` to plot
#'   raw estimates without resampling.
#'   
#' @param error_threshold Numeric. Relative error threshold shown
#'   as a horizontal reference line in the plot (e.g. `0.05` for 5%).
#'   
#' @param pal A character vector of two colours, used for estimates
#'   within and outside the error threshold respectively.
#'   Defaults to `c("#007d80", "#A12C3B")`.
#'   
#' @param ... Reserved for internal use.
#' 
#' @return
#' A `ggplot` object. Displays relative error as a function of
#' population sample size, with one panel (or two if two target) 
#' per design. Point estimates, confidence intervals, and a horizontal
#' reference line at `error_threshold`.
#'
#' @details
#' If `n_resamples` is not `NULL`, the function draws `n_resamples`
#' random combinations of individuals at each population sample size
#' and computes a population-level estimate for each. This step
#' 
#' Each design is represented by a single stochastic run. Apparent
#' differences between designs may reflect random variation rather
#' than genuine performance differences. Use [md_replicate()] to
#' generate robust, replicated results for each design, and
#' [md_compare()] to compare multiple designs.
#' 
#' @examples
#' if (interactive()) {
#' 
#'   data(buffalo)
#'   inputA <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 1, unit = "month"),
#'     dti = list(value = 1, unit = "day"),
#'     add_individual_variation = FALSE,
#'     grouped = TRUE,
#'     set_target = "hr",
#'     which_meta = "mean")
#'   
#'   inputB <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 10, unit = "days"),
#'     dti = list(value = 1, unit = "day"),
#'     add_individual_variation = TRUE,
#'     grouped = TRUE,
#'     set_target = "hr",
#'     which_meta = "mean")
#'   
#'   outputA <- md_run(inputA)
#'   outputB <- md_run(inputB)
#'   md_compare_preview(list(outputA,
#'                           outputB), error_threshold = 0.05)
#' }
#' 
#' @seealso
#'   [md_run()] to generate each input object.
#'   [md_plot_preview()] for a single-design equivalent.
#'   [md_replicate()] for robust multi-replicate outputs per design.
#'   [md_check()] to assess convergence across replicates.
#'   
#' @export
md_compare_preview <- function(x,
                               n_resamples = NULL,
                               error_threshold = 0.05,
                               pal = c("#007d80", "#A12C3B"),
                               ...) {
  
  dots <- list(...)
  .seed <- dots[[".seed"]] %||% NULL
  
  n <- group <- error <- error_sd <- NULL
  single_obj <- FALSE
  
  if (inherits(x, "movedesign_processed") &&
      inherits(x, "movedesign")) {
    x <- list(x) 
    single_obj <- TRUE
  }
  
  if (!is.null(n_resamples)) {
    stopifnot(is.numeric(n_resamples) && n_resamples > 0)
    if (n_resamples < 5)
      stop("`n_resamples` must be set to at least 5.")
  }
  
  # Check that all objects have the same target:
  
  set_targets <- vapply(x, function(obj) {
    if (!inherits(obj, "movedesign_processed")) {
      stop(paste(
        "All elements must be 'movedesign_processed' objects.",
        "Use output from md_run()."))
    }
    paste(sort(unique(obj$set_target)), collapse = ",")
  }, character(1))

  if (length(unique(set_targets)) > 1) {
    stop("All input objects must have identical",
         " `set_target` values.\n ",
         " Found differing `set_target` values across objects: ",
         paste(unique(set_targets), collapse = " | "))
  }
  
  resampled <- !is.null(n_resamples)
  
  plots <- list()
  set_titles <- set_subtitles <- c()
  global_y_range <- c(Inf, -Inf)
  
  .worker <- function(obj, name) {
    
    if (!inherits(obj, "movedesign_processed")) {
      stop(paste(
        "Each object must be a 'movedesign_processed' object.\n",
        "Use the output of md_run()."))
    }
    
    iter_step <- ifelse(length(obj$simList) <= 10, 2, 4)
    
    # Run meta-analyses:
    if (resampled) {
      out <- run_meta_resamples(obj, 
                                set_target = obj$set_target,
                                subpop = obj$grouped, randomize = TRUE,
                                max_draws = n_resamples, trace = TRUE,
                                .automate_seq = TRUE, .seed = .seed)
    } else {
      out <- run_meta(obj, 
                      set_target = obj$set_target,
                      subpop = obj$grouped, iter_step = iter_step,
                      trace = TRUE, .seed = .seed)
    }
    
    out <- out %>%
      dplyr::mutate(
        type = factor(.data$type, levels = c("hr", "ctsd")),
        overlaps = factor(
          abs(.data$error) <= error_threshold,
          levels = c(TRUE, FALSE)))
    
    if (is.null(n_resamples)) {
      global_y_range <<- c(
        min(global_y_range[1], 
            ifelse(is.na(out$error_lci),
                   global_y_range[1],
                   min(out$error_lci, na.rm = TRUE))),
        max(global_y_range[2],
            ifelse(is.na(out$error_uci),
                   global_y_range[2],
                   min(out$error_uci, na.rm = TRUE))))
    } else {
      out_mean <- .summarize_error(out, error_threshold = error_threshold)
      global_y_range <<- c(
        min(global_y_range[1], 
            ifelse(is.na(out_mean$pred_lci),
                   global_y_range[1],
                   min(out_mean$pred_lci, na.rm = TRUE))),
        max(global_y_range[2],
            ifelse(is.na(out_mean$pred_uci),
                   global_y_range[2],
                   min(out_mean$pred_uci, na.rm = TRUE))))
    }
    
    return(out)
  }
  
  processed_outs <- lapply(seq_along(x), function(i) {
    .header(paste("Design", i), 3)
    .worker(x[[i]], paste0("Obj", i))
  })
  
  id <- 0
  outList <- list()
  for (i in seq_along(processed_outs)) {
    id <- id + 1
    
    out <- processed_outs[[i]]
    only_one_m <- length(unique(out$m)) == 1
    if (only_one_m) { 
      out$m <- factor(out$m)
      max_m <- unique(out$m)
    } else {
      max_m <- max(unique(out$m))
    }
    
    set_titles <- c(set_titles, paste("Design", id))
    
    dti <- fix_unit(x[[id]]$dti$value, x[[id]]$dti$unit)
    txt_tdi <- ifelse(dti$value == 1,
                      dti$unit, paste(dti$value, dti$unit))
    
    set_subtitles <- c(
      set_subtitles, paste0(
        max_m, " tags, tracked for ",
        paste(x[[id]]$dur$value, x[[id]]$dur$unit), " every ",
        txt_tdi))
    
    grouped <- unique(out$is_grouped)
    set_target <- unique(out$type)
    
    label_threshold <- sprintf("%s%%", error_threshold * 100)
    labels_target <- c(
      "hr" = "Home range area estimation",
      "ctsd" = "Speed & distance estimation")
    
    if (resampled) {
      
      max_draws <- max(unique(out$sample))
      if (grouped) out <- dplyr::filter(out, group != "All")
      
      out_mean <- .summarize_error(out, error_threshold = error_threshold)
      outList[[i]] <- out_mean
      label_ci <- sprintf("%g%%", unique(out_mean$ci) * 100)
      
      only_one_m <- ifelse(length(unique(out$m)) == 1, TRUE, FALSE)
      if (only_one_m) { 
        out$m <- factor(out$m)
        warning(paste0(
          "Only ", .msg("one", "danger"),
          " unique value of `m` detected per group.\n",
          "No resampling possible for design ", id, ".\n"),
          call. = FALSE)
      }
      
      m_resampled <- out_mean$m[out_mean$n > 1L]
      
      p <- out_mean %>%
        ggplot2::ggplot(
          ggplot2::aes(x = .data$m,
                       y = .data$error,
                       group = .data$group,
                       shape = .data$group,
                       color = .data$overlaps)) +
        
        ggplot2::geom_hline(
          yintercept = 0,
          linewidth = 0.3,
          linetype = "solid") +
        ggplot2::annotate(
          "rect",
          xmin = -Inf, xmax = Inf,
          ymin = -error_threshold,
          ymax = error_threshold,
          alpha = 0.05) +
        ggplot2::geom_hline(
          yintercept = c(-error_threshold, error_threshold),
          linetype = "dashed",
          linewidth = 0.35, alpha = 0.8) +
        ggplot2::annotate(
          "text",
          x = Inf, y = error_threshold,
          label = paste0("+", label_threshold),
          hjust = 1.08, vjust = -0.5,
          size = 3.2) +
        ggplot2::annotate(
          "text",
          x = Inf, y = -error_threshold,
          label = paste0("\u2212", label_threshold),
          hjust = 1.08, vjust = 1.5,
          size = 3.2) +
        
        { if (!only_one_m)
          ggplot2::geom_jitter(
            data = dplyr::filter(out, .data$m %in% m_resampled),
            mapping = ggplot2::aes(x = .data$m,
                                   y = .data$error,
                                   group = .data$group,
                                   shape = .data$group,
                                   color = .data$overlaps),
            position = ggplot2::position_jitterdodge(
              dodge.width = 0.4),
            color = "grey80", size = 3, alpha = 0.9) } +
        
        # Prediction intervals:
        { if (!all(is.na(out_mean$pred_lci)) &&
              !all(is.na(out_mean$pred_uci)))
        ggplot2::geom_linerange(
          ggplot2::aes(ymin = .data$pred_lci,
                       ymax = .data$pred_uci),
          position = ggplot2::position_dodge(width = 0.5),
          linewidth = 3,
          alpha = 0.18,
          show.legend = FALSE) } +
        
        # Confidence intervals:
        { if (!all(is.na(out_mean$error_lci)) &&
              !all(is.na(out_mean$error_uci)))
          ggplot2::geom_linerange(
          ggplot2::aes(ymin = .data$error_lci,
                       ymax = .data$error_uci),
          position = ggplot2::position_dodge(width = 0.5),
          linewidth = 1.1,
          alpha = 0.55,
          show.legend = FALSE) } +
        
        ggplot2::geom_point(
          position = ggplot2::position_dodge(width = 0.5),
          size = 4,
          show.legend = TRUE) +
        
        { if (length(set_target) > 1)
          ggplot2::facet_wrap(
            . ~ .data$type, scales = "free_y",
            labeller = ggplot2::labeller(type = labels_target))
        } +
        
        ggplot2::labs(
          x = "Population sample size",
          y = "Relative error (%)",
          color = sprintf(
            "Within error threshold (\u00B1%g%%)?",
            error_threshold * 100),
          caption = sprintf(
            paste0(
              "Wide bars: %s prediction interval  \u2022  ",
              "Narrow bars: %s confidence interval\n",
              "Based on %d resamples"),
            label_ci, label_ci, max_draws)) +
        
        { if (length(unique(out$m)) == 2) {
          ggplot2::scale_x_continuous(
            breaks = unique(out$m))
        } else if (!only_one_m) {
          ggplot2::scale_x_continuous(
            breaks = scales::breaks_pretty())
        }
        } +
        
        ggplot2::scale_y_continuous(
          labels = scales::percent,
          breaks = scales::breaks_pretty(),
          expand = ggplot2::expansion(mult = c(0.05, 0.10))) +
        ggplot2::scale_color_manual(
          values = c("TRUE" = pal[1], "FALSE" = pal[2]),
          drop = FALSE) +
        
        { if (grouped)
          ggplot2::scale_shape_manual(
            "Groups:", values = c(16, 17)) } +
        
        { if (grouped)
          ggplot2::guides(
            color = ggplot2::guide_legend(
              override.aes = list(
                shape = 22, size = 4,
                fill = c(pal[1], pal[2])))) } +
        
        { if (!single_obj && !only_one_m)
          ggplot2::coord_cartesian(ylim = global_y_range) } +
        .theme_movedesign_report()
      
      if (!grouped) p <- p + ggplot2::guides(shape = "none")
      
    } else {
      
      if (grouped) out <- dplyr::filter(out, group != "All")
      outList[[i]] <- out
      
      only_one_m <- ifelse(length(unique(out$m)) == 1, TRUE, FALSE)
      if (only_one_m) { 
        out$m <- factor(out$m)
        warning(paste0(
          "Only ", .msg("one", "danger"),
          " unique value of `m` detected per group.\n"),
          call. = FALSE)
      }
      
      label_ci <- "95%"
      
      p <- out %>%
        ggplot2::ggplot(
          ggplot2::aes(x = .data$m,
                       y = .data$error,
                       shape = .data$group,
                       group = .data$group,
                       color = .data$overlaps)) +
        
        ggplot2::geom_hline(
          yintercept = 0,
          linewidth = 0.3,
          linetype = "solid") +
        ggplot2::annotate(
          "rect",
          xmin = -Inf, xmax = Inf,
          ymin = -error_threshold,
          ymax = error_threshold,
          alpha = 0.05) +
        ggplot2::geom_hline(
          yintercept = c(-error_threshold, error_threshold),
          linetype = "dashed",
          linewidth = 0.35, alpha = 0.8) +
        ggplot2::annotate(
          "text",
          x = Inf, y = error_threshold,
          label = paste0("+", label_threshold),
          hjust = 1.08, vjust = -0.5,
          size = 3.2) +
        ggplot2::annotate(
          "text",
          x = Inf, y = -error_threshold,
          label = paste0("\u2212", label_threshold),
          hjust = 1.08, vjust = 1.5,
          size = 3.2) +
        
        # Confidence intervals:
        ggplot2::geom_linerange(
          ggplot2::aes(ymin = .data$error_lci,
                       ymax = .data$error_uci),
          position = if (grouped)
            ggplot2::position_dodge(width = 0.5) else 
              ggplot2::position_identity(),
          linewidth = 1.1,
          alpha = 0.55,
          show.legend = FALSE) +
        
        ggplot2::geom_point(
          position = if (grouped)
            ggplot2::position_dodge(width = 0.5) else 
              ggplot2::position_identity(),
          size = 4,
          show.legend = TRUE) +
        
        { if (length(set_target) > 1)
          ggplot2::facet_wrap(
            . ~ .data$type, scales = "free_y",
            labeller = ggplot2::labeller(type = labels_target))
        } +
        
        ggplot2::labs(
          x = "Population sample size",
          y = "Relative error (%)",
          color = paste0("Within error threshold (\u00B1",
                         error_threshold * 100, "%)?"),
          caption = sprintf(
            "Narrow bars: %s confidence interval", label_ci)) +
        
        { if (length(unique(out$m)) == 2) {
          ggplot2::scale_x_continuous(
            breaks = unique(out$m),
            expand = ggplot2::expansion(mult = c(0.10, 0.10)))
        } else if (!only_one_m) {
          ggplot2::scale_x_continuous(
            breaks = scales::breaks_pretty(),
            expand = ggplot2::expansion(mult = c(0.10, 0.10)))
        }
        } +
        
        ggplot2::scale_y_continuous(
          labels = scales::percent,
          breaks = scales::breaks_pretty(),
          expand = ggplot2::expansion(mult = c(0.10, 0.10))) +
        ggplot2::scale_color_manual(
          values = c("TRUE" = pal[1], "FALSE" = pal[2]),
          drop = FALSE) +
        
        { if (grouped)
          ggplot2::scale_shape_manual(
            "Groups:", values = c(16, 17),
          ) } +
        
        { if (grouped)
          ggplot2::guides(
            color = ggplot2::guide_legend(
              override.aes = list(
                shape = 22, size = 4,
                fill = c(pal[1], pal[2])))) } +
        { if (grouped)
          ggplot2::guides(
            shape = ggplot2::guide_legend(
              override.aes = list(size = 4))) } +
        
        { if (!single_obj)
          ggplot2::coord_cartesian(ylim = global_y_range) } +
        .theme_movedesign_report()
      
      if (!grouped) p <- p + ggplot2::guides(shape = "none")
      
    }
    
    plots[[i]] <- p
  }
  
  if (resampled) {
    warning(
      crayon::bold("! PRELIMINARY RESULTS ONLY\n"),
      " This plot displays outputs from ",
      crayon::yellow(paste0(
        "1 replicate + ",
        length(unique(out$sample)), " resample(s)")),
      ".\n Results may change substantially",
      " with additional replicates or resamples.\n\n",
      crayon::bold("  Run `md_replicate()`"),
      crayon::bold(" for more robust inferences.\n"),
      call. = FALSE)
    
  } else {
    warning(
      crayon::bold("! PRELIMINARY RESULTS ONLY\n"),
      " This plot displays outputs from ",
      crayon::yellow("a single replicate"),
      " and no resampling.\n",
      "  Results may change substantially",
      " with additional replicates or resamples.\n\n",
      crayon::bold("  Run `md_replicate()`"),
      crayon::bold(" for more robust inferences.\n"),
      call. = FALSE)
  }
  
  plots <- lapply(
    seq_along(plots),
    function(i) plots[[i]] + 
      ggplot2::labs(title = set_titles[[i]],
                    subtitle = set_subtitles[[i]])
  )
  
  caption <- paste(
    "This plot displays preliminary outputs from a",
    "single replicate.\n",
    "For more robust inferences, run additional replicates",
    "using `md_replicate()`.")
  
  suppressWarnings(print(
    patchwork::wrap_plots(
      plots,
      ncol = length(plots),
      guides = "collect") +
      patchwork::plot_annotation(
        caption = caption,
        theme = ggplot2::theme(legend.position = "bottom"))))
  
  if (is.null(n_resamples)) {
    return(invisible(NULL))
  } else {
    return(invisible(outList))
  }
  
}


#' @title Compare estimation error across study design workflows
#' 
#' @description
#' The final step in the `movedesign` workflow. Takes replicated
#' outputs from two or more study designs, ranks them by estimation
#' performance, identifies the best-performing design, and produces
#' a density plot of relative error across replicates.
#' 
#' Use this function after running [md_replicate()] for each design
#' and confirming convergence with [md_check()]. For a quick visual
#' comparison before full replication, use [md_compare_preview()]
#' instead.
#' 
#' @param x A list of at least two `movedesign_output` objects, each
#'   returned by [md_replicate()]. All designs must share the same
#'   `set_target`. Designs typically differ in sampling parameters
#'   such as `dur`, `dti`, or `n_individuals`.
#'   
#' @param stat Character. Summary statistic used to represent the
#'   centre of the error distribution in the plot and ranking.
#'   Must be `"mean"` (default) or `"median"`.
#'   
#' @param ci Numeric. Coverage probability of the credible interval
#'   computed over the distribution of relative error across
#'   replicates. Must be strictly between 0 and 1. Defaults to
#'   `0.80` (80% CI).
#'   
#' @param method Character. Method used to compute the credible
#'   interval, passed to [bayestestR::ci()]. Defaults to `"HDI"`
#'   (Highest Density Interval), which is generally preferred for
#'   asymmetric or skewed error distributions. See
#'   `?bayestestR::ci` for all available options.
#'   
#' @param pal A character vector of colours for the density curves,
#'   CI shading, and centre line. Defaults to `c("#007d80", "#A12C3B")`.
#'   
#' @param m (Optional) Numeric. If provided, restricts all results
#'   and ranking to a specific *population* sample size. Defaults to
#'   `NULL`, which uses the maximum sample size defined by
#'   `n_individuals`.
#'   
#' @param show_text Logical. Whether to display annotation text in
#'   the plot with the mean (or median) relative errors.
#'   Defaults to `TRUE`.
#'   
#' @return An object of class `movedesign_report`.
#' A density plot of relative error is printed as a side effect;
#' to reproduce it later, call [md_plot()] on any of the input
#' designs.
#' 
#' This object contains:
#' \describe{
#'   \item{ranking}{
#'   Data frame with one row per design per target
#'   (and per group if grouping is used). Columns include
#'   the centre error statistic (`error`), credible
#'   interval bounds (`error_lci`, `error_uci`), CI width,
#'   distance from zero error, and rank. Lower rank
#'   indicates better performance.
#'   }
#'
#'   \item{winners}{
#'   Data frame identifying designs that rank first across
#'   all groups for each target. A design is a winner when
#'   it has the lowest absolute error and the credible
#'   interval closest to zero across every group.
#'   Returns empty if no single design dominates.
#'   }
#' }
#' 
#' @details
#' 
#' Designs are ranked separately for each target metric (home
#' range, speed) and group (if present). The ranking criterion
#' combines absolute relative error and distance of the credible
#' interval from zero: designs with lower error and tighter
#' intervals closer to zero rank higher. A design is
#' identified as the overall winner only if it ranks first across
#' all groups for a given target.
#' 
#' The function prints a density plot showing the full
#' distribution of relative error across replicates for each
#' design. The centre statistic (`stat`) and credible interval
#' (`ci`) are overlaid. When groups are present, density curves
#' for each group are shown side by side using the colours in
#' `pal`.
#' 
#' ## Recommended workflow
#' 
#' This function is designed to be called at the end of the
#' `movedesign` workflow:
#' 
#' 1. Build each design with [md_prepare()].
#' 2. Run [md_replicate()] for each design.
#' 3. Confirm convergence with [md_check()].
#' 4. Compare and rank designs with [md_compare()].
#' 
#' @seealso
#' [`md_replicate()`],
#' [`md_check()`] for convergence diagnostics,
#' and refer to `bayestestR::ci()` for details on credible interval
#' computation and interpretation.
#'
#' @examples
#' if (interactive()) {
#' 
#'   data(buffalo)
#'   inputA <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 1, unit = "month"),
#'     dti = list(value = 1, unit = "day"),
#'     add_individual_variation = TRUE,
#'     grouped = TRUE,
#'     set_target = "hr",
#'     which_meta = "mean")
#'   
#'   inputB <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 10, unit = "days"),
#'     dti = list(value = 1, unit = "day"),
#'     add_individual_variation = TRUE,
#'     grouped = TRUE,
#'     set_target = "hr",
#'     which_meta = "mean")
#'   
#'   outputA <- md_replicate(inputA, n_replicates = 20)
#'   outputB <- md_replicate(inputB, n_replicates = 20)
#'   
#'   # Plot with 80% credible intervals:
#'   md_compare(list(outputA, outputB), ci = 0.80, method = "HDI")
#'   
#' }
#' 
#' @family workflow_steps
#' @export
md_compare <- function(x,
                       stat = c("mean", "median"),
                       ci = 0.8,
                       method = "HDI",
                       pal = c("#007d80", "#A12C3B"),
                       m = NULL,
                       show_text = TRUE) {
  
  if (inherits(x, "movedesign")) {
    stop("x` is a single `movedesign_output` object.\n",
         "  `md_compare()` requires a list of at least two objects.\n",
         "  To evaluate a single design, use `md_plot()` instead.")
  }
  
  if (!is.list(x) || length(x) < 2L) {
    stop("`x` must be a list of at least ",
         "two `movedesign_output` objects, ",
         "but received ", if (is.list(x))
           length(x) else class(x)[[1L]], ".")
  }
  
  invalid_elements <- which(!vapply(
    x, inherits, logical(1L), "movedesign_output"))

  if (length(invalid_elements) > 0) {
    stop(paste("All elements of `x` must be of",
               "class 'movedesign_output'.\n ",
               "Invalid element(s) at position(s): ",
               paste(invalid_elements, collapse = ", ")))
  }
  
  if (!all(stat %in% c("mean", "median")))
    stop("`stat` must be either 'mean' or 'median'.")
  stat <- match.arg(stat)
  
  if (!is.numeric(ci) || length(ci) != 1 || ci <= 0 || ci >= 1) 
    stop("`ci` must be a single numeric value strictly between ",
         " 0 and 1 (e.g. `ci = 0.80` for an 80% credible interval).")
  
  # Check that all objects have the same target:
  
  .check_field_consistency <- function(x,
                                       field,
                                       parent = "input",
                                       label  = field) {
    
    values <- vapply(x, function(y)
        paste(y[[parent]][[field]], collapse = ","),
      character(1L))
    
    if (length(unique(values)) > 1L) {
      stop(
        "All designs must share the same `", label, "`, ",
        "but found: ",
        paste(unique(values), collapse = ", "), ".\n",
        "  - Designs at fault: ",
        paste(which(values != values[[1L]]), collapse = ", "), "."
      )
    }
    
    invisible(x[[1L]][[parent]][[field]])
    
  }
  
  set_target <- .check_field_consistency(x, "set_target")
  target_map <- c(hr = "Home range estimation",
                  ctsd = "Movement speed estimation")
  has_groups <- x[[1]]$input$grouped
  # error thresholds much match?
  error_threshold <- x[[1]]$error_threshold
  
  # Generate plots and processed outputs:
  
  plot_output <- tryCatch({
    md_plot(x = x,
            stat = stat,
            ci = ci,
            method = method,
            pal = pal,
            show_text = show_text,
            .verbose = TRUE,
            .override = TRUE)
  }, error = function(e) stop("Error in md_plot(): ", e$message))
  
  if (!all(c("plots", "outputs") %in% names(plot_output))) {
    stop("`md_plot()` must return a list with plots and outputs.")
  }
  
  print(plot_output$plots)
  
  out <- plot_output$outputs
  if (!is.list(out) || length(out) == 0) {
    stop("`out` must be a non-empty list of data.frames.")
  }
  
  out_long <- do.call(rbind,
                      lapply(seq_along(out), function(i) {
    
    tmp <- out[[i]]
    input_i <- x[[i]]$input
    data_i <- x[[i]]$data
    
    required_input <- c("n_individuals", "grouped", "set_target")
    required_data <- c("dur", "dti")
    
    missing_input <- setdiff(required_input, names(input_i))
    missing_data <- setdiff(required_data, names(data_i))
    
    if (length(missing_input) > 0) 
      stop("Missing input fields in design ", i, ": ",
           paste(missing_input, collapse = ", "))
    if (length(missing_data) > 0) 
      stop("Missing data fields in design ", i, ": ",
           paste(missing_data, collapse = ", "))
    
    tmp$design_id <- i
    tmp$m <- input_i$n_individuals
    tmp$dur <- data_i$dur$value
    tmp$dur_unit  <- data_i$dur$unit
    tmp$dti <- data_i$dti$value
    tmp$dti_unit  <- data_i$dti$unit
    
    return(tmp)
    
  }))
  
  # Compute evaluation metrics:
  
  out_long$abs_estimate <- abs(out_long$est)
  out_long$overlaps_with_zero <- out_long$lci <= 0 & out_long$uci >= 0
  out_long$dist_to_zero <- ifelse(
    out_long$overlaps_with_zero,
    0, pmin(abs(out_long$lci), abs(out_long$uci)))
  out_long$ci_width <- out_long$uci - out_long$lci
  
  # Rank designs:
  
  ranking <- out_long[order(out_long$type,
                            out_long$group,
                            out_long$abs_estimate,
                            -out_long$overlaps_with_zero,
                            out_long$dist_to_zero,
                            out_long$ci_width), ]
  
  # ranking$rank <- stats::ave(
  #   seq_len(nrow(ranking)),
  #   interaction(ranking$type, ranking$group, drop = TRUE),
  #   FUN = function(z) {
  #     rank(ranking$abs_estimate[z] + 
  #            ranking$dist_to_zero[z] + 
  #            ranking$ci_width[z],
  #          ties.method = "min")
  #   }
  # )
  
  ranking$rank <- stats::ave(
    seq_len(nrow(ranking)),
    interaction(ranking$type, ranking$group, drop = TRUE),
    FUN = function(z) {
      rank(ranking$abs_estimate[z] + 
             ranking$dist_to_zero[z],
           ties.method = "min")
    }
  )
  
  rownames(ranking) <- NULL
  
  # Identify preliminary winners:
  
  preliminary_winners <- ranking[ranking$rank == 1L, , drop = FALSE]
  
  if (nrow(preliminary_winners) == 0L) {
    joint_winners <- data.frame()
    
  } else {
    joint_winners <- lapply(unique(ranking$type), function(tp) {
      
      pre_winners_t <- preliminary_winners[
        preliminary_winners$type == tp, , drop = FALSE]
      
      if (nrow(pre_winners_t) == 0L) return(NULL)
      
      unique_groups <- unique(pre_winners_t$group)
      win_counts <- tapply(pre_winners_t$group,
                           pre_winners_t$design_id,
                           function(g) length(unique(g)))
      
      joint_ids <- as.integer(names(win_counts)[
        win_counts == length(unique_groups)])
      
      if (length(joint_ids) == 0) return(NULL)
      
      data.frame(type = tp,
                 design_id = joint_ids,
                 stringsAsFactors = FALSE)
    })
    
    joint_winners <- do.call(rbind, joint_winners)
    
    # If no winners found, return empty:
    if (is.null(joint_winners) || nrow(joint_winners) == 0L) {
      joint_winners <- data.frame()
    }
  }
  
  ranking_out <- ranking[, c(
    "type", "group", 
    "est", "ci", "lci" ,"uci",
    "ci_width", "design_id", "m",
    "dur", "dur_unit", "dti", "dti_unit",
    "overlaps_with_zero", "dist_to_zero", "rank")]
  
  colnames(ranking_out)[
    colnames(ranking_out) %in% c("est", "lci", "uci")
  ] <- c("error", "error_lci", "error_uci")
  
  report <- structure(
    list(info = list(grouped = has_groups,
                     set_target = set_target,
                     error_threshold = error_threshold),
         ranking = ranking_out,
         winners = joint_winners),
    class = c("movedesign_report", "movedesign"))
  
  summary(report)
  
  return(invisible(report))
  
}


#' Read a `movedesign` session file
#'
#' @description
#' Reads a `.rds` file exported from the \pkg{movedesign}
#' Shiny application, validates its structure, and reconstitutes
#' each component to its corresponding `movedesign` class.
#' 
#' The returned list always contains:
#' \describe{
#'   \item{\code{design_input}}{
#'     A \code{movedesign_input} object: sampling design
#'     parameters as specified by the user.
#'   }
#'   \item{\code{design_processed}}{
#'     A \code{movedesign_processed} object: results from a
#'     single simulation replicate.
#'   }
#' }
#' 
#' (Optional) If multiple replicates have been run:
#' \describe{
#'   \item{\code{design_output}}{
#'     A \code{movedesign_output} object: aggregated results
#'     across all simulation replicates.
#'   }
#' }
#' 
#' @param filepath Path to an `.rds` session file.
#' 
#' @return A named list of three \pkg{movedesign} objects.
#' 
#' @examples
#' \dontrun{
#' md_objects <- read_md("path/to/my_file.rds")
#' 
#' md_objects$design_input      # movedesign_input object
#' md_objects$design_processed  # movedesign_processed object
#' md_objects$design_output     # movedesign_output (if available)
#' }
#' 
#' @export
read_md <- function(filepath) {
  
  if (!is.character(filepath) ||
      length(filepath) != 1L  ||
      !nzchar(filepath)) {
    stop(paste0(
      crayon::bold("`filepath`"),
      " must be a single character string.\n",
      "  Received class: ",
      crayon::red(paste(class(filepath), collapse = ", "))))
  }
  
  if (!file.exists(filepath)) {
    stop(paste0(
      crayon::red("\u2716 "), 
      "File ", crayon::red("not found.\n"),
      "     Path: ", crayon::red(filepath), "\n",
      "     Check for typos or a missing file extension."))
  }
  
  if (tolower(tools::file_ext(filepath)) != "rds") {
    writeLines(c(
      paste0(
        crayon::red("\u26a0 "), crayon::red(filepath),
        " does not have a ", crayon::red(".rds"), " extension."),
      crayon::silver("  Attempting to read anyway.")))
  }
  
  obj <- tryCatch(
    readRDS(filepath),
    error = function(e) {
      stop(paste0(
        "Failed to read: ",
        crayon::yellow(filepath), "\n",
        "  ", conditionMessage(e)))
    }
  )
  
  if (is.null(obj)) {
    stop(paste0(
      crayon::red("\u2716 File loaded but returned NULL.\n"),
      crayon::silver("  Verify that "),
      crayon::yellow(filepath),
      crayon::silver(" contains a valid object.")
    ))
  }
  
  out <- tryCatch(
    .as_md(obj),
    error = function(e) {
      stop(paste(
        "Could not extract",
        crayon::bold("movedesign"), "objects.\n",
        conditionMessage(e)))
    }
  )
  
  message <- c(
    crayon::cyan("\u2713 File", 
                 crayon::bold(basename(filepath)),
                 "read successfully."),
    "",
    paste0(
      "  Objects now available within ",
      crayon::cyan("returned object"), ":\n",
      "    ", crayon::cyan("design_input"),
      " -> an 'movedesign_input' object (initial parameters)\n",
      "    ", crayon::cyan("design_processed"), 
      " -> an 'movedesign_processed' object (single replicate)",
      if ("design_output" %in% names(out)) {
        paste0("\n    ", crayon::cyan("design_output"),
               " -> an 'movedesign_output' object ",
               "(multiple replicates)")
      } else ""))
  
  writeLines(message)
  
  if ("design_output" %in% names(out)) {
    return(list(design_input = out$design_input,
                design_processed = out$design_processed,
                design_output = out$design_output))
  } else {
    
    return(list(design_input = out$design_input,
                design_processed = out$design_processed))
  }
}

