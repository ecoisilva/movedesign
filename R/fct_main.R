
#' @title Prepare movement study design inputs
#'
#' @description
#' Prepares, validates, and organizes all required inputs and parameters
#' for evaluating the study design of animal movement projects. This
#' function checks data inputs, fits or verifies movement models,
#' extracts key parameters, and consolidates all settings in a
#' structured object for easy and reproducible downstream use.
#'
#' @param species Character. Scientific or common name of the focal
#'   species used as a workflow label.
#' @param data A named list of telemetry objects (from
#'   [ctmm::as.telemetry()]) to be used as the empirical basis for the
#'   simulations. Each telemetry object must contain valid metadata
#'   and timestamped locations.
#' @param models (Optional) Named list of fitted ctmm models (from
#'   [ctmm::ctmm.fit()] or [ctmm::ctmm.select()]). If not supplied,
#'   models are fitted automatically.
#' @param n_individuals Integer. Number of animals (tags) to include
#'    in the study design; defines the target *population* sample size.
#' @param dur A list with elements `value` and `unit` (e.g.,
#'   `list(value = 2, unit = "months")`), for the study's maximum
#'   duration. `unit` must be either `"second"`, `"minute"`, `"hour"`,
#'   `"day"`, `"month"`, or `"year"`.
#' @param dti A list with elements `value` and `unit` (e.g.,
#'   `list(value = 1, unit = "day")`), specifying the intended
#'   sampling interval between relocations. `unit` must be either
#'   `"second"`, `"minute"`, `"hour"`, `"day"`, `"month"`, or `"year"`.
#' @param set_target Character. Specifies the primary research target(s):
#'   must be either `hr` (home range estimation), `ctsd` 
#'   (movement speed), or a character vector including both. This
#'   argument controls which target metrics are processed, analyzed,
#'   and reported in the study design workflow.
#' @param which_meta Character. Specifies the analytical target for
#'   population-level inference: `NULL`, `"mean"` (default), or
#'   `"ratio"`. Use `NULL` for a single individual, `"mean"` for
#'   population means, or `"ratio"` to compare group means
#'   (requires `groups`).
#'   
#' @param add_individual_variation Logical. If \code{TRUE}, simulates
#'   variation by drawing movement parameters from the population 
#'   distribution.
#' @param groups (Optional) A named list for group assignments.
#'   Each element is a character vector of individual names
#'   (matching `data`). For example,
#'   `list(A = c("id1", "id2"), B = c("id3", "id4"))` for groups
#'   "A" and "B".Required when `which_meta = \"ratio\"`.
#' @param parallel Logical. If `TRUE`, enables parallel processing
#'   for model fitting, which speeds up analyses.
#'
#' @details
#' This function is designed to streamline and standardize the preparation
#' of input data and study design parameters for simulation-based movement
#' ecology analyses. It performs the following key steps:
#' \itemize{
#'   \item Validates that `data` is a non-empty list of telemetry
#'     objects with metadata and location records.
#'   \item Fits movement models to each individual if not supplied.
#'   \item Checks supplied movement models for validity.
#'   \item Extracts parameters (e.g., `sigma`, `tau_p`, `tau_v`)
#'     for simulation.
#'   \item Gathers settings (sample size, duration, sampling, grouping)
#'     into a single object.
#' }
#'
#' @return
#' An object of class `movedesign_input` (and `movedesign`). This is
#' a structured S3 list containing all validated inputs, model fits,
#' and derived parameters for the study design workflow.
#'
#' The returned object includes:
#' \itemize{
#'   \item `design`: 
#'         A `movedesign` object with all study settings and metadata.
#'   \item `data`: 
#'         The original or validated list of telemetry objects.
#'   \item `fitList`: 
#'         List of fitted movement models for each individual.
#'   \item `meanfitList`:
#'         List of population or group-level mean models.
#'   \item `sigma`, `tau_p`, `tau_v`: 
#'         Movement parameters extracted from data provided for
#'         downstream simulations.
#'   \item `mu`: List of mean locations.
#'   \item `groups`: Group structure if specified, otherwise `NULL`.
#'   \item Other slots describing *population* sample size, sampling
#'         duration, sampling interval, targets, and workflow options.
#' }
#'
#' This object is ready for use in downstream `movedesign` output
#' and diagnostic functions.
#'   
#' @examples
#' if(interactive()) {
#'   data(buffalo)
#'   input <- md_prepare(
#'     data = buffalo,
#'     models = models,
#'     species = "buffalo",
#'     n_individuals = 5,
#'     dur = list(value = 1, unit = "month"),
#'     dti = list(value = 1, unit = "day"),
#'     add_individual_variation = TRUE,
#'     set_target = "hr",
#'     which_meta = "mean")
#'  summary(input)
#' }
#'
#' @seealso
#'   [ctmm::as.telemetry()],
#'   [ctmm::ctmm.select()]
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
                       parallel = FALSE) {
  
  if (is.null(species))
    stop("Add species label.")
  
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
        stop("If `which_meta` is NULL, 'data$identity' must not be NULL.")
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
  set_target <- .validate_target(set_target)
  .validate_meta(which_meta, data)
  
  stopifnot(is.list(data))
  if (length(data) == 0) stop("Input 'data' cannot be empty.")
  
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
  if (missing(dur) || 
      !is.list(dur) ||
      !all(c("value", "unit") %in% names(dur)))
    stop("'dur' must be a list with elements 'value' and 'unit'.")
  if (missing(dti) || 
      !is.list(dti) || 
      !all(c("value", "unit") %in% names(dti)))
    stop("'dti' must be a list with elements 'value' and 'unit'.")
  
  if (!is.null(groups) && n_individuals %% 2 != 0)
    stop("'n_individuals' must be even when 'groups' is not NULL.")
  
  fitList <- if (is.null(models)) {
    fitting_model(data, .parallel = parallel)
  } else {
    if (!all(sapply(models, function(m) 
      inherits(m, c("ctmm", "ctmm.select")))))
      stop("Models must be from ctmm.fit() or ctmm.select().")
    models
  }
  names(fitList) <- names(data)
  
  meanfitList <- list(mean(fitList))
  names(meanfitList) <- "All"
  
  if (add_individual_variation) {
    sigma <- extract_pars(meanfitList, "sigma")
    tau_p <- extract_pars(meanfitList, "position")
    tau_v <- extract_pars(meanfitList, "velocity")
  } else {
    sigma <- extract_pars(fitList, "sigma", meta = TRUE)
    tau_p <- extract_pars(fitList, "position", meta = TRUE)
    tau_v <- extract_pars(fitList, "velocity", meta = TRUE)
  }
  
  mu <- list(array(0, dim = 2, dimnames = list(c("x", "y"))))
  names(sigma) <- names(tau_p) <- names(tau_v) <- "All"
  names(mu) <- "All"
  
  if (!is.null(groups)) {
    
    groups[[1]] <- groups
    groups[[2]] <- list(A = c(), B = c())
    names(groups) <- NULL
    
    fitA <- fitList[groups[[1]][["A"]]]
    fitB <- fitList[groups[[1]][["B"]]]
    
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
      extract_pars(
        obj = fit[[x]],
        name = "sigma", meta = TRUE)[[1]]
    }))
    names(sigma) <- c("All", "A", "B") 
    
    tau_p <- c(tau_p, lapply(1:2, function(x) {
      extract_pars(
        obj = fit[[x]], 
        name = "position", meta = TRUE)[[1]]
    }))
    names(tau_p) <- c("All", "A", "B") 
    
    tau_v <- c(tau_v, lapply(1:2, function(x) {
      extract_pars(
        obj = fit[[x]], 
        name = "velocity", meta = TRUE)[[1]]
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
    which_m = "set_m",
    parallel = parallel,
    fitList = fitList,
    meanfitList = meanfitList,
    sigma = sigma,
    tau_p = tau_p,
    tau_v = tau_v,
    mu = mu))
  
  return(design)
}

#' @title Run study design workflow
#'
#' @description
#' Executes a complete simulation and analysis workflow for an animal
#' movement study design prepared using [md_prepare()]. This function
#' simulates telemetry data, fits movement models, estimates home ranges
#' and/or movement speeds, and stores all results in the returned object.
#' Progress and timing messages are printed by default.
#' 
#' @param design An object of class `movedesign` (and
#'   `movedesign_input`), as returned by [md_prepare()], containing
#'   all study design parameters and data.
#' @param trace Logical. If TRUE (default), print progress and timing
#'   messages to the console.
#'
#' @return An updated `movedesign` object (subclass
#'   `movedesign_preprocess`) containing all simulation and outputs
#'   components:
#'   \itemize{
#'     \item `simList`: List of simulated telemetry datasets,
#'       one per individual.
#'     \item `seedList`: List of random seeds used for
#'       reproducibility.
#'     \item `simfitList`: List of fitted movement models for
#'       each simulation.
#'     \item `akdeList`: List of home range (AKDE) estimates,
#'       present if the `hr` target was listed in `set_target`.
#'     \item `ctsdList`: List of continuous-time speed and
#'       distance (CTSD) estimates, present if the `ctsd` target
#'       was listed in `set_target`.
#'   }
#'
#' @details
#' This function ensures reproducibility by saving all random seeds and
#' intermediate results. Progress and timing messages help track the
#' simulation workflow.
#'
#' Typical workflow:
#' \itemize{
#'   \item Prepare a study design with [md_prepare()].
#'   \item Run all simulations and analyses with [md_run()].
#'   \item Summarize or plot outputs from the returned object.
#' }
#'
#' @seealso
#'   [md_prepare()],
#'   [md_replicate()],
#'   [md_check()],
#'   [md_plot()]
#'
#' @examples
#' if(interactive()) {
#' input <- md_prepare(
#'   data = buffalo,
#'   models = models,
#'   species = "buffalo",
#'   n_individuals = 5,
#'   dur = list(value = 1, unit = "month"),
#'   dti = list(value = 1, unit = "day"),
#'   add_individual_variation = TRUE,
#'   set_target = "hr",
#'   which_meta = "mean"
#' )
#' output <- md_run(input)
#' }
#'
#' @export
md_run <- function(design, trace = TRUE) {
  
  if (!inherits(design, "movedesign")) {
    stop(paste("The object must be of class 'movedesign'.",
               "Run md_prepare() first."))
  }
  
  start_total <- Sys.time()
  
  if (trace) message("Simulating data...")
  if (trace) start <- Sys.time()
  
  m <- as.integer(design$n_individuals)
  design$simList <- vector("list", m)
  design$seedList <- vector("list", m)
  design$simfitList <- vector("list", m)
  design$akdeList <- vector("list", m)
  design$ctsdList <- vector("list", m)
  
  nms <- list()
  for (i in seq_len(m)) {
    seed <- generate_seed(nms)
    
    if (design$grouped) {
      if (i %% 2 == 0) next
      
      tmp <- simulating_data(design, seed)
      design$simList[[i]] <- tmp[[1]]
      design$simList[[i + 1]] <- tmp[[2]]
      
      design$groups[[2]][["A"]] <- c(
        as.character(design$groups[[2]]$A),
        as.character(seed))
      design$groups[[2]][["B"]] <- c(
        as.character(design$groups[[2]]$B),
        as.character(seed + 1))
      nms[[i]] <- seed
      nms[[i + 1]] <- seed + 1
    } else {
      design$simList[[i]] <- simulating_data(design, seed)[[1]]
      nms[[i]] <- seed
    }
  }
  design$seedList <- nms
  names(design$simList) <- nms
  if (trace) print(Sys.time() - start)
  
  if (trace) message("Fitting movement models...")
  if (trace) start <- Sys.time()
  design$simfitList <- fitting_model(
    design$simList, .parallel = design$parallel)
  names(design$simfitList) <- names(design$simList)
  if (trace) print(Sys.time() - start)
  gc()
  
  if ("hr" %in% design$set_target) {
    if (trace) message("Estimating home range...")
    start <- Sys.time()
    design$akdeList <- estimate_hr(design)
    if (trace) print(Sys.time() - start)
  }
  
  if ("ctsd" %in% design$set_target) {
    if (trace) message("Estimating speed...")
    start <- Sys.time()
    design$ctsdList <- estimate_speed(design)
    if (trace) print(Sys.time() - start)
  }
  
  message("------------------- Elapsed time:")
  print(difftime(Sys.time(), start_total))
  
  return(movedesign_preprocess(list(
    data = design$data,
    get_species = design$get_species,
    data_type = design$data_type,
    n_individuals = design$n_individuals,
    dur = design$dur,
    dti = design$dti,
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
    seedList = design$seedList,
    simfitList = design$simfitList,
    akdeList = design$akdeList,
    ctsdList = design$ctsdList)))
  
}


#' @title Merge multiple simulation outputs
#'
#' @description
#' Merges the results of multiple simulation runs, each produced by
#' [`md_run()`], into a single unified `movedesign_output` object. This
#' is especially useful when running replicate simulations for power
#' analyses, sensitivity testing, or batch processing. Merging allows
#' you to aggregate all simulated individuals, outputs, and related
#' metadata, enabling streamlined downstream analyses.
#'
#' @param ... One or more objects of class `movedesign_preprocess`,
#'   typically generated by [`md_run()`]. Each object must contain, at
#'   minimum, the elements `simList`, `simfitList`, and `seedList`.
#'   Optional elements such as `akdeList` and `ctsdList` 
#'   are merged if present.
#'
#' @return
#' A single `movedesign_output` object that contains all merged
#' simulation outputs and inherits metadata from the first input
#' object. The output includes:
#' - Merged list of simulated individuals (`simList`),
#' - Merged list of fitted models (`simfitList`),
#' - Merged list of seeds used for each simulation replicate
#'   (`seedList`),
#' - Optionally, merged home range (`akdeList`) and
#'   speed (`ctsdList`) outputs,
#' - Relevant metadata describing the study design parameters.
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
#'     grouped = FALSE,
#'     set_target = "hr",
#'     which_meta = "mean"
#'   )
#'
#'   output1 <- md_run(input)
#'   output2 <- md_run(input)
#'
#'   merged <- md_merge(output1, output2)
#' }
#'
#' @seealso
#'   \code{\link{md_prepare}},
#'   \code{\link{md_run}}
#'
#' @export
md_merge <- function(...) {
  outs <- list(...)
  
  if (length(outs) == 1 &&
      is.list(outs[[1]]) &&
      all(sapply(outs[[1]], is.list))) {
    outs <- outs[[1]]
  }
  
  class_list <- vapply(outs, function(x)
    if (is.list(x) && length(class(x)) > 0)
      class(x)[1] else NA_character_, character(1))
  
  allowed_classes <- c("movedesign_preprocess",
                       "movedesign_output")
  if (!all(class_list %in% allowed_classes)) {
    stop(
      "All inputs to md_merge() must be of class ",
      "'movedesign_preprocess' or 'movedesign_output'.\n",
      "Offending element(s): ",
      paste(which(!class_list %in% allowed_classes),
            collapse = ", "))
  }
  
  # if (length(unique(class_list)) > 1) {
  #   stop(
  #     "All inputs to md_merge() must be of the same movedesign ",
  #     "class ('movedesign_preprocess' or 'movedesign_output').")
  # }
  
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
    "get_species", "individual_no",
    "dur", "dti", "add_ind_var",
    "grouped", "groups",
    "set_target", "which_meta", "parallel",
    "sigma", "tau_p", "tau_v", "mu",
    "meanfitList"
  )
  
  # Helper function to compare metadata element-wise:
  find_mismatches <- function(metadatas) {
    ref <- metadatas[[1]]
    n <- length(metadatas)
    mismatches <- vector("list", length(metadata_fields))
    names(mismatches) <- metadata_fields
    
    for (field in metadata_fields) {
      vals <- lapply(metadatas, `[[`, field)
      identicals <- vapply(vals[-1], function(v)
        identical(v, vals[[1]]), logical(1))
      if (!all(identicals)) {
        mismatches[[field]] <- vals
      } else {
        mismatches[[field]] <- NULL
      }
    }
    
    Filter(Negate(is.null), mismatches)
  }
  
  # Extract metadata:
  metadatas <- lapply(outs, function(x) x[metadata_fields])
  
  # Check and stop if mismatches exist:
  mismatched_elements <- find_mismatches(metadatas)
  
  if (length(mismatched_elements) > 0) {
    mismatched_names <- names(mismatched_elements)
    stop(
      "Metadata mismatch across simulation outputs. Merge aborted.\n",
      "Mismatched fields: ",
      paste(mismatched_names, collapse = ", "), "\n",
      "Inspect elements in `mismatched_elements` for more detail."
    )
  }
  
  # # Extract metadata:
  # metadatas <- lapply(outs, function(x) x[metadata_fields])
  # # Check that all metadata are identical:
  # if (!all(vapply(metadatas, function(m) 
  #   identical(m, metadatas[[1]]), logical(1)))) {
  #   stop(
  #     "Metadata mismatch across simulation outputs. Merge aborted.",
  #     "\nEnsure all inputs share the same design and settings."
  #   )
  # }
  
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
  meta$n_individuals <- length(combined_simList)
  
  out <- c(meta, list(
    simList = combined_simList,
    simfitList = combined_simfitList,
    seedList = combined_seedList,
    akdeList = combined_akdeList,
    ctsdList = combined_ctsdList
  ))
  
  out[sapply(out, is.null)] <- NULL
  class(out) <- "movedesign_output"
  
  return(out)
}


#' @title Replicate study design and aggregate simulation outputs
#'
#' @description
#' Runs the specified movement study design multiple times and aggregates
#' outputs and summary statistics across independent replicates. This
#' enables sensitivity analyses and quantifies variability arising from
#' random sampling, especially when individual-level variation is enabled
#' (i.e., `add_individual_variation = TRUE` in [`md_prepare()`]). 
#' Replication helps assess how stochasticity and design choices impact
#' simulation inference.
#'
#' @param obj An object of class `movedesign` created by
#'   [`md_prepare()`]. It contains all parameters and input data
#'   defining the movement study.
#' @param n_replicates Integer specifying how many independent
#'   simulation replicates to run.
#' @param verbose Logical. If `TRUE`, the function performs
#'   population-level inference iteratively, increasing the population
#'   sample size and saving the results at each increment. This allows
#'   users to observe how parameter estimates and uncertainty change as
#'   more individuals are included. If `FALSE` (default), the inference
#'   runs only once using the maximum sample size specified by
#'   `n_individuals` in [`md_prepare()`].
#' @param verbose Logical; if `TRUE`, runs population-level inferences
#'   iteratively for increasing population sample sizes, saving results
#'   at each step. Defaults to `FALSE`, which runs only once for the
#'   maximum sample size defined by `n_individuals` in [`md_prepare()`].
#' @param trace Logical; if `TRUE` (default), prints progress and
#'   timing messages to the console.
#' @param parallel Logical; if `TRUE`, enables parallel processing via
#'   [`parallel::mclapply()`]. Default is `FALSE` (serial execution).
#' @param ncores Integer; number of CPU cores to use for parallel
#'   processing. Defaults to all available cores detected by
#'   [`parallel::detectCores()`].
#'
#' @return
#' A list of class `movedesign_output` with two elements:
#' \itemize{
#'   \item `data`: A list containing
#'     merged simulation outputs from all replicates.
#'   \item `summary`: A `data.table` summarizing key
#'     statistics for each replicate.
#' }
#'
#' @details
#' Each replicate runs independently using the same study design object
#' but with a unique random seed to ensure independence. Results from all
#' replicates are merged using [`md_merge()`], and summary statistics
#' combine into a single `data.table` for convenient downstream analyses
#' and evaluation. Parallel processing can significantly reduce runtime
#' when running many replicates; use `ncores` to specify the number of CPU
#' cores used. If function is interrupted (e.g., Ctrl+C), it returns
#' results from all completed replicates up to that point.
#'
#' @seealso
#' [`md_prepare()`], [`md_run()`], [`md_merge()`]
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
#'     grouped = FALSE,
#'     set_target = "hr",
#'     which_meta = "mean"
#'   )
#'
#'   output <- md_replicate(input, n_replicates = 5)
#' }
#'
#' @importFrom data.table rbindlist
#' @export
md_replicate <- function(obj,
                         n_replicates,
                         verbose = FALSE,
                         trace = TRUE,
                         parallel = FALSE,
                         ncores = parallel::detectCores()) {
  
  stopifnot(inherits(obj, "movedesign"))
  stopifnot(is.numeric(n_replicates) && n_replicates > 0)
  
  .worker <- function(i) {
    
    if (trace) message(.msg(
      sprintf("\u2014 Replicate %s of %s", i, n_replicates), "main"))
    out <- md_run(obj, trace = trace)
    meta <- run_meta_resamples(
      out,
      set_target = obj$set_target,
      subpop = obj$grouped,
      .m = ifelse(verbose, NULL, obj$n_individuals)
    )
    list(out = out, meta = meta)
  }
  
  outList <- vector("list", n_replicates)
  metaList <- vector("list", n_replicates)
  completed <- 0
  
  tryCatch({
    if (parallel) {
      tmp <- parallel::mclapply(
        seq_len(n_replicates), 
        function(i) .worker(i), mc.cores = ncores)
      for (i in seq_along(n_replicates)) {
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
    message(.msg(sprintf(
      "\nUser interrupt detected. Returning %s out of %s results.",
      completed, n_replicates), "error"))
    FALSE
  })
  
  # Remove NULL elements (if interrupted)
  outList <- outList[seq_len(completed)]
  metaList <- metaList[seq_len(completed)]
  
  if (length(metaList) > 0) {
    summary <- data.table::rbindlist(
      metaList, fill = TRUE, idcol = "replicate")
  } else {
    summary <- data.table::data.table()
  }
  
  if (length(outList) > 0) {
    
    if (obj$grouped) {
      group_keys <- c("A", "B")
      common_names <- outList[[1]]$groups[[1]]
      merged_ids <- lapply(outList, function(x) x$groups[[2]])
      merged_ids <- Reduce(
        function(x, y) Map(c, x, y), merged_ids)
      
      for (x in seq_along(outList)) {
        outList[[x]]$groups <- list(common_names, merged_ids)
      }
    }
    
    merged <- md_merge(outList)
    class(merged) <- unique(c("moveoutput", class(merged)))
  } else {
    merged <- NULL
  }
  
  if (trace) {
    
    message(.msg(sprintf(
      "Resampling completed! %s replicates merged.", 
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
  
  merged$n_replicates <- completed
  
  out <- structure(
    list(data = merged, summary = summary), class = "movedesign")
  class(out) <- unique(c("movedesign_output", class(out)))
  return(out)
}


#' @title Visualize study design outputs
#'
#' @description
#' Produces a publication-ready density plot showing the distribution of
#' relative error estimates from study design simulations. The plot
#' highlights the mean and a shaded credible interval (CI) region,
#' following the computation of credible intervals as implemented in
#' [`bayestestR::ci()`]. If groups are present, density curves for each
#' group are overlaid for comparison, using customizable colors.
#'
#' This function is typically used after running [`md_replicate()`],
#' providing a visual diagnostic of simulation results.
#'
#' @param obj A `movedesign_output` object, as returned by
#'   [`md_replicate()`]. The object must contain a
#'   `summary` data frame with, at a minimum, the following columns:
#'   \describe{
#'     \item{error}{Relative error values for each replicate.}
#'     \item{error_lci}{Lower credible interval bound for error.}
#'     \item{error_uci}{Upper credible interval bound for error.}
#'     \item{group}{(Optional) Group label for comparing densities.}
#'   }
#'
#' @param ci Numeric scalar between 0 and 1. The probability of the
#'   credible interval (CI) to be estimated. Default to `0.95` (⁠95%⁠).
#' @param method Character. Credible interval estimation method (passed
#'   to [`bayestestR::ci()`]; default: `"HDI"`). See [`bayestestR::ci()`]
#'   for more details.
#' @param pal Character vector of color(s) for the density, CI shading,
#'   and mean line. If a single group, supply one color (default:
#'   `"#007d80"`). If groups are present, supply two colors (default:
#'   `c("#007d80", "#A12C3B")`).
#' @param m Numeric (optional). If provided, restricts the
#'   results for a specific population sample size (`m`).
#'   Defaults to `NULL`, which checks up to the maximum population
#'   sample size.
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
#' and refer to [`bayestestR::ci()`] for details on credible interval
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
#' @import ggplot2
#' @importFrom stats density
#' @importFrom scales percent percent_format breaks_pretty
#' @importFrom dplyr filter mutate across
#' @importFrom bayestestR ci
#' @export
md_plot <- function(obj,
                    ci = 0.95,
                    method = "HDI",
                    pal = c("#007d80", "#A12C3B"),
                    m = NULL) {
  
  x <- y <- type <- caption <- group <- NULL
  
  if (!inherits(obj, "movedesign") ||
      !("summary" %in% names(obj))) {
    stop(paste(
      "Input does not appear to be a 'movedesign_output' object.\n",
      "Please provide the output of md_replicate()."))
  }
  
  data <- obj$summary
  
  stopifnot(is.data.frame(data))
  if (!all(c("error", "error_lci", "error_uci") %in% names(data))) {
    stop("Input data must have columns: error, error_lci, error_uci")
  }
  stopifnot(is.numeric(ci), length(ci) == 1, ci > 0, ci < 1)
  
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
  
  data <- dplyr::mutate(
    data, dplyr::across(
      type, ~factor(., levels = c("hr", "ctsd"))))
  set_target <- obj$data$set_target
  
  facet_by_type <- length(set_target) == 2
  
  has_groups <- "group" %in% names(data) &&
    all(c("A", "B") %in% unique(as.character(data$group)))
  
  if (has_groups || facet_by_type) stopifnot(length(pal) == 2)
  
  # Single group plotting:
  if (!has_groups) {
    
    data <- data[data$group == "All", ]
    dens <- do.call(
      rbind,
      lapply(unique(data$type), function(tp) {
        d <- data[data$type == tp, ]
        
        if (nrow(d) == 0) return(NULL)
        dens <- stats::density(d$error, na.rm = TRUE)
        df <- data.frame(x = dens$x, y = dens$y, type = tp)
        cri <- suppressMessages(quiet(
          .extract_cri(d$error, method = method, ci = ci)))
        if (!is.na(cri$lci) && !is.na(cri$uci)) {
          df_cri <- subset(df, x >= cri$lci & x <= cri$uci)
          if (nrow(df_cri) > 0) {
            df_cri$shaded <- TRUE
            return(df_cri)
          }
        }
        
        NULL }))
    
    if (is.null(dens)) {
      warning(paste(
        "The credible intervals (`ci`) may be too large",
        "or the input data contains too few replicates,",
        "returning NAs."))
      caption <- paste0("No credible intervals (CI) available; ",
                        "dotted line: mean of all replicates")
    } else {
      caption <- paste0("Shaded region: ", as.integer(ci * 100),
                        "% credible interval (CI); ",
                        "dotted line: mean of all replicates")
    }
    
    text <- do.call(
      rbind, lapply(unique(data$type), function(tp) {
        d <- data[data$type == tp, ]
        cri <- suppressMessages(quiet(
          .extract_cri(d$error, method = method, ci = ci)))
        dens <- stats::density(d$error, na.rm = TRUE)
        max_y <- max(dens$y)
        x_range <- max(d$error_uci, na.rm = TRUE) -
          min(d$error_lci, na.rm = TRUE)
        x_adjust <- x_range * 0.005
        
        return(data.frame(type = tp,
                          mean = mean(d$error, na.rm = TRUE),
                          lci = cri$lci,
                          uci = cri$uci,
                          max_y = max_y,
                          x_adjust = x_adjust))
      }))
    
    vline_summary <- data %>%
      dplyr::group_by(.data$group, .data$type) %>%
      dplyr::summarise(
        mean_error = mean(.data$error, na.rm = TRUE),
        .groups = "drop") %>%
      dplyr::mutate(add_vline = abs(.data$mean_error) < 0.8) %>%
      dplyr::filter(.data$add_vline) %>%
      dplyr::mutate(xintercept = 0)
    
    p <- ggplot2::ggplot(data = data) +
      
      ggplot2::geom_vline(
        data = vline_summary,
        mapping = ggplot2::aes(
          xintercept = .data$xintercept,
          group = interaction(.data$group, .data$type)),
        color = "grey50",
        linetype = "solid") +
      
      # ggplot2::geom_vline(
      #   xintercept = 0,
      #   color = "grey50",
      #   linetype = "solid") +
      
      ggplot2::geom_line(
        data = do.call(
          rbind,
          lapply(unique(data$type), function(tp) {
            d <- data[data$type == tp, ]
            if (nrow(d) == 0) return(NULL)
            dens <- stats::density(d$error, na.rm = TRUE)
            data.frame(x = dens$x, y = dens$y, type = tp)
          })),
        
        mapping = ggplot2::aes(x = .data$x,
                               y = .data$y,
                               color = .data$type),
        alpha = 0.3, linewidth = 1) +
      
      ggplot2::geom_jitter(
        mapping = ggplot2::aes(x = .data$error, y = 0),
        height = 0, width = 0.001,
        shape = "|", size = 8, alpha = 0.8,
        inherit.aes = FALSE) +
      
      { if (!is.null(dens) && nrow(dens) > 0)
        ggplot2::geom_area(
          data = dens,
          mapping = ggplot2::aes(
            x = .data$x,
            y = .data$y, 
            fill = .data$type),
          alpha = 0.2, position = "identity") } +
      
      ggplot2::geom_vline(
        data = data.frame(
          type = unique(data$type),
          mean = tapply(data$error, data$type, mean, na.rm = TRUE)),
        mapping = ggplot2::aes(xintercept = .data$mean,
                               color = .data$type),
        linetype = "dotted", linewidth = 1) +
      
      ggplot2::geom_text(
        data = text,
        ggplot2::aes(x = .data$mean + .data$x_adjust,
                     y = .data$max_y * 1.05,
                     label = sprintf(
                       "Mean = %s", 
                       scales::percent(.data$mean, 0.1)),
                     color = .data$type),
        size = 6, 
        hjust = -0.05, 
        show.legend = FALSE) +
      
      { if (!is.null(dens) && nrow(dens) > 0)
        ggplot2::geom_text(
          data = text,
          ggplot2::aes(
            x = .data$lci - .data$x_adjust, y = 0,
            label = sprintf(
              "LCI = %s", scales::percent(.data$lci, 0.1)),
            color = .data$type),
          size = 6, 
          vjust = -3, hjust = 1, 
          show.legend = FALSE) } +
      { if (!is.null(dens) && nrow(dens) > 0)
        ggplot2::geom_text(
          data = text,
          ggplot2::aes(
            x = .data$uci + .data$x_adjust, y = 0,
            label = sprintf(
              "UCI = %s", scales::percent(.data$uci, 0.1)),
            color = .data$type),
          size = 6,
          vjust = -3, hjust = 0, 
          show.legend = FALSE) } +
      
      { if (length(set_target) > 1)
        ggplot2::facet_wrap(
          . ~ .data$type, scales = "free",
          labeller = ggplot2::labeller(
            type = c(
              "hr" = "Home range estimation", 
              "ctsd" = "Speed \u0026 distance estimation"))) } +
      
      ggplot2::scale_color_manual(values = pal, drop = FALSE) +
      ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
      ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = scales::breaks_pretty()) +
      
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.05)),
        limits = c(0, NA)) +
      
      ggplot2::labs(
        title = paste("Number of replicates:", obj$data$n_replicates),
        caption = caption,
        x = "Relative error (%)", y = NULL) +
      
      ggplot2::theme_classic() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 18),
        axis.text.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 18),
        strip.background.x = ggplot2::element_rect(
          color = NA, fill = NA),
        strip.background.y = ggplot2::element_rect(
          color = NA, fill = NA),
        plot.margin = ggplot2::unit(c(0.2, 0.2, 0.3, 0.2), "cm")) +
      ggplot2::guides(color = "none", fill = "none", shape = "none")
    
    return(p)
  }
  
  # Two groups plotting:
  if (has_groups) {
    
    data <- data[data$group != "All", ]
    
    groups <- c("A", "B")
    types <- unique(data$type)
    
    dens <- do.call(
      rbind,
      lapply(groups, function(gr) {
        lapply(types, function(tp) {
          d <- data[data$group == gr & data$type == tp, ]
          if (nrow(d) == 0) return(NULL)
          dens <- stats::density(d$error, na.rm = TRUE)
          df <- data.frame(x = dens$x, 
                           y = dens$y,
                           group = gr,
                           type = tp)
          cri <- suppressMessages(quiet(
            .extract_cri(d$error, method = method, ci = ci)))
          if (!is.na(cri$lci) && !is.na(cri$uci)) {
            df_cri <- subset(df, x >= cri$lci & x <= cri$uci)
            if (nrow(df_cri) > 0) {
              df_cri$shaded <- TRUE
              return(df_cri)
            }
          }
        })
      })
    )
    if (length(dens) > 0) dens <- do.call(rbind, dens)
    
    if (is.null(dens)) {
      warning(paste(
        "The credible intervals (`ci`) may be too large",
        "or the input data contains too few replicates,",
        "returning NAs."))
      caption <- paste0("No credible intervals (CI) available; ",
                        "A = blue, B = red; dotted lines: means")
    } else {
      caption <- paste0(
        "Shaded region: ", as.integer(ci * 100), "% CI; ",
        "A = blue, B = red; dotted lines: means")
    }
    
    text <- do.call(
      rbind,
      lapply(groups, function(gr) {
        lapply(types, function(tp) {
          d <- data[data$group == gr & data$type == tp, ]
          if (nrow(d) == 0) return(NULL)
          cri <- suppressMessages(quiet(
            .extract_cri(d$error, method = method, ci = ci)))
          dens <- stats::density(d$error, na.rm = TRUE)
          max_y <- max(dens$y)
          x_range <- max(d$error_uci, na.rm = TRUE) - 
            min(d$error_lci, na.rm = TRUE)
          x_adjust <- x_range * 0.005
          data.frame(
            group = gr,
            type = tp,
            mean = mean(d$error, na.rm = TRUE),
            lci = cri$lci,
            uci = cri$uci,
            max_y = max_y,
            x_adjust = x_adjust
          )
        })
      })
    )
    if (length(text) > 0) text <- do.call(rbind, text)
    
    group_id <- as.numeric(factor(text$group))
    text$y_offset_top <- group_id * 0.1 * text$max_y
    text$y_offset_bottom <- group_id * 0.1 * text$max_y
    
    p <- ggplot2::ggplot(data = data) +
      
      # ggplot2::geom_vline(
      #   xintercept = 0, 
      #   color = "grey50", 
      #   linetype = "solid") +
      
      ggplot2::geom_line(
        data = do.call(
          rbind,
          Filter(
            Negate(is.null),
            unlist(lapply(groups, function(gr) {
              lapply(types, function(tp) {
                d <- data[data$group == gr & data$type == tp, ]
                if (nrow(d) == 0) return(NULL)
                
                dens <- stats::density(d$error, na.rm = TRUE)
                data.frame(x = dens$x,
                           y = dens$y,
                           group = gr,
                           type = tp)
              })
            }), recursive = FALSE)
          )
        ),
        mapping = ggplot2::aes(x = .data$x,
                               y = .data$y,
                               color = .data$group),
        alpha = 0.3, linewidth = 1.1) +
      
      ggplot2::geom_jitter(
        mapping = ggplot2::aes(
          x = .data$error, y = 0,
          color = .data$group),
        height = 0, width = 0.001,
        shape = "|", size = 8, alpha = 0.8,
        inherit.aes = FALSE) +
      
      { if (!is.null(dens) && nrow(dens) > 0)
        ggplot2::geom_area(
          data = dens,
          mapping = ggplot2::aes(x = .data$x, y = .data$y, 
                                 fill = .data$group),
          alpha = 0.18, position = "identity") } +
      
      ggplot2::geom_vline(
        data = text,
        mapping = ggplot2::aes(xintercept = mean, color = group),
        linetype = "dotted", linewidth = 1) +
      
      ggplot2::geom_text(
        data = text,
        ggplot2::aes(
          x = .data$mean + .data$x_adjust,
          y = .data$max_y * 1.05 + .data$y_offset_top,
          label = sprintf(
            "Mean %s = %s",
            .data$group, scales::percent(.data$mean, 0.1)),
          color = .data$group),
        size = 4.5,
        hjust = -0.05,
        show.legend = FALSE) +
      
      { if (!any(is.na(text$lci)))
        ggplot2::geom_text(
          data = text,
          ggplot2::aes(
            x = .data$lci - .data$x_adjust,
            y = .data$y_offset_bottom,
            label = sprintf(
              "LCI %s = %s",
              .data$group, scales::percent(.data$lci, 0.1)),
            color = .data$group),
          size = 4.5,
          vjust = -5, hjust = 1,
          show.legend = FALSE) } +
      
      { if (!any(is.na(text$uci)))
        ggplot2::geom_text(
          data = text,
          ggplot2::aes(
            x = .data$uci + .data$x_adjust,
            y = .data$y_offset_bottom,
            label = sprintf(
              "UCI %s = %s",
              .data$group, scales::percent(.data$uci, 0.1)),
            color = .data$group),
          size = 4.5,
          vjust = -5, hjust = 0,
          show.legend = FALSE)  } +
      
      { if (length(set_target) > 1)
        ggplot2::facet_wrap(
          . ~ .data$type, scales = "free",
          labeller = ggplot2::labeller(
            type = c(
              "hr" = "Home range estimation", 
              "ctsd" = "Speed \u0026 distance estimation"))) } +
      
      ggplot2::scale_color_manual(
        values = pal, drop = FALSE, guide = "none") +
      ggplot2::scale_fill_manual(
        values = pal, drop = FALSE, guide = "none") +
      ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = scales::breaks_pretty()
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.05)),
        limits = c(0, NA)
      ) +
      ggplot2::labs(
        title = if (!is.null(obj$data$n_replicates))
          paste("Number of replicates:", obj$data$n_replicates)
        else NULL,
        caption = caption,
        x = "Relative error (%)", y = NULL) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 14),
        axis.text.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        plot.margin = ggplot2::unit(c(0.2, 0.2, 0.3, 0.2), "cm")) +
      ggplot2::guides(color = "none", fill = "none", shape = "none")
    
    return(p)
    
  }
}


#' @title Preview plot for movedesign workflow outputs (single replicate)
#'
#' @description
#' Generates a quick visualization of relative error for home range or
#' movement speed estimation from a single replicate of a movedesign
#' workflow. This functions shows preliminary outputs only and should not
#' be used to evaluate study design.
#' 
#' Use the output of [md_run()] (a
#' `movedesign_preprocess` object). Users should run [md_replicate()] for
#' a full assessment.
#'
#' @param obj An object of class `movedesign_preprocess` 
#'   (output of [md_run()]).
#' @param error_threshold Numeric. Error threshold (e.g. `0.05` for 5%)
#'   to display as a reference in the plot.
#' @param pal Character vector of two colors for within/outside threshold
#'   (default: c("#007d80", "#A12C3B")).
#'
#' @return
#' A ggplot object displaying relative error by population sample size,
#' with point estimate and confidence intervals for mean estimates,
#' and horizontal error threshold lines.
#'
#' @details
#' For robust results and credible intervals, use [md_replicate()].
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
#'   output <- md_run(input)
#'   md_plot_preview(output, error_threshold = 0.05)
#' }
#' 
#' @seealso [md_run()], [md_replicate()]
#' @export
md_plot_preview <- function(obj,
                            error_threshold = 0.05,
                            pal = c("#007d80", "#A12C3B")) {
  
  x <- y <- type <- group <- NULL
  
  if (!inherits(obj, "movedesign_preprocess")) {
    stop(paste(
      "Input does not appear to be a 'movedesign_preprocess' object.",
      "\nPlease provide the output of md_run()."))
  }
  
  iter_step <- ifelse(length(obj$simList) <= 10, 2, 4)
  out <- run_meta(obj, 
                  set_target = obj$set_target,
                  subpop = obj$grouped, 
                  iter_step = iter_step,
                  trace = TRUE)
  
  out <- out %>% 
    dplyr::mutate(
      dplyr::across(
        type, ~factor(., levels = c("hr", "ctsd")))) %>% 
    dplyr::mutate(
      overlaps = dplyr::between(
        .data$error, 
        -error_threshold, error_threshold),
      overlaps = factor(
        .data$overlaps, levels = c("TRUE", "FALSE")))
  
  if (obj$grouped) {
    
    out <- dplyr::filter(out, group != "All") 
    out_mean <- out %>%
      dplyr::group_by(.data$group, .data$type) %>%
      dplyr::slice_max(.data$m) %>%
      dplyr::summarise(
        error_mean = .data$error,
        x_pos = .data$m,
        y_pos = .data$error_lci,
        color = ifelse(abs(.data$error) < error_threshold,
                       pal[1], pal[2]),
        .groups = "drop")
    
    p <- out %>%
      ggplot2::ggplot(
        ggplot2::aes(x = .data$m,
                     y = .data$error,
                     shape = .data$group,
                     group = .data$group,
                     color = .data$overlaps)) +
      
      ggplot2::geom_hline(
        yintercept = error_threshold,
        alpha = 0.5,
        linetype = "dotted", linewidth = 0.4) +
      ggplot2::geom_hline(
        yintercept = -error_threshold,
        alpha = 0.5,
        linetype = "dotted", linewidth = 0.4) +
      
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.3,
        linetype = "solid") +
      
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$error_lci,
                     ymax = .data$error_uci),
        position = ggplot2::position_dodge(width = 0.5),
        linewidth = 2.5, alpha = .2,
        show.legend = TRUE) +
      ggplot2::geom_line(
        position = ggplot2::position_dodge(width = 0.5),
        linewidth = 0.6, alpha = 0.5,
        show.legend = TRUE) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.5),
        size = 3,
        show.legend = TRUE) +
      
      { if (length(obj$set_target) > 1)
        ggplot2::facet_wrap(
          . ~ .data$type, scales = "free_y",
          labeller = ggplot2::labeller(
            type = c(
              "hr" = "Home range estimation", 
              "ctsd" = "Speed \u0026 distance estimation"))) } +
      
      ggplot2::labs(
        x = "Population sample size",
        y = "Relative error (%)",
        color = paste0("Within error threshold (\u00B1",
                       error_threshold * 100, "%)?"),
        caption = paste(
          "This plot displays preliminary outputs from a",
          "single replicate.\n",
          "For robust inferences, run multiple replicates using",
          "`md_replicate()`.")) +
      
      ggplot2::scale_x_continuous(
        breaks = scales::breaks_pretty()) +
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        breaks = scales::breaks_pretty()) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = pal[1],
                   "FALSE" = pal[2]), drop = FALSE) +
      ggplot2::scale_shape_manual(
        "Groups:", values = c(16, 17)) +
      
      ggplot2::theme_classic() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 18),
        legend.position = "bottom",
        strip.text = ggplot2::element_text(size = 18),
        strip.background.x = ggplot2::element_rect(
          color = NA, fill = NA),
        strip.background.y = ggplot2::element_rect(
          color = NA, fill = NA),
        plot.margin = ggplot2::unit(c(0.2, 0.2, 0.3, 0.2), "cm")) +
      ggplot2::guides()
    
  } else {
    
    # y_offset_df <- out %>%
    #   dplyr::group_by(.data$type) %>%
    #   dplyr::summarise(
    #     y_range = max(.data$error, na.rm = TRUE) - 
    #                   min(.data$error, na.rm = TRUE),
    #     .groups = "drop"
    #   )
    
    out_mean <- out %>%
      dplyr::group_by(.data$group, .data$type) %>%
      dplyr::slice_max(.data$m) %>%
      dplyr::summarise(
        error_mean = .data$error,
        x_pos = .data$m,
        y_pos = .data$error_lci,
        color = ifelse(abs(.data$error) < error_threshold,
                       pal[1], pal[2]),
        .groups = "drop"
      ) # %>%
    # dplyr::left_join(y_offset_df, by = "type") %>%
    # dplyr::mutate(
    #   y_offset = ifelse(is.finite(y_range) & y_range > 0,
    #                     y_range * 0.01, 0.01),
    #   y_pos_label = y_pos - y_offset)
    
    p <- out %>%
      ggplot2::ggplot(
        ggplot2::aes(x = .data$m,
                     y = .data$error,
                     group = .data$group,
                     color = .data$overlaps)) +
      
      ggplot2::geom_hline(
        yintercept = error_threshold,
        alpha = 0.5,
        linetype = "dotted", linewidth = 0.4) +
      ggplot2::geom_hline(
        yintercept = -error_threshold,
        alpha = 0.5,
        linetype = "dotted", linewidth = 0.4) +
      
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.3,
        linetype = "solid") +
      
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$error_lci,
                     ymax = .data$error_uci),
        position = ggplot2::position_dodge(width = 0.8),
        linewidth = 2.5, alpha = .2,
        show.legend = TRUE) +
      ggplot2::geom_line(
        position = ggplot2::position_dodge(width = 0.8),
        linewidth = 0.6, alpha = 0.5,
        show.legend = TRUE) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.8),
        size = 3,
        show.legend = TRUE) +
      
      { if (length(obj$set_target) > 1)
        ggplot2::facet_wrap(
          . ~ .data$type, scales = "free_y",
          labeller = ggplot2::labeller(
            type = c(
              "hr" = "Home range estimation", 
              "ctsd" = "Speed \u0026 distance estimation"))) } +
      
      ggplot2::labs(
        x = "Population sample size",
        y = "Relative error (%)",
        color = paste0("Within error threshold (\u00B1",
                       error_threshold * 100, "%)?"),
        caption = paste(
          "This plot displays preliminary outputs from a",
          "single replicate.\n",
          "For robust inferences, run multiple replicates using",
          "`md_replicate()`.")) +
      
      ggplot2::scale_x_continuous(
        breaks = scales::breaks_pretty()) +
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        breaks = scales::breaks_pretty()) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = pal[1],
                   "FALSE" = pal[2]), drop = FALSE) +
      
      ggplot2::theme_classic() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 18),
        legend.position = "bottom",
        strip.text = ggplot2::element_text(size = 18),
        strip.background.x = ggplot2::element_rect(
          color = NA, fill = NA),
        strip.background.y = ggplot2::element_rect(
          color = NA, fill = NA),
        plot.margin = ggplot2::unit(c(0.2, 0.2, 0.3, 0.2), "cm")) +
      ggplot2::guides(shape = "none")
  }
  
  warning(
    "You are viewing preliminary results from a single replicate. ",
    "To assess variability and credible intervals, please run ",
    "`md_replicate()` to generate multiple replicates and ",
    "aggregate results.")
  
  return(p)
  
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
#'   tolerance required to confirm convergence. Defaults to `10`.
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
#'   # After running a simulation or resampling:
#'   md_check(output, tol = 0.05, n_converge = 10)
#' }
#' 
#' @importFrom dplyr filter
#' 
#' @export
md_check <- function(obj,
                     m = NULL,
                     tol = 0.05,
                     n_converge = 10,
                     plot = TRUE,
                     pal = c("#007d80", "#A12C3B")) {
  
  if (!inherits(obj, "movedesign") ||
      !("summary" %in% names(obj))) {
    stop(paste(
      "Input does not appear to be a 'movedesign_output' object.",
      "\nPlease provide the output of md_run()."))
  }
  
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
  
  data_subset <- data %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::mutate(
      cummean = cumsum(.data[[variable]]) / dplyr::row_number()
    ) %>%
    dplyr::ungroup()
  
  dt_plot <- data_subset %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::mutate(
      index = dplyr::row_number(),
      value = c(NA, abs(diff(.data$cummean)))) %>%
    dplyr::ungroup()
  
  get_r <- ifelse(has_groups,
                  length(data_subset$cummean) / 2,
                  length(data_subset$cummean))
  if (length(set_target) > 1) get_r <- get_r / 2
  if (get_r < n_converge) {
    stop(sprintf(
      "Not enough replicates (n = %d) to check %d convergence steps.",
      get_r, n_converge))
  }  
  
  # Compute convergence diagnostics:
  diag <- data_subset %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::summarise(
      mean_error = mean(.data$error, na.rm = TRUE),
      recent_cummean = list(tail(.data$cummean, n_converge)),
      recent_deltas = list(abs(diff(.data$recent_cummean[[1]]))),
      max_delta = max(.data$recent_deltas[[1]], na.rm = TRUE),
      has_converged = all(.data$recent_deltas[[1]] < tol),
      .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$type))
  
  if (plot) {
    
    .find_stable_x <- function(deltas, tol) {
      n <- length(deltas)
      below_tol <- deltas < tol
      for (i in seq_len(n)) {
        if (is.na(below_tol[i])) next
        if (below_tol[i] && all(below_tol[i:n], na.rm = TRUE)) {
          return(i + 1)
        }
      }
      return(NA_integer_)
    }
    
    dt_stable_idx <- data_subset %>%
      dplyr::select(.data$type, .data$group, .data$cummean) %>%
      dplyr::group_by(.data$type, .data$group) %>%
      dplyr::filter(!is.na(.data$cummean)) %>%
      dplyr::filter(dplyr::n() >= n_converge + 1) %>%
      dplyr::summarise(
        idx_stable_start = .find_stable_x(
          deltas = abs(diff(.data$cummean)),
          tol = tol), .groups = "drop")
    label_y <- paste0("|", "\u0394", "(cumulative mean)|")
    
    dt_plot <- dt_plot %>%
      dplyr::left_join(diag, by = c("type", "group")) %>%
      dplyr::left_join(dt_stable_idx, by = c("type", "group"))
    
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
          paste(.data$type, .data$group, sep = " / ")
        } else {
          dplyr::case_when(
            .data$type == "hr" ~ "mean home range",
            .data$type == "ctsd" ~ "mean speed",
            TRUE ~ as.character(.data$type))
        }
      ) %>%
      dplyr::pull(.data$group_label)
    
    if (has_groups) {
      failed_groups <- gsub("^hr", "mean home range", failed_groups)
      failed_groups <- gsub("^ctsd", "mean speed", failed_groups)
    }
    
    subtitle_text <- if (length(failed_groups) == 0) {
      sprintf("All groups converged: last %d values within \u00b1 %g",
              n_converge - 1, tol)
    } else if (length(set_target) > 1) {
      paste(
        sprintf("Did not converge: %s (tolerance = \u00b1 %g)", 
                failed_groups, tol),
        collapse = "\n"
      )
    } else {
      sprintf("Did not converge: %s (tolerance = \u00b1 %g)", 
              paste(failed_groups, collapse = ", "), tol)
    }
    
    dt_plot$type_f <- factor(dt_plot$type, levels = c("hr", "ctsd"))
    dt_rect$type_f <- factor(dt_rect$type, levels = c("hr", "ctsd"))
    
    p <- NULL
    p <- dt_plot %>%
      ggplot2::ggplot(
        ggplot2::aes(x = .data$index, 
                     y = .data$value,
                     group = .data$group,
                     linetype = .data$group,
                     color = .data$has_converged)) +
      
      { if (length(set_target) > 1)
        ggplot2::facet_wrap(
          . ~ .data$type_f, # scales = "free",
          labeller = ggplot2::labeller(
            type_f = c(
              "hr" = "Home range estimation", 
              "ctsd" = "Speed \u0026 distance estimation"))) } +
      
      ggplot2::geom_line(linewidth = 1.1) +
      
      ggplot2::geom_hline(
        yintercept = tol,
        linetype = "dashed") +
      
      { if (nrow(dt_rect) > 0) 
        ggplot2::geom_rect(
          data = dt_rect,
          mapping = ggplot2::aes(
            xmin = .data$xmin,
            xmax = .data$xmax, 
            ymin = -Inf, 
            ymax = Inf),
          fill = "black",
          alpha = 0.08,
          inherit.aes = FALSE) } +
      
      ggplot2::scale_color_manual(
        values = c("TRUE" = pal[1],
                   "FALSE" = pal[2]),
        drop = FALSE) +
      
      { if (has_groups)
        ggplot2::scale_linetype_manual(
          "Groups:", values = c("solid", "dashed"),
          drop = FALSE) } +
      
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.05)),
        limits = c(0, NA)) +
      
      ggplot2::labs(
        title = "Stepwise change in cumulative mean",
        subtitle = subtitle_text,
        x = "Replicate index",
        y = label_y
      ) +
      
      ggplot2::theme_classic() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 18),
        strip.text = ggplot2::element_text(size = 18),
        strip.background.x = ggplot2::element_rect(
          color = NA, fill = NA),
        strip.background.y = ggplot2::element_rect(
          color = NA, fill = NA),
        legend.position = "bottom",
        legend.key.width = ggplot2::unit(1, "cm"),
        plot.title = ggplot2::element_text(face = "bold"),
        plot.subtitle = ggplot2::element_text(face = "italic"),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::unit(c(0.2, 0.2, 0.3, 0.2), "cm")) +
      ggplot2::guides(color = "none", fill = "none")
    
    if (!has_groups)
      p <- p + ggplot2::guides(linetype = "none")
    
    if (length(set_target) > 1)
      p <- p + ggplot2::theme(
        plot.subtitle = ggplot2::element_text(lineheight = 0.7))
    
    suppressWarnings(print(p))
  }
  
  # if (verbose) {
  #   
  #   for (i in seq_len(nrow(diag))) {
  #     
  #     if (has_groups) {
  #       tg <- paste0(
  #         ifelse(diag$type[i] == "hr", "home range estimation",
  #                ifelse(diag$type[i] == "ctsd", "speed estimation",
  #                       diag$type[i])),
  #         " - Group ", 
  #         as.character(diag$group[i]))
  #     } else {
  #       tg <- paste0(
  #         ifelse(diag$type[i] == "hr", "home range estimation",
  #                ifelse(diag$type[i] == "ctsd", "speed estimation",
  #                       diag$type[i])))
  #     }
  #     
  #     mean_error_i <- diag$mean_error[i]
  #     recent_cummean_i <- diag$recent_cummean[[i]]
  #     max_delta_i <- diag$max_delta[i]
  #     has_converged_i <- diag$has_converged[i]
  #     n_eval <- length(recent_cummean_i)
  #     type_i <- diag$type[i]
  #     
  #     if (has_groups) {
  #       if (i == 1 && type_i == "hr") {
  #         .header("Home range estimation", 5) }
  #       if (i == 3 && type_i == "ctsd") {
  #         .header("Speed \u0026 distance estimation", 5) }
  #     } else {
  #       if (i == 1 && type_i == "hr") {
  #         .header("Home range estimation", 5) }
  #       if (i == 2 && type_i == "ctsd") {
  #         .header("Speed \u0026 distance estimation", 5) }
  #     }
  #     
  #     message(sprintf(
  #       "Convergence check for %s", tg))
  #     if (abs(mean_error_i) < tol) {
  #       message(
  #         "   Mean estimate error: ",
  #         .msg(paste0(.err_to_txt(mean_error_i), "%"), "success"))
  #     } else {
  #       message(
  #         "   Mean estimate error: ",
  #         .msg(paste0(.err_to_txt(mean_error_i), "%"), "danger"))
  #     }
  #     message(sprintf(
  #       "   Tolerance: %g, Steps evaluated: %d", tol, n_eval))
  #     message(sprintf(
  #       "   Max |\u0394(mean)| in last steps: %g", max_delta_i))
  #     
  #     if (has_converged_i) {
  #       message(sprintf(
  #         "\u2713 Converged: all %d steps within \u00B1%g.", 
  #         n_eval - 1, tol))
  #     } else {
  #       message(
  #         .msg("\u2717 Did not converge: ", "danger"),
  #         "at least one step exceeded tolerance.")
  #     }
  #     message("")
  #   }
  # }
  
  structure(list(
    # recent_deltas = recent_deltas,
    # max_delta = max_delta,
    grouped = has_groups,
    diagnostics_table = diag,
    tolerance = tol,
    n_converge = n_converge,
    has_converged = diag$has_converged,
    stabilized_at = dt_stable_idx,
    cumulative_means = data_subset %>%
      dplyr::select(c(.data$type,
                      .data$group,
                      .data$cummean)) %>%
      dplyr::arrange(dplyr::desc(.data$type))
  ), class = "movedesign_check")
  
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
#' a list of telemetry objects, obtained from [`ctmm::as.telemetry()`]).
#' Arguments with defaults can be skipped using <Enter>. The argument
#' `models` is optional, and if omitted, models will be fitted
#' automatically.
#'
#' @param data A named list of telemetry objects (from
#'   [ctmm::as.telemetry()]) to be used as the empirical basis for the
#'   simulations. Each telemetry object must contain valid metadata
#'   and timestamped locations.
#' @param models (Optional) Named list of fitted ctmm models (from
#'   [ctmm::ctmm.fit()] or [ctmm::ctmm.select()]). If not supplied,
#'   models are fitted automatically.
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
#' @export
md_configure <- function(data, models = NULL) {
  
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
  
  cat(.header("Movement design interactive setup:"))
  
  # Prompt for species label:
  species <- .ask_input(
    "Species label", NULL, function(x) nzchar(x),
    "(scientific or common name)?"
  )
  
  # Fit models automatically (if not provided):
  if (is.null(models)) {
    cat("\nNo models provided. Fitting models automatically...\n")
    models <- fitting_model(data, .parallel = FALSE)
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
      "Find the minimum population sample size required for a target")
  )
  
  # sample_mode <- .ask_choice(
  #   "Do you want to:",
  #   c("Set a specific population sample size (number of tags)",
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
    c(paste0("Mean estimate of ", .msg("sampled population", "success")),
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
  
  .header("Sampling parameters:")
  
  # Sampling duration:
  
  if (sample_mode == 1) {
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
    sigma <- extract_pars(meanfitList, "sigma")
    tau_p <- extract_pars(meanfitList, "position")
    tau_v <- extract_pars(meanfitList, "velocity")
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
      extract_pars(
        obj = fit[[x]],
        name = "sigma", meta = TRUE)[[1]]
    }))
    names(sigma) <- c("All", "A", "B") 
    
    tau_p <- c(tau_p, lapply(1:2, function(x) {
      extract_pars(
        obj = fit[[x]], 
        name = "position", meta = TRUE)[[1]]
    }))
    names(tau_p) <- c("All", "A", "B") 
    
    tau_v <- c(tau_v, lapply(1:2, function(x) {
      extract_pars(
        obj = fit[[x]], 
        name = "velocity", meta = TRUE)[[1]]
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


#' @title Optimize population sample size and sampling parameters
#'
#' @description
#' Repeatedly simulates movement datasets across a range of
#' candidate population sample sizes to identify the minimal sample size
#' and associated sampling parameters (e.g., duration, sampling interval)
#' required to achieve a predefined error threshold for key space-use and
#' movement metrics (home range area, or speed).
#'
#' The function quantifies estimation error for each metric and sample
#' size, evaluates which population sample size reliably meets target
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
#' @param error_threshold Numeric. Error threshold (e.g. `0.05` for 5%)
#'   to display as a reference in the plot.
#' @param verbose Logical. If `TRUE` (default), prints a summary
#'   of the convergence check to the console.
#' @param trace Logical; if `TRUE` (default), prints progress and
#'   timing messages to the console.
#' @param parallel Logical; if `TRUE`, enables parallel processing via
#'   [`parallel::mclapply()`]. Default is `FALSE` (serial execution).
#' @param ncores Integer; number of CPU cores to use for parallel
#'   processing. Defaults to all available cores detected by
#'   [`parallel::detectCores()`].
#' @param plot Logical. If TRUE, displays a diagnostic plot of
#'   the final results.
#' @param ... Additional arguments used internally.
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
#'                      n_replicates = 25,
#'                      error_threshold = 0.05,
#'                      plot = TRUE)
#' }
#' 
#' @seealso
#' [`md_prepare()`], [`md_configure()`]
#' 
#' @export
md_optimize <- function(obj,
                        n_replicates = NULL,
                        error_threshold = NULL,
                        verbose = FALSE,
                        trace = TRUE,
                        parallel = FALSE,
                        ncores = parallel::detectCores(),
                        plot = FALSE,
                        ...) {
  
  `%||%` <- function(x, y) if (!is.null(x)) x else y
  
  dots <- list(...)
  .n_converge <- dots[[".n_converge"]] %||% 5
  .tol <- dots[[".tol"]] %||% 0.05
  
  . <- type <- NULL
  stopifnot(inherits(obj, "movedesign_input"))
  stopifnot(is.numeric(error_threshold) &&
              error_threshold > 0 && error_threshold < 1)
  stopifnot(is.numeric(n_replicates) && n_replicates > 0)
  
  .worker <- function(r, m) {
    if (trace) message(.msg(
      sprintf("\u2014 Replicate %s of %s", r,
              n_replicates), "main"))
    
    obj$n_individuals <- ifelse(obj$grouped, m * 2, m)
    out <- md_run(obj, trace = FALSE)
    return(out)
  }
  
  set_target <- obj$set_target
  has_groups <- obj$grouped
  tau_p <- obj$tau_p
  tau_v <- obj$tau_v
  .max_m <- obj$n_individuals
  
  group <- groups <- top_facet <- NULL
  hr_dur <- hr_dti <- ctsd_dur <- ctsd_dti <- NULL
  
  if (length(set_target) == 1) {
    err_prev <- setNames(list(rep(1, 10)), set_target)
  } else if (length(set_target) == 2) {
    err_prev <- list("hr" = rep(1, 10), "ctsd" = rep(1, 10))
  }
  
  for (target in set_target) {
    optimal_dur <- round((tau_p[[1]]$value[2] %#% 
                            tau_p[[1]]$unit[2]) * 100)
    optimal_dti <- round((tau_v[[1]]$value[2] %#% 
                            tau_v[[1]]$unit[2]) / 3)
    
    if (target == "hr") {
      dti <- 1 %#% "day"
      dur <- optimal_dur
      count <- 0
      repeat {
        n <- floor(dur / dti)
        if (n <= 2000 || count > 20) break
        dti <- dti * 2
        count <- count + 1
      }
      hr_dur <- fix_unit(dur, "seconds", convert = TRUE)
      hr_dti <- fix_unit(dti, "seconds", convert = TRUE)
    }
    
    if (target == "ctsd") {
      dur <- 8 %#% "days"
      dti <- optimal_dti
      count <- 0
      repeat {
        n <- floor(dur / dti)
        if ((n >= 1000 && n <= 2000) || count > 20) break
        dur <- dur / 2
        count <- count + 1
        if (dur < 2 %#% "days") break
      }
      ctsd_dur <- fix_unit(dur, "seconds", convert = TRUE)
      ctsd_dti <- fix_unit(dti, "seconds", convert = TRUE)
    }
  } # end of [target] loop
  
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
  
  .iter_step <- 2
  m_seq <- .get_sequence(seq_len(.max_m),
                         .step = .iter_step,
                         .max_m = .max_m,
                         .automate_seq = TRUE,
                         grouped = obj$grouped)
  
  message(.msg(paste(
    "Running simulations",
    "for the following population sample sizes:"), "success"))
  msg_seq <- format(m_seq, big.mark = ",", trim = TRUE)
  msg_seq <- paste(msg_seq, collapse = ", ")
  msg_seq <- sub(", ([^,]+)$", " and \\1", msg_seq)
  suffix <- ifelse(!is.null(groups),
                   " individuals per group", " individuals")
  message(.msg(sprintf("  %s%s (total of %d sets)\n",
                       msg_seq, suffix, length(m_seq)), "success"))
  
  broke <- FALSE
  outList <- vector("list", length(m_seq))
  summaryList <- vector("list", length(m_seq))
  
  start_total <- Sys.time()
  
  for (i in seq_along(m_seq)) {
    
    m_current <- m_seq[[i]]
    m_prev <- ifelse(i == 1, 0, m_seq[[i - 1]])
    m <- m_current - m_prev
    
    message(.msg(
      sprintf("Set %s out of %s...", i, length(m_seq)),
      "success"))
    print(paste(
      "Current sampled population:", 
      if (!is.null(groups)) m_current * 2 else m_current, 
      "individual(s)"
    ))
    
    reps <- 0
    max_replicates <- 100
    tmpList <- list()
    metaList <- list()
    
    broke_converge <- FALSE
    while (!broke_converge && reps < max_replicates) {
      
      reps <- reps + 1
      batch_tmpList <- if (parallel) {
        
        parallel::mclapply(
          seq_len(n_replicates), function(r) .worker(r, m),
          mc.cores = ncores)
        
      } else {
        
        if (!trace) pb <- txtProgressBar(
          min = 0, max = n_replicates, style = 3)
        
        res <- vector("list", n_replicates)
        for (r in seq_len(n_replicates)) {
          res[[r]] <- .worker(r, m)
          if (!trace) setTxtProgressBar(pb, r)
        }
        
        if (!trace) close(pb)
        res
        
      }
      
      if (length(tmpList) == 0) {
        tmpList <- batch_tmpList
      } else {
        tmpList <- c(tmpList, batch_tmpList)
      }
      
      batch_metaList <- lapply(batch_tmpList, function(x) {
        run_meta_resamples(x,
                           set_target = obj$set_target,
                           subpop = obj$grouped,
                           .m = m_seq[[i]])
      })
      metaList <- c(metaList, batch_metaList)
      
      if (length(metaList) > 0) {
        summary <- data.table::rbindlist(
          metaList, fill = TRUE, idcol = "replicate")
      } else {
        summary <- data.table::data.table()
      }
      
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
      
      diag <- data_subset %>%
        dplyr::group_by(.data$type, .data$group) %>%
        dplyr::summarise(
          recent_cummean = list(tail(.data$cummean, .n_converge)),
          recent_deltas = list(abs(diff(.data$recent_cummean[[1]]))),
          max_delta = max(.data$recent_deltas[[1]], na.rm = TRUE),
          has_converged = all(.data$recent_deltas[[1]] < .tol),
          .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(.data$type))
      
      if (diag$has_converged) {
        broke_converge <- TRUE
        break
      }
      
    } # end of while
    
    if (i == 1) {
      outList[[i]] <- tmpList
      class(outList[[i]]) <- "movedesign_preprocess"
    }
    
    nms <- list()
    if (i > 1) {
      
      if (has_groups) {
        
        group_keys <- c("A", "B")
        common_names <- tmpList[[1]]$groups[[1]]
        
        for (x in seq_along(tmpList)) {
          ids_tmp <- tmpList[[x]]$groups[[2]]
          ids_out <- outList[[i - 1]][[x]]$groups[[2]]
          
          merged_ids <- setNames(
            lapply(group_keys, function(gr) {
              c(ids_tmp[[gr]], ids_out[[gr]])
            }), group_keys)
          
          nms[[x]] <- list(common_names, merged_ids)
          tmpList[[x]]$groups <- nms[[x]]
          outList[[i - 1]][[x]]$groups <- nms[[x]]
        }
      }
      
      tmpList <- Map(function(prev, curr) {
        merged <- md_merge(prev, curr)
        merged$n_individuals <- m_current
        return(merged)
      }, outList[[i - 1]], tmpList)
      
      outList[[i]] <- tmpList
      class(outList[[i]]) <- "movedesign_preprocess"
    }
    class(outList[[i]][[1]])
    names(outList[[i]][[1]])
    
    metaList <- lapply(tmpList, function(x) {
      run_meta_resamples(x,
                         set_target = obj$set_target,
                         subpop = obj$grouped,
                         .m = m_seq[[i]])
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
    
    if (obj$which_meta == "mean") {
      
      overlaps_with_truth <- dplyr::between(
        unique(data$truth),
        mean(data$lci, na.rm = TRUE),
        mean(data$uci, na.rm = TRUE))
      
      all(unlist(err_replicates) < error_threshold)
      all(err_values < error_threshold)
      overlaps_with_truth
      diag$has_converged
      
      if (!diag$has_converged) {
        warning(
          sprintf(
            "Model failing to converge with %s replicates. %s",
            n_replicates, "Consider increasing 'n_replicates'."
          ), call. = FALSE)
      }
      
      if (all(err_values < error_threshold) &&
          all(unlist(err_replicates) < error_threshold) &&
          overlaps_with_truth && diag$has_converged) {
        broke <- TRUE
        break
      }
      
    } # end of if (which_meta == "mean")
    
    if (obj$which_meta == "compare") {
      
      cov <- Inf
      if (all(err_values < error_threshold)) {
        
        input <- .get_groups(obj$fitList, groups = groups)
        
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
            if (target == "ctsd") tmp_input <- tmp$akdeList
            
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
        
        # out_cov
        # overlaps_with_truth
        # ratios
        
        cov <- lapply(out_cov, tail, n = 1)
        
        # if cov -> infinity,
        # still sensitive to small changes in the mean.
        if (!all(is.infinite(unlist(cov))) && 
            all(unlist(overlaps_with_truth))) {
          broke <- TRUE
          break
        }
      }
      
    } # end of if (which_meta == "compare")
    
  } # end of [i] loop
  
  summary_full <- data.table::rbindlist(summaryList, fill = TRUE)
  
  if (plot) {
    
    pal <- list("TRUE" = "#007d80", "FALSE" = "#A12C3B")
    
    if (has_groups) {
      dt_plot <- summary_full[summary_full$group != "All", ]
    } else {
      dt_plot <- summary_full[summary_full$group == "All", ]
    }
    
    dt_plot <- dt_plot %>%
      dplyr::mutate(
        error_threshold = error_threshold,
        overlaps = dplyr::between(
          .data$error,
          -.data$error_threshold, .data$error_threshold)) 
    dt_plot_T <- dplyr::filter(dt_plot, overlaps)
    dt_plot_F <- dplyr::filter(dt_plot, !overlaps)
    
    dt_plot_means <- dt_plot %>% 
      dplyr::mutate(type = factor(
        .data$type, levels = c("hr", "ctsd"))) %>%
      dplyr::select(
        "type", "m", "group",
        "error", "error_lci", "error_uci") %>% 
      dplyr::distinct() %>%
      dplyr::group_by(.data$type, .data$group, .data$m) %>% 
      dplyr::summarize(
        n = dplyr::n(),
        error_mean_lci = mean(.data$error_lci, na.rm = TRUE),
        error_mean_uci = mean(.data$error_uci, na.rm = TRUE),
        error_mean = mean(.data$error, na.rm = TRUE),
        .groups = "drop") %>%
      dplyr::mutate(
        error_threshold = error_threshold,
        overlaps = dplyr::between(
          .data$error_mean,
          -.data$error_threshold, .data$error_threshold),
        overlaps = factor(.data$overlaps,
                          levels = c("TRUE", "FALSE")),
        top_facet = .data$type == "hr") %>%
      dplyr::ungroup()
    
    if (!has_groups) {
      
      p <- dt_plot_means %>%
        ggplot2::ggplot(
          ggplot2::aes(x = .data$m,
                       y = .data$error_mean,
                       color = .data$overlaps,
                       fill = .data$overlaps,
                       group = .data$m)) +
        
        { if (length(set_target) > 1)
          ggplot2::facet_wrap(
            ggplot2::vars(type),
            scales = "free") } +
        
        ggplot2::geom_jitter(
          dt_plot_F,
          mapping = ggplot2::aes(x = .data$m,
                                 y = .data$error,
                                 group = .data$replicate),
          position = ggplot2::position_dodge(width = 1),
          size = 3, shape = 21, alpha = 0.5,
          color = pal[["FALSE"]],
          fill = pal[["FALSE"]]
        ) +
        
        ggplot2::geom_jitter(
          data = dt_plot_T,
          mapping = ggplot2::aes(x = .data$m,
                                 y = .data$error,
                                 group = .data$sample),
          position = ggplot2::position_dodge(width = 1),
          size = 3, shape = 21, alpha = 0.5,
          color = pal[["TRUE"]],
          fill = pal[["TRUE"]]
        ) +
        
        ggplot2::geom_hline(
          yintercept = 0,
          linewidth = 0.3,
          linetype = "solid") +
        ggplot2::geom_hline(
          data = subset(dt_plot_means, top_facet),
          ggplot2::aes(yintercept = error_threshold),
          linewidth = 0.7,
          linetype = "dotted") +
        ggplot2::geom_hline(
          data = subset(dt_plot_means, top_facet),
          ggplot2::aes(yintercept = -error_threshold),
          linewidth = 0.7,
          linetype = "dotted") +
        
        ggplot2::geom_hline(
          data = subset(dt_plot_means, !top_facet),
          ggplot2::aes(yintercept = error_threshold),
          linewidth = 0.7,
          linetype = "dotted") +
        ggplot2::geom_hline(
          data = subset(dt_plot_means, !top_facet),
          ggplot2::aes(yintercept = -error_threshold),
          linewidth = 0.7,
          linetype = "dotted") +
        
        ggplot2::geom_linerange(
          ggplot2::aes(ymin = .data$error_mean_lci,
                       ymax = .data$error_mean_uci),
          show.legend = TRUE,
          position = ggplot2::position_dodge(width = 0.4),
          color = "black", linewidth = 0.4) +
        ggplot2::geom_point(
          show.legend = TRUE,
          position = ggplot2::position_dodge(width = 0.4),
          color = "black", shape = 21, size = 5) +
        
        ggplot2::labs(
          x = "Population sample size",
          y = "Relative error (%)") +
        
        ggplot2::scale_fill_manual(
          name = paste0("Within error threshold (\u00B1",
                        error_threshold * 100, "%)?"),
          breaks = c("TRUE", "FALSE"),
          values = pal, drop = FALSE,
          guide = ggplot2::guide_legend(
            override.aes = list(color = pal,
                                fill = pal,
                                size = 3),
            order = 1,
            text.vjust = 4,
            label.vjust = 0.4,
            theme = ggplot2::theme(
              legend.key.width = ggplot2::unit(2, "lines"),
              legend.key.height = ggplot2::unit(0.5, "lines")))) +
        
        ggplot2::scale_shape_manual(values = c(16, 16)) +
        ggplot2::scale_x_continuous(
          breaks = scales::breaks_pretty()) +
        ggplot2::scale_y_continuous(
          labels = scales::percent,
          breaks = scales::breaks_pretty()) +
        
        ggplot2::theme_classic() +
        ggplot2::theme(
          text = ggplot2::element_text(size = 15),
          legend.position = "bottom",
          strip.text = ggplot2::element_text(size = 18),
          strip.background.x = ggplot2::element_rect(
            color = NA, fill = NA),
          strip.background.y = ggplot2::element_rect(
            color = NA, fill = NA),
          plot.margin = ggplot2::unit(
            c(1, 1, 1, 1), "cm")) +
        ggplot2::guides(shape = "none")
      
    } else {
      
      p <- dt_plot_means %>%
        ggplot2::ggplot(
          ggplot2::aes(x = .data$m,
                       y = .data$error_mean,
                       group = .data$group,
                       shape = .data$group,
                       fill = .data$overlaps)) +
        
        { if (length(set_target) > 1)
          ggplot2::facet_wrap(
            ggplot2::vars(type),
            scales = "free") } +
        
        ggplot2::geom_jitter(
          dt_plot,
          mapping = ggplot2::aes(
            x = .data$m,
            y = .data$error,
            group = .data$group,
            shape = .data$group,
            fill = .data$overlaps),
          position = ggplot2::position_jitterdodge(dodge.width = 0.4),
          size = 3, alpha = 0.5, color = "transparent") +
        
        ggplot2::geom_hline(
          yintercept = 0,
          linewidth = 0.3,
          linetype = "solid") +
        ggplot2::geom_hline(
          data = subset(dt_plot_means, top_facet),
          ggplot2::aes(yintercept = error_threshold),
          linewidth = 0.7,
          linetype = "dotted") +
        ggplot2::geom_hline(
          data = subset(dt_plot_means, top_facet),
          ggplot2::aes(yintercept = -error_threshold),
          linewidth = 0.7,
          linetype = "dotted") +
        
        ggplot2::geom_hline(
          data = subset(dt_plot_means, !top_facet),
          ggplot2::aes(yintercept = error_threshold),
          linewidth = 0.7,
          linetype = "dotted") +
        ggplot2::geom_hline(
          data = subset(dt_plot_means, !top_facet),
          ggplot2::aes(yintercept = -error_threshold),
          linewidth = 0.7,
          linetype = "dotted") +
        
        ggplot2::geom_linerange(
          ggplot2::aes(ymin = .data$error_mean_lci,
                       ymax = .data$error_mean_uci),
          position = ggplot2::position_dodge(width = 0.4),
          color = "black", linewidth = 0.4,
          show.legend = FALSE) +
        ggplot2::geom_point(
          position = ggplot2::position_dodge(width = 0.4),
          size = 5) +
        
        ggplot2::labs(
          x = "Population sample size",
          y = "Relative error (%)") +
        
        ggplot2::scale_fill_manual(
          name = paste0("Within error threshold (\u00B1",
                        error_threshold * 100, "%)?"),
          breaks = c("TRUE", "FALSE"),
          values = pal, drop = FALSE,
          guide = ggplot2::guide_legend(
            override.aes = list(color = pal,
                                fill = pal,
                                size = 3),
            order = 1,
            text.vjust = 4,
            label.vjust = 0.4,
            theme = ggplot2::theme(
              legend.key.width = ggplot2::unit(2, "lines"),
              legend.key.height = ggplot2::unit(0.5, "lines")))) +
        
        ggplot2::scale_shape_manual(
          "Groups:", values = c(21, 24)) +
        ggplot2::scale_x_continuous(
          breaks = scales::breaks_pretty()) +
        ggplot2::scale_y_continuous(
          labels = scales::percent,
          breaks = scales::breaks_pretty()) +
        
        ggplot2::theme_classic() +
        ggplot2::theme(
          text = ggplot2::element_text(size = 15),
          legend.position = "bottom",
          strip.text = ggplot2::element_text(size = 18),
          strip.background.x = ggplot2::element_rect(
            color = NA, fill = NA),
          strip.background.y = ggplot2::element_rect(
            color = NA, fill = NA),
          plot.margin = ggplot2::unit(
            c(1, 1, 1, 1), "cm"))
    }
    
    print(p)
    
  }
  
  cat("\n")
  .header("Final parameters", 4)
  
  if (broke) {
    message(format(
      .msg(paste0("   Minimum population sample size: "),
           "success"), width = 3, justify = "left"),
      m_seq[[i]])
    message(format(
      .msg(paste0("   Sampling duration: "),
           "success"), width = 3, justify = "left"),
      round(obj$dur$value, 1), " ", obj$dur$unit)
    message(format(
      .msg(paste0("   Sampling interval: "), 
           "success"), width = 3, justify = "left"),
      round(obj$dti$value, 1), " ", obj$dti$unit)
    message(sprintf(
      " \u2713 Error below threshold of %.1f%%. %s",
      error_threshold * 100,
      "Minimum sample size achieved!"))
  } else {
    message(format(
      .msg(paste0("   Maximum population sample size evaluated: "),
           "danger"), width = 3, justify = "left"),
      m_seq[[i]])
    message(format(
      .msg(paste0("   Sampling duration: "),
           "danger"), width = 3, justify = "left"),
      round(obj$dur$value, 1), " ", obj$dur$unit)
    message(format(
      .msg(paste0("   Sampling interval: "), 
           "danger"), width = 3, justify = "left"),
      round(obj$dti$value, 1), " ", obj$dti$unit)
    message(sprintf(
      " \u2717 Error above threshold of %.1f%%. %s",
      error_threshold * 100,
      "More individuals needed!"))
    message(paste(
      "   Increase", .msg("maximum population sample size", "danger"),
      "and try again."))
  }
  
  # message("Total elapsed time:")
  # print(difftime(Sys.time(), start_total))
  
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
  
  if (length(tmpList) > 0) {
    
    if (obj$grouped) {
      group_keys <- c("A", "B")
      common_names <- tmpList[[1]]$groups[[1]]
      merged_ids <- lapply(tmpList, function(x) x$groups[[2]])
      merged_ids <- Reduce(
        function(x, y) Map(c, x, y), merged_ids)
      
      for (x in seq_along(tmpList)) {
        tmpList[[x]]$groups <- list(common_names, merged_ids)
      }
    }
    
    merged <- md_merge(tmpList)
    class(merged) <- unique(c("moveoutput", class(merged)))
  } else {
    merged <- NULL
  }
  
  out <- structure(list(
    data = merged,
    summary = out_summary,
    plot = ifelse(plot, p, NULL),
    error_threshold = error_threshold,
    sampling_duration = paste0(
      round(obj$dur$value, 1), " ", obj$dur$unit),
    sampling_interval = paste0(
      round(obj$dti$value, 1), " ", obj$dti$unit),
    sample_size_achieved = broke,
    minimum_population_sample_size = m_seq[[i]]
  ), class = "movedesign")
  class(out) <- unique(c("movedesign_report", class(out)))
  return(out)
  
}

