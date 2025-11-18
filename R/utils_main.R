
#' @title Coerce object to a `movedesign` object
#'
#' @description
#' Internal constructor that validates and wraps a list object as a
#' `movedesign_preprocess` object. The function checks that the input
#' file contains required metadata fields and either output objects.
#' 
#' @keywords internal
#' 
#' @noRd
.as_md <- function(obj, outputs = NULL, ...) {
  
  dots <- list(...)
  
  if (!is.list(obj)) {
    stop("Object must be a list.")
  }
  
  if (is.null(dots[["ignore_mismatch"]])) {
    ignore_mismatch <- FALSE
  } else {
    ignore_mismatch <- dots[["ignore_mismatch"]]
  }
  
  if (!is.null(outputs)) {
    
    n_replicates <- length(obj)
    
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
      
      merged <- md_merge(obj, ignore_mismatch = ignore_mismatch)
      class(merged) <- unique(c("moveoutput", class(merged)))
    } else {
      merged <- NULL
    }
    
    merged$n_replicates <- n_replicates
    
    out <- structure(
      list(data = merged, summary = summary), class = "movedesign")
    class(out) <- unique(c("movedesign_output", class(out)))
    return(out)
  }
  
  metadata_fields <- c(
    "data", "data_type",
    "get_species", "n_individuals",
    "dur", "dti", "add_ind_var",
    "grouped", "groups",
    "set_target", "which_meta", "parallel",
    "sigma", "tau_p", "tau_v", "mu",
    "meanfitList"
  )
  
  missing_fields <- setdiff(metadata_fields, names(obj))
  if (length(missing_fields) > 0) {
    stop("Missing required metadata fields: ",
         paste(missing_fields, collapse = ", "))
  }
  
  if (!("akdeList" %in% names(obj) ||
        "ctsdList" %in% names(obj))) {
    stop("Object must contain either 'akdeList' or 'ctsdList'.")
  }
  
  class(obj) <- unique(c("movedesign_preprocess", class(obj)))
  return(obj)
}


#' @title Construct a `movedesign` S3 object
#' 
#' @description
#' A generic internal constructor for creating S3 objects
#' representing different stages of a "movedesign" workflow, such as
#' input, preprocessing, simulation, output, diagnostics, and plots.
#' 
#' @param x The underlying data or object to be wrapped
#'   (e.g., list, tibble, simulation object, output).
#' @param subclass Character string specifying the subclass
#'   (e.g., `movedesign_input`, `movedesign_output`).
#' @param ... Additional attributes to set on the object.
#' 
#' @return An object of class `subclass` and `movedesign`.
#' 
#' @keywords internal
new_movedesign <- function(x, subclass, ...) {
  structure(x, class = c(subclass, "movedesign"), ...)
}


#' @title Create a `movedesign_input` object for simulation workflows
#'
#' @description
#' Constructs an S3 object of class `movedesign_input`, encapsulating
#' all parameters and metadata required for a simulation-based
#' study design. This object includes elements such as number of
#' individuals (*population* sample size), study duration, sampling
#' interval, movement models, grouping structure (if specified), and
#' estimation targets. The standardized `movedesign_input` object is
#' the expected input for downstream `movedesign` functions.
#'
#' Use [`md_prepare()`] to construct a complete study design input
#' object, which can then be passed to functions like
#' [`md_run()`] and [`md_replicate()`].
#'
#' @param design A named list containing user-specified inputs for
#'   a `movedesign` workflow. At a minimum, this includes elements
#'   such as `data`, `dur`, `dti`, `n_individuals`, and `set_target`.
#'
#' @return
#' An object of class `movedesign_input` and `movedesign`,
#' which contains all input parameters and metadata required
#' for a `movedesign` workflow.
#'
#' @seealso [`md_prepare()`]
#'   
#' @keywords internal
movedesign_input <- function(design) {
  new_movedesign(design, subclass = "movedesign_input")
}


#' @title Create a `movedesign_preprocess` object for preprocessed results
#' 
#' @description
#' Creates an S3 object of class `movedesign_preprocess` to 
#' store preprocessing steps or outputs during a `movedesign` workflow.
#'
#' @param input A list, containing preprocessed results and metadata.
#' 
#' @return
#' An object of class `movedesign_preprocess` and `movedesign`.
#' 
#' @seealso [`md_run()`]
#'   
#' @keywords internal
movedesign_preprocess <- function(input) {
  new_movedesign(input, subclass = "movedesign_preprocess")
}


#' @title Create a `movedesign_output` object to store simulation outputs
#'
#' @description
#' Constructs an S3 object of class `movedesign_output` that stores the
#' outputs and summaries from a specific simulation workflow. The
#' resulting object bundles all relevant metadata from the original
#' study design (inputs), simulation outputs (e.g., home range or
#' speed estimates), and post-processing summaries (e.g., errors,
#' credible intervals).
#'
#' The `movedesign_output` object is returned by simulation functions like
#' [`md_run()`] or [`md_replicate()`], and acts as the primary data
#' structure for downstream analyses and visualization via
#' functions such as [`md_plot()`] or [`md_check()`].
#'
#' @return
#' An S3 object of class `movedesign_output` containing simulation
#' outputs, summaries, and associated metadata.
#'
#' @seealso
#'   [`md_run()`],
#'   [`md_replicate()`],
#'   [`md_check()`],
#'   [`md_plot()`]
#' 
#' @keywords internal
#' @export
movedesign_output <- function(input) {
  new_movedesign(input, subclass = "movedesign_preprocess")
}


#' Summary method for `movedesign_input`
#'
#' @param object An object of class `movedesign_input`
#' @param ... Additional arguments
#' @export
summary.movedesign_input <- function(object, ...) {
  
  .header("Data", 5)
  message(format(
    .msg(paste0("   Species: "), "main"),
    width = 3, justify = "left"),
   object$get_species %||% "Not specified")
  
  if (object$which_m == "set_m") {
    message(format(
      .msg(paste0("   Number of individuals used: "), "main"),
      width = 3, justify = "left"),
      length(object$data)) }
  message(format(
    .msg(paste0("   Grouped: "), "main"),
    width = 3, justify = "left"),
    ifelse(object$grouped, "TRUE", "FALSE"))
  if (object$which_m == "set_m") {
  message(format(
    .msg(paste0("   Effective sample size (area): "), "main"),
    width = 3, justify = "left"),
    round(mean(unlist(extract_dof(object$fitList, "area")),
               na.rm = TRUE), 1)) }
  if (object$which_m == "set_m" || 
      "ctsd" %in% object$set_target) {
  message(format(
    .msg(paste0("   Effective sample size (speed): "), "main"),
    width = 3, justify = "left"),
    round(mean(unlist(extract_dof(object$fitList, "speed")),
               na.rm = TRUE), 1)) }

  .header("Species parameters", 4)
  message(format(
    .msg(paste0("   Position autocorrelation timescale: "), "main"),
    width = 3, justify = "left"),
    round(object$tau_p[["All"]]$value[2], 1), " ",
    object$tau_p[["All"]]$unit[2])
  if ("ctsd" %in% object$set_target) {
  message(format(
    .msg(paste0("   Velocity autocorrelation timescale: "), "main"),
    width = 3, justify = "left"),
    round(object$tau_v[["All"]]$value[2], 1), " ",
    object$tau_v[["All"]]$unit[2]) }
  message(format(
    .msg(paste0("   Location variance: "), "main"),
    width = 3, justify = "left"),
    round(object$sigma[["All"]]$value[2], 1), " ",
    object$sigma[["All"]]$unit[2])
  
  .header("Study design parameters", 4)
  message(format(
    .msg(paste0("   Number of individuals requested: "), "main"),
    width = 3, justify = "left"), object$n_individuals)
  if (object$which_m == "set_m") {
    # message(format(
    #   .msg(paste0("   Population sample size requested: "), "main"),
    #   width = 3, justify = "left"),
    #   object$n_individuals) 
    message(format(
      .msg(paste0("   Sampling duration requested: "), "main"),
      width = 3, justify = "left"),
      round(object$dur$value, 1), " ", object$dur$unit)
    message(format(
      .msg(paste0("   Sampling interval requested: "), "main"),
      width = 3, justify = "left"),
      round(object$dti$value, 1), " ", object$dti$unit)
  }
  message(format(
    .msg(paste0("   Research target(s) requested: "), "main"),
    width = 3, justify = "left"),
    paste(object$set_target, collapse = ", "))
  message(format(
    .msg(paste0("   Analytical target(s) requested: "), "main"),
    width = 3, justify = "left"),
    paste(object$set_target, collapse = ", "))
  
  .header("Workflow requested", 4)
  
  if (object$which_m == "set_m") {
    message(.msg(
      paste0("   Verifying study design for a specific ",
             .msg("population sample size", "success"), "."),
      "main"))
  }
  if (object$which_m == "get_m") {
    message(.msg(
      paste0("   Find the minimum ",
             .msg("population sample size", "success"),
             " required for a target."),
      "main"))
  }
  
  if (object$which_meta == "mean") {
    message(.msg(
      paste0("   Mean estimate of ",
             .msg("sampled population", "success"), "."),
      "main"))
  }
  if (object$which_meta == "compare") {
    message(.msg(
      paste0("   Comparing estimates of ",
             .msg("two", "success"), " sampled groups."),
      "main"))
  }
  
  invisible(object)
}


#' @title Print method for `movedesign_input`
#'
#' @param x An object of class `movedesign_input`
#' @param ... Additional arguments
#' @export
print.movedesign_input <- function(x, ...) {
  summary(x)
}


#' Summary method for `movedesign_preprocess`
#'
#' @param object An object of class `movedesign_preprocess`
#' @param ... Additional arguments
#' @export
summary.movedesign_preprocess <- function(object, ...) {
  
  .header("Simulation parameters", 5)
  message(format(
    .msg("   Number of individuals available: ", "main"),
    width = 3, justify = "left"), object$n_individuals)
  message(format(
      .msg(paste0("   Effective sample size (area): "), "main"),
      width = 3, justify = "left"),
      round(mean(unlist(extract_dof(object$simfitList, "area")),
                 na.rm = TRUE), 1))
  if ("ctsd" %in% object$set_target) {
    message(format(
      .msg(paste0("   Effective sample size (speed): "), "main"),
      width = 3, justify = "left"),
      round(mean(unlist(extract_dof(object$simfitList, "speed")),
                 na.rm = TRUE), 1)) }
  
  .header("Notes", 4)
  message(format(
    .msg("   Number of replicates: ", "main"),
    width = 3, justify = "left"), 1)
  message(paste(
    "   This object contains outputs from a single replicate.\n",
    "  Use `md_plot_preview()` to inspect individual performance",
    "and convergence.\n    To assess uncertainty, run multiple",
    "replicates with `md_replicate()`."))
  
  invisible(object)
}


#' @title Print method for `movedesign_preprocess`
#'
#' @param x An object of class `movedesign_preprocess`
#' @param ... Additional arguments
#' @export
print.movedesign_preprocess <- function(x, ...) {
  summary(x)
}


#' Summary method for `movedesign_output`
#'
#' @param object An object of class `movedesign_output`
#' @param ... Additional arguments
#' @export
summary.movedesign_output <- function(object, ...) {
  
  .is_available <- function(x) {
    is.list(x) && any(!vapply(x, is.null, logical(1)))
  }
  
  .format_txt <- function(x) {
    if (length(x) > 0 && .is_available(x)) 
      crayon::green("Yes") else crayon::red("No")
  }
  
  as.movedesign <- function(x) {
    known_design <- c(
      "data",
      "species",
      "data_type",
      "n_individuals",
      "dur", "dti", 
      "add_ind_var", 
      "grouped",
      "set_target",
      "which_meta", 
      "parallel",
      "fitList",
      "meanfitList",
      "sigma", "tau_p", "tau_v", "mu"
    )
    design_slots <- x[names(x) %in% known_design]
    structure(design_slots, class = "movedesign")
  }
  summary(as.movedesign(object))
  
  sim_counts <- object$summary %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarise(n_sims = dplyr::n()) %>%
    dplyr::ungroup()
  same_n <- length(unique(sim_counts$n_sims)) == 1
  
  # if (!same_n) {
  #   warning("Replicates have different numbers of simulations!")
  # }
  
  .header("Simulation details", 5)
  message(format(
    .msg(paste0("   Total number of replicates: "), "main"),
    width = 3, justify = "left"),
    max(object$summary$replicate, na.rm = TRUE))
  message(format(
    .msg(paste0("   Total number of simulations: "), "main"),
    width = 3, justify = "left"),
    length(object$data$simList))
  message(format(
    .msg(paste0("   Simulations per replicate: "), "main"),
    width = 3, justify = "left"),
    object$summary %>%
      dplyr::group_by(.data$replicate) %>%
      dplyr::pull(.data$m) %>%
      max(na.rm = TRUE))
  
  if (!is.null(object$data$grouped))
    if (object$data$grouped) {
      message(format(
        .msg(paste0("   Simulations per replicate per group: "), "main"),
        width = 3, justify = "left"),
        length(object$data$simList)/2/
          object$data$n_replicates) }
  if ("hr" %in% object$data$set_target) {
    message(format(
      .msg(paste0("   Home range outputs available? "), "main"),
      width = 3, justify = "left"),
      .format_txt(object$data$akdeList)) }
  if ("ctsd" %in% object$data$set_target) {
    message(format(
      .msg(paste0("   Movement speed outputs available? "), "main"),
      width = 3, justify = "left"),
      .format_txt(object$data$ctsdList)) }
  
  invisible(object)
}


#' @title Print method for `movedesign_output`
#' 
#' @param x An object of class `movedesign_output`
#' @param ... Additional arguments
#' @export
print.movedesign_output <- function(x, ...) {
  summary(x)
}


#' Summary method for `movedesign_check` objects
#' @param object An object of class `movedesign_check`
#' @param ... Unused
#' @export
summary.movedesign_check <- function(object, ...) {
  
  type <- NULL
  
  diag <- object$diag
  has_groups <- object$grouped
  tol <- object$tolerance
  
  for (i in seq_len(nrow(diag))) {
    
    if (has_groups) {
      tg <- paste0(
        ifelse(diag$type[i] == "hr", "home range estimation",
               ifelse(diag$type[i] == "ctsd", "speed estimation",
                      diag$type[i])),
        " - Group ", 
        as.character(diag$group[i]))
    } else {
      tg <- paste0(
        ifelse(diag$type[i] == "hr", "home range estimation",
               ifelse(diag$type[i] == "ctsd", "speed estimation",
                      diag$type[i])))
    }
    
    type_i <- diag$type[i]
    mean_error_i <- diag$last_cummean[i]
    recent_cummean_i <- diag$recent_cummean[[i]]
    has_converged_i <- diag$has_converged[i]
    
    n_eval <- length(recent_cummean_i)
    type_i <- diag$type[i]
    stabilized_at_i <- object$stabilized_at %>%
      dplyr::filter(type == type_i) %>%
      dplyr::pull(.data$idx_stable_start)
    
    if (has_groups) {
      if (i == 1 && type_i == "hr") {
        .header("Home range estimation", 5) }
      if (i == 3 && type_i == "ctsd") {
        .header("Speed \u0026 distance estimation", 5) }
    } else {
      if (i == 1 && type_i == "hr") {
        .header("Home range estimation", 5) }
      if (i == 2 && type_i == "ctsd") {
        .header("Speed \u0026 distance estimation", 5) }
    }
    
    message(sprintf(
      "Convergence check for %s", tg))
    if (abs(mean_error_i) < tol) {
      message(
        "   Mean estimate error: ",
        .msg(paste0(.err_to_txt(mean_error_i), "%"), "success"))
    } else {
      message(
        "   Mean estimate error: ",
        .msg(paste0(.err_to_txt(mean_error_i), "%"), "danger"))
    }
    message(sprintf(
      "   Tolerance: %g%%, Steps evaluated: %d", tol * 100, n_eval))
    
    if (has_converged_i) {
      message(sprintf(
        "\u2713 Converged: all %d steps within \u00B1 %g%%.", 
        n_eval, tol * 100))
      message(sprintf(
        "    Stabilized at replicate %d.", stabilized_at_i))
    } else {
      message(
        .msg("\u2717 Did not converge: ", "danger"),
        "at least one step exceeded tolerance.")
    }
    
    if (!is.null(object$warning)) {
      warning(sub("^Warning:\\s*", "", object$warning))
    }
    
    # message("")
  }
  
  invisible(object)
}


#' Print method for `movedesign_check` objects
#' @param x An object of class `movedesign_check`
#' @param ... Unused
#' @export
print.movedesign_check <- function(x, ...) {
  summary(x)
}


#' Summary method for `movedesign_report` objects
#' @param object An object of class `movedesign_report`
#' @param ... Unused
#' @export
summary.movedesign_report <- function(object, ...) {
  
  type <- NULL
  has_groups <- object$data$grouped
  set_target <- object$data$set_target
  
  target <- "hr"
  for (target in set_target) {
    
    if (target == "hr") {
      .header("Home range estimation", 5) }
    if (target == "ctsd") {
      .header("Speed \u0026 distance estimation", 5) }
    
    if (object$sample_size_achieved) {
      message(format(
        .msg(paste0("   Minimum population sample size: "),
             "success"), width = 3, justify = "left"),
        object$minimum_population_sample_size)
      message(format(
        .msg(paste0("   Sampling duration: "),
             "success"), width = 3, justify = "left"),
        object$sampling_duration)
      message(format(
        .msg(paste0("   Sampling interval: "), 
             "success"), width = 3, justify = "left"),
        object$sampling_interval)
      message(sprintf(
        " \u2713 Error below threshold of %.1f%%. %s",
        object$error_threshold * 100,
        "Minimum sample size achieved!"))
    } else {
      message(format(
        .msg(paste0("   Maximum population sample size evaluated: "),
             "danger"), width = 3, justify = "left"),
        object$minimum_population_sample_size)
      message(format(
        .msg(paste0("   Sampling duration: "),
             "danger"), width = 3, justify = "left"),
        object$sampling_duration)
      message(format(
        .msg(paste0("   Sampling interval: "), 
             "danger"), width = 3, justify = "left"),
        object$sampling_interval)
      message(sprintf(
        " \u2717 Error above threshold of %.1f%%. %s",
        object$error_threshold * 100,
        "More individuals needed!"))
      message(paste(
        "   Increase", .msg("maximum population sample size", "danger"),
        "and try again."))
    }
    
  } # end of [target] loop
  
  invisible(object)
}


#' Print method for `movedesign_report` objects
#' @param x An object of class `movedesign_report`
#' @param ... Unused
#' @export
print.movedesign_report <- function(x, ...) {
  invisible(x)
}


# Internal utility for styled message header.
#' @noRd
.header <- function(title, n_dash = 5) {
  
  emdash <- "\u2500"
  header_line <- paste0(
    strrep(emdash, n_dash), " ", title,
    ":") # " ", strrep(emdash, n_dash), "\n")
  cat(crayon::bold(header_line))
  
}

# Internal utility for styled message output.
#' @noRd
.msg <- function(txt, type = "main") {
  
  switch(type,
         main = msg_main(txt),
         success = msg_success(txt),
         danger = msg_danger(txt),
         warning = msg_warning(txt),
         txt)
}
