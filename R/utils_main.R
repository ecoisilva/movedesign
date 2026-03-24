
#' @title Internal utility for styled message header
#' @noRd
.header <- function(title, n_dash = 5) {
  emdash <- "\u2500"
  header_line <- paste0(
    strrep(emdash, n_dash), " ", title, ":")
  message(crayon::bold(header_line))
  
}


#' @title Internal utility for styled message output
#' @noRd
.msg <- function(txt, type = "main") {
  
  switch(type,
         main = msg_main(txt),
         success = msg_success(txt),
         danger = msg_danger(txt),
         warning = msg_warning(txt),
         txt)
}

#' @title Function to calculate elapsed time in hh:mm:ss
#' @noRd
.msg_time <- function(start, text) {
  
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  
  elapsed_hms <- sprintf("%02d:%02d:%02d (hh:mm:ss)",
                 elapsed %/% 3600,
                 (elapsed %% 3600) %/% 60,
                 round(elapsed %% 60))
  
  return(message(text,
                 crayon::yellow(crayon::bold(elapsed_hms))))
  
}

#' @title Coerce object to a `movedesign` object
#'
#' @description
#' Internal constructor that validates and wraps a list object as a
#' `movedesign_processed` object. The function checks that the input
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
  
  # For input object:
  
  missing_fields <- setdiff(c("datList",
                              "species",
                              "n_sims"), names(obj))
  if (length(missing_fields) > 0) {
    stop("The provided file is not a valid `movedesign` output.",
         call. = FALSE)
  }
  
  names(obj)[names(obj) == "datList"] <- "data"
  names(obj)[names(obj) == "species"] <- "get_species"
  names(obj)[names(obj) == "n_sims"] <- "n_individuals"
  
  if (is.null(obj$version)) obj$seedInit <- obj$seedList[[1]]
  
  metadata_fields <- c(
    "data", "data_type",
    "get_species", "n_individuals",
    "dur", "dti", "add_ind_var",
    "grouped", "groups",
    "set_target", "which_meta", "which_m", "parallel",
    "fitList", "meanfitList",
    "sigma", "tau_p", "tau_v", "mu",
    "seedInit")
  
  missing_fields <- setdiff(metadata_fields, names(obj))
  if (length(missing_fields) > 0) {
    stop("Missing required metadata fields: ",
         paste(missing_fields, collapse = ", "))
  }
  
  if (!("akdeList" %in% names(obj) ||
        "ctsdList" %in% names(obj))) {
    stop("Object must contain either 'akdeList' or 'ctsdList'.")
  }
  
  use_global_parameters <- is.list(obj$dur) &&
    all(c("value", "unit") %in% names(obj$dur)) &&
    !any(sapply(obj$dur, is.list))
  
  design_input <- movedesign_input(list(
    data = obj$data,
    data_type = obj$data_type,
    get_species = obj$get_species,
    n_individuals = as.numeric(obj$n_individuals),
    dur = obj$dur,
    dti = obj$dti,
    use_global_parameters = use_global_parameters,
    add_ind_var = obj$add_ind_var,
    grouped = obj$grouped,
    groups = if (obj$grouped) obj$groups else NULL,
    set_target = obj$set_target,
    which_meta = obj$which_meta,
    which_m = obj$which_m,
    parallel = obj$parallel,
    fitList = obj$fitList,
    meanfitList = obj$meanfitList,
    sigma = obj$sigma,
    tau_p = obj$tau_p,
    tau_v = obj$tau_v,
    mu = obj$mu,
    seed = obj$seedInit))
  
  # For processed object:
  
  design_processed <- movedesign_processed(list(
    data = design_input$data,
    get_species = design_input$get_species,
    data_type = design_input$data_type,
    n_individuals = design_input$n_individuals,
    dur = design_input$dur,
    dti = design_input$dti,
    use_global_parameters = design_input$use_global_parameters,
    add_ind_var = design_input$add_ind_var,
    grouped = design_input$grouped,
    groups = design_input$groups,
    set_target = design_input$set_target,
    which_meta = design_input$which_meta,
    parallel = design_input$parallel,
    fitList = design_input$fitList,
    meanfitList = design_input$meanfitList,
    sigma = design_input$sigma,
    tau_p = design_input$tau_p,
    tau_v = design_input$tau_v,
    mu = design_input$mu,
    simList = design_input$simList,
    seedInit = design_input$seed,
    seedList = obj$seedList,
    simfitList = obj$simfitList,
    akdeList = obj$akdeList,
    ctsdList = obj$ctsdList))
  
  if (!is.null(obj$merged)) {
    
    # For output object:
    design_output <- structure(
      list(input = design_processed,
           data = obj$merged,
           summary = obj$meta_tbl_replicates,
           error_threshold = obj$error_threshold,
           verbose = TRUE), class = "movedesign")
    class(design_output) <- unique(c("movedesign_output",
                                     class(design_output)))
    
    outList <- list(design_input = design_input,
                    design_processed = design_processed,
                    design_output = design_output)
  } else {
    
    outList <- list(design_input = design_input,
                    design_processed = design_processed)
  }
  
  return(invisible(outList))
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


#' @title Create a `movedesign_processed` object for preprocessed results
#' 
#' @description
#' Creates an S3 object of class `movedesign_processed` to 
#' store preprocessing steps or outputs during a `movedesign` workflow.
#'
#' @param input A list, containing preprocessed results and metadata.
#' 
#' @return
#' An object of class `movedesign_processed` and `movedesign`.
#' 
#' @seealso [`md_run()`]
#'   
#' @keywords internal
movedesign_processed <- function(input) {
  new_movedesign(input, subclass = "movedesign_processed")
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
  new_movedesign(input, subclass = "movedesign_processed")
}


#' Summary method for `movedesign_input`
#' 
#' @param object An object of class `movedesign_input`
#' @param ... Additional arguments
#' 
#' @importFrom ctmm %#%
#' 
#' @export
summary.movedesign_input <- function(object, ...) {
  
  .format_lgc <- function(x) {
    if (x) {
      crayon::yellow("\u2713 Yes")
    } else { crayon::red("\u2718 No") }
  }
  
  .line <- function(label, value, width = 40L) {
    
    pad <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    
    return(message("     ", crayon::silver(label),
                   crayon::silver(dots), " ", value))
  }
  
  .section <- function(title) {
    
    rule <- paste(rep("\u2500", 60L), collapse = "" )
    message("")
    message("     ",crayon::bold(title))
    message("     ", rule)
    
  }
  
  data_src <- if (
    identical(object$data_type, "simulated")
  ) "simulated" else "empirical"
  
  target_map <- c(hr = "Home range",
                  ctsd = "Speed \u0026 distance")
  
  meta_label <- switch(
    object$which_meta %||% "mean",
    mean = "Population mean",
    compare = "Group comparison",
    "Individual")
  
  species <- object$get_species %||% "Unknown"
  
  .header("Input summary", 5)
  
  if (object$data_type != "simulated") {
    
    .section("Data:")
    message(crayon::silver("     Species: "),
            crayon::bold(species),
            crayon::yellow(paste0(" [", data_src, "]")))
    
    if (object$which_m == "set_m") {
      .line("No. of individual used",
            length(object$data))
    }
    
    if (object$grouped) {
      .line("Groups: ", paste0(
        crayon::yellow("\u2713 Yes"), "\n",
        "             ",
        crayon::yellow(names(object$groups[[2]])[[1]]), " = ",
        paste0('"', object$groups[[1]][[1]], '"', collapse = ", "), "\n",
        "             ",
        crayon::yellow(names(object$groups[[2]])[[2]]), " = ",
        paste0('"', object$groups[[1]][[2]], '"', collapse = ", "), "\n"))
    }
    
    if (object$which_m == "set_m") {
      
      .line("Effective sample size (area)",
            round(mean(unlist(extract_dof(object$fitList, "area")),
                       na.rm = TRUE), 1))
    }
    
    if ((object$which_m == "set_m" || 
         "ctsd" %in% object$set_target)) {
      
      .line("Effective sample size (speed)",
            round(mean(unlist(extract_dof(object$fitList, "speed")),
                       na.rm = TRUE), 1))
    }
  }
  
  .section("Species parameters:")
  
  .line("Position autocorrelation timescale",
        paste(round(object$tau_p[["All"]]$value[2], 1),
              fix_unit(object$tau_p[["All"]][2, ])$unit))
  
  if ("ctsd" %in% object$set_target ||
      object$data_type == "simulated") {
    
    .line("Velocity autocorrelation timescale",
          paste(round(object$tau_v[["All"]]$value[2], 1),
                fix_unit(object$tau_v[["All"]][2, ])$unit))
  }
  
  sig <- fix_unit(
    object$sigma[["All"]]$value[2],
    unit = object$sigma[["All"]]$unit[2],
    convert = TRUE, ui = TRUE)
  
  .line("Location variance", paste(sig$value, sig$unit))
  
  cat("\n")
  .header("Workflow requested", 5)
  
  .section("Study design parameters:")
  
  .line("No. of individuals requested", object$n_individuals)

  if (!is.null(object$dur) && !is.null(object$dti)) {
    
    .line("Sampling duration requested",
          paste(object$dur$value, object$dur$unit))
    .line("Sampling interval requested",
          paste(object$dti$value, object$dti$unit))
    
    dur0 <- round(object$dur$value %#% object$dur$unit, 0)
    dti0 <- round(object$dti$value %#% object$dti$unit, 0)
    t0 <- seq(0, dur0, by = dti0)[-1]
    
    .line("Expected absolute sample size", length(t0))
    
  } else {
    
    message("     ",
            "Optimizing both ",
            crayon::yellow("sampling duration"), " and ",
            crayon::yellow("interval"), ".\n")
    
  }
  
  .line("Estimation target",
        paste(target_map[object$set_target], collapse = ", "))
  .line("Inference target", meta_label)
  .line("Individual variation",
        .format_lgc(object$add_ind_var))
  
  message(" ")
  if (object$which_m == "set_m") {
    message(crayon::silver(
      paste0("     Assessing study design for a specific ",
             .msg("population sample size", "success"), ",")))
  }
  if (object$which_m == "get_m") {
    message(crayon::silver(
      paste0("    Find the minimum ",
             .msg("population sample size", "success"),
             " required for a target,")))
  }
  if (object$which_m == "get_all") {
    message(crayon::silver(
      paste0("     Find the recommended ",
             .msg("sampling parameters", "success"),
             " required for a target,")))
  }
  
  if (object$which_meta == "mean") {
    message(crayon::silver(
      paste0("     Estimating the population mean for a ",
             .msg("sampled population", "success"), ".")))
  }
  if (object$which_meta == "compare") {
    message(crayon::silver(
      paste0("     Comparing estimates of ",
             .msg("two", "success"), " sampled groups.")))
  }
  
  invisible(object)
}


#' @title Print method for `movedesign_input`
#'
#' @param x An object of class `movedesign_input`
#' @param ... Additional arguments
#' @export
print.movedesign_input <- function(x, ...) {
  
  .line <- function(label, value, width = 40L) {
    pad <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    paste0("     ", label, dots, " ", value)
  }
  
  .section <- function(title) {
    rule <- paste(rep("\u2500", 60L), collapse = "")
    c("", paste0("     ", title), paste0("     ", rule))
  }
  
  source_map <- if (
    identical(x$data_type, "simulated")
  ) "simulated" else "empirical"
  target_map <- c(hr = "Home range", ctsd = "Speed \u0026 distance")
  meta_map <- switch(x$which_meta %||% "mean",
                     mean = "Population mean",
                     compare = "Group comparison",
                     "Individual")
  
  species <- x$get_species %||% "Unknown"
  
  if (!is.null(x$dur) && !is.null(x$dti)) {
    dur0 <- round(x$dur$value %#% x$dur$unit, 0)
    dti0 <- round(x$dti$value %#% x$dti$unit, 0)
    t0 <- seq(0, dur0, by = dti0)[-1]
  }
  
  sig <- fix_unit(
    x$sigma[["All"]]$value[2],
    unit = x$sigma[["All"]]$unit[2],
    convert = TRUE,
    ui = TRUE)
  
  lines <- c(
    "", "\u2500\u2500\u2500\u2500 Input summary:")
  
  if (x$data_type != "simulated") {
    
    lines <- c(
      lines,
      .section("Data:"),
      .line("Species", paste0(species, " [", source_map, "]")),
      .line("No. of individual used", length(x$data)))
    
    if (x$grouped) {
      group_info <- paste0(
        "\u2713 Yes\n",
        "                       ", names(x$groups[[2]])[[1]], " = ",
        paste0('"', x$groups[[1]][[1]], '"', collapse = ", "), "\n",
        "                       ", names(x$groups[[2]])[[2]], " = ",
        paste0('"', x$groups[[1]][[2]], '"', collapse = ", "), "\n")
      
      lines <- c(lines,
                 .line("Groups: ", group_info))
    }
    
    if (x$which_m == "set_m") {
      
      lines <- c(
        lines,
        .line("Effective sample size (area)",
              round(mean(unlist(extract_dof(x$fitList, "area")),
                         na.rm = TRUE), 1)))
    }
    
    if ((x$which_m == "set_m" || 
         "ctsd" %in% x$set_target)) {
      
      lines <- c(
        lines,
        .line("Effective sample size (speed)",
              round(mean(unlist(extract_dof(x$fitList, "speed")),
                         na.rm = TRUE), 1)))
    }
  }
  
  lines <- c(
    lines,
    .section("Species parameters:"),
    .line("Position autocorrelation timescale",
          paste(round(x$tau_p[["All"]]$value[2], 1),
                fix_unit(x$tau_p[["All"]][2, ])$unit)))
  
  if ("ctsd" %in% x$set_target || x$data_type == "simulated") {
    
    lines <- c(
      lines,
      .line("Velocity autocorrelation timescale",
            paste(round(x$tau_v[["All"]]$value[2], 1),
                  fix_unit(x$tau_v[["All"]][2, ])$unit)))
  }
  
  if (!is.null(x$dur) && !is.null(x$dti)) {
    
    sampling_parameters <- c(
      .line("Sampling duration requested",
            paste(x$dur$value, x$dur$unit)),
      .line("Sampling interval requested",
            paste(x$dti$value, x$dti$unit)))
    
  } else {
    
    sampling_parameters <- c(paste0(
      "     ", "Optimizing both sampling duration and interval."),
      "")
  }
  
  lines <- c(
    lines,
    .line("Location variance", paste(sig$value, sig$unit)),
    
    "", "\u2500\u2500\u2500\u2500 Workflow requested:",
    .section("Study design parameters:"),
    .line("No. of individuals requested",  x$n_individuals),
    sampling_parameters,
    if (!is.null(x$dur) && !is.null(x$dti)) {
      .line("Expected absolute sample size", length(t0))
    } else NULL,
    .line("Estimation target",
          paste(target_map[x$set_target], collapse = ", ")),
    .line("Inference target",              meta_map),
    .line("Individual variation",
          if (x$add_ind_var) "Yes" else "No"),
    "")
  
  if (x$which_m == "set_m") {
    lines <- c(
      lines,
      paste0("     Assessing study design for a specific ",
             "population sample size,"))
  }
  if (x$which_m == "get_m") {
    lines <- c(
      lines,
      paste0("    Find the minimum population sample size",
             " required for a target,"))
  }
  if (x$which_m == "get_all") {
    lines <- c(
      lines,
      paste0("     Find the recommended sampling parameters",
             " required for a target,"))
  }
  
  if (x$which_meta == "mean") {
    lines <- c(
      lines,
      paste0("     Estimating the population mean for a ",
             "sampled population."))
  }
  if (x$which_meta == "compare") {
    lines <- c(
      lines, 
      "     Comparing estimates of sampled groups.")
  }
  
  lines <- c(
    lines, "")
  
  cat(paste(lines, collapse = "\n"), "\n")
  
  return(invisible(x))
}


#' Summary method for `movedesign_processed`
#'
#' @param object An object of class `movedesign_processed`
#' @param ... Additional arguments
#' @export
summary.movedesign_processed <- function(object, ...) {
  
  .line <- function(label, value, width = 40L) {
    
    pad <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    
    return(message("     ", crayon::silver(label),
                   crayon::silver(dots), " ", value))
  }
  
  .section <- function(title) {
    
    rule <- paste(rep("\u2500", 60L), collapse = "" )
    message("")
    message("     ",crayon::bold(title))
    message("     ", rule)
    
  }
  
  .header("Workflow summary", 5)
  .line("No. of individuals",
        as.character(object$n_individuals))
  
  dur0 <- round(object$dur$value %#% object$dur$unit, 0)
  dti0 <- round(object$dti$value %#% object$dti$unit, 0)
  t0 <- seq(0, dur0, by = dti0)[-1]
  
  .line("Mean absolute sample size", length(t0))
  .line("Mean effective sample size (area)",
        round(mean(unlist(extract_dof(object$fitList, "area")),
                   na.rm = TRUE), 1))
  
  if ("ctsd" %in% object$set_target) {
    
    .line("Mean effective sample size (speed)",
          round(mean(unlist(extract_dof(object$fitList, "speed")),
                     na.rm = TRUE), 1))
  }
  
  .header("Notes", 5)
  .line("No. of replicates", .msg(1, "danger"))
  
  message(paste0(
    "\n",
    "     This object contains preliminary outputs ",
    "from a single replicate.\n     Use ",
    .msg("`md_plot_preview()`", "success"),
    " to examine its performance.\n     ",
    crayon::bold(paste0("For more robust inferences, ",
                 "run more replicates using ",
                 .msg("`md_replicate()", "success"), "."))))
  
  invisible(object)
}


#' @title Print method for `movedesign_processed`
#'
#' @param x An object of class `movedesign_processed`
#' @param ... Additional arguments
#' @export
print.movedesign_processed <-function(x, ...) {
  
  .line <- function(label, value, width = 40L) {
    pad  <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    paste0("     ", label, dots, " ", value)
  }
  
  .section <- function(title) {
    rule <- paste(rep("\u2500", 60L), collapse = "")
    c("", paste0("     ", title), paste0("     ", rule))
  }
  
  dur0 <- round(x$dur$value %#% x$dur$unit, 0)
  dti0 <- round(x$dti$value %#% x$dti$unit, 0)
  t0 <- seq(0, dur0, by = dti0)[-1]
  
  N_area <- round(
    mean(unlist(extract_dof(x$fitList, "area")), na.rm = TRUE),
    1)
  
  lines <- c(
    "", "\u2500\u2500\u2500\u2500 Workflow summary:",
    "",
    .line("No. of individuals", x$n_individuals),
    .line("Mean absolute sample size", length(t0)),
    .line("Mean effective sample size (area)", N_area)
  )
  
  if ("ctsd" %in% x$set_target) {
    
    N_speed <- round(
      mean(unlist(extract_dof(x$fitList, "speed")), na.rm = TRUE),
      1)
    
    lines <- c(
      lines,
      .line("Mean effective sample size (speed)", N_speed))
  }
  
  lines <- c(
    lines,
    
    "",
    "", "\u2500\u2500\u2500\u2500 Notes:",
    "",
    .line("No. of replicates", 1),
    "",
    paste0(
      "     This object contains preliminary outputs from a",
      " single replicate."),
    "     Use `md_plot_preview()` to examine its performance.",
    paste0(
      "     For more robust inferences, run more replicates",
      " using `md_replicate()`."),
    "")
  
  cat(paste(lines, collapse = "\n"), "\n")
  
  invisible(x)
}


#' @title Summarise a study design output
#'
#' @description
#' Print a structured summary of a `movedesign_output` object
#' produced by [md_replicate()] or [md_stack()]. The summary
#' reports the study design, replication settings, estimation
#' performance for each target metric, and a convergence
#' assessment.
#'
#' This method runs automatically when calling
#' `summary(output)` on a `movedesign_output` object.
#'
#' @param object A `movedesign_output` object returned by
#'   [md_replicate()] or [md_stack()].
#'
#' @param verbose Logical. If `TRUE`, run [md_check()] and
#'   print the full convergence diagnostics. This can also
#'   display a convergence plot when `plot = TRUE`.
#'   If `FALSE` (default), only the convergence status is
#'   printed.
#'   
#' @param m Numeric (Optional). If provided, restricts the results to
#'   a specific population sample size (`m`). Defaults to `NULL`, which
#'   checks up to the maximum population sample size.
#'   
#' @param ci Confidence level for the intervals. Applied to both
#'   the narrow confidence bars and wide prediction bands. Must be
#'   between `0` and `1`. Default: `0.95` (95%).
#'   
#' @param tol Numeric. The tolerance threshold for absolute change in
#'   the cumulative mean to declare convergence. Defaults to
#'   `0.05`.
#'   
#' @param n_converge Integer. Number of consecutive steps within
#'   tolerance required to confirm convergence.
#'   
#' @param plot Logical. If `TRUE` (default), generates a plot of
#'   stepwise changes in the cumulative mean, highlighting when
#'   convergence is achieved.
#'   
#' @param pal Character vector of color(s) of the plot, such as
#'   `c("#007d80", "#A12C3B")`) (default).
#'   
#' @param ... Additional arguments
#'   
#' @examples
#' if(interactive()) {
#' 
#' data(buffalo)
#' 
#' input <- md_prepare(
#'   species = "African buffalo",
#'   data = buffalo,
#'   n_individuals = 5,
#'   dur = list(value = 1, unit = "month"),
#'   dti = list(value = 1, unit = "day"),
#'   set_target = "hr",
#'   which_meta = "mean")
#' 
#' output <- md_replicate(input, n_replicates = 20)
#' 
#' # Print standard summary:
#' summary(output)
#' 
#' # Run full convergence diagnostics:
#' summary(output, verbose = TRUE, tol = 0.05)
#' 
#' }
#' 
#' @seealso
#' [md_replicate()], [md_stack()] to generate results.
#' [md_check()] to inspect convergence directly.
#' [md_compare()] to compare designs after convergence.
#'
#' @method summary movedesign_output
#' @export
summary.movedesign_output <- function(object,
                                      verbose = FALSE,
                                      m = NULL,
                                      ci = 0.95,
                                      tol = 0.05,
                                      n_converge = 9,
                                      plot = TRUE,
                                      pal = c("#007d80",
                                              "#A12C3B"),
                                      ...) {
  
  .has_results <- function(x) {
    is.list(x) && any(!vapply(x, is.null, logical(1)))
  }
  
  .format_txt <- function(x) {
    if (length(x) > 0 && .has_results(x)) {
      crayon::yellow("\u2713 Yes")
    } else { crayon::red("\u2718 No") }
  }
  
  .format_lgc <- function(x) {
    if (x) {
      crayon::yellow("\u2713 Yes")
    } else { crayon::red("\u2718 No") }
  }
  
  .format_pct <- function(x) {
    paste0(round(abs(x) * 100, 1L), "%")
  }
  
  .line <- function(label, value, width = 40L) {
    
    pad <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    
    return(message("     ", crayon::silver(label),
                   crayon::silver(dots), " ", value))
  }
  
  .section <- function(title) {
    
    rule <- paste(rep("\u2500", 60L), collapse = "" )
    message("")
    message("     ",crayon::bold(title))
    message("     ", rule)
    
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
      "sigma", "tau_p", "tau_v", "mu")
    design_slots <- x[names(x) %in% known_design]
    structure(design_slots, class = "movedesign")
  }
  
  summary(as.movedesign(object))
  
  data <- object$data
  summ <- object$summary
  n_reps <- max(summ$replicate, na.rm = TRUE)
  n_sims <- length(data$simList)
  error_threshold <- object$error_threshold
  
  grouped <- isTRUE(data$grouped)
  targets <- data$set_target
  species <- data$get_species %||% "Unknown"
  
  data_src <- if (
    identical(data$data_type, "simulated")
  ) "simulated" else "empirical"
  
  
  sim_counts <- object$summary %>%
    dplyr::group_by(.data$replicate) %>%
    dplyr::summarise(n_sims = dplyr::n()) %>%
    dplyr::ungroup()
  
  same_n <- length(unique(sim_counts$n_sims)) == 1
  
  target_map <- c(
    hr = "Home range",
    ctsd = "Speed \u0026 distance")
  
  meta_label <- switch(
    data$which_meta %||% "mean",
    mean = "Population mean",
    compare = "Group comparison",
    "Individual")
  
  .header("Workflow summary", 5)
  
  if (data_src == "simulated") {
    message(.msg(paste0("     Species: "), "main"),
            crayon::yellow(paste0("[", data_src, "]")))
  } else {
    message(.msg(paste0("     Species: "), "main"),
            crayon::bold(species),
            crayon::yellow(paste0(" [", data_src, "]")))
  }
  
  .section("Study design:")
  
  .line("Sampling duration",
        paste(data$dur$value, data$dur$unit))
  .line("Sampling interval",
        paste(data$dti$value, data$dti$unit))
  .line("No. of individuals",
        as.character(data$n_individuals))
  .line("Grouped design",
        .format_txt(grouped))
  
  if (grouped) {
    .line("No. of individuals per group", data$n_individuals / 2)
  }
  
  .line("Inference target",  meta_label)
  .line("Estimation target",
    paste(target_map[targets], collapse = ", "))
  .line("Individual variation",
        .format_lgc(data$add_ind_var))
  
  .section("Replication:")
  
  .line("No. of replicates",
        as.character(n_reps))
  .line("No. of simulations across replicates",
        as.character(n_sims))
  .line("No. simulations per replicate",
        as.character(unique(sim_counts$n_sims)))
  
  .section("Estimator performance:")
  
  summ_mean <- .summarize_error(summ, conf_level = ci,
                                error_threshold = error_threshold) %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::filter(m == max(m))
  
  if (grouped) {
    summ_mean <- dplyr::filter(summ_mean, .data$group != "All")
  }
  
  for (i in seq_len(nrow(summ_mean))) {
    
    row <- summ_mean[i, ]
    t_lbl <- target_map[row$type]
    t_lbl <- if (is.na(t_lbl)) row$type else t_lbl
    
    grp_tag <- if (grouped &&
                   !is.na(row$group) &&
                   row$group != "") {
      paste0(" [Group ", row$group, "]")
    } else {
      ""
    }
    
    err_col <- if (is.na(row$error)) {
      crayon::silver("n/a")
    } else if (abs(row$error) <= error_threshold) {
      crayon::green(.format_pct(row$error))
    } else { crayon::red(.format_pct(row$error)) }
    
    cis_str <- if (!is.na(row$error_lci) &&
                   !is.na(row$error_uci)) {
      paste0(" [", round(row$error_lci * 100),
             ", ", .format_pct(row$error_uci), "]")
    } else {
      ""
    }
    
    .line(paste0(t_lbl, grp_tag),
          paste0(err_col, cis_str))
  }
  
  if (verbose) {
    
    .section("Convergence")
    
    out_check <- tryCatch(
      md_check(object,
               m = m,
               tol = tol,
               n_converge = n_converge,
               plot = plot,
               pal = pal),
      error = function(e) e)
    
    if (inherits(out_check, "error")) {
      
      .line("Status",
            crayon::red("\u2718 Could not assess"))
      
      if (grepl("Not enough replicates",
                conditionMessage(out_check))) {
        
        warning(sprintf(
          "Not enough replicates to check %d convergence steps.",
          n_converge), call. = FALSE)
      }
      
      message("\n  ", paste0(
          "\u2015\u2015\u2015 Add more replicates: ",
          crayon::silver(
            "md_replicate(output, n_replicates = n)")))
      
    } else {
      
      diag <- out_check$diagnostics_table
      
      status <- if (all(diag$has_converged)) {
        crayon::yellow("\u2713 Converged")
        
      } else {
        if (nrow(diag) == 1 && !grouped) {
          crayon::red("\u2718 Not yet converged")
        } else if (grouped) {
          crayon::red("\u2718 Not yet converged for all groups")
        } else {
          crayon::red("\u2718 Not yet converged for all targets")
        }
      }
      
      .line("Steps evaluated", n_converge)
      .line("Tolerance set to", paste0(tol * 100, "%"))
      .line("Acceptable error threshold",
            .format_pct(error_threshold))
      .line("Status", status)
      
      message("\n  ", paste0(
        "\u2015\u2015\u2015 See further details on convergence: ",
        crayon::silver(
          "md_check(output)")))
      
      if (any(!diag$has_converged)) {
        message(paste0(
          "  \u2015\u2015\u2015 Add more replicates: ",
          crayon::silver(
          "md_replicate(output, n_replicates = n)")))
      }
    }
    
    if (n_reps <= 10) {
      warning(paste0(
        "Convergence diagnostics may be ", 
        crayon::yellow("unreliable"),
        " with few replicates.\n",
        crayon::bold(
          "  Increase number of replicates "),
        crayon::bold("for more robust inferences.")
      ), call. = FALSE)
    }
  } else {
    message(" ")
  }
  
  invisible(object)
}


#' Print method for `movedesign_output`
#'
#' @param x An object of class `movedesign_output`.
#' @param verbose Logical. If `TRUE`, convergence diagnostics are evaluated
#'   using [md_check()] and included in the output.
#' @param m Optional integer specifying the sample size used in convergence
#'   checks.
#' @param ci Numeric value giving the confidence level used when summarizing
#'   estimator error. Default is `0.95`.
#' @param tol Numeric tolerance used when assessing convergence. Default is
#'   `0.05`.
#' @param n_converge Integer giving the number of convergence steps to
#'   evaluate.
#' @param plot Logical indicating whether convergence diagnostics should
#'   produce plots when `verbose = TRUE`. Default is `TRUE`.
#' @param pal Character vector specifying colors used in convergence plots.
#' @param ... Additional arguments
#'
#' @method print movedesign_output
#' @export
print.movedesign_output <- function(x,
                                    verbose = FALSE,
                                    m = NULL,
                                    ci = 0.95,
                                    tol = 0.05,
                                    n_converge = 9,
                                    plot = TRUE,
                                    pal = c("#007d80", "#A12C3B"),
                                    ...) {
  
  .has_results <- function(x) {
    is.list(x) && any(!vapply(x, is.null, logical(1)))
  }
  
  .format_txt <- function(x) {
    if (length(x) > 0 && .has_results(x)) "Yes" else "No"
  }
  
  .format_lgc <- function(x) {
    if (x) "Yes" else "No"
  }
  
  .format_pct <- function(x) {
    paste0(round(abs(x) * 100, 1), "%")
  }
  
  .line <- function(label, value, width = 40L) {
    pad <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    paste0("     ", label, dots, " ", value)
  }
  
  .section <- function(title) {
    rule <- paste(rep("\u2500", 60L), collapse = "")
    c("", paste0("     ", title), paste0("     ", rule))
  }
  
  data <- x$data
  summ <- x$summary
  
  n_reps <- max(summ$replicate, na.rm = TRUE)
  n_sims <- length(data$simList)
  error_threshold <- x$error_threshold
  
  grouped <- isTRUE(data$grouped)
  targets <- data$set_target
  species <- data$get_species %||% "Unknown"
  
  data_src <- if (identical(data$data_type, "simulated"))
    "simulated" else "empirical"
  
  sim_counts <- x$summary %>%
    dplyr::group_by(.data$replicate) %>%
    dplyr::summarise(n_sims = dplyr::n()) %>%
    dplyr::ungroup()
  
  target_map <- c(
    hr = "Home range",
    ctsd = "Speed & distance"
  )
  
  meta_label <- switch(
    data$which_meta %||% "mean",
    mean = "Population mean",
    compare = "Group comparison",
    "Individual"
  )
  
  lines <- c(
    "", "\u2500\u2500\u2500\u2500 Workflow summary", "")
  
  if (data_src == "simulated") {
    lines <- c(lines,
               paste0("     Species: [", data_src, "]"))
  } else {
    lines <- c(lines,
               paste0("     Species: ", species, " [", data_src, "]"))
  }
  
  lines <- c(lines,
             
             .section("Study design:"),
             .line("Sampling duration",
                   paste(data$dur$value, data$dur$unit)),
             .line("Sampling interval",
                   paste(data$dti$value, data$dti$unit)),
             .line("No. of individuals",
                   data$n_individuals),
             .line("Grouped design",
                   .format_txt(grouped))
  )
  
  if (grouped) {
    
    lines <- c(
      lines,
      .line("No. of individuals per group",
            data$n_individuals / 2))
  }
  
  lines <- c(
    lines,
    
    .line("Inference target", meta_label),
    .line("Estimation target",
          paste(target_map[targets], collapse = ", ")),
    .line("Individual variation",
          .format_lgc(data$add_ind_var))
  )
  
  lines <- c(
    lines,
    
    .section("Replication:"),
    .line("No. of replicates", n_reps),
    .line("No. of simulations across replicates", n_sims),
    .line("No. simulations per replicate",
          paste(unique(sim_counts$n_sims), collapse = ", "))
  )
  
  lines <- c(lines, .section("Estimator performance:"))
  
  summ_mean <- .summarize_error(summ, conf_level = ci,
                                error_threshold = error_threshold) %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::filter(m == max(m))
  
  if (grouped) {
    summ_mean <- dplyr::filter(summ_mean, .data$group != "All")
  }
  
  for (i in seq_len(nrow(summ_mean))) {
    
    row <- summ_mean[i, ]
    
    t_lbl <- target_map[row$type]
    if (is.na(t_lbl)) t_lbl <- row$type
    
    grp_tag <- if (grouped &&
                   !is.na(row$group) &&
                   row$group != "") {
      paste0(" [Group ", row$group, "]")
    } else ""
    
    err_val <- if (is.na(row$error)) {
      "N/A"
    } else {
      .format_pct(row$error)
    }
    
    cis_str <- if (!is.na(row$error_lci) &&
                   !is.na(row$error_uci)) {
      paste0(" [", round(row$error_lci * 100),
             ", ", .format_pct(row$error_uci), "]")
    } else ""
    
    lines <- c(
      lines,
      .line(paste0(t_lbl, grp_tag),
            paste0(err_val, cis_str)))
  }
  
  if (verbose) {
    
    lines <- c(lines, .section("Convergence"))
    
    out_check <- tryCatch(
      md_check(x,
               m = m,
               tol = tol,
               n_converge = n_converge,
               plot = plot,
               pal = pal),
      error = function(e) e)
    
    if (inherits(out_check, "error")) {
      
      lines <- c(
        lines,
        .line("Status", "Could not assess"),
        "",
        paste("     \u2015\u2015\u2015 Add more replicates:",
              "`md_replicate(output, n_replicates = n)`"))
      
    } else {
      
      diag <- out_check$diagnostics_table
      
      status <- if (all(diag$has_converged)) {
        "Converged"
      } else if (nrow(diag) == 1 && !grouped) {
        "Not yet converged"
      } else if (grouped) {
        "Not yet converged for all groups"
      } else {
        "Not yet converged for all targets"
      }
      
      lines <- c(
        lines,
        
        .line("Steps evaluated", n_converge),
        .line("Tolerance set to", paste0(tol * 100, "%")),
        .line("Acceptable error threshold", .format_pct(error_threshold)),
        .line("Status", status),
        
        "",
        paste("     \u2015\u2015\u2015 See convergence diagnostics:",
              "`md_check(output)`"))
      
      if (any(!diag$has_converged)) {
        lines <- c(
          lines,
          paste("     \u2015\u2015\u2015 Add more replicates:",
                "`md_replicate(output, n_replicates = n)`"))
      }
    }
    
    if (n_reps <= 10) {
      lines <- c(
        lines,
        "",
        paste("     Warning: convergence diagnostics may be",
              "unreliable with few replicates."))
    }
  }
  
  lines <- c(lines, " ")
  cat(paste(lines, collapse = "\n"), "\n")
  
  return(invisible(x))
}


#' Summary method for `movedesign_check` objects
#' @param object An object of class `movedesign_check`
#' @param verbose Add interpretation text
#' @param ... Unused
#' @export
summary.movedesign_check <- function(object,
                                     verbose = FALSE,
                                     ...) {
  
  . <- type <- group <- NULL
  
  .line <- function(label, value, width = 40L) {
    
    pad <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    
    return(message("     ", crayon::silver(label),
                   crayon::silver(dots), " ", value))
  }
  
  .section <- function(title) {
    
    rule <- paste(rep("\u2500", 60L), collapse = "" )
    message("")
    message("     ",crayon::bold(title))
    message("     ", rule)
    
  }
  
  .target_map <- function(target) {
    switch(target,
           hr = "Home range estimation",
           ctsd = "Movement speed estimation",
           target)
  }
  
  .stable_at <- function(type, group) {
    idx <- object$stabilized_at
    idx <- idx[idx$type == type, ]
    if (object$grouped) idx <- idx[idx$group == group, ]
    idx$idx_stable_start
  }
  
  diag <- object$diagnostics_table
  tol <- object$tolerance
  error_threshold <- object$error_threshold
  
  types <- unique(diag$type)
  has_groups <- object$grouped
  
  cat("\n")
  .header("Convergence diagnostics", 5)
  
  message(format(crayon::yellow("Steps evaluated: "),
                 width = 3, justify = "left"),
          length(diag$recent_cummean[[1]]))
  message("Tolerance set to ",
          crayon::yellow(paste0(.err_to_txt(tol), "%")))
  
  if (!is.null(error_threshold)) {
    message("Acceptable error threshold: ",
            crayon::yellow(paste0(.err_to_txt(error_threshold), "%")))
  }
  
  for (type_i in types) {
    
    .section(paste0(.target_map(type_i), ":"))
    
    rows <- diag[diag$type == type_i, ]
    
    for (i in seq_len(nrow(rows))) {
      
      row <- rows[i, ]
      
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
      
      mean_err <- row$last_cummean
      converged <- row$has_converged
      
      within_threshold <- if (!is.null(error_threshold)) {
        abs(mean_err) <= error_threshold
      } else {
        NA
      }
      
      message(sprintf(
        "     Convergence check for %s", tg))
      
      if (converged) {
        
        message("        Mean estimate error: ",
                .msg(paste0(.err_to_txt(mean_err), "%"),
                     "success"))
        
        stable <- .stable_at(type_i, row$group)
        message(sprintf(
          "        \u2713 Converged at replicate %d.", stable))
        
      } else {
        
        message("        Mean estimate error: ",
                .msg(paste0(.err_to_txt(mean_err), "%"),
                     "danger"))
        
        # Identify which criteria failed:
        
        failed_delta <- any(sapply(
          diag$recent_delta_cummean,
          function(x) any(abs(x) > tol, na.rm = TRUE)))
        
        failed_sd <- any(sapply(
          diag$recent_roll_sd,
          function(x) any(x > tol, na.rm = TRUE)))
        
        # Construct informative message:
        
        fail_reasons <- c()
        if (failed_delta) fail_reasons <- c(
          "\n           \u2500 stepwise change exceeded tolerance")
        if (failed_sd) fail_reasons <- c(
          fail_reasons,
          "\n           \u2500 recent variability exceeded tolerance")
        
        message(.msg("        \u2717 Did not converge: ", "danger"),
                paste(fail_reasons, collapse = "; "), ".")
      }
      
      if (within_threshold) {
        
        txt_start <- ifelse(converged,
                            "Error",
                            "However, error")
        message(.msg(
          paste0(
            "        \u2713 ", txt_start, 
            " within acceptable threshold ",
            "(\u2264 \u00B1", .err_to_txt(error_threshold),
            "%)."), "success"))
        
      } else {
        
        txt_start <- ifelse(converged,
                            "However, error",
                            "Error")
        message(.msg(
          paste0(
            "        \u2717 ", txt_start, 
            " exceeds acceptable threshold ",
            "(\u003E \u00B1", .err_to_txt(error_threshold),
            "%)."), "danger"))
      }
      
      if (!verbose) {
        message(" ")
        cat("\n")
        next
      } else {
        message(" ")
      }
      
      .header("Interpretation", 15)
      
      if (converged) {
        message(paste(
          "Cumulative mean error is", .msg("stable", "success"),
          "within specified tolerance and steps."))
        
      } else {
        message(paste(
          "Cumulative mean error is", .msg("unstable", "danger"),
          "within specified tolerance and steps.\n    ",
          crayon::bold(
            "Increase replicates for more robust inferences.")))
      }
      
      if (!is.null(error_threshold)) {
        message("")
        
        if (within_threshold) {
          message(paste0(
            "Error is within the ",
            .msg("predefined", "success"), " error threshold.\n     ",
            crayon::bold("Design parameters meet target.")))
          
        } else {
          
          txt_start <- ifelse(converged,
                              "However, error ",
                              "Error ")
          
          message(paste0(
           txt_start, .msg("exceeds", "danger"),
            " the predefined error threshold.\n     ",
           crayon::bold("Adjust design parameters to meet target.")))
        }
      }
      
      cat("\n")
    }
  }

  if (!is.null(object$warning)) {
    warning(paste0(
        "Convergence diagnostics may be ", 
        crayon::yellow("unreliable"),
        " with few replicates.\n",
        crayon::bold(
          "  Increase number of replicates "),
        crayon::bold("for more robust inferences.")
      ), call. = FALSE)
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


#' @title Summarise a study design optimization
#'
#' @description
#' Print a structured summary of a `movedesign_optimized` object
#' produced by [md_optimize()]. The summary
#' reports the study design, replication settings, estimation
#' performance for each target metric, and a convergence
#' assessment.
#'
#' This method runs automatically when calling
#' `summary(output)` on a `movedesign_optimized` object.
#'
#' @param object A `movedesign_optimized` object returned by
#'   [md_replicate()] or [md_stack()].
#'
#' @param verbose Logical. If `TRUE`, run [md_check()] and
#'   print the full convergence diagnostics. This can also
#'   display a convergence plot when `plot = TRUE`.
#'   If `FALSE` (default), only the convergence status is
#'   printed.
#'   
#' @param m Numeric (Optional). If provided, restricts the results to
#'   a specific population sample size (`m`). Defaults to `NULL`, which
#'   checks up to the maximum population sample size.
#'   
#' @param ci Confidence level for the intervals. Applied to both
#'   the narrow confidence bars and wide prediction bands. Must be
#'   between `0` and `1`. Default: `0.95` (95%).
#'   
#' @param tol Numeric. The tolerance threshold for absolute change in
#'   the cumulative mean to declare convergence. Defaults to
#'   `0.05`.
#'   
#' @param n_converge Integer. Number of consecutive steps within
#'   tolerance required to confirm convergence.
#'   
#' @param plot Logical. If `TRUE` (default), generates a plot of
#'   stepwise changes in the cumulative mean, highlighting when
#'   convergence is achieved.
#'   
#' @param pal Character vector of color(s) of the plot, such as
#'   `c("#007d80", "#A12C3B")`) (default).
#'   
#' @param ... Additional arguments
#'   
#' @method summary movedesign_optimized
#' @export
summary.movedesign_optimized <- function(object,
                                         verbose = FALSE,
                                         m = NULL,
                                         ci = 0.95,
                                         tol = 0.05,
                                         n_converge = 9,
                                         plot = TRUE,
                                         pal = c("#007d80",
                                                 "#A12C3B"),
                                         ...) {
  
  if (!verbose) {
    extra_args_used <- !missing(m) ||
      !missing(ci) ||
      !missing(tol) ||
      !missing(n_converge) ||
      !missing(plot) ||
      !missing(pal) ||
      length(list(...)) > 0
    
    if (extra_args_used) {
      stop("Additional arguments require verbose = TRUE.",
           call. = FALSE)
    }
  }
  
  has_groups <- object$data$grouped
  set_target <- object$data$set_target
  error_threshold <- object$error_threshold
  n_replicates <- object$data$n_replicates
  
  groups <- if (has_groups) c("A", "B") else c("All")
  
  .target_map <- function(target) {
    switch(target,
           hr = "Home range estimation",
           ctsd = "Movement speed estimation",
           target)
  }
  
  .format_pct <- function(x) {
    paste0(round(abs(x) * 100, 1L), "%")
  }
  
  .color <- function(x, ok) {
    if (ok) crayon::yellow(x) else crayon::red(x)
  }
  
  .make_header <- function(title, n_dash = 10) {
    header_line <- paste0(strrep("\u2500", n_dash), " ", title, ":")
    return(crayon::bold(header_line))
  }
  
  .make_section <- function(title, width = 65L) {
    rule <- paste(rep("\u2500", width), collapse = "")
    c("",
      paste0("     ", title),
      paste0("     ", rule))
  }
  
  .make_line <- function(label, value, width = 45L) {
    pad <- width - nchar(crayon::strip_style(label))
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    paste0("     ", label, dots, " ", value)
  }
  
  set_species <- object$data$get_species %||% "Unknown"
  
  set_source <- if (
    identical(object$data_type, "simulated")
  ) "simulated" else "empirical"
  
  set_meta <- switch(
    object$which_meta %||% "mean",
    mean = "Population mean",
    compare = "Group comparison",
    "Individual")
  
  m_max <- object$data$n_individuals /
    object$data$n_replicates
  
  out <- .md_plot_meta(object$summary,
                       error_threshold = error_threshold,
                       set_target = set_target,
                       has_groups = has_groups)
  
  # Begin report:
  
  report <- c(
    " ", .make_header("Workflow summary", 4))
  
  report <- c(
    report,
    .make_line("Species",
               paste(crayon::bold(set_species),
                     crayon::yellow(paste0("[", set_source, "]")))),
    .make_line("Population sample size evaluated",
               crayon::yellow(m_max)))
  
  report <- c(
    report,
    " ", .make_header("Study design performance", 4))
  
  for (target in set_target) {
    
    report <- c(report,
                .make_section(.target_map(target)))
    
    for (g in seq_along(groups)) {
      
      all_broke_when <- out$dt_plot_means %>%
        dplyr::inner_join(
          out$dt_plot %>%
            dplyr::group_by(.data$type, .data$group, .data$m) %>%
            dplyr::summarize(
              all_within_threshold = all(
                abs(.data$error) <= error_threshold),
              .groups = "drop"),
          by = c("type", "group", "m")) %>%
        dplyr::filter(.data$overlaps == TRUE,
                      .data$all_within_threshold == TRUE) %>%
        dplyr::group_by(.data$type, .data$group) %>%
        dplyr::filter(.data$type == target) %>%
        dplyr::filter(.data$group == groups[[g]]) %>%
        dplyr::slice_min(.data$m, n = 1) %>%
        dplyr::ungroup()
      
      broke_when <- out$dt_plot_means %>%
        dplyr::filter(abs(.data$error) <= error_threshold) %>%
        dplyr::group_by(.data$type, .data$group) %>%
        dplyr::arrange(.data$m, .by_group = TRUE) %>%
        dplyr::filter(.data$type == target) %>%
        dplyr::filter(.data$group == groups[[g]]) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()
      
      error <- out$dt_plot_means %>%
        dplyr::filter(.data$type == target) %>%
        dplyr::filter(.data$group == groups[[g]]) %>%
        .summarize_error(error_threshold = error_threshold) %>%
        dplyr::slice_max(.data$m) %>%
        dplyr::pull(.data$error)
      
      error_ok <- abs(error) <= error_threshold
      # Population size that achieves mean error below threshold:
      error_ok_m <- if (error_ok) broke_when$m else NA
      
      all_error_ok <- !is.na(all_broke_when$m)
      if (length(all_error_ok) == 0) all_error_ok <- FALSE
      # Population size that achieves all replicates below threshold:
      all_error_ok_m <- if (all_error_ok) all_broke_when$m else NA
      
      if (error_ok && all_error_ok) {
        if (g == 1) {
          report <- c(
            report,
            .make_line(
              paste("Minimum",
                    .color("population", all_error_ok), "sample size"),
              .color(all_error_ok_m, all_error_ok)))
        }
      } else if (error_ok && !all_error_ok) {
        if (g == 1) {
          report <- c(
            report,
            .make_line(
              paste("Minimum",
                    .color("population", error_ok), "sample size"),
              .color(error_ok_m, error_ok)))
        }
      } else {
        if (g == 1) {
          report <- c(
            report,
            .make_line(
              paste(crayon::red("Population"), "sample size"),
              paste(crayon::red(m_max),
                    crayon::red("[insufficient]"))))
        }
      }
      
      if (g == 1) {
        report <- c(
          report,
          .make_line(
            paste("Recommended sampling",
                  crayon::yellow("duration")),
            paste("at least",
                  crayon::yellow(object$sampling_duration))),
          .make_line(
            paste("Recommended sampling", 
                  crayon::yellow("interval")),
            paste("no more than", crayon::yellow(
              object$sampling_interval))))
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
      
      if (error_ok) {
        report <- c(
          report,
          "",
          paste0("     ",
                 crayon::yellow("\u2713 "), "Mean relative error ",
                 crayon::yellow("within threshold"), " (\u00B1",
                 round(error_threshold * 100, 0), "%) achieved at m = ",
                 crayon::yellow(error_ok_m)))
        
        n_outside_threshold <- out$dt_plot %>%
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
        
        n_outside_threshold <- out$dt_plot %>%
          dplyr::group_by(.data$type, .data$group) %>%
          dplyr::filter(.data$m == max(.data$m, na.rm = TRUE)) %>%
          dplyr::summarise(
            n_outside_threshold = sum(
              abs(.data$error) > error_threshold, na.rm = TRUE),
            n_replicates = dplyr::n(),
            .groups = "keep") %>%
          dplyr::pull(n_outside_threshold)
        set_m <- m_max
      }
      
      if (all_error_ok) {
        extra_words <- ifelse(error_ok, "All", "However, all")
        report <- c(
          report,
          paste0("     ",
                 crayon::yellow("\u2713 "), extra_words, " replicates ",
                 crayon::yellow("within threshold"), " (\u00B1",
                 round(error_threshold * 100, 0), "%) achieved at m = ",
                 crayon::yellow(all_error_ok_m), "."))
        
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
    
    
    if (verbose) {
      
      out_check <- tryCatch(
        md_check(object,
                 m = m,
                 tol = tol,
                 n_converge = n_converge,
                 plot = plot,
                 pal = pal),
        error = function(e) e)
      
      report <- c(
        report,
        .make_section("Convergence:"))
      
      if (inherits(out_check, "error")) {
        
        report <- c(
          report,
          .make_line("Status",
                     crayon::red("\u2718 Could not assess")))
        
        if (grepl("Not enough replicates",
                  conditionMessage(out_check))) {
          
          warning(sprintf(
            "Not enough replicates to check %d convergence steps.",
            n_converge), call. = FALSE)
        }
        
        report <- c(
          report,
          "",
          paste0(
            "     \u2015\u2015\u2015 Add more replicates: ",
            crayon::silver("md_optimize(input, n_replicates = n)")))
        
      } else {
        
        diag <- out_check$diagnostics_table
        
        if (all(diag$has_converged)) {
          status <- crayon::yellow("\u2713 Converged")
          status_ok <- TRUE
        } else {
          status_ok <- FALSE
          if (nrow(diag) == 1 && !has_groups) {
            status <- crayon::red(
              "\u2718 Not yet converged")
          } else if (has_groups) {
            status <- crayon::red(
              "\u2718 Not yet converged for all groups")
          } else {
            status <- crayon::red(
              "\u2718 Not yet converged for all targets")
          }
        }
        
        report <- c(
          report,
          .make_line("Steps evaluated",
                     .color(n_converge, status_ok)),
          .make_line("Tolerance set to", paste0(tol * 100, "%")),
          .make_line("Acceptable error threshold",
                     .format_pct(error_threshold)),
          .make_line("Status", status),
          "",
          paste0(
            "  \u2015\u2015\u2015",
            " See further details on convergence: ",
            crayon::silver("md_check(output)")))
        
        if (any(!diag$has_converged)) {
          
          report <- c(
            report,
            paste0(
              "  \u2015\u2015\u2015 Add more replicates: ",
              crayon::silver("md_optimize(input, n_replicates = n)")))
        }
      }
    }
    
  } # end of [target] loop
  
  writeLines(report)
  invisible(object)
}


#' Print method for `movedesign_optimized` objects
#'
#' @description
#' Print a structured summary of a `movedesign_optimized` object produced
#' by [md_optimize()]. This includes study design details, replication
#' settings, estimation performance per target metric, and an optional
#' convergence assessment.
#'
#' @param x An object of class `movedesign_optimized`.
#' @param verbose Logical. If `TRUE`, run [md_check()] and print full
#'   convergence diagnostics. Also displays a convergence plot if
#'   `plot = TRUE`. Defaults to `FALSE`.
#' @param m Numeric (optional). Restricts results to a specific population
#'   sample size. Defaults to `NULL`, which uses the maximum sample size.
#' @param ci Numeric. Confidence level for intervals (applied to narrow
#'   confidence bars and wide prediction bands). Must be between
#'   `0` and `1`. Default is `0.95`.
#' @param tol Numeric. Tolerance threshold for absolute change in
#'   cumulative mean to declare convergence. Default is `0.05`.
#' @param n_converge Integer. Number of consecutive steps within
#'   tolerance required to confirm convergence. Default is `9`.
#' @param plot Logical. If `TRUE`, generates a convergence plot. Default
#'   is `TRUE`.
#' @param pal Character vector of colors for the convergence plot, e.g.
#'   `c("#007d80", "#A12C3B")`. Default is `c("#007d80", "#A12C3B")`.
#' @param ... Additional arguments (currently unused).
#'
#' @method print movedesign_optimized
#' @export
print.movedesign_optimized <- function(x,
                                       verbose = TRUE,
                                       m = NULL,
                                       ci = 0.95,
                                       tol = 0.05,
                                       n_converge = 9,
                                       plot = TRUE,
                                       pal = c("#007d80",
                                               "#A12C3B"),
                                       ...) {
  
  grouped <- NULL
  
  if (!verbose) {
    extra_args_used <- !missing(m) ||
      !missing(ci) ||
      !missing(tol) ||
      !missing(n_converge) ||
      !missing(plot) ||
      !missing(pal) ||
      length(list(...)) > 0
    
    if (extra_args_used) {
      stop("Additional arguments are only allowed when verbose = TRUE.",
           call. = FALSE)
    }
  }
  
  type <- NULL
  has_groups <- x$data$grouped
  set_target <- x$data$set_target
  error_threshold <- x$error_threshold
  
  .target_map <- function(target) {
    switch(target,
           hr = "Home range estimation",
           ctsd = "Movement speed estimation",
           target)
  }
  
  .format_pct <- function(x) {
    paste0(round(abs(x) * 100, 1L), "%")
  }
  
  .line <- function(label, value, width = 46L) {
    
    pad <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    
    paste0("     ", label, dots, " ", value)
  }
  
  .section <- function(title) {
    
    rule <- paste(rep("\u2500", 65L), collapse = "")
    
    c("",
      paste0("     ", title),
      paste0("     ", rule))
  }
  
  set_species <- x$data$get_species %||% "Unknown"
  
  set_source <- if (identical(x$data_type, "simulated")) {
    "simulated"
  } else {
    "empirical"
  }
  
  set_meta <- switch(
    x$which_meta %||% "mean",
    mean = "Population mean",
    compare = "Group comparison",
    "Individual")
  
  lines <- c(
    "",
    "     Workflow summary",
    paste0("     Species: ", set_species, " [", set_source, "]")
  )
  
  for (target in set_target) {
    
    lines <- c(
      lines,
      .section(paste0(.target_map(target), ":"))
    )
    
    if (x$sample_size_achieved) {
      
      lines <- c(
        lines,
        .line("Minimum population sample size",
              x$minimum_population_sample_size),
        .line("Recommended sampling duration",
              x$sampling_duration),
        .line("Recommended sampling interval",
              x$sampling_interval),
        paste0(
          "     \u2713 Error within threshold of \u00B1",
          error_threshold * 100,
          "%. Parameters are sufficient!"))
      
    } else {
      
      lines <- c(
        lines,
        .line("Population sample size evaluated",
              x$minimum_population_sample_size),
        .line("Recommended sampling duration",
              x$sampling_duration),
        .line("Recommended sampling interval",
              x$sampling_interval),
        "",
        paste0(
          "     \u2718 Error outside threshold of \u00B1",
          error_threshold * 100,
          "%. More individuals needed!"),
        "     Increase maximum population sample size and try again.")
    }
    
    if (verbose) {
      
      lines <- c(
        lines,
        "",
        .section("Convergence:")
      )
      
      out_check <- tryCatch(
        md_check(x,
                 m = m,
                 tol = tol,
                 n_converge = n_converge,
                 plot = plot,
                 pal = pal),
        error = function(e) e)
      
      if (inherits(out_check, "error")) {
        
        lines <- c(
          lines,
          .line("Status", "\u2718 Could not assess")
        )
        
        if (grepl("Not enough replicates",
                  conditionMessage(out_check))) {
          
          warning(sprintf(
            "Not enough replicates to check %d convergence steps.",
            n_converge), call. = FALSE)
        }
        
        lines <- c(
          lines,
          paste("  \u2015\u2015\u2015 Add more replicates:",
                "`md_optimize(input, n_replicates = n)`"))
        
      } else {
        
        diag <- out_check$diagnostics_table
        
        status <- if (all(diag$has_converged)) {
          "\u2713 Converged"
          
        } else {
          if (nrow(diag) == 1 && !has_groups) {
            "\u2718 Not yet converged"
          } else if (has_groups) {
            "\u2718 Not yet converged for all groups"
          } else {
            "\u2718 Not yet converged for all targets"
          }
        }
        
        lines <- c(
          lines,
          .line("Steps evaluated", n_converge),
          .line("Tolerance set to", paste0(tol * 100, "%")),
          .line("Acceptable error threshold",
                .format_pct(error_threshold)),
          .line("Status", status),
          "",
          paste("  \u2015\u2015\u2015",
                "See further details on convergence:",
                "`md_check(output)`"))
        
        if (any(!diag$has_converged)) {
          lines <- c(
            lines,
            paste("  \u2015\u2015\u2015 Add more replicates:",
                  "`md_optimize(input, n_replicates = n)`"))
        }
      }
    }
  }
  
  
  lines <- c(lines, " ")
  writeLines(lines)
  return(invisible(x))
}


#' Plot movedesign report outputs
#'
#' @description
#' S3 method for plotting a `movedesign_optimized` object.
#' Returns the precomputed ggplot stored in the object.
#'
#' @param x A `movedesign_optimized` object returned by
#'   `md_optimized()` or similar.
#' @param ... Unused
#'
#' @export
plot.movedesign_optimized <- function(x, ...) {
  
  if (is.null(x$plot)) {
    stop("No plot stored in this `movedesign_optimized` object.")
  }
  
  return(x$plot)
}


#' Summary method for `movedesign_report` objects
#' @param object An object of class `movedesign_report`
#' @param ... Unused
#' @export
summary.movedesign_report <- function(object, ...) {
  
  .target_map <- function(target) {
    switch(target,
           hr = "Home range estimation",
           ctsd = "Movement speed estimation",
           target)
  }
  
  .make_header <- function(title, n_dash = 10) {
    header_line <- paste0(
      crayon::yellow(strrep("\u2500", n_dash)), " ", title, ":")
    return(crayon::bold(header_line))
  }
  
  .line <- function(label, value, width = 20L) {
    
    pad <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    
    return(message("     ", crayon::silver(label),
                   crayon::silver(dots), " ", value))
  }
  
  .section <- function(title) {
    
    rule <- paste(rep("\u2500", 70L), collapse = "" )
    message("")
    message("     ",crayon::bold(title))
    message("     ", rule)
    
  }
  
  ranking <- object$ranking
  joint_winners <- object$winners
  
  error_threshold <- object$info$error_threshold
  has_groups <- object$info$grouped
  
  set_targets <- unique(joint_winners$type)
  
  .header("Design comparison for", 3)
  
  if (nrow(joint_winners) == 0) {
    message(.msg("   No designs achieved rank 1 for any target.",
                 "warning"))
    
  } else {
    for (target in set_targets) {
      
      message(crayon::bold(paste0(
        strrep("\u2500", 8), " ", .target_map(target))))
      
      # Get joint winners for this target:
      jw_rows <- joint_winners[
        joint_winners$type == target, , drop = FALSE]
      
      # Check if all designs are joint winners:
      if (nrow(jw_rows) > 1 && 
          nrow(jw_rows) == length(unique(ranking$design_id))) {
        
        message(paste0(
          crayon::bold(
            "     Multiple designs are",
            crayon::yellow("equally optimal"),
            "for this target;")))
        message(paste0("     No single design outperforms the ",
                       "others across all groups.\n"))
        next
      }
      
      # Otherwise, report each joint winner:
      for (winner in seq_len(nrow(jw_rows))) {
        
        jw <- jw_rows[winner, , drop = FALSE]
        d_id <- jw$design_id
        
        # Check if multiple designs tied rank 1:
        rank_rows <- ranking[ranking$design_id == d_id &
                               ranking$type == target &
                               ranking$rank == 1L, , drop = FALSE]
        
        if ((nrow(rank_rows) > 1 && !has_groups) ||
            (nrow(rank_rows) > 2 && has_groups)) {
          .line("Best study design",
                paste0("multiple designs are ",
                       .msg("equally optimal", "danger"), "."))
        } else {
          .line("Best study design", crayon::yellow(d_id))
        }
        
        # Groups:
        if (has_groups && "groups_won" %in% names(jw)) {
          .line("Wins for groups", .msg(jw$groups_won, "success"))
        }
        
        .section("Parameters of best study design:")
        
        if (has_groups) {
          .line("No. of individuals",
                paste0(crayon::yellow(unique(rank_rows$m)), " (",
                       crayon::yellow(as.numeric(unique(rank_rows$m))/2),
                       " per group)"))
        } else {
          .line("No. of individuals",
                .msg(unique(rank_rows$m), "success"))
        }
        .line("Sampling duration",
              paste(round(unique(rank_rows$dur), 1),
                    unique(rank_rows$dur_unit)))
        .line("Sampling interval",
              paste(round(unique(rank_rows$dti), 1),
                    unique(rank_rows$dti_unit)))
        
        # Error and CI per design:
        est_rows <- ranking[ranking$design_id == d_id &
                              ranking$type == target, , drop = FALSE]
        
        .section("Estimator performance:")
        
        for (r in seq_len(nrow(est_rows))) {
          
          w <- est_rows[r, ]
          
          if (has_groups) {
            message(crayon::yellow("     For group", w$group))
          }
          
          if (is.na(w$error_lci) || is.na(w$error_uci)) {
            .line("Relative error", 
                  .msg(paste0(.err_to_txt(w$error), "%"), "danger"))
            .line("CI", .msg("N/A", "danger"))
          } else {
            .line("Relative error", 
                  paste0(.err_to_txt(w$error), "%"))
            .line("CI", paste0("[", .err_to_txt(w$error_lci),
                               ", ", .err_to_txt(w$error_uci), "%]"))
          }
          
          # Reason for ranking:
          
          reason <- if (is.na(w$overlaps_with_zero)) {
            "Absolute error minimized."
          } else if (w$overlaps_with_zero) {
            paste0("Absolute error ",
                   .msg("minimized", "success"),
                   ", and CI overlaps with ",
                   .msg("0", "success"), ".")
          } else {
            paste0("Absolute error ",
                   .msg("minimized", "success"), 
                   ", and CI closest to 0.")
          }
          
          .line("Reason", reason)
          
        }
        
        cat("\n")
        
      }
    }
  }
  
  invisible(object)
}


#' Print method for `movedesign_report` objects
#' @param x An object of class `movedesign_report`
#' @param ... Unused
#' @export
print.movedesign_report <- function(x, ...) {
  
  .target_map <- function(target) {
    switch(target,
           hr = "Home range estimation",
           ctsd = "Movement speed estimation",
           target)
  }
  
  .line <- function(label, value, width = 20L) {
    pad <- width - nchar(label)
    dots <- paste(rep(".", max(pad, 1L)), collapse = "")
    paste0("     ", label, dots, " ", value)
  }
  
  .section <- function(title) {
    rule <- paste(rep("\u2500", 70L), collapse = "")
    c("",
      paste0("     ", title),
      paste0("     ", rule))
  }
  
  ranking <- x$ranking
  joint_winners <- x$winners
  
  error_threshold <- x$info$error_threshold
  has_groups <- x$info$grouped
  
  set_targets <- unique(joint_winners$type)
  
  lines <- c("", " Design comparison for")
  
  if (nrow(joint_winners) == 0) {
    
    lines <- c(
      lines,
      "   No designs achieved rank 1 for any target.")
    
  } else {
    
    for (target in set_targets) {
      
      lines <- c(
        lines,
        paste0(" ", strrep("\u2500", 3),
               " ", .target_map(target), ":"))
      
      jw_rows <- joint_winners[
        joint_winners$type == target, , drop = FALSE]
      
      if (nrow(jw_rows) > 1 &&
          nrow(jw_rows) == length(unique(ranking$design_id))) {
        
        lines <- c(
          lines,
          paste0("     Multiple designs are ",
                 "equally optimal for this target;"),
          paste0("     ",
                 "No single design outperforms the ",
                 "others across all groups."))
        
        next
      }
      
      for (winner in seq_len(nrow(jw_rows))) {
        
        jw <- jw_rows[winner, , drop = FALSE]
        d_id <- jw$design_id
        
        rank_rows <- ranking[
          ranking$design_id == d_id &
            ranking$type == target &
            ranking$rank == 1L, , drop = FALSE]
        
        if ((nrow(rank_rows) > 1 && !has_groups) ||
            (nrow(rank_rows) > 2 && has_groups)) {
          
          lines <- c(
            lines,
            .line("Best study design",
                  "multiple designs are equally optimal."))
          
        } else {
          
          lines <- c(
            lines,
            .line("Best study design", d_id))
        }
        
        if (has_groups && "groups_won" %in% names(jw)) {
          lines <- c(
            lines,
            .line("Wins for groups", jw$groups_won))
        }
        
        lines <- c(
          lines,
          .section("Parameters of best study design:"))
        
        if (has_groups) {
          
          lines <- c(
            lines,
            .line(
              "No. of individuals",
              paste0(unique(rank_rows$m),
                     " (",
                     as.numeric(unique(rank_rows$m)) / 2,
                     " per group)")))
          
        } else {
          
          lines <- c(
            lines,
            .line("No. of individuals",
                  unique(rank_rows$m)))
        }
        
        lines <- c(
          lines,
          .line("Sampling duration",
                paste(round(unique(rank_rows$dur), 1),
                      unique(rank_rows$dur_unit))),
          .line("Sampling interval",
                paste(round(unique(rank_rows$dti), 1),
                      unique(rank_rows$dti_unit))))
        
        est_rows <- ranking[
          ranking$design_id == d_id &
            ranking$type == target,
          , drop = FALSE]
        
        lines <- c(lines, .section("Estimator performance:"))
        
        for (r in seq_len(nrow(est_rows))) {
          
          w <- est_rows[r, ]
          
          if (has_groups) {
            lines <- c(
              lines,
              paste0("     For group ", w$group))
          }
          
          if (is.na(w$error_lci) || is.na(w$error_uci)) {
            
            lines <- c(
              lines,
              .line("Relative error",
                    paste0(.err_to_txt(w$error), "%")),
              .line("CI", "N/A"))
            
          } else {
            
            lines <- c(
              lines,
              .line("Relative error",
                    paste0(.err_to_txt(w$error), "%")),
              .line("CI",
                    paste0("[", .err_to_txt(w$error_lci),
                           ", ", .err_to_txt(w$error_uci), "%]")))
          }
          
          reason <- if (is.na(w$overlaps_with_zero)) {
            "Absolute error minimized."
          } else if (w$overlaps_with_zero) {
            "Absolute error minimized, and CI overlaps with 0."
          } else {
            "Absolute error minimized, and CI closest to 0."
          }
          
          lines <- c(
            lines,
            .line("Reason", reason))
        }
        
        lines <- c(lines, "")
      }
    }
  }
  
  cat(paste(lines, collapse = "\n"), "\n")
  
  return(invisible(x))
  
}

