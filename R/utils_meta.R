
#' @title Capture meta-analysis output
#'
#' @description Capture all outputs from the ctmm::meta() function.
#' @keywords internal
#'
#' @importFrom ctmm %#%
#' @importFrom dplyr %>%
#' 
#' @noRd
.capture_meta <- function(
    x, 
    variable = c("tau position", "tau velocity", "area", "speed"),
    type = NULL,
    sort = TRUE, 
    units = FALSE,
    verbose = TRUE,
    plot = TRUE) {
  
  if (length(variable) > 1) stop("Add 'variable' argument.")
  variable <- match.arg(variable)
  
  if (variable == "tau position" || variable == "tau velocity") {
    type <- variable
    x.new <- lapply(seq_along(x), function(i) {
      if (variable == "tau position") E <- x[[i]]$tau[1]
      if (variable == "tau velocity") E <- x[[i]]$tau[2]
      if (is.null(E)) out <- NULL
      else if (is.na(E)) out <- NULL
      else out <- x[[i]]
    })
    names(x.new) <- names(x)
    x <- x.new
    rm(x.new)
    
    x[sapply(x, is.null)] <- NULL
    if (length(x) == 0) return(NULL)
  }
  
  is_grouped <- class(x)[1] == "list" && class(x[[1]])[1] == "list" && 
    !(length(names(x[[1]])) == 2 && 
        all(names(x[[1]]) == c("DOF", "CI")))
  
  num_groups <- ifelse(is_grouped, length(x), 1)
  num_out <- ifelse(is_grouped, length(x) + 1, 1)

  if (is_grouped) {
    
    x[[1]][sapply(x[[1]], is.null)] <- NULL
    x[[2]][sapply(x[[2]], is.null)] <- NULL
    
    # x <- lapply(x, \(y) y[sapply(y, is.null)] <- NULL)
  }
  
  out <- tryCatch({
    ctmm::meta(x, 
               variable = variable,
               units = FALSE, 
               verbose = FALSE, 
               plot = FALSE) %>% 
      suppressMessages() %>% 
      suppressWarnings() %>% 
      quiet()
  # }, warning = function(w) {
  #   message("A warning occurred:", conditionMessage(w), "\n")
  #   return(NULL)
  }, error = function(e) {
    message(paste(
      "Population-level mean", variable, "could not be extracted."))
    message("An error occurred: ", conditionMessage(e), "\n")
    return(NULL)
  })
  if (is.null(out)) return(NULL)
  
  out <- capture.output(
    ctmm::meta(x, 
               variable = variable,
               units = FALSE, 
               verbose = FALSE, 
               plot = FALSE)
  ) %>% suppressMessages() %>% quiet()
  
  i <- 1
  num_row <- 2
  out_mods <- out_logs <- list()
  for (i in seq_len(num_out)) {
    
    if (i == 1) num_row <- 2 
    else num_row <- num_row + 3
    
    mods <- c("inverse-Gaussian", "Dirac-d")
    extract_meta_mods <- function(y) {
      model <- ifelse(
        startsWith(y, "Dirac"), "Dirac-d", "inverse-Gaussian")
      delta_AICc <- unlist(
        stringr::str_extract_all(y, '\\d+([.,]\\d+)?'))
      if (length(delta_AICc) == 0) delta_AICc <- "Inf"
      list("model" = model, "delta_AICc" = as.numeric(delta_AICc))
    }
    
    tmp_out <- extract_meta_mods(out[num_row]) %>% list()
    tmp_out[[2]] <- extract_meta_mods(out[num_row + 1])
    
    out_mods[[i]] <- as.data.frame(do.call(rbind, tmp_out))
    out_logs[[i]] <- ifelse(
      startsWith(out[num_row], "Dirac"),
      FALSE,
      TRUE)
    if (!is_grouped) 
      out_logs[[i]] <- list("subpop_detected" = out_logs[[i]])
    
  } # end of loop [i]
  
  if (num_out == 1) {
    
    return(list(meta = ctmm::meta(x, 
                                  variable = variable,
                                  units = units, 
                                  verbose = verbose, 
                                  plot = plot) %>% quiet(),
                mods = out_mods[[1]],
                logs = out_logs[[1]],
                names = names(x),
                type = type))
    
  } else {
    
    extract_meta_best_mods <- function(y) {
      model <- ifelse(
        startsWith(y, "Sub-population"), 
        "Sub-population", "Joint population")
      delta_AICc <- unlist(
        stringr::str_extract_all(y, '\\d+([.,]\\d+)?'))
      if (length(delta_AICc) == 0) delta_AICc <- "Inf"
      list("model" = model, "delta_AICc" = as.numeric(delta_AICc))
    }
    
    out_mods[[length(out_mods) + 1]] <- data.frame(
      do.call(rbind, list(
        extract_meta_best_mods(out[num_row + 3]),
        extract_meta_best_mods(out[num_row + 3 + 1])
      )))
    
    if (diff(do.call(c, out_mods[[
      length(out_mods)]]$delta_AICc)) <= 2) {
      sub_detected <- FALSE
    } else {
      sub_detected <- ifelse(
        startsWith(
          out_mods[[length(out_mods)]][[1,1]],
          "Sub-population"),
        TRUE, FALSE)
    }
    
    out_logs[[length(out_logs) + 1]] <- ifelse(
      sub_detected,
      TRUE,
      FALSE)
    
    names(out_mods) <- names(out_logs) <- c(
      paste0("var_subpop", 1:num_groups),
      "var_jointpop",
      "subpop_detected")
    
    return(list(meta = ctmm::meta(x, 
                                  variable = variable,
                                  units = units, 
                                  verbose = verbose, 
                                  plot = plot) %>% quiet(),
                mods = out_mods,
                logs = out_logs,
                names = names(x),
                type = type))
  }
}


#' @title Check for Inf speed outputs
#'
#' @keywords internal
#'
#' @noRd
.check_for_inf_speed <- function(ctsd_list) {
  sapply(ctsd_list, function(x) {
    any(x$CI[, "est"] == "Inf")
  })
}


#' @title Compute expected values
#'
#' @keywords internal
#'
#' @noRd
.get_expected_values <- function(set_target = c("hr", "ctsd"),
                                 rv, summarized = TRUE) {
  
  truthList <- lapply(set_target, function(x) {
    out <- switch(
      x,
      "hr" = get_true_hr(
        sigma = rv$sigma,
        emulated = rv$is_emulate,
        fit = if (rv$is_emulate) rv$meanfitList else NULL,
        grouped = rv$grouped,
        groups = if (rv$grouped) rv$groups[[2]] else NULL,
        summarized = summarized),
      "ctsd" = get_true_speed(
        data = rv$simList,
        seed = rv$seedList,
        tau_p = rv$tau_p,
        tau_v = rv$tau_v,
        sigma = rv$sigma,
        emulated = rv$is_emulate,
        fit = if (rv$is_emulate) rv$meanfitList else NULL,
        grouped = rv$grouped,
        groups = if (rv$grouped) rv$groups[[2]] else NULL,
        summarized = summarized))
    return(out)
  })
  
  names(truthList) <- set_target
  return(truthList)
  
}


#' @title Get sequence for meta-analyses
#'
#' @description Get sequence for a iterately larger population sample size.
#' @keywords internal
#'
#' @noRd
.get_sequence <- function(input, grouped = FALSE) {
  if (is.null(input)) stop("ERROR!")
  
  n <- length(input)
  if (grouped) { 
    n <- n / 2
  }
  
  if (n == 1) return(stop("Cannot run meta() on one individual."))
  if (n == 3) return(3)
  if (n == 5) return(c(3, 5))
  
  set_seq <- if (n %% 2 == 0) seq(2, n, by = if (n <= 6) 1 else 2) 
  else seq(1, n, by = 2)[-1 * (n > 5)]
  
  while (length(set_seq) >= 14 && n >= 18) {
    set_seq <- set_seq[seq_along(set_seq) %% 2 == (length(set_seq) %% 2)]
  }
  
  if (length(set_seq) == 0) return(NULL)
  
  if (max(set_seq) != n) 
    stop("Error: max(output) does not equal length(input)")
  
  return(set_seq)
}

#' @title Get sets for meta-analyses
#'
#' @description Get sets for a iterately larger population sample size.
#' @keywords internal
#'
#' @noRd
#'
.get_sets <- function(input, groups = NULL, set_size = 2) {
  
  group_labels <- NULL
  if (!is.null(groups)) {
    nms_group_A <- names(input[groups[["A"]]])
    nms_group_B <- names(input[groups[["B"]]])
    
    group_labels <- ifelse(
      names(input) %in% nms_group_A, "A", 
      ifelse(names(input) %in% nms_group_B, "B", NA))
  }
  
  num_sets <- ceiling(length(input) / set_size)
  if (length(input) %% set_size != 0)
    out <- rep(seq_len(num_sets), each = set_size)[1:length(input)]
  else 
    out <- rep(seq_len(num_sets), each = set_size,
               length.out = length(input))
  
  return(list(names = names(input),
              labels = group_labels,
              out = out,
              out_random = sample(out),
              sets = max(unique(out))))
}

#' @title Extract groups for meta-analyses
#' 
#' @keywords internal
#' 
#' @noRd
.get_groups <- function(input, groups) {
  group_A <- input[groups[["A"]]]
  # group_A[sapply(group_A, is.null)] <- NULL # TODO TOCHECK
  group_B <- input[groups[["B"]]]
  # group_B[sapply(group_B, is.null)] <- NULL # TODO TOCHECK
  return(list(A = group_A,
              B = group_B))
}


#' @title Extract ratios
#' 
#' @keywords internal
#' 
#' @noRd
.get_ratios <- function(x, rev = TRUE) {
  if (is.null(names(x$meta))) stop("No subgroups evaluated.")
  if (length(x$names) != 2) stop("Only two groups accepted.")
  
  if (rev) {
    xy <- c(1, 2)
    nm <- paste0(names(x$meta)[1], "/", names(x$meta)[2])
  } else {
    xy <- c(2, 1)
    nm <- paste0(names(x$meta)[2], "/", names(x$meta)[1])
  }
  
  x <- x$meta$`mean ratio`
  return(data.frame(name = nm,
                    lci = x[xy[1], xy[2], "low"],
                    est = x[xy[1], xy[2], "est"],
                    uci = x[xy[1], xy[2], "high"]))
}

#' @title Extract estimates
#' 
#' @keywords internal
#'
#' @noRd
.get_estimates <- function(object) {
  tmpname <- rownames(object)
  tmpunit <- extract_units(tmpname[grep("^mean", tmpname)])
  
  c("lci" = object[1, 1] %#% tmpunit,
    "est" = object[1, 2] %#% tmpunit,
    "uci" = object[1, 3] %#% tmpunit)
}

#' @title Extract relative errors
#' 
#' @keywords internal
#'
#' @noRd
.get_errors <- function(estimate, truth) {
  (estimate - truth) / truth
}

#' @title Process truth from list
#' 
#' @keywords internal
#'
#' @noRd
.process_truth <- function(truthList,
                           set_target = c("hr", "ctsd"),
                           subpop = FALSE) {
  
  true_estimate <- list()
  true_ratio <- list()
  
  if (target == "hr") {
    true_estimate[[target]] <- truthList[["hr"]][["All"]]$area
    if (subpop) {
      true_estimate[[
        paste0(target, "_A")]] <- truthList[["hr"]][["A"]]$area
      true_estimate[[
        paste0(target, "_B")]] <- truthList[["hr"]][["B"]]$area
      true_ratio[[target]] <- truthList[["hr"]][["A"]]$area / 
        truthList[["hr"]][["B"]]$area
    }
  }
  
  if (target == "ctsd") {
    true_estimate[["ctsd"]] <- truthList[["ctsd"]][["All"]]
    if (subpop) {
      true_estimate[[
        paste0(target, "_A")]] <- truthList[["ctsd"]][["A"]]
      true_estimate[[
        paste0(target, "_B")]] <- truthList[["ctsd"]][["B"]]
      true_ratio[[target]] <- truthList[["ctsd"]][["A"]] /
        truthList[["ctsd"]][["B"]]
    }
  }
  
  return(list(true_estimate = true_estimate,
              true_ratio = true_ratio))
}
