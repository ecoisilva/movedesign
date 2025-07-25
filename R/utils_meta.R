
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
    
    # x <- lapply(x, fuunction(y) y[sapply(y, is.null)] <- NULL)
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
.get_expected_values <- function(rv,
                                 set_target = c("hr", "ctsd"),
                                 summarized = TRUE) {
  
  truthList <- setNames(lapply(set_target, function(target) {
    
    if (target == "hr") {
      out <- get_true_hr(
        data = if (!summarized) rv$simList else NULL,
        sigma = rv$sigma,
        emulated = rv$is_emulate,
        fit = if (rv$is_emulate) rv$meanfitList else NULL,
        grouped = rv$grouped,
        groups = if (rv$grouped) rv$groups[[2]] else NULL,
        summarized = summarized) 
      if (!summarized) names(out) <- names(rv$simList)
    }
    
    if (target == "ctsd") {
      out <- get_true_speed(
        data = if (!summarized) rv$simList else NULL,
        tau_p = rv$tau_p,
        tau_v = rv$tau_v,
        sigma = rv$sigma,
        emulated = rv$is_emulate,
        fit = if (rv$is_emulate) rv$meanfitList else NULL,
        grouped = rv$grouped,
        groups = if (rv$grouped) rv$groups[[2]] else NULL,
        summarized = summarized)
      if (!summarized) names(out) <- names(rv$simList)
    }
    
    return(out)
    
  }), set_target) # end of lapply
  
  return(truthList)
  
}


#' @title Get sequence for meta-analyses
#'
#' @description Get sequence for a iterately larger population sample size.
#' @keywords internal
#'
#' @noRd
.get_sequence <- function(input, .iter_step = 2, grouped = FALSE) {
  
  if (is.null(input)) stop("No input!")
  
  n <- length(input)
  if (grouped) n <- n / 2
  
  if (n == 1) return(stop("Cannot run meta() on one individual."))
  
  if (.iter_step == 2) {
    start_value <- ifelse(n %% 2 == 0, 2, 1)
  } else {
    start_value <- n %% .iter_step
    if (start_value == 0) start_value <- .iter_step
  }
  
  out_seq <- seq(start_value, n, by = .iter_step)
  if (grouped && 1 %in% out_seq) {
    out_seq <- out_seq[out_seq != 1]
  }
  
  while (length(out_seq) >= 18) {
    out_seq <- out_seq[
      seq_along(out_seq) %% 2 == (length(out_seq) %% 2)]
  }
  
  if (max(out_seq) != n) 
    stop("Error: max(output) does not equal length(input)")
  
  return(out_seq)
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
    out <- rep(seq_len(num_sets), each = set_size)[seq_along(input)]
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
  group_B <- input[groups[["B"]]]
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
  
  if (set_target == "hr") {
    true_estimate[[set_target]] <- truthList[["hr"]][["All"]]$area
    if (subpop) {
      true_estimate[[
        paste0(set_target, "_A")]] <- truthList[["hr"]][["A"]]$area
      true_estimate[[
        paste0(set_target, "_B")]] <- truthList[["hr"]][["B"]]$area
      true_ratio[[set_target]] <- truthList[["hr"]][["A"]]$area / 
        truthList[["hr"]][["B"]]$area
    }
  }
  
  if (set_target == "ctsd") {
    true_estimate[["ctsd"]] <- truthList[["ctsd"]][["All"]]
    if (subpop) {
      true_estimate[[
        paste0(set_target, "_A")]] <- truthList[["ctsd"]][["A"]]
      true_estimate[[
        paste0(set_target, "_B")]] <- truthList[["ctsd"]][["B"]]
      true_ratio[[set_target]] <- truthList[["ctsd"]][["A"]] /
        truthList[["ctsd"]][["B"]]
    }
  }
  
  return(list(true_estimate = true_estimate,
              true_ratio = true_ratio))
}


#' @title Plot meta (resamples)
#'
#' @noRd 
#' 
plot_meta_resamples <- function(rv,
                                set_target = c("hr", "ctsd"),
                                random = FALSE, 
                                subpop = FALSE, 
                                colors = NULL) {
  
  stopifnot(!is.null(rv$meta_tbl),
            !is.null(rv$which_m),
            !is.null(rv$which_meta),
            !is.null(rv$which_question),
            !is.null(set_target))
  if (length(rv$simList) <= 1)
    stop("simList must have more than one element.")
  if (random) {
    stopifnot(!is.null(rv$meta_tbl_resample),
              !is.null(rv$error_threshold))
  }
  if (rv$which_meta == "compare") {
    stopifnot(!is.null(rv$metaList_groups),
              !is.null(rv$metaList[[set_target]]))
  }
  
  dodge_width <- 0.25
  txt_title <- if (length(rv$which_question) > 1) {
    ifelse(set_target == "hr", 
           "For home range:", "For speed & distance:")
  }
  
  if (is.null(colors)) { pal_values <- c(
    "Yes" = "#009da0", "Near" = "#77b131", "No" = "#dd4b39")
  } else pal_values <- c(
    "Yes" = colors[[1]], "Near" = colors[[2]], "No" = colors[[3]])
  
  if (random) {
    
    if (!is.null(rv$meta_nresample))
      out <- dplyr::filter(rv$meta_tbl_resample,
                           .data$sample <= rv$meta_nresample)
    else out <- rv$meta_tbl_resample
    
    out <- out %>% 
      dplyr::mutate(m = as.integer(.data$m)) %>% 
      dplyr::filter(.data$type == set_target)
    if (subpop) out <- dplyr::filter(out, .data$group != "All")
    
    req(nrow(out) > 0)
    stopifnot(all(!is.na(out$est)), nrow(out) > 0)
    
    max_draws <- max(unique(out$sample))
    
    out_mean <- out %>% 
      dplyr::group_by(.data$type, .data$group, .data$m) %>% 
      dplyr::summarize(
        n = dplyr::n(),
        error = mean(.data$error, na.rm = TRUE),
        error_lci = mean(.data$error_lci, na.rm = TRUE),
        error_uci = mean(.data$error_uci, na.rm = TRUE)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        within_threshold = 
          (.data$error >= -rv$error_threshold &
             .data$error <= rv$error_threshold),
        overlaps_with_threshold = 
          (.data$error_lci <= rv$error_threshold & 
             .data$error_uci >= -rv$error_threshold),
        status = dplyr::case_when(
          within_threshold ~ "Yes",
          !within_threshold & overlaps_with_threshold ~ "Near",
          TRUE ~ "No")) %>% 
      quiet() %>% 
      suppressMessages() %>% 
      suppressWarnings()
    
    txt_color <- paste0(
      "Within error threshold (\u00B1",
      rv$error_threshold * 100, "%)?")
    
    plot_subtitle <- paste(
      "<b>Maximum number of samples:</b>", max_draws)
    
    p.optimal <- out_mean %>% 
      ggplot2::ggplot(
        ggplot2::aes(x = as.factor(.data$m),
                     y = .data$error,
                     group = .data$group,
                     shape = .data$group,
                     color = .data$status)) + 
      
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.3,
        linetype = "solid") +
      
      ggplot2::geom_hline(
        yintercept = rv$error_threshold,
        color = "black",
        linetype = "dotted") +
      ggplot2::geom_hline(
        yintercept = -rv$error_threshold,
        color = "black",
        linetype = "dotted") +
      
      ggplot2::geom_jitter(
        data = out,
        mapping = ggplot2::aes(x = as.factor(.data$m),
                               y = .data$error,
                               group = .data$group,
                               shape = .data$group,
                               color = .data$status),
        position = ggplot2::position_jitterdodge(dodge.width = 0.4),
        size = 3.5, color = "grey80", alpha = 0.9) +
      
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$error_lci,
                     ymax = .data$error_uci),
        show.legend = TRUE,
        position = ggplot2::position_dodge(width = 0.4),
        linewidth = 2.2, alpha = 0.3) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.4),
        show.legend = TRUE,
        size = 3.5) +
      
      ggplot2::labs(
        title = txt_title,
        subtitle = plot_subtitle,
        x = "<i>Population</i> sample size, <i>m</i>",
        y = "Relative error (%)",
        color = txt_color) +
      
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(),
                                  labels = scales::percent) +
      
      ggplot2::scale_color_manual(values = pal_values,
                                  na.translate = FALSE, drop = FALSE) +
      ggplot2::scale_shape_manual("Groups:", values = c(16, 18)) +
      
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggtext::element_markdown(
          size = 15, face = 2, hjust = 1,
          margin = ggplot2::margin(b = 2)),
        plot.subtitle = ggtext::element_markdown(
          size = 14, hjust = 1, margin = ggplot2::margin(b = 15)))
    p.optimal
    
    if (rv$which_meta == "mean") {
      p.optimal <- p.optimal +
        ggplot2::guides(shape = "none")
    }
    
  } else {
    
    out <- out_all <- dplyr::distinct(rv$meta_tbl) %>% 
      dplyr::select(-c(.data$est, .data$lci, .data$uci)) %>%
      dplyr::filter(.data$type == set_target)
    
    stopifnot(all(!is.na(out$est)), nrow(out) > 0)
    
    if (subpop) subpop_detected <- NULL
    if (subpop) out <- dplyr::filter(out, .data$group != "All")
    
    stopifnot(all(!is.na(out$est)), nrow(out) > 0)
    
    txt_color <- paste0(
      "Within error threshold (\u00B1",
      rv$error_threshold * 100, "%)?")
    
    out <- out %>%
      dplyr::group_by(.data$type) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(
        within_threshold = 
          (.data$error >= -rv$error_threshold & 
             .data$error <= rv$error_threshold),
        overlaps_with_threshold = 
          (.data$error_lci <= rv$error_threshold & 
             .data$error_uci >= -rv$error_threshold),
        color = dplyr::case_when(
          within_threshold ~ "Yes",
          !within_threshold & overlaps_with_threshold ~ "Near",
          TRUE ~ "No"))
    
    txt_caption <- NULL
    txt_color <- paste0(
      "Within error threshold (\u00B1", rv$error_threshold * 100, "%)?")
    
    if (rv$which_meta == "compare") {
      dodge_width <- .4
      txt_color <- "Groups:"
      
      is_subpop <- rv$metaList_groups[["intro"]][[
        set_target]]$logs$subpop_detected
      
      is_final_subpop <- out_all %>%
        dplyr::filter(.data$group == "All") %>%
        dplyr::pull(.data$subpop_detected)
      is_final_subpop <- rep(is_final_subpop, each = 2)
      is_final_subpop <- out$is_final_subpop <- ifelse(
        is_final_subpop == "FALSE", "No", "Yes")
      out$color <- out$group
      
      pal_values <- c("A" = "#77b131", "B" = "#009da0")
      
      txt_color <- "Groups:"
      txt_caption <- "(*) Asterisks indicate subpopulations were found."
      
    } # Note: refers to finding subpops within the population.
    
    p.optimal <- out %>%
      ggplot2::ggplot(
        ggplot2::aes(x = as.factor(.data$m),
                     y = .data$error,
                     group = .data$group,
                     shape = .data$group,
                     color = .data$color)) +
      
      ggplot2::geom_hline(
        yintercept = rv$error_threshold,
        color = "black",
        linetype = "dotted") +
      ggplot2::geom_hline(
        yintercept = -rv$error_threshold,
        color = "black",
        linetype = "dotted") +
      
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.3,
        linetype = "solid") +
      ggplot2::geom_point(
        size = 4,
        position = ggplot2::position_dodge(width = dodge_width)) +
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$error_lci,
                     ymax = .data$error_uci),
        position = ggplot2::position_dodge(width = dodge_width)) +
      
      { if (rv$which_meta == "compare")
        ggplot2::geom_text(
          data = subset(out, subpop_detected == TRUE),
          mapping = ggplot2::aes(x = as.factor(.data$m),
                                 y = .data$error_uci + 0.05,
                                 label = "*"),
          color = "black", size = 5, 
          position = ggplot2::position_dodge(width = 0.4))
        
      } +
      
      ggplot2::labs(
        title = txt_title,
        x = "<i>Population</i> sample size, <i>m</i>",
        y = "Relative error (%)",
        color = txt_color,
        caption = txt_caption) +
      
      ggplot2::scale_y_continuous(labels = scales::percent,
                                  breaks = scales::breaks_pretty()) +
      ggplot2::scale_color_manual(txt_color, values = pal_values) +
      ggplot2::scale_shape_manual("Groups:", values = c(16, 18)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    
    if (rv$which_meta == "mean") {
      p.optimal <- p.optimal +
        ggplot2::guides(shape = "none")
    }
    
  } # end of (!random)
  
  return(p.optimal)
  
}
