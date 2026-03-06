
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
    
    # x <- lapply(x, function(y) y[sapply(y, is.null)] <- NULL)
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
  
  if (rv$data_type == "simulated") {
    
    truthList <- setNames(lapply(set_target, function(target) {
      
      if (target == "hr") {
        out <- get_true_hr(
          sigma = rv$sigma,
          ind_var = FALSE,
          grouped = rv$grouped,
          groups = if (rv$grouped) rv$groups[[2]] else NULL,
          summarized = summarized) 
        if (!summarized) names(out) <- names(rv$simList)
      }
      
      if (target == "ctsd") {
        out <- get_true_speed(
          tau_p = rv$tau_p,
          tau_v = rv$tau_v,
          sigma = rv$sigma,
          ind_var = FALSE,
          grouped = rv$grouped,
          groups = if (rv$grouped) rv$groups[[2]] else NULL,
          summarized = summarized)
        if (!summarized) names(out) <- names(rv$simList)
      }
      
      return(out)
      
    }), set_target) # end of lapply
    
  } else {
    
    truthList <- setNames(lapply(set_target, function(target) {
      
      if (target == "hr") {
        out <- get_true_hr(
          data = if (!summarized) rv$simList else NULL,
          sigma = rv$sigma,
          ind_var = rv$add_ind_var,
          fit = if (rv$add_ind_var) rv$meanfitList else NULL,
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
          ind_var = rv$add_ind_var,
          fit = if (rv$add_ind_var) rv$meanfitList else NULL,
          grouped = rv$grouped,
          groups = if (rv$grouped) rv$groups[[2]] else NULL,
          summarized = summarized)
        if (!summarized) names(out) <- names(rv$simList)
      }
      
      return(out)
      
    }), set_target) # end of lapply
  }
  
  return(truthList)
  
}


#' @title Generate sequence
#'
#' @description Get sequence for a iterately larger population sample size.
#' @keywords internal
#'
#' @noRd
.get_sequence <- function(input,
                          .step = 2, 
                          .max_m = NULL,
                          .automate_seq = TRUE,
                          grouped = FALSE) {
  
  if (is.null(input)) stop("No input!")
  
  n <- length(input)
  if (!is.null(.max_m)) {
    if (.max_m > n) stop("'.max_m' argument is invalid.")
    n <- .max_m
  }
  
  if (grouped) n <- n / 2
  
  if (n == 1) return(stop("Cannot run meta() on one individual."))
  
  if (.step == 2) {
    start_value <- ifelse(n %% 2 == 0, 2, 1)
  } else {
    start_value <- n %% .step
    if (start_value == 0) start_value <- .step
  }
  
  if (!.automate_seq) return(seq(start_value, n, by = .step))
  
  out_seq <- seq(start_value, n, by = .step)
  if (1 %in% out_seq) {
    out_seq <- out_seq[out_seq != 1]
  }
  
  while (length(out_seq) >= 18) {
    out_seq <- out_seq[
      seq_along(out_seq) %% 2 == (length(out_seq) %% 2)]
  }
  
  if (max(out_seq) != n)
    warning("max(output) does not equal length(input)")
  
  return(out_seq)
}


#' @title Generate index sets
#'
#' @description
#' Creates sequential group assignments and generates combinations of a
#' given size. Enumerates all combinations when feasible; otherwise
#' generates up to `.max_draws` random combinations to limit
#' computational cost.
#' @keywords internal
#' 
#' @importFrom utils combn
#' 
#' @noRd
.get_sets <- function(input,
                      groups = NULL, 
                      set_size = 2,
                      .seed = NULL,
                      .max_draws = 100) {
  
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
  
  if (!is.null(.seed)) set.seed(.seed)
  n_possible <- choose(length(input), set_size)
  n_draws <- min(.max_draws, n_possible)
  batch_size <- ceiling(n_draws * 1.2)
  
  if (n_possible <= 1e7) {
    # Safe to enumerate everything:
    out_random <- t(combn(length(input), set_size))
    
    if (!is.null(.seed)) set.seed(.seed)
    n_randomize <- sample.int(nrow(out_random))
    out_random <- out_random[n_randomize, , drop = FALSE]
    
  } else {
    # Generate bounded number of random unique combinations:
    if (!is.null(.seed)) set.seed(.seed)
    out_random <- t(replicate(
      batch_size, sort(sample.int(
        length(input), set_size, replace = FALSE))))
    out_random <- unique(out_random)
    
    while (nrow(out_random) < n_draws) {
      remaining <- n_draws - nrow(out_random)
      extra_batch <- ceiling(remaining * 1.2)
      
      if (!is.null(.seed)) set.seed(.seed)
      new_draws <- t(replicate(
        extra_batch, sort(sample.int(
          length(input), set_size, replace = FALSE))))
      out_random <- unique(rbind(out_random, new_draws))
    }
    
    out_random <- out_random[seq_len(n_draws), , drop = FALSE]
  }
  
  return(list(names = names(input),
              labels = group_labels,
              out = out,
              out_random = out_random,
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


#' @title Plot meta-analyses outputs
#'
#' @noRd 
#' 
.plot_meta <- function(rv,
                       set_target = c("hr", "ctsd"),
                       randomize = FALSE,
                       replicate = FALSE,
                       subpop = FALSE,
                       colors = NULL,
                       filter_to = NULL) {
  
  stopifnot(!is.null(rv$meta_tbl),
            !is.null(rv$which_m),
            !is.null(rv$which_meta),
            !is.null(rv$which_question),
            !is.null(set_target))
  
  n <- error <- error_sd <- error_mean <- NULL
  type <- top_facet <- NULL
  
  if (length(rv$simList) <= 1)
    stop("simList must have more than one element.")
  if (randomize) {
    stopifnot(!is.null(rv$meta_tbl_resample),
              !is.null(rv$error_threshold))
  }
  if (rv$which_meta == "compare") {
    stopifnot(!is.null(rv$metaList_groups),
              !is.null(rv$metaList[[set_target]]))
  }
  if (replicate) {
    stopifnot(!is.null(rv$error_threshold),
              !is.null(rv$n_replicates))
  }
  
  global_y_range <- c(-Inf, Inf)
  dodge_width <- 0.25
  txt_title <- if (length(rv$which_question) > 1) {
    ifelse(set_target == "hr", 
           "For home range:", "For speed & distance:")
  }
  
  if (is.null(colors)) { pal_values <- c(
    "Yes" = "#009da0", "No" = "#A12C3B")
  } else { pal_values <- c(
    "Yes" = colors[[1]], "No" = colors[[2]])
  }
  
  if (!is.null(filter_to)) {
    if (filter_to == 1) {
      replicate <- FALSE
      randomize <- FALSE
    }
  }
  
  if (replicate) {
    
    if (is.null(rv$meta_tbl_replicates)) {
      out <- rv$meta_tbl %>%
        dplyr::filter(.data$type == set_target)
    } else {
      out <- rv$meta_tbl_replicates %>%
        dplyr::filter(.data$type == set_target)
    }
    
    pal_values <- list("TRUE" = pal_values[[1]],
                       "FALSE" = pal_values[[2]])
    
    if (subpop) {
      out <- out[out$group != "All", ]
      set_shapes <- c(21, 24)
      set_shapes_manual <- c(16, 17)
    } else {
      out <- out[out$group == "All", ]
      set_shapes <- c(21, 21)
      set_shapes_manual <- c(16, 16)
    }
    
    out <- out %>%
      dplyr::mutate(
        overlaps = dplyr::between(
          abs(.data$error), 0, rv$error_threshold))
    
    out_mean <- out %>%
      dplyr::mutate(
        dplyr::across(
          "type", ~factor(., levels = c("hr", "ctsd")))) %>%
      dplyr::select(
        "type", "m", "group",
        "error", "error_lci", "error_uci") %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$type, .data$group, .data$m) %>% 
      dplyr::summarize(
        n = dplyr::n(),
        error_sd = stats::sd(.data$error, na.rm = TRUE),
        error_mean = mean(.data$error, na.rm = TRUE),
        error_mean_lci = error_mean - stats::qt(
          0.975, df = n - 1) * error_sd / sqrt(n),
        error_mean_uci = error_mean + stats::qt(
          0.975, df = n - 1) * error_sd / sqrt(n),
        pred_lci = error_mean - stats::qt(
          0.975, df = n - 1) * error_sd * sqrt(1 + 1 / n),
        pred_uci = error_mean + stats::qt(
          0.975, df = n - 1) * error_sd * sqrt(1 + 1 / n),
        .groups = "drop") %>%
      dplyr::mutate(
        overlaps = dplyr::between(
          abs(.data$error_mean), 0, rv$error_threshold),
        overlaps = factor(.data$overlaps,
                          levels = c("TRUE", "FALSE")),
        top_facet = .data$type == "hr") %>%
      dplyr::ungroup()
    
    max_m <- max(unique(out$m))
    set_target <- unique(out$type)
    
    p.optimal <- out_mean %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = if (subpop && length(unique(out$m)) == 1) {
            as.factor(.data$m) 
          } else { 
            .data$m 
          },
          y = .data$error_mean,
          group = .data$group,
          shape = .data$group,
          fill = .data$overlaps)) +
      
      { if (length(set_target) > 1)
        ggplot2::facet_grid(
          cols = ggplot2::vars(type),
          scales = "free",
          labeller = ggplot2::labeller(
            type = c(
              "hr" = "Home range estimation", 
              "ctsd" = "Speed \u0026 distance estimation"))) } +
      
      ggplot2::geom_jitter(
        out,
        mapping = ggplot2::aes(
          x = if (subpop && length(unique(out$m)) == 1) {
            as.factor(.data$m) 
          } else { 
            .data$m 
          },
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
        data = subset(out_mean, top_facet),
        ggplot2::aes(yintercept = rv$error_threshold),
        linewidth = 0.7,
        linetype = "dotted") +
      ggplot2::geom_hline(
        data = subset(out_mean, top_facet),
        ggplot2::aes(yintercept = -rv$error_threshold),
        linewidth = 0.7,
        linetype = "dotted") +
      
      ggplot2::geom_hline(
        data = subset(out_mean, !top_facet),
        ggplot2::aes(yintercept = rv$error_threshold),
        linewidth = 0.7,
        linetype = "dotted") +
      ggplot2::geom_hline(
        data = subset(out_mean, !top_facet),
        ggplot2::aes(yintercept = -rv$error_threshold),
        linewidth = 0.7,
        linetype = "dotted") +
      
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$error_mean_lci,
                     ymax = .data$error_mean_uci),
        show.legend = TRUE,
        position = ggplot2::position_dodge(width = 0.4),
        color = "black", linewidth = 1) +
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$pred_lci,
                     ymax = .data$pred_uci),
        show.legend = TRUE,
        position = ggplot2::position_dodge(width = 0.4),
        color = "black", linewidth = 0.4) +
      
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.4),
        stroke = 1.05, size = 5) +
      
      ggplot2::labs(
        title = txt_title,
        x = "Population sample size",
        y = "Relative error (%)") +
      
      ggplot2::scale_fill_manual(
        name = paste0("Within error threshold (\u00B1",
                      rv$error_threshold * 100, "%)?"),
        breaks = c("TRUE", "FALSE"),
        values = pal_values, drop = FALSE,
        guide = ggplot2::guide_legend(
          override.aes = list(color = pal_values,
                              fill = pal_values, size = 3),
          order = 1,
          label.vjust = 0.4)) +
      
      { if (!subpop) {
        ggplot2::scale_shape_manual(
          name = "Groups:",
          values = set_shapes)
      } else {
        ggplot2::scale_shape_manual(
          name = "Groups:",
          values = set_shapes,
          guide = ggplot2::guide_legend(
            override.aes = list(shape = set_shapes_manual, size = 3)))
      }
      } +
      
      { if (length(unique(out$m)) == 2) {
        ggplot2::scale_x_continuous(
          breaks = unique(out$m))
      } else if (length(unique(out$m)) > 1) {
        ggplot2::scale_x_continuous(
          breaks = scales::breaks_pretty())
      }
      } +
      
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        breaks = scales::breaks_pretty()) +
      
      ggplot2::theme_classic() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 13),
        legend.position = "bottom",
        strip.text = ggplot2::element_text(size = 16),
        strip.background.x = ggplot2::element_rect(color = NA, fill = NA),
        strip.background.y = ggplot2::element_rect(color = NA, fill = NA),
        plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"))
    
    if (!subpop) {
      p.optimal <- p.optimal + ggplot2::guides(shape = "none")
    }
    
  } else if (randomize) {
    req(rv$meta_nresample > 1)
    
    if (!is.null(rv$meta_nresample)) {
      out <- dplyr::filter(rv$meta_tbl_resample,
                           .data$sample <= rv$meta_nresample)
    } else { out <- rv$meta_tbl_resample }
    
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
        error_sd = stats::sd(.data$error, na.rm = TRUE),
        error = mean(.data$error, na.rm = TRUE),
        error_lci = error - stats::qt(
          0.975, df = n - 1) * error_sd / sqrt(n),
        error_uci = error + stats::qt(
          0.975, df = n - 1) * error_sd / sqrt(n)) %>%
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
    
    if (rv$which_meta == "mean") {
      p.optimal <- p.optimal +
        ggplot2::guides(shape = "none")
    }
    
  } else {
    
    if (is.null(rv$meta_tbl_replicates)) {
      out <- rv$meta_tbl
    } else {
      out <- rv$meta_tbl_replicates %>%
        dplyr::filter(replicate <= filter_to) %>%
        dplyr::mutate(
          overlaps = dplyr::between(
            abs(.data$error), 0, rv$error_threshold))
    }
    
    out <- out_all <- dplyr::distinct(out) %>% 
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
    
  }
  
  return(p.optimal)
  
}


#' @title Process replicates
#'
#' @noRd 
#' 
.process_replicates <- function(rv,
                                out_replicate,
                                start = Sys.time()) {
  
  n <- m <- NULL
  groups <- NULL
  
  rv$metaEst <- NULL
  rv$metaErr <- NULL
  rv$metaEst_groups <- NULL
  rv$metaErr_groups <- NULL
  
  data <- out_replicate$data
  if (rv$grouped) groups <- out_replicate$data$groups
  
  est <- error_sd <- NULL
  tmpsumm <- out_replicate$summary %>%
    dplyr::select(
      "type", "m", "group",
      "error", "error_lci", "error_uci") %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$type, .data$group) %>%
    dplyr::filter(m == max(.data$m)) %>%
    dplyr::summarize(
      n = dplyr::n(),
      error_sd = stats::sd(.data$error, na.rm = TRUE),
      est = mean(.data$error, na.rm = TRUE),
      lci = est - stats::qt(0.975, df = n - 1) * error_sd / sqrt(n),
      uci = est + stats::qt(0.975, df = n - 1) * error_sd / sqrt(n),
      .groups = "drop") %>%
    dplyr::ungroup()
  
  rv$metaErr <- tmpsumm %>%
    dplyr::filter(group == "All") %>%
    dplyr::select("type", "group", "lci", "est", "uci")
  
  if (rv$grouped) {
    rv$metaErr_groups <- tmpsumm %>%
      dplyr::filter(group != "All") %>%
      dplyr::select("type", "group", "lci", "est", "uci")
  }
  
  seedList <- out_replicate$data$seedList
  simList <- out_replicate$data$simList
  simfitList <- out_replicate$data$simfitList
  
  rv$seedList_replicates <- c(rv$seedList_replicates, seedList)
  
  .get_group <- function(seed, groups) {
    if (as.character(seed) %in% groups[["A"]]) "A" else "B"
  }
  
  for (i in seq_along(simfitList)) {
    
    group <- 1
    if (rv$grouped) {
      group <- .get_group(seedList[[i]], groups[[2]])
    }
    
    if (rv$add_ind_var) {
      tau_p <- extract_pars(
        emulate_seeded(rv$meanfitList[[group]], seedList[[i]]),
        "position")[[1]]
      tau_v <- extract_pars(
        emulate_seeded(rv$meanfitList[[group]], seedList[[i]]),
        "velocity")[[1]]
      sigma <- extract_pars(
        emulate_seeded(rv$meanfitList[[group]], seedList[[i]]),
        "sigma")[[1]]
    } else {
      tau_p <- rv$tau_p[[group]]
      tau_v <- rv$tau_v[[group]]
      sigma <- rv$sigma[[group]]
    }
    
    rv$dev$tbl <- rbind(
      rv$dev$tbl,
      .build_tbl(
        device = rv$device_type,
        group = if (rv$grouped) group else NA,
        data = simList[[i]],
        seed = seedList[[i]],
        obj = simfitList[[i]],
        tau_p = tau_p,
        tau_v = tau_v,
        sigma = sigma
      )
    )
  }
  
  if ("hr" %in% rv$set_target) {
    
    akdeList <- out_replicate$data$akdeList
    truthList <- get_true_hr(
      data = simList,
      seed = seedList,
      sigma = rv$sigma,
      
      ind_var = rv$add_ind_var,
      fit = if (rv$add_ind_var) rv$meanfitList else NULL,
      
      grouped = rv$grouped,
      groups = if (rv$grouped) groups[[2]] else NULL)
    
    out_est_df <- data.frame(seed = numeric(0),
                             lci = numeric(0),
                             est = numeric(0),
                             uci = numeric(0),
                             unit = character(0))
    out_err_df <- data.frame(seed = numeric(0),
                             lci = numeric(0),
                             est = numeric(0),
                             uci = numeric(0))
    
    for (i in seq_along(akdeList)) {
      
      group <- 1
      if (rv$grouped) {
        nm <- names(simList)[[i]]
        group <- ifelse(nm %in% groups[[2]]$A, "A", "B")
      }
      
      if (rv$add_ind_var) {
        tau_p <- extract_pars(
          emulate_seeded(rv$meanfitList[[group]],
                         seedList[[i]]),
          "position")[[1]]
        tau_v <- extract_pars(
          emulate_seeded(rv$meanfitList[[group]],
                         seedList[[i]]),
          "velocity")[[1]]
        sigma <- extract_pars(
          emulate_seeded(rv$meanfitList[[group]],
                         seedList[[i]]),
          "sigma")[[1]]
      } else {
        tau_p <- rv$tau_p[[group]]
        tau_v <- rv$tau_v[[group]]
        sigma <- rv$sigma[[group]]
      }
      
      seed <- as.character(seedList[[i]])
      hr_truth <- truthList[[seed]]$area
      N1 <- extract_dof(simfitList[[i]], "area")[[1]]
      
      tmpsum <- tryCatch(
        summary(akdeList[[i]]),
        error = function(e) e)
      
      if (is.null(akdeList[[i]]) ||
          is.null(tmpsum) || length(tmpsum) == 0 ||
          any(tmpsum[[1]] == 0) ||
          inherits(tmpsum, "error") || N1 < 0.001) {
        
        out_est_df <- out_est_df %>%
          dplyr::add_row(
            seed = seedList[[i]],
            lci = NA, est = NA, uci = NA, unit = NA)
        out_err_df <- out_err_df %>%
          dplyr::add_row(
            seed = seedList[[i]],
            lci = NA, est = NA, uci = NA)
        
        rv$hr$tbl <- rbind(
          rv$hr$tbl,
          .build_tbl(
            target = "hr",
            group = if (rv$grouped) group else NA,
            data = simList[[i]],
            seed = names(simList)[[i]],
            obj = akdeList[[i]],
            tau_p = tau_p,
            tau_v = tau_v,
            sigma = sigma,
            area = out_est_df[i, ],
            area_error = out_err_df[i, ]))
        next
      }
      
      tmpname <- rownames(summary(akdeList[[i]])$CI)
      tmpunit <- extract_units(tmpname[grep('^area', tmpname)])
      
      out_est_df <- out_est_df %>%
        dplyr::add_row(
          seed = seedList[[i]],
          lci = tmpsum$CI[1],
          est = tmpsum$CI[2],
          uci = tmpsum$CI[3],
          unit = tmpunit)
      out_err_df <- out_err_df %>%
        dplyr::add_row(
          seed = seedList[[i]],
          lci = ((tmpsum$CI[1] %#% tmpunit) - hr_truth) / hr_truth,
          est = ((tmpsum$CI[2] %#% tmpunit) - hr_truth) / hr_truth,
          uci = ((tmpsum$CI[3] %#% tmpunit) - hr_truth) / hr_truth)
      
      rv$hr$tbl <- rbind(
        rv$hr$tbl,
        .build_tbl(
          target = "hr",
          group = if (rv$grouped) group else NA,
          data = simList[[i]],
          seed = seedList[[i]],
          obj = akdeList[[i]],
          tau_p = tau_p,
          tau_v = tau_v,
          sigma = sigma,
          area = out_est_df[i, ],
          area_error = out_err_df[i, ]))
    }
    
    rv$hrEst <- rbind(rv$hrEst, out_est_df)
    rv$hrErr <- rbind(rv$hrErr, out_err_df)
    
    rv$hr_completed <- TRUE
  }
  
  if ("ctsd" %in% rv$set_target) {
    
    rv$sd_completed <- TRUE
    ctsdList <- out_replicate$data$ctsdList
    
    truthList <- get_true_speed(
      data = simList,
      seed = seedList,
      
      tau_p = rv$tau_p,
      tau_v = rv$tau_v,
      sigma = rv$sigma,
      
      ind_var = rv$add_ind_var,
      fit = if (rv$add_ind_var) rv$meanfitList else NULL,
      
      grouped = rv$grouped,
      groups = if (rv$grouped) groups[[2]] else NULL)
    
    out_est_df <- data.frame(seed = numeric(0),
                             lci = numeric(0),
                             est = numeric(0),
                             uci = numeric(0),
                             unit = character(0))
    out_err_df <- data.frame(seed = numeric(0),
                             lci = numeric(0),
                             est = numeric(0),
                             uci = numeric(0))
    
    for (i in seq_along(ctsdList)) {
      
      sdList <- ctsdList[[i]]
      
      if (is.null(sdList)) {
        out_est_df <- out_est_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = NA, est = NA, uci = NA, unit = NA)
        out_err_df <- out_err_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = NA, est = NA, uci = NA)
        next
      }
      
      if ("CI" %in% names(sdList)) sdList <- sdList$CI
      to_check <- sdList[1, "est"]
      
      if (is.infinite(to_check)) {
        out_est_df <- out_est_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = NA, est = NA, uci = NA, unit = NA)
        out_err_df <- out_err_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = NA, est = NA, uci = NA)
        next
      }
      
      tmpname <- rownames(sdList)
      tmpunit <- extract_units(tmpname[grep("speed", tmpname)])
      
      group <- 1
      if (rv$grouped) {
        nm <- names(simList)[[i]]
        group <- ifelse(nm %in% groups[[2]]$A, "A", "B")
      }
      
      seed <- as.character(seedList[[i]])
      sd_truth <- truthList[[seed]]
      
      out_est_df <- out_est_df %>%
        dplyr::add_row(seed = seedList[[i]],
                       lci = sdList[1],
                       est = sdList[2],
                       uci = sdList[3],
                       unit = tmpunit)
      
      out_err_df <- out_err_df %>%
        dplyr::add_row(
          seed = seedList[[i]],
          lci = ((sdList[[1]] %#% tmpunit) - sd_truth) / sd_truth,
          est = ((sdList[[2]] %#% tmpunit) - sd_truth) / sd_truth,
          uci = ((sdList[[3]] %#% tmpunit) - sd_truth) / sd_truth)
    }
    
    rv$speedEst <- rbind(rv$speedEst, out_est_df)
    rv$speedErr <- rbind(rv$speedErr, out_err_df)
    
    out_dist_est_df <- data.frame(seed = numeric(0),
                                  lci = numeric(0),
                                  est = numeric(0),
                                  uci = numeric(0),
                                  unit = character(0))
    out_dist_err_df <- data.frame(seed = numeric(0),
                                  lci = numeric(0),
                                  est = numeric(0),
                                  uci = numeric(0))
    
    dur_days <- "days" %#% rv$dur$value %#% rv$dur$unit
    unit_new <- "kilometers/day"
    
    pathList <- list()
    for (i in seq_along(ctsdList)) {
      
      sdList <- ctsdList[[i]]
      pathList[[i]] <- estimate_trajectory(
        data = simList[i],
        fit = simfitList[i],
        groups = if (rv$grouped) groups[[2]] else NULL,
        dur = rv$dur,
        tau_v = rv$tau_v,
        seed = seedList[i])[[1]]
      
      if (is.null(sdList) ||
          is.null(pathList[[i]])) {
        out_dist_est_df <- out_dist_est_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = NA, est = NA, uci = NA, unit = NA)
        out_dist_err_df <- out_dist_err_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = NA, est = NA, uci = NA)
        next
      }
      
      truth <- sum(pathList[[i]]$dist, na.rm = TRUE)
      unit_old <- rv$speedEst$unit[i]
      
      if (!is.na(rv$speedEst$est[i])) {
        
        dist_lci <- (unit_new %#% rv$speedEst$lci[i]
                     %#% unit_old) * dur_days
        dist_est <- (unit_new %#% rv$speedEst$est[i]
                     %#% unit_old) * dur_days
        dist_uci <- (unit_new %#% rv$speedEst$uci[i]
                     %#% unit_old) * dur_days
        
        dist_unit <- "kilometers"
        truth <- dist_unit %#% truth
        
        out_dist_est_df <- out_dist_est_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = dist_lci,
                         est = dist_est,
                         uci = dist_uci,
                         unit = dist_unit)
        
        out_dist_err_df <- out_dist_err_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = (dist_lci - truth) / truth,
                         est = (dist_est - truth) / truth,
                         uci = (dist_uci - truth) / truth)
      } else {
        out_dist_est_df <- out_dist_est_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = NA, est = NA, uci = NA, unit = NA)
        out_dist_err_df <- out_dist_err_df %>%
          dplyr::add_row(seed = seedList[[i]],
                         lci = NA, est = NA, uci = NA)
      }
    }
    
    rv$distEst <- rbind(rv$distEst, out_dist_est_df)
    rv$distErr <- rbind(rv$distErr, out_dist_err_df)
    
    for (i in seq_along(ctsdList)) {
      
      group <- 1
      if (rv$grouped) {
        group <- ifelse(
          names(simList)[[i]] %in% groups[[2]]$A,
          "A", "B")
      }
      
      if (rv$add_ind_var) {
        tau_p <- extract_pars(
          emulate_seeded(rv$meanfitList[[group]],
                         seedList[[i]]),
          "position")[[1]]
        tau_v <- extract_pars(
          emulate_seeded(rv$meanfitList[[group]],
                         seedList[[i]]),
          "velocity")[[1]]
        sigma <- extract_pars(
          emulate_seeded(rv$meanfitList[[group]],
                         seedList[[i]]),
          "sigma")[[1]]
      } else {
        tau_p <- rv$tau_p[[group]]
        tau_v <- rv$tau_v[[group]]
        sigma <- rv$sigma[[group]]
      }
      
      rv$sd$tbl <- rbind(
        rv$sd$tbl,
        .build_tbl(
          target = "ctsd",
          group = if (rv$grouped) group else NA,
          data = simList[[i]],
          seed = names(simList)[[i]],
          obj = ctsdList[[i]],
          tau_p = tau_p,
          tau_v = tau_v,
          sigma = sigma,
          speed = rv$speedEst[i, ],
          speed_error = rv$speedErr[i, ],
          distance = rv$distEst[i, ],
          distance_error = rv$distErr[i, ]))
    }
    
    rv$sd_completed <- TRUE
  }
  
  rv$dev$tbl <- dplyr::distinct(rv$dev$tbl)
  if ("hr" %in% rv$set_target)
    rv$hr$tbl <- dplyr::distinct(rv$hr$tbl)
  if ("ctsd" %in% rv$set_target)
    rv$sd$tbl <- dplyr::distinct(rv$sd$tbl)
  
  return(out_replicate$summary)
  
}


