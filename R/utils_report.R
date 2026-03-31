
#' @title Format error as rounded percentage string
#' @noRd
.err_to_txt <- function(x, digits = 1) { 
  if (length(x) > 1) return(round(mean(x, na.rm = TRUE) * 100, digits))
  else return(round(x * 100, digits))
}

#' @title Get credible intervals
#' @noRd
.extract_cri <- function(x, ci = 0.95, method = "HDI") {
  
  out <- tryCatch(
    bayestestR::ci(x, ci = ci, method = method),
    error = function(e) e)
  
  if (!inherits(out, "error")) {
    out <- data.frame(
      "lci" = ifelse(is.null(out$CI_low), NA, out$CI_low),
      "est" = mean(x, na.rm = TRUE),
      "uci" = ifelse(is.null(out$CI_high), NA, out$CI_high),
      "ci" = ci)
  } else {
    out <- data.frame(
      "lci" = NA,
      "est" = mean(x, na.rm = TRUE),
      "uci" = NA,
      "ci" = ci)
    
  }
  
  return(out)
}


#' @title Get text for mean errors
#' 
#' @noRd
.format_mean_error <- function(rv,
                               set_target = c("hr", "ctsd")) {
  
  set_target <- match.arg(set_target)
  err_name <- paste0(set_target, "Err")
  
  # Determine number of tags
  n_tags <- if (rv$which_m == "get_all") {
    rv$n_units
  } else if (rv$which_m == "get_m") {
    if (is.null(rv$n_tags_current)) rv$n_sims else rv$n_tags_current
  } else {
    length(rv$simList)
  }
  
  txt_n_tags <- if (n_tags == 1) {
    "a single individual"
  } else {
    paste(n_tags, "individuals")
  }
  
  if (rv$which_meta == "none") {
    return(paste0(
      "Your mean error estimate based on ", txt_n_tags, " was ",
      .err_to_txt(rv[[err_name]]$est), "%."))
  }
  
  meta <- if (is.null(rv$meta_tbl_replicates)) {
    rv$meta_tbl
  } else {
    rv$meta_tbl_replicates
  }
  
  txt_replicates <- if (!is.null(rv$meta_tbl_replicates)) {
    paste(" and", rv$n_replicates, "replicates")
  } else ""
  
  summarize_meta <- function(sub_meta) {
    n <- nrow(sub_meta)
    error <- mean(sub_meta$error, na.rm = TRUE)
    sd_error <- stats::sd(sub_meta$error, na.rm = TRUE)
    se <- sd_error / sqrt(n)
    data.frame(
      m = sub_meta$m[1],
      error = error,
      error_lci = error - stats::qt(0.975, df = n - 1) * se,
      error_uci = error + stats::qt(0.975, df = n - 1) * se)
  }
  
  summarize_by_max_m <- function(sub_meta) {
    if (rv$which_m != "get_all" && is.null(rv$meta_tbl_replicates)) {
      sub_meta[which.max(sub_meta$m), , drop = FALSE]
    } else {
      summaries <- lapply(split(sub_meta, sub_meta$m), summarize_meta)
      summaries <- do.call(rbind, summaries)
      summaries[which.max(summaries$m), , drop = FALSE]
    }
  }
  
  txt_target <- switch(set_target,
                       hr = "home range area",
                       ctsd = "speed")
  
  if (rv$grouped) {
    
    err <- summarize_by_max_m(
      meta[meta$type == set_target, , drop = FALSE])
    txt_direction <- ifelse(err$error > 0
                            , "overestimated", "underestimated")
    
    paste0(
      "The mean ", txt_target, " based on ",
      txt_n_tags, " (both groups)", txt_replicates,
      " was ", txt_direction, " by ",
      .err_to_txt(err$error), "% [",
      .err_to_txt(err$error_lci), ", ",
      .err_to_txt(err$error_uci), "].")
    
  } else {
    
    grp_meta <- meta[meta$group == "All" &
                       meta$type == set_target, , drop = FALSE]
    err <- summarize_by_max_m(grp_meta)
    txt_direction <- ifelse(err$error > 0,
                            "overestimated", "underestimated")
    
    paste0(
      "The mean ", txt_target, " based on ",
      txt_n_tags, txt_replicates,
      " was ", txt_direction, " by ",
      .err_to_txt(err$error), "% [",
      .err_to_txt(err$error_lci), ", ",
      .err_to_txt(err$error_uci), "].")
  }
}


#' @title Prepare species and sampling parameters for the report
#' 
#' @noRd
.build_parameters <- function(rv) {
  
  # Characteristic timescales:
  
  tau_p <- rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
  tau_v <- ifelse(is.null(rv$tau_v[[1]]), 0,
                  rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2])
  
  # Ideal sampling design:
  
  ideal_dur <- fix_unit(tau_p * 30, "seconds", convert = TRUE)
  dur_unit <- ideal_dur$unit
  
  if (is.null(rv$tau_v[[1]])) {
    ideal_dti <- data.frame(value = Inf, unit = "days")
  } else {
    ideal_dti <- fix_unit(rv$tau_v[[1]]$value[2],
                          rv$tau_v[[1]]$unit[2], convert = TRUE)
  }
  dti_unit <- ifelse(is.null(rv$tau_v[[1]]), "days", ideal_dti$unit)
  
  # Current sampling design:
  
  dur <- dur_unit %#% rv$dur$value %#% rv$dur$unit
  dur <- fix_unit(dur, dur_unit)
  dti <- dti_unit %#% rv$dti$value %#% rv$dti$unit
  dti <- fix_unit(dti, dti_unit)
  
  # Effective sample sizes:
  
  N1 <- N2 <- NULL
  
  if ("Home range" %in% rv$which_question) {
    N1 <- rv$dev$N1
    if (is.list(rv$dev$N1)) N1 <- do.call(c, N1)
    N1 <- scales::label_comma(accuracy = 1)(
      mean(N1, na.rm = TRUE))
  }
  
  if ("Speed & distance" %in% rv$which_question) {
    N2 <- rv$dev$N2
    if (is.list(rv$dev$N2)) N2 <- do.call(c, N2)
    N2 <- scales::label_comma(accuracy = 1)(
      mean(N2, na.rm = TRUE))
  }
  
  return(list(
    tau_p = tau_p,
    tau_v = tau_v,
    dur = dur,
    dti = dti,
    dur_unit = dur_unit,
    dti_unit = dti_unit,
    ideal_dti = ideal_dti,
    ideal_dur = ideal_dur,
    N1 = N1,
    N2 = N2))
  
}

#' @title Prepare CIs from meta outputs
#' 
#' @noRd
.extract_ci <- function(meta, type_key) {
  CI <- meta[grep(type_key, meta$type), ]
  c("lci" = CI[nrow(CI), "lci"],
    "est" = CI[nrow(CI), "est"],
    "uci" = CI[nrow(CI), "uci"])
}

#' @title Prepare outputs for the report
#' 
#' @noRd
.format_outputs <- function(rv, ratio = FALSE) {
  
  out_truth <- out_coi <- out_cri <- list()
  txt_target <- txt_title <- list()
  
  txt_ratio_order <- NULL
  if (rv$grouped) txt_ratio_order <- "(for group A/group B)"
  
  out_targets <- NULL
  if ("Home range" %in% rv$which_question) {
    out_targets <- c(out_targets, "hr")
    
    out_truth[["hr"]] <- get_true_hr(
      sigma = rv$sigma,
      ind_var = rv$add_ind_var,
      fit = if (rv$add_ind_var) rv$meanfitList else NULL,
      grouped = FALSE,
      summarized = TRUE)[["All"]]$area
    
    txt_target[["hr"]] <- "home range area"
    txt_title[["hr"]] <- "Home range meta-analyses:"
    
    out_coi[["hr"]] <- .extract_ci(rv$metaErr, "hr")
    out_cri[["hr"]] <- c("lci" = rv$hr_cri$lci,
                         "est" = rv$hr_cri$est,
                         "uci" = rv$hr_cri$uci)
  }
  
  if ("Speed & distance" %in% rv$which_question) {
    out_targets <- c(out_targets, "ctsd")
    
    out_truth[["ctsd"]] <- get_true_speed(
      data = rv$simList,
      seed = rv$seedList,
      tau_p = rv$tau_p,
      tau_v = rv$tau_v,
      sigma = rv$sigma,
      ind_var = rv$add_ind_var,
      fit = if (rv$add_ind_var) rv$meanfitList else NULL,
      grouped = FALSE,
      summarized = TRUE)[["All"]]
    
    txt_target[["ctsd"]] <- "movement speed"
    txt_title[["ctsd"]] <- "Speed meta-analyses:"
    
    out_coi[["ctsd"]] <- .extract_ci(rv$metaErr, "sd")
    out_cri[["ctsd"]] <- c("lci" = rv$sd_cri$lci,
                           "est" = rv$sd_cri$est,
                           "uci" = rv$sd_cri$uci)
  }
  
  css_title <- paste("display: inline-block;",
                     "font-family: var(--sans);",
                     "font-weight: 400;",
                     "font-style: italic;",
                     "font-size: 18px;",
                     "color: var(--sea-dark);",
                     "margin-bottom: 8px;")
  
  txt_meta_link <- span(
    style = paste("text-align: center;",
                  "font-weight: bold;",
                  "font-family: var(--monosans);"),
    "For more information, check the",
    shiny::icon("layer-group", class = "cl-sea"),
    span("Meta-analyses", class = "cl-sea"), "tab.")

  return(list(css_title = css_title,
              out_targets = out_targets,
              out_truth = out_truth,
              out_coi = out_coi,
              out_cri = out_cri,
              txt_target = txt_target,
              txt_title = txt_title,
              txt_meta_link = txt_meta_link,
              txt_ratio_order = txt_ratio_order))
}
