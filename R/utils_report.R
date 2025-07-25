
#' @title Format error as rounded percentage string
#' @noRd
.err_to_txt <- function(x) { 
  if (length(x) > 1) return(round(mean(x, na.rm = TRUE) * 100, 1))
  else return(round(x * 100, 1))
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

#' @title Get text for mean error estimate
#' @noRd
.get_txt_nsim <- function(rv, set_target = c("hr", "ctsd")) {
  set_target <- match.arg(set_target)
  name <- paste0(set_target, "Err")
  
  nsims <- ifelse(
    length(rv$simList) == 1,
    "a single simulation",
    paste(length(rv$simList), "simulations")
  )
  
  if (rv$which_meta == "none") {
    txt_nsim <- paste0(
      "Your mean error estimate based on ", nsims, " was ",
      .err_to_txt(rv[[name]]$est), "%."
    )
  } else {
    err <- rv$meta_tbl %>%
      dplyr::filter(.data$group == "All", 
                    .data$type == set_target) %>%
      dplyr::slice(which.max(.data$m))
    
    direction <- ifelse(dplyr::pull(err, .data$error) > 0,
                        "overestimated", "underestimated")
    
    if (set_target == "hr") {
      txt_nsim <- paste0(
        "The mean home range area based on ", nsims, " was ",
        direction, " by ", 
        .err_to_txt(dplyr::pull(err, .data$error)), "% [",
        .err_to_txt(dplyr::pull(err, .data$error_lci)), ", ",
        .err_to_txt(dplyr::pull(err, .data$error_uci)), "]."
      )
    } else if (set_target == "ctsd") {
      txt_nsim <- paste0(
        "The mean speed based on ", nsims, " was ",
        direction, " by ", 
        .err_to_txt(dplyr::pull(err, .data$error)), "% [",
        .err_to_txt(dplyr::pull(err, .data$error_lci)), ", ",
        .err_to_txt(dplyr::pull(err, .data$error_uci)), "]."
      )
    }
  }
  return(txt_nsim)
}


#' @title Prepare species and sampling parameters for the report
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
#' @noRd
.extract_ci <- function(meta, type_key) {
  CI <- meta[grep(type_key, meta$type), ]
  c("lci" = CI[nrow(CI), "lci"],
    "est" = CI[nrow(CI), "est"],
    "uci" = CI[nrow(CI), "uci"])
}

#' @title Prepare outputs for the report
#' @noRd
.build_outputs <- function(rv, ratio = FALSE) {
  
  set_target <- NULL
  txt_ratio_order <- txt_link_meta <- NULL
  truth <- coi <- cri <- list()
  txt_target <- txt_title <- list()
  if (rv$grouped) {
    txt_ratio_order <- "(for group A/group B)"
  }
  
  if ("Home range" %in% rv$which_question) {
    set_target <- c(set_target, "hr")
    txt_target[["hr"]] <- "home range area"
    txt_title[["hr"]] <- "Home range meta-analyses:"
    
    truth_summarized <- get_true_hr(
      sigma = rv$sigma,
      ind_var = rv$add_ind_var,
      fit = if (rv$add_ind_var) rv$meanfitList else NULL,
      grouped = FALSE,
      summarized = TRUE)
    
    truth[["hr"]] <- truth_summarized[["All"]]$area
    
    coi[["hr"]] <- .extract_ci(rv$metaErr, "hr")
    cri[["hr"]] <- c("lci" = rv$hr_cri$lci,
                     "est" = rv$hr_cri$est,
                     "uci" = rv$hr_cri$uci)
  }
  
  if ("Speed & distance" %in% rv$which_question) {
    set_target <- c(set_target, "ctsd")
    txt_target[["ctsd"]] <- "movement speed"
    txt_title[["ctsd"]] <- "Speed meta-analyses:"
    
    truth_summarized <- get_true_speed(
      data = rv$simList,
      seed = rv$seedList,
      tau_p = rv$tau_p,
      tau_v = rv$tau_v,
      sigma = rv$sigma,
      ind_var = rv$add_ind_var,
      fit = if (rv$add_ind_var) rv$meanfitList else NULL,
      grouped = FALSE,
      summarized = TRUE)
    
    truth[["ctsd"]] <- truth_summarized[["All"]]
    
    coi[["ctsd"]] <- .extract_ci(rv$metaErr, "sd")
    cri[["ctsd"]] <- c("lci" = rv$sd_cri$lci,
                       "est" = rv$sd_cri$est,
                       "uci" = rv$sd_cri$uci)
  }
  
  set_style_title <- paste("display: inline-block;",
                           "font-family: var(--sans);",
                           "font-weight: 400;",
                           "font-style: italic;",
                           "font-size: 18px;",
                           "color: var(--sea-dark);",
                           "margin-bottom: 8px;")
  
  txt_link_meta <- span(
    style = paste("text-align: center;",
                  "font-weight: bold;",
                  "font-family: var(--monosans);"),
    "For more information, check the",
    shiny::icon("layer-group", class = "cl-sea"),
    span("Meta-analyses", class = "cl-sea"), "tab.")

  return(list(set_target = set_target,
              txt_target = txt_target,
              txt_title = txt_title,
              get_truth = truth,
              get_coi = coi,
              get_cri = cri,
              set_style_title = set_style_title,
              txt_link_meta = txt_link_meta,
              txt_ratio_order = txt_ratio_order))
}
