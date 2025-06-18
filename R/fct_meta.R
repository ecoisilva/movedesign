
#' @title Build objects for hierarchical modeling
#'
#' @description This function constructs objects based on the provided inputs, either for home range (hr) or speed & distance continuous-time (ctsd).
#'
#' @param rv A list containing outputs, settings and data objects. Must not be NULL.
#' @param set_target Character. Research target: `"hr"` for home range or `"ctsd"` for speed & distance.
#' @param subpop Logical. If TRUE, will run meta-analyses with groups. Default is FALSE.
#' @param trace Logical. If TRUE, prints progress messages. Default is FALSE.
#'
#' @return A list containing:
#' \item{datList}{A list containing the inputs (and optionally, grouped inputs).}
#' \item{outList}{A list of additional computed meta outputs.}
#' \item{truthList}{A list of expected values derived from the target set.}
#' 
#' @noRd
.build_meta_objects <- function(rv,
                                set_target = c("hr", "ctsd"),
                                subpop = FALSE,
                                trace = FALSE) {
  
  if (missing(rv) || is.null(rv)) {
    stop("'rv' must be provided and cannot be NULL")
  }
  
  datList <- list(All = list(), groups = list())
  outList <- list(All = list(), groups = list())
  truthList <- .get_expected_values(rv, set_target)
  
  if ("hr" %in% set_target) {
    datList[[1]][["hr"]] <- rv$akdeList
    
    danger_msg <- "Outputs are empty. Run analyses first."
    if (is.null(datList[[1]][["hr"]])) stop(danger_msg)
    else if (length(datList[[1]][["hr"]]) == 0) stop(danger_msg)
    
    datList[[1]][["hr"]][sapply(
      datList[[1]][["hr"]], is.null)] <- NULL
    
    outList[[1]][["hr"]] <- .capture_meta(
      datList[[1]][["hr"]],
      variable = "area",
      units = TRUE,
      verbose = FALSE,
      plot = FALSE,
      type = "hr")
    
    danger_msg <- "Outputs are empty. Run home range estimation first."
    if (is.null(datList[[1]][["hr"]])) {
      stop(danger_msg)
    } else {
      if (length(datList[[1]][["hr"]]) == 0) stop(danger_msg)
    }
  }
  
  if ("ctsd" %in% set_target) {
    datList[[1]][["ctsd"]] <- rv$ctsdList[
      !.check_for_inf_speed(rv$ctsdList)]
    
    danger_msg <- "Outputs are empty. Run analyses first."
    if (is.null(datList[[1]][["ctsd"]])) stop(danger_msg)
    else if (length(datList[[1]][["ctsd"]]) == 0) stop(danger_msg)
    
    datList[[1]][["ctsd"]][sapply(
      datList[[1]][["ctsd"]], is.null)] <- NULL
    
    outList[[1]][["ctsd"]] <- .capture_meta(
      datList[[1]][["ctsd"]],
      variable = "speed",
      units = TRUE,
      verbose = FALSE,
      plot = FALSE,
      type = "ctsd")
    
    danger_msg <- "Outputs are empty. Run speed estimation first."
    if (is.null(datList[[1]][["ctsd"]])) {
      stop(danger_msg)
    } else {
      if (length(datList[[1]][["ctsd"]]) == 0) stop(danger_msg)
    }
  }
  
  if (subpop) {
    datList[[2]] <- lapply(
      set_target, function(x)
        .get_groups(datList[[1]][[x]], groups = rv$groups[[2]]))
    names(datList[[2]]) <- set_target
    
    if ("hr" %in% set_target) {
      outList[[2]][["hr"]] <- .capture_meta(
        datList[[2]][["hr"]],
        variable = "area",
        units = TRUE,
        verbose = TRUE,
        plot = FALSE,
        type = "hr") %>%
        suppressMessages() %>%
        suppressWarnings() %>%
        quiet()
      
      if (is.null(outList[[2]][["hr"]])) {
        msg_log(
          style = "danger",
          message = paste0(
            msg_danger("Home range"),
            " meta-analyses for groups ",
            msg_danger("failed"), ","),
          detail = "Run more simulations in the appropriate tab.")
      }
    }
    
    if ("ctsd" %in% set_target) {
      outList[[2]][["ctsd"]] <- .capture_meta(
        datList[[2]][["ctsd"]],
        variable = "speed",
        units = TRUE,
        verbose = TRUE,
        plot = FALSE,
        type = "ctsd") %>%
        suppressMessages() %>%
        suppressWarnings() %>%
        quiet()
      
      if (is.null(outList[[2]][["ctsd"]])) {
        msg_log(
          style = "danger",
          message = paste0(
            msg_danger("Speed"),
            " meta-analyses for groups ",
            msg_danger("failed"), ","),
          detail = "Run more simulations in the appropriate tab.")
      }
    }
    
  } # end of if (subpop)
  
  return(list(datList = datList,
              outList = outList,
              truthList = truthList))
  
}

#' @title Running hierarchical model meta-analyses (with resamples)
#'
#' @description This function performs a meta-analysis on movement tracking data, for mean home range area (AKDE) or continuous-time speed and distance (CTSD) estimates for a sampled population. It leverages the `ctmm` R package, specifically the `meta()` function, to obtain population-level mean parameters. This function helps to evaluate the significance of results under combination testing.
#'
#' @param rv A list containing outputs, settings and data objects. Must not be NULL.
#' @param set_target Character. Research target: `"hr"` for home range or `"ctsd"` for speed & distance.
#' @param subpop Logical. If TRUE, will run meta-analyses with groups. Default is FALSE.
#' @param random Logical. If TRUE, samples random subsets of individuals. Default is FALSE.
#' @param max_samples Integer. Maximum number of resamples when `random = TRUE`. Must be positive. Default is 100.
#' @param iter_step Numeric. The size of each iteration step. Default is 2.
#' @param trace Logical. If TRUE, prints progress messages. Default is FALSE.
#' @param .automate_seq Logical. If TRUE, overwrites sequence automatically to improve plot readability. Default is FALSE.
#' @param .only_max_m Logical. If TRUE, will only run the maximum number of individuals. Default is FALSE.
#' @param .lists A list containing already created meta inputs. Default is NULL.
#' 
#' @examples
#'\dontrun{
#' # Running:
#' run_meta_resampled(rv, set_target = "hr")
#'}
#'
#' @encoding UTF-8
#' @return A data frame containing meta-analysis outputs, including estimates, errors, confidence intervals, and group information.
#' @author Inês Silva \email{i.simoes-silva@@hzdr.de}
#' 
#' @export
run_meta_resampled <- function(rv,
                               set_target = c("hr", "ctsd"),
                               subpop = FALSE,
                               random = FALSE,
                               max_samples = 100,
                               iter_step = 2,
                               trace = FALSE,
                               .automate_seq = FALSE,
                               .only_max_m = FALSE,
                               .lists = NULL) {
  
  if (!is.logical(random) || length(random) != 1) {
    stop("'random' must be a single logical value (TRUE or FALSE)")
  }
  
  if (random) {
    if (!is.numeric(max_samples) ||
        length(max_samples) != 1 ||
        max_samples <= 0) {
      stop("'max_samples' must be a single positive numeric value",
           " when 'random' is TRUE")
    }
  } else {
    max_samples <- 1
  }
  
  dt_meta <- data.frame(
    "type" = character(0),
    "m" = numeric(0),
    "sample" = numeric(0),
    "truth" = numeric(0),
    "est" = numeric(0),
    "lci" = numeric(0),
    "uci" = numeric(0),
    "error" = numeric(0),
    "error_lci" = numeric(0),
    "error_uci" = numeric(0),
    "ratio_truth" = numeric(0),
    "ratio_est" = numeric(0),
    "ratio_lci" = numeric(0),
    "ratio_uci" = numeric(0),
    "overlaps" = logical(0),
    "is_grouped" = logical(0),
    "group" = character(0),
    "subpop_detected" = character(0))
  
  datList <- truthList <- NULL
  
  if (is.null(.lists)) {
    lists <- .build_meta_objects(rv, 
                                 set_target = set_target,
                                 subpop = subpop,
                                 trace = trace)
  }
  
  list2env(lists, envir = environment())
  
  true_estimate <- c()
  true_ratio <- c()
  
  out <- lapply(set_target, function(target) {
    
    if (trace) {
      if (target == "hr") message("-- Running for home range:")
      if (target == "ctsd") message("-- Running for speed:")
    }
    
    if (target == "hr") {
      true_estimate[[target]] <- truthList[["hr"]][["All"]]$area
      if (subpop) {
        true_estimate[[
          paste0(target, "_A")]] <- truthList[["hr"]][["A"]]$area
        true_estimate[[
          paste0(target, "_B")]] <- truthList[["hr"]][["B"]]$area
        true_ratio[[target]] <- truthList[["hr"]][["A"]]$area/
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
        true_ratio[[target]] <- truthList[["ctsd"]][["A"]]/
          truthList[["ctsd"]][["B"]]
      }
    }
    
    input <- list()
    input[["All"]] <- datList[["All"]][[target]]
    input_groups <- list(input)
    
    if (subpop) {
      input_groups <- datList[["groups"]][[target]]
      nms_group_A <- names(input[["All"]][rv$groups[[2]][["A"]]])
      nms_group_B <- names(input[["All"]][rv$groups[[2]][["B"]]])
      input <- c(input, input_groups)
    }
    
    m_iter <- NULL
    if (.automate_seq) m_iter <- .get_sequence(input[["All"]],
                                               .iter_step = iter_step,
                                               grouped = subpop)
    else m_iter <- seq(
      2, length(input[["All"]]), by = iter_step)
    
    if (.only_max_m) m_iter <- max(m_iter)
    
    for (m in m_iter) {
      if (m == 1) next
      
      if (trace) {
        message("---- Combinations of ", m, " individuals:")
        start_t <- Sys.time()
      }
      
      arg <- setNames(lapply(input, function(x) {
        sets <- .get_sets(x, set_size = m)
        if (sets$sets == 1) return(sets)
        
        if (random) {
          out_random <- if (length(x) < 10) {
            do.call(rbind, combinat::permn(sets$out))
          } else {
            do.call(rbind, replicate(
              10^6, sample(sets$out), simplify = FALSE))
          }
          
          out_random <- dplyr::distinct(as.data.frame(out_random))
          if (nrow(out_random) > max_samples) {
            out_random <- out_random[
              sample(nrow(out_random), max_samples), , drop = FALSE]
          }
          
          sets$out_random <- out_random
        }
        
        return(sets)
        
      }), names(input))
      
      n_samples <- 1
      if (random && max_samples > 1) {
        n_samples <- if(subpop) nrow(arg[["A"]]$out_random) else 
          nrow(arg[["All"]]$out_random)
        
        if (subpop) { if (arg[["A"]]$sets == 1) n_samples <- 1
        } else { if (arg[["All"]]$sets == 1) n_samples <- 1 }
      }
      
      msg_error <- "Error during combination testing."
      if (length(n_samples) == 0) stop(msg_error)
      
      for (sample in seq_len(n_samples)) {
        
        inputList <- list()
        if (subpop && random) {
          # No group assigments:
          inputList[["All"]] <- if (arg[["All"]]$sets == 1) {
            input[["All"]][arg[["All"]]$out_random == 1]
          } else {
            input[["All"]][arg[["All"]]$out_random[sample, ] == 1]
          }
        } else {
          inputList[["All"]] <- if (n_samples == 1) {
            input[["All"]][arg[["All"]]$out_random == 1]
          } else {
            input[["All"]][arg[["All"]]$out_random[sample, ] == 1]
          }
        }
        
        if (subpop) {
          # Group assigments:
          inputList[["groups"]] <- c(input_groups[["A"]],
                                     input_groups[["B"]])
          out_random <- c( 
            if (n_samples == 1) arg[["A"]]$out_random else
              arg[["A"]]$out_random[sample, ],
            if (n_samples == 1) arg[["B"]]$out_random else
              arg[["B"]]$out_random[sample, ])
          
          if (all(names(inputList[["groups"]]) != 
                  c(arg[["A"]]$names, arg[["B"]]$names)))
            stop("Issue with groups..")
          
          inputList[["groups"]] <- .get_groups(
            inputList[["groups"]][out_random == 1],
            groups = rv$groups[[2]])
        }
        
        if (target == "hr") variable <- "area"
        if (target == "ctsd") variable <- "speed"
        
        out_meta <- setNames(lapply(inputList, function(x) {
          return(.capture_meta(x,
                               variable = variable,
                               sort = TRUE,
                               units = FALSE,
                               verbose = TRUE,
                               plot = FALSE) %>%
                   suppressMessages())
        }), names(inputList))
        
        truth <- list()
        out_est <- list()
        out_err <- list()
        subpop_detected <- list()
        
        nm_groups <- if (subpop) c("A", "B") else c("All")
        n_groups <- length(nm_groups)
        
        if (is.null(out_meta[["All"]])) {
          
          dt_meta <- rbind(
            dt_meta,
            data.frame(
              type = target,
              m = m,
              sample = sample,
              truth = NA,
              est = NA,
              lci = NA,
              uci = NA,
              error = NA,
              error_lci = NA,
              error_uci = NA,
              ratio_truth = NA,
              ratio_est = NA,
              ratio_lci = NA,
              ratio_uci = NA,
              overlaps = NA,
              is_grouped = subpop,
              group = "All",
              subpop_detected = NA))
          
        } else {
          
          truth[["All"]] <- true_estimate[[target]]
          
          out_est[["All"]] <- .get_estimates(out_meta[["All"]]$meta)
          out_err[["All"]] <- sapply(out_est[["All"]], .get_errors,
                                     truth = truth[["All"]])
          
          truth_ratio <- NA
          out_ratio <- c("lci" = NA, "est" = NA, "uci" = NA)
          subpop_detected[["All"]] <- out_meta[["All"]]$
            logs$subpop_detected
          
          dt_meta <- rbind(
            dt_meta,
            data.frame(
              type = target,
              m = m,
              sample = sample,
              truth = truth[["All"]],
              est = out_est[["All"]][["est"]],
              lci = out_est[["All"]][["lci"]],
              uci = out_est[["All"]][["uci"]],
              error = out_err[["All"]][["est"]],
              error_lci = out_err[["All"]][["lci"]],
              error_uci = out_err[["All"]][["uci"]],
              ratio_truth = truth_ratio,
              ratio_est = out_ratio[["est"]],
              ratio_lci = out_ratio[["lci"]],
              ratio_uci = out_ratio[["uci"]],
              overlaps = NA,
              is_grouped = subpop,
              group = "All",
              subpop_detected = as.character(
                subpop_detected[["All"]])))
          
        } # end of if (is.null(out_meta[["All"]]))
        
        if (subpop) {
          
          if (is.null(out_meta[["groups"]])) {
            for (group in seq_len(n_groups)) {
              
              dt_meta <- rbind(
                dt_meta,
                data.frame(
                  type = target,
                  m = m,
                  sample = sample,
                  truth = NA,
                  est = NA,
                  lci = NA,
                  uci = NA,
                  error = NA,
                  error_lci = NA,
                  error_uci = NA,
                  ratio_truth = NA,
                  ratio_est = NA,
                  ratio_lci = NA,
                  ratio_uci = NA,
                  overlaps = NA,
                  is_grouped = subpop,
                  group = nm_groups[group],
                  subpop_detected = NA))
              
            } # end of [group] loop
            
          } else {
            
            truth_ratio <- true_ratio[[target]]
            ratios <- .get_ratios(out_meta[["groups"]])
            
            out_ratio <- c(
              "lci" = .get_ratios(out_meta[["groups"]])$lci,
              "est" = .get_ratios(out_meta[["groups"]])$est,
              "uci" = .get_ratios(out_meta[["groups"]])$uci)
            
            # out_ratio_err <- c(
            #   "lci" = (ratio_lci - truth_ratio) / truth_ratio,
            #   "est" = (ratio_est - truth_ratio) / truth_ratio,
            #   "uci" = (ratio_uci - truth_ratio) / truth_ratio)
            
            truth[["A"]] <- true_estimate[[paste0(target, "_A")]]
            truth[["B"]] <- true_estimate[[paste0(target, "_B")]]
            
            out_est[["A"]] <- .get_estimates(out_meta[["groups"]]$meta$A)
            out_err[["A"]] <- sapply(out_est[["A"]], .get_errors,
                                     truth = truth[["A"]])
            
            out_est[["B"]] <- .get_estimates(out_meta[["groups"]]$meta$B)
            out_err[["B"]] <- sapply(out_est[["B"]], .get_errors,
                                     truth = truth[["B"]])
            
            subpop_detected[["A"]] <- subpop_detected[["B"]] <- 
              out_meta[["groups"]]$logs$subpop_detected
            
            for (group in seq_len(n_groups)) {
              
              dt_meta <- rbind(
                dt_meta,
                data.frame(
                  type = target,
                  m = m,
                  sample = sample,
                  truth = truth[[nm_groups[group]]],
                  est = out_est[[nm_groups[group]]][["est"]],
                  lci = out_est[[nm_groups[group]]][["lci"]],
                  uci = out_est[[nm_groups[group]]][["uci"]],
                  error = out_err[[nm_groups[group]]][["est"]],
                  error_lci = out_err[[nm_groups[group]]][["lci"]],
                  error_uci = out_err[[nm_groups[group]]][["uci"]],
                  ratio_truth = truth_ratio,
                  ratio_est = out_ratio[["est"]],
                  ratio_lci = out_ratio[["lci"]],
                  ratio_uci = out_ratio[["uci"]],
                  overlaps = NA,
                  is_grouped = subpop,
                  group = nm_groups[group],
                  subpop_detected = as.character(
                    subpop_detected[[nm_groups[group]]])))
              
            } # end of [group] loop
            
          } # end of if (is.null(out_meta[["groups"]]))
        } # end of if (subpop)
        
      } # end of [sample] loop [based on the number of samples]
      
      if (trace) {
        message("Elapsed time:")
        elapsed <- Sys.time() - start_t
        cat("Elapsed time since start:", format(elapsed), "\n")
      }
      
    } # end of [m] loop [based on the number of individuals]
    
    return(dt_meta)
    
  }) # end of [set_target] lapply
  
  return(dplyr::distinct(do.call(rbind, out)))
  
}

#' @title Running hierarchical models
#'
#' @description This function wraps around the `run_meta_resampled` function to run hierarchical models (with no resamples) for a quick evaluation.
#' 
#' @inheritParams run_meta_resampled
#' 
#' @return The outputs of the `run_meta_resampled` function for a single combination.
#' @export
run_meta <- function(rv,
                     set_target = c("hr", "ctsd"),
                     subpop = FALSE, 
                     trace = FALSE,
                     iter_step = 2,
                     .automate_seq = FALSE,
                     .only_max_m = FALSE,
                     .lists = NULL) {
  
  return(run_meta_resampled(rv,
                            set_target = set_target,
                            subpop = subpop,
                            random = FALSE,
                            max_samples = NULL,
                            trace = trace,
                            iter_step = iter_step,
                            .automate_seq = .automate_seq,
                            .only_max_m = .only_max_m,
                            .lists = .lists))
}


#' @title Running LOOCV on hierarchical model outputs
#'
#' @description This function runs hierarchical models on movement tracking data,
#' for mean home range area (AKDE) or continuous-time speed and distance (CTSD)
#' estimates for a sampled population. It leverages the `ctmm` R package,
#' specifically the `meta()` function, to obtain population-level mean parameters.
#' This function helps to assess outputs via leave-one-out cross-validation (LOOCV).
#'
#' @param rv A list containing outputs, settings and data objects. Must not be NULL.
#' @param set_target Character. Research target: `"hr"` for home range or `"ctsd"` for speed & distance.
#' @param subpop Logical. If TRUE, will run meta-analyses with groups. Default is FALSE.
#' @param trace Logical. If TRUE, prints progress messages. Default is FALSE.
#' @param .only_max_m Logical. If TRUE, will only run the maximum number of individuals. Default is FALSE.
#' @param .progress Logical. If TRUE, will display a progress bar. Default is FALSE.
#' @param .lists A list containing already created meta inputs. Default is NULL.
#' 
#' @examples
#'\dontrun{
#' # Running:
#' run_meta_loocv(rv, set_target = "hr")
#'}
#'
#' @encoding UTF-8
#' @return A data frame containing meta-analysis outputs, including estimates, errors, confidence intervals, and group information.
#' @author Inês Silva \email{i.simoes-silva@@hzdr.de}
#' 
#' @export
run_meta_loocv <- function(rv,
                           set_target = c("hr", "ctsd"),
                           subpop = FALSE,
                           trace = FALSE,
                           .progress = FALSE,
                           .only_max_m = TRUE,
                           .lists = NULL) {
  
  dt_meta <- NULL
  
  if (inherits(rv, "reactivevalues"))
    rv_list <- reactiveValuesToList(rv) else rv_list <- rv
    
    out <- lapply(set_target, function(target) {
      
      if (target == "ctsd") {
        is_ctsd <- !.check_for_inf_speed(rv_list$ctsdList)
        simList <- rv_list$simList[is_ctsd]
        ctsdList <- rv_list$ctsdList[is_ctsd]
      } else {
        simList <- rv_list$simList
      }
      
      if (length(simList) == 0) return(NULL)
      
      if (.progress) {
        total_steps <- length(simList)
        pb <- txtProgressBar(min = 0, max = total_steps, style = 3)
      }
      
      x <- 1
      for (x in seq_along(simList)) {
        
        if (trace)
          message(paste("---", x, "out of", length(rv_list$simList)))
        
        tmp_file <- rlang::duplicate(rv_list, shallow = FALSE)
        tmp_file$seedList <- rv_list$seedList[-x]
        tmp_file$simList <- simList[-x]
        tmp_file$simfitList <- rv_list$simfitList[-x]
        if (target == "hr") tmp_file$akdeList <- rv_list$akdeList[-x]
        if (target == "ctsd") tmp_file$ctsdList <- ctsdList[-x]
        tmp_file$seedList <- rv_list$seedList[-x]
        
        if (target == "ctsd" && length(tmp_file$ctsdList) > 0) {
          tmp_file$ctsdList[sapply(tmp_file$ctsdList, is.null)] <- NULL
          
          new_i <- 0
          new_list <- list()
          for (i in seq_along(tmp_file$ctsdList)) {
            if (tmp_file$ctsdList[[i]]$CI[, "est"] != "Inf") {
              new_i <- new_i + 1
              new_list[[new_i]] <- tmp_file$ctsdList[[i]]
            }
          }
          
          if (length(new_list) == 0) new_list <- NULL
          
          tmp_file$ctsdList <- new_list
          tmp_file$ctsdList[sapply(tmp_file$ctsdList, is.null)] <- NULL
          
        } # end of if (target == "ctsd" &&
        #              length(tmp_file$ctsdList) > 0)
        
        tmp_dt <- NULL
        tmp_dt <- run_meta(tmp_file, 
                           set_target = target,
                           .only_max_m = TRUE,
                           .automate_seq = TRUE)
        
        if (nrow(tmp_dt) > 0) {
          tmp_dt$x <- x
          if (is.null(dt_meta)) {
            dt_meta <- tmp_dt
          } else {
            dt_meta <- rbind(dt_meta, tmp_dt)
          }
        }
        if (.progress) {
          setTxtProgressBar(pb, x)
        }
        
      } # end of [x] loop (individuals)
      
      return(dt_meta)
      
    }) # end of [set_target] lapply
    
    # close(pb)
    
    return(dplyr::distinct(do.call(rbind, out)))
    
}


#' @title Plot meta (combinations)
#'
#' @noRd 
#' 
plot_meta_combinations <- function(rv,
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
    
    stopifnot(all(!is.na(out$est)), nrow(out) > 0)
    
    max_samples <- max(unique(out$sample))
    max_samples
    
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
      "<b>Maximum number of samples:</b>", max_samples)
    
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
      ggplot2::scale_shape_manual("Group:", values = c(16, 18)) +
      
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
      txt_color <- "Group:"
      
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
          data = subset(out, .data$subpop_detected == TRUE),
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
      ggplot2::scale_shape_manual("Group:", values = c(16, 18)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    
    if (rv$which_meta == "mean") {
      p.optimal <- p.optimal +
        ggplot2::guides(shape = "none")
    }
    
  } # end of (!random)
  
  return(p.optimal)
  
}
