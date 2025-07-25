
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
#' @description This function performs a meta-analysis on movement tracking data, for mean home range area (AKDE) or mean movement speed (CTSD) estimates for a sampled population. It leverages the `ctmm` R package, specifically the `meta()` function, to obtain population-level mean parameters. This function helps to evaluate outputs through the resampling of individuals.
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
#' if(interactive()) {
#' run_meta_resampled(rv, set_target = "hr")
#' }
#'
#' @encoding UTF-8
#' @return A data frame containing meta-analysis outputs, including estimates, errors, confidence intervals, and group information.
#' @author Inês Silva \email{i.simoes-silva@@hzdr.de}
#' 
#' @export
run_meta_resamples <- function(rv,
                               set_target = c("hr", "ctsd"),
                               subpop = FALSE,
                               random = FALSE,
                               max_draws = 100,
                               iter_step = 2,
                               trace = FALSE,
                               ...) {
  
  `%||%` <- function(x, y) if (!is.null(x)) x else y
  
  dots <- list(...)
  .only_max_m <- dots[[".only_max_m"]] %||% FALSE
  .max_m <- dots[[".max_m"]] %||% NULL
  .m <- dots[[".m"]] %||% NULL
  .lists <- dots[[".lists"]] %||% NULL
  .automate_seq <- dots[[".automate_seq"]] %||% FALSE
  
  if (!is.logical(random) || length(random) != 1) {
    stop("'random' must be a single logical value (TRUE or FALSE)")
  }
  
  if (random && (!is.numeric(max_draws) ||
                 length(max_draws) != 1 ||
                 max_draws <= 0))
    stop("'max_draws' must be a single positive numeric",
         " value when 'random' is TRUE")
  if (!random) max_draws <- 1
  
  if (!is.null(.m)) {
    if (!is.numeric(.m) || .m %% 1 != 0 || .m < 1) {
      stop("'.m' must be a positive integer.")
    }
    if (.m > length(rv$simList)) {
      stop(sprintf(
        "Argument `.m` (%d) exceeds number of simulations (%d)!",
        .m, length(rv$simList)))
    }
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
    .lists <- .build_meta_objects(rv, 
                                  set_target = set_target,
                                  subpop = subpop,
                                  trace = trace)
  }
  
  list2env(.lists, envir = environment())
  
  true_estimate <- c()
  true_ratio <- c()
  
  out <- lapply(set_target, function(target) {
    
    if (trace) message(
      sprintf("-- Running for %s:",
              ifelse(target == "hr", "home range", "speed")))
    
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
    
    m_seq <- NULL
    if (is.null(.m)) {
      m_seq <- .get_sequence(input[["All"]],
                             .step = iter_step,
                             .max_m = .max_m,
                             .automate_seq = .automate_seq,
                             grouped = subpop)
      if (.only_max_m) m_seq <- max(m_seq)
    } else {
      m_seq <- .m
    }
    
    for (m in m_seq) {
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
          if (nrow(out_random) > max_draws) {
            out_random <- out_random[
              sample(nrow(out_random), max_draws), , drop = FALSE]
          }
          
          sets$out_random <- out_random
        }
        
        return(sets)
        
      }), names(input))
      
      n_samples <- 1
      if (random && max_draws > 1) {
        n_samples <- if(subpop) nrow(arg[["A"]]$out_random) else 
          nrow(arg[["All"]]$out_random)
        
        if (subpop) { if (arg[["A"]]$sets == 1) n_samples <- 1
        } else { if (arg[["All"]]$sets == 1) n_samples <- 1 }
      }
      
      msg_error <- "Error during resampling."
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
        cat(format(elapsed), "\n")
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
                     ...) {
  dots <- list(...)
  .automate_seq <- dots[[".automate_seq"]] %||% FALSE
  .only_max_m <- dots[[".only_max_m"]] %||% FALSE
  .lists <- dots[[".lists"]] %||% NULL
  
  return(run_meta_resamples(rv,
                            set_target = set_target,
                            subpop = subpop,
                            random = FALSE,
                            max_draws = NULL,
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
#' if(interactive()) {
#' run_meta_loocv(rv, set_target = "hr")
#' }
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
                           ...) {
  
  dots <- list(...)
  .only_max_m <- dots[[".only_max_m"]] %||% TRUE
  .max_m <- dots[[".max_m"]] %||% NULL
  .progress <- dots[[".progress"]] %||% FALSE
  .lists <- dots[[".lists"]] %||% NULL
  
  pb <- NULL
  dt_meta <- NULL
  
  if (inherits(rv, "reactivevalues")) {
    rv_list <- reactiveValuesToList(rv)
  } else { rv_list <- rv }
  
  out <- lapply(set_target, function(target) {
    
    if (target == "ctsd") {
      is_ctsd <- !.check_for_inf_speed(rv_list$ctsdList)
      simList <- rv_list$simList[is_ctsd]
      ctsdList <- rv_list$ctsdList[is_ctsd]
    } else {
      simList <- rv_list$simList
    }
    
    if (length(simList) == 0) return(NULL)
    
    n_total <- length(simList)
    if (n_total == 0) return(NULL)
    max_m <- if (is.null(.max_m)) n_total else min(.max_m, n_total)
    
    if (.progress) {
      pb <- txtProgressBar(min = 1, max = max_m, style = 3)
    }
    
    x <- 1
    for (x in seq_len(max_m)) {
      
      if (trace) message(sprintf("--- %d out of %d", x, max_m))
      
      sim_idx <- seq_len(max_m)
      tmp_file <- rlang::duplicate(rv_list, shallow = FALSE)
      
      tmp_file$seedList <- rv_list$seedList[sim_idx][-x]
      tmp_file$simList <- simList[sim_idx][-x]
      tmp_file$simfitList <- rv_list$simfitList[sim_idx][-x]
      if (target == "hr") 
        tmp_file$akdeList <- rv_list$akdeList[sim_idx][-x]
      if (target == "ctsd") 
        tmp_file$ctsdList <- ctsdList[sim_idx][-x]
      tmp_file$seedList <- rv_list$seedList[sim_idx][-x]
      
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
  
  if (.progress) close(pb)
  
  return(dplyr::distinct(do.call(rbind, out)))
  
}
