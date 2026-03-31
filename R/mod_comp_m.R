#' comp_m UI Function
#'
#' @description Module for setting the number of simulations.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comp_m_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Number of simulations: ----------------------------------------------
    
    shinydashboardPlus::box(
      title = span("Simulations", class = "ttl-box_solid"),
      id = ns("mBox_nsims"),
      status = "warning",
      width = NULL,
      solidHeader = TRUE,
      collapsible = FALSE,
      
      column(
        width = 12, align = "left",
        
        fluidRow(
          p(style = "padding: 0px;"),
          
          shinyWidgets::autonumericInput(
            inputId = ns("nsims"),
            label = "Number of tags:",
            currencySymbol = " tag(s)",
            currencySymbolPlacement = "s",
            decimalPlaces = 0,
            minimumValue = 1,
            maximumValue = 100,
            value = 1, wheelStep = 1),
          
          shinyWidgets::autonumericInput(
            inputId = ns("nsims_max"),
            label = "Number of tags (maximum):",
            currencySymbol = " tag(s)",
            currencySymbolPlacement = "s",
            decimalPlaces = 0,
            minimumValue = 2,
            maximumValue = 500,
            value = 2, wheelStep = 2),
          
          shinyWidgets::autonumericInput(
            inputId = ns("n_replicates_minimize"),
            label = "Number of replicates:",
            currencySymbol = " replicates(s)",
            currencySymbolPlacement = "s",
            decimalPlaces = 0,
            minimumValue = 5,
            maximumValue = 500,
            value = 5,
            wheelStep = 1),
          
          fluidRow(
            column(width = 12,
                   verbatimTextOutput(outputId = ns("txt_m_groups"))
            )),
          br(),
          
          shiny::uiOutput(ns("mUI_nsims_iter")),
          uiOutput(ns("error_threshold_for_m")),
          
          fluidRow(
            column(width = 12,
                   div(id = ns("txt_ratio_label"),
                       p(style = "text-align: left !important;",
                         HTML("&nbsp;"), "Ratio:") %>%
                         tagAppendAttributes(class = 'label_split')),
                   
                   verbatimTextOutput(outputId = ns("txt_ratio"))
            ))
          
        ) # end of fluidRow
        
      ), # end of column
      
      footer = column(
        width = 12, align = "right",
        style = "padding-left: 0px; padding-right: 0px;",
        
        shiny::actionButton(
          inputId = ns("mButton_repeat"),
          icon = icon("bolt"),
          label = "Simulate",
          class = "btn-sims",
          width = "125px")
        
      ) # end of column (footer)
    ), # end of box // mBox_nsims
    
  ) # end of tagList
}

#' comp_m Server Functions
#'
#' @noRd 
mod_comp_m_server <- function(id, rv,
                              error_threshold,
                              set_analysis = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    rv$m <- reactiveValues(proceed = NULL, 
                           needs_fit = NULL, 
                           tmpList = NULL)
    
    observeEvent(input$error_threshold, {
      rv$error_threshold <- input$error_threshold/100
    }, priority = 1, ignoreInit = TRUE)
    
    observe({
      req(rv$which_m)
      req(rv$which_m == "get_m")
      rv$n_replicates <- input$n_replicates_minimize
    })
    
    ## Estimating time: ---------------------------------------------------
    
    estimating_time <- reactive({
      
      out_time <- guess_time(data = rv$simList,
                             error = rv$error,
                             parallel = rv$parallel)
      return(out_time)
      
    }) %>% # end of reactive, estimating_time()
      bindCache(c(rv$tau_p, 
                  rv$tau_v,
                  rv$dur, 
                  rv$dti))
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at start: --------------------------------------------
    
    shinyjs::hide(id = "mBox_nsims")
    
    shinyjs::hide(id = "ratio")
    shinyjs::hide(id = "txt_ratio")
    shinyjs::hide(id = "txt_ratio_label")
    
    shinyjs::hide(id = "nsims")
    shinyjs::hide(id = "nsims_max")
    shinyjs::hide(id = "n_replicates_minimize")
    shinyjs::hide(id = "mUI_nsims_iter")
    
    ## Reveal elements based on workflow: ---------------------------------
    
    observe({
      req(rv$which_meta)
      
      if ("compare" == rv$which_meta) {   
        shinyjs::show(id = "ratio")
        shinyjs::show(id = "txt_ratio_label")
        shinyjs::show(id = "txt_ratio")
        
      }
      
      if ("mean" == rv$which_meta) {   
        shinyjs::hide(id = "ratio")
        shinyjs::hide(id = "txt_ratio_label")
        shinyjs::hide(id = "txt_ratio")
      }
      
      if ("none" == rv$which_meta) {
        shinyjs::hide(id = "nsims")
        shinyjs::hide(id = "nsims_max")
      }
      
    }) %>% # end of observe,
      bindEvent(rv$which_meta)
    
    observe({
      req(rv$which_m)
      shinyjs::show("error_threshold")
      
      if (rv$which_m == "set_m") {
        req(input$nsims)
        rv$n_sims <- as.numeric(input$nsims)
      } else if (rv$which_m == "get_m") {
        req(input$nsims_max)
        rv$n_sims <- as.numeric(input$nsims_max)
      }
      
    }) %>% # end of observe,
      bindEvent(list(rv$active_tab,
                     input$nsims,
                     input$nsims_max))
    
    observe({
      req(rv$which_m, rv$which_meta)
      req(rv$active_tab == 'hr' || rv$active_tab == 'ctsd')
      
      if (rv$which_m == "set_m") {
        shinyjs::show(id = "nsims")
        shinyjs::hide(id = "nsims_max")
        shinyjs::hide(id = "n_replicates_minimize")
      } else if (rv$which_m == "get_m") {
        shinyjs::hide(id = "nsims")
        shinyjs::show(id = "nsims_max")
        shinyjs::show(id = "n_replicates_minimize")
      } else {
        shinyjs::hide(id = "nsims")
        shinyjs::hide(id = "nsims_max")
        shinyjs::hide(id = "n_replicates_minimize")
      }
      
      req(length(rv$simList) >= 2)
      wheel_step <- ifelse("compare" %in% rv$which_meta, 2, 1)
      
      req(rv$which_m == "set_m")
      set_value <- length(rv$simList)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "nsims",
        label = "Number of tags (total):",
        value = set_value, 
        options = list(
          decimalPlaces = 0,
          minimumValue = 1,
          maximumValue = 100,
          wheelStep = wheel_step))
      
      # if (rv$which_m == "get_m") {
      #   shinyWidgets::updateAutonumericInput(
      #     session = session,
      #     inputId = "nsims_max",
      #     label = "Number of tags (maximum):",
      #     value = set_value,
      #     options = list(
      #       decimalPlaces = 0,
      #       minimumValue = 1,
      #       maximumValue = 100,
      #       wheelStep = wheel_step))
      # }
      
    }) %>% # end of observe,
      bindEvent(rv$active_tab)
    
    observe({
      req(rv$which_meta, rv$is_analyses)
      
      if (rv$is_analyses && rv$which_meta != "none")
        shinyjs::show(id = "mBox_nsims") else
          shinyjs::hide(id = "mBox_nsims")
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'meta')
      shinyjs::show(id = "mBox_nsims")
      
    }) # end of observe
    
    ## Render number of simulations: --------------------------------------
    
    # output$nsims_total <- renderText({
    #   req(input$nsims)
    #   
    #   m <- 1 + input$nsims
    #   if (!is.null(rv$simList)) m <- length(rv$simList) + input$nsims
    #   return(m)
    #   
    # }) # end of renderText, "nsims_total"
    
    observe({
      req(rv$which_m)
      
      if (rv$which_m == "set_m") 
        shinyjs::hide(id = "nsims") else
          shinyjs::show(id = "nsims")
      
    }) # end of observe
    
    ## Update number of tags: ---------------------------------------------
    
    observe({
      req(rv$simList)
      req(rv$active_tab == 'hr' || rv$active_tab == 'ctsd')
      req(length(rv$simList) == 1)
      
      wheel_step <- ifelse("compare" %in% rv$which_meta, 2, 1)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "nsims",
        label = "Number of tags (total):",
        value = 1,
        options = list(
          decimalPlaces = 0,
          minimumValue = 1,
          maximumValue = 100,
          wheelStep = wheel_step))

    }) %>% # end of observe,
      bindEvent(rv$simList)
    
    ## Rendering effect size (based on groups): ---------------------------
    
    output$txt_ratio <- renderText({
      req("compare" %in% rv$which_meta)
      req(rv$metaList_groups[[1]],
          rv$set_analysis)
      req(rv$set_analysis == set_analysis)
      
      meta <- rv$metaList_groups[[1]][[rv$set_analysis]]
      req(meta)
      
      ratio <- round(.get_ratios(meta)$est, 1)
      req(ratio)
      
      out_txt <- NULL
      if (rv$set_analysis == "hr") {
        var <- "home range area"
        txt_diff <- c("smaller", "larger")
      }
      
      if (set_analysis == "ctsd") {
        var <- "speed"
        txt_diff <- c("slower", "faster")
      }
      
      if (ratio == 1) {
        out_txt <- paste0(
          "Group A's ", var, " should be equal to Group B's.")
      } else if (ratio < 1) {
        out_txt <- paste0(
          "Group A's ", var, " should be ",
          round(abs(100 - ratio * 100), 1),
          "% ", txt_diff[[1]], " than Group B's.")
      } else if (ratio > 1) { 
        out_txt <- paste0(
        "Group A's ", var, " area should be ",
        round(abs(100 - ratio * 100), 1),
        "% ", txt_diff[[2]], " than Group B's.")
      }
      
      return(out_txt)
      
    }) %>% # end of renderText, "txt_ratio",
      bindEvent(rv$set_analysis)
    
    ## Rendering number of tags per group: --------------------------------
    
    output$txt_m_groups <- renderText({
      req(input$nsims, "compare" %in% rv$which_meta)
      req(input$nsims > 1)
      
      if (input$nsims == 2) return("1 tag per group")
      else return(paste(input$nsims / 2, "tags per group"))
      
    }) %>% # end of renderText, "txt_m_groups",
      bindEvent(input$nsims)
    
    ## Rendering (and updating) error threshold: --------------------------
    
    output$error_threshold_for_m <- renderUI({
      
      shinyWidgets::numericInputIcon(
        inputId = ns("error_threshold"),
        label = "Error threshold:",
        min = 1, max = 100, step = 1,
        value = rv$error_threshold * 100,
        icon = list(NULL, icon("percent")),
        help_text =  "Must be between 1 and 100.")
    })
    
    observe({
      req(rv$active_tab == 'hr' || rv$active_tab == 'ctsd')
      
      shinyWidgets::updateNumericInputIcon(
        session = session,
        input = "error_threshold",
        value = rv$error_threshold * 100)
      
    }, priority = 1) %>% # end of observe,
      bindEvent(rv$active_tab)
    
    ## Rendering iteration size: ------------------------------------------
    
    output$mUI_nsims_iter <- shiny::renderUI({
      req(rv$which_m, rv$n_sims)
      req(rv$n_sims > 0)
      
      v <- input$nsims_iter %||% 2
      
      shinyWidgets::sliderTextInput(
        inputId = ns("nsims_iter"),
        label = paste0("Check convergence every ", v, " tags:"),
        choices = seq(2, max(2, round(rv$n_sims/2, 0)), by = 2),
        selected = v)
      
    }) # end of renderUI
    
    observe({
      if (rv$which_m == "set_m") {
        shinyjs::hide(id = "mUI_nsims_iter")
      } else if (rv$n_sims <= 8) {
        shinyjs::hide(id = "mUI_nsims_iter")
      } else {
        shinyjs::show(id = "mUI_nsims_iter")
      }
    }) %>% # end of observe,
      bindEvent(rv$n_sims)
    
    # SIMULATIONS ---------------------------------------------------------
    ## Run multiple simulations (set number of tags): ---------------------
    
    observe({
      req(rv$which_meta,
          rv$which_m == "set_m")
      req(rv$datList,
          rv$dur, rv$dti,
          rv$dev$is_valid,
          rv$simList)
      
      if (rv$data_type != "simulated") req(rv$fitList)
      else req(rv$modList)
      
      if ("compare" %in% rv$which_meta) req((rv$n_sims - 2) > 0) 
      else req((rv$n_sims - 1) > 0)
      
      rv$m$needs_fit <- FALSE
      has_groups <- rv$grouped
      
      start <- Sys.time()
      tmpList <- list()
      
      num_sims <- input$nsims - length(rv$simList)
      if (rv$grouped) num_sims <- num_sims / 2
      
      if (length(num_sims) == 0 || num_sims == 0) {
        shinybusy::remove_modal_spinner()
        
        # If more simulations are requested for both questions
        # (case when simList is done, but ctsdList is not):
        if (!is.null(rv$ctsdList) && !is.null(rv$akdeList))
          if (length(rv$simList) == length(rv$akdeList) &&
              length(rv$simList) != length(rv$ctsdList) &&
              rv$active_tab == "ctsd") {
            rv$sd_completed <- FALSE
            rv$m$proceed <- TRUE
          }
      }
      
      req(length(num_sims) > 0)
      req(num_sims > 0)
      if (rv$which_meta == "compare") req(rv$groups)
      if (rv$add_ind_var) req(rv$meanfitList)
        
      rv$meta_tbl <- NULL
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = tagList(span(
          style = "font-size: 18px;",
          span("Simulating multiple", style = "color: #797979;"),
          wrap_none(span("datasets", class = "cl-sea"),
                    span("...", style = "color: #797979;")))
        ))
      
      shinyFeedback::showToast(
        type = "info",
        message = paste0("Simulating ", input$n_sims, "datasets..."),
        .options = list(
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right")
      )
      
      msg_log(
        style = "warning",
        message = paste0("Simulations ",
                         msg_warning("in progress"), "..."))
      
      tmpnames_new <- list()
      for (i in seq_len(num_sims)) {
        
        rv$seed0 <- generate_seed(rv$seedList)
        simList <- simulating_data(rv, rv$seed0)
        if (!rv$grouped) {
          names(simList) <- c(rv$seed0)
        } else {
          rv$groups[[2]][["A"]] <- c(as.character(rv$groups[[2]]$A),
                                     as.character(rv$seed0))
          rv$groups[[2]][["B"]] <- c(as.character(rv$groups[[2]]$B),
                                     as.character(rv$seed0 + 1))
          names(simList) <- c(rv$seed0, rv$seed0 + 1)
        }
        
        # If there is tag failure:
        
        failure_occurred <- FALSE
        if (!is.null(rv$fail_prob)) {
          if (req(rv$fail_prob) > 0) {
            
            fail_prob <- rv$fail_prob
            simList <- lapply(simList, function(x) {
              
              failure_occurred <- sample(
                c(FALSE, TRUE), size = 1, 
                prob = c(1 - fail_prob, fail_prob))
              
              to_keep_vec <- rep(1, nrow(x))
              if (failure_occurred) {
                
                to_keep_vec <- c(rep(1, 10), cumprod(
                  1 - stats::rbinom(nrow(x) - 10, 1, prob = 0.01)))
                if (!any(to_keep_vec == 0)) failure_occurred <- FALSE
                
                rv$dev_failed <- c(rv$dev_failed, failure_occurred)
                return(x[to_keep_vec == 1, ])
                
              } else {
                rv$dev_failed <- c(rv$dev_failed, failure_occurred)
                return(x)
              }
              
            }) # end of lapply
            
          } # end of if (rv$fail_prob > 0)
        } else rv$dev_failed <- c(rv$dev_failed, failure_occurred)
        
        # If there is data loss:
        
        if (!is.null(rv$lost))
          if (rv$lost$perc > 0) {
            
            simList <- lapply(simList, function(x) {
              to_keep <- round(nrow(x) * (1 - rv$lost$perc), 0)
              to_keep_vec <- sort(
                sample(seq_len(nrow(x)), to_keep, replace = FALSE))
              x[to_keep_vec, ] })
            
          } # end of data loss
        
        # If there are errors associated with each location:
        
        if (!is.null(rv$error))
          if (req(rv$error) > 0) {
            
            simList <- lapply(simList, function(x) {
              
              x$error_x <- x$error_y <- stats::rnorm(
                nrow(x), mean = 0, sd = rv$error)
              
              x$HDOP <- sqrt(2) * sqrt(x$error_x^2 + x$error_y^2) /
                sqrt(-2 * log(0.05))
              
              x$original_x <- x$x
              x$original_y <- x$y
              x[c("x", "y")] <- x[c("x", "y")] + c(x$error_x,
                                                   x$error_y)
              
              ctmm::uere(x) <- 1
              
              return(x) })
            
          } # end of location error
        
        # Add to lists:
        
        if (rv$grouped) {
          tmpList <- c(tmpList, simList)
          tmpnames_new[[i]] <- names(simList)
          tmpnames <- names(rv$simList)
          
          rv$simList <- c(rv$simList, simList)
          rv$seedList <- c(rv$seedList, rv$seed0, rv$seed0 + 1)
          names(rv$simList) <- c(tmpnames, rv$seed0, rv$seed0 + 1)
        } else {
          tmpList[[i]] <- simList[[1]] 
          tmpnames_new[[i]] <- names(simList)
          tmpnames <- names(rv$simList)
          
          rv$simList[[length(rv$simList) + 1]] <- simList[[1]]
          rv$seedList[[length(rv$seedList) + 1]] <- rv$seed0
          names(rv$simList) <- c(tmpnames, rv$seed0)
        }
        
      } # end of for loop
      
      rv$tmpList <- tmpList
      names(rv$tmpList) <- do.call(c, tmpnames_new)
      
      rv$dev$n <- lapply(seq_along(rv$simList), function(x)
        nrow(rv$simList[[x]]))
      
      rv$m$needs_fit <- TRUE
      rv$is_analyses <- FALSE
      rv$hr_completed <- FALSE
      rv$sd_completed <- FALSE
      
      msg_log(
        style = "success",
        message = paste0("Simulations ",
                         msg_success("completed"), "."),
        run_time = difftime(Sys.time(), start, units = "sec"))
      
      shinyFeedback::showToast(
        type = "success",
        message = "Simulations completed!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"
        )
      )
      
      shinybusy::remove_modal_spinner()
      
    }, label = "o-m_sims") %>% # end of observer
      bindEvent(input$mButton_repeat)
    
    observe({
      req(rv$which_m == "set_m",
          rv$datList,
          rv$simList,
          rv$simfitList,
          rv$dur, 
          rv$dti,
          rv$dev$is_valid,
          rv$m$needs_fit)
      req(rv$set_analysis == set_analysis)
      
      rv$m$proceed <- NULL
      
      loading_modal("Calculating run time")
      expt <- estimating_time()
      
      x <- NULL
      confirm_time <- NULL
      if ((as.numeric(expt$max) %#% expt$unit) > 900) {
        
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            confirm_time <- x
          },
          text = tagList(span(
            "Expected run time for the next phase", br(),
            "is approximately",
            span(expt$min, "\u2013", expt$max,
                 class = "cl-dgr"),
            wrap_none(span(expt$unit,
                           class = "cl-dgr"), ".")
          )),
          type = "warning",
          showCancelButton = TRUE,
          cancelButtonText = "Stop",
          confirmButtonCol = pal$mdn,
          confirmButtonText = "Proceed",
          html = TRUE
        )
      } else { confirm_time <- TRUE }
      
      shinybusy::remove_modal_spinner()
      
      req(confirm_time)
      
      start <- Sys.time()
      num_sims <- length(rv$tmpList)
      loading_modal("Selecting movement model", type = "fit",
                    exp_time = rv$expt,
                    n = num_sims,
                    parallel = rv$parallel)
      
      simList <- rv$tmpList
      
      if (is.null(rv$error)) {
        guessList <- lapply(seq_along(simList), function (x)
          ctmm::ctmm.guess(simList[[x]], interactive = FALSE))
      } else {
        guessList <- lapply(seq_along(simList), function (x)
          ctmm::ctmm.guess(simList[[x]],
                           CTMM = ctmm::ctmm(error = TRUE),
                           interactive = FALSE))
      }
        
      if (rv$parallel) {
        
        msg_log(
          style = "warning",
          message = paste0("Model selection for ", num_sims,
                           " simulation(s) (out of ", rv$n_sims, ") ",
                           msg_warning("in progress"), ","),
          detail = "This may take a while...")
        
        simfitList <- fitting_models(simList,
                                     set_target = rv$set_analysis,
                                     .dur = rv$dur,
                                     .dti = rv$dti,
                                     .tau_p = rv$tau_p,
                                     .tau_v = rv$tau_v,
                                     .error_m = rv$error,
                                     .check_sampling = TRUE,
                                     .rerun = TRUE,
                                     parallel = rv$parallel)
        
        rv$dev$N1 <- c(rv$dev$N1, extract_dof(simfitList, "area"))
        rv$dev$N2 <- c(rv$dev$N2, extract_dof(simfitList, "speed"))
        
        m <- length(rv$simfitList)
        rv$simfitList <- c(rv$simfitList, simfitList)
        names(rv$simfitList) <- names(rv$simList)
        
        lapply(seq_along(simList), function(x) {
          nm <- names(rv$simList)[[(rv$n_sims - num_sims) + x]]
          
          group <- 1
          if (rv$grouped) {
            group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
          }
          
          if (rv$add_ind_var) {
            tau_p <- suppressWarnings(extract_pars(
              emulate_seeded(
                rv$meanfitList[[group]], 
                rv$seedList[[(rv$n_sims - num_sims) + x]]),
              "position"))[[1]]
            tau_v <- suppressWarnings(extract_pars(
              emulate_seeded(
                rv$meanfitList[[group]], 
                rv$seedList[[(rv$n_sims - num_sims) + x]]),
              "velocity"))[[1]]
            sigma <- suppressWarnings(extract_pars(
              emulate_seeded(
                rv$meanfitList[[group]], 
                rv$seedList[[(rv$n_sims - num_sims) + x]]),
              "sigma"))[[1]]
            
          } else {
            tau_p <- rv$tau_p[[group]]
            tau_v <- rv$tau_v[[group]]
            sigma <- rv$sigma[[group]]
          }
          
          rv$dev$tbl <<- rbind(
            rv$dev$tbl,
            .build_tbl(
              device = rv$device_type,
              group = if (rv$grouped) group else NA,
              data = simList[[x]],
              seed = names(simList)[[x]],
              obj = simfitList[[x]],
              tau_p = tau_p,
              tau_v = tau_v,
              sigma = sigma))
        })
        
      } else {
        
        for (i in seq_along(simList)) {
          msg_log(
            style = "warning",
            message = paste0("Model fit for sim no. ", num_sims + 1,
                             " ", msg_warning("in progress"), ","),
            detail = "Please wait for model selection to finish:")
          
          start_i <- Sys.time()
          fit <- par.ctmm.select(simList[i], guessList[i])
          time_i <- difftime(Sys.time(), start_i, units = "secs")
          
          rv$simfitList[[length(rv$simfitList) + 1]] <- fit
          rv$dev$N1 <- c(rv$dev$N1, extract_dof(fit, "area"))
          rv$dev$N2 <- c(rv$dev$N2, extract_dof(fit, "speed"))
          
          nm <- names(rv$simList)[[(rv$n_sims - num_sims) + i]]
          
          group <- 1
          if (rv$grouped) {
            group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
          }
          
          if (rv$add_ind_var) {
            tau_p <- suppressWarnings(extract_pars(
              emulate_seeded(
                rv$meanfitList[[group]], 
                rv$seedList[[(rv$n_sims - num_sims) + x]]),
              "position"))[[1]]
            tau_v <- suppressWarnings(extract_pars(
              emulate_seeded(
                rv$meanfitList[[group]], 
                rv$seedList[[(rv$n_sims - num_sims) + x]]),
              "velocity"))[[1]]
            sigma <- suppressWarnings(extract_pars(
              emulate_seeded(
                rv$meanfitList[[group]], 
                rv$seedList[[(rv$n_sims - num_sims) + x]]),
              "sigma"))[[1]]
            
          } else {
            tau_p <- rv$tau_p[[group]]
            tau_v <- rv$tau_v[[group]]
            sigma <- rv$sigma[[group]]
          }
          
          rv$dev$tbl <<- rbind(
            rv$dev$tbl,
            .build_tbl(
              device = rv$device_type,
              group = if (rv$grouped) group else NA,
              data = simList[[i]], 
              seed = rv$seedList[[(rv$n_sims - num_sims) + i]],
              obj = fit,
              tau_p = tau_p,
              tau_v = tau_v,
              sigma = sigma))
          
          msg_log(
            style = "warning",
            message = paste0("Model fit for sim no. ", i + 1, " ",
                             msg_success("completed"), "..."),
            run_time = time_i)
        }
      }
      
      rv$m$needs_fit <- FALSE
      fit_time <- difftime(Sys.time(), start, units = "secs")
      msg_log(
        style = 'success',
        message = paste0("Model selection for ", num_sims,
                         " simulation(s) ",
                         msg_success("completed"), "."),
        run_time = fit_time)
      
      rv$m$proceed <- TRUE
      
      shinyFeedback::showToast(
        type = "success",
        message = "Simulations completed!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"
        )
      )
      
      shinybusy::remove_modal_spinner()
      
    }, label = "o-m_sims_fit") %>% # end of observe,
      bindEvent(rv$m$needs_fit)
    
    ## Run multiple simulations (minimum number of tags): -----------------
    
    observe({
      req(rv$which_question,
          rv$which_meta != "none",
          rv$which_m == "get_m")
      req(rv$datList,
          rv$dur, rv$dti,
          rv$dev$is_valid,
          rv$simList,
          input$error_threshold)
      
      if (rv$data_type != "simulated") 
        req(rv$fitList) else req(rv$modList)
      
      if ("compare" %in% rv$which_meta) 
        req((rv$n_sims - 2) > 0) else req((rv$n_sims - 1) > 0)
      
      out_meta <- outList <- NULL
      n <- est <- error_sd <- groups <- NULL
      
      num_sims <- length(rv$simList)
      seq_for <- (num_sims + 1):rv$n_sims
      rv$m$needs_fit <- FALSE
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = tagList(span(
          style = "font-size: 18px;",
          span("Simulating multiple", style = "color: #797979;"),
          wrap_none(span("datasets", class = "cl-sea"),
                    span("...", style = "color: #797979;")))))
      
      shinyFeedback::showToast(
        type = "info",
        message = paste0("Simulating multiple datasets..."),
        .options = list(
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"))
      
      m_max <- input$nsims_max
      m_available <- length(rv$simList)
      
      if (m_max == 0) {
        msg_log(
          style = "error",
          message = paste0(
            "Simulations are already  ", msg_danger("available"), ","),
          detail = "Restart from sampling design tab.")
        
        shinybusy::remove_modal_spinner()  
      }
      req(m_max > 0)
      
      msg_log(
        style = "warning",
        message = paste0("Simulations ",
                         msg_warning("in progress"), "..."))
      
      # Initialize values:
      
      trace <- TRUE
      threshold <- input$error_threshold/100
      has_groups <- rv$grouped
      
      m_seq <- 1
      iter_step <- ifelse(
        is.null(input$nsims_iter), 2, input$nsims_iter)
      m_seq <- .get_sequence(seq_len(m_max),
                             .step = iter_step,
                             .max_m = m_max,
                             .automate_seq = TRUE,
                             grouped = has_groups)
      
      start_time <- Sys.time()
      
      history <- data.frame(
        m = rep(NA_real_, length(m_seq)),
        err = rep(NA_real_, length(m_seq)),
        error_ok = rep(NA, length(m_seq)),
        all_error_ok = rep(NA, length(m_seq)),
        is_stable = rep(NA, length(m_seq)),
        has_converged = rep(NA, length(m_seq)),
        lci_within = rep(NA, length(m_seq)),
        uci_within = rep(NA, length(m_seq)),
        overlaps_with_zero = rep(NA, length(m_seq)))
      
      replicate_dt <- data.frame(
        set = integer(0),
        rep = integer(0),
        seed = numeric(0))
      
      broke <- FALSE
      for (i in seq_along(m_seq)) {
        
        start_set <- Sys.time()
        m_current <- m_seq[[i]]
        
        if (trace) shinyFeedback::showToast(
          type = "info",
          message = paste0("Set ", i, 
                           " out of ", length(m_seq), "..."),
          .options = list(
            progressBar = FALSE,
            closeButton = TRUE,
            preventDuplicates = TRUE,
            positionClass = "toast-bottom-right"))
        
        if (trace) msg_log(
          style = "warning",
          message = paste0("Set ", i,
                           " out of ", length(m_seq), " ",
                           msg_warning("in progress"), ","),
          detail = "or until outputs are stable and within threshold.")
        
        for (rep in seq_len(rv$n_replicates)) {
          
          init_m <- NULL
          
          if (rep == 1 && !has_groups && length(rv$simList) == 1) {
            init_m <- m_seq[[i]] - length(rv$simList)
          }
          if (rep == 1 && has_groups && length(rv$simList) == 2) {
            if (m_seq[[i]] == 2) next
            init_m <- iter_step / (length(rv$simList))
            if (init_m == 0) next
          }
          
          if (trace) writeLines(paste0(
            crayon::yellow("\u2015\u2015\u2015\u2015\u2015\u2015"),
            " Replication ", crayon::yellow(rep), " out of ",
            crayon::yellow(rv$n_replicates)))
          
          if ((i == 1 && m_seq[[i]] == 2) |
              (i == 1 && m_seq[[i]] == 3)) {
            .md_add_m(rv,
                      m = m_seq[[i]],
                      init_m = init_m,
                      has_groups = has_groups,
                      trace = TRUE)
          } else {
            .md_add_m(rv,
                      m = iter_step,
                      init_m = init_m,
                      has_groups = has_groups,
                      trace = TRUE)
          }
          
          replicate_dt <- rbind(
            replicate_dt,
            data.frame(set = m_current,
                       rep = rep,
                       seed = tail(unlist(rv$seedList),
                                   iter_step)))
          
          .msg_time(start_set, "Elapsed time: ")
          
        } # end of [rep] loop
        
        # Run meta-analyses:
        
        dt_meta <- suppressWarnings(
          run_meta_resamples(rv,
                             set_target = rv$set_target,
                             subpop = has_groups,
                             randomize = TRUE,
                             max_draws = 30,
                             .m = m_seq[i],
                             .only_max_m = TRUE))
        
        if (has_groups) {
          data <- dt_meta[dt_meta$group != "All", ]
        } else {
          data <- dt_meta[dt_meta$group == "All", ]
        }
        
        # Compute convergence diagnostics:
        
        .n_converge <- rv$n_replicates - 1
        .tol <- 0.05
        
        diag <- data %>%
          dplyr::arrange(.data$type, 
                         .data$group,
                         .data$sample) %>%
          dplyr::group_by(.data$type, .data$group) %>%
          dplyr::mutate(
            cummean = cumsum(.data[["error"]]) /
              dplyr::row_number()) %>%
          dplyr::mutate(
            delta_cummean = abs(.data$cummean - 
                                  dplyr::last(.data$cummean))) %>%
          dplyr::summarise(
            n_steps = length(.data$cummean),
            has_enough_reps = .data$n_steps >= .n_converge,
            deltas = list(.data$delta_cummean),
            recent_cummean = list(tail(
              .data$delta_cummean, .n_converge)),
            recent_deltas = if (.data$has_enough_reps)
              list(tail(
                .data$delta_cummean, .n_converge)) else list(NA),
            is_stable = if (.data$has_enough_reps)
              all(abs(unlist(.data$recent_deltas)) < .tol) else FALSE,
            is_within_threshold = .data$has_enough_reps & {
              w <- unlist(.data$recent_cummean)
              ok <- abs(tail(w, 1)) <= error_threshold
              streak_n <- ceiling(.n_converge * 0.6)
              streak_ok <- all(abs(
                tail(w, streak_n)) <= error_threshold)
              ok & streak_ok
            },
            idx_stable_start = .find_stable(
              .data$delta_cummean, .tol, .n_converge),
            .groups = "drop") %>%
          dplyr::mutate(
            has_converged = .data$n_steps >= 5 &
              .data$is_stable &
              .data$is_within_threshold) %>%
          dplyr::arrange(dplyr::desc(.data$type))
        
        diag$is_stable
        diag$is_within_threshold
        diag$has_converged
        
        dt_means <- .summarize_error(dt_meta, conf_level = 0.95,
                                     error_threshold = error_threshold)
        
        if (has_groups) {
          dt_means <- dt_means[dt_means$group != "All", ]
        } else {
          dt_means <- dt_means[dt_means$group == "All", ]
        }
        
        all_broke_when <- dt_means %>%
          dplyr::mutate(m = as.numeric(as.character(.data$m))) %>%
          dplyr::inner_join(
            data %>%
              dplyr::mutate(m = as.numeric(as.character(.data$m))) %>%
              dplyr::group_by(.data$type, .data$group, .data$m) %>%
              dplyr::summarize(
                all_within_threshold = all(
                  abs(.data$error) <= error_threshold),
                .groups = "drop"),
            by = c("type", "group", "m")) %>%
          dplyr::filter(.data$overlaps == TRUE,
                        .data$all_within_threshold == TRUE) %>%
          dplyr::filter(.data$type == rv$set_target) %>%
          dplyr::ungroup()
        
        broke_when <- dt_means %>%
          dplyr::filter(abs(.data$error) <= error_threshold) %>%
          dplyr::arrange(.data$m, .by_group = TRUE) %>%
          dplyr::filter(.data$type == rv$set_target) %>%
          dplyr::ungroup()
        
        error <- dt_means %>%
          dplyr::filter(.data$type == rv$set_target) %>%
          .summarize_error(error_threshold = error_threshold) %>%
          dplyr::slice_max(.data$m) %>%
          dplyr::pull(.data$error)
        
        error_ok <- max(abs(error)) <= error_threshold
        all_error_ok <- !all(is.na(all_broke_when$m))
        if (length(all_error_ok) == 0) all_error_ok <- FALSE
        
        history$m[i] <- m_current
        history$err[i] <- error[which.max(abs(error))]
        history$error_ok[i] <- error_ok
        history$all_error_ok[i] <- all_error_ok
        history$is_stable[i] <- all(diag$is_stable)
        history$has_converged[i] <- all(diag$has_converged)
        
        if (trace) {
          if (!has_groups) {
            message(paste(
              .color("\u2015\u2015\u2015", error_ok),
              "Current sampled population:", 
              .color(m_current, error_ok),
              "individual(s)"))
          } else {
            message(paste(
              .color("\u2015\u2015\u2015", error_ok),
              "Current sampled population:", 
              .color(m_current, error_ok),
              "individual(s) per group"))
          }
          
          .format_error_ci <- function(x, threshold) {
            sprintf(
              "%s [%s, %s%%]",
              .color_error(x$error, threshold),
              round(x$pred_lci * 100, 1),
              round(x$pred_uci * 100, 1))
          }
          
          out_error <- dt_means %>%
            dplyr::filter(.data$type == rv$set_target) %>%
            dplyr::slice_max(.data$m) %>%
            dplyr::select(.data$error, .data$pred_lci, .data$pred_uci)
          
          if (has_groups) {
            message(paste(
              .color("\u2015\u2015\u2015", error_ok),
              "Current mean relative error for group A:",
              .format_error_ci(
                dplyr::slice(out_error, 1), error_threshold)
            ))
            
            message(paste(
              .color("\u2015\u2015\u2015", error_ok),
              "Current mean relative error for group B:",
              .format_error_ci(
                dplyr::slice(out_error, 2), error_threshold)
            ))
            
          } else {
            message(paste(
              .color("\u2015\u2015\u2015", error_ok),
              "Current mean relative error:",
              .format_error_ci(out_error, error_threshold)
            ))
          }
        }
        
        if (trace) msg_log(
          style = 'warning',
          message = paste0("Estimation for set no. ", i, " ",
                           msg_success("completed"), "..."),
          run_time = difftime(Sys.time(), start_set, units = "secs"))
        
        shinyFeedback::showToast(
          type = "success",
          message = paste0("Set ", i, " out of ",
                           length(m_seq), " completed."),
          .options = list(
            progressBar = FALSE,
            closeButton = TRUE,
            preventDuplicates = TRUE,
            positionClass = "toast-bottom-right"))
        
        # Break conditions:
        
        if (rv$which_meta == "mean") {
          
          dt_ci <- dt_means %>%
            dplyr::filter(.data$type == rv$set_target) %>%
            dplyr::filter(.data$group == "All") %>%
            dplyr::mutate(
              lci_within = .data$pred_lci >= -error_threshold,
              uci_within = .data$pred_uci <= error_threshold,
              overlaps_zero = .data$pred_lci <= 0 &
                .data$pred_uci >= 0) %>%
            dplyr::select("type", "group", "m",
                          "error", "error_lci", "error_uci",
                          "lci_within",
                          "uci_within",
                          "overlaps_zero")
          
          history$lci_within[i] <- dt_ci$lci_within
          history$uci_within[i] <- dt_ci$uci_within
          history$overlaps_with_zero[i] <- dt_ci$overlaps_zero
          
          if (error_ok && all_error_ok && diag$has_converged) {
            if (dt_ci$lci_within && dt_ci$uci_within) {
              writeLines(c(paste0(
                crayon::cyan("\u2015\u2015\u2015"),
                crayon::cyan(
                  " Convergence reached. "),
                "Stopping loop early at m = ",
                crayon::cyan(m_current), ".")))
              rv$n_tags_current <- m_current
              broke <- TRUE
              break
            }
          }
          
        } # end of if (rv$which_meta == "mean")
        
        if (rv$which_meta == "compare") {
          
          dt_ci <- dt_means %>%
            dplyr::filter(.data$type == rv$set_target) %>%
            dplyr::filter(.data$group != "All") %>%
            dplyr::mutate(
              lci_within = .data$pred_lci >= -error_threshold,
              uci_within = .data$pred_uci <= error_threshold,
              overlaps_zero = .data$pred_lci <= 0 &
                .data$pred_uci >= 0) %>%
            dplyr::select("type", "group", "m",
                          "error", "error_lci", "error_uci",
                          "lci_within",
                          "uci_within",
                          "overlaps_zero")
          
          history$lci_within[i] <- dt_ci$lci_within[
            which.max(abs(dt_ci$lci_within))]
          history$uci_within[i] <- dt_ci$uci_within[
            which.max(abs(dt_ci$uci_within))]
          history$overlaps_with_zero[i] <- all(dt_ci$overlaps_zero)
          
          cov <- Inf
          if (all(error_ok) && all(all_error_ok) &&
              all(diag$has_converged)) {
            
            cov_list <- lapply(rv$set_target, function(target) {
              
              tmp_dt_meta <- dplyr::filter(dt_meta, .data$type == target)
              if (!is.na(tmp_dt_meta[nrow(tmp_dt_meta), ]$est)) {
                cov <- out_meta[[target]][["All"]]$meta[
                  grep("CoV", rownames(
                    out_meta[[target]][["All"]]$meta)), 2][[2]]
                return(cov)
              }
              
              return(NULL)
              
            }) # end of lapply
            
            overlaps_with_truth <- FALSE
            if (!is.null(out_meta[[rv$set_target]][["groups"]])) {
              
              meta_truth <- rv$metaList_groups[[1]][[rv$set_target]]
              overlaps_with_truth <- dplyr::between(
                .get_ratios(out_meta[[rv$set_target]][["groups"]])$est,
                .get_ratios(meta_truth)$lci, 
                .get_ratios(meta_truth)$uci)
            }
            
            cov <- cov_list[[rv$set_target]]
            
            # if cov -> infinity,
            # still sensitive to small changes in the mean.
            if (!is.infinite(cov) && overlaps_with_truth) {
              writeLines(c(paste0(
                crayon::cyan("\u2015\u2015\u2015"),
                crayon::cyan(
                  " Convergence reached. "),
                "Stopping loop early at m = ",
                crayon::cyan(m_current), ".")))
              rv$n_tags_current <- m_current
              broke <- TRUE
              break
            }
          }
          
        } # end of if (rv$which_meta == "compare")
        
      } # end of while()
      
      truthList_inds <- .get_expected_values(
        rv, rv$set_target, summarized = FALSE)
      
      if ("Home range" %in% rv$which_question) {
        rv$truth$hr <- truthList_inds[[rv$set_target]]
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        rv$truth$ctsd <- truthList_inds[[rv$set_target]]
      }
      
      rv$dev$n <- lapply(seq_along(rv$simList), function(x)
        nrow(rv$simList[[x]]))
      
      for (i in seq_for) {
        if (i > length(rv$simfitList)) next
        
        N1 <- N2 <- NULL
        nm <- names(rv$simList)[[i]]
        seed <- as.character(nm)
        
        group <- 1
        if (has_groups) group <- ifelse(
          nm %in% rv$groups[[2]]$A, "A", "B")
        
        if ("Home range" %in% rv$which_question) {
          
          truth <- rv$truth$hr[[seed]]$area
          N1 <- extract_dof(rv$akdeList[[i]], "area")[[1]]
          
          if (is.null(N1)) {
            out_est <- rep(NA, 3) 
            out_err <- rep(NA, 3)
            tmpunit <- NA
            
          } else if (N1 < 0.001) {
            out_est <- rep(NA, 3) 
            out_err <- rep(NA, 3)
            tmpunit <- NA
            
          } else {
            tmpsum <- summary(rv$akdeList[[i]])
            tmpname <- rownames(summary(rv$akdeList[[i]])$CI)
            tmpunit <- extract_units(tmpname[grep("^area", tmpname)])
            
            out_est <- c(
              "lci" = tmpsum$CI[1], 
              "est" = tmpsum$CI[2], 
              "uci" = tmpsum$CI[3]) 
            out_err <- c( 
              "lci" = ((out_est[[1]] %#% tmpunit) - truth) / truth, 
              "est" = ((out_est[[2]] %#% tmpunit) - truth) / truth, 
              "uci" = ((out_est[[3]] %#% tmpunit) - truth) / truth) 
          }
          
          out_est_df <- data.frame(
            seed = seed,
            lci = out_est[[1]], 
            est = out_est[[2]], 
            uci = out_est[[3]], 
            unit = tmpunit)
          
          out_err_df <- data.frame(
            seed = seed,
            lci = out_err[[1]], 
            est = out_err[[2]], 
            uci = out_err[[3]])
          
        } # end of if (hr)
        
        if ("Speed & distance" %in% rv$which_question) {
          
          truth <- rv$truth$ctsd[[seed]]
          N2 <- extract_dof(rv$ctsdList[[i]], "speed")[[1]]
          
          if (N2 < 0.001) {
            out_est <- rep(NA, 3) 
            out_err <- rep(NA, 3)
            tmpunit_speed <- NA
            
            out_dist_est <- rep(NA, 3)
            out_dist_err <- rep(NA, 3)
            tmpunit_dist <- NA
            
          } else {
            tmpsum <- rv$ctsdList[[i]]
            tmpname <- rownames(tmpsum$CI)
            tmpunit_speed <- extract_units(
              tmpname[grep("speed", tmpname)])
            
            out_est <- c(
              "lci" = tmpsum$CI[1], 
              "est" = tmpsum$CI[2], 
              "uci" = tmpsum$CI[3]) 
            out_err <- c( 
              "lci" = ((out_est[[1]] %#%
                          tmpunit_speed) - truth) / truth, 
              "est" = ((out_est[[2]] %#%
                          tmpunit_speed) - truth) / truth, 
              "uci" = ((out_est[[3]] %#%
                          tmpunit_speed) - truth) / truth) 
            
            if (is.null(rv$pathList[[i]])) {
              out_dist_est <- rep(NA, 3)
              out_dist_err <- rep(NA, 3)
              tmpunit_dist <- NA
            } else {
              dur_days <- "days" %#% rv$dur$value %#% rv$dur$unit
              truth_dist <- sum(rv$pathList[[i]]$dist, na.rm = TRUE)
              out_dist_est <- c(
                "lci" = ("kilometers/day" %#% out_est[[1]]
                         %#% tmpunit_speed) * dur_days, 
                "est" = ("kilometers/day" %#% out_est[[2]]
                         %#% tmpunit_speed) * dur_days, 
                "uci" = ("kilometers/day" %#% out_est[[3]]
                         %#% tmpunit_speed) * dur_days)
              
              tmpunit_dist <- "kilometers"
              truth_dist <- tmpunit_dist %#% truth_dist
              
              out_dist_err <- c( 
                "lci" = (out_dist_est[[1]] - truth_dist) / truth_dist, 
                "est" = (out_dist_est[[2]] - truth_dist) / truth_dist, 
                "uci" = (out_dist_est[[3]] - truth_dist) / truth_dist) 
            }
          }
          
          out_est_df <- data.frame(
            seed = seed,
            lci = out_est[[1]], 
            est = out_est[[2]], 
            uci = out_est[[3]], 
            unit = tmpunit_speed)
          
          out_err_df <- data.frame(
            seed = seed,
            lci = out_err[[1]], 
            est = out_err[[2]], 
            uci = out_err[[3]])
          
          out_dist_est_df <- data.frame(
            seed = nm,
            lci = out_dist_est[[1]], 
            est = out_dist_est[[2]], 
            uci = out_dist_est[[3]], 
            unit = tmpunit_dist)
          
          out_dist_err_df <- data.frame(
            seed = seed,
            lci = out_dist_err[[1]], 
            est = out_dist_err[[2]], 
            uci = out_dist_err[[3]])
          
        } # end of if (ctsd)
        
        if (rv$add_ind_var) {
          tau_p <- suppressWarnings(extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           rv$seedList[[i]]),
            "position"))[[1]]
          tau_v <- suppressWarnings(extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           rv$seedList[[i]]),
            "velocity"))[[1]]
          sigma <- suppressWarnings(extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           rv$seedList[[i]]),
            "sigma"))[[1]]
        } else {
          tau_p <- rv$tau_p[[group]]
          tau_v <- rv$tau_v[[group]]
          sigma <- rv$sigma[[group]]
        }
        
        if ("Home range" %in% rv$which_question) {
          
          rv$hrEst <<- rbind(rv$hrEst, out_est_df)
          rv$hrErr <<- rbind(rv$hrErr, out_err_df)
          
          rv$hr$tbl <<- tryCatch(
            rbind(
              rv$hr$tbl, 
              .build_tbl(
                target = "hr",
                group = if (has_groups) group else NA,
                data = rv$simList[[i]], 
                seed = names(rv$simList)[[i]],
                obj = rv$akdeList[[i]],
                tau_p = tau_p,
                tau_v = tau_v,
                sigma = sigma,
                area = out_est_df,
                area_error = out_err_df)),
            error = function(e) e)
        }
        
        if ("Speed & distance" %in% rv$which_question) {
          
          rv$speedEst <<- rbind(rv$speedEst, out_est_df)
          rv$speedErr <<- rbind(rv$speedErr, out_err_df)
          
          rv$distEst <<- rbind(rv$distEst, out_dist_est_df)
          rv$distErr <<- rbind(rv$distErr, out_dist_err_df)
          
          rv$sd$tbl <<- rbind(
            rv$sd$tbl,
            .build_tbl(
              target = "ctsd",
              group = if (rv$grouped) group else NA,
              data = rv$simList[[i]],
              seed = names(rv$simList)[[i]],
              obj = rv$ctsdList[[i]],
              tau_p = tau_p,
              tau_v = tau_v,
              sigma = sigma,
              speed = rv$speedEst[i, ],
              speed_error = rv$speedErr[i, ],
              distance = rv$distEst[i, ],
              distance_error = rv$distErr[i, ]))
        }
        
      } # end of [i] loop (individuals)
      
      if (rv$set_analysis == "hr") rv$hr_completed <- TRUE
      if (rv$set_analysis == "ctsd") rv$sd_completed <- TRUE
      rv$is_analyses <- TRUE
      rv$is_report <- FALSE
      
      if (broke) m_seq <- m_seq[m_seq <= m_current]
      rv$n_tags <- max(m_seq)
      
      metaList <- list()
      start_meta_total <- Sys.time()
      for (i in seq_along(m_seq)) {
        
        tmp <- lapply(
          seq_len(rv$n_replicates), function(x) {
            
            run_meta_resamples(rv,
                               set_target = rv$set_target,
                               subpop = rv$grouped,
                               .m = m_seq[[i]])
          })
        
        if (length(tmp) > 0) {
          metaList[[i]] <- data.table::rbindlist(
            tmp, fill = TRUE, idcol = "replicate")
        } else {
          metaList[[i]] <- data.table::data.table()
        }
      }
      
      rv$is_meta <- TRUE
      summary <- data.table::rbindlist(metaList, fill = TRUE)
      rv$meta_tbl_replicates <<- dplyr::distinct(summary)
      
      rv$meta_tbl <- run_meta_resamples(
        rv, set_target = rv$set_target,
        subpop = rv$grouped,
        randomize = FALSE,
        trace = FALSE,
        .max_m = max(m_seq),
        .automate_seq = TRUE,
        .seed = rv$seedInit)
      
      rv$metaEst <- NULL
      rv$metaErr <- NULL
      rv$metaEst_groups <- NULL
      rv$metaErr_groups <- NULL
      
      tmpsumm <- summary %>%
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
          lci = est - stats::qt(
            0.975, df = n - 1) * error_sd / sqrt(n),
          uci = est + stats::qt(
            0.975, df = n - 1) * error_sd / sqrt(n),
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
      
      rv$seedList_replicates <<- c(
        rv$seedList_replicates, rv$seedList)
      
      seedList <- rv$seedList
      simList <- rv$simList
      simfitList <- rv$simfitList
      
      rv$dev$tbl <- NULL
      rv$hr$tbl <- NULL
      rv$sd$tbl <- NULL
      
      rv$hrEst <- NULL
      rv$hrErr <- NULL
      
      lapply(seq_along(simfitList), function(x) {
        
        group <- 1
        if (rv$grouped) {
          
          get_group <- function(seed, groups) {
            if (as.character(seed) %in% groups[["A"]]) {
              return("A") } else { return("B") }
          }
          
          group <- get_group(seedList[[x]], groups[[2]])
        }
        
        if (rv$add_ind_var) {
          tau_p <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           seedList[[x]]),
            "position")[[1]]
          tau_v <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           seedList[[x]]),
            "velocity")[[1]]
          sigma <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           seedList[[x]]),
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
            data = simList[[x]],
            seed = seedList[[x]],
            obj = simfitList[[x]],
            tau_p = tau_p,
            tau_v = tau_v,
            sigma = sigma))
      })
      
      rv$report_dev_yn <- TRUE
      
      if ("hr" %in% rv$set_target) {
        
        akdeList <- rv$akdeList
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
        
        rv$hrEst <<- rbind(rv$hrEst, out_est_df)
        rv$hrErr <<- rbind(rv$hrErr, out_err_df)
        
        rv$hr_completed <- TRUE
      }
      
      if ("ctsd" %in% rv$set_target) {
        
        rv$sd_completed <- TRUE
        ctsdList <- rv$ctsdList
        
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
          
          # If speed() returns NULL (multiple simulation)
          if (is.null(sdList)) {
            out_est_df <- out_est_df %>%
              dplyr::add_row(seed = seedList[[i]],
                             lci = NA, est = NA, uci = NA, unit = NA)
            out_err_df <- out_err_df %>%
              dplyr::add_row(seed = seedList[[i]],
                             lci = NA, est = NA, uci = NA)
            next
          }
          
          if ("CI" %in% names(sdList))
            sdList <- sdList$CI
          
          # If speed() returns Inf
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
        
        rv$speedEst <- out_est_df
        rv$speedErr <- out_err_df
        
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
        
        rv$distEst <- out_dist_est_df
        rv$distErr <- out_dist_err_df
        
        rv$sd$tbl <- NULL
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
      
      rv$dev$tbl <- rv$dev$tbl %>%
        dplyr::left_join(replicate_dt, by = c("seed" = "seed")) %>%
        dplyr::rename(replicate = rep)
      if ("hr" %in% rv$set_target)
        rv$hr$tbl <- rv$hr$tbl %>%
        dplyr::left_join(replicate_dt, by = c("seed" = "seed")) %>%
        dplyr::rename(replicate = rep)
      if ("ctsd" %in% rv$set_target)
        rv$sd$tbl <- rv$sd$tbl %>%
        dplyr::left_join(replicate_dt, by = c("seed" = "seed")) %>%
        dplyr::rename(replicate = rep)
      
      rv$dev$tbl <- dplyr::distinct(rv$dev$tbl)
      if ("hr" %in% rv$set_target)
        rv$hr$tbl <- dplyr::distinct(rv$hr$tbl)
      if ("ctsd" %in% rv$set_target)
        rv$sd$tbl <- dplyr::distinct(rv$sd$tbl)
      
      datList <- truthList <- NULL
      lists <- .build_meta_objects(rv,
                                   set_target = rv$set_target,
                                   subpop = rv$grouped,
                                   trace = FALSE)
      list2env(lists, envir = environment())
      
      metaList <- list()
      if ("Home range" %in% rv$which_question) {
        metaList[["hr"]] <- outList[["All"]][["hr"]] }
      if ("Speed & distance" %in% rv$which_question) {
        metaList[["ctsd"]] <- outList[["All"]][["ctsd"]] }
      
      if (rv$grouped) {
        
        datList_groups <- list()
        
        if ("Home range" %in% rv$which_question) {
          datList_groups <- datList[["groups"]][["hr"]]
          rv$metaList_groups[[2]][["hr"]] <- 
            outList[["groups"]][["hr"]]
        }
        
        if ("Speed & distance" %in% rv$which_question) {
          datList_groups <- .get_groups(rv$ctsdList, rv$groups[[2]])
          rv$metaList_groups[[2]][["ctsd"]] <-
            outList[["groups"]][["ctsd"]]
        }
        
      } # end of if (rv$grouped)
      
      
      for (i in seq_along(metaList)) {
        out <- metaList[[i]]
        
        name <- "mean"
        sum.obj <- out$meta
        nms.obj <- rownames(sum.obj)
        tmp <- sum.obj[grep(name, nms.obj), ]
        tmpunit <- extract_units(nms.obj[grep(name, nms.obj)])
        
        if (out$type == "hr") {
          truth_summarized <- get_true_hr(
            sigma = rv$sigma,
            
            ind_var = rv$add_ind_var,
            fit = if (rv$add_ind_var) rv$meanfitList else NULL,
            
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL,
            summarized = TRUE)
          truth <- truth_summarized[["All"]]$area
        }
        
        if (out$type == "ctsd") {
          truth_summarized <- get_true_speed(
            data = rv$simList,
            seed = rv$seedList,
            
            tau_p = rv$tau_p,
            tau_v = rv$tau_v,
            sigma = rv$sigma,
            
            ind_var = rv$add_ind_var,
            fit = if (rv$add_ind_var) rv$meanfitList else NULL,
            
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL,
            
            summarized = TRUE)
          truth <- truth_summarized[["All"]]
        }
        
        rv$metaEst <<- rbind(rv$metaEst, data.frame(
          type = out$type,
          group = "All",
          "lci" = tmp[[1]],
          "est" = tmp[[2]],
          "uci" = tmp[[3]],
          unit = tmpunit))
        
        rv$metaErr <<- rbind(rv$metaErr, data.frame(
          type = out$type,
          group = "All",
          "lci" = ((tmp[[1]] %#% tmpunit) - truth) / truth,
          "est" = ((tmp[[2]] %#% tmpunit) - truth) / truth,
          "uci" = ((tmp[[3]] %#% tmpunit) - truth) / truth))
      }
      
      if (rv$grouped) {
        for (target in rv$set_target) {
          out_groups <- rv$metaList_groups[[2]][[target]]
          
          sum.objA <- out_groups$meta$A
          sum.objB <- out_groups$meta$B
          nms.objA <- rownames(sum.objA)
          nms.objB <- rownames(sum.objB)
          
          tmpA <- sum.objA[grep(name, nms.objA), ]
          tmpB <- sum.objB[grep(name, nms.objB), ]
          tmpunitA <- extract_units(nms.objA[grep(name, nms.objA)])
          tmpunitB <- extract_units(nms.objB[grep(name, nms.objB)])
          
          if (out_groups$type == "hr") {
            
            truth_summarized <- get_true_hr(
              sigma = rv$sigma,
              ind_var = rv$add_ind_var,
              fit = if (rv$add_ind_var) rv$meanfitList else NULL,
              grouped = rv$grouped,
              groups = if (rv$grouped) rv$groups[[2]] else NULL,
              summarized = TRUE)
            
            truth_A <- truth_summarized[["A"]]$area
            truth_B <- truth_summarized[["B"]]$area
          }
          
          if (out_groups$type == "ctsd") {
            
            truth_summarized <- get_true_speed(
              data = rv$simList,
              seed = rv$seedList,
              
              tau_p = rv$tau_p,
              tau_v = rv$tau_v,
              sigma = rv$sigma,
              
              ind_var = rv$add_ind_var,
              fit = if (rv$add_ind_var) rv$meanfitList else NULL,
              
              grouped = rv$grouped,
              groups = if (rv$grouped) rv$groups[[2]] else NULL,
              
              summarized = TRUE)
            
            truth_A <- truth_summarized[["A"]]
            truth_B <- truth_summarized[["B"]]
          }
          
          rv$metaEst_groups <<- rbind(rv$metaEst_groups, data.frame(
            type = out_groups$type,
            group = "A",
            lci = tmpA[[1]],
            est = tmpA[[2]],
            uci = tmpA[[3]],
            unit = tmpunitA))
          rv$metaEst_groups <<- rbind(rv$metaEst_groups, data.frame(
            type = out_groups$type,
            group = "B",
            lci = tmpB[[1]],
            est = tmpB[[2]],
            uci = tmpB[[3]],
            unit = tmpunitB))
          
          rv$metaErr_groups <<- rbind(rv$metaErr_groups, data.frame(
            type = out_groups$type,
            group = "A",
            lci = ((tmpA[[1]] %#% tmpunitB) - truth_A) / truth_A,
            est = ((tmpA[[2]] %#% tmpunitB) - truth_A) / truth_A,
            uci = ((tmpA[[3]] %#% tmpunitB) - truth_A) / truth_A))
          rv$metaErr_groups <<- rbind(rv$metaErr_groups, data.frame(
            type = out_groups$type,
            group = "B",
            lci = ((tmpB[[1]] %#% tmpunitB) - truth_B) / truth_B,
            est = ((tmpB[[2]] %#% tmpunitB) - truth_B) / truth_B,
            uci = ((tmpB[[3]] %#% tmpunitB) - truth_B) / truth_B))
        }
      }
      
      rv$metaList <- metaList
      if (rv$grouped) rv$metaList_groups[[3]] <- TRUE
      
      msg_log(
        style = "success",
        message = paste0("Simulations ",
                         msg_success("completed"), "."),
        run_time = difftime(Sys.time(), start_time, units = "sec"))
      
      shinyFeedback::showToast(
        type = "success",
        message = "Simulations completed!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"
        )
      )
      
      shinybusy::remove_modal_spinner()
      
      # txt_reference <- tagList(
      #   h4(style = "margin-top: 30px;", "For more information:"),
      #   
      #   p(style = "font-family: var(--monosans);",
      #     "Silva, I., Fleming, C. H., Noonan, M. J.,",
      #     "Fagan, W. F. & Calabrese, J. M. (preprint). Too few,",
      #     "too many, or just right? Optimizing sample sizes for",
      #     "population-level inferences in animal tracking",
      #     "projects (10.1101/2025.07.30.667390v1)."))
      
      if (broke) {
        
        shiny::showModal(
          shiny::modalDialog(
            title = h4(span("Minimum", class = "cl-sea"),
                       "number of tags:"),
            
            fluidRow(
              style = paste("margin-right: 20px;",
                            "margin-left: 20px;"),
              
              tagList(
                p("You specified a maximum of", rv$n_sims, "tags.",
                  "Under the current assumptions and an error",
                  "threshold of",
                  wrap_none(rv$error_threshold * 100, "%,"),
                  "a stable estimate of the population mean",
                  "may be achieved by deploying",
                  rv$n_tags_current, "tags."),
                p("If the", span("recommended number of tags",
                                 style = "font-weight: bold;"),
                  "is close to (or equal to) the",
                  wrap_none(span("maximum number of tags",
                                 style = "font-weight: bold;"), ","),
                  "consider increasing the number of tags to reduce",
                  "uncertainty. For a more detailed evaluation,",
                  "explore the outputs in the",
                  shiny::icon("layer-group", class = "cl-sea"),
                  span("Meta-analyses", class = "cl-sea"), "tab.")) #,
              # txt_reference
              
            ), # end of fluidRow
            
            footer = modalButton("Dismiss"),
            size = "m")) # end of modal
        
      } else {
        
        shiny::showModal(
          shiny::modalDialog(
            title = h4(span("Minimum", class = "cl-sea"),
                       "number of tags:"),
            
            fluidRow(
              style = paste("margin-right: 20px;",
                            "margin-left: 20px;"),
              
              p("You specified a maximum of", rv$n_sims, "tags,",
                "which was not sufficient to achieve a stable",
                "estimate of the population mean",
                "within the threshold of", 
                wrap_none(rv$error_threshold * 100, "%."),
                br(),
                "If you wish to continue testing, please increase",
                "the", wrap_none(
                  span("maximum number of tags",
                       style = "font-weight: bold;"), ","),
                "or change sampling parameters in the",
                fontawesome::fa("stopwatch", fill = pal$sea),
                span("Sampling design", class = "cl-sea"), "tab.",
                "For a more detailed evaluation,",
                "explore the outputs in the",
                shiny::icon("layer-group", class = "cl-sea"),
                span("Meta-analyses", class = "cl-sea"), "tab.")
              
            ), # end of fluidRow
            
            footer = modalButton("Dismiss"),
            size = "m")) # end of modal
        
      }
      
    }, label = "o-m_sims_minimum_m") %>% # end of observer
      bindEvent(input$mButton_repeat)
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_comp_m_ui("comp_m_1")

## To be copied in the server
# mod_comp_m_server("comp_m_1")
