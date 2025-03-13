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
    
    # Number of simulations: --------------------------------------------
    
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
            maximumValue = 100,
            value = 2, wheelStep = 2),
          
          fluidRow(
            column(width = 12,
                   verbatimTextOutput(outputId = ns("txt_m_groups"))
            )),
          br(),
          
          shinyWidgets::numericInputIcon(
            inputId = ns("error_threshold"),
            label = "Error threshold:",
            min = 1,
            max = 50,
            value = 5,
            step = 1,
            icon = list(NULL, icon("percent"))),
          
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
mod_comp_m_server <- function(id, rv, set_analysis = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    rv$m <- reactiveValues(proceed = NULL, 
                           needs_fit = NULL, 
                           tmpList = NULL)
    
    observe({
      req(input$error_threshold)
      rv$error_threshold <- input$error_threshold/100
    })
    
    ## Simulating data: ---------------------------------------------------
    
    simulating_data <- reactive({
      
      dur <- rv$dur$value %#% rv$dur$unit
      dti <- rv$dti$value %#% rv$dti$unit
      t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      
      if (rv$data_type == "simulated") {
        fit <- fitA <- rv$modList[[1]]
        if (rv$grouped) fitB <- rv$modList[[2]]
      }
      
      if (rv$data_type != "simulated") {
        
        if (rv$is_emulate) {
          req(rv$meanfitList)
          fit <- emulate_seeded(rv$meanfitList[[1]], rv$seed0)
          if (length(fit$isotropic) > 1)
            fit$isotropic <- fit$isotropic[["sigma"]]
          
          # Recenter to 0,0:
          fit$mu[["x"]] <- 0
          fit$mu[["y"]] <- 0
          
        } else {
          fit <- prepare_mod(
            tau_p = rv$tau_p[[1]][2, ],
            tau_v = rv$tau_v[[1]][2, ],
            sigma = rv$sigma[[1]][2, ],
            mu = rv$mu[[1]])
          
        }
        
        if ("compare" %in% rv$which_meta) {
          req(rv$groups)
          
          if (rv$is_emulate) {
            fitA <- emulate_seeded(rv$meanfitList[["A"]], rv$seed0)
            fitB <- emulate_seeded(rv$meanfitList[["B"]], rv$seed0 + 1)
            if (length(fitA$isotropic) > 1)
              fitA$isotropic <- fitA$isotropic[["sigma"]]
            if (length(fitB$isotropic) > 1)
              fitB$isotropic <- fitB$isotropic[["sigma"]]
            
            # Recenter to 0,0:
            fitA$mu[["x"]] <- 0
            fitA$mu[["y"]] <- 0
            fitB$mu[["x"]] <- 0
            fitB$mu[["y"]] <- 0
            
          } else {
            fitA <- prepare_mod(
              tau_p = rv$tau_p[[2]][2, ],
              tau_v = rv$tau_v[[2]][2, ],
              sigma = rv$sigma[[2]][2, ],
              mu = rv$mu[[2]])
            
            fitB <- prepare_mod(
              tau_p = rv$tau_p[[3]][2, ],
              tau_v = rv$tau_v[[3]][2, ],
              sigma = rv$sigma[[3]][2, ],
              mu = rv$mu[[3]])
          }
        }
        
        # rv$modList <- list(fit)
      }
      
      if (rv$grouped) {
        # rv$modList_groups <- list(A = fitA, B = fitB)
        simA <- ctmm::simulate(fitA, t = t_new, seed = rv$seed0)
        simB <- ctmm::simulate(fitB, t = t_new, seed = rv$seed0 + 1)
        simA <- pseudonymize(simA)
        simB <- pseudonymize(simB)
        sim <- list(simA, simB)
        
        rv$groups[[2]][["A"]] <- c(as.character(rv$groups[[2]]$A),
                                   as.character(rv$seed0))
        rv$groups[[2]][["B"]] <- c(as.character(rv$groups[[2]]$B),
                                   as.character(rv$seed0 + 1))
        
        return(sim)
        
      } else {
        sim <- ctmm::simulate(fit, t = t_new, seed = rv$seed0)
        sim <- pseudonymize(sim)
        return(list(sim))
      }
      
    }) # end of reactive, simulating_data()
    
    ## Estimating time: ---------------------------------------------------
    
    estimating_time <- reactive({
      
      out_time <- guess_time(data = rv$simList, parallel = rv$parallel)
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
        rv$nsims <- as.numeric(input$nsims)
      } else if (rv$which_m == "get_m") {
        req(input$nsims_max)
        rv$nsims <- as.numeric(input$nsims_max)
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
      } else if (rv$which_m == "get_m") {
        shinyjs::hide(id = "nsims")
        shinyjs::show(id = "nsims_max")
      } else {
        shinyjs::hide(id = "nsims")
        shinyjs::hide(id = "nsims_max")
      }
      
      req(length(rv$simList) >= 2)
      wheel_step <- ifelse("compare" %in% rv$which_meta, 2, 1)
      
      if (rv$which_m == "set_m") {
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "nsims",
        label = "Number of tags (total):",
        value = length(rv$simList), 
        options = list(
          decimalPlaces = 0,
          minimumValue = 1,
          maximumValue = 100,
          wheelStep = wheel_step))
      }
      
      if (rv$which_m == "get_m") {
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "nsims_max",
        label = "Number of tags (maximum):",
        value = length(rv$simList),
        options = list(
          decimalPlaces = 0,
          minimumValue = 1,
          maximumValue = 100,
          wheelStep = wheel_step))
      }
      
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
      
      if ("compare" %in% rv$which_meta) req((rv$nsims - 2) > 0) 
      else req((rv$nsims - 1) > 0)
      
      rv$m$needs_fit <- FALSE
      
      start <- Sys.time()
      tmpList <- list()
      
      num_sims <- input$nsims - length(rv$simList)
      if (rv$grouped) num_sims <- num_sims / 2
      
      if (length(num_sims) == 0 || num_sims == 0) {
        shinybusy::remove_modal_spinner()
        
        # If more simulations are requested for both questions:
        # (case when simList is done, but ctsdList is not)
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
      
      for (i in seq_len(num_sims)) {
        
        rv$seed0 <- generate_seed(rv$seedList)
        simList <- simulating_data()
        
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
                if (!any(to_keep_vec == 0))
                  failure_occurred <- FALSE
                
                rv$dev_failed <- c(rv$dev_failed, failure_occurred)
                return(x[to_keep_vec == 1, ])
              } else {
                return(x)
              }
            })
            
          } # end of input$device_failure
        } else {
          rv$dev_failed <- c(rv$dev_failed, failure_occurred)
        }
        
        # If there is data loss:
        
        if (!is.null(rv$lost))
          if (rv$lost$perc > 0) {
            
            to_keep <- round(sapply(simList, function(x)
              nrow(x) * (1 - rv$lost$perc)))
            
            simList <- lapply(simList, function(x) {
              to_keep_vec <- sort(sample(1:nrow(x),
                                         to_keep, replace = FALSE))
              x[to_keep_vec, ] })
            
          } # end of data loss
        
        # If there are errors associated with each location:
        
        if (!is.null(rv$error))
          if (req(rv$error) > 0) {
            simList <- lapply(simList, function(x) {
              error_x <- stats::rnorm(nrow(x), mean = 0, sd =  rv$error)
              error_y <- stats::rnorm(nrow(x), mean = 0, sd =  rv$error)
              x[c("x", "y")] <- x[c("x", "y")] + c(error_x, error_y)
              return(x) })
            
          } # end of location error
        
        # Add to lists:
        
        if (rv$grouped) {
          tmpList <- c(tmpList, simList)
          tmpnames <- names(rv$simList)
          rv$simList <- c(rv$simList, simList)
          rv$seedList <- c(rv$seedList, rv$seed0, rv$seed0 + 1)
          names(rv$simList) <- c(tmpnames, rv$seed0, rv$seed0 + 1)
        } else {
          tmpList[[i]] <- simList[[1]]       
          tmpnames <- names(rv$simList)
          rv$simList[[length(rv$simList) + 1]] <- simList[[1]]
          rv$seedList[[length(rv$seedList) + 1]] <- rv$seed0
          names(rv$simList) <- c(tmpnames, rv$seed0)
        }
        
      } # end of for loop
      
      rv$tmpList <- tmpList
      rv$dev$n <- lapply(seq_along(rv$simList), function(x)
        n <- nrow(rv$simList[[x]]))
      
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
      guessList <- lapply(seq_along(simList), function (x)
        ctmm::ctmm.guess(simList[[x]],
                         interactive = FALSE))
      
      if (rv$parallel) {
        
        msg_log(
          style = "warning",
          message = paste0("Model selection for ", num_sims,
                           " simulation(s) (out of ", rv$nsims, ") ",
                           msg_warning("in progress"), ","),
          detail = "This may take a while...")
        
        current_dur <- rv$dur$value %#% rv$dur$unit
        optimal_dur <- (rv$tau_p[[1]]$value[2] %#%
                          rv$tau_p[[1]]$unit[2]) * 10
        
        current_dti <- rv$dti$value %#% rv$dti$unit
        optimal_dti <- (rv$tau_v[[1]]$value[2] %#%
                          rv$tau_v[[1]]$unit[2]) / 3
        
        if (optimal_dur <= current_dur && current_dti <= optimal_dti)
          simfitList <- tryCatch(
            par.ctmm.fit(simList, guessList, parallel = rv$parallel),
            error = function(e) e)
        else
          simfitList <- tryCatch(
            par.ctmm.select(simList, guessList, parallel = rv$parallel),
            error = function(e) e)
        
        if (num_sims == 1) {
          simfitList <- list(simfitList)
        }
        
        N_type <- ifelse(rv$set_analysis == "hr", "area", "speed")
        N <- extract_dof(simfitList, N_type)
        to_rerun <- which(N < 0.1)
        
        if (any(N < 0.1)) {
          for (z in seq_along(to_rerun)) {
            simfitList[[z]] <- par.ctmm.select(
              simList[to_rerun[[z]]], 
              guessList[to_rerun[[z]]],
              parallel = rv$parallel)
          }
        }
        
        lapply(seq_along(simfitList), function (x) {
          simfitList[[x]]$mu[[1, "x"]] <- 0
          simfitList[[x]]$mu[[1, "y"]] <- 0 # recenter to 0,0
        })
        
        rv$dev$N1 <- c(rv$dev$N1, extract_dof(simfitList, "area"))
        rv$dev$N2 <- c(rv$dev$N2, extract_dof(simfitList, "speed"))
        
        m <- length(rv$simfitList)
        rv$simfitList <- c(rv$simfitList, simfitList)
        names(rv$simfitList) <- names(rv$simList)
        
        lapply(seq_along(simList), function(x) {
          nm <- names(rv$simList)[[(rv$nsims - num_sims) + x]]
          
          group <- 1
          if (rv$grouped) {
            group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
          }
          
          if (rv$is_emulate) {
            tau_p <- extract_pars(
              emulate_seeded(rv$meanfitList[[group]], 
                             rv$seedList[[(rv$nsims - num_sims) + x]]),
              "position")[[1]]
            tau_v <- extract_pars(
              emulate_seeded(rv$meanfitList[[group]], 
                             rv$seedList[[(rv$nsims - num_sims) + x]]),
              "velocity")[[1]]
            sigma <- extract_pars(
              emulate_seeded(rv$meanfitList[[group]], 
                             rv$seedList[[(rv$nsims - num_sims) + x]]),
              "sigma")[[1]]
            
          } else {
            tau_p <- rv$tau_p[[group]]
            tau_v <- rv$tau_v[[group]]
            sigma <- rv$sigma[[group]]
          }
          
          newrow <- devRow(
            device = rv$device_type,
            group = if (rv$grouped) group else NA,
            
            data = simList[[x]], 
            seed = rv$seedList[[(rv$nsims - num_sims) + x]],
            fit = simfitList[[x]],
            
            tau_p = tau_p,
            tau_v = tau_v,
            sigma = sigma)
          
          rv$dev$tbl <<- rbind(rv$dev$tbl, newrow)
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
          
          nm <- names(rv$simList)[[(rv$nsims - num_sims) + i]]
          
          group <- 1
          if (rv$grouped) {
            group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
          }
          
          if (rv$is_emulate) {
            tau_p <- extract_pars(
              emulate_seeded(rv$meanfitList[[group]], 
                             rv$seedList[[(rv$nsims - num_sims) + x]]),
              "position")[[1]]
            tau_v <- extract_pars(
              emulate_seeded(rv$meanfitList[[group]], 
                             rv$seedList[[(rv$nsims - num_sims) + x]]),
              "velocity")[[1]]
            sigma <- extract_pars(
              emulate_seeded(rv$meanfitList[[group]], 
                             rv$seedList[[(rv$nsims - num_sims) + x]]),
              "sigma")[[1]]
            
          } else {
            tau_p <- rv$tau_p[[group]]
            tau_v <- rv$tau_v[[group]]
            sigma <- rv$sigma[[group]]
          }
          
          
          newrow <- devRow(
            device = rv$device_type,
            group = if (rv$grouped) group else NA,
            
            data = simList[[i]], 
            seed = rv$seedList[[(rv$nsims - num_sims) + i]],
            fit = fit,
            
            tau_p = tau_p,
            tau_v = tau_v,
            sigma = sigma)
          
          rv$dev$tbl <<- rbind(rv$dev$tbl, newrow)
          
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
    ### Home range area: --------------------------------------------------
    
    observe({
      req(rv$which_meta != "none",
          rv$which_question == "Home range",
          rv$which_m == "get_m")
      req(rv$datList,
          rv$dur, rv$dti,
          rv$dev$is_valid,
          rv$simList,
          input$error_threshold)
      
      if (rv$data_type != "simulated") req(rv$fitList)
      else req(rv$modList)
      
      if ("compare" %in% rv$which_meta) 
        req((rv$nsims - 2) > 0) else req((rv$nsims - 1) > 0)
      
      num_sims <- length(rv$simList)
      seq_for <- (num_sims + 1):rv$nsims
      rv$m$needs_fit <- FALSE
      
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
        message = paste0("Simulating multiple datasets..."),
        .options = list(
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right")
      )
      
      if (length(rv$simList) == 1) {
        m_max <- input$nsims_max
      } else {
        m_max <- input$nsims_max - length(rv$simList)
      }
      
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
      
      m <- 2
      m_sets <- 1
      if (m < m_max) m_sets <- seq(m, m_max, by = m)
      
      # Initialize values:
      threshold <- input$error_threshold/100 # default 5%
      err <- 1
      hex <- rep("grey50", 5)
      
      start_time <- Sys.time()
      trace <- TRUE
      dt_meta <- data.frame(
        "type" = character(0),
        "m" = numeric(0),
        "lci" = numeric(0),
        "est" = numeric(0),
        "uci" = numeric(0),
        "overlaps" = logical(0),
        "subpop" = logical(0),
        "group" = character(0))
      
      i <- 0
      broke <- FALSE
      while (i < length(m_sets)) {
        i <- i + 1
        start_time_i <- Sys.time()
        
        if (trace) shinyFeedback::showToast(
          type = "info",
          message = paste0("Set ", i, " out of ",
                           length(m_sets), "..."),
          .options = list(
            progressBar = FALSE,
            closeButton = TRUE,
            preventDuplicates = TRUE,
            positionClass = "toast-bottom-right"))
        
        if (trace) msg_log(
          style = "warning",
          message = paste0("Simulation set no. ", i,
                           " out of ", length(m_sets), " ",
                           msg_warning("in progress"), ","),
          detail = paste("or until error threshold is reached."))
        
        if (length(rv$simList) == 1) {
          # Running one extra simulation at the beginning:
          rv$seed0 <- generate_seed(rv$seedList)
          simList <- simulating_data()
          seedList <- list(rv$seed0)
          rv$seedList <- c(rv$seedList, rv$seed0)
        } else {
          if (rv$grouped) {
            rv$seed0 <- generate_seed(rv$seedList)
            simList <- simulating_data()
            seedList <- list(rv$seed0, rv$seed0 + 1)
            rv$seedList <- c(rv$seedList, rv$seed0, rv$seed0 + 1)
            
          } else {
            simList <- lapply(seq_len(m), function(x) {
              rv$seed0 <- generate_seed(rv$seedList)
              out <- simulating_data()[[1]]
              rv$seedList <- c(rv$seedList, rv$seed0)
              return(out) 
            })
            seedList <- utils::tail(rv$seedList, m)
          }
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
                if (!any(to_keep_vec == 0))
                  failure_occurred <- FALSE
                
                rv$dev_failed <- c(rv$dev_failed, failure_occurred)
                return(x[to_keep_vec == 1, ])
              } else {
                return(x)
              }
            })
            
          } # end of input$device_failure
        } else {
          rv$dev_failed <- c(rv$dev_failed, failure_occurred)
        }
        
        # If there is data loss:
        
        if (!is.null(rv$lost))
          if (rv$lost$perc > 0) {
            
            to_keep <- round(sapply(simList, function(x)
              nrow(x) * (1 - rv$lost$perc)))
            
            simList <- lapply(simList, function(x) {
              to_keep_vec <- sort(sample(1:nrow(x),
                                         to_keep, replace = FALSE))
              x[to_keep_vec, ] })
            
          } # end of input$device_fixsuccess
        
        # If there are errors associated with each location:
        
        if (!is.null(rv$error))
          if (req(rv$error) > 0) {
            simList <- lapply(simList, function(x) {
              error_x <- stats::rnorm(nrow(x), mean = 0, sd =  rv$error)
              error_y <- stats::rnorm(nrow(x), mean = 0, sd =  rv$error)
              x[c("x", "y")] <- x[c("x", "y")] + c(error_x, error_y)
              return(x) })
            
          } # end of input$device_error
        
        tmpnames <- names(rv$simList)
        rv$simList <- c(rv$simList, simList)
        if (rv$grouped) {
          names(rv$simList) <- c(tmpnames, rv$seed0, rv$seed0 + 1)
        } else {
          names(rv$simList) <- unlist(rv$seedList)
        }
        
        current_dur <- rv$dur$value %#% rv$dur$unit
        optimal_dur <- (rv$tau_p[[1]]$value[2] %#%
                          rv$tau_p[[1]]$unit[2]) * 10
        
        current_dti <- rv$dti$value %#% rv$dti$unit
        optimal_dti <- (rv$tau_v[[1]]$value[2] %#%
                          rv$tau_v[[1]]$unit[2]) / 3
        
        fitList <- lapply(seq_along(simList), function(x) {
          guess <- ctmm::ctmm.guess(simList[[x]], interactive = F)
          if (optimal_dur <= current_dur && current_dti <= optimal_dti)
            out <- ctmm::ctmm.fit(simList[[x]], guess, trace = F)
          else
            out <- ctmm::ctmm.select(simList[[x]], guess, trace = F)
          rv$simfitList <- c(rv$simfitList, list(out))
          return(out)
        })
        
        req(length(rv$simList) == length(rv$simfitList))
        
        akdeList <- lapply(seq_along(simList), function(x) {
          out <- tryCatch(
            ctmm::akde(simList[[x]], fitList[[x]]),
            warning = function(w) NULL,
            error = function(e) NULL)
          rv$akdeList <- c(rv$akdeList, list(out))
          return(out)
        })
        
        if (length(rv$simList) == length(rv$akdeList)) {
          names(rv$akdeList) <- names(rv$simList)
          in_list <- rv$akdeList
          in_list[sapply(in_list, is.null)] <- NULL
          out_meta <- .capture_meta(in_list,
                                   variable = "area",
                                   sort = TRUE,
                                   units = FALSE,
                                   verbose = TRUE,
                                   plot = FALSE)
          
          if (rv$which_meta == "compare") {
            
            out_meta_groups <- .capture_meta(
              .get_groups(rv$akdeList, rv$groups[[2]]),
              variable = "area",
              units = TRUE, 
              verbose = TRUE,
              plot = FALSE,
              type = "hr") %>% 
              suppressMessages() %>% 
              suppressWarnings() %>% 
              quiet()
          }
          
        } else {
          out_meta <- NULL
        }
        
        if (!is.null(out_meta)) {
          
          hex <- c(hex, ifelse(
            out_meta$logs$subpop_detected, pal$dgr, pal$sea))
          
          tmpname <- rownames(out_meta$meta)
          tmpunit <- extract_units(tmpname[grep("^mean", tmpname)])
          
          truth_summarized <- get_true_hr(
            sigma = rv$sigma,
            emulated = rv$is_emulate,
            fit = if (rv$is_emulate) rv$meanfitList else NULL,
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL,
            summarized = TRUE)
          truth <- truth_summarized[["All"]]$area
          
          out_est <- c(
            "lci" = out_meta$meta[1, 1] %#% tmpunit, 
            "est" = out_meta$meta[1, 2] %#% tmpunit, 
            "uci" = out_meta$meta[1, 3] %#% tmpunit) 
          out_err <- c( 
            "lci" = ((out_est[["lci"]] %#% tmpunit) - truth) / truth, 
            "est" = ((out_est[["est"]] %#% tmpunit) - truth) / truth, 
            "uci" = ((out_est[["uci"]] %#% tmpunit) - truth) / truth) 
          
          err <- out_err[["est"]]
          dt_meta <- dt_meta %>% dplyr::add_row(
            type = "hr",
            m = length(rv$simList),
            lci = out_err[[1]], est = out_err[[2]], uci = out_err[[3]],
            overlaps = dplyr::between(truth,
                                      out_meta$meta[1, 1],
                                      out_meta$meta[1, 3]),
            subpop = out_meta$logs$subpop_detected,
            group = "All")
        } else {
          err <- rv$err_prev[length(rv$err_prev)]
          dt_meta <- dt_meta %>% dplyr::add_row(
            type = "hr",
            m = length(rv$simList),
            lci = NA, est = NA, uci = NA,
            overlaps = NA,
            subpop = NA,
            group = "All")
        }
        
        if (trace) message(" - No. sims (total): ", length(rv$simList))
        if (trace) message(paste0(" - Error: ",
                                  round(abs(err) * 100, 1), "%"))
        
        if (trace) msg_log(
          style = 'warning',
          message = paste0("Estimation for set no. ", i, " ",
                           msg_success("completed"), "..."),
          run_time = difftime(Sys.time(), start_time_i, units = "secs"))
        
        shinyFeedback::showToast(
          type = "success",
          message = paste0("Set ", i, " out of ",
                           length(m_sets), " completed."),
          .options = list(
            progressBar = FALSE,
            closeButton = TRUE,
            preventDuplicates = TRUE,
            positionClass = "toast-bottom-right"))
        
        rv$err_prev <- c(rv$err_prev, abs(err))
        last_values <- (length(rv$err_prev)-4):length(rv$err_prev)
        
        if (rv$which_meta == "mean") {
          
          if (all(rv$err_prev[last_values] < threshold)) {
            if (!is.null(out_meta)) {
              
              means <- out_meta$meta[
                grep("mean", rownames(out_meta$meta)), ]
              tmpunit <- extract_units(rownames(out_meta$meta)[[1]])
              
              overlaps_with_truth <- dplyr::between(
                truth,
                means[1, "low"] %#% tmpunit,
                means[1, "high"] %#% tmpunit)
              
              if (overlaps_with_truth) {
                broke <- TRUE
                break
              }
            }
          }
        } # end of if (rv$which_meta == "mean")
        
        if (rv$which_meta == "compare") {
          
          cov <- Inf
          if (all(rv$err_prev[last_values] < threshold)) {
            if (!is.na(dt_meta[nrow(dt_meta), ]$est)) {
              cov <- out_meta$meta[
                grep("CoV", rownames(out_meta$meta)), 2][[2]]
            }
            
            if (!is.null(out_meta_groups)) {
              meta_truth <- rv$metaList_groups[[1]][["hr"]]
              overlaps_with_truth <- dplyr::between(
                .get_ratios(out_meta_groups)$est,
                .get_ratios(meta_truth)$lci, 
                .get_ratios(meta_truth)$uci)
            }
            
            # if cov -> infinity,
            # still sensitive to small changes in the mean.
            if (!is.infinite(cov) && overlaps_with_truth) {
              broke <- TRUE
              break
            }
          }
        } # end of if (rv$which_meta == "compare")
        
      } # end of while()
      
      truthList <- get_true_hr(
        data = rv$simList,
        seed = rv$seedList,
        sigma = rv$sigma,
        
        emulated = rv$is_emulate,
        fit = if (rv$is_emulate) rv$meanfitList else NULL,
        
        grouped = rv$grouped,
        groups = if (rv$grouped) rv$groups[[2]] else NULL)
      rv$truth$hr <- truthList
      
      rv$meta_tbl <<- rbind(rv$meta_tbl, dt_meta)
      rv$dev$n <- lapply(seq_along(rv$simList), function(x)
        n <- nrow(rv$simList[[x]]))
      
      for (i in seq_for) {
        if (i > length(rv$simfitList)) next
        
        group <- 1
        if (rv$grouped) {
          nm <- names(rv$simList)[[i]]
          group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
        }
        
        seed <- as.character(rv$seedList[[i]])
        truth <- rv$truth$hr[[seed]]$area
        N1 <- extract_dof(rv$simfitList[[i]], "area")[[1]]
        
        if (N1 < 0.001) {
          out_est <- rep(NA, 3) 
          out_err <- rep(NA, 3)
          tmpunit <- NA
        } else {
          tmpsum <- summary(rv$akdeList[[i]])
          tmpname <- rownames(summary(rv$akdeList[[i]])$CI)
          tmpunit <- extract_units(tmpname[grep('^area', tmpname)])
          
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
          seed = rv$seedList[[i]],
          lci = out_est[[1]], 
          est = out_est[[2]], 
          uci = out_est[[3]], 
          unit = tmpunit)
        out_err_df <- data.frame(
          seed = rv$seedList[[i]],
          lci = out_err[[1]], 
          est = out_err[[2]], 
          uci = out_err[[3]])
        
        if (rv$is_emulate) {
          tau_p <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]], 
                           rv$seedList[[i]]),
            "position")[[1]]
        } else {
          tau_p <- rv$tau_p[[group]]
        }
        
        rv$hr$tbl <<- rbind(
          rv$hr$tbl, 
          .build_tbl_hr(
            group = if (rv$grouped) group else NA,
            data = rv$simList[[i]], 
            seed = rv$seedList[[i]],
            obj = rv$simfitList[[i]],
            par = tau_p,
            area = out_est_df,
            error = out_err_df))
      }
      
      rv$hrEst <<- rbind(rv$hrEst, out_est_df)
      rv$hrErr <<- rbind(rv$hrErr, out_err_df)
      
      N1 <- extract_dof(rv$simfitList, "area")
      
      if (rv$set_analysis == "hr") rv$hr_completed <- TRUE
      if (rv$set_analysis == "ctsd") rv$sd_completed <- TRUE
      rv$is_analyses <- TRUE
      rv$is_report <- FALSE
      rv$is_meta <- FALSE
      
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
      
    }, label = "o-m_sims_minimum_hr") %>% # end of observer
      bindEvent(input$mButton_repeat)
    
    ### Speed & distance: -------------------------------------------------
    
    observe({
      req(rv$which_meta,
          rv$which_question == "Speed & distance",
          rv$which_m == "get_m")
      req(rv$datList,
          rv$dur, rv$dti,
          rv$dev$is_valid,
          rv$simList,
          input$error_threshold)
      
      if (rv$data_type != "simulated") req(rv$fitList)
      else req(rv$modList)
      
      if ("compare" %in% rv$which_meta)
        req((rv$nsims - 2) > 0) else req((rv$nsims - 1) > 0)
      
      num_sims <- length(rv$simList)
      seq_for <- (num_sims + 1):rv$nsims
      rv$m$needs_fit <- FALSE
      
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
        message = paste0("Simulating multiple datasets..."),
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
      
      if (length(rv$simList) == 1) {
        m_max <- input$nsims_max
      } else {
        m_max <- input$nsims_max - length(rv$simList)
      }
      req(m_max > 0)
      
      m <- 2
      m_sets <- 1
      if (m < m_max) m_sets <- seq(m, m_max, by = m)
      
      # Initialize values:
      threshold <- input$error_threshold/100 # default 5%
      err <- 1
      hex <- rep("grey50", 5)
      
      trace <- TRUE
      start_time <- Sys.time()
      dt_meta <- data.frame(
        "type" = character(0),
        "m" = numeric(0),
        "lci" = numeric(0),
        "est" = numeric(0),
        "uci" = numeric(0),
        "overlaps" = logical(0),
        "subpop" = logical(0),
        "group" = character(0))
      
      i <- 0
      broke <- FALSE
      while (i < length(m_sets)) {
        i <- i + 1
        start_time_i <- Sys.time()
        
        if (trace) shinyFeedback::showToast(
          type = "info",
          message = paste0("Set ", i, " out of ",
                           length(m_sets), "..."),
          .options = list(
            progressBar = FALSE,
            closeButton = TRUE,
            preventDuplicates = TRUE,
            positionClass = "toast-bottom-right"))
        
        if (trace) msg_log(
          style = "warning",
          message = paste0("Simulation set no. ", i,
                           " out of ", length(m_sets), " ",
                           msg_warning("in progress"), ","),
          detail = paste("or until error threshold is reached."))
        
        if (length(rv$simList) == 1) {
          
          # Running one extra simulation at the beginning:
          rv$seed0 <- generate_seed(rv$seedList)
          simList <- simulating_data()
          
          seedList <- list(rv$seed0)
          rv$seedList <- c(rv$seedList, rv$seed0)
          
        } else {
          
          if (rv$grouped) {
            rv$seed0 <- generate_seed(rv$seedList)
            simList <- simulating_data()
            
            seedList <- list(rv$seed0, rv$seed0 + 1)
            rv$seedList <- c(rv$seedList, rv$seed0, rv$seed0 + 1)
            
          } else {
            simList <- lapply(seq_len(m), function(x) {
              rv$seed0 <- generate_seed(rv$seedList)
              out <- simulating_data()[[1]]
              rv$seedList <- c(rv$seedList, rv$seed0)
              return(out) 
            })
            seedList <- utils::tail(rv$seedList, m)
          }
          
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
                if (!any(to_keep_vec == 0))
                  failure_occurred <- FALSE
                
                rv$dev_failed <- c(rv$dev_failed, failure_occurred)
                return(x[to_keep_vec == 1, ])
              } else {
                return(x)
              }
            })
            
          } # end of input$device_failure
        } else {
          rv$dev_failed <- c(rv$dev_failed, failure_occurred)
        }
        
        # If there is data loss:
        
        if (!is.null(rv$lost))
          if (rv$lost$perc > 0) {
            
            simList <- lapply(simList, function(x) {
              
              to_keep <- round(nrow(x) * (1 - rv$lost$perc), 0)
              to_keep_vec <- sort(sample(1:nrow(x),
                                         to_keep, replace = FALSE))
              x[to_keep_vec, ] })
            
          } # end of input$device_fixsuccess
        
        # If there are errors associated with each location:
        
        if (!is.null(rv$error))
          if (req(rv$error) > 0) {
            simList <- lapply(simList, function(x) {
              error_x <- stats::rnorm(nrow(x), mean = 0, sd =  rv$error)
              error_y <- stats::rnorm(nrow(x), mean = 0, sd =  rv$error)
              x[c("x", "y")] <- x[c("x", "y")] + c(error_x, error_y)
              return(x) })
            
          } # end of input$device_error
        
        tmpnames <- names(rv$simList)
        rv$simList <- c(rv$simList, simList)
        if (rv$grouped) {
          names(simList) <- c(rv$seed0, rv$seed0 + 1)
          names(rv$simList) <- c(tmpnames, rv$seed0, rv$seed0 + 1)
        } else {
          names(simList) <- unlist(seedList)
          names(rv$simList) <- unlist(rv$seedList)
        }
        
        current_dur <- rv$dur$value %#% rv$dur$unit
        optimal_dur <- (rv$tau_p[[1]]$value[2] %#%
                          rv$tau_p[[1]]$unit[2]) * 10
        
        current_dti <- rv$dti$value %#% rv$dti$unit
        optimal_dti <- (rv$tau_v[[1]]$value[2] %#%
                          rv$tau_v[[1]]$unit[2]) / 3
        
        fitList <- lapply(seq_along(simList), function(x) {
          guess <- ctmm::ctmm.guess(simList[[x]], interactive = F)
          if (optimal_dur <= current_dur && current_dti <= optimal_dti)
            out <- ctmm::ctmm.fit(simList[[x]], guess, trace = F)
          else
            out <- ctmm::ctmm.select(simList[[x]], guess, trace = F)
          return(out)
        })
        
        rv$simfitList <- c(rv$simfitList, fitList)
        req(length(rv$simList) == length(rv$simfitList))
        
        ctsdList <- par.speed(
          simList,
          fitList,
          seed = seedList,
          parallel = rv$parallel)
        rv$ctsdList <- c(rv$ctsdList, ctsdList)
        
        speedDatList <- lapply(seq_along(simList), function(x) {
          ctmm::speeds(simList[[x]], fitList[[x]], units = FALSE)
        })
        rv$speedDatList <- c(rv$speedDatList, speedDatList)
        
        pathList <- estimate_trajectory(
          data = simList,
          fit = fitList,
          groups = if (rv$grouped) rv$groups[[2]] else NULL,
          dur = rv$dur,
          tau_v = rv$tau_v,
          seed = seedList)
        rv$pathList <<- c(rv$pathList, pathList)
        
        if (length(rv$simList) == length(rv$ctsdList) &&
            length(rv$simList) == length(rv$pathList)) {
          names(rv$ctsdList) <- names(rv$simList)
          names(rv$pathList) <- names(rv$simList)
          
          in_list <- rv$ctsdList
          in_list[sapply(in_list, is.null)] <- NULL
          out_meta <- .capture_meta(in_list,
                                   variable = "speed",
                                   sort = TRUE,
                                   units = FALSE,
                                   verbose = TRUE,
                                   plot = FALSE)
          
          if (rv$which_meta == "compare") {
            
            out_meta_groups <- .capture_meta(
              .get_groups(rv$ctsdList, rv$groups[[2]]),
              variable = "speed",
              units = TRUE, 
              verbose = TRUE,
              plot = FALSE,
              type = "hr") %>% 
              suppressMessages() %>% 
              suppressWarnings() %>% 
              quiet()
          }
          
        } else {
          out_meta <- NULL
          out_meta_groups <- NULL
        }
        
        if (!is.null(out_meta)) {
          
          hex <- c(hex, ifelse(
            out_meta$logs$subpop_detected, pal$dgr, pal$sea))
          
          tmpname <- rownames(out_meta$meta)
          tmpunit <- extract_units(tmpname[grep("^mean", tmpname)])
          truth <- get_true_speed(
            data = rv$simList,
            seed = rv$seedList,
            
            tau_p = rv$tau_p,
            tau_v = rv$tau_v,
            sigma = rv$sigma,
            
            emulated = rv$is_emulate,
            fit = if (rv$is_emulate) rv$meanfitList else NULL,
            
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL,
            
            summarized = TRUE)[["All"]]
          
          out_est <- c(
            "lci" = out_meta$meta[1, 1] %#% tmpunit,
            "est" = out_meta$meta[1, 2] %#% tmpunit,
            "uci" = out_meta$meta[1, 3] %#% tmpunit)
          out_err <- c(
            "lci" = ((out_est[["lci"]] %#% tmpunit) - truth) / truth,
            "est" = ((out_est[["est"]] %#% tmpunit) - truth) / truth,
            "uci" = ((out_est[["uci"]] %#% tmpunit) - truth) / truth)
          
          err <- out_err[["est"]]
          dt_meta <- dt_meta %>% dplyr::add_row(
            type = "ctsd",
            m = length(rv$simList),
            lci = out_err[[1]],
            est = out_err[[2]],
            uci = out_err[[3]],
            overlaps = dplyr::between(truth,
                                      out_meta$meta[1, 1],
                                      out_meta$meta[1, 3]),
            subpop = out_meta$logs$subpop_detected,
            group = "All")
          
        } else {
          err <- rv$err_prev[length(rv$err_prev)]
          dt_meta <- dt_meta %>% dplyr::add_row(
            type = "hr",
            m = length(rv$simList),
            lci = NA, est = NA, uci = NA,
            overlaps = NA,
            subpop = NA,
            group = "All")
        }
        
        if (trace) message(" - No. sims (total): ", length(rv$simList))
        if (trace) message(paste0(" - Error: ",
                                  round(abs(err) * 100, 1), "%"))
        
        if (trace) msg_log(
          style = 'warning',
          message = paste0("Estimation for set no. ", i, " ",
                           msg_success("completed"), "..."),
          run_time = difftime(Sys.time(), start_time_i, units = "secs"))
        
        shinyFeedback::showToast(
          type = "success",
          message = paste0("Set ", i, " out of ",
                           length(m_sets), " completed."),
          .options = list(
            progressBar = FALSE,
            closeButton = TRUE,
            preventDuplicates = TRUE,
            positionClass = "toast-bottom-right"))
        
        rv$err_prev <- c(rv$err_prev, abs(err))
        last_values <- (length(rv$err_prev)-4):length(rv$err_prev)
        
        # Break conditions:
        
        if (rv$which_meta == "mean") {
          
          if (all(rv$err_prev[last_values] < threshold)) {
            if (!is.null(out_meta)) {
              
              means <- out_meta$meta[
                grep("mean", rownames(out_meta$meta)), ]
              tmpunit <- extract_units(rownames(out_meta$meta)[[1]])
              
              overlaps_with_truth <- dplyr::between(
                truth,
                means[1, "low"] %#% tmpunit,
                means[1, "high"] %#% tmpunit)
              
              if (overlaps_with_truth) {
                broke <- TRUE
                break
              }
            }
          }
        } # end of if (rv$which_meta == "mean")
        
        if (rv$which_meta == "compare") {
          
          cov <- Inf
          if (all(rv$err_prev[last_values] < threshold)) {
            if (!is.na(dt_meta[nrow(dt_meta), ]$est)) {
              cov <- out_meta$meta[
                grep("CoV", rownames(out_meta$meta)), 2][[2]]
            }
            
            if (!is.null(out_meta_groups)) {
              meta_truth <- rv$metaList_groups[[1]][["ctsd"]]
              overlaps_with_truth <- dplyr::between(
                .get_ratios(out_meta_groups)$est,
                .get_ratios(meta_truth)$lci, 
                .get_ratios(meta_truth)$uci)
            }
            
            # if cov -> infinity,
            # still sensitive to small changes in the mean.
            if (!is.infinite(cov) && overlaps_with_truth) {
              broke <- TRUE
              break
            }
          }
        } # end of if (rv$which_meta == "compare")
      
      } # end of while()
      
      truthList <- get_true_hr(
        data = rv$simList,
        seed = rv$seedList,
        sigma = rv$sigma,
        
        emulated = rv$is_emulate,
        fit = if (rv$is_emulate) rv$meanfitList else NULL,
        
        grouped = rv$grouped,
        groups = if (rv$grouped) rv$groups[[2]] else NULL)
      rv$truth$ctsd <- truthList
      
      rv$meta_tbl <<- rbind(rv$meta_tbl, dt_meta)
      rv$dev$n <- lapply(seq_along(rv$simList), function(x)
        n <- nrow(rv$simList[[x]]))
      
      for (i in seq_for) {
        group <- 1
        if (rv$grouped) {
          nm <- names(rv$simList)[[i]]
          group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
          truth <- get_true_speed(
            data = rv$simList,
            seed = rv$seedList,
            
            tau_p = rv$tau_p,
            tau_v = rv$tau_v,
            sigma = rv$sigma,
            
            emulated = rv$is_emulate,
            fit = if (rv$is_emulate) rv$meanfitList else NULL,
            
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL,
            
            summarized = TRUE)[[group]]
        }
        
        N2 <- extract_dof(rv$simfitList[[i]], "speed")[[1]]
        if (N2 < 0.001) {
          out_est <- rep(NA, 3)
          out_err <- rep(NA, 3)
          tmpunit <- NA
        } else {
          tmpsum <- rv$ctsdList[[i]]
          tmpname <- rownames(tmpsum$CI)
          tmpunit <- extract_units(tmpname[grep('^speed', tmpname)])
          
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
          seed = rv$seedList[[i]],
          lci = out_est[[1]],
          est = out_est[[2]],
          uci = out_est[[3]],
          unit = tmpunit)
        out_err_df <- data.frame(
          seed = rv$seedList[[i]],
          lci = out_err[[1]],
          est = out_err[[2]],
          uci = out_err[[3]])
        
        rv$speedEst <<- rbind(rv$speedEst, out_est_df)
        rv$speedErr <<- rbind(rv$speedErr, out_err_df)
      }
      
      N2 <- extract_dof(rv$simfitList, "speed")
      
      ### Calculating distance:
      
      for (i in seq_for) {
        group <- 1
        if (rv$grouped) {
          nm <- names(rv$simList)[[i]]
          group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
          truth <- truth <- get_true_speed(
            data = rv$simList,
            seed = rv$seedList,
            
            tau_p = rv$tau_p,
            tau_v = rv$tau_v,
            sigma = rv$sigma,
            
            emulated = rv$is_emulate,
            fit = if (rv$is_emulate) rv$meanfitList else NULL,
            
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL,
            
            summarized = TRUE)[[group]]
        }
        
        dur_days <- "days" %#% rv$dur$value %#% rv$dur$unit
        unit_new <- "kilometers/day"
        
        truth <- sum(rv$pathList[[i]]$dist, na.rm = TRUE)
        unit_old <- rv$speedEst$unit[i]
        
        if (is.null(rv$pathList[[i]])) {
          out_dist_est_df <- data.frame(
            seed = rv$seedList[[i]],
            lci = NA, est = NA, uci = NA, unit = NA)
          out_dist_err_df <- data.frame(
            seed = rv$seedList[[i]],
            lci = NA, est = NA, uci = NA)
          next
        }
        
        dist_lci <- (unit_new %#% rv$speedEst$lci[i]
                     %#% unit_old) * dur_days
        dist_est <- (unit_new %#% rv$speedEst$est[i]
                     %#% unit_old) * dur_days
        dist_uci <- (unit_new %#% rv$speedEst$uci[i]
                     %#% unit_old) * dur_days
        
        dist_unit <- "kilometers"
        truth <- dist_unit %#% truth
        
        out_dist_est_df <-  data.frame(
          seed = rv$seedList[[i]],
          lci = dist_lci, 
          est = dist_est, 
          uci = dist_uci, 
          unit = dist_unit)
        
        out_dist_err_df <-  data.frame(
          seed = rv$seedList[[i]],
          lci = (dist_lci - truth) / truth,
          est = (dist_est - truth) / truth,
          uci = (dist_uci - truth) / truth)
        
        rv$distEst <<- rbind(rv$distEst, out_dist_est_df)
        rv$distErr <<- rbind(rv$distErr, out_dist_err_df)
        
        if (rv$is_emulate) {
          tau_v <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]], 
                           rv$seedList[[i]]),
            "velocity")[[1]]
        } else {
          tau_v <- rv$tau_v[[group]]
        }
        
        rv$sd$tbl <<- rbind(
          rv$sd$tbl,
          .build_tbl_sd(
            group = if (rv$grouped) group else NA,
            data = rv$simList[[i]],
            seed = rv$seedList[[i]],
            obj = rv$ctsdList[[i]],
            par = tau_v,
            speed = rv$speedEst[i, ],
            speed_error = rv$speedErr[i, ],
            distance = rv$distEst[i, ],
            distance_error = rv$distErr[i, ]))
      }
      
      if (rv$set_analysis == "ctsd") rv$sd_completed <- TRUE
      rv$is_analyses <- TRUE
      rv$is_report <- FALSE
      rv$is_meta <- FALSE
      
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
      
    }, label = "o-m_sims_minimum_ctsd") %>% # end of observer
      bindEvent(input$mButton_repeat)
    
    # TABLES --------------------------------------------------------------
    
    observe({
      req(rv$simList, rv$dev$N1, rv$hrErr, input$add_hr_table)
      
      shinyjs::show(id = "hrBox_summary")
      rv$hr$tbl <- dplyr::distinct(rv$hr$tbl)
      
    }) %>% # end of observe
      bindEvent(list(input$add_hr_table, rv$hrErr))
    
    
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_comp_m_ui("comp_m_1")

## To be copied in the server
# mod_comp_m_server("comp_m_1")
