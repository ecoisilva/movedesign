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
          
          shinyWidgets::numericInputIcon(
            inputId = ns("error_threshold"),
            label = "Error threshold:",
            min = 1,
            max = 50,
            value = 10,
            step = 1,
            icon = list(NULL, icon("percent"))),
          
          uiOutput(ns("mUI_ratio")),
          
          fluidRow(
            column(width = 12,
                   verbatimTextOutput(outputId = ns("txt_ratio"))
            )) #,
          
          ##TODO
          # p(style = "margin-top: 10px;"),
          # fluidRow(
          #   column(width = 12, align = "center",
          #          shinyWidgets::awesomeCheckbox(
          #            inputId = ns("add_individual_var"),
          #            label = span("Simulate",
          #                         span("individual variation",
          #                              class = "cl-jgl")),
          #            value = FALSE)))
          
        ) # end of fluidRow
        
      ), # end of column
      
      footer = column(
        width = 12, align = "right",
        style = "padding-left: 0px; padding-right: 0px;",
        
        shiny::actionButton(
          inputId = ns("mButton_repeat"),
          icon = icon("bolt"), # icon("repeat"),
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
        fit <- rv$modList[[1]]
        if (rv$grouped) {
          fitA <- rv$modList_groups[["A"]]
          fitB <- rv$modList_groups[["B"]]
        }
      }
      
      if (rv$grouped) {
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
      
      out_time <- guess_time(rv$simList, parallel = rv$parallel)
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
    
    shinyjs::hide(id = "nsims")
    shinyjs::hide(id = "nsims_max")
    
    ## Reveal elements based on workflow: ---------------------------------
    
    observe({
      req(rv$which_meta)
      
      if ("compare" == rv$which_meta) {   
        shinyjs::show(id = "ratio")
        shinyjs::show(id = "txt_ratio")
      }
      
      if ("mean" == rv$which_meta) {   
        shinyjs::hide(id = "ratio")
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
      
      if (rv$which_m == "set_m") {
        req(input$nsims)
        rv$nsims <- as.numeric(input$nsims)
        shinyjs::hide("error_threshold")
      } else {
        req(input$nsims_max)
        rv$nsims <- as.numeric(input$nsims_max)
        shinyjs::show("error_threshold")
      }
      
    }) # end of observe
    
    observe({
      req(rv$which_m)
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
      
    }) %>%  # end of observe,
      bindEvent(rv$active_tab)
    
    observe({
      req(rv$which_meta, rv$is_analyses)
      
      if (rv$is_analyses && (rv$which_meta != "none"))
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
    
    # observe({
    #   req(rv$is_analyses)
    #   shinyWidgets::updateAutonumericInput(
    #     session = session,
    #     inputId = "nsims",
    #     label = NULL,
    #     value = 0)
    #   
    # }) %>% # end of observe,
    #   bindEvent(rv$is_analyses)
    
    observe({
      req(rv$which_meta, length(rv$simList) > 1)
      
      wheel_step <- ifelse("compare" %in% rv$which_meta, 2, 1)
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "nsims",
        label = "Number of tags:",
        value = length(rv$simList), 
        options = list(
          decimalPlaces = 0,
          minimumValue = length(rv$simList),
          maximumValue = 100,
          wheelStep = wheel_step))
      
    }) %>% # end of observe,
      bindEvent(rv$simList)
    
    ## Rendering effect size (to set or based on groups): -----------------
    
    output$mUI_ratio <- renderUI({
      req("compare" %in% rv$which_meta)
      if (length(rv$groups[[1]]$A) == 0 || 
          length(rv$groups[[1]]$B) == 0) {
        ui <- shinyWidgets::autonumericInput(
          inputId = ns("ratio"),
          label = "Ratio:",
          minimumValue = 1, # 0.1,
          maximumValue = 1, # 4,
          value = 1,
          decimalPlaces = 1)
        
      } else {
        req(rv$ratio)
        
        ui <- shinyWidgets::autonumericInput(
          inputId = ns("ratio"),
          label = "Ratio:",
          minimumValue = round(rv$ratio[[rv$set_analysis]], 3),
          maximumValue = round(rv$ratio[[rv$set_analysis]], 3),
          value = round(rv$ratio[[rv$set_analysis]], 3),
          decimalPlaces = 3,
          modifyValueOnWheel = FALSE)
      }
      
      return(ui)
      
    }) %>% # end of renderUI, "mUI_ratio"
      bindEvent(list(rv$groups, rv$ratio))
    
    ## Rendering new text output (for effect size): -----------------------
    
    output$txt_ratio <- renderText({
      req(input$ratio)
      
      out_txt <- NULL
      var <- "home range area"
      diff <- c("smaller", "larger")
      if (set_analysis == "ctsd") {
        var <- "speed"
        diff <- c("slower", "faster")
      }
      
      if (input$ratio == 1) out_txt <- paste0(
        "Group A's ", var, " is equal to Group B's.")
      else if (input$ratio < 1) out_txt <- paste0(
        "Group A's ", var, " is ",
        round(abs(100 - input$ratio * 100), 1),
        "% ", diff[1], " than Group B's.")
      else if (input$ratio > 1) out_txt <- paste0(
        "Group A's ", var, " area is ",
        round(abs(100 - input$ratio * 100), 1),
        "% ", diff[2], " than Group B's.")
      
      return(out_txt)
      
    }) # end of renderText, "txt_ratio"
    
    # SIMULATIONS ---------------------------------------------------------
    ## Run multiple simulations (set number of tags): ---------------------
    
    observe({
      req(rv$which_meta,
          rv$which_m == "set_m")
      req(rv$datList,
          rv$fitList,
          rv$dur, rv$dti,
          rv$dev$is_valid,
          rv$simList)
      
      if ("compare" %in% rv$which_meta) 
        req((rv$nsims - 2) > 0) else req((rv$nsims - 1) > 0)
      # add alert?
      
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
      
      start <- Sys.time()
      tmpList <- list()
      
      num_sims <- input$nsims - length(rv$simList)
      if (rv$grouped) num_sims <- num_sims / 2
      req(num_sims > 0)
      
      for (i in seq_len(num_sims)) {
        
        rv$seed0 <- generate_seed(rv$seedList)
        simList <- simulating_data()
        
        # If there is data loss:
        
        if (!is.null(input$device_loss))
          if (req(input$device_loss) > 0) {
            to_keep <- round(sapply(simList, function(x)
              nrow(x) * (1 - rv$lost$perc/100)))
            
            simList <- lapply(simList, function(x) {
              to_keep_vec <- sort(sample(1:nrow(x),
                                         to_keep, replace = TRUE))
              x[to_keep_vec, ] })
            
          } # end of input$device_loss
        
        # If there are errors associated with each location:
        
        if (!is.null(input$device_error))
          if (req(input$device_error) > 0) {
            rv$error <- input$device_error
            simList <- lapply(simList, function(x) {
              error_x <- stats::rnorm(nrow(x), mean = 0,
                                      sd = input$device_error)
              error_y <- stats::rnorm(nrow(x), mean = 0,
                                      sd = input$device_error)
              x[c("x", "y")] <- x[c("x", "y")] + c(error_x, error_y)
              return(x) })
            
          } # end of input$device_error
        
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
                    n = num_sims)
      
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
        
        simfitList <- tryCatch(
          par.ctmm.select(simList, guessList, parallel = rv$parallel),
          error = function(e) e)
        if (num_sims == 1) simfitList <- list(simfitList)
        
        if (inherits(simfitList, "error")) {
          msg_log(
            style = "danger",
            message = paste0(
              "Model selection ", msg_danger("failed"), "."))
          return(NULL)
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
          newrow <- devRow(
            seed = rv$seedList[[(rv$nsims - num_sims) + x]],
            group = if (rv$grouped)
              ifelse(nm %in% rv$groups[[2]]$A, "A", "B") else NA,
            device = rv$device_type,
            dur = rv$dur, dti = rv$dti,
            data = simList[[x]], 
            fit = simfitList[[x]])
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
          newrow <- devRow(
            seed = rv$seedList[[(rv$nsims - num_sims) + i]],
            group = if (rv$grouped)
              ifelse(nm %in% rv$groups[[2]]$A, "A", "B") else NA,
            device = rv$device_type,
            dur = rv$dur, dti = rv$dti,
            data = simList[[i]], 
            fit = fit)
          rv$dev$tbl <<- rbind(rv$dev$tbl, newrow)
          
          msg_log(
            style = 'warning',
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
          rv$fitList,
          rv$dur, rv$dti,
          rv$dev$is_valid,
          rv$simList)
      
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
      threshold <- 0.1 # 10% error
      err <- 1
      err_prev <- rep(err, 5)
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
        "subpop" = logical(0))
      
      i <- 0
      while (i < length(m_sets)) {
        i <- i + 1
        if (trace) message("Set ", i, " out of ", length(m_sets))
        
        if (length(rv$simList) == 1) {
          # Running one extra simulation at the beginning:
          rv$seed0 <- generate_seed(rv$seedList)
          simList <- simulating_data()
          rv$seedList <- c(rv$seedList, rv$seed0)
        } else {
          if (rv$grouped) { 
            rv$seed0 <- generate_seed(rv$seedList)
            simList <- simulating_data()
            for (x in seq_along(simList))
              simList[[x]] <- ctmm:::pseudonymize(simList[[x]])
            rv$seedList <- c(rv$seedList, rv$seed0, rv$seed0 + 1)
          } else { 
            simList <- lapply(seq_len(m), function(x) {
              rv$seed0 <- generate_seed(rv$seedList)
              out <- simulating_data()[[1]]
              out <- ctmm:::pseudonymize(out)
              rv$seedList <- c(rv$seedList, rv$seed0)
              return(out) })
          }
        }
        
        # If there is data loss:
        
        if (!is.null(input$device_loss))
          if (req(input$device_loss) > 0) {
            to_keep <- round(sapply(simList, function(x)
              nrow(x) * (1 - rv$lost$perc/100)))
            
            simList <- lapply(simList, function(x) {
              to_keep_vec <- sort(sample(1:nrow(x),
                                         to_keep, replace = TRUE))
              x[to_keep_vec, ] })
            
          } # end of input$device_loss
        
        # If there are errors associated with each location:
        
        if (!is.null(input$device_error))
          if (req(input$device_error) > 0) {
            rv$error <- input$device_error
            simList <- lapply(simList, function(x) {
              error_x <- stats::rnorm(nrow(x), mean = 0,
                                      sd = input$device_error)
              error_y <- stats::rnorm(nrow(x), mean = 0,
                                      sd = input$device_error)
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
                          rv$tau_p[[1]]$unit[2]) * 3 # reduce run time
        
        fitList <- lapply(seq_along(simList), function(x) {
          guess <- ctmm::ctmm.guess(simList[[x]], interactive = F)
          if (optimal_dur < current_dur)
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
          out_meta <- capture_meta(in_list,
                                   sort = TRUE,
                                   units = FALSE,
                                   verbose = TRUE,
                                   plot = FALSE)
        } else {
          out_meta <- NULL
        }
        
        if (!is.null(out_meta)) {
          
          hex <- c(hex, ifelse(
            out_meta$logs$subpop_detected, pal$dgr, pal$sea))
          
          tmpname <- rownames(out_meta$meta)
          tmpunit <- extract_units(tmpname[grep("^mean", tmpname)])
          truth <- rv$truth$hr[["area"]][[1]]
          
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
            subpop = out_meta$logs$subpop_detected)
        } else {
          err <- err_prev[length(err_prev)]
          dt_meta <- dt_meta %>% dplyr::add_row(
            type = "hr",
            m = length(rv$simList),
            lci = NA, est = NA, uci = NA,
            overlaps = NA,
            subpop = NA)
        }
        
        if (trace) 
          message("-- Number of simulations: ", length(rv$simList))
        if (trace) 
          message(paste0("-- Error: ", round(abs(err) * 100, 1), "%"))
        
        err_prev <- c(err_prev, abs(err))
        last_values <- (length(err_prev)-5):length(err_prev)
        mean_err <- mean(abs(err_prev)[last_values])
        
        if (i >= 5 && abs(err) < threshold) break
        if (i >= 5 && all((
          abs(err_prev)[last_values] - mean_err) < 0.02))
          break
        
      } # end of while()
      
      rv$meta_tbl <<- rbind(rv$meta_tbl, dt_meta)
      rv$dev$n <- lapply(seq_along(rv$simList), function(x)
        n <- nrow(rv$simList[[x]]))
      
      for (i in seq_for) {
        group <- 1
        if (rv$grouped) {
          nm <- names(rv$simList)[[i]]
          group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
          truth <- rv$truth$hr[["area"]][[group]]
        }
        
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
        
        rv$hr$tbl <<- rbind(
          rv$hr$tbl, 
          hrRow(seed = rv$seedList[[i]],
                group = if (rv$grouped) group else NA,
                data = rv$simList[[i]], 
                tau_p = rv$tau_p[[group]],
                dur = rv$dur,
                dti = rv$dti,
                fit = rv$simfitList[[i]],
                area = out_est_df,
                error = out_err_df))
      }
      
      rv$hrEst <- out_est_df
      rv$hrErr <- out_err_df
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
          rv$fitList,
          rv$dur, rv$dti,
          rv$dev$is_valid,
          rv$simList,
          input$error_threshold)
      
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
      threshold <- input$error_threshold/100 # default 10% error
      err <- 1
      err_prev <- rep(err, 5)
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
        "subpop" = logical(0))
      
      i <- 0
      while (i < length(m_sets)) {
        i <- i + 1
        
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
          rv$seedList <- c(rv$seedList, rv$seed0)
        } else {
          if (rv$grouped) {
            rv$seed0 <- generate_seed(rv$seedList)
            simList <- simulating_data()
            for (x in seq_along(simList))
              simList[[x]] <- ctmm:::pseudonymize(simList[[x]])
            rv$seedList <- c(rv$seedList, rv$seed0, rv$seed0 + 1)
          } else {
            simList <- lapply(seq_len(m), function(x) {
              rv$seed0 <- generate_seed(rv$seedList)
              out <- simulating_data()[[1]]
              out <- ctmm:::pseudonymize(out)
              rv$seedList <- c(rv$seedList, rv$seed0)
              return(out) 
            })
          }
        }
        
        # If there is data loss:
        
        if (!is.null(input$device_loss))
          if (req(input$device_loss) > 0) {
            to_keep <- round(sapply(simList, function(x)
              nrow(x) * (1 - rv$lost$perc/100)))
            
            simList <- lapply(simList, function(x) {
              to_keep_vec <- sort(sample(1:nrow(x),
                                         to_keep, replace = TRUE))
              x[to_keep_vec, ] })
            
          } # end of input$device_loss
        
        # If there are errors associated with each location:
        
        if (!is.null(input$device_error))
          if (req(input$device_error) > 0) {
            rv$error <- input$device_error
            simList <- lapply(simList, function(x) {
              error_x <- stats::rnorm(nrow(x), mean = 0,
                                      sd = input$device_error)
              error_y <- stats::rnorm(nrow(x), mean = 0,
                                      sd = input$device_error)
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
                          rv$tau_p[[1]]$unit[2]) * 3 # reduce run time
        
        fitList <- lapply(seq_along(simList), function(x) {
          guess <- ctmm::ctmm.guess(simList[[x]], interactive = F)
          if (optimal_dur < current_dur)
            out <- ctmm::ctmm.fit(simList[[x]], guess, trace = F)
          else
            out <- ctmm::ctmm.select(simList[[x]], guess, trace = F)
          rv$simfitList <- c(rv$simfitList, list(out))
          return(out)
        })
        
        req(length(rv$simList) == length(rv$simfitList))
        
        seq_for_ctsd <- (num_sims + m_sets[i] - 1):
          (num_sims + m_sets[i])
        
        ctsdList <- par.speed(
          rv$simList[seq_for_ctsd],
          rv$simfitList[seq_for_ctsd],
          seed = rv$seedList[seq_for_ctsd],
          parallel = rv$parallel)
        rv$ctsdList <- c(rv$ctsdList, ctsdList)
        
        pathList <- estimate_trajectory(
          data = rv$simList[seq_for_ctsd],
          fit = rv$simfitList[seq_for_ctsd],
          groups = if (rv$grouped) rv$groups[[2]] else NULL,
          dur = rv$dur,
          tau_v = rv$tau_v,
          seed = rv$seedList[seq_for_ctsd])
        rv$pathList <<- c(rv$pathList, pathList)
        
        if (length(rv$simList) == length(rv$ctsdList) &&
            length(rv$simList) == length(rv$pathList)) {
          names(rv$ctsdList) <- names(rv$simList)
          names(rv$pathList) <- names(rv$simList)
          
          in_list <- rv$ctsdList
          in_list[sapply(in_list, is.null)] <- NULL
          out_meta <- capture_meta(in_list,
                                   sort = TRUE,
                                   units = FALSE,
                                   verbose = TRUE,
                                   plot = FALSE)
        } else {
          out_meta <- NULL
        }
        
        if (!is.null(out_meta)) {
          
          hex <- c(hex, ifelse(
            out_meta$logs$subpop_detected, pal$dgr, pal$sea))
          
          tmpname <- rownames(out_meta$meta)
          tmpunit <- extract_units(tmpname[grep("^mean", tmpname)])
          truth <- rv$truth[["ctsd"]][[1]]
          
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
            lci = out_err[[1]], est = out_err[[2]], uci = out_err[[3]],
            overlaps = dplyr::between(truth,
                                      out_meta$meta[1, 1],
                                      out_meta$meta[1, 3]),
            subpop = out_meta$logs$subpop_detected)
          
        } else {
          err <- err_prev[length(err_prev)]
          dt_meta <- dt_meta %>% dplyr::add_row(
            type = "hr",
            m = length(rv$simList),
            lci = NA, est = NA, uci = NA,
            overlaps = NA,
            subpop = NA)
        }
        
        if (trace) message(" - No. sims (total): ", length(rv$simList))
        if (trace) message(paste0(" - Error: ",
                                  round(abs(err) * 100, 1), "%"))
        
        if (trace) msg_log(
          style = 'warning',
          message = paste0("Estimation for set no. ", i, " ",
                           msg_success("completed"), "..."),
          run_time = difftime(Sys.time(), start_time, units = "secs"))
        
        shinyFeedback::showToast(
          type = "success",
          message = paste0("Set ", i, " out of ",
                           length(m_sets), " completed."),
          .options = list(
            progressBar = FALSE,
            closeButton = TRUE,
            preventDuplicates = TRUE,
            positionClass = "toast-bottom-right"))
        
        err_prev <- c(err_prev, abs(err))
        last_values <- (length(err_prev)-5):length(err_prev)
        mean_err <- mean(abs(err_prev)[last_values])
        
        if (i >= 5 && abs(err) < threshold) break
        if (i >= 5 && all((
          abs(err_prev)[last_values] - mean_err) < 0.02))
          break
        
      } # end of while()
      
      rv$meta_tbl <<- rbind(rv$meta_tbl, dt_meta)
      rv$dev$n <- lapply(seq_along(rv$simList), function(x)
        n <- nrow(rv$simList[[x]]))
      
      for (i in seq_for) {
        group <- 1
        if (rv$grouped) {
          nm <- names(rv$simList)[[i]]
          group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
          truth <- rv$truth[["ctsd"]][[group]]
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
      
      ### Calculating
      
      for (i in seq_for) {
        group <- 1
        if (rv$grouped) {
          nm <- names(rv$simList)[[i]]
          group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
          truth <- rv$truth[["ctsd"]][[group]]
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
        
        rv$sd$tbl <<- rbind(
          rv$sd$tbl,
          sdRow(seed = rv$seedList[[i]],
                group = if (rv$grouped) group else NA,
                data = rv$simList[[i]],
                tau_v = rv$tau_v[[group]],
                dur = rv$dur,
                dti = rv$dti,
                fit = rv$simfitList[[i]],
                speed = rv$speedEst[i, ],
                speed_error = rv$speedErr[i, ],
                distance = rv$distEst[i, ],
                distance_error = rv$distErr[i, ]))
      }
      
      # if (rv$set_analysis == "hr") rv$hr_completed <- TRUE
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
