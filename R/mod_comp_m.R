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
            
            shinyWidgets::autonumericInput(
              inputId = ns("nsims"),
              label = "No. simulations (to run):",
              minimumValue = 0,
              maximumValue = 100,
              value = 0,
              decimalPlaces = 0,
              wheelStep = 1),
            
            # fluidRow(
            #   column(width = 12, align = "left",
            #          shinyWidgets::awesomeCheckbox(
            #            inputId = ns("add_individual_var"),
            #            label = span("Simulate",
            #                         span("individual variation",
            #                              class = "cl-jgl")),
            #            value = FALSE))),
            
            p(style = "padding: 0px;"),
            p(style = "text-align: right !important;",
              HTML("&nbsp;"), "No. simulations (total):") %>%
              tagAppendAttributes(class = 'label_split'),
            
            fluidRow(
              column(width = 12,
                     verbatimTextOutput(outputId = ns("nsims_total"))
              )), p(style = "padding: 0px;"),

            shinyWidgets::autonumericInput(
              inputId = ns("ratio"),
              label = "Ratio:",
              minimumValue = 0.1,
              maximumValue = 4,
              
              value = 1,
              decimalPlaces = 1,
              wheelStep = 0.1),
            
            fluidRow(
              column(width = 12,
                     verbatimTextOutput(outputId = ns("txt_ratio"))
              ))
            
          ) # end of fluidRow
          
        ), # end of column
        
        footer = column(
          width = 12, align = "right",
          style = "padding-left: 0px; padding-right: 0px;",
          
          shiny::actionButton(
            inputId = ns("mButton_repeat"),
            icon = icon("repeat"),
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
    
    rv$m <- reactiveValues(needs_fit = NULL, tmpList = NULL)
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at start: --------------------------------------------
    
    shinyjs::hide(id = "ratio")
    shinyjs::hide(id = "txt_ratio")
    
    ## Reveal elements based on workflow: ---------------------------------
    
    observe({
      req(rv$which_meta)
      
      if ("compare" %in% rv$which_meta) {   
        shinyjs::show(id = "ratio")
        shinyjs::show(id = "txt_ratio")
        
      }
    }) %>% # end of observe,
      bindEvent(rv$which_meta)
    
    ## Render number of simulations: --------------------------------------
    
    output$nsims_total <- renderText({
      req(input$nsims)
      
      m <- 1 + input$nsims
      if (!is.null(rv$simList)) m <- length(rv$simList) + input$nsims
      return(m)
      
    }) # end of renderText, "nsims_total"
    
    ## Update numeric input: ----------------------------------------------
    
    observe({

      if (rv$is_analyses)
        shinyWidgets::updateAutonumericInput(
          session = session,
          inputId = "nsims",
          label = "No. simulations (to run):",
          value = 0)

    }) %>% bindEvent(rv$is_analyses)

    ## Render new text (for effect size): ---------------------------------
    
    output$txt_ratio <- renderText({
      req(input$ratio)
      
      out_txt <- NULL
      if (set_analysis == "hr") {
        var <- "home range area"
        diff <- c("smaller", "larger")
      }
      if (set_analysis == "ctsd") {
        var <- "speed"
        diff <- c("slower", "faster")
      }
      
      if (input$ratio == 1) out_txt <- paste0(
        "Group A's ", var, " is equal to Group B's.")
      else if (input$ratio < 1) out_txt <- paste0(
        "Group A's ", var, " is ",
        abs(100 - round(input$ratio, 1) * 100),
        "% ", diff[1], " than Group B's.")
      else if (input$ratio > 1) out_txt <- paste0(
        "Group A's ", var, " area is ",
        abs(100 - round(input$ratio, 1) * 100),
        "% ", diff[2], " than Group B's.")
      
      return(out_txt)
      
    }) # end of renderText, "txt_ratio"
    
    # SIMULATIONS ---------------------------------------------------------
    ## Simulating new datasets: -------------------------------------------
    
    simulating_data <- reactive({
      
      dur <- rv$dur$value %#% rv$dur$unit
      dti <- rv$dti$value %#% rv$dti$unit
      t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      
      if (rv$data_type == "simulated") {
        fit <- rv$modList[[1]]
      } else {
        if (length(rv$fitList) == 1 &&
            rv$fitList[[1]]$isotropic == TRUE) fit <- rv$fitList
        else { fit <- prepare_mod(
          tau_p = rv$tau_p0$value[2],
          tau_p_unit = rv$tau_p0$unit[2],
          tau_v = rv$tau_v0$value[2],
          tau_v_unit = rv$tau_v0$unit[2],
          sigma = rv$sigma0$value[2],
          sigma_unit = rv$sigma0$unit[2],
          mu = rv$mu0) #TODO change to emulate() if ind. var.
        rv$modList <- list(fit)
        }
      }
      
      fit$mu[[1, "x"]] <- 0
      fit$mu[[1, "y"]] <- 0 # recenter to 0,0
      
      sim <- ctmm::simulate(fit, t = t_new, seed = rv$seed0)
      sim <- pseudonymize(sim)
      return(sim)
      
    }) %>% # end of reactive, simulating_data()
      bindCache(c(rv$species,
                  rv$id,
                  rv$dur, 
                  rv$dti,
                  rv$seed0))
    
    ## Run multiple simulations: ------------------------------------------
    
    observe({
      req(rv$datList,
          rv$fitList,
          rv$dur, 
          rv$dti,
          rv$dev$is_valid,
          input$nsims > 0,
          rv$nsims >= 1,
          rv$simList) 
      
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
      n_sims <- length(rv$simList) + input$nsims
      for (i in seq_len(input$nsims)) {
        
        ## (Re)generate seed:
        set.seed(NULL)
        tmp <- round(stats::runif(1, min = 1, max = 10000), 0)
        while (tmp %in% rv$seedList) 
          tmp <- round(stats::runif(1, min = 1, max = 10000), 0)
        rv$seed0 <- tmp
        
        sim <- simulating_data()
        
        # If there is data loss:
        
        if (!is.null(rv$lost$perc))
          if (rv$lost$perc > 0) {
            to_keep <- round(nrow(sim) * (1 - rv$lost$perc/100))
            to_keep_vec <- sort(sample(1:nrow(sim), to_keep))
            sim <- sim[to_keep_vec,]
            
          } # end of rv$lost$perc
        
        # If there are errors associated with each location:
        
        if (!is.null(rv$error))
          if (rv$error > 0) {
            error_x <- stats::rnorm(nrow(sim), mean = 0, sd = rv$error)
            error_y <- stats::rnorm(nrow(sim), mean = 0, sd = rv$error)
            sim[c("x", "y")] <- sim[c("x", "y")] + c(error_x, error_y)
            
          } # end of rv$error
        
        tmpList[[i]] <- sim 
        rv$simList[[length(rv$simList) + 1]] <- sim
        rv$seedList[[length(rv$seedList) + 1]] <- rv$seed0
        
      } # end of for loop
      
      rv$tmpList <- tmpList 
      rv$dev$n <- lapply(seq_along(rv$simList), function(x)
        n <- nrow(rv$simList[[x]]))
      
      rv$m$needs_fit <- TRUE
      rv$is_analyses <- NULL
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
  
    estimating_time <- reactive({
      
      out_time <- guess_time(rv$simList, parallel = rv$parallel)
      return(out_time)
      
    }) %>% # end of reactive, estimating_time()
      bindCache(c(rv$tau_p0, 
                  rv$tau_v0,
                  rv$dur, 
                  rv$dti))
    
    observe({
      req(rv$datList,
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
      
      loading_modal("Selecting movement model", type = "fit",
                    exp_time = rv$expt,
                    n = rv$nsims)
      
      start <- Sys.time()
      no_sims <- length(rv$tmpList)
      rv$nsims <- length(rv$simList)
      
      simList <- rv$tmpList
      guessList <- lapply(seq_along(simList), function (x)
          ctmm::ctmm.guess(simList[[x]],
                           interactive = FALSE))
      
      if (rv$parallel) {
        
        msg_log(
          style = "warning",
          message = paste0("Model selection for ", no_sims,
                           " simulation(s) (out of ", rv$nsims, ") ",
                           msg_warning("in progress"), ","),
          detail = "This may take a while...")
        
        simfitList <- tryCatch(
          par.ctmm.select(simList, guessList, parallel = rv$parallel),
          error = function(e) e)
        if (no_sims == 1) simfitList <- list(simfitList)
        
        if (inherits(simfitList, "error")) {
          msg_log(
            style = "danger",
            message = paste0(
              "Model selection ", msg_danger("failed"), "."))
          return(NULL)
        }
        
        lapply(seq_along(simfitList), function (x) {
          simfitList[[x]]$mu[[1, "x"]] <- 0
          simfitList[[x]]$mu[[1, "y"]] <- 0 # center to 0,0
        })
        
        rv$dev$N1 <- c(rv$dev$N1, 
                       extract_dof(simfitList, "area"))
        rv$dev$N2 <- c(rv$dev$N2, 
                       extract_dof(simfitList, "speed"))
        
        m <- length(rv$simfitList)
        rv$simfitList <- c(rv$simfitList, simfitList)
        
        lapply(seq_along(simList), function(x) {
          newrow <- devRow(
            seed = rv$seedList[[(rv$nsims - no_sims) + x]],
            device = rv$device_type,
            dur = rv$dur, dti = rv$dti,
            data = simList[[x]], 
            fit = simfitList[[x]])
          rv$dev$tbl <<- rbind(rv$dev$tbl, newrow)
        })
        
      } else {
        
        #TODO needs to be tested
        for (i in seq_along(simList)) {
          msg_log(
            style = "warning",
            message = paste0("Model fit for sim no. ", no_sims + 1, " ",
                             msg_warning("in progress"), ","),
            detail = "Please wait for model selection to finish:")
          
          start_i <- Sys.time()
          fit1 <- par.ctmm.select(
            simList[[i]], guessList[[i]], parallel = rv$parallel)
          time_i <- difftime(Sys.time(), start_i, units = "secs")
          
          rv$simfitList[[length(rv$simfitList) + 1]] <- fit1
          rv$dev$N1 <- c(rv$dev$N1, extract_dof(fit1, "area"))
          rv$dev$N2 <- c(rv$dev$N2, extract_dof(fit1, "speed"))
          
          newrow <- devRow(
            seed = rv$seedList[[(rv$nsims - no_sims) + x]],
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
        message = paste0("Model selection for ", no_sims,
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
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_comp_m_ui("comp_m_1")
    
## To be copied in the server
# mod_comp_m_server("comp_m_1")
