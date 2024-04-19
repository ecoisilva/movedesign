#' tab_report UI Function
#'
#' @description Final report tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_tab_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      # [left column] -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-4 col-md-4 col-lg-2",
          
          div(class = "tabBox_noheaders",
              shinydashboardPlus::box(
                id = ns("repBox_details"),
                width = NULL,
                headerBorder = FALSE,
                
                div(class = "tabBox_main",
                    shinydashboard::tabBox(
                      id = ns("repTabs_details"),
                      width = NULL, side = "right",
                      
                      tabPanel(
                        title = "Settings:",
                        value = ns("repPanel_settings"),
                        icon = icon("hammer"),
                        
                        ### Research question(s) --------------------------
                        
                        uiOutput(outputId = ns("report_question")),
                        uiOutput(outputId = ns("report_device")),
                        p(style = "padding-top: 0px"),
                        
                        p(HTML("&nbsp;"),
                          "No. of simulations:") %>%
                          tagAppendAttributes(class = 'label_split'),
                        fluidRow(
                          column(width = 12,
                                 verbatimTextOutput(
                                   outputId = ns("rep_nsims"))
                          )), p(style = "padding-bottom: 5px;"),
                        
                        uiOutput(outputId = ns("report_emulate")),
                        p(style = "padding-bottom: 5px;"),
                        
                        shinyWidgets::autonumericInput(
                          inputId = ns("ci"),
                          label = span("Credible intervals:",
                                       style = "align: left !important;"),
                          currencySymbol = "%",
                          currencySymbolPlacement = "s",
                          decimalPlaces = 0,
                          minimumValue = 0,
                          maximumValue = 100,
                          value = 95),
                        
                        actionButton(
                          inputId = ns("build_report"),
                          icon = icon("bookmark"),
                          label = "Build report",
                          width = "100%",
                          class = "btn-primary")
                        
                      ) # end of panel
                    )) # end of tabBox // repTabs_details
              ) # end of box // repBox_details
              
          ), # end of div (no headers)
          
          shinydashboardPlus::box(
            title = span("Home range:", class = "ttl-box"),
            id = ns("repBox_hr_err"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            mod_blocks_ui(ns("repBlock_hrErr")),
            br()
            
          ), # end of box
          
          shinydashboardPlus::box(
            title = span("Speed:", class = "ttl-box"),
            id = ns("repBox_speed_err"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            mod_blocks_ui(ns("repBlock_speedErr")),
            br()
            
          ), # end of box
          
          shinydashboardPlus::box(
            title = span("Distance:", class = "ttl-box"),
            id = ns("repBox_dist_err"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            mod_blocks_ui(ns("repBlock_distErr")),
            br()
            
          ) # end of box
              
      ), # end of UI column (left)
      
      # [right column] ----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-8 col-md-8 col-lg-10",
          
          ## Parameter information: ---------------------------------------
          
          div(class = "col-lg-6 no-padding-left tabBox_noheaders",
              shinydashboardPlus::box(
                id = ns("repBox_pars"),
                width = NULL,
                headerBorder = FALSE,
                
                shinydashboard::tabBox(
                  id = ns("repTabs_pars"),
                  width = NULL,
                  
                  tabPanel(title = "Species:",
                           value = ns("regPanel_species"),
                           icon = icon("paw"),
                           
                           splitLayout(
                             mod_blocks_ui(ns("repBlock_taup")),
                             mod_blocks_ui(ns("repBlock_tauv"))
                           ), mod_blocks_ui(ns("repBlock_sigma"))
                           
                  ), # end of panel (1 out of 2)
                  
                  tabPanel(title = "Sampling design:",
                           value = ns("repPanel_regime"),
                           icon = icon("stopwatch"),
                           
                           splitLayout(
                             mod_blocks_ui(ns("repBlock_dur")),
                             mod_blocks_ui(ns("repBlock_dti")))
                           
                  ) # end of panel (2 out of 2)
                  
                ) # end of tabBox
              )), # end of box // regBox_pars
          
          div(class = "col-lg-6 no-padding-right tabBox_noheaders",
              shinydashboardPlus::box(
                id = ns("regBox_pars_sizes"),
                width = NULL,
                headerBorder = FALSE,
                
                shinydashboard::tabBox(
                  id = ns("repTabs_pars_size"),
                  width = NULL,
                  
                  tabPanel(title = "Sample sizes:",
                           value = ns("repPanel_sizes"),
                           icon = icon("calculator"),
                           
                           uiOutput(ns("repUI_sizes")))
                ) # end of tabBox
                
              ) # end of box // regBox_pars_sizes
          ) # end of div
      ), # end of UI column (right)
      
      ## Final report: ----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-8 col-md-8 col-lg-10",
          
          shinydashboardPlus::box(
            title = span("Report:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "box-archive",
                                   height = "21px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "var(--sea-dark)"),
            id = ns("repBox_analyses"),
            width = NULL,
            headerBorder = FALSE,
            
            div(class = "col-report-left no-padding-left",
                uiOutput(ns("end_report"))),
            
            div(class = "col-report-right no-padding-right",
                
                div(id = "section-two_questions",
                    uiOutput(ns("end_report_both"))),
                
                div(id = ns("section-comparison"),
                    
                    p("Quick comparison with other",
                      wrap_none(span("designs", 
                                     class = "cl-sea"), "?")) %>%
                      tagAppendAttributes(class = "subheader"),
                    
                    column(width = 12, align = "center",
                           style = paste("z-index: 999;"),
                           uiOutput(ns("highlighting_reg"))),
                    
                    uiOutput(outputId = ns("reportPlots_error")),
                    uiOutput(ns("repPlotLegend3")),
                    
                    uiOutput(ns("end_comparison")),
                    uiOutput(ns("end_meta"))
                    
                ) # end of div // section-comparison
                
            ) # end of div
          ) # end of box // repBox_analyses
          
      ), # end of div
      
      # [bottom column] ---------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          ## Tables: ------------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Tables:", class = "ttl-box"),
            id = ns("repBox_tables"),
            width = NULL,
            solidHeader = FALSE,
            
            reactable::reactableOutput(ns("endTable"))
            
          ) # end of box // tables
          
      ) # end of UI column (bottom)
      
    ) # end of fluidRow
  ) # end of tagList
}

#' tab_report Server Functions
#'
#' @noRd
mod_tab_report_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    rv$report <- reactiveValues()
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at start: --------------------------------------------
    
    shinyjs::hide(id = "end_comparison")
    shinyjs::hide(id = "repBox_hr_err")
    shinyjs::hide(id = "repBox_speed_err")
    shinyjs::hide(id = "repBox_dist_err")
    shinyjs::hide(id = "repBox_analyses")
    shinyjs::hide(id = "repBox_tables")
    
    observe({
      req(rv$active_tab == 'report')
      
      rv$report$species <- NULL
      rv$report$regime <- NULL
      rv$report$analyses <- NULL
      shinyjs::hide(id = "repBox_analyses")
      output$end_report <- renderUI({ NULL })
      output$end_report_both <- renderUI({ NULL })
          
    }) %>% # end of observe,
      bindEvent(rv$active_tab)
    
    ## Rendering research questions: --------------------------------------
    
    output$report_question <- renderUI({
      
      ui_hr <- staticBlock("Home range", active = FALSE)
      ui_sd <- staticBlock("Speed & distance", active = FALSE)
      
      out_ui <- tagList(ui_hr, ui_sd)
      
      if (!is.null(rv$which_question)) {
        if ("Home range" %in% rv$which_question) {
          ui_hr <- staticBlock("Home range", active = TRUE)
        }
        
        if ("Speed & distance" %in% rv$which_question) {
          ui_sd <- staticBlock("Speed & distance", active = TRUE)
        }
        out_ui <- tagList(ui_hr, ui_sd)
      }
      
      return(out_ui)
      
    }) # end of renderUI, "report_question"
    
    ## Rendering device limitations: --------------------------------------
    
    output$report_device <- renderUI({
      req(rv$which_limitations)
      
      if ("loss" %in% rv$which_limitations) {
        ui_loss <- staticBlock(
          paste0(rv$lost$perc * 100, "%"), active = TRUE)
      } else if (!("loss" %in% rv$which_limitations)) {
        ui_loss <- staticBlock("No data loss", active = FALSE)
      }
      
      if ("error" %in% rv$which_limitations) {
        ui_error <- staticBlock(paste(rv$error, "meters"), active = TRUE)
      } else if (!("error" %in% rv$which_limitations)) {
        ui_error <- staticBlock("No error", active = FALSE)
      }
      
      if ("limit" %in% rv$which_limitations) {
        ui_limit <- staticBlock(paste(rv$storage, "locations"),
                                type = "max", active = TRUE)
      } else if (!("limit" %in% rv$which_limitations)) {
        ui_limit <- staticBlock("No limit", active = FALSE)
      }
      
      out <- tagList(
        br(),
        span("Data loss:", class = "txt-label"), ui_loss,
        span("Location error:", class = "txt-label"), ui_error,
        span("Storage limit:", class = "txt-label"), ui_limit)
      
      return(out)
      
    }) # end of renderUI, "report_device"
    
    ## Rendering if uncertainty was propagated or not: --------------------
    
    output$report_emulate <- renderUI({
      
      ui <- staticBlock("Uncertainty not propagated", active = FALSE)
      
      if (req(rv$is_emulate)) {
        ui <- staticBlock("Uncertainty propagated", active = TRUE)
      }
      
      out_ui <- tagList(ui)
      return(out_ui)
      
    }) # end of renderUI, "report_emulate"
    
    ## Rendering regime comparison inputs: --------------------------------
    
    output$highlighting_reg <- renderUI({
      req(rv$which_question, rv$which_meta)
      req(rv$which_meta == "none")
      
      # TODO show if tau values of pre-run sims apply
      if ("Home range" %in% rv$which_question) {
        
        out <- out_hr <- shinyWidgets::pickerInput(
          inputId = ns("highlight_dur"),
          label = span("Sampling duration (in days):",
                       class = "txt-label"),
          choices = 2^seq(1, 12, by = 1),
          options = list(title = "(select here)"),
          width = "200px")
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        
        dat <- movedesign::sims_speed[[2]]
        out <- out_sd <- shinyWidgets::pickerInput(
          inputId = ns("highlight_dti"),
          label = span("Sampling interval:",
                       class = "txt-label"),
          choices = dat$dti_notes %>% unique,
          options = list(title = "(select here)"),
          width = "200px")
      }
      
      if (length(rv$which_question) > 1) {
        out <- tagList(out_hr, out_sd)
      }
      
      return(out)
      
    }) # end of renderUI, "highlighting_reg"
    
    observe({
      req(input$highlight_dur)
      rv$highlight_dur <- input$highlight_dur
    })
    
    observe({
      req(input$highlight_dti)
      rv$highlight_dti <- input$highlight_dti
    })
    
    ## Rendering total number of simulations: -----------------------------
    
    output$rep_nsims <- renderText({
      req(rv$simList)
      return(length(rv$simList))
      
    }) # end of renderText, "rep_nsims"
    
    # OPERATIONS ----------------------------------------------------------
    ## Credible intervals for home range estimation: ----------------------
    
    observe({
      req(rv$active_tab == 'report')
      req(rv$which_meta, rv$tau_p[[1]], rv$dur$value, rv$dur$unit)
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      
      taup_unit <- "days"
      input_taup <- taup_unit %#% 
        rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      input_dur <- taup_unit %#% rv$dur$value %#% rv$dur$unit
      
      if (rv$which_meta == "none") {
        dat <- movedesign::sims_hrange[[1]] %>%
          dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
          dplyr::mutate(duration = round("days" %#% duration, 1))
        
        out_taup <- dat$tau_p[which.min(abs(dat$tau_p - input_taup))]
        out_dur <- dat$dur[which.min(abs(dat$dur - input_dur))]
        
        newdat <- dat %>%
          dplyr::filter(tau_p == out_taup) %>%
          dplyr::filter(duration == out_dur)
        
      } else {
        req(rv$hr$tbl)
        
        newdat <- data.frame(
          tau_p = input_taup,
          duration = input_dur,
          error = rv$hr$tbl$area_err,
          error_lci = rv$hr$tbl$area_err_min, 
          error_uci = rv$hr$tbl$area_err_max)
        
        out_taup <- input_taup
        out_dur <- input_dur
      }
      
      rv$hr_CI <- data.frame(
        mean = mean(newdat$error, na.rm = TRUE),
        CI_low = min(newdat$error, na.rm = TRUE),
        CI_high = max(newdat$error, na.rm = TRUE))
      
      # Credible intervals:
      hr_HDI <- tryCatch(
        bayestestR::ci(newdat$error, ci = ci, method = "HDI"),
        error = function(e) e)
      
      rv$hr_HDI <- data.frame("CI" = ci,
                              "CI_low" = NA,
                              "CI_high" = NA)
      
      if (!inherits(hr_HDI, "error"))
        if (!is.na(hr_HDI$CI_low) && !is.na(hr_HDI$CI_high))
          rv$hr_HDI <- hr_HDI
      
    }) # end of observe
    
    observe({ # For comparison with new duration:
      req(rv$highlight_dur > 0)
      shinyjs::show(id = "end_comparison")
      
      input_taup <- "days" %#% rv$tau_p[[1]]$value[2] %#%
        rv$tau_p[[1]]$unit[2]
      
      dat <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      out_taup <- dat$tau_p[which.min(abs(dat$tau_p - input_taup))]
      out_dur <- as.numeric(rv$highlight_dur)
      
      newdat <- dat %>%
        dplyr::filter(tau_p == out_taup) %>%
        dplyr::filter(duration == out_dur)
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      rv$hr_CI_new <- data.frame(
        mean = mean(newdat$error, na.rm = TRUE),
        CI_low = mean(newdat$error_lci, na.rm = TRUE),
        CI_high = mean(newdat$error_uci, na.rm = TRUE))
      
      # Credible intervals:
      rv$hr_HDI_new <- suppressWarnings(
        bayestestR::ci(newdat$error, ci = ci, method = "HDI"))
      
    }) %>% # end of observe,
      bindEvent(rv$highlight_dur)
    
    ## Credible intervals for speed & distance estimation: ----------------
    
    observe({
      req(rv$active_tab == 'report')
      req(rv$which_meta, rv$tau_v, rv$dti$value, rv$dti$unit)
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      
      input_tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
      input_dur <- "days" %#% rv$dur$value %#% rv$dur$unit
      input_dti <- rv$dti$value %#% rv$dti$unit
      
      if (rv$which_meta == "none") {
        dat <- movedesign::sims_speed[[1]] %>%
          dplyr::mutate(dur = round("days" %#% dur, 0))
        
        out_tauv <- dat$tau_v[which.min(abs(dat$tau_v - input_tauv))]
        out_dti <- dat$dti[which.min(abs(dat$dti - input_dti))]
        out_dur <- dat$dur[which.min(abs(dat$dur - input_dur))]
        
        newdat <- dat %>%
          dplyr::filter(tau_v == out_tauv) %>%
          dplyr::filter(dur == out_dur) %>%
          dplyr::filter(dti == out_dti)
        
      } else {
        req(rv$sd$tbl)
        
        newdat <- data.table::data.table(
          # tau_v = input_tauv,
          # dur = input_dur,
          # dti = input_dti,
          error = rv$sd$tbl$ctsd_err,
          error_lci = rv$sd$tbl$ctsd_err_min, 
          error_uci = rv$sd$tbl$ctsd_err_max)
      }
      
      rv$sd_CI <- data.frame(
        mean = mean(newdat$error, na.rm = TRUE),
        CI_low = mean(newdat$error_lci, na.rm = TRUE),
        CI_high = mean(newdat$error_uci, na.rm = TRUE))
      
      # Credible intervals:
      sd_HDI <- tryCatch(
        bayestestR::ci(newdat$error, ci = ci, method = "HDI"),
        error = function(e) e)
      
      rv$sd_HDI <- data.frame("CI" = ci,
                              "CI_low" = NA,
                              "CI_high" = NA)
      
      if (!inherits(sd_HDI, "error"))
        if (!is.na(sd_HDI$CI_low) && !is.na(sd_HDI$CI_high))
          rv$sd_HDI <- sd_HDI
      
    }) # end of observe
    
    observe({ # For comparison with new interval:
      req(rv$highlight_dti > 0)
      shinyjs::show(id = "end_comparison")
      
      input_tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
      
      dat <- movedesign::sims_speed[[1]]
      
      out_tauv <- dat$tau_v[which.min(abs(dat$tau_v - input_tauv))]
      opts <- movedesign::sims_speed[[1]] %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      out_dti <- fix_unit(
        opts$dti[match(rv$highlight_dti, opts$dti_notes)], "seconds")
      
      newdat <- dat %>%
        dplyr::filter(tau_v == out_tauv) %>%
        dplyr::filter(dti == out_dti$value)
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      rv$sd_CI_new <- data.frame(
        mean = mean(newdat$error, na.rm = TRUE),
        CI_low = mean(newdat$error_lci, na.rm = TRUE),
        CI_high = mean(newdat$error_uci, na.rm = TRUE))
      
      # Credible intervals:
      rv$sd_HDI_new <- suppressWarnings(
        bayestestR::ci(newdat$error, ci = ci, method = "HDI"))
      
    }) %>% # end of observe,
      bindEvent(rv$highlight_dti)
    
    # REPORT --------------------------------------------------------------
    
    observe({
      req(rv$which_question,
          rv$data_type,
          rv$is_analyses,
          rv$simList)
      
      questions <- NULL
      if ("Home range" %in% rv$which_question) {
        req(rv$hr_completed)
        questions <- "Home range"
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        req(rv$sd_completed)
        add_to <- ifelse(length(rv$which_question) > 1, ", ", "")
        questions <- paste0(questions, add_to, "Speed & distance")
      }
      
      msg_log(
        style = "warning",
        message = paste0("Building ",
                         msg_warning("report"), "..."),
        detail = paste("Current question(s):", questions))
      
      boxnames <- c("analyses", "tables")
      for (i in 1:length(boxnames)) {
        shinyjs::show(id = paste0("repBox_", boxnames[i]))
      }
      
    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    ### Reporting DATA: ---------------------------------------------------
    
    observe({
      req(rv$which_question,
          rv$data_type,
          rv$is_analyses,
          rv$simList)
      
      if (length(rv$id) == 1) {
        out_id <- span(rv$id, class = "cl-grn")
      } else {
        get_id <- toString(rv$id)
        out_id <- span(
          "multiple individuals",
          wrap_none("(", span(get_id, class = "cl-grn"), ")"))
      }
      
      switch(rv$data_type,
             "selected" = {
               out_species <- span(
                 "These outputs are based on parameters",
                 "extracted from", out_id, "and species", 
                 span(rv$species_common, class = "cl-grn"),
                 wrap_none("(", em(rv$species_binom), ")."))
             },
             "uploaded" = {
               out_species <- span(
                 "These outputs are based on parameters extracted",
                 "from ", out_id, "and species",
                 wrap_none(em(rv$species, class = "cl-grn"), "."))
             },
             "simulated" = {
               out_species <- span(
                 "These outputs are based on a",
                 span("simulated", class = "cl-grn"),
                 "dataset.")
             }
      ) # end of switch
      
      out_bias <- NULL
      if (rv$add_note) {
        out_bias <- span(
          style = paste("font-weight: 800;",
                        "font-family: var(--monosans);"),
          
          "However, due to ",
          span("very low effective sample sizes",
               style = "color: var(--danger)"),
          "of the",
          rv$data_type, "data, these parameters may not be",
          "accurate, and lead to negatively biased outputs.")
      }
      
      rv$report$species <- p(
        out_species, 
        span(style = paste("font-weight: 800;",
                           "font-family: var(--monosans);"),
             "Please see the",
             icon("paw", class = "cl-sea"),
             span("Species", class = "cl-sea"),
             "parameters above for more details."),
        out_bias)
      
    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    ### Reporting DESIGN: -------------------------------------------------
    
    observe({
      req(rv$which_question,
          rv$data_type,
          rv$is_analyses,
          rv$simList)
      
      if ("Home range" %in% rv$which_question) req(rv$hr_completed)
      if ("Speed & distance" %in% rv$which_question) req(rv$sd_completed)
      
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
      
      rv$hr_col <- rv$ctsd_col <- data.frame(
        hex = pal$sea, css = "var(--sea)")
      
      if (dur$value <= ideal_dur$value) {
        rv$hr_col$hex <- pal$dgr
        rv$hr_col$css <- "var(--danger)"
      }
      
      if (!is.infinite(ideal_dti$value)) {
        
        if (dti$value <= ideal_dti$value) {
          diff_dti <- tau_v / (rv$dti$value %#% rv$dti$unit)
        } else {
          diff_dti <- 1 / (tau_v / (rv$dti$value %#% rv$dti$unit))
        }
        
        min_dti <- fix_unit(dti_unit %#% (tau_v * 3), dti_unit)
        
        if (dti$value > 3 * ideal_dti$value) {
          rv$ctsd_col$hex <- pal$dgr
          rv$ctsd_col$css <- "var(--danger)"
        }
        
        if ((dti$value %#% dti$unit) <= tau_v) {
          dti_text <- span(
            wrap_none("\u03C4", tags$sub("v"), "/",
                      round(diff_dti, 1)))
        } else {
          dti_text <- span(
            round(diff_dti, 1), icon(name = "xmark"),
            wrap_none("\u03C4", tags$sub("v")))
        }
      }
      
      ### For home range estimation:
      
      if ("Home range" %in% rv$which_question) {
        req(rv$hr_HDI)
        
        N1 <- rv$dev$N1
        if (is.list(rv$dev$N1)) N1 <- do.call(c, N1)
        N1 <- scales::label_comma(accuracy = 1)(
          mean(N1, na.rm = TRUE))
        
        out_regime <- out_reg_hr <-
          p("The ideal", span("sampling duration", class = "cl-sea"),
            "for", span("home range", class = "cl-grn"), "estimation",
            "should be at least 30", icon(name = "xmark"),
            span("position autocorrelation", class = "cl-sea"),
            wrap_none("parameter (\u03C4", tags$sub("p"), "),"),
            "or \u2248", wrap_none(ideal_dur$value, " ", ideal_dur$unit,
                                   css = "cl-grn", end = "."),
            "Your current duration is",
            round(dur$value / (dur$unit %#% tau_p), 0),
            icon(name = "xmark"), wrap_none("\u03C4", tags$sub("p")),
            "\u2248", wrap_none(dur$value, " ", dur$unit, ","),
            "resulting in an effective sample size equivalent to",
            N1, "independent locations.")
        
      } # end of "Home range"
      
      ## Speed and distance estimation:
      
      if ("Speed & distance" %in% rv$which_question) {
        req(rv$sd_HDI)
        
        N2 <- rv$dev$N2
        if (is.list(rv$dev$N2)) N2 <- do.call(c, N2)
        N2 <- scales::label_comma(accuracy = 1)(
          mean(N2, na.rm = TRUE))
        
        out_regime <- out_reg_ctsd <-
          p("The ", span("sampling interval", class = "cl-sea"),
            "for", span("speed & distance", class = "cl-grn"),
            "estimation should ideally be near or less than the",
            span("velocity autocorrelation", class = "cl-sea"),
            wrap_none("parameter (", tags$sub("v"), "),"),
            "or \u2264", wrap_none(ideal_dti$value, " ", ideal_dti$unit,
                                   css = "cl-grn", end = "."),
            "Or, at a minimum, less than 3", icon(name = "xmark"),
            wrap_none("\u03C4", tags$sub("v")), "(\u2264",
            wrap_none(min_dti$value, " ", min_dti$unit, ")"),
            "for longer sampling durations.",
            "Your current interval (\u0394t) is",
            dti_text,
            "\u2248", wrap_none(dti$value, " ", dti$unit,
                                color = rv$ctsd_col[1], end = ","),
            "resulting in an effective sample size equivalent to",
            N2, "independently sampled velocities.")
        
      } # end of "Speed & distance"
      
      ### Both home range and speed & distance:
      
      if (length(rv$which_question) > 1)
        out_regime <- tagList(out_reg_hr, out_reg_ctsd)
      
      rv$report$regime <- out_regime
      
    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    ### Reporting OUTPUTS: ------------------------------------------------
    
    observe({
      req(rv$which_question,
          rv$data_type,
          rv$is_analyses,
          rv$simList)
      
      rv$report$analyses <- NULL
      
      if ("Home range" %in% rv$which_question) 
        req(rv$hrEst, rv$hr_completed)
      if ("Speed & distance" %in% rv$which_question)
        req(rv$speedEst, rv$distEst, rv$sd_completed)
      
      # Characteristic timescales:
      
      tau_p <- rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      tau_v <- ifelse(is.null(rv$tau_v[[1]]), 0,
                      rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2])
      
      # Sampling design:
      
      ideal_dur <- fix_unit(tau_p * 30, "seconds", convert = TRUE)
      dur_unit <- ideal_dur$unit
      
      if (is.null(rv$tau_v[[1]])) {
        ideal_dti <- data.frame(value = Inf, unit = "days")
      } else {
        ideal_dti <- fix_unit(rv$tau_v[[1]]$value[2],
                              rv$tau_v[[1]]$unit[2], convert = TRUE)
      }
      dti_unit <- ifelse(is.null(rv$tau_v[[1]]), "days", ideal_dti$unit)
      
      dur <- dur_unit %#% rv$dur$value %#% rv$dur$unit
      dur <- fix_unit(dur, dur_unit)
      dti <- dti_unit %#% rv$dti$value %#% rv$dti$unit
      dti <- fix_unit(dti, dti_unit)
      
      ## Home range estimation:
      
      if ("Home range" %in% rv$which_question) {
        req(rv$hr_HDI, rv$hrErr)
       
        hrCI <- c(round(rv$hr_HDI$CI_low * 100, 1),
                  round(rv$hr_HDI$CI * 100, 0),
                  round(rv$hr_HDI$CI_high * 100, 1))
        
        txt_level <- "estimation."
        if (!is.na(rv$hr_HDI$CI_low) && !is.na(rv$hr_HDI$CI_high))
          txt_level <- ifelse(
            rv$hr_HDI$CI_high < .3 & rv$hr_HDI$CI_low > -.3,
            "estimation, and with low uncertainty.", 
            "estimation, but with high uncertainty.")
        
        opts_dur <- 2^seq(1, 12, by = 1)
        plot_dur <- opts_dur[which.min(abs(
          opts_dur - ("days" %#% dur$value %#% dur$unit)))]
        
        out_analyses <- NULL
        
        if (dur$value >= ideal_dur$value) {
          rv$report$is_hr <- TRUE
          out_hr1 <- span(
            style = "font-weight: bold;",
            "Your current sampling duration is likely sufficient",
            "for home range", txt_level)
          
        } else {
          rv$report$is_hr <- FALSE
          out_hr1 <- span(
            style = "font-weight: bold;",
            "Your current sampling duration may be insufficient",
            "for home range estimation.")
        }
        
        out_hr2 <- NULL
        if (rv$which_meta == "none") {
          
          #   out_hr2 <- span(
          #     "Keep in mind that, for a similar duration of",
          #     plot_dur, "days, there is a",
          #     wrap_none(hrCI[2], "%", css = "cl-blk"),
          #     "probability that the relative error will lie between",
          #     wrap_none(hrCI[1], "%", css = "cl-blk"),
          #     "and", wrap_none(hrCI[3], "%", end = ".", css = "cl-blk"))
          
          out_hr2 <- span(
            style = paste("font-weight: 800;",
                          "font-family: var(--monosans);"),
            
            "To obtain credible intervals from multiple",
            "simulations, select a different",
            span("analytical target", class = "cl-sea"),
            "in the", icon("house", class = "cl-mdn"),
            span("Home", class = "cl-mdn"), "tab.")
          
        } else {
          
          if (!is.na(hrCI[1]) && !is.na(hrCI[3])) {
            out_hr2 <- span(
              "There is a", wrap_none(hrCI[2], "%", css = "cl-blk"),
              "probability the relative error will lie between",
              wrap_none(hrCI[1], "%", css = "cl-blk"),
              "and", wrap_none(hrCI[3], "%", end = ".", css = "cl-blk"))
            
          } else {
            out_hr2 <- span(
              "The number of simulations was insufficient so credible",
              "intervals (CIs) could not be calculated, returning ",
              wrap_none(span("NAs", class = "cl-dgr"), "."),
              span(
                style = paste("font-weight: 800;",
                              "font-family: var(--monosans);"),
                "Please run more simulations in the corresponding",
                shiny::icon("compass-drafting", class = "cl-sea"),
                span("Analyses", class = "cl-sea"), "tab to obtain",
                wrap_none(span("valid CIs", class = "cl-dgr"), ".")))
          }
        }
        
        nsims <- ifelse(
          length(rv$simList) == 1,
          "a single simulation",
          paste(length(rv$simList), "simulations"))
        
        out_nsims <- span(
          "Your error estimate based on",
          span(style = "font-weight: bold;", nsims),
          "was",
          wrap_none(
            round(mean(rv$hrErr$est, na.rm = TRUE) * 100, 1),
            "%."))
        
        out_analyses <- out_hr <- p(
          out_hr1,
          out_nsims,
          out_hr2)
        
      } # end of 'Home range'
      
      ## Speed and distance estimation:
      
      if ("Speed & distance" %in% rv$which_question) {
        
        req(!is.null(rv$is_ctsd))
        if (rv$is_ctsd) {
          req(rv$sd_HDI, rv$speedErr)
          
          sdCI <- c(round(rv$sd_HDI$CI_low * 100, 1),
                    round(rv$sd_HDI$CI * 100, 0),
                    round(rv$sd_HDI$CI_high * 100, 1))
          
          txt_level <- "estimation."
          if (!is.na(rv$sd_HDI$CI_low) && !is.na(rv$sd_HDI$CI_high))
            txt_level <- ifelse(
              rv$sd_HDI$CI_high < .3 & rv$sd_HDI$CI_low > -.3,
              "estimation, and with low uncertainty.", 
              "estimation, but with high uncertainty.")
          
          ctsd_err <- mean(rv$speedErr$est, na.rm = TRUE)
        }
        
        dti_options <- movedesign::sims_speed[[1]] %>%
          dplyr::select(dti, dti_notes) %>%
          unique()
        
        index_dti <- which.min(
          abs(dti_options$dti - (dti$value %#% dti$unit)))
        plotted_dti <- sub('^\\w+\\s\\w+\\s\\w+\\s', '',
                           dti_options[index_dti, 2])
        
        N2 <- rv$dev$N2
        if (is.list(rv$dev$N2)) N2 <- do.call(c, N2)
        N2 <- mean(N2, na.rm = TRUE)
        
        if (N2 >= 30) {
          rv$report$is_ctsd <- TRUE
          out_ctsd1 <- span(
            style = "font-weight: bold;",
            "Your current sampling interval is likely sufficient",
            "for speed & distance", txt_level)
        } else if (N2 >= 5) {
          rv$report$is_ctsd <- FALSE
          out_ctsd1 <- span(
            style = "font-weight: bold;",
            "Your current sampling interval may be sufficient",
            "for speed & distance estimation.")
        } else if (N2 > 0) {
          rv$report$is_ctsd <- FALSE
          out_ctsd1 <- span(
            style = "font-weight: bold;",
            "Your current sampling interval may be insufficient",
            "for speed & distance estimation.")
        } else {
          rv$report$is_ctsd <- FALSE
          out_ctsd1 <- span(
            style = "font-weight: bold;",
            "Your current sampling interval was too coarse",
            "for speed & distance estimation.")
        }
        
        if (rv$is_ctsd) {
          out_ctsd2 <- NULL
          
          if (rv$which_meta == "none") {
            out_ctsd2 <- span(
              style = paste("font-weight: 800;",
                            "font-family: var(--monosans);"),
              
              "To obtain credible intervals from multiple",
              "simulations, select a different",
              span("analytical target", class = "cl-sea"),
              "in the", icon("house", class = "cl-mdn"),
              span("Home", class = "cl-mdn"), "tab.")
            
          } else {
            
            if (!is.na(sdCI[1]) && !is.na(sdCI[3]))
              out_ctsd2 <- span(
                "There is a", wrap_none(sdCI[2], "%", css = "cl-blk"),
                "probability that the relative error will lie within",
                wrap_none(sdCI[1], "%", css = "cl-blk"), "and",
                wrap_none(sdCI[3], "%", end = ".", css = "cl-blk"))
            else
              out_ctsd2 <- span(
                "The number of simulations was insufficient so credible",
                "intervals (CIs) could not be calculated, returning ",
                wrap_none(span("NAs", class = "cl-dgr"), "."),
                span(
                  style = paste("font-weight: 800;",
                                "font-family: var(--monosans);"),
                  "Please run more simulations in the corresponding",
                  shiny::icon("compass-drafting", class = "cl-sea"),
                  span("Analyses", class = "cl-sea"), "tab to obtain",
                  wrap_none(span("valid CIs", class = "cl-dgr"), ".")))
          }
          
          nsims <- ifelse(
            length(rv$simList) == 1,
            "a single simulation",
            paste(length(rv$simList), "simulations"))
          
          out_nsims <- span(
            "Your error estimate based on",
            span(style = "font-weight: bold;", nsims),
            "was",
            wrap_none(
              round(mean(rv$speedErr$est, na.rm = TRUE) * 100, 1),
              "%."))
          
          out_analyses <- out_ctsd <- p(
            out_ctsd1,
            out_nsims,
            out_ctsd2)
          
        } else {
          out_ctsd2 <- NULL
          if (is.na(hrCI[1]) || is.na(hrCI[3]))
            out_ctsd2 <- span(
              "The number of simulations was insufficient so credible",
              "intervals (CIs) could not be calculated, returning ",
              wrap_none(span("NAs", class = "cl-dgr"), "."),
              
              span(style = paste("font-weight: 800;",
                                 "font-family: var(--monosans);"),
                   "Please run more simulations in the corresponding",
                   shiny::icon("compass-drafting", class = "cl-sea"),
                   span("Analyses", class = "cl-sea"), "tab to obtain",
                   wrap_none(span("valid CIs", class = "cl-dgr"), ".")))
          out_analyses <- out_ctsd <- p(out_ctsd1, out_ctsd2)
        }
        
      } # end of "Speed & distance"
      
      req(length(rv$which_question) == 1)
      rv$report$analyses <- out_analyses
      
    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    observe({
      req(length(rv$which_question) > 1,
          rv$hr_completed,
          rv$sd_completed,
          rv$simList)
      
      ### Both home range and speed & distance:
      
      is_hr <- rv$report$is_hr
      is_ctsd <- rv$report$is_ctsd
      req(!is.null(is_hr), !is.null(is_ctsd))
      req(rv$hr_completed, rv$sd_completed)
      
      sufficient <- span("sufficient", class = "cl-grn")
      insufficient <- span("insufficient", class = "cl-dgr")
      
      txt_hr_uncertainty <- NULL
      txt_sd_uncertainty <- NULL
      
      ## Number of simulations:
      
      out_nsims <- span(
        "a", span(style = "font-weight: bold;", "single"), 
        "simulation")
      
      if (length(rv$simList) > 1) {
        out_nsims <- span(style = "font-weight: bold;", 
                          length(rv$simList), "simulations")
      }
      
      # Home range estimation errors:
      
      hrErr_lci <- round(mean(rv[["hrErr"]]$lci, 
                              na.rm = TRUE) * 100, 1)
      hrErr_est <- round(mean(rv[["hrErr"]]$est, 
                              na.rm = TRUE) * 100, 1)
      hrErr_uci <- round(mean(rv[["hrErr"]]$uci, 
                              na.rm = TRUE) * 100, 1)
      
      is_hr_ci <- FALSE
      txt_hr_uncertainty <- NULL
      if (length(rv$simList) > 1) {
        req(rv$hr_HDI)
        hrErr_lci <- round(rv$hr_HDI$CI_low * 100, 1)
        hrErr_uci <- round(rv$hr_HDI$CI_high * 100, 1)
        
        if (!is.na(hrErr_lci) && !is.na(hrErr_uci)) {
          txt_hr_uncertainty <- case_when(
            abs(hrErr_uci) > 50 && abs(hrErr_lci) > 50 ~
              "(with high uncertainty)",
            abs(hrErr_uci) > 10 && abs(hrErr_lci) > 10 ~
              "(with low uncertainty)",
            TRUE ~ "(with very low uncertainty)")
          
          is_hr_ci <- TRUE
        }
      }
      
      # Speed and distance errors:
      
      if (any(rv$dev$N2 > 0)) {
        sdErr_lci <- round(mean(rv[["speedErr"]]$lci, 
                                na.rm = TRUE) * 100, 1)
        sdErr_est <- round(mean(rv[["speedErr"]]$est, 
                                na.rm = TRUE) * 100, 1)
        sdErr_uci <- round(mean(rv[["speedErr"]]$uci, 
                                na.rm = TRUE) * 100, 1)
        
        is_sd_ci <- FALSE
        txt_sd_uncertainty <- NULL
        if (length(rv$simList) > 1) {
          req(rv$sd_HDI)
          sdErr_lci <- round(rv$sd_HDI$CI_low * 100, 1)
          sdErr_uci <- round(rv$sd_HDI$CI_high * 100, 1)
          
          if (!is.na(sdErr_lci) && !is.na(sdErr_uci)) {
            txt_sd_uncertainty <- case_when(
              abs(sdErr_uci) > 50 && abs(sdErr_lci) > 50 ~
                "(with high uncertainty)",
              abs(sdErr_uci) > 10 && abs(sdErr_lci) > 10 ~
                "(with low uncertainty)",
              TRUE ~ "(with very low uncertainty)")
            
            is_sd_ci <- TRUE
          }
        }
      }
      
      if (is_hr & !is_ctsd) {
        out <- span(
          style = "font-weight: bold;",
          
          "Your current tracking regime is likely sufficient",
          "for home range",
          ifelse(is_hr_ci,
                 wrap_none("estimation ", txt_hr_uncertainty, ","),
                 "estimation,"),
          "but insufficient for speed & distance",
          ifelse(is_sd_ci,
                 wrap_none("estimation ", txt_sd_uncertainty, "."),
                 "estimation."))
        
        if (any(rv$dev$N2 == 0))
          out <- span(
            style = "font-weight: bold;",
            
            "Your current tracking regime is likely sufficient",
            "for home range",
            ifelse(is_hr_ci,
                   wrap_none("estimation ", txt_hr_uncertainty, ","),
                   "estimation,"),
            "and is",
            span("inappropriate", class = "cl-dgr"),
            "for speed & distance",
            ifelse(is_sd_ci,
                   wrap_none("estimation ", txt_sd_uncertainty, "."),
                   "estimation."))
      }
      
      if (!is_hr & is_ctsd) {
        out <- span(
          style = "font-weight: bold;",
          
          "Your current tracking regime may be insufficient",
          "for home range",
          ifelse(is_hr_ci,
                 wrap_none("estimation ", txt_hr_uncertainty, ","),
                 "estimation,"),
          "but likely sufficient for speed & distance",
          ifelse(is_sd_ci,
                 wrap_none("estimation ", txt_sd_uncertainty, "."),
                 "estimation."))
      }
      
      if (is_hr & is_ctsd) {
        out <- span(
          style = "font-weight: bold;",
          
          "Your current tracking regime is likely sufficient",
          "for both home range",
          ifelse(is_hr_ci,
                 wrap_none("estimation ", txt_hr_uncertainty, ","),
                 "estimation,"),
          "and for speed & distance",
          ifelse(is_sd_ci,
                 wrap_none("estimation ", txt_sd_uncertainty, "."),
                 "estimation."))
      }
      
      if (!is_hr & !is_ctsd) {
        out <- span(
          style = "font-weight: bold;",
          
          "Your current tracking regime may be insufficient",
          "for both home range",
          ifelse(is_hr_ci,
                 wrap_none("estimation ", txt_hr_uncertainty, ","),
                 "estimation,"),
          "and for speed & distance",
          ifelse(is_sd_ci,
                 wrap_none("estimation ", txt_sd_uncertainty, "."),
                 "estimation."))
        
        if (any(rv$dev$N2 == 0))
          out <- span(
            style = "font-weight: bold;",
            
            "Your current tracking regime may be insufficient",
            "for home range",
            ifelse(is_hr_ci,
                   wrap_none("estimation ", txt_hr_uncertainty, ","),
                   "estimation,"),
            "and is",
            span("inappropriate", class = "cl-dgr"),
            "for speed & distance",
            ifelse(is_sd_ci,
                   wrap_none("estimation ", txt_sd_uncertainty, "."),
                   "estimation."))
      }
      
      if (is.na(hrErr_lci) || is.na(hrErr_uci)) {
        out_hr_err <- tagList(
          ifelse(hrErr_est == 0, "less than 0.01%",
                 wrap_none(hrErr_est, "%")),
          "for home range estimation,")
      } else {
        out_hr_err <- tagList(
          ifelse(hrErr_est == 0, "less than 0.01%",
                 wrap_none(hrErr_est, "%")),
          wrap_none("[", hrErr_lci, ", ", hrErr_uci,
                    "%]", css = "cl-blk"), 
          "for home range estimation,")
      }
      
      if (is.na(sdErr_lci) || is.na(sdErr_uci)) {
        out_sd_err <- tagList(
          "and", ifelse(sdErr_est == 0, "less than 0.01%",
                        wrap_none(sdErr_est, "%")),
          "for speed estimation.")
      } else {
        out_sd_err <- tagList(
          "and", ifelse(sdErr_est == 0, "less than 0.01%",
                        wrap_none(sdErr_est, "%")),
          wrap_none("[", sdErr_lci, ", ", sdErr_uci,
                    "%]", css = "cl-blk"), 
          "for speed estimation.")
      }
      
      if (is.null(txt_hr_uncertainty)) {
        if (rv$which_meta == "none") {
          out_extra <- span(
            style = paste("font-weight: 800;",
                          "font-family: var(--monosans);"),
            
            "To obtain credible intervals from multiple",
            "simulations, select a different",
            span("analytical target", class = "cl-sea"),
            "in the", icon("house", class = "cl-mdn"),
            span("Home", class = "cl-mdn"), "tab.")
        } else {
          out_extra <- span(
            "The number of simulations was insufficient so credible",
            "intervals (CIs) could not be calculated, returning ",
            wrap_none(span("NAs", class = "cl-dgr"), "."),
            
            span(style = paste("font-weight: 800;",
                               "font-family: var(--monosans);"),
                 "Please run more simulations in the corresponding",
                 shiny::icon("compass-drafting", class = "cl-sea"),
                 span("Analyses", class = "cl-sea"), "tab to obtain",
                 wrap_none(span("valid CIs", class = "cl-dgr"), ".")))
        }
      } else { out_extra <- "" }
      
      out_analyses <- p(
        out,
        "Your error estimate based on",
        out_nsims, "was", out_hr_err,
        if (any(rv$dev$N2 > 0)) { out_sd_err
        } else { span("but the sampling interval was",
                      "too coarse to estimate speed.") },
        out_extra
      )
      
      rv$report$analyses <- out_analyses
      
    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    
    ## Reporting META (if available): ------------------------------------
    
    observe({
      req(rv$which_meta)
      
      if (rv$which_meta == "none") {
        shinyjs::hide(id = "end_meta")
      } else { shinyjs::show(id = "end_meta") }
      
    }) # end of observe
    
    output$end_meta <- renderUI({
      req(rv$report$meta, rv$report$groups)
      
      div(id = "report_meta",
          style = paste0("background-color: #f4f4f4;",
                         "padding: 20px;",
                         "margin-top: 20px;"),
          rv$report$meta,
          rv$report$groups)
      
    }) # end of renderUI, "end_meta"
    
    #### Mean of research target(s) ---------------------------------------
    
    observe({
      req(rv$active_tab == "report")
      rv$report$meta <- out_meta <- span("")
      
      req(!rv$grouped,
          !is.null(rv$is_emulate),
          rv$which_meta == "mean",
          rv$metaList)
      
      type <- get_truth <- get_CI <- get_HDI <- NULL
      txt_type <- txt_title <- NULL
      
      if ("Home range" %in% rv$which_question) {
        type <- c("hr")
        txt_type <- c("home range area")
        txt_title <- "Home range meta-analyses:"
        
        truth_summarized <- get_true_hr(
          sigma = rv$sigma,
          
          emulated = rv$is_emulate,
          fit = if (rv$is_emulate) rv$meanfitList else NULL,
          
          grouped = FALSE,
          summarized = TRUE)
        
        get_truth <- truth_summarized[["All"]]$area
        get_CI <- rv$metaErr[grep("hr", rv$metaErr$type), ]
        get_CI <- c("CI_low" = get_CI[nrow(get_CI), "lci"],
                    "CI_est" = get_CI[nrow(get_CI), "est"],
                    "CI_high" = get_CI[nrow(get_CI), "uci"])
        
        get_HDI <- c("CI" = rv$hr_HDI$CI,
                     "CI_low" = rv$hr_HDI$CI_low,
                     "CI_high" = rv$hr_HDI$CI_high)
      }

      if ("Speed & distance" %in% rv$which_question) {
        type <- c(type, "ctsd")
        txt_type <- c(txt_type, "movement speed")
        txt_title <- c(txt_title, "Speed meta-analyses:")

        truth_summarized <- get_true_speed(
          data = rv$simList,
          seed = rv$seedList,

          tau_p = rv$tau_p,
          tau_v = rv$tau_v,
          sigma = rv$sigma,

          emulated = rv$is_emulate,
          fit = if (rv$is_emulate) rv$meanfitList else NULL,

          grouped = FALSE,
          summarized = TRUE)
        
        truth <- truth_summarized[["All"]]
        
        CI <- rv$metaErr[grep("ctsd", rv$metaErr$type), ]
        CI <- c("CI_low" = CI[nrow(CI), "lci"],
                "CI_est" = CI[nrow(CI), "est"],
                "CI_high" = CI[nrow(CI), "uci"])
        
        HDI <- c("CI" = rv$sd_HDI$CI,
                 "CI_low" = rv$sd_HDI$CI_low,
                 "CI_high" = rv$sd_HDI$CI_high)
        
        if (length(rv$which_question) == 1) {
          get_truth <- get_truth
          get_HDI <- HDI
          get_CI <- CI
        } else {
          get_truth <- list(get_truth, truth)
          get_HDI <- list(get_HDI, HDI)
          get_CI <- list(get_CI, CI)
        }
      }
      
      if (!is.list(get_truth)) get_truth <- list(get_truth)
      if (!is.list(get_HDI)) get_HDI <- list(get_HDI)
      if (!is.list(get_CI)) get_HDI <- list(get_CI)
      
      # i <- 1
      for (i in seq_along(type)) {
        
        out <- as.data.frame(rv$metaList[[type[[i]]]]$meta)
        tmpunit <- extract_units(rownames(
          out[grep("mean", rownames(out)), ]))
        
        est <- out[grep("mean", rownames(out)), ]$est
        lci <- out[grep("mean", rownames(out)), ]$low
        uci <- out[grep("mean", rownames(out)), ]$high
        
        overlaps_with <- dplyr::between(
            tmpunit %#% get_truth[[i]], lci, uci)
        
        txt_nsims <- ifelse(
          length(rv$simList) == 1,
          "of a single simulation",
          paste("of our set of", length(rv$simList), "simulations"))
        
        txt_mean <- span(
          "The mean", txt_type[[i]], txt_nsims,
          if (overlaps_with) {
            span("overlapped", class = "cl-sea")
          } else {
            span("did not overlap", class = "cl-dgr") },
          "with the truth.")
        
        if (is.na(get_CI[[i]][["CI_low"]]) &&
            is.na(get_CI[[i]][["CI_high"]])) {
          txt_uncertainty <- 
            "however, run more simulations to confirm."
        } else {
          txt_uncertainty <- ifelse(
            get_CI[[i]][["CI_high"]] < .3 && 
              get_CI[[i]][["CI_low"]] > -.3,
            "and with low uncertainty.",
            "but with high uncertainty.")
        }
        
        txt_final <- span(
          style = paste("font-weight: 800;",
                        "font-family: var(--monosans);"),
          
          "The number of simulations is likely insufficient",
          "to obtain an accurate mean", 
          wrap_none(txt_type[[i]], color = pal$dgr, end = ","),
          txt_uncertainty)
        
        if (overlaps_with) {
          txt_final <- span(
            style = paste("font-weight: 800;",
                          "font-family: var(--monosans);"),
            
            "The number of simulations is likely sufficient",
            "to obtain an accurate mean", 
            wrap_none(txt_type[[i]], color = pal$sea, end = ","),
            txt_uncertainty)
        }
        
        # if (is.na(HDI$CI_low) && is.na(HDI$CI_high)) {
        #   txt_final <- tagList(
        #     txt_final,
        #     span(
        #     style = paste("font-weight: 800;",
        #                   "font-family: var(--monosans);"),
        #     "Please run more simulations in the corresponding",
        #     shiny::icon("compass-drafting", class = "cl-sea"),
        #     span("Analyses", class = "cl-sea"), "tab to confirm."))
        # }
        
        if (i == 1 && length(type) == 1)
          out_meta <- p(
            p(txt_title[[i]],
                 style = paste("font-size: 18px;",
                               "color: var(--sea-dark);",
                               "font-family: var(--monosans);"),
                 class = "ttl-tab"),
            p(out_meta,
              txt_mean,
              txt_final))
        else if (i == 1 && length(type) > 1)
          out_meta <- tagList(
            out_meta,
            txt_mean, 
            txt_final)

        if (i == 2)
          out_meta <- p(
            p("Home range meta-analyses:",
                 style = paste("font-size: 18px;",
                               "color: var(--sea-dark);",
                               "font-family: var(--monosans);")),
            p(out_meta),
            p(),
            p("Speed & distance meta-analyses:",
                 style = paste("font-size: 18px;",
                               "color: var(--sea-dark);",
                               "font-family: var(--monosans);")),
            p(txt_mean, 
              txt_final),
            p())
      }

      rv$report$meta <- tagList(
        out_meta,
        p(style = paste("font-size: 16px;",
                           "font-weight: 800;",
                           "font-family: var(--monosans);"),
             "Check the", shiny::icon("layer-group",
                                      class = "cl-sea"),
             span("Meta-analyses", class = "cl-sea"), "tab",
             "for more detailed information."))

    }) # end of observe
    
    #### Ratios of research target(s) -------------------------------------
    
    observe({
      req(rv$active_tab == "report")
      rv$report$groups <- out_groups <- span("")
      
      req(rv$grouped, rv$groups,
          rv$which_meta == "compare")
      req(rv$metaList_groups[[3]],
          rv$metaList)
      
      type <- get_N <- get_CI <- get_HDI <- NULL
      txt_type <- txt_title <- NULL
      if ("Home range" %in% rv$which_question) {
        type <- c("hr")
        txt_type <- c("home range area")
        txt_title <- "Home range meta-analyses:"
        
        get_CI <- rv$metaErr[grep("hr", rv$metaErr$type), ]
        get_CI <- c("CI_low" = get_CI[nrow(get_CI), "lci"],
                    "CI_est" = get_CI[nrow(get_CI), "est"],
                    "CI_high" = get_CI[nrow(get_CI), "uci"])
        
        get_HDI <- c("CI" = rv$hr_HDI$CI,
                     "CI_low" = rv$hr_HDI$CI_low,
                     "CI_high" = rv$hr_HDI$CI_high)
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        type <- c(type, "ctsd")
        txt_type <- c(txt_type, "movement speed")
        txt_title <- c(txt_title, "Speed meta-analyses:")
        
        CI <- rv$metaErr[grep("ctsd", rv$metaErr$type), ]
        CI <- c("CI_low" = CI[nrow(CI), "lci"],
                "CI_est" = CI[nrow(CI), "est"],
                "CI_high" = CI[nrow(CI), "uci"])
        
        if (length(rv$which_question) == 1) {
          get_HDI <- c("CI" = rv$sd_HDI$CI,
                       "CI_low" = rv$sd_HDI$CI_low,
                       "CI_high" = rv$sd_HDI$CI_high)
          get_CI <- CI
        } else {
          get_HDI <- list(get_HDI, 
                          c("CI" = rv$sd_HDI$CI,
                            "CI_low" = rv$sd_HDI$CI_low,
                            "CI_high" = rv$sd_HDI$CI_high))
          get_CI <- list(get_CI, CI)
        }
      }
      
      if (!is.list(get_HDI)) get_HDI <- list(get_HDI)
      if (!is.list(get_CI)) get_HDI <- list(get_CI)
      
      # i <- 1
      for (i in seq_along(type)) {
        
        meta_truth <- rv$metaList_groups[[1]][[type[[i]]]]
        meta <- rv$metaList_groups[[2]][[type[[i]]]]
        
        is_subpop <- meta_truth$logs$subpop_detected
        is_subpop_detected <- meta$logs$subpop_detected
        
        txt_certainty <- NULL
        if (is_subpop) {
          txt_subpop <- span(
            "We expected to detect a sub-population",
            "through meta-analyses.")
          
        } else {
          if (meta_truth$mods$subpop_detected[[2,2]] < 2)
            txt_subpop <- span(
              "There was insufficient evidence in the",
              rv$data_type, "dataset to detect subpopulations.")
          else 
            txt_subpop <- span(
              "We expected no sub-populations to be detected",
              "through meta-analyses.")
        }
        
        if (is_subpop == is_subpop_detected) {
          txt_to_add <- "As expected,"
          col_subpop <- "cl-sea-d"
        } else {
          txt_to_add <- "However,"
          if (meta_truth$mods$subpop_detected[[2,2]] < 2) {
            col_subpop <- "cl-gld"
          } else {
            col_subpop <- "cl-dgr"
          }
        }
        
        if (is_subpop_detected) {
          txt_subpop_detected <- span(
            txt_to_add, 
            span("sub-populations were detected", class = col_subpop),
            ifelse(meta$mods$subpop_detected[[2,2]] < 2, 
                   "(though with \u0394AICc \uFF1C 2).",
                   "(\u0394AICc \uFF1E 2)."))
        } else {
          txt_subpop_detected <- span(
            txt_to_add, 
            span("sub-populations were not detected", class = col_subpop),
            ifelse(meta$mods$subpop_detected[[2,2]] < 2, 
                   "(though with \u0394AICc \uFF1C 2).",
                   "(\u0394AICc \uFF1E 2)."))
        }
        
        expected_ratio <- extract_ratios(meta_truth)
        observed_ratio <- extract_ratios(meta)
        txt_ratio_order <- "(group A/group B)"
        
        ratio <- paste0(round(observed_ratio$est, 2), ":1")
        overlaps_with <- list(
          "truth" = dplyr::between(observed_ratio$est,
                                   expected_ratio$lower, 
                                   expected_ratio$upper),
          "one_expected" = expected_ratio$lower <= 1 &
            expected_ratio$upper >= 1,
          "one_observed" = observed_ratio$lower <= 1 &
            observed_ratio$upper >= 1)
        
        txt_ratio <- span(
          "The", txt_type[[i]], "ratio", txt_ratio_order,
          ifelse(
            overlaps_with$one_observed,
            paste0("overlapped with one (i.e., ",
                   "no difference between groups)."),
            paste0("did not overlap with one (ratio point estimate of ",
                   wrap_none(ratio, ")."))))
        
        txt_final <- span(
          style = paste("font-weight: 800;",
                        "font-family: var(--monosans);"),
          
          "The number of simulations is likely insufficient",
          "to obtain accurate", 
          wrap_none(txt_type[[i]], color = pal$sea, " ratios."))
        
        if (overlaps_with$truth &&
            (overlaps_with$one_expected == overlaps_with$one_observed) &&
            (!is.na(get_CI[[i]][["CI_low"]]) &&
             !is.na(get_CI[[i]][["CI_high"]]))) {
          
          txt_final <- span(
            style = paste("font-weight: 800;",
                          "font-family: var(--monosans);"),
            
            "The number of simulations is likely sufficient",
            "to obtain", 
            wrap_none(txt_type[[i]], color = pal$sea, " ratios."))
          
        } else {
          txt_final <- span(
            style = paste("font-weight: 800;",
                          "font-family: var(--monosans);"),
            
            "The number of simulations is likely insufficient",
            "to obtain", # accurate", 
            wrap_none(txt_type[[i]], color = pal$sea, " ratios."))
        }
        
        if (i == 1 && length(type) == 1)
          out_groups <- p(
            span(txt_title[[i]],
                 style = paste("font-size: 18px;",
                               "color: var(--sea-dark);",
                               "font-family: var(--monosans);"),
                 class = "ttl-tab"), br(),
            out_groups,
            txt_subpop,
            txt_subpop_detected,
            txt_ratio,
            txt_final)
        else if (i == 1 && length(type) > 1)
          out_groups <- tagList(
            out_groups,
            txt_subpop,
            txt_subpop_detected,
            txt_ratio,
            txt_final)
        
        if (i == 2)
          out_groups <- p(
            span("Home range meta-analyses:", 
                 style = paste("font-size: 18px;",
                               "color: var(--sea-dark);",
                               "font-family: var(--monosans);")), br(),
            out_groups,
            p(),
            span("Speed & distance meta-analyses:", 
                 style = paste("font-size: 18px;",
                               "color: var(--sea-dark);",
                               "font-family: var(--monosans);")), br(),
            p(txt_subpop,
              txt_subpop_detected,
              txt_ratio,
              txt_final))
        
      }
      
      rv$report$groups <- tagList(
        out_groups,
        span(style = paste("font-size: 16px;",
                        "font-weight: 800;",
                        "font-family: var(--monosans);"),
          "Check the", shiny::icon("layer-group",
                                   class = "cl-sea"),
          span("Meta-analyses", class = "cl-sea"), "for more",
          "detailed information."))
      
    }) # end of observe
    
    
    ## Rendering complete report: -----------------------------------------
    
    observe({
      req(rv$which_question,
          rv$report$species,
          rv$report$regime,
          rv$report$analyses)
      
      if (length(rv$which_question) > 1) {
        shinyjs::hide(id = "section-comparison")
      } else { shinyjs::show(id = "section-comparison") }
      
      is_both <- ifelse(length(rv$which_question) > 1, "Yes", "No")
      
      switch(
        is_both,
        "No" = {
          if (rv$which_question == "Home range") {
            output$end_report <- renderUI({
              
              out <- tagList(
                rv$report$species,
                rv$report$regime,
                
                div(width = 12, align = "center",
                    style = "z-index: 999;",
                    
                    shinyWidgets::awesomeCheckbox(
                      inputId = ns("scale_density"),
                      label = span("Scaled density to 1", 
                                   icon("wrench")),
                      value = TRUE)),
                
                ggiraph::girafeOutput(
                  outputId = ns("repPlot_hr"),
                  width = "100%", height = "100%"),
                uiOutput(ns("repPlotLegend1")),
                
                # fluidRow(
                #   style = "text-align: center;",
                #   span("Precision of",
                #        wrap_none(span("designs",
                #                       class = "cl-sea"), ":")) %>%
                #     tagAppendAttributes(class = "subheader"),
                #   actionButton(
                #     inputId = ns("explain_precision"),
                #     icon = icon("circle-question"),
                #     label = NULL,
                #     style = paste("background-color: #fff;",
                #                   "color: black;",
                #                   "padding: 0;",
                #                   "margin: -5px 0 0 0;")) %>%
                #     bsplus::bs_attach_modal(id_modal = "modal_precision")
                # ), # end of fluidRow
                
                ggiraph::girafeOutput(
                  outputId = ns("repPlot_precision"),
                  width = "100%", height = "100%"),
                uiOutput(ns("repPlotLegend2")),
                
                rv$report$analyses) # end of tagList
              
            }) # end of renderUI, "end_report"
          } # end of hr only section
          
          if (rv$which_question == "Speed & distance") {
            output$end_report <- renderUI({
              
              out <- tagList(
                rv$report$species,
                rv$report$regime,
                
                div(width = 12, align = "center",
                    style = "z-index: 999;",
                    
                    shinyWidgets::awesomeCheckbox(
                      inputId = ns("scale_density"),
                      label = span("Scaled density to 1", 
                                   icon("wrench")),
                      value = TRUE)),
                
                ggiraph::girafeOutput(
                  outputId = ns("repPlot_sd"),
                  width = "100%", height = "100%"),
                uiOutput(ns("repPlotLegend1")),
                
                # fluidRow(
                #   span("Precision of",
                #        wrap_none(span("designs",
                #                       class = "cl-sea"), ":")) %>%
                #     tagAppendAttributes(class = "subheader"),
                #   actionButton(
                #     inputId = ns("explain_precision"),
                #     icon = icon("circle-question"),
                #     label = NULL,
                #     style = paste("background-color: #fff;",
                #                   "color: black;",
                #                   "padding: 0;",
                #                   "margin: -5px 0 0 0;")) %>%
                #     bsplus::bs_attach_modal(
                #       id_modal = "modal_precision")
                # ), # end of fluidRow
                
                ggiraph::girafeOutput(
                  outputId = ns("repPlot_precision"),
                  width = "100%", height = "100%"),
                uiOutput(ns("repPlotLegend2")),
                
                rv$report$analyses) # end of tagList
              
            }) # end of renderUI, "end_report"
          } # end of sd only section
        },
        "Yes" = {
          
          output$end_report <- renderUI({
            out <- tagList(
              rv$report$species,
              rv$report$regime,
              rv$report$analyses)
          })
          
          output$end_report_both <- renderUI({
            req(length(rv$which_question) > 1)
            
            out <- div(
              tagList(
                splitLayout(
                  cellWidths = c("50%", "50%"), 
                  tagList(
                    p(span("AKDE error", class = "cl-mdn")) %>%
                      tagAppendAttributes(class = "subheader"),
                    ggiraph::girafeOutput(
                      outputId = ns("repPlot_hr"),
                      width = "100%", height = "100%")), 
                  tagList(
                    p(span("CTSD error", class = "cl-mdn")) %>%
                      tagAppendAttributes(class = "subheader"),
                    ggiraph::girafeOutput(
                      outputId = ns("repPlot_sd"),
                      width = "100%", height = "100%"))),
                uiOutput(ns("repPlotLegend1")),
                
                # fluidRow(
                #   span("Precision of",
                #        wrap_none(span("designs",
                #                       class = "cl-sea"), ":")) %>%
                #     tagAppendAttributes(class = "subheader"),
                #   actionButton(
                #     inputId = ns("explain_precision"),
                #     icon = icon("circle-question"),
                #     label = NULL,
                #     style = paste("background-color: #fff;",
                #                   "color: black;",
                #                   "padding: 0;",
                #                   "margin: -5px 0 0 0;")) %>%
                #     bsplus::bs_attach_modal(
                #       id_modal = "modal_precision")
                # ), # end of fluidRow
                
                ggiraph::girafeOutput(
                  outputId = ns("repPlot_precision"),
                  width = "100%", height = "100%"),
                
                uiOutput(ns("repPlotLegend2")),
                
                div(id = "report_meta",
                    style = paste0("background-color: #f4f4f4;",
                                   "padding: 20px;",
                                   "margin-top: 20px;"),
                    rv$report$meta,
                    rv$report$groups)
                
              ) # end of tagList
            ) # end of div
            
          }) # end of renderUI, "end_report_both"
          
        }) # end of switch
      
      
      if (rv$which_meta == "none") {
        shinyjs::show(id = "section-highlight_dur")
        shinyjs::show(id = "section-highlight_dti")
      } else {
        shinyjs::hide(id = "section-highlight_dur")
        shinyjs::hide(id = "section-highlight_dti")
      }
      
    }) %>% bindEvent(input$build_report)
    
    ## Reporting COMPARISON (if available): ------------------------------
    
    observe({
      out_comp <- out_comp_hr <- span("")
      
      if (length(rv$which_question) == 1 &
          "Home range" %in% rv$which_question) {
        req(rv$highlight_dur)
        
        highlighted_dur <- as.numeric(rv$highlight_dur)
        
        CI <- round(rv$hr_HDI_new$CI * 100, 0)
        LCI <- round(rv$hr_HDI_new$CI_low * 100, 1)
        UCI <- round(rv$hr_HDI_new$CI_high * 100, 1)
        
        txt_level <- ifelse(
          rv$hr_HDI_new$CI_high < .3 & rv$hr_HDI_new$CI_low > -.3,
          "and with low", "but with high")
        
        ideal_dur <- fix_unit(
          ("days" %#% rv$tau_p[[1]]$value[2] %#%
             rv$tau_p[[1]]$unit[2]) * 10,
          "days")
        
        if (highlighted_dur >= ideal_dur$value) {
          out_comp <- out_comp_hr <-
            p("Your new sampling duration would likely be sufficient",
              "for", span("home range", class = "cl-grn"),
              "estimation,", txt_level, "uncertainty:",
              "for a duration of",
              highlighted_dur, "days, there is a",
              wrap_none(CI, "%", css = "cl-blk"),
              "probability that the relative error will lie within",
              wrap_none(LCI, "%", css = "cl-blk"),
              "and", wrap_none(UCI, "%", end = ".", css = "cl-blk"))
          
        } else {
          out_comp <- out_comp_hr <-
            p("Your new sampling duration would likely be insufficient",
              "for", span("home range", class = "cl-grn"),
              "estimation.", br(),
              "For a duration of", highlighted_dur,
              "days, there is high uncertainty",
              wrap_none("(", CI, "%", css = "cl-blk"),
              "probability that the relative error will lie within",
              wrap_none(LCI, "%", css = "cl-blk"),
              "and", wrap_none(UCI, "%", end = ").", css = "cl-blk"))
        }
      } # end of 'Home range'
      
      ## Speed and distance estimation:
      
      if (length(rv$which_question) == 1 &
          "Speed & distance" %in% rv$which_question) {
        req(rv$highlight_dti)
        
        opts <- movedesign::sims_speed[[1]] %>%
          dplyr::select(dti, dti_notes) %>%
          unique()
        
        highlighted_dti <- opts$dti[match(rv$highlight_dti,
                                          opts$dti_notes)]
        
        out_dti <- fix_unit(highlighted_dti, "seconds",
                            convert = TRUE)
        
        CI <- round(rv$sd_HDI_new$CI * 100, 0)
        LCI <- round(rv$sd_HDI_new$CI_low * 100, 1)
        UCI <- round(rv$sd_HDI_new$CI_high * 100, 1)
        
        txt_level <- ifelse(
          rv$sd_HDI_new$CI_high < .3 & rv$sd_HDI_new$CI_low > -.3,
          "and with low", "but with high")
        
        ideal_dti <- fix_unit(
          (rv$tau_v[[1]]$value[2] %#% 
             rv$tau_v[[1]]$unit[2]) / 3, "seconds")
        
        if (highlighted_dti <= ideal_dti$value) {
          out_comp <- out_comp_sd <-
            p("Your new sampling interval would likely be sufficient",
              "for", span("speed & distance", class = "cl-grn"),
              "estimation,", txt_level, "uncertainty:",
              "for a sampling interval of",
              wrap_none(out_dti$value, " ", out_dti$unit, ", there"),
              "is a", wrap_none(CI, "%", css = "cl-blk"),
              "probability that the relative error will lie within",
              wrap_none(LCI, "%", css = "cl-blk"),
              "and", wrap_none(UCI, "%", end = ".", css = "cl-blk"))
          
        } else {
          out_comp <- out_comp_sd <-
            p("Your new sampling interval would likely be insufficient",
              "for", span("speed & distance", class = "cl-grn"),
              "estimation. For a sampling interval of",
              wrap_none(out_dti$value, " ", out_dti$unit, ", there is"),
              "high uncertainty",
              wrap_none("(", CI, "%", css = "cl-blk"),
              "probability that the relative error will lie within",
              wrap_none(LCI, "%", css = "cl-blk"),
              "and", wrap_none(UCI, "%", end = ").", css = "cl-blk"))
        }
      } # end of "Speed & distance"
      
      ### Both home range and speed & distance:
      
      if (length(rv$which_question) > 1) {
        out_analyses <-
          span("Your new tracking regime (...)",
               "for", span("home range", class = "cl-grn"), "...",
               "for", span("speed & distance", class = "cl-grn"), "...")
      }
      
      output$end_comparison <- renderUI({
        div(id = "report_comparison",
            style = paste0("background-color: #f4f4f4;",
                           "padding: 20px;",
                           "margin-top: 20px;"),
            out_comp)
        
      }) # end of renderUI, "end_comparison"
      
    }) # end of observe
    
    # PLOTS ---------------------------------------------------------------
    ## Rendering density plots: -------------------------------------------
    
    output$repPlotLegend1 <- renderUI({
      req(rv$which_question, rv$which_meta, input$ci)
      req(rv$tau_p[[1]], rv$tau_v[[1]], rv$dur, rv$dti)
      
      m <- ifelse(rv$which_meta == "none", 400, length(rv$simList))
      taup_unit <- ifelse(rv$which_meta == "none", 
                          "days", rv$tau_p[[1]]$unit[2])
      
      input_taup <- taup_unit %#% 
        rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      input_tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
      input_dur <- taup_unit %#% rv$dur$value %#% rv$dur$unit
      input_dti <- rv$dti$value %#% rv$dti$unit
      
      dt_hr <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      out_taup <- dt_hr$tau_p[which.min(abs(dt_hr$tau_p - input_taup))]
      
      if (rv$which_meta != "none") {
        taup_unit <- fix_unit(input_taup, taup_unit)$unit
        dur_for_hr <- paste0(round(input_dur, 1), " ", taup_unit, ",")
      } else {
        dur_for_hr <- paste(
          dt_hr$dur[which.min(
            abs(dt_hr$dur - input_dur))],
          " ", taup_unit, ",")
      }
      
      dt_sd <- movedesign::sims_speed[[1]] %>%
        dplyr::mutate(dur = round("days" %#% dur, 1))
      out_tauv <- dt_sd$tau_v[which.min(abs(dt_sd$tau_v - input_tauv))]
      out_tauv <- fix_unit(out_tauv, "seconds", convert = TRUE)
      tauv_unit <- out_tauv$unit
      dur_for_sd <- dt_sd$dur[which.min(abs(dt_sd$dur - input_dur))]
      
      dt_sd <- movedesign::sims_speed[[1]] %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      out_dti <- dt_sd$dti[which.min(abs(dt_sd$dti - input_dti))]
      dti_for_sd <- dt_sd$dti_notes[match(out_dti, dt_sd$dti)]
      
      txt_highlight <- ""
      
      add_txt_highlight <- FALSE
      if (!is.null(rv$highlight_dur)) {
        if (rv$highlight_dur != "") add_txt_highlight <- TRUE
      }
      
      if (!is.null(rv$highlight_dti)) {
        if (rv$highlight_dti != "") add_txt_highlight <- TRUE
      }
      
      if (add_txt_highlight) {
        txt_highlight <- span(
          "The comparison requested is in",
          wrap_none(
            fontawesome::fa("diamond", fill = "black"),
            " in ", span("black", style = "color: black;"), "."))
      }
      
      
      if (length(rv$which_question) > 1) {
        if (rv$which_meta == "none") {
          sim_hr_details <- paste0(
            "movement processes with \u03C4\u209A = ", 
            out_taup, " day(s);")
          sim_sd_details <- paste0(
            "\u03C4\u1D65 = ", 
            out_tauv$value, " ", out_tauv$unit, ".")
        } else {
          input_tauv <- tauv_unit %#% input_tauv
          tauv_unit <- abbrv_unit(tauv_unit)
          sim_hr_details <- paste0(
            "movement processes with \u03C4\u209A = ", 
            round(input_taup, 1), " ", taup_unit, ";")
          sim_sd_details <- paste0(
            "\u03C4\u1D65 = ", 
            round(input_tauv, 1), " ", tauv_unit, ".")
        }
        
        ui <- tagList(
          fontawesome::fa("circle-exclamation", fill = pal$dgr),
          span("Note:", class = "help-block-note"), 
          "These plots show the probability density of estimate errors",
          "based on ", m, " simulations, with the medians",
          wrap_none(
            "(", fontawesome::fa("diamond"), " in lighter colors),"),
          "and the", wrap_none(input$ci, "%"),
          "credible intervals (shaded areas + lines).",
          "For AKDE, the ", m, " simulations were based on", 
          sim_hr_details,
          "for CTSD,",
          sim_sd_details,
          "Your simulation(s)' mean",
          "estimate errors are the circles", wrap_none(
            "(", fontawesome::fa("circle", prefer_type = "solid"), ")"),
          "in darker colors.")
        
      } else {
        
        switch(
          rv$which_question,
          "Home range" = {
            
            if (rv$which_meta == "none") {
              sim_details <- paste0(
                "movement processes with \u03C4\u209A = ", 
                out_taup, " day(s),")
            } else {
              sim_details <- paste0(
                "movement processes with \u03C4\u209A = ", 
                round(input_taup, 1), " ", taup_unit, ";")
            }
            
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "This plot shows the probability density of estimate",
              "errors based on", m, "simulations for",
              sim_details,
              "and for a sampling duration of", 
              dur_for_hr, "with the median",
              wrap_none(
                "(", fontawesome::fa("diamond", fill = pal$sea),
                " in ", span("light blue", class = "cl-sea"), "),"),
              "and the", wrap_none(input$ci, "%"),
              "credible intervals (shaded area).",
              "In contrast, your simulation(s)' mean",
              "error is the circle", wrap_none(
                "(", fontawesome::fa("circle", prefer_type = "solid",
                                     fill = pal$sea_d), ")"),
              "in", wrap_none(span("darker blue",
                                   class = "cl-sea-d"), "."),
              txt_highlight)
          },
          "Speed & distance" = {
            
            input_tauv <- tauv_unit %#% input_tauv
            tauv_unit <- abbrv_unit(tauv_unit)
            
            if (rv$which_meta == "none") {
              sim_details <- paste0(
                "movement processes with \u03C4\u1D65 = ", 
                out_tauv$value, " ", out_tauv$unit)
            } else {
              sim_details <- paste0(
                "movement processes with \u03C4\u1D65 = ", 
                round(input_tauv, 1), " ", tauv_unit)
            }
            
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "This plot shows the probability density of estimate",
              "errors based on", m, "simulations for",
              sim_details,
              "and for a sampling interval of", 
              wrap_none(dti_for_sd, ","), "with the median",
              wrap_none(
                "(", fontawesome::fa("diamond", fill = pal$sea),
                " in ", span("light blue", class = "cl-sea"), "),"),
              "and the", wrap_none(input$ci, "%"),
              "credible intervals (shaded area).",
              "Your simulation(s)' mean",
              "error is the circle", wrap_none(
                "(", fontawesome::fa("circle", prefer_type = "solid",
                                     fill = pal$sea_d), ")"),
              "in", wrap_none(span("darker blue",
                                   class = "cl-sea-d"), "."),
              txt_highlight)
          },
          stop(paste0("No handler for ",
                      rv$which_question, "."))
        )
      }
      
      ui <- span(class = "help-block", ui)
      
      return(ui)
      
    }) # end of renderUI, "repPlotLegend1"
    
    #### Accuracy of home range simulations: ------------------------------
    
    output$repPlot_hr <- ggiraph::renderGirafe({
      req(rv$which_question, rv$which_meta, input$ci)
      
      m <- ifelse(rv$which_meta == "none", 400, length(rv$simList))
      
      taup_unit <- ifelse(rv$which_meta == "none", 
                          "days", rv$tau_p[[1]]$unit[2])
      
      input_ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      input_taup <- taup_unit %#% 
        rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      input_dur <- taup_unit %#% rv$dur$value %#% rv$dur$unit
      
      is_both <- FALSE
      rv$ft_size <- 13
      if (length(rv$which_question) > 1) {
        is_both <- TRUE
        rv$ft_size <- 16
      }
      
      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")
      
      # Preparing if () statements:
      
      is_dur <- FALSE
      if (!is.null(rv$highlight_dur))
        if (!is.na(as.numeric(rv$highlight_dur))) 
          is_dur <- TRUE
      
      is_log <- FALSE
      if (!is.null(input$scale_density))
        if (input$scale_density) 
          is_log <- TRUE
      
      # Prepare datasets:
      
      if (rv$which_meta == "none") {
        dt_hr <- movedesign::sims_hrange[[1]] %>%
          dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
          dplyr::mutate(duration = round("days" %#% duration, 1))
        
        out_taup <- dt_hr$tau_p[which.min(abs(dt_hr$tau_p - input_taup))]
        dur_for_hr <- dt_hr$dur[which.min(abs(dt_hr$dur - input_dur))]
        
        # Create density data frames:
        ds1_hr <- dt_hr %>%
          dplyr::filter(tau_p == out_taup) %>%
          dplyr::filter(duration == dur_for_hr) %>%
          stats::na.omit()
        
      } else {
        req(rv$hr$tbl)
        
        dt_hr <- data.frame(
          tau_p = input_taup,
          duration = input_dur,
          error = rv$hr$tbl$area_err,
          error_lci = rv$hr$tbl$area_err_min, 
          error_uci = rv$hr$tbl$area_err_max)
        
        out_taup <- input_taup
        dur_for_hr <- input_dur
        
        # Create density data frames:
        ds1_hr <- stats::na.omit(dt_hr)
      }
      
      rv$report$dur_for_hr <- paste0(round(dur_for_hr, 1),
                                     " ", taup_unit, ",")
      
      # Calculate median/ci:
      med <- stats::median(ds1_hr$error)
      ds1_hr <- stats::density(ds1_hr$error)
      ds1_hr <- data.frame(x = ds1_hr$x, y = ds1_hr$y)
      rv$report$ds1_hr <- data.frame(
        "median" = med,
        "max" = max(ds1_hr$x),
        "min" = min(ds1_hr$x),
        "done" = FALSE)
      
      if (is_log) ds1_hr$y <- ds1_hr$y / max(ds1_hr$y)
      
      ci1_hr <- subset(
        ds1_hr, x >= rv$hr_HDI$CI_low & x <= rv$hr_HDI$CI_high)
      
      if (is_dur) {
        req(rv$hr_HDI_new)
        
        out_dur_new <- dt_hr$dur[
          abs(dt_hr$dur - as.numeric(rv$highlight_dur)) %>%
            which.min()]
        
        ds2_hr <- dt_hr %>%
          dplyr::filter(tau_p == out_taup) %>%
          dplyr::filter(duration == out_dur_new) %>%
          stats::na.omit()
        med <- stats::median(ds2_hr$error)
        
        ds2_hr <- stats::density(ds2_hr$error)
        ds2_hr <- data.frame(x = ds2_hr$x, y = ds2_hr$y)
        rv$report$ds2_hr <- data.frame(
          "median" = med,
          "max" = max(ds2_hr$x),
          "min" = min(ds2_hr$x))
        
        rv$hr_HDI_new <- 
          suppressWarnings(bayestestR::ci(
            ds2_hr$x, ci = input_ci, method = "HDI"))
        
        if (is_log) ds2_hr$y <- ds2_hr$y / max(ds2_hr$y)
        
        ci2_hr <- subset(
          ds2_hr, x >= rv$hr_HDI_new$CI_low &
            x <= rv$hr_HDI_new$CI_high)
        
        hr_p1 <- ggplot2::geom_line(
          data = ds2_hr, mapping = ggplot2::aes(x = x, y = y),
          col = pal$mdn, linetype = "dotted")
        
        hr_p2 <- ggplot2::geom_area(
          data = ci2_hr,
          mapping = ggplot2::aes(x = x, y = y),
          alpha = 0.2, fill = pal$mdn)
        
        hr_p3 <- ggplot2::geom_segment(
          data = rv$hr_HDI_new,
          mapping = ggplot2::aes(
            x = .data$CI_low,
            xend = .data$CI_high,
            y = 0, yend = 0,
            col = "est_new", linetype = "est_new"),
          size = .8) %>% 
          suppressWarnings()
        
        hr_p4 <- ggplot2::geom_point(
          mapping = ggplot2::aes(
            x = rv$report$ds2_hr[["median"]], y = 0,
            col = "est_new", shape = "est_new"),
          size = 6) %>% 
          suppressWarnings()
      }
      
      lbl <- c(
        paste0("AKDE error"),
        paste0("Median AKDE error + ", rv$hr_HDI$CI * 100,
               "% HDI for ", rv$report$dur_for_hr))
      brk <- c("now", "est")
      
      val_fill <- val_col <- c("now" = pal$sea_d, "est" = pal$sea)
      val_linetype <- c("now" = "blank", "est" = "solid")
      val_shape <- c("now" = 19, "est" = 18)
      
      override_size <- c(.8, .8)
      override_stroke <- c(4, 4)
      
      if (is_dur) {
        lbl <- c(
          lbl, paste0("Median AKDE error + ", rv$hr_HDI$CI * 100,
                      "% HDI for ", rv$highlight_dur, " days"))
        brk <- c(brk, "est_new")
        
        val_fill <- val_col <- c(val_fill, "est_new" = pal$mdn)
        val_linetype <- c(val_linetype, "est_new" = "solid")
        val_shape <- c(val_shape, "est_new" = 18)
        
        override_size <- c(override_size, .8)
        override_stroke <- c(override_stroke, 4)
      }
      
      y_lab <- ifelse(input$scale_density,
                      "Probability density", "Density")
      
      p <- ds1_hr %>%
        ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_vline(xintercept = 0, alpha = 1) +
        
        { if (is_dur) hr_p1 } +
        { if (is_dur) hr_p2 } +
        
        ggplot2::geom_line(
          ggplot2::aes(col = "est"),
          linetype = "dotted") +
        
        { if (!is.na(rv$hr_HDI$CI_low) && !is.na(rv$hr_HDI$CI_low))
          ggplot2::geom_area(
            data = ci1_hr,
            mapping = ggplot2::aes(x = x, y = y, fill = "est"),
            alpha = 0.4) } +
        
        { if (is_dur) hr_p3 } +
        
        { if (!is.na(rv$hr_HDI$CI_low) && !is.na(rv$hr_HDI$CI_low))
          ggplot2::geom_segment(
            data = ~ head(.x, 1),
            mapping = ggplot2::aes(
              x = rv$hr_HDI$CI_low,
              xend = rv$hr_HDI$CI_high,
              y = 0, yend = 0, col = "est",
              linetype = "est"),
            size = .8) %>% 
            suppressWarnings() } +
        
        { if (is_dur) hr_p4 } +
        
        ggplot2::geom_point(
          data = ~ head(.x, 1),
          mapping = ggplot2::aes(
            x = rv$report$ds1_hr[["median"]], y = 0,
            col = "est", shape = "est"),
          size = 6) %>% 
        suppressWarnings() +
        
        { if (!is.null(rv$hrErr)) 
          ggplot2::geom_point(
            data = ~ head(.x, 1),
            ggplot2::aes(x = mean(rv$hrErr$est, na.rm = TRUE),
                         y =  0, col = "now", shape = "now"),
            size = 6, alpha = .7) %>% 
            suppressWarnings()
        } +
        
        ggplot2::scale_x_continuous(labels = scales::percent) +
        { if (!is.null(input$scale_density))
          if (input$scale_density)
            ggplot2::scale_y_continuous(breaks = seq(0, 1, .5))
        } +
        
        ggplot2::scale_color_manual(
          name = "", labels = lbl, breaks = brk,
          values = val_col) +
        { if (!is.na(rv$hr_HDI$CI_low) && !is.na(rv$hr_HDI$CI_low))
          ggplot2::scale_fill_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_fill) } +
        { if (!is.na(rv$hr_HDI$CI_low) && !is.na(rv$hr_HDI$CI_low))
          ggplot2::scale_linetype_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_linetype) } +
        ggplot2::scale_shape_manual(
          name = "", labels = lbl, breaks = brk,
          values = val_shape) +
        
        ggplot2::labs(x = "Estimate error (%)",
                      y = y_lab) +
        
        theme_movedesign(font_available = rv$is_font,
                         ft_size = rv$ft_size) +
        ggplot2::theme(
          legend.position = "none",
          axis.title.x = ggplot2::element_blank())
      
      rv$report$ds1_hr[["done"]] <- TRUE
      
      ggiraph::girafe(
        ggobj = suppressMessages(suppressWarnings(p)),
        width_svg = 6, height_svg = 4,
        options = list(
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("r: 4pt;",
                        "fill: #006263;",
                        "stroke: #006263;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("r: 4pt;",
                        "fill: #004647;",
                        "stroke: #004647;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))
      
    }) %>% # end of renderGirafe // repPlot_hr
      bindEvent(list(input$build_report,
                     input$scale_density,
                     rv$highlight_dur))
    
    #### Accuracy of speed & distance simulations: ------------------------
    
    observe({
      req(rv$simList, rv$tau_v[[1]],
          rv$dur, rv$dti,
          input$ci, rv$which_question)
      
      is_both <- FALSE
      rv$ft_size <- 13
      if (!is.null(rv$which_question)) {
        if (length(rv$which_question) > 1) {
          is_both <- TRUE
          rv$ft_size <- 16
        }
      }
      
      req("Speed & distance" %in% rv$which_question)
      input_ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      
      input_tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
      input_dur <- "days" %#% rv$dur$value %#% rv$dur$unit
      input_dti <- rv$dti$value %#% rv$dti$unit
      
      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")
      
      # Preparing if () statements:
      
      is_dti <- FALSE
      if (!is.null(rv$highlight_dti)) {
        if (rv$highlight_dti != "")
          is_dti <- TRUE
      }
      
      is_log <- FALSE
      if (!is.null(input$scale_density)) {
        if (input$scale_density)
          is_log <- TRUE
      }
      
      # Prepare datasets:
      
      dt_sd <- movedesign::sims_speed[[1]] %>%
        dplyr::mutate(dur = round("days" %#% dur, 0)) %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 0))
      sd_opts <- movedesign::sims_speed[[1]] %>%
        dplyr::mutate(dur = round("days" %#% dur, 0)) %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      
      out_tauv <- dt_sd$tau_v[which.min(abs(dt_sd$tau_v - input_tauv))]
      dur_for_sd <- dt_sd$dur[which.min(abs(dt_sd$dur - input_dur))]
      
      out_dti <- dt_sd$dti[which.min(abs(dt_sd$dti - input_dti))]
      txt_dti <- sd_opts$dti_notes[match(out_dti, sd_opts$dti)]
      rv$report$txt_dti <- txt_dti
      
      # Create density data frames:
      
      ds1_sd <- dt_sd %>%
        dplyr::filter(tau_v == out_tauv) %>%
        dplyr::filter(dur == dur_for_sd) %>%
        dplyr::filter(dti == out_dti) %>% 
        stats::na.omit()
      med <- stats::median(ds1_sd$error)
      
      ds1_sd <- stats::density(ds1_sd$error)
      ds1_sd <- data.frame(x = ds1_sd$x, y = ds1_sd$y)
      rv$report$ds1_sd <- data.frame(
        "median" = med,
        "max" = max(ds1_sd$x),
        "min" = min(ds1_sd$x),
        "done" = FALSE)
      
      if (is_log) ds1_sd$y <- ds1_sd$y / max(ds1_sd$y)
      
      ci1_sd <- subset(
        ds1_sd, x >= rv$sd_HDI$CI_low & x <= rv$sd_HDI$CI_high)
      
      if (is_dti) {
        req(rv$sd_HDI_new)
        
        dti_new <- sd_opts$dti[match(rv$highlight_dti,
                                     sd_opts$dti_notes)]
        out_dti_new <- dt_sd$dti[which.min(abs(dt_sd$dti - dti_new))]
        txt_dti_new <- sd_opts$dti_notes[match(out_dti_new,
                                               sd_opts$dti)]
        rv$report$txt_dti_new <- txt_dti_new
        
        ds2_sd <- dt_sd %>%
          dplyr::filter(tau_v == out_tauv) %>%
          dplyr::filter(dur == dur_for_sd) %>%
          dplyr::filter(dti == out_dti_new) %>%
          stats::na.omit()
        med <- stats::median(ds2_sd$error)
        
        if (nrow(ds2_sd) > 2) {
          msg_log(style = "warning",
                  message = paste0("Duration too low for ",
                                   msg_warning("comparison"), "."))
          
          shinyFeedback::showToast(
            type = "error",
            message = "Interval invalid.",
            .options = list(
              timeOut = 2500,
              extendedTimeOut = 3500,
              progressBar = TRUE,
              closeButton = TRUE,
              preventDuplicates = TRUE,
              positionClass = "toast-bottom-right")
          )
          req(nrow(ds2_sd) > 2)
        }
        
        ds2_sd <- stats::density(ds2_sd$error)
        ds2_sd <- data.frame(x = ds2_sd$x, y = ds2_sd$y)
        rv$report$ds2_sd <- data.frame(
          "median" = med,
          "max" = max(ds2_sd$x),
          "min" = min(ds2_sd$x))
        
        rv$sd_HDI_new <-
          suppressWarnings(bayestestR::ci(
            ds2_sd$x, ci = input_ci, method = "HDI"))
        
        if (is_log) ds2_sd$y <- ds2_sd$y / max(ds2_sd$y)
        
        ci2_sd <- subset(
          ds2_sd, x >= rv$sd_HDI_new$CI_low & x <= rv$sd_HDI_new$CI_high)
        
        sd_p1 <- ggplot2::geom_line(
          data = ds2_sd, mapping = ggplot2::aes(x = x, y = y),
          col = pal$mdn, linetype = "dotted")
        
        sd_p2 <- ggplot2::geom_area(
          data = ci2_sd,
          mapping = ggplot2::aes(x = x, y = y),
          alpha = 0.2, fill = pal$mdn)
        
        sd_p3 <- ggplot2::geom_segment(
          data = rv$sd_HDI_new,
          mapping = ggplot2::aes(
            x = .data$CI_low,
            xend = .data$CI_high,
            y = 0, yend = 0,
            col = "est_new", linetype = "est_new"),
          size = .8)
        
        sd_p4 <- ggplot2::geom_point(
          data = ds2_sd,
          mapping = ggplot2::aes(
            x = mean(x), y = 0,
            col = "est_new", shape = "est_new"),
          size = 6)
      }
      
      lbl <- c(
        paste0("CTSD error"),
        paste0("Median CTSD error + ", rv$sd_HDI$CI * 100,
               "% HDI for ", txt_dti))
      brk <- c("now", "est")
      
      val_fill <- val_col <- c(
        "now" = ifelse(is_both, pal$grn_d, pal$sea_d), 
        "est" = ifelse(is_both, pal$grn, pal$sea))
      val_linetype <- c("now" = "blank", "est" = "solid")
      val_shape <- c("now" = 19, "est" = 18)
      
      override_size <- c(.8, .8)
      override_stroke <- c(4, 4)
      
      if (is_dti) {
        lbl <- c(
          lbl, paste0("Median CTSD error + ", rv$sd_HDI$CI * 100,
                      "% HDI for ", txt_dti_new))
        brk <- c(brk, "est_new")
        
        val_fill <- val_col <- c(val_fill, "est_new" = pal$mdn)
        val_linetype <- c(val_linetype, "est_new" = "solid")
        val_shape <- c(val_shape, "est_new" = 18)
        
        override_size <- c(override_size, .8)
        override_stroke <- c(override_stroke, 4)
      }
      
      y_lab <- ifelse(input$scale_density,
                      "Probability density", "Density")
      
      #### Speed simulations:
      output$repPlot_sd <- ggiraph::renderGirafe({
        
        p <- ds1_sd %>%
          ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_vline(xintercept = 0, alpha = 1) +
          
          {if (is_dti) sd_p1 } +
          {if (is_dti) sd_p2 } +
          
          ggplot2::geom_line(
            ggplot2::aes(col = "est"),
            linetype = "dotted") +
          
          { if (!is.na(rv$sd_HDI$CI_low) && !is.na(rv$sd_HDI$CI_low))
            ggplot2::geom_area(
              data = ci1_sd,
              mapping = ggplot2::aes(x = x, y = y, fill = "est"),
              alpha = 0.4) } +
          
          { if (is_dti) sd_p3 } +
          
          { if (!is.na(rv$sd_HDI$CI_low) && !is.na(rv$sd_HDI$CI_low))
            ggplot2::geom_segment(
              data = ~ head(.x, 1),
              mapping = ggplot2::aes(
                x = rv$sd_HDI$CI_low,
                xend = rv$sd_HDI$CI_high,
                y = 0, yend = 0, col = "est",
                linetype = "est"),
              size = .8)
          } +
          
          { if (is_dti) sd_p4 } +
          
          ggplot2::geom_point(
            data = ~ head(.x, 1),
            mapping = ggplot2::aes(
              x = mean(ds1_sd$x), y = 0,
              col = "est", shape = "est"),
            size = 6) +
          
          { if (!is.null(rv$speedErr))
            ggplot2::geom_point(
              data = ~ head(.x, 1),
              ggplot2::aes(x = mean(rv$speedErr$est, na.rm = TRUE),
                           y = 0, col = "now", shape = "now"),
              size = 6, alpha = .7)
          } +
          
          ggplot2::scale_x_continuous(labels = scales::percent) +
          { if (!is.null(input$scale_density))
            if (input$scale_density)
              ggplot2::scale_y_continuous(breaks = seq(0, 1, .5))
          } +
          
          ggplot2::scale_color_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_col) +
          { if (!is.na(rv$sd_HDI$CI_low) && !is.na(rv$sd_HDI$CI_low))
            ggplot2::scale_fill_manual(
              name = "", labels = lbl, breaks = brk,
              values = val_fill) } +
          { if (!is.na(rv$sd_HDI$CI_low) && !is.na(rv$sd_HDI$CI_low))
            ggplot2::scale_linetype_manual(
              name = "", labels = lbl, breaks = brk,
              values = val_linetype) } +
          ggplot2::scale_shape_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_shape) +
          
          ggplot2::labs(x = "Estimate error (%)",
                        y = y_lab) +
          
          theme_movedesign(font_available = rv$is_font,
                           ft_size = 16) +
          ggplot2::theme(
            legend.position = "none",
            axis.title.x = ggplot2::element_blank())
        rv$report$ds1_sd[["done"]] <- TRUE
        
        ggiraph::girafe(
          ggobj = p,
          width_svg = 6, height_svg = 4,
          options = list(
            ggiraph::opts_zoom(max = 5),
            ggiraph::opts_hover(
              css = paste("r: 4pt;",
                          "fill: #006263;",
                          "stroke: #006263;")),
            ggiraph::opts_selection(
              type = "single",
              css = paste("r: 4pt;",
                          "fill: #004647;",
                          "stroke: #004647;")),
            ggiraph::opts_toolbar(saveaspng = FALSE)))
        
      }) # end of renderGirafe // repPlot_sd
      
    }) %>% # end of observe,
      bindEvent(list(input$build_report,
                     input$scale_density,
                     rv$highlight_dti))
    
    ## Rendering precision plot: ------------------------------------------
    
    output$repPlotLegend2 <- renderUI({
      req(rv$which_question, rv$which_meta, input$ci)
      req(rv$which_meta == "none")
      req(rv$tau_p[[1]], rv$tau_v[[1]], rv$dur, rv$dti)
      
      m <- ifelse(rv$which_meta == "none", 400, length(rv$simList))
      taup_unit <- ifelse(rv$which_meta == "none", 
                          "days", rv$tau_p[[1]]$unit[2])
      
      input_taup <- taup_unit %#% 
        rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      input_dur <- taup_unit %#% rv$dur$value %#% rv$dur$unit
      input_dti <- rv$dti$value %#% rv$dti$unit
      
      dt_hr <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      out_taup <- dt_hr$tau_p[which.min(abs(dt_hr$tau_p - input_taup))]
      
      if (rv$which_meta != "none") {
        taup_unit <- fix_unit(input_taup, taup_unit)$unit
        dur_for_hr <- paste0(round(input_dur, 1),
                             " ", taup_unit, ",")
      } else {
        dur_for_hr <- paste(
          round(dt_hr$dur[which.min(abs(dt_hr$dur - input_dur))], 1),
          " ", taup_unit, ",")
      }
      
      if (!is.null(rv$tau_v)) {
        input_tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
        
        dt_sd <- movedesign::sims_speed[[1]] %>%
          dplyr::mutate(dur = round("days" %#% dur, 1))
        out_tauv <- dt_sd$tau_v[which.min(abs(dt_sd$tau_v - input_tauv))]
        out_tauv <- fix_unit(out_tauv, "seconds", convert = TRUE)
        # TODO TOCHECK
      }
      
      dt_sd <- movedesign::sims_speed[[1]] %>%
        dplyr::mutate(dur = round(input_dur, 0)) %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      
      out_dti <- dt_sd$dti[which.min(abs(dt_sd$dti - input_dti))]
      dti_for_sd <- dt_sd$dti_notes[match(out_dti, dt_sd$dti)]
      dti_for_sd
      
      if (length(rv$which_question) > 1) {
        ui <- tagList(
          fontawesome::fa("circle-exclamation", fill = pal$dgr),
          span("Note:", class = "help-block-note"), 
          "This plot has the same information as above,",
          "but showing each expected error in a different line.",
          "If your simulations",
          wrap_none(
            "(", fontawesome::fa("circle", prefer_type = "solid"),
            " and ", fontawesome::fa("diamond"), " in darker colors)"),
          "do not show lines, then the credible intervals (CIs) were",
          "too large or the number of simulations",
          wrap_none("insufficient", class = "cl-dgr", end = "."),
          "Run more simulations to obtain valid CIs.")
        
      } else {
        
        switch(
          rv$which_question,
          "Home range" = {
            
            if (rv$which_meta == "none") {
              sim_details <- paste0(
                "movement processes with \u03C4\u209A = ", 
                out_taup, " day(s),")
            } else {
              sim_details <- paste0(
                "movement processes with \u03C4\u209A = ", 
                round(input_taup, 1), " ", taup_unit, ",")
            }
            
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "This plot shows the expected error",
              "based on", m, "simulations for",
              sim_details,
              "for a sampling duration of", 
              dur_for_hr, "with the medians",
              wrap_none(
                "(", fontawesome::fa("diamond", fill = pal$sea),
                " in ", span("light blue", class = "cl-sea"), "),"),
              "and the", wrap_none(input$ci, "%"),
              "credible intervals (lines in",
              wrap_none(span("light blue", class = "cl-sea"), ")."),
              "For comparison, your simulation(s)",
              "mean error (with CIs if applicable) is the circle",
              wrap_none(
                "(", fontawesome::fa("circle", prefer_type = "solid",
                                     fill = pal$sea_d), ")"),
              "in", wrap_none(span("darker blue",
                                   class = "cl-sea-d"), "."))
          },
          "Speed & distance" = {
            
            # if (rv$which_meta == "none") {
            #   sim_details <- paste0(
            #     "movement processes with \u03C4\u1D65 = ", 
            #     out_tauv$value, out_tauv$unit)
            # } else {
            #   sim_details <- paste0(
            #     "movement processes with \u03C4\u1D65 = ", 
            #     out_tauv$value, out_tauv$unit)
            # }
            
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "This plot shows the expected error",
              "based on", m, "simulations for",
              
              "movement processes with \u03C4\u1D65 = ", 
              out_tauv$value, out_tauv$unit,
              
              "for a sampling interval of", 
              wrap_none(dti_for_sd, ","), "with the medians",
              wrap_none(
                "(", fontawesome::fa("diamond", fill = pal$sea),
                " in ", span("light blue", class = "cl-sea"), "),"),
              "and the", wrap_none(input$ci, "%"),
              "credible intervals (lines in",
              wrap_none(span("light blue", class = "cl-sea"), ")."),
              "For comparison, your simulation(s)",
              "mean error (with CIs, if applicable) is the circle",
              wrap_none(
                "(", fontawesome::fa("circle", prefer_type = "solid",
                                     fill = pal$sea_d), ")"),
              "in", wrap_none(span("darker blue",
                                   class = "cl-sea-d"), "."))
          },
          stop(paste0("No handler for ",
                      rv$which_question, "."))
        )
      }
      
      ui <- span(class = "help-block", ui)
      
      return(ui)
      
    }) # end of renderUI, "repPlotLegend2"
    
    output$repPlot_precision <- ggiraph::renderGirafe({
      req(rv$hr_HDI, rv$sd_HDI)
      req(rv$which_meta == "none")
      
      is_both <- FALSE
      if (!is.null(rv$which_question))
        if (length(rv$which_question) > 1)
          is_both <- TRUE

      is_dur <- FALSE
      if (!is.null(rv$highlight_dur)) {
        if (!is.na(as.numeric(rv$highlight_dur)))
          is_dur <- TRUE
      }

      is_dti <- FALSE
      if (!is.null(rv$highlight_dti)) {
        if (rv$highlight_dti != "")
          is_dti <- TRUE
      }

      girafe_height <- 2
      details <- data.frame(
        # To plot:
        question = character(0),
        group = character(0),
        type = character(0),
        value = numeric(0),
        lci = numeric(0),
        uci = numeric(0),
        # For scale_manual:
        label = character(0),
        fill = character(0),
        col = character(0),
        linetype = character(0),
        shape = numeric(0))
      
      details_length <- 0
      xmin <- xmax <- NA

      if (is_dur && !is_both) {
        req(rv$report$ds2_hr, rv$hr_HDI_new)

        details <- details %>%
          dplyr::add_row(
            question = "Home range",
            group = "est_new",
            type = "hr_est_new",
            value = rv$report$ds2_hr[["median"]],
            lci = rv$hr_HDI_new$CI_low,
            uci = rv$hr_HDI_new$CI_high,
            label = paste0("AKDE error for ",
                           rv$highlight_dur, " days"),
            fill = pal$mdn,
            col = pal$mdn,
            linetype = "solid",
            shape = 18)

        girafe_height <- 2.5
        details_length <- details_length + 1
        xmin <- min(xmin, rv$report$ds2_hr[["min"]])
        xmax <- max(xmax, rv$report$ds2_hr[["max"]])
      }

      if (is_dti && !is_both) {
        req(rv$report$ds2_sd, rv$sd_HDI_new)

        details <- details %>%
          dplyr::add_row(
            question = "Speed & distance",
            group = "est_new",
            type = "sd_est_new",
            value = rv$report$ds2_sd[["median"]],
            lci = rv$sd_HDI_new$CI_low,
            uci = rv$sd_HDI_new$CI_high,
            label = paste0("CTSD error for ",
                           rv$report$txt_dti_new),
            fill = pal$mdn,
            col = pal$mdn,
            linetype = "solid",
            shape = 18)

        girafe_height <- 2.5
        details_length <- details_length + 1
        xmin <- min(xmin, rv$report$ds2_sd[["min"]])
        xmax <- max(xmax, rv$report$ds2_sd[["max"]])
      }

      if ("Home range" %in% rv$which_question) {
        req(rv$report$ds1_hr[["done"]])

        details <- details %>%
          dplyr::add_row(
            question = "Home range",
            group = "est",
            type = "hr_est",
            value = rv$report$ds1_hr[["median"]],
            lci = rv$hr_HDI$CI_low,
            uci = rv$hr_HDI$CI_high,
            label = paste0("AKDE error for ",
                           rv$report$dur_for_hr),
            fill = pal$sea,
            col = pal$sea,
            linetype = "solid",
            shape = 18)
        
        if (!is.null(rv$simList)) {
          ci <- ifelse(is.null(input$ci), .95, input$ci/100)
          err <- suppressWarnings(
            bayestestR::ci(rv$hrErr$est,
                           ci = ci, method = "HDI"))
          err <- data.frame(
            lci = ifelse(is.null(err$CI_low), NA, err$CI_low),
            mean = mean(rv$hrErr$est, na.rm = TRUE),
            uci = ifelse(is.null(err$CI_high), NA, err$CI_high))
        }

        input_dur <- "days" %#% rv$dur$value %#% rv$dur$unit
        details <- details %>%
          dplyr::add_row(
            question = "Home range",
            group = "now",
            type = "hr_now",
            value = ifelse(!is.null(rv$hrErr), err[[2]], NA),
            lci = ifelse(!is.null(rv$hrErr), err[[1]], NA),
            uci = ifelse(!is.null(rv$hrErr), err[[3]], NA),
            label = paste0("AKDE error for ",
                           round(input_dur, 1), " days"),
            fill = pal$sea_d,
            col = pal$sea_d,
            linetype = "dashed",
            shape = 19)

        details_length <- details_length + 2
        xmin <- rv$report$ds1_hr[["min"]]
        xmax <- rv$report$ds1_hr[["max"]]

      } # end of hr

      if ("Speed & distance" %in% rv$which_question) {
        req(rv$report$ds1_sd[["done"]])

        details <- details %>%
          dplyr::add_row(
            question = "Speed & distance",
            group = "est",
            type = "sd_est",
            value = rv$report$ds1_sd[["median"]],
            lci = rv$sd_HDI$CI_low,
            uci = rv$sd_HDI$CI_high,
            label = paste0("CTSD error for ",
                           rv$report$txt_dti),
            fill = ifelse(is_both, pal$grn, pal$sea),
            col = ifelse(is_both, pal$grn, pal$sea),
            linetype = "solid",
            shape = 18)
        
        if (!is.null(rv$simList)) {
          ci <- ifelse(is.null(input$ci), .95, input$ci/100)
          err <- suppressWarnings(
            bayestestR::ci(rv$speedErr$est,
                           ci = ci, method = "HDI"))
          err <- data.frame(
            lci = ifelse(is.null(err$CI_low), NA, err$CI_low),
            mean = mean(rv$speedErr$est, na.rm = TRUE),
            uci =  ifelse(is.null(err$CI_high), NA, err$CI_high))
        }
        
        input_dur <- "days" %#% rv$dur$value %#% rv$dur$unit
        input_dti <- rv$dti$value %#% rv$dti$unit
        sd_opts <- movedesign::fixrates %>%
          dplyr::select(.data$dti, .data$dti_notes) %>%
          unique()

        out_dti <- sd_opts$dti[which.min(abs(sd_opts$dti - input_dti))]
        input_dti <- sd_opts$dti_notes[match(out_dti, sd_opts$dti)]
        input_dti

        details <- details %>%
          dplyr::add_row(
            question = "Speed & distance",
            group = "now",
            type = "sd_now",
            value = ifelse(!is.null(rv$speedErr), err[[2]], NA),
            lci = ifelse(!is.null(rv$speedErr), err[[1]], NA),
            uci = ifelse(!is.null(rv$speedErr), err[[3]], NA),
            label = paste0("CTSD error for ", input_dti),
            fill = ifelse(is_both, pal$grn_d, pal$sea_d),
            col = ifelse(is_both, pal$grn_d, pal$sea_d),
            linetype = "dashed",
            shape = 19)

        details_length <- details_length + 2
        xmin <- min(xmin, rv$report$ds1_sd[["min"]])
        xmax <- max(xmax, rv$report$ds1_sd[["max"]])

      } # end of sd

      details <- details %>%
        dplyr::filter(!is.na(.data$value)) %>%
        dplyr::mutate(group = factor(group,
                                     levels = c("est_new",
                                                "est",
                                                "now"))) %>%
        droplevels()

      if (is_both) {
        add_val <- ifelse(is.null(rv$speedErr), 1, 0)
        details_length <- 2 + add_val
        girafe_height <- 3
      }
      
      override_size <- rep(4, details_length)
      override_stroke <- rep(1, details_length)
      y_labels <- rep("___", details_length)

      p <- ggplot2::ggplot() +
        ggplot2::geom_vline(xintercept = 0) +

        ggplot2::geom_point(
          data = details,
          mapping = ggplot2::aes(
            x = .data$value,
            y = .data$type,
            group = .data$question,
            col = .data$type,
            fill = .data$type,
            shape = .data$type),
          size = 5) +

        ggplot2::geom_segment(
          data = details,
          mapping = ggplot2::aes(
            x = .data$lci,
            y = .data$type,
            group = .data$question,
            col = .data$type,
            xend = .data$uci,
            yend = .data$type),
          linewidth = .8) +

        { if (is_both) {
          ggplot2::scale_x_continuous(
            labels = scales::percent)
        } else {
          ggplot2::scale_x_continuous(
            labels = scales::percent,
            limits = c(xmin, xmax)) }
        } +
        ggplot2::scale_y_discrete(
          labels = y_labels) +

        ggplot2::scale_color_manual(
          name = "", labels = details$label,
          breaks = details$type,
          values = details$col) +
        ggplot2::scale_fill_manual(
          name = "", labels = details$label,
          breaks = details$type,
          values = details$fill) +
        ggplot2::scale_shape_manual(
          name = "", labels = details$label,
          breaks = details$type,
          values = details$shape) +
        ggplot2::scale_linetype_manual(
          name = "", labels = details$label,
          breaks = details$type,
          values = details$linetype) +

        ggplot2::labs(x = "Estimate error (%)", y = "") +

        theme_movedesign(font_available = rv$is_font) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(color = "#ffffff"),
          legend.position = "bottom",
          legend.direction = "vertical",
          legend.title = ggplot2::element_blank()) +
        ggplot2::guides(
          shape = ggplot2::guide_legend(
            override.aes = list(
              alpha = 1,
              size = override_size,
              stroke = override_stroke)))

      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = girafe_height,
        options = list(
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("r: 4pt;",
                        "fill: #006263;",
                        "stroke: #006263;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("r: 4pt;",
                        "fill: #004647;",
                        "stroke: #004647;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))

    }) # end of renderGirafe // repPlot_precision
    
    ## Rendering simulations + quick comparison plots: --------------------
    
    output$repPlotLegend3 <- renderUI({
      req(rv$which_question, 
          input$ci, rv$tau_p[[1]], rv$tau_v[[1]], rv$dur, rv$dti)
      req(length(rv$which_question) == 1)
      
      input_taup <- "days" %#%
        rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      input_dur <- "days" %#% rv$dur$value %#% rv$dur$unit
      input_dti <- rv$dti$value %#% rv$dti$unit
      
      dat <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      out_taup <- dat$tau_p[which.min(abs(dat$tau_p - input_taup))]
      
      switch(
        rv$which_question,
        "Home range" = {
          
          max_dur <- max(dat$duration)
          unit <- ifelse(out_taup == 1, "day", "days")
          
          ui <- tagList(
            fontawesome::fa("triangle-exclamation", fill = pal$dgr),
            span("Warning:", class = "help-block-note"), 
            "This plot shows only the mean expected errors and",
            "(confidence intervals) simulated for",
            
            "movement processes with \u03C4\u209A = ", 
            span(out_taup, unit, class = "cl-dgr"),
            
            "and for different sampling",
            "durations (from 1 day to", wrap_none(max_dur, " days)"),
            "in grey.",
            "As sampling duration increases, we will expect lower",
            "estimate errors (points) and",
            "lower uncertainty (shaded area).", br(),
            "These are based on aggregated information from",
            "pre-run simulations, so values may not match.",
            "Evaluate with",
            wrap_none("caution", css = "cl-dgr", "."))
          
        },
        "Speed & distance" = {
          
          dt_sd <- movedesign::sims_speed[[1]] %>%
            dplyr::mutate(duration = round("days" %#% dur, 1)) %>%
            dplyr::select(.data$duration) %>%
            unique()
          max_dur <- max(dt_sd$duration)
          
          ui <- tagList(
            fontawesome::fa("circle-exclamation", fill = pal$dgr),
            span("Note:", class = "help-block-note"), 
            "This plot shows only the mean expected errors and",
            "(confidence intervals) simulated",
            
            # TODO add out_tauv
            
            "for different sampling",
            "durations (from 1 day to", wrap_none(max_dur, " days)"),
            "in grey.",
            "As sampling duration increases, we will expect lower",
            "estimate errors (points) and",
            "lower uncertainty (shaded area).", br(),
            "These are based on aggregated information from",
            "pre-run simulations, so values may not match.",
            "Evaluate with",
            wrap_none("caution", css = "cl-dgr", "."))
          
        },
        stop(paste0("No handler for ",
                    rv$which_question, "."))
      )
      
      ui <- span(class = "help-block", ui)
      
      return(ui)
      
    }) # end of renderUI, "repPlotLegend3"
    
    output$repPlot_comp_hr <- ggiraph::renderGirafe({
      req(rv$which_meta, rv$hrErr)
      
      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")
      
      input_taup <- "days" %#% 
        rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      input_dur <- "days" %#% rv$dur$value %#% rv$dur$unit
      
      dat <- movedesign::sims_hrange[[2]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      index_taup <- which.min(abs(dat$tau_p - input_taup))
      filtering <- dat$tau_p[index_taup]
      newdat_filtered <- dat_filtered <- dat %>% 
        dplyr::filter(tau_p == filtering)
      index_dur <- which.min(abs(dat_filtered$duration - input_dur))
      
      if (rv$which_meta != "none") {
        req(rv$hr$tbl)
        newdat_filtered <- data.frame(
          tau_p = input_taup,
          duration = input_dur,
          error = mean(rv$hr$tbl$area_err, na.rm = TRUE),
          error_lci = mean(rv$hr$tbl$area_err_min, na.rm = TRUE),
          error_uci = mean(rv$hr$tbl$area_err_max, na.rm = TRUE))
        index_dur <- 1
      }
      
      dat$id <- 1:nrow(dat)
      
      if (rv$highlight_dur > 0) {
        dur_NEW <- as.numeric(rv$highlight_dur)
        is_highlight <- TRUE
      } else {
        is_highlight <- NULL
      }
      
      pd <- ggplot2::position_dodge(width = 0.6)
      
      if (rv$highlight_dur > 0) {
        
        newdat <- dat_filtered %>%
          dplyr::filter(duration == dur_NEW)
        y_start <- newdat %>% dplyr::pull(error_lci)
        y_end <- newdat %>% dplyr::pull(error_uci)
        
        p1 <- ggplot2::geom_segment(
          ggplot2::aes(x = dur_NEW,
                       xend = dur_NEW,
                       y = y_start,
                       yend = y_end),
          col = pal$mdn,
          linetype = "solid",
          size = 1.5, alpha = .8)
        
        p2 <- ggiraph::geom_point_interactive(
          data = newdat,
          mapping = ggplot2::aes_string(
            x = "duration",
            y = "error"),
          size = 3, col = pal$mdn)
      }
      
      p <- ggplot2::ggplot() +
        
        ggplot2::geom_ribbon(
          data = dat_filtered,
          mapping = ggplot2::aes_string(
            x = "duration",
            y = "error",
            ymin = "error_lci",
            ymax = "error_uci"),
          col = NA, fill = "grey90",
          alpha = .5) +
        
        ggplot2::geom_line(
          data = dat_filtered,
          mapping = ggplot2::aes_string(x = "duration",
                                        y = "error",
                                        group = "tau_p"),
          col = "grey20", linetype = "dotted",
          size = 0.5) +
        
        ggplot2::geom_point(
          data = dat_filtered,
          mapping = ggplot2::aes_string(x = "duration",
                                        y = "error",
                                        group = "tau_p"),
          size = 2.5, shape = 18, col = "grey40") +
        
        ggplot2::geom_segment(
            data = newdat_filtered[index_dur,],
            ggplot2::aes_string(x = "duration",
                                xend = "duration",
                                y = "error_lci",
                                yend = "error_uci"),
            col = pal$sea,
            linetype = "solid",
            size = 1.5, alpha = .8) +
        
        ggplot2::geom_point(
          data = newdat_filtered[index_dur,],
          mapping = ggplot2::aes_string(x = "duration",
                                        y = "error",
                                        group = "tau_p"),
          col = pal$sea,
          position = pd, size = 5) +
        
        ggplot2::geom_hline(yintercept = 0,
                            linetype = "solid", size = .5) +
        
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(x = "Sampling duration (in days)",
                      y = "Estimate error (%)") +
        
        { if (rv$highlight_dur > 0) p1 } +
        { if (rv$highlight_dur > 0) p2 } +
        
        theme_movedesign(font_available = rv$is_font,
                         ft_size = rv$ft_size) +
        ggplot2::theme(legend.position = "none")
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 4.5,
        options = list(
          ggiraph::opts_tooltip(css = tooltip_css),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("r: 4pt;",
                        "fill: #006263;",
                        "stroke: #006263;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("r: 4pt;",
                        "fill: #004647;",
                        "stroke: #004647;"))))
      
    }) # end of renderGirafe // repPlot_comp_hr
    
    output$repPlot_comp_sd <- ggiraph::renderGirafe({
      req(rv$speedErr)
      
      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")
      
      input_tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
      input_dur <- "days" %#% rv$dur$value %#% rv$dur$unit
      input_dti <- rv$dti$value %#% rv$dti$unit
      
      reveal_if <- FALSE
      if (!is.null(rv$highlight_dti)) {
        if (rv$highlight_dti != "") reveal_if <- TRUE
      }
      
      sims <- movedesign::sims_speed[[2]]
      dat <- sims %>%
        dplyr::mutate(dur = round("days" %#% dur, 0))
      dat$id <- 1:nrow(dat)
      
      opts <- sims %>%
        dplyr::select(dti, dti_notes) %>%
        unique()
      
      if (reveal_if) {
        dti_new <- opts$dti[match(rv$highlight_dti,
                                  opts$dti_notes)]
        is_highlight <- TRUE
      } else {
        is_highlight <- NULL
      }
      
      index_dur <- which.min(abs(dat$dur - input_dur))
      filtering_dur <- dat$dur[index_dur]
      
      index_tauv <- which.min(abs(dat$tau_v - input_tauv))
      filtering_tauv <- dat$tau_v[index_tauv]
      
      index_dti <- which.min(abs(dat$dti - input_dti))
      filtering_dti <- dat$dti[index_dti]
      
      dat_filtered <- dat %>%
        dplyr::filter(tau_v == filtering_tauv) %>%
        dplyr::filter(dti == filtering_dti) %>%
        stats::na.omit()
      
      pd <- ggplot2::position_dodge(width = 0.6)
      
      if (reveal_if) {
        newdat <- dat %>%
          dplyr::filter(tau_v == filtering_tauv) %>%
          dplyr::filter(dti == dti_new) %>%
          stats::na.omit()
      }
      
      p <- ggplot2::ggplot(
        data = dat_filtered,
        mapping = ggplot2::aes(
          x = .data$dur,
          y = .data$error,
          group = as.factor(.data$dti),
          ymin = .data$error - .data$ci,
          ymax = .data$error + .data$ci)) +
        
        ggplot2::geom_hline(yintercept = 0,
                            linetype = "solid", size = .5) +
        
        ggplot2::geom_ribbon(
          fill = pal$sea,
          alpha = .2) +
        
        ggplot2::geom_line(
          col = pal$sea, linetype = "dotted",
          size = 0.5) +
        
        ggplot2::geom_point(
          size = 2.5, shape = 18, col = pal$sea) +
        
        { if (reveal_if)
          ggplot2::geom_ribbon(
            data = newdat,
            ggplot2::aes(x = .data$dur,
                         y = .data$error,
                         group = as.factor(.data$dti),
                         ymin = .data$error - .data$ci,
                         ymax = .data$error + .data$ci),
            alpha = .1, fill = pal$mdn,
            position = ggplot2::position_dodge(width = 0.3)) } +
        
        { if (reveal_if)
          ggplot2::geom_line(
            data = newdat,
            ggplot2::aes(x = .data$dur,
                         y = .data$error,
                         group = as.factor(.data$dti)),
            size = .5, alpha = .8,
            linetype = "solid", col = pal$mdn,
            position = ggplot2::position_dodge(
              width = 0.3)) } +
        
        { if (reveal_if)
          ggiraph::geom_point_interactive(
            data = newdat,
            ggplot2::aes(x = .data$dur,
                         y = .data$error,
                         group = as.factor(.data$dti)),
            size = 3, col = pal$mdn) } +
        
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(x = "Sampling duration (in days)",
                      y = "Estimate error (%)") +
        theme_movedesign(font_available = rv$is_font) +
        ggplot2::theme(legend.position = "none")
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 4.5,
        options = list(
          ggiraph::opts_tooltip(css = tooltip_css),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("r: 4pt;",
                        "fill: #006263;",
                        "stroke: #006263;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("r: 4pt;",
                        "fill: #004647;",
                        "stroke: #004647;"))))
      
    }) # end of renderGirafe // repPlot_comp_sd
    
    output$reportPlots_error <- renderUI({
      req(rv$which_question)
      
      if ("Home range" %in% rv$which_question) {
        out <- out_hr <- ggiraph::girafeOutput(
          outputId = ns("repPlot_comp_hr"),
          width = "100%", height = "100%")
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        out <- out_sd <- ggiraph::girafeOutput(
          outputId = ns("repPlot_comp_sd"),
          width = "100%", height = "100%")
      }
      
      # if (length(rv$which_question) > 1) {
      #   out <- tagList(out_hr, out_sd)
      # }
      
      return(out)
      
    }) # end of renderUI, "reportPlots_error"
    
    # TABLES --------------------------------------------------------------
    ## Final report table (combining previous results): -------------------
    
    reportRow <- reactive({
      n_sims <- length(rv$simList)
      
      dt_regs <- rv$dev$tbl
      dt_regs <- dt_regs %>% dplyr::slice_tail(n = n_sims)
      
      if ("Home range" %in% rv$which_question) {
        req(rv$hr$tbl)
        dt_hr <- rv$hr$tbl
        dt_hr <- dt_hr %>% dplyr::filter(data == "Initial") 
        dt_hr <- dt_hr %>% dplyr::slice_tail(n = n_sims)
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        req(rv$sd$tbl)
        dt_sd <- rv$sd$tbl
        dt_sd <- dt_sd %>% dplyr::filter(data == "Initial") 
        dt_sd <- dt_sd %>% dplyr::slice_tail(n = n_sims)
      }
      
      dat <- data.frame(
        seed = numeric(0),
        device = character(0),
        taup = character(0),
        tauv = character(0),
        sigma = character(0),
        dur = character(0),
        dti = character(0),
        n = numeric(0),
        N1 = numeric(0),
        N2 = numeric(0))
      
      tmpdat <- dt_regs %>% dplyr::select(.data$seed:.data$N2)
      tmpdat <- suppressMessages(dplyr::full_join(dat, tmpdat))
      
      tmpdat$taup <- paste(
        scales::label_comma(accuracy = .1)(rv$tau_p[[1]]$value[2]),
        abbrv_unit(rv$tau_p[[1]]$unit[2]))
      
      if (!is.null(rv$tau_v[[1]])) {
        tmpdat$tauv <- paste(
          scales::label_comma(accuracy = .1)(rv$tau_v[[1]]$value[2]),
          abbrv_unit(rv$tau_v[[1]]$unit[2]))
      }
      
      out <- fix_unit(rv$sigma[[1]]$value[2],
                      rv$sigma[[1]]$unit[2], convert = TRUE)
      tmpdat$sigma <- paste(out$value, abbrv_unit(out$unit))
      
      if ("Home range" %in% rv$which_question) {
        tmphr <- dt_hr %>% dplyr::select(
          c(.data$seed, .data$taup:.data$area_err_max))
        tmpdat <- suppressMessages(
          tmpdat %>% dplyr::group_by(seed) %>% 
            dplyr::full_join(tmphr))
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        tmpsd <- dt_sd %>% dplyr::select(
          c(.data$seed, .data$tauv:.data$dist_err))
        tmpdat <- suppressMessages(
          tmpdat %>% dplyr::group_by(seed) %>% 
            dplyr::full_join(tmpsd))
      }
      
      # if (length(rv$simList) == 1) {
      #   if (nrow(tmpdat == 2))
      #     tmpdat <- dplyr::coalesce(tmpdat[1,], tmpdat[2,])
      # }
      
      tmpdat <- tmpdat %>%
        dplyr::group_by(seed) %>%
        dplyr::summarize(dplyr::across(dplyr::everything(), 
                         ~ifelse(all(is.na(.)), NA,
                                 .[!is.na(.)][1])))
      
      return(tmpdat)
      
    }) # end of reactive
    
    observe({
      req(rv$active_tab == 'report',
          rv$which_question,
          rv$dev$tbl,
          rv$simList,
          !rv$is_report)
      
      rv$report$tbl <- reportRow()
      dat <- rv$report$tbl
      
      if (length(rv$simList) == 1) {
        if (nrow(dat) >= 2) {
          if (all(dat[nrow(dat) - 1,3:7] == dat[nrow(dat), 3:7]))
            dat <- dplyr::coalesce(dat[1, ], dat[2, ])
        }
      }
      
      rv$report$tbl <- dplyr::distinct(dat)
      rv$is_report <- TRUE
      set.seed(NULL)
      
    }) # end of observe
    
    output$endTable <- reactable::renderReactable({
      req(rv$report$tbl,
          rv$is_analyses, 
          rv$which_question,
          rv$is_report)
      
      if (length(rv$which_question) == 2)
        req(rv$hr_completed, rv$sd_completed)
      
      choices <- choices_subset <- c(
        "device",
        "taup",
        "tauv",
        "sigma",
        "dur",
        "dti",
        "n",
        "N1",
        "N2",
        "area",
        "area_err",
        "area_err_min",
        "area_err_max",
        "ctsd",
        "ctsd_err",
        "ctsd_err_min",
        "ctsd_err_max",
        "dist",
        "dist_err")
      
      if (length(rv$which_question) == 1) {
        if (rv$which_question == "Home range")
          choices_subset <- choices[c(1:8, 10:13)]
        
        if (rv$which_question == "Speed & distance")
          choices_subset <- choices[c(1:7, 9, 14:19)]
      }
      
      nms <- data.frame(
        device = "Type",
        taup = "\u03C4\u209A",
        tauv = "\u03C4\u1D65",
        sigma = "\u03C3\u209A",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = "N (area)",
        N2 = "N (speed)",
        area = "HR area",
        area_err = "Error",
        area_err_min = "Error (95% LCI)",
        area_err_max = "Error (95% UCI)",
        ctsd = "CTSD",
        ctsd_err = "Error",
        ctsd_err_min = "Error (95% LCI)",
        ctsd_err_max = "Error (95% UCI)",
        dist = "Distance",
        dist_err = "Error")
      
      dat <- rv$report$tbl[, -1]
      
      if (!is.null(choices_subset)) {
        dat <- dat %>% dplyr::select(choices_subset)
      }
      
      if ("Home range" %in% rv$which_question) {
        nms_sizes <- reactable::colGroup(
          name = "Sample sizes", 
          columns = c("n", "N1"))
        nms_hr <- reactable::colGroup(
          name = "Home range",
          columns = c("area",
                      "area_err",
                      "area_err_min",
                      "area_err_max"))
        
        colgroups <- list(nms_sizes,
                          nms_hr)
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        nms_sizes <- reactable::colGroup(
          name = "Sample sizes", 
          columns = c("n", "N2"))
        nms_ctsd <- reactable::colGroup(
          name = "Speed",
          columns = c("ctsd",
                      "ctsd_err",
                      "ctsd_err_min",
                      "ctsd_err_max"))
        nms_dist <- reactable::colGroup(
          name = "Distance",
          columns = c("dist", "dist_err"))
        
        colgroups <- list(nms_sizes,
                          nms_ctsd,
                          nms_dist)
      }
      
      if (length(rv$which_question) == 2) {
        nms_sizes <- reactable::colGroup(
          name = "Sample sizes", 
          columns = c("n", "N1", "N2"))
        
        colgroups <- list(nms_sizes,
                          nms_hr,
                          nms_ctsd,
                          nms_dist)
      }
      
      namedcolumns <- list(
        device = if ("device" %in% choices_subset) {
          reactable::colDef(
            name = nms[1, "device"]) },
        taup = if ("taup" %in% choices_subset) {
          reactable::colDef(
            minWidth = 100, name = nms[1, "taup"],
            style = list(fontWeight = "bold")) },
        tauv = if ("tauv" %in% choices_subset) {
          reactable::colDef(
            minWidth = 100, name = nms[1, "tauv"],
            style = list(fontWeight = "bold")) },
        sigma = if ("sigma" %in% choices_subset) {
          reactable::colDef(
            minWidth = 100, name = nms[1, "sigma"]) },
        dur = if ("dur" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "dur"],
            style = list(fontWeight = "bold")) },
        dti = if ("dti" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "dti"],
            style = list(fontWeight = "bold")) },
        n = if ("n" %in% choices_subset) {
          reactable::colDef(
            name = nms[1, "n"],
            style = format_num,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 0)) },
        N1 = if ("N1" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "N1"],
            style = format_num,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)) },
        N2 = if ("N2" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "N2"],
            style = format_num,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)) },
        area = if ("area" %in% choices_subset) {
          reactable::colDef(
            minWidth = 100, name = nms[1, "area"]) },
        area_err = if ("area_err" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err"],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        area_err_min = if ("area_err_min" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err_min"],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        area_err_max = if ("area_err_max" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err_max"],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        ctsd = if ("ctsd" %in% choices_subset) {
          reactable::colDef(
            minWidth = 100, name = nms[1, "ctsd"]) },
        ctsd_err = if ("ctsd_err" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd_err"],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        ctsd_err_min = if ("ctsd_err_min" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd_err_min"],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        ctsd_err_max = if ("ctsd_err_max" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd_err_max"],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        dist = if ("dist" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 100, name = nms[1, "dist"]) },
        dist_err = if ("dist_err" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "dist_err"],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) }
      )
      
      namedcolumns[sapply(namedcolumns, is.null)] <- NULL
      
      dt <- reactable::reactable(
        dat,
        compact = TRUE,
        highlight = TRUE,
        striped = TRUE,
        
        defaultPageSize = 5,
        paginationType = "jump",
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 20),
        showPageInfo = FALSE,
        
        defaultColDef =
          reactable::colDef(
            headerClass = "rtable_header",
            align = "center",
            minWidth = 50),
        
        columns = namedcolumns,
        columnGroups = colgroups
        
      ) # end of reactable
      
      return(dt)
      
    }) # end of renderReactable // endTable
    
    
    # BLOCKS --------------------------------------------------------------
    ## Timescale parameters: ----------------------------------------------
    
    observe({
      req(rv$tau_p[[1]])
      
      mod_blocks_server(
        id = "repBlock_taup", 
        rv = rv, type = "tau", name = "tau_p",
        input_name = list(
          chr = "data_taup0",
          html = wrap_none("Position autocorrelation ",
                           "(\u03C4", tags$sub("p"), ")")))
      
    }) # end of observe
    
    observe({
      req(rv$tau_v[[1]])
      
      mod_blocks_server(
        id = "repBlock_tauv",
        rv = rv, type = "tau", name = "tau_v",
        input_name = list(
          chr = "data_tauv0",
          html = wrap_none("Velocity autocorrelation ",
                           "(\u03C4", tags$sub("v"), ")")))
      
    }) # end of observe
    
    ## Location variance: -------------------------------------------------
    
    observe({
      req(rv$sigma[[1]])
      
      mod_blocks_server(
        id = "repBlock_sigma",
        rv = rv, type = "sigma", name = "sigma",
        input_name = list(
          chr = "data_sigma0",
          html = wrap_none("Location variance ",
                           "(\u03C3", tags$sub("p"), ")")))
      
    }) # end of observe
    
    ## Tracking regime: ---------------------------------------------------
    
    observe({
      req(rv$active_tab == 'report')
      req(rv$datList, rv$id)
      
      mod_blocks_server(
        id = "repBlock_dur",
        rv = rv, data = rv$simList,
        type = "dur")
      
      mod_blocks_server(
        id = "repBlock_dti", 
        rv = rv, data = rv$simList,
        type = "dti")
      
    }) # end of observe
    
    ## Sample sizes: ------------------------------------------------------
    
    output$repUI_sizes <- renderUI({
      req(rv$dev$is_valid)
      
      if (is.null(rv$which_question) ||
          length(rv$which_question) > 1) {
        out <- tagList(
          mod_blocks_ui(ns("repBlock_n")),
          splitLayout(
            mod_blocks_ui(ns("repBlock_Narea")),
            mod_blocks_ui(ns("repBlock_Nspeed"))
          ))
      }
      
      if (length(rv$which_question) == 1 &&
          "Home range" %in% rv$which_question) {
        out <- splitLayout(
          mod_blocks_ui(ns("repBlock_n")),
          mod_blocks_ui(ns("repBlock_Narea"))) }
      
      if (length(rv$which_question) == 1 &&
          "Speed & distance" %in% rv$which_question) {
        out <- splitLayout(
          mod_blocks_ui(ns("repBlock_n")),
          mod_blocks_ui(ns("repBlock_Nspeed"))) }
      
      return(out)
    }) # end of renderUI, "repUI_sizes"
    
    observe({
      req(rv$active_tab == 'report', rv$simList)
      
      mod_blocks_server(
        id = "repBlock_n", 
        rv = rv, data = rv$simList, type = "n",
        options = list(rightBorder = FALSE,
                       marginBottom = TRUE))
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'report', rv$simList, rv$simfitList)
      
      mod_blocks_server(
        id = "repBlock_Narea", 
        rv = rv, data = rv$simList, fit = rv$simfitList,
        type = "N", name = "area")
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'report', rv$simList, rv$simfitList)
      
      mod_blocks_server(
        id = "repBlock_Nspeed", 
        rv = rv, data = rv$simList, fit = rv$simfitList,
        type = "N", name = "speed")
      
    }) # end of observe
    
    ## Outputs: -----------------------------------------------------------
    
    observe({
      req(rv$simList, rv$hrErr)
      req(nrow(rv$hrErr) == length(rv$simList))
      
      if ("Home range" %in% rv$which_question)
        shinyjs::show(id = "repBox_hr_err") else
          shinyjs::hide(id = "repBox_hr_err")
      
      mod_blocks_server(
        id = "repBlock_hrErr",
        rv = rv, type = "hr", name = "hrErr")
      
    }) # end of observe
    
    observe({
      req(rv$simList, rv$speedErr)
      req(nrow(rv$speedErr) == length(rv$simList))
      
      if ("Speed & distance" %in% rv$which_question)
        shinyjs::show(id = "repBox_speed_err") else
          shinyjs::hide(id = "repBox_speed_err")
      
      mod_blocks_server(
        id = "repBlock_speedErr",
        rv = rv, type = "ctsd", name = "speedErr")
      
    }) # end of observe
    
    # observe({
    #   req(rv$ctsdList, rv$distErr)
    #   req(nrow(rv$distErr) == length(rv$simList))
    #   
    #   if ("Speed & distance" %in% rv$which_question)
    #     shinyjs::show(id = "repBox_dist_err") else
    #       shinyjs::hide(id = "repBox_dist_err")
    #   
    #   mod_blocks_server(
    #     id = "repBlock_distErr",
    #     rv = rv, type = "dist", name = "distErr")
    #   
    # }) # end of observe
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_report_ui("tab_report_1")

## To be copied in the server
# mod_tab_report_server("tab_report_1")
