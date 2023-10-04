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
                             uiOutput(ns("repBlock_dur")),
                             uiOutput(ns("repBlock_dti")))
                           
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
                    uiOutput(ns("end_comparison"))
                    
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
mod_tab_report_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    vals$report <- reactiveValues()
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at start: --------------------------------------------
    
    shinyjs::hide(id = "end_comparison")
    shinyjs::hide(id = "repBox_hr_err")
    shinyjs::hide(id = "repBox_speed_err")
    shinyjs::hide(id = "repBox_dist_err")
    
    boxnames <- c("analyses", "tables")
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("repBox_", boxnames[i])) 
    }
    
    ## Rendering research questions: --------------------------------------
    
    output$report_question <- renderUI({

      ui_hr <- staticBlock("Home range", active = FALSE)
      ui_sd <- staticBlock("Speed & distance", active = FALSE)
      
      out_q <- tagList(ui_hr, ui_sd)
      
      if (!is.null(vals$which_question)) {
        if ("Home range" %in% vals$which_question) {
          ui_hr <- staticBlock("Home range", active = TRUE)
        }
        
        if ("Speed & distance" %in% vals$which_question) {
          ui_sd <- staticBlock("Speed & distance", active = TRUE)
        }
        out_q <- tagList(ui_hr, ui_sd)
      }
      
      return(out_q)
      
    }) # end of renderUI, "report_question"
    
    ## Rendering device limitations: --------------------------------------
    
    output$report_device <- renderUI({
      req(vals$which_limits)
      
      if ("loss" %in% vals$which_limits) {
        ui_loss <- staticBlock(paste0(vals$lost$perc, "%"), active = TRUE)
      } else if (!("loss" %in% vals$which_limits)) {
        ui_loss <- staticBlock("No data loss", active = FALSE)
      }
      
      if ("error" %in% vals$which_limits) {
        ui_error <- staticBlock(paste(vals$error, "meters"), active = TRUE)
      } else if (!("error" %in% vals$which_limits)) {
        ui_error <- staticBlock("No error", active = FALSE)
      }
      
      if ("limit" %in% vals$which_limits) {
        ui_limit <- staticBlock(paste(vals$storage, "locations"),
                                type = "max", active = TRUE)
      } else if (!("limit" %in% vals$which_limits)) {
        ui_limit <- staticBlock("No limit", active = FALSE)
      }
      
      out <- tagList(
        br(),
        span("Data loss:", class = "txt-label"), ui_loss,
        span("Location error:", class = "txt-label"), ui_error,
        span("Storage limit:", class = "txt-label"), ui_limit)
      
      return(out)
      
    }) # end of renderUI, "report_question"
    
    ## Rendering regime comparison inputs: --------------------------------
    
    output$highlighting_reg <- renderUI({
      req(vals$which_question)
      
      if ("Home range" %in% vals$which_question) {
        
        out <- out_hr <- shinyWidgets::pickerInput(
          inputId = ns("highlight_dur"),
          label = span("Sampling duration (in days):",
                       class = "txt-label"),
          choices = 2^seq(1, 12, by = 1),
          options = list(title = "(select here)"),
          width = "200px")
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        
        dat <- movedesign::sims_speed[[2]]
        out <- out_sd <- shinyWidgets::pickerInput(
          inputId = ns("highlight_dti"),
          label = span("Sampling interval:",
                       class = "txt-label"),
          choices = dat$dti_notes %>% unique,
          options = list(title = "(select here)"),
          width = "200px")
      }
      
      if (length(vals$which_question) > 1) {
        out <- tagList(out_hr, out_sd)
      }
      
      return(out)
      
    }) # end of renderUI, "highlighting_reg"
    
    ## Rendering total number of simulations: -----------------------------
    
    output$rep_nsims <- renderText({
      req(vals$simList)
      return(length(vals$simList))
      
    }) # end of renderText, "rep_nsims"
    
    # ALERTS --------------------------------------------------------------
    
    observe({
      req(vals$active_tab == 'report')
      
      if (!is.null(vals$is_analyses)) {
        if (!vals$is_analyses) 
          shinyalert::shinyalert(
            type = "error",
            title = "Regime does not match analyses",
            text = tagList(span(
              "You have changed the regime without re-running",
              "estimations. Please go back to the",
              icon("compass-drafting", class = "cl-mdn"),
              span("Analyses", class = "cl-mdn"), "tab",
              "and make sure to click the",
              icon("paper-plane", class = "cl-mdn"),
              span("'Run estimation'", class = "cl-mdn"), "button."
            )),
            html = TRUE,
            size = "s")
      }
      
    }) # end of observer
    
    # OPERATIONS ----------------------------------------------------------
    ## Credible intervals for home range estimation: ----------------------
    
    observe({
      req(vals$active_tab == 'report')
      req(vals$tau_p0, vals$dur$value, vals$dur$unit)
      
      input_taup <- "days" %#% 
        vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      
      dat <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      index_taup <- which.min(abs(dat$tau_p - input_taup))
      out_taup <- dat$tau_p[index_taup]
      
      index_dur <- which.min(abs(dat$dur - input_dur))
      out_dur <- dat$dur[index_dur]
      
      newdat <- dat %>%
        dplyr::filter(tau_p == out_taup) %>%
        dplyr::filter(duration == out_dur)
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      vals$hr_CI <- data.frame(
        mean = mean(newdat$error, na.rm = TRUE),
        CI_low = mean(newdat$error_lci, na.rm = TRUE),
        CI_high = mean(newdat$error_uci, na.rm = TRUE))

      # Credible intervals:
      vals$hr_HDI <- suppressWarnings(
        bayestestR::ci(newdat$error, ci = ci, method = "HDI"))
      
    }) # end of observe
    
    
    observe({ # For comparison with new duration:
      req(input$highlight_dur > 0)
      shinyjs::show(id = "end_comparison")
      
      input_taup <- "days" %#% 
        vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      
      dat <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      index_taup <- which.min(abs(dat$tau_p - input_taup))
      out_taup <- dat$tau_p[index_taup]
      out_dur <- as.numeric(input$highlight_dur)
      
      newdat <- dat %>%
        dplyr::filter(tau_p == out_taup) %>%
        dplyr::filter(duration == out_dur)
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      vals$hr_CI_new <- data.frame(
        mean = mean(newdat$error, na.rm = TRUE),
        CI_low = mean(newdat$error_lci, na.rm = TRUE),
        CI_high = mean(newdat$error_uci, na.rm = TRUE))
      
      # Credible intervals:
      vals$hr_HDI_new <-
        suppressWarnings(bayestestR::ci(
          newdat$error, ci = ci, method = "HDI"))
      
    }) %>% # end of observe,
      bindEvent(input$highlight_dur)
    
    ## Credible intervals for speed & distance estimation: ----------------
    
    observe({
      req(vals$active_tab == 'report')
      req(vals$tau_v0, vals$dti$value, vals$dti$unit)
      
      input_tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      input_dti <- vals$dti$value %#% vals$dti$unit
      
      dat <- movedesign::sims_speed[[1]] %>%
        dplyr::mutate(dur = round("days" %#% dur, 0))
      
      index_tauv <- which.min(abs(dat$tau_v - input_tauv))
      out_tauv <- dat$tau_v[index_tauv]
      
      index_dti <- which.min(abs(dat$dti - input_dti))
      out_dti <- dat$dti[index_dti]
      
      index_dur <- which.min(abs(dat$dur - input_dur))
      out_dur <- dat$dur[index_dur]
      
      newdat <- dat %>%
        dplyr::filter(tau_v == out_tauv) %>%
        dplyr::filter(dur == out_dur) %>%
        dplyr::filter(dti == out_dti)
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      vals$sd_CI <- data.frame(
        mean = mean(newdat$error, na.rm = TRUE),
        CI_low = mean(newdat$error_lci, na.rm = TRUE),
        CI_high = mean(newdat$error_uci, na.rm = TRUE))
      
      # Credible intervals:
      vals$sd_HDI <- suppressWarnings(
        bayestestR::ci(newdat$error, ci = ci, method = "HDI"))
      
    }) # end of observe
    
    observe({ # For comparison with new interval:
      req(input$highlight_dti > 0)
      shinyjs::show(id = "end_comparison")
      
      input_tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      
      dat <- movedesign::sims_speed[[1]]
      index_tauv <- which.min(abs(dat$tau_v - input_tauv))
      
      out_tauv <- dat$tau_v[index_tauv]
      opts <- movedesign::sims_speed[[1]] %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      out_dti <- fix_unit(
        opts$dti[match(input$highlight_dti, opts$dti_notes)],
        "seconds")
      
      newdat <- dat %>%
        dplyr::filter(tau_v == out_tauv) %>%
        dplyr::filter(dti == out_dti$value)
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      vals$sd_CI_new <- data.frame(
        mean = mean(newdat$error, na.rm = TRUE),
        CI_low = mean(newdat$error_lci, na.rm = TRUE),
        CI_high = mean(newdat$error_uci, na.rm = TRUE))
      
      # Credible intervals:
      vals$sd_HDI_new <- suppressWarnings(
        bayestestR::ci(newdat$error, ci = ci, method = "HDI"))
      
    }) %>% # end of observe,
      bindEvent(input$highlight_dti)
    
    # REPORT --------------------------------------------------------------
    
    observe({
      req(vals$which_question,
          vals$data_type,
          vals$is_analyses,
          vals$simList)
      
      questions <- NULL
      if ("Home range" %in% vals$which_question) {
        req(vals$hr_completed)
        questions <- "Home range"
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        req(vals$sd_completed)
        add_to <- ifelse(length(vals$which_question) > 1, ", ", "")
        questions <- paste0(questions, add_to, "Speed & distance")
      }
      
      msg_log(
        style = "warning",
        message = paste0("Building ",
                         msg_warning("report"), "..."),
        detail = paste("Current question(s):", questions))

    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    observe({
      ### reaching variable name limit, need to simplify
      req(vals$which_question,
          vals$data_type,
          vals$is_analyses,
          vals$simList)

      if ("Home range" %in% vals$which_question) {
        req(vals$hr_completed)
      }

      if ("Speed & distance" %in% vals$which_question) {
        req(vals$sd_completed)
        add_to <- ifelse(length(vals$which_question) > 1, ", ", "")
      }
      
      boxnames <- c("analyses", "tables")
      for (i in 1:length(boxnames)) {
        shinyjs::show(id = paste0("repBox_", boxnames[i]))
      }

      # Characteristic timescales:

      tau_p <- vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      tau_v <- ifelse(is.null(vals$tau_v0), 0,
                      vals$tau_v0$value[2] %#% vals$tau_v0$unit[2])

      # Ideal sampling design:

      ideal_dur <- fix_unit(tau_p * 30, "seconds", convert = TRUE)
      dur_unit <- ideal_dur$unit
      
      if (is.null(vals$tau_v0)) {
        ideal_dti <- data.frame(value = Inf, unit = "days")
      } else {
        ideal_dti <- fix_unit(vals$tau_v0$value[2],
                              vals$tau_v0$unit[2], convert = TRUE)
      }

      dti_unit <- ifelse(is.null(vals$tau_v0), "days", ideal_dti$unit)

      # Current sampling design:

      dur <- dur_unit %#% vals$dur$value %#% vals$dur$unit
      dur <- fix_unit(dur, dur_unit)
      dti <- dti_unit %#% vals$dti$value %#% vals$dti$unit
      dti <- fix_unit(dti, dti_unit)
      
      vals$hr_col <- vals$ctsd_col <- data.frame(
        hex = pal$sea, css = "var(--sea)")

      if (dur$value <= ideal_dur$value) {
        vals$hr_col$hex <- pal$dgr
        vals$hr_col$css <- "var(--danger)"
      }

      if (!is.infinite(ideal_dti$value)) {

        if (dti$value <= ideal_dti$value) {
          diff_dti <- tau_v / (vals$dti$value %#% vals$dti$unit)
        } else {
          diff_dti <- 1 / (tau_v / (vals$dti$value %#% vals$dti$unit))
        }

        min_dti <- fix_unit(dti_unit %#% (tau_v * 3), dti_unit)

        if (dti$value > 3 * ideal_dti$value) {
          vals$ctsd_col$hex <- pal$dgr
          vals$ctsd_col$css <- "var(--danger)"
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

      ## Reporting DATA: --------------------------------------------------

      switch(vals$data_type,
             "selected" = {
               out_species <- span(
                 "These outputs are based on parameters extracted",
                 "from", span(vals$id, class = "cl-grn"),
                 "and species", span(vals$species_common,
                                     class = "cl-grn"),
                 wrap_none("(", em(vals$species_binom), ")."))
             },
             "uploaded" = {
               out_species <- span(
                 "These outputs are based on parameters extracted",
                 "from individual", span(vals$id, class = "cl-grn"),
                 "and species", wrap_none(em(vals$species,
                                             class = "cl-grn"), "."))
             },
             "simulated" = {
               out_species <- span(
                 "These outputs are based on a",
                 span("simulated", class = "cl-grn"),
                 "dataset.")
             }
      ) # end of switch

      vals$report$species <- p(
        out_species,
        "Please see the",
        icon("paw", class = "cl-sea"),
        span("Species", class = "cl-sea"),
        "parameters above for more details.")

      ## Reporting DESIGN: ------------------------------------------------

      ### For home range estimation:

      if ("Home range" %in% vals$which_question) {
        req(vals$hr_HDI)
        N1 <- scales::label_comma(accuracy = 1)(vals$dev$N1)

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

      if ("Speed & distance" %in% vals$which_question) {
        req(vals$sd_HDI)
        N2 <- scales::label_comma(accuracy = 1)(vals$dev$N2)

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
                                color = vals$ctsd_col[1], end = ","),
            "resulting in an effective sample size equivalent to",
            N2, "independently sampled velocities.")

      } # end of "Speed & distance"

      ### Both home range and speed & distance:

      if (length(vals$which_question) > 1)
        out_regime <- tagList(out_reg_hr, out_reg_ctsd)

      vals$report$regime <- out_regime

      ## Reporting OUTPUTS: -----------------------------------------------

      vals$report$analyses <- NULL

      ## Home range estimation:

      if ("Home range" %in% vals$which_question) {
        req(vals$hr_HDI, vals$hrErr)

        hrCI <- c(round(vals$hr_HDI$CI_low * 100, 1),
                  round(vals$hr_HDI$CI * 100, 0),
                  round(vals$hr_HDI$CI_high * 100, 1))

        txt_level <- ifelse(
          vals$hr_HDI$CI_high < .3 & vals$hr_HDI$CI_low > -.3,
          "and with low", "but with high")

        opts_dur <- 2^seq(1, 12, by = 1)
        index_dur <- which.min(abs(
          opts_dur - ("days" %#% dur$value %#% dur$unit)))
        plotted_dur <- opts_dur[index_dur]

        out_analyses <- NULL

        if (dur$value >= ideal_dur$value) {
          vals$report$is_hr <- TRUE
          out_hr1 <- span(
            style = "font-weight: bold;",
            "Your current sampling duration is likely sufficient",
            "for home range estimation,", txt_level, "uncertainty.")

        } else {
          vals$report$is_hr <- FALSE
          out_hr1 <- span(
            style = "font-weight: bold;",
            "Your current sampling duration may be insufficient",
            "for home range estimation.")
        }

        out_hr2 <- span(
          "Keep in mind that, for a similar duration of",
          plotted_dur, "days, there is a",
          wrap_none(hrCI[2], "%", css = "cl-blk"),
          "probability that the relative error will lie between",
          wrap_none(hrCI[1], "%", css = "cl-blk"),
          "and", wrap_none(hrCI[2], "%", end = ".", css = "cl-blk"))

        nsims <- ifelse(
          length(vals$simList) == 1,
          "a single simulation",
          paste(length(vals$simList), "simulations"))

        out_nsims <- span(
          "Your error estimate based on",
          span(style = "font-weight: bold;", nsims),
          "was",
          wrap_none(
            ifelse(
              nsims == 1,
              round(vals$hrErr$value[[2]] * 100, 1),
              round(mean(vals$hrErrList$est, na.rm = TRUE) * 100, 1)),
            "%."))

        out_analyses <- out_hr <- p(
          out_hr1,
          out_nsims,
          out_hr2)

      } # end of 'Home range'

      ## Speed and distance estimation:

      if ("Speed & distance" %in% vals$which_question) {

        req(!is.null(vals$is_ctsd))
        if (vals$is_ctsd) {
          req(vals$sd_HDI, vals$speedErr$value)

          sdCI <- c(round(vals$sd_HDI$CI_low * 100, 1),
                    round(vals$sd_HDI$CI * 100, 0),
                    round(vals$sd_HDI$CI_high * 100, 1))

          txt_level <- ifelse(
            vals$sd_HDI$CI_high < .3 &
              vals$sd_HDI$CI_low > -.3,
            "and with low", "but with high")

          ctsd_err <- vals$speedErr$value[[2]]
        }

        dti_options <- movedesign::sims_speed[[1]] %>%
          dplyr::select(dti, dti_notes) %>%
          unique()

        index_dti <- which.min(
          abs(dti_options$dti - (dti$value %#% dti$unit)))
        plotted_dti <- sub('^\\w+\\s\\w+\\s\\w+\\s', '',
                           dti_options[index_dti, 2])

        if (vals$dev$N2 >= 30) {

          vals$report$is_ctsd <- TRUE
          out_ctsd1 <- span(
            style = "font-weight: bold;",
            "Your current sampling interval is likely sufficient",
            "for speed & distance estimation,",
            txt_level, "uncertainty.")

        } else {
          if (vals$dev$N2 > 0) {
            vals$report$is_ctsd <- FALSE
            out_ctsd1 <- span(
              style = "font-weight: bold;",
              "Your current sampling interval may be insufficient",
              "for speed & distance estimation.")

          } else {
            vals$report$is_ctsd <- FALSE
            out_ctsd1 <- span(
              style = "font-weight: bold;",
              "Your current sampling interval was too coarse",
              "for speed & distance estimation.")
          }
        }

        if (vals$is_ctsd) {
          out_ctsd2 <-
            span("Keep in mind that, for a similar interval of",
                 wrap_none(dti$value, " ", dti$unit, ","),
                 "there is a",
                 wrap_none(sdCI[2], "%", css = "cl-blk"),
                 "probability that the relative error will lie within",
                 wrap_none(sdCI[1], "%", css = "cl-blk"), "and",
                 wrap_none(sdCI[3], "%", end = ".", css = "cl-blk"))

          out_analyses <- out_ctsd <- p(
            out_ctsd1,
            "Your error estimate based on a",
            span(style = "font-weight: bold;", "single"),
            "simulation was",
            wrap_none(round(ctsd_err * 100, 1), "%."),
            out_ctsd2)

        } else {
          out_analyses <- out_ctsd <- p(out_ctsd1)
        }

      } # end of "Speed & distance"

      req(length(vals$which_question) == 1)
      vals$report$analyses <- out_analyses

    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    observe({
      req(length(vals$which_question) > 1,
          vals$hr_completed,
          vals$sd_completed,
          vals$simList)
      
      ### Both home range and speed & distance:
      
      is_hr <- vals$report$is_hr
      is_ctsd <- vals$report$is_ctsd
      req(!is.null(is_hr), !is.null(is_ctsd))
      
      sufficient <- span("sufficient", class = "cl-grn")
      insufficient <- span("insufficient", class = "cl-dgr")
      
      ## Number of simulations:
      
      out_nsims <- span(
        "a", span(style = "font-weight: bold;", "single"), 
        "simulation")
      if (length(vals$simList) > 1) {
        out_nsims <- span(
          span(style = "font-weight: bold;", length(vals$simList)), 
          "simulations")
      }
      
      # Home range estimation errors:
      
      hrErr_lci <- round(vals$hrErr$value[[1]] * 100, 1)
      hrErr_est <- round(vals$hrErr$value[[2]] * 100, 1)
      hrErr_uci <- round(vals$hrErr$value[[3]] * 100, 1)
      
      if (length(vals$simList) > 1) {
        req(vals[["hrErrList"]])
        
        ci <- suppressWarnings(bayestestR::ci(
          vals$hrErrList$est, ci = .95, method = "HDI"))
        hrErr_lci <- ci$CI_low
        hrErr_uci <- ci$CI_high
        
        # hrErr_lci <- round(
        #   mean(vals[["hrErrList"]]$lci * 100, 
        #        na.rm = TRUE), 1)
        hrErr_est <- round(
          mean(vals[["hrErrList"]]$est * 100, 
               na.rm = TRUE), 1)
        # hrErr_uci <- round(
        #   mean(vals[["hrErrList"]]$uci * 100, 
        #        na.rm = TRUE), 1)
      }
      
      # Speed and distance errors:
      
      if (vals$dev$N2 > 0) {
        sdErr_lci <- round(vals$speedErr$value[[1]] * 100, 1)
        sdErr_est <- round(vals$speedErr$value[[2]] * 100, 1)
        sdErr_uci <- round(vals$speedErr$value[[3]] * 100, 1)
        
        if (length(vals$simList) > 1) {
          req(vals[["speedErrList"]])
          
          ci <- suppressWarnings(bayestestR::ci(
            vals$hrErrList$est, ci = .95, method = "HDI"))
          sdErr_uci <- ci$CI_low
          sdErr_lci <- ci$CI_high
          
          # sdErr_lci <- round(
          #   mean(vals[["speedErrList"]]$lci * 100, 
          #        na.rm = TRUE), 1)
          sdErr_est <- round(
            mean(vals[["speedErrList"]]$est * 100, 
                 na.rm = TRUE), 1)
          # sdErr_uci <- round(
          #   mean(vals[["speedErrList"]]$uci * 100, 
          #        na.rm = TRUE), 1)
        }
      }
      
      if (is_hr & !is_ctsd) {
        out <- span(
          style = "font-weight: bold;",
          
          "Your current tracking regime is likely sufficient",
          "for home range estimation, but insufficient",
          "for speed & distance estimation.")
        
        if (vals$dev$N2 == 0)
          out <- span(
            style = "font-weight: bold;",
            
            "Your current tracking regime is likely sufficient",
            "for home range estimation, and is inappropriate for",
            span("inappropriate", class = "cl-dgr"),
            "for speed & distance estimation.")
      }
      
      if (!is_hr & is_ctsd) {
        out <- span(
          style = "font-weight: bold;",
          
          "Your current tracking regime may be insufficient",
          "for home range estimation, but likely sufficient",
          "for speed & distance estimation.")
      }
      
      if (is_hr & is_ctsd) {
        out <- span(
          style = "font-weight: bold;",
          
          "Your current tracking regime is likely sufficient",
          "for both home range estimation and for",
          "speed & distance estimation.")
      }
      
      if (!is_hr & !is_ctsd) {
        out <- span(
          style = "font-weight: bold;",
          
          "Your current tracking regime may be insufficient",
          "for both home range estimation and for",
          "speed & distance estimation.")
        
        if (vals$dev$N2 == 0)
          out <- span(
            style = "font-weight: bold;",
            
            "Your current tracking regime may be insufficient",
            "for home range estimation, and is inappropriate for",
            span("inappropriate", class = "cl-dgr"),
            "for speed & distance estimation.")
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
      
      # if (is.na(hrErr_lci) || is.na(hrErr_uci)) {
      #   out_extra <- span(
      #     "Credible intervals (CIs) are too large or the number of",
      #     "simulations insufficient, returning ",
      #     wrap_none(span("NAs", class = "cl-dgr"), "."),
      #     "Run more simulations to obtain valid (CIs).") 
      # } else { out_extra <-  "" }
      
      out_analyses <- p(
        out,
        "Your error estimate based on",
        out_nsims, "was", out_hr_err,
        if (vals$dev$N2 > 0) { out_sd_err
        } else {
          span("but the sampling interval was",
               "too coarse to estimate speed.")
        }
      )
      
      vals$report$analyses <- out_analyses
      
    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    ## Rendering complete report: -----------------------------------------
    
    observe({
      req(vals$which_question)
      
      if (length(vals$which_question) > 1) {
        shinyjs::hide(id = "section-comparison")
      } else {
        shinyjs::show(id = "section-comparison")
      }
      
      is_both <- ifelse(length(vals$which_question) > 1, "Yes", "No")
      
      switch(
        is_both,
        "No" = {
          if ("Home range" %in% vals$which_question) {
            output$end_report <- renderUI({
              
              out <- tagList(
                vals$report$species,
                vals$report$regime,
                
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
                
                vals$report$analyses) # end of tagList
              
            }) # end of renderUI, "end_report"
          } # end of hr only section
          
          if ("Speed & distance" %in% vals$which_question) {
            output$end_report <- renderUI({
              
              out <- tagList(
                vals$report$species,
                vals$report$regime,
                
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
                #     bsplus::bs_attach_modal(id_modal = "modal_precision")
                # ), # end of fluidRow
                
                ggiraph::girafeOutput(
                  outputId = ns("repPlot_precision"),
                  width = "100%", height = "100%"),
                uiOutput(ns("repPlotLegend2")),
                
                vals$report$analyses) # end of tagList
              
            }) # end of renderUI, "end_report"
          } # end of sd only section
        },
        "Yes" = {
          
          output$end_report <- renderUI({
            out <- tagList(
              vals$report$species,
              vals$report$regime,
              vals$report$analyses)
          })
          
          output$end_report_both <- renderUI({
            req(length(vals$which_question) > 1)
            
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
                #     bsplus::bs_attach_modal(id_modal = "modal_precision")
                # ), # end of fluidRow
                
                ggiraph::girafeOutput(
                  outputId = ns("repPlot_precision"),
                  width = "100%", height = "100%"),
                
                uiOutput(ns("repPlotLegend2"))
                
                
              ) # end of tagList
            ) # end of div
            
          }) # end of renderUI, "end_report_both"
          
        }) # end of switch
      
    }) %>% bindEvent(input$build_report)
    
    ## Reporting COMPARISON (if available): ------------------------------
    
    observe({
      out_comp <- out_comp_hr <- span("")
      
      if (length(vals$which_question) == 1 &
          "Home range" %in% vals$which_question) {
        req(input$highlight_dur)
        
        highlighted_dur <- as.numeric(input$highlight_dur)
        
        CI <- round(vals$hr_HDI_new$CI * 100, 0)
        LCI <- round(vals$hr_HDI_new$CI_low * 100, 1)
        UCI <- round(vals$hr_HDI_new$CI_high * 100, 1)
        
        txt_level <- ifelse(
          vals$hr_HDI_new$CI_high < .3 & vals$hr_HDI_new$CI_low > -.3,
          "and with low", "but with high")
        
        ideal_dur <- fix_unit(
          ("days" %#% vals$tau_p0$value[2] %#%
             vals$tau_p0$unit[2]) * 10,
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
      
      if (length(vals$which_question) == 1 &
          "Speed & distance" %in% vals$which_question) {
        req(input$highlight_dti)
        
        opts <- movedesign::sims_speed[[1]] %>%
          dplyr::select(dti, dti_notes) %>%
          unique()
        
        highlighted_dti <- opts$dti[match(input$highlight_dti,
                                          opts$dti_notes)]
        
        out_dti <- fix_unit(highlighted_dti, "seconds",
                            convert = TRUE)
        
        CI <- round(vals$sd_HDI_new$CI * 100, 0)
        LCI <- round(vals$sd_HDI_new$CI_low * 100, 1)
        UCI <- round(vals$sd_HDI_new$CI_high * 100, 1)
        
        txt_level <- ifelse(
          vals$sd_HDI_new$CI_high < .3 & vals$sd_HDI_new$CI_low > -.3,
          "and with low", "but with high")
        
        ideal_dti <- fix_unit(
          (vals$tau_v0$value[2] %#% 
             vals$tau_v0$unit[2]) / 3, "seconds")
        
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
      
      if (length(vals$which_question) > 1) {
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
        
      }) # end of renderUI // end_comparison
      
    }) # end of observe
    
    # PLOTS ---------------------------------------------------------------
    ## Rendering density plots: -------------------------------------------
    
    output$repPlotLegend1 <- renderUI({
      req(vals$which_question, input$ci,
          vals$tau_p0, vals$tau_v0, vals$dur, vals$dti)
      
      input_taup <- "days" %#% 
        vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      input_tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      input_dti <- vals$dti$value %#% vals$dti$unit
      
      dt_hr <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      out_taup <- dt_hr$tau_p[which.min(abs(dt_hr$tau_p - input_taup))]
      dur_for_hr <- dt_hr$dur[which.min(abs(dt_hr$dur - input_dur))]
      
      dt_sd <- movedesign::sims_speed[[1]] %>%
        dplyr::mutate(dur = round("days" %#% dur, 1))
      out_tauv <- dt_sd$tau_v[which.min(abs(dt_sd$tau_v - input_tauv))]
      out_tauv <- fix_unit(out_tauv, "seconds", convert = TRUE)
      dur_for_sd <- dt_sd$dur[which.min(abs(dt_sd$dur - input_dur))]
      
      dt_sd <- movedesign::sims_speed[[1]] %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      out_dti <- dt_sd$dti[which.min(abs(dt_sd$dti - input_dti))]
      dti_for_sd <- dt_sd$dti_notes[match(out_dti, dt_sd$dti)]
      dti_for_sd
      
      txt_highlight <- ""
      
      add_txt_highlight <- FALSE
      if (!is.null(input$highlight_dur)) {
        if (input$highlight_dur != "") add_txt_highlight <- TRUE
      }
      
      if (!is.null(input$highlight_dti)) {
        if (input$highlight_dti != "") add_txt_highlight <- TRUE
      }
      
      if (add_txt_highlight) {
        txt_highlight <- span(
          "The comparison requested is in",
          wrap_none(
            fontawesome::fa("diamond", fill = "black"),
            " in ", span("black", style = "color: black;"), "."))
      }
      
      if (length(vals$which_question) > 1) {
        ui <- tagList(
          fontawesome::fa("circle-exclamation", fill = pal$dgr),
          span("Note:", class = "help-block-note"), 
          "These plots show the probability density of estimate errors",
          "based on 400 simulations, with the medians",
          wrap_none(
            "(", fontawesome::fa("diamond"), " in lighter colors),"),
          "and the", wrap_none(input$ci, "%"),
          "credible intervals (shaded areas + lines).",
          "For AKDE, the 400 simulations were based on", 
          "movement processes with \u03C4\u209A = ", out_taup, "days;",
          "for CTSD, \u03C4\u1D65 = ", 
          out_tauv$value, paste0(out_tauv$unit, "."),
          "Your simulation(s)",
          "estimate errors are the circles", wrap_none(
            "(", fontawesome::fa("circle", prefer_type = "solid"), ")"),
          "in darker colors.")
        
      } else {
        
        switch(
          vals$which_question,
          "Home range" = {
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "This plot shows the probability density of estimate errors",
              "based on 400 simulations for a sampling duration of", 
              dur_for_hr, "days,", "with the median",
              wrap_none(
                "(", fontawesome::fa("diamond", fill = pal$sea),
                " in ", span("light blue", class = "cl-sea"), "),"),
              "and the", wrap_none(input$ci, "%"),
              "credible intervals (shaded area).",
              "In contrast, your simulation(s)'",
              "error is the circle", wrap_none(
                "(", fontawesome::fa("circle", prefer_type = "solid",
                                     fill = pal$sea_d), ")"),
              "in", wrap_none(span("darker blue",
                                   class = "cl-sea-d"), "."),
              txt_highlight)
          },
          "Speed & distance" = {
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "This plot shows the probability density of estimate errors",
              "based on 400 simulations for a sampling interval of", 
              wrap_none(dti_for_sd, ","), "with the median",
              wrap_none(
                "(", fontawesome::fa("diamond", fill = pal$sea),
                " in ", span("light blue", class = "cl-sea"), "),"),
              "and the", wrap_none(input$ci, "%"),
              "credible intervals (shaded area).",
              "Your simulation(s)'",
              "error is the circle", wrap_none(
                "(", fontawesome::fa("circle", prefer_type = "solid",
                                     fill = pal$sea_d), ")"),
              "in", wrap_none(span("darker blue",
                                   class = "cl-sea-d"), "."),
              txt_highlight)
          },
          stop(paste0("No handler for ",
                      vals$which_question, "."))
        )
      }
      
      ui <- span(class = "help-block", ui)
      
      return(ui)
      
    }) # end of renderUI, "repPlotLegend1"
    
    #### Accuracy of home range simulations: ------------------------------
    
    output$repPlot_hr <- ggiraph::renderGirafe({
      req(vals$which_question, input$ci)
      
      input_ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      
      input_taup <- "days" %#% 
        vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      
      is_both <- FALSE
      vals$ft_size <- 13
      if (length(vals$which_question) > 1) {
        is_both <- TRUE
        vals$ft_size <- 16
      }
      
      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")
      
      # Preparing if () statements:
      
      is_dur <- FALSE
      if (!is.null(input$highlight_dur))
        if (!is.na(as.numeric(input$highlight_dur))) 
          is_dur <- TRUE
      
      is_log <- FALSE
      if (!is.null(input$scale_density))
        if (input$scale_density) 
          is_log <- TRUE
      
      # Prepare datasets:
      
      dt_hr <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      out_taup <- dt_hr$tau_p[which.min(abs(dt_hr$tau_p - input_taup))]
      dur_for_hr <- dt_hr$dur[which.min(abs(dt_hr$dur - input_dur))]
      vals$report$dur_for_hr <- dur_for_hr
      
      # Create density data frames:
      
      ds1_hr <- dt_hr %>%
        dplyr::filter(tau_p == out_taup) %>%
        dplyr::filter(duration == dur_for_hr) %>%
        stats::na.omit()
      med <- stats::median(ds1_hr$error)
      
      ds1_hr <- stats::density(ds1_hr$error)
      ds1_hr <- data.frame(x = ds1_hr$x, y = ds1_hr$y)
      vals$report$ds1_hr <- data.frame(
        "median" = med,
        "max" = max(ds1_hr$x),
        "min" = min(ds1_hr$x),
        "done" = FALSE)
      
      if (is_log) ds1_hr$y <- ds1_hr$y / max(ds1_hr$y)
      
      ci1_hr <- subset(
        ds1_hr, x >= vals$hr_HDI$CI_low & x <= vals$hr_HDI$CI_high)
      
      if (is_dur) {
        req(vals$hr_HDI_new)
        
        out_dur_new <- dt_hr$dur[
          abs(dt_hr$dur - as.numeric(input$highlight_dur)) %>%
            which.min()]
        
        ds2_hr <- dt_hr %>%
          dplyr::filter(tau_p == out_taup) %>%
          dplyr::filter(duration == out_dur_new) %>%
          stats::na.omit()
        med <- stats::median(ds2_hr$error)
        
        ds2_hr <- stats::density(ds2_hr$error)
        ds2_hr <- data.frame(x = ds2_hr$x, y = ds2_hr$y)
        vals$report$ds2_hr <- data.frame(
          "median" = med,
          "max" = max(ds2_hr$x),
          "min" = min(ds2_hr$x))
        
        vals$hr_HDI_new <- 
          suppressWarnings(bayestestR::ci(
            ds2_hr$x, ci = input_ci, method = "HDI"))
        
        if (is_log) ds2_hr$y <- ds2_hr$y / max(ds2_hr$y)
        
        ci2_hr <- subset(
          ds2_hr, x >= vals$hr_HDI_new$CI_low & x <= vals$hr_HDI_new$CI_high)
        
        hr_p1 <- ggplot2::geom_line(
          data = ds2_hr, mapping = ggplot2::aes(x = x, y = y),
          col = pal$mdn, linetype = "dotted")
        
        hr_p2 <- ggplot2::geom_area(
          data = ci2_hr,
          mapping = ggplot2::aes(x = x, y = y),
          alpha = 0.2, fill = pal$mdn)
        
        hr_p3 <- ggplot2::geom_segment(
          data = vals$hr_HDI_new,
          mapping = ggplot2::aes(
            x = .data$CI_low,
            xend = .data$CI_high,
            y = 0, yend = 0,
            col = "est_new", linetype = "est_new"),
          size = .8)
        
        hr_p4 <- ggplot2::geom_point(
          mapping = ggplot2::aes(
            x = vals$report$ds2_hr[["median"]], y = 0,
            col = "est_new", shape = "est_new"),
          size = 6)
      }
      
      lbl <- c(
        paste0("AKDE error"),
        paste0("Median AKDE error + ", vals$hr_HDI$CI * 100,
               "% HDI for ", dur_for_hr, " days"))
      brk <- c("now", "est")
      
      val_fill <- val_col <- c("now" = pal$sea_d, "est" = pal$sea)
      val_linetype <- c("now" = "blank", "est" = "solid")
      val_shape <- c("now" = 19, "est" = 18)
      
      override_size <- c(.8, .8)
      override_stroke <- c(4, 4)
      
      if (is_dur) {
        lbl <- c(
          lbl, paste0("Median AKDE error + ", vals$hr_HDI$CI * 100,
                      "% HDI for ", input$highlight_dur, " days"))
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
        
        ggplot2::geom_area(
          data = ci1_hr,
          mapping = ggplot2::aes(x = x, y = y),
          alpha = 0.4, fill = pal$sea) +
        
        { if (is_dur) hr_p3 } +
        
        ggplot2::geom_segment(
          mapping = ggplot2::aes(
            x = vals$hr_HDI$CI_low,
            xend = vals$hr_HDI$CI_high,
            y = 0, yend = 0, col = "est",
            linetype = "est"),
          size = .8) +
        
        { if (is_dur) hr_p4 } +
        
        ggplot2::geom_point(
          mapping = ggplot2::aes(
            x = vals$report$ds1_hr[["median"]], y = 0,
            col = "est", shape = "est"),
          size = 6) +
        
        { if (!is.null(vals$hrErr)) 
          ggplot2::geom_point(
            ggplot2::aes(x = vals$hrErr$value[[2]], y =  0,
                         col = "now", shape = "now"),
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
        ggplot2::scale_fill_manual(
          name = "", labels = lbl, breaks = brk,
          values = val_fill) +
        ggplot2::scale_linetype_manual(
          name = "", labels = lbl, breaks = brk,
          values = val_linetype) +
        ggplot2::scale_shape_manual(
          name = "", labels = lbl, breaks = brk,
          values = val_shape) +
        
        ggplot2::labs(x = "Estimate error (%)",
                      y = y_lab) +
        
        theme_movedesign(ft_size = vals$ft_size) +
        ggplot2::theme(
          legend.position = "none",
          axis.title.x = ggplot2::element_blank())
      
      vals$report$ds1_hr[["done"]] <- TRUE
      
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
      
    }) %>% # end of renderGirafe // repPlot_hr
      bindEvent(list(input$build_report,
                     input$scale_density,
                     input$highlight_dur))
    
    #### Accuracy of speed & distance simulations: ------------------------
    
    observe({
      req(vals$data0, vals$tau_v0,
          vals$dur$value, vals$dur$unit,
          vals$dti$value, vals$dti$unit,
          input$ci, vals$which_question)
      
      is_both <- FALSE
      vals$ft_size <- 13
      if (!is.null(vals$which_question)) {
        if (length(vals$which_question) > 1) {
          is_both <- TRUE
          vals$ft_size <- 16
        }
      }
      
      req("Speed & distance" %in% vals$which_question)
      input_ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      
      input_tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      input_dti <- vals$dti$value %#% vals$dti$unit
      
      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")
      
      # Preparingif () statements:
      
      is_dti <- FALSE
      if (!is.null(input$highlight_dti)) {
        if (input$highlight_dti != "")
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
      vals$report$txt_dti <- txt_dti
      
      # Create density data frames:
      
      ds1_sd <- dt_sd %>%
        dplyr::filter(tau_v == out_tauv) %>%
        dplyr::filter(dur == dur_for_sd) %>%
        dplyr::filter(dti == out_dti) %>% 
        stats::na.omit()
      med <- stats::median(ds1_sd$error)
        
      ds1_sd <- stats::density(ds1_sd$error)
      ds1_sd <- data.frame(x = ds1_sd$x, y = ds1_sd$y)
      vals$report$ds1_sd <- data.frame(
        "median" = med,
        "max" = max(ds1_sd$x),
        "min" = min(ds1_sd$x),
        "done" = FALSE)
      
      if (is_log) ds1_sd$y <- ds1_sd$y / max(ds1_sd$y)
      
      ci1_sd <- subset(
        ds1_sd, x >= vals$sd_HDI$CI_low & x <= vals$sd_HDI$CI_high)
      
      if (is_dti) {
        req(vals$sd_HDI_new)
        
        dti_new <- sd_opts$dti[match(input$highlight_dti,
                                     sd_opts$dti_notes)]
        out_dti_new <- dt_sd$dti[which.min(abs(dt_sd$dti - dti_new))]
        txt_dti_new <- sd_opts$dti_notes[match(out_dti_new,
                                               sd_opts$dti)]
        vals$report$txt_dti_new <- txt_dti_new
        
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
        vals$report$ds2_sd <- data.frame(
          "median" = med,
          "max" = max(ds2_sd$x),
          "min" = min(ds2_sd$x))
        
        vals$sd_HDI_new <-
          suppressWarnings(bayestestR::ci(
            ds2_sd$x, ci = input_ci, method = "HDI"))
        
        if (is_log) ds2_sd$y <- ds2_sd$y / max(ds2_sd$y)
        
        ci2_sd <- subset(
          ds2_sd, x >= vals$sd_HDI_new$CI_low & x <= vals$sd_HDI_new$CI_high)
        
        sd_p1 <- ggplot2::geom_line(
          data = ds2_sd, mapping = ggplot2::aes(x = x, y = y),
          col = pal$mdn, linetype = "dotted")
        
        sd_p2 <- ggplot2::geom_area(
          data = ci2_sd,
          mapping = ggplot2::aes(x = x, y = y),
          alpha = 0.2, fill = pal$mdn)
        
        sd_p3 <- ggplot2::geom_segment(
          data = vals$sd_HDI_new,
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
        paste0("Median CTSD error + ", vals$sd_HDI$CI * 100,
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
          lbl, paste0("Median CTSD error + ", vals$sd_HDI$CI * 100,
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
          
          ggplot2::geom_area(
            data = ci1_sd,
            mapping = ggplot2::aes(x = x, y = y),
            alpha = 0.4, fill = 
              ifelse(is_both, pal$grn, pal$sea)) +
          
          {if (is_dti) sd_p3 } +
          
          ggplot2::geom_segment(
            mapping = ggplot2::aes(
              x = vals$sd_HDI$CI_low,
              xend = vals$sd_HDI$CI_high,
              y = 0, yend = 0, col = "est",
              linetype = "est"),
            size = .8) +
          
          {if (is_dti) sd_p4 } +
          
          ggplot2::geom_point(
            data = ds1_sd,
            mapping = ggplot2::aes(
              x = mean(x), y = 0,
              col = "est", shape = "est"),
            size = 6) +
          
          { if (!is.null(vals$speedErr$value))
            ggplot2::geom_point(
              ggplot2::aes(x = vals$speedErr$value[[2]], y = 0,
                           col = "now", shape = "now"),
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
          ggplot2::scale_fill_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_fill) +
          ggplot2::scale_linetype_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_linetype) +
          ggplot2::scale_shape_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_shape) +
          
          ggplot2::labs(x = "Estimate error (%)",
                        y = y_lab) +
          
          theme_movedesign(ft_size = 16) +
          ggplot2::theme(
            legend.position = "none",
            axis.title.x = ggplot2::element_blank())
        
        vals$report$ds1_sd[["done"]] <- TRUE
        
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
                     input$highlight_dti))
    
    ## Rendering precision plot: ------------------------------------------
    
    output$repPlotLegend2 <- renderUI({
      req(vals$which_question, input$ci,
          vals$tau_p0, vals$tau_v0, vals$dur, vals$dti)
      
      input_taup <- "days" %#% 
        vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      input_dti <- vals$dti$value %#% vals$dti$unit
      
      dt_hr <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      
      out_taup <- dt_hr$tau_p[which.min(abs(dt_hr$tau_p - input_taup))]
      dur_for_hr <- dt_hr$dur[which.min(abs(dt_hr$dur - input_dur))]
      
      dt_sd <- movedesign::sims_speed[[1]] %>%
        dplyr::mutate(dur = round(input_dur, 0)) %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      
      out_dti <- dt_sd$dti[which.min(abs(dt_sd$dti - input_dti))]
      dti_for_sd <- dt_sd$dti_notes[match(out_dti, dt_sd$dti)]
      dti_for_sd
      
      if (length(vals$which_question) > 1) {
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
          vals$which_question,
          "Home range" = {
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "This plot shows the expected error",
              "based on 400 simulations for a sampling duration of", 
              dur_for_hr, "days,", "with the medians",
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
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "This plot shows the expected error",
              "based on 400 simulations for a sampling interval of", 
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
                      vals$which_question, "."))
        )
      }
      
      ui <- span(class = "help-block", ui)
      
      return(ui)
      
    }) # end of renderUI, "repPlotLegend2"
    
    output$repPlot_precision <- ggiraph::renderGirafe({
      req(vals$hr_HDI, vals$sd_HDI)
      
      is_both <- FALSE
      if (!is.null(vals$which_question))
        if (length(vals$which_question) > 1)
        is_both <- TRUE
      
      is_dur <- FALSE
      if (!is.null(input$highlight_dur)) {
        if (!is.na(as.numeric(input$highlight_dur))) 
          is_dur <- TRUE
      }
      
      is_dti <- FALSE
      if (!is.null(input$highlight_dti)) {
        if (input$highlight_dti != "")
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
        req(vals$report$ds2_hr, vals$hr_HDI_new)
        
        details <- details %>%
          dplyr::add_row(
            question = "Home range",
            group = "est_new",
            type = "hr_est_new",
            value = vals$report$ds2_hr[["median"]],
            lci = vals$hr_HDI_new$CI_low,
            uci = vals$hr_HDI_new$CI_high,
            label = paste0("AKDE error for ",
                           input$highlight_dur, " days"),
            fill = pal$mdn,
            col = pal$mdn,
            linetype = "solid",
            shape = 18)
        
        girafe_height <- 2.5
        details_length <- details_length + 1
        xmin <- min(xmin, vals$report$ds2_hr[["min"]])
        xmax <- max(xmax, vals$report$ds2_hr[["max"]])
      }
      
      if (is_dti && !is_both) {
        req(vals$report$ds2_sd, vals$sd_HDI_new)
        
        details <- details %>%
          dplyr::add_row(
            question = "Speed & distance",
            group = "est_new",
            type = "sd_est_new",
            value = vals$report$ds2_sd[["median"]],
            lci = vals$sd_HDI_new$CI_low,
            uci = vals$sd_HDI_new$CI_high,
            label = paste0("CTSD error for ", 
                           vals$report$txt_dti_new),
            fill = pal$mdn,
            col = pal$mdn,
            linetype = "solid",
            shape = 18)
        
        girafe_height <- 2.5
        details_length <- details_length + 1
        xmin <- min(xmin, vals$report$ds2_sd[["min"]])
        xmax <- max(xmax, vals$report$ds2_sd[["max"]])
      }
      
      if ("Home range" %in% vals$which_question) {
        req(vals$report$ds1_hr[["done"]])
        
        details <- details %>%
          dplyr::add_row(
            question = "Home range",
            group = "est",
            type = "hr_est",
            value = vals$report$ds1_hr[["median"]],
            lci = vals$hr_HDI$CI_low,
            uci = vals$hr_HDI$CI_high,
            label = paste0("AKDE error for ",
                           vals$report$dur_for_hr, " days"),
            fill = pal$sea,
            col = pal$sea,
            linetype = "solid",
            shape = 18)
        
        if (!is.null(vals$simList)) {
          if (length(vals$simList) == 1) {
            err <- vals$hrErr$value
            err[[1]] <- NA
            err[[3]] <- NA
          } else {
            ci <- ifelse(is.null(input$ci), .95, input$ci/100)
            err <- suppressWarnings(
              bayestestR::ci(vals$hrErrList$est,
                             ci = ci, method = "HDI"))
            err <- data.frame(
              lci = err$CI_low,
              mean = mean(vals$hrErrList$est, na.rm = TRUE),
              uci = err$CI_high)
          }
        }
        
        input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
        details <- details %>%
          dplyr::add_row(
            question = "Home range",
            group = "now",
            type = "hr_now",
            value = ifelse(!is.null(vals$hrErr), err[[2]], NA),
            lci = ifelse(!is.null(vals$hrErr), err[[1]], NA),
            uci = ifelse(!is.null(vals$hrErr), err[[3]], NA),
            label = paste0("AKDE error for ", 
                           round(input_dur, 1), " days"),
            fill = pal$sea_d,
            col = pal$sea_d,
            linetype = "dashed",
            shape = 19)
        
        details_length <- details_length + 2
        xmin <- vals$report$ds1_hr[["min"]]
        xmax <- vals$report$ds1_hr[["max"]]
        
      } # end of hr
      
      if ("Speed & distance" %in% vals$which_question) {
        req(vals$report$ds1_sd[["done"]])
        
        details <- details %>%
          dplyr::add_row(
            question = "Speed & distance",
            group = "est",
            type = "sd_est",
            value = vals$report$ds1_sd[["median"]],
            lci = vals$sd_HDI$CI_low,
            uci = vals$sd_HDI$CI_high,
            label = paste0("CTSD error for ",
                           vals$report$txt_dti),
            fill = ifelse(is_both, pal$grn, pal$sea),
            col = ifelse(is_both, pal$grn, pal$sea),
            linetype = "solid",
            shape = 18)
        
        if (!is.null(vals$simList)) {
          if (length(vals$simList) == 1) {
            err <- vals$speedErr$value
            err[[1]] <- NA
            err[[3]] <- NA
          } else {
            ci <- ifelse(is.null(input$ci), .95, input$ci/100)
            err <- suppressWarnings(
              bayestestR::ci(vals$speedErrList$est,
                             ci = ci, method = "HDI"))
            err <- data.frame(
              lci = err$CI_low,
              mean = mean(vals$speedErrList$est, na.rm = TRUE),
              uci = err$CI_high)
          }
        }
        
        input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
        input_dti <- vals$dti$value %#% vals$dti$unit
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
            value = ifelse(!is.null(vals$speedErr), err[[2]], NA),
            lci = ifelse(!is.null(vals$speedErr), err[[1]], NA),
            uci = ifelse(!is.null(vals$speedErr), err[[3]], NA),
            label = paste0("CTSD error for ", input_dti),
            fill = ifelse(is_both, pal$grn_d, pal$sea_d),
            col = ifelse(is_both, pal$grn_d, pal$sea_d),
            linetype = "dashed",
            shape = 19)
        
        details_length <- details_length + 2
        xmin <- min(xmin, vals$report$ds1_sd[["min"]])
        xmax <- max(xmax, vals$report$ds1_sd[["max"]])
        
      } # end of sd
      
      details <- details %>%
        dplyr::filter(!is.na(.data$value)) %>% 
        dplyr::mutate(group = factor(group,
                                     levels = c("est_new",
                                                "est", 
                                                "now"))) %>% 
        droplevels()
      
      if (is_both) {
        add_val <- ifelse(is.null(vals$speedErr$value), 1, 0)
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
          size = 6) +
        
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
        
        theme_movedesign() +
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
      req(vals$which_question, 
          input$ci, vals$tau_p0, vals$tau_v0, vals$dur, vals$dti)
      req(length(vals$which_question) == 1)
      
      input_taup <- "days" %#%
        vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      input_dti <- vals$dti$value %#% vals$dti$unit
      
      switch(
        vals$which_question,
        "Home range" = {
          
          dt_hr <- movedesign::sims_hrange[[1]] %>%
            dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
            dplyr::mutate(duration = round("days" %#% duration, 1))
          max_dur <- max(dt_hr$duration)
          
          ui <- tagList(
            fontawesome::fa("circle-exclamation", fill = pal$dgr),
            span("Note:", class = "help-block-note"), 
            "This plot shows only the mean expected errors and",
            "(confidence intervals) simulated for different sampling",
            "durations (from 1 day to", wrap_none(max_dur, " days)"),
            "These are based on aggregated information from",
            "up to 400 simulations, so mean values will not match",
            "your current values.", br(),
            "As sampling duration increases, we will expect lower",
            "estimate errors (points) and",
            "narrower confidence intervals (shaded area).")
          
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
            "(confidence intervals) simulated for different sampling",
            "durations (from 1 day to", wrap_none(max_dur, " days)"),
            "These are based on aggregated information from",
            "up to 400 simulations, so mean values will not match",
            "your current values.", br(),
            "As sampling duration increases, we will expect lower",
            "estimate errors (points) and",
            "lower uncertainty (shaded area).")
          
        },
        stop(paste0("No handler for ",
                    vals$which_question, "."))
      )
      
      ui <- span(class = "help-block", ui)
      
      return(ui)
      
    }) # end of renderUI, "repPlotLegend3"
    
    output$repPlot_comp_hr <- ggiraph::renderGirafe({
      req(vals$hrErr)
      
      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")
      
      input_taup <- "days" %#% 
        vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      
      dat <- movedesign::sims_hrange[[2]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))
      dat$id <- 1:nrow(dat)
      
      if (input$highlight_dur > 0) {
        dur_NEW <- as.numeric(input$highlight_dur)
        is_highlight <- TRUE
      } else {
        is_highlight <- NULL
      }
      
      index_taup <- which.min(abs(dat$tau_p - input_taup))
      filtering <- dat$tau_p[index_taup]
      dat_filtered <- dat %>% dplyr::filter(tau_p == filtering)
      index_dur <- which.min(abs(dat_filtered$duration - input_dur))
      selected <- dat_filtered$id[index_dur]
      
      pd <- ggplot2::position_dodge(width = 0.6)
        
      if (input$highlight_dur > 0) {
        
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
          data = dat_filtered[index_dur,],
          ggplot2::aes_string(x = "duration",
                              xend = "duration",
                              y = "error_lci",
                              yend = "error_uci"),
          col = pal$sea,
          linetype = "solid",
          size = 1.5, alpha = .8) +
        
        ggplot2::geom_point(
          data = dat_filtered[index_dur,],
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
        
        { if (input$highlight_dur > 0) p1 } +
        { if (input$highlight_dur > 0) p2 } +
        
        theme_movedesign(ft_size = vals$ft_size) +
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
      req(vals$speedErr$value)
      
      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")
      
      input_tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      input_dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      input_dti <- vals$dti$value %#% vals$dti$unit
      
      reveal_if <- FALSE
      if (!is.null(input$highlight_dti)) {
        if (input$highlight_dti != "") reveal_if <- TRUE
      }
        
      sims <- movedesign::sims_speed[[2]]
      dat <- sims %>%
        dplyr::mutate(dur = round("days" %#% dur, 0))
      dat$id <- 1:nrow(dat)
      
      opts <- sims %>%
        dplyr::select(dti, dti_notes) %>%
        unique()
        
      if (reveal_if) {
        dti_new <- opts$dti[match(input$highlight_dti,
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
      selected <- dat_filtered$id[index_dti]
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
        theme_movedesign() +
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
      req(vals$which_question)
      
      if ("Home range" %in% vals$which_question) {
        out <- out_hr <- ggiraph::girafeOutput(
          outputId = ns("repPlot_comp_hr"),
          width = "100%", height = "100%")
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        out <- out_sd <- ggiraph::girafeOutput(
          outputId = ns("repPlot_comp_sd"),
          width = "100%", height = "100%")
      }
      
      # if (length(vals$which_question) > 1) {
      #   out <- tagList(out_hr, out_sd)
      # }
      
      return(out)
      
    }) # end of renderUI // reportPlots_error
    
    # TABLES --------------------------------------------------------------
    ## Final report table (combining previous results): -------------------
    
    reportRow <- reactive({
      req(vals$simList)
      n_sims <- length(vals$simList)
      
      dt_regs <- vals$dev$tbl
      dt_regs <- dt_regs %>% dplyr::slice_tail(n = n_sims)
      
      if ("Home range" %in% vals$which_question) {
        req(vals$hr$tbl)
        dt_hr <- vals$hr$tbl
        dt_hr <- dt_hr %>% dplyr::filter(data == "Initial") 
        dt_hr <- dt_hr %>% dplyr::slice_tail(n = n_sims)
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        req(vals$sd$tbl)
        dt_sd <- vals$sd$tbl
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
        scales::label_comma(accuracy = .1)(vals$tau_p0$value[2]),
        abbrv_unit(vals$tau_p0$unit[2]))
      
      if (!is.null(vals$tau_v0)) {
        tmpdat$tauv <- paste(
          scales::label_comma(accuracy = .1)(vals$tau_v0$value[2]),
          abbrv_unit(vals$tau_v0$unit[2]))
      }
      
      out <- fix_unit(vals$sigma0$value[2],
                      vals$sigma0$unit[2], convert = TRUE)
      tmpdat$sigma <- paste(out$value, abbrv_unit(out$unit))
      
      if ("Home range" %in% vals$which_question) {
        tmphr <- dt_hr %>% dplyr::select(
          c(.data$seed, .data$taup:.data$area_err_max))
        tmpdat <- suppressMessages(
          tmpdat %>% dplyr::group_by(seed) %>% 
            dplyr::full_join(tmphr))
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        tmpsd <- dt_sd %>% dplyr::select(
          c(.data$seed, .data$tauv:.data$dist_err))
        tmpdat <- suppressMessages(
          tmpdat %>% dplyr::group_by(seed) %>% 
            dplyr::full_join(tmpsd))
      }
      
      if (length(vals$simList) == 1) {
        if (nrow(tmpdat == 2))
          tmpdat <- dplyr::coalesce(tmpdat[1,], tmpdat[2,])
      }
      
      return(tmpdat)
      
    }) # end of reactive
    
    observe({
      req(vals$active_tab == 'report', 
          vals$dev$tbl)
      
      # vals$report_full <<- rbind(vals$report_full, reportRow())
      ## issue with number of columns of arguments,
      ## (and may not be necessary).
      
      vals$report_full <- reportRow()
      dat <- vals$report_full
      
      if (length(vals$simList) == 1) {
        if (nrow(dat) >= 2) {
          if (all(dat[nrow(dat) - 1,3:7] == dat[nrow(dat), 3:7]))
            dat <- dplyr::coalesce(dat[1, ], dat[2, ])
        }
      }
      
      vals$report_full <- dplyr::distinct(dat)
      set.seed(NULL)
      
    }) %>% # end of observe
      bindEvent(input$build_report)
    
    
    output$endTable <- reactable::renderReactable({
      req(vals$report_full,
          vals$is_analyses, 
          vals$which_question)
      
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
      
      if (length(vals$which_question) == 1) {
        if (vals$which_question == "Home range")
          choices_subset <- choices[c(1:8, 10:13)]
        
        if (vals$which_question == "Speed & distance")
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
      
      dat <- vals$report_full[, -1]
      
      if (!is.null(choices_subset)) {
        dat <- dat %>% dplyr::select(choices_subset)
      }
      
      if ("Home range" %in% vals$which_question) {
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
      
      if ("Speed & distance" %in% vals$which_question) {
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
      
      if (length(vals$which_question) == 2) {
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
            name = nms[1, "taup"],
            style = list(fontWeight = "bold")) },
        tauv = if ("tauv" %in% choices_subset) {
          reactable::colDef(
            name = nms[1, "tauv"],
            style = list(fontWeight = "bold")) },
        sigma = if ("sigma" %in% choices_subset) {
          reactable::colDef(
            minWidth = 60, name = nms[1, "sigma"]) },
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
            minWidth = 80, name = nms[1, "area"]) },
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
            minWidth = 80, name = nms[1, "ctsd"]) },
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
            minWidth = 80, name = nms[1, "dist"]) },
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
    ## Species: -----------------------------------------------------------
    
    observe({
      req(vals$tau_p0)
      
      mod_blocks_server(
        id = "repBlock_taup", 
        vals = vals, data = vals$data0, fit = vals$fit0,
        type = "tau", name = "tau_p0",
        input_name = list(
          html = wrap_none("Position autocorrelation ",
                           "(\u03C4", tags$sub("p"), ")")))
      
    }) # end of observe
    
    observe({
      req(vals$tau_v0)
      
      mod_blocks_server(
        id = "repBlock_tauv", 
        vals = vals, data = vals$data0, fit = vals$fit0,
        type = "tau", name = "tau_v0",
        input_name = list(
          html = wrap_none("Velocity autocorrelation ",
                           "(\u03C4", tags$sub("v"), ")")))
      
    }) # end of observe
    
    observe({
      req(vals$sigma0)
      
      mod_blocks_server(
        id = "repBlock_sigma",
        vals = vals,
        type = "sigma", name = "sigma0",
        input_name = list(
          html = wrap_none("Location variance ",
                           "(\u03C3", tags$sub("p"), ")")))
      
    }) # end of observe
    
    ## Tracking regime: ---------------------------------------------------
    
    output$repBlock_dur <- renderUI({
      req(vals$dur)
      
      out <- fix_unit(vals$dur, convert = TRUE)
      
      if (grepl("month", out$unit)) {
        out_new <- convert_to(out, new_unit = "days", to_text = TRUE)
        txt <- paste0("(or ", out_new, ")")
      } else if (grepl("year", out$unit)) {
        out_new <- convert_to(out, new_unit = "months", to_text = TRUE)
        txt <- paste0("(or ", out_new, ")")
      } else { 
        txt <- NULL 
      }
      
      parBlock(
        header = "Sampling duration",
        value = paste(out[1], out[2]),
        subtitle = txt)
      
    }) # end of renderUI, "repBlock_dur"
    
    output$repBlock_dti <- renderUI({
      req(vals$data1)
      
      dti <- extract_pars(vals$data1, name = "interval")
      out <- fix_unit(dti, convert = TRUE)
      
      parBlock(
        header = "Sampling interval",
        value = paste(out[1], out[2]),
        subtitle = "between fixes")
      
    }) # end of renderUI // repBlock_dti
    
    ## Sample sizes: ------------------------------------------------------
    
    output$repUI_sizes <- renderUI({
      req(vals$dev$is_valid)
      
      if (is.null(vals$which_question) ||
          length(vals$which_question) > 1) {
        out <- tagList(
          uiOutput(ns("repBlock_n")),
          splitLayout(
            mod_blocks_ui(ns("repBlock_Narea")),
            mod_blocks_ui(ns("repBlock_Nspeed"))
          ))
      }
      
      if (length(vals$which_question) == 1 &&
          "Home range" %in% vals$which_question) {
        out <- splitLayout(
          uiOutput(ns("repBlock_n")),
          mod_blocks_ui(ns("repBlock_Narea"))) }
      
      if (length(vals$which_question) == 1 &&
          "Speed & distance" %in% vals$which_question) {
        out <- splitLayout(
          uiOutput(ns("repBlock_n")),
          mod_blocks_ui(ns("repBlock_Nspeed"))) }
      
      return(out)
    }) # end of renderUI // repUI_sizes
    
    output$repBlock_n <- renderUI({
      req(vals$data1)
      
      n <- nrow(vals$data1)
      
      number_n <- ""
      icon_n <- FALSE
      
      if (!is.null(vals$lost)) {
        if (vals$lost$n > 0) {
          number_n <- paste0(vals$lost$perc, "%")
          icon_n <- TRUE
        }
      }
      
      sampleBlock(
        number = number_n,
        numberIcon = icon_n,
        header = n,
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)
      
    }) # end of renderUI // repBlock_n
    
    observe({
      req(vals$fit1)
      
      mod_blocks_server(
        id = "repBlock_Narea", 
        vals = vals, data = vals$data1, fit = vals$fit1,
        type = "N", name = "area")
      
    }) # end of observe
    
    observe({
      req(vals$fit1)
      
      mod_blocks_server(
        id = "repBlock_Nspeed", 
        vals = vals, data = vals$data1, fit = vals$fit1,
        type = "N", name = "speed")
      
    }) # end of observe
    
    ## Outputs: -----------------------------------------------------------
    
    observe({
      req(vals$simList,
          vals$hrErr, "Home range" %in% vals$which_question)
      
      shinyjs::show(id = "repBox_hr_err")
      
      if (length(vals$simList) == 1) {
        err_to_show <- "hrErr"
      } else {
        req(nrow(vals$hrEstList) == length(vals$simList))
        err_to_show <- "hrErrList"
      }
      
      mod_blocks_server(
        id = "repBlock_hrErr",
        vals = vals, type = "hr", name = err_to_show)    
      
    }) # end of observe
    
    observe({
      req(vals$simList,
          vals$speedErr, "Speed & distance" %in% vals$which_question)
      
      shinyjs::show(id = "repBox_speed_err")
      
      if (length(vals$simList) == 1) {
        err_to_show <- "speedErr"
      } else {
        req(nrow(vals$speedEstList) == length(vals$simList))
        err_to_show <- "speedErrList"
      }
      
      mod_blocks_server(
        id = "repBlock_speedErr",
        vals = vals, type = "ctsd", name = err_to_show)    
      
    }) # end of observe
    
    observe({
      req(vals$simList,
          vals$distErr, "Speed & distance" %in% vals$which_question)
      
      shinyjs::show(id = "repBox_dist_err")
      
      if (length(vals$simList) == 1) {
        err_to_show <- "distErr"
      } else {
        req(nrow(vals$distEstList) == length(vals$simList))
        err_to_show <- "distErrList"
      }
      
      mod_blocks_server(
        id = "repBlock_distErr",
        vals = vals, type = "ctsd", name = err_to_show)
      
    }) # end of observe
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_report_ui("tab_report_1")

## To be copied in the server
# mod_tab_report_server("tab_report_1")
