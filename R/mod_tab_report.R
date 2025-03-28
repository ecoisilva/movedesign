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
                           value = ns("repPanel_schedule"),
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
            title = span("General report:", class = "ttl-tab"),
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
                    
                ) # end of div [section-comparison]
                
            ) # end of div
          ), # end of box, "repBox_analyses"
          
          shinydashboardPlus::box(
            title = span("Meta-analyses report:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "box-archive",
                                   height = "21px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "var(--sea-dark)"),
            id = ns("repBox_meta"),
            width = NULL,
            headerBorder = FALSE,
            
            div(class = "col-report-left no-padding-left",
                shinyWidgets::radioGroupButtons(
                  inputId = ns("reportInput_target"),
                  width = "100%",
                  label = "Show outputs for:",
                  choices = c(
                    "Home range" = "hr",
                    "Speed & distance" = "ctsd"),
                  selected = "hr",
                  checkIcon = list(yes = icon("circle-check")),
                  justified = TRUE),
                mod_viz_meta_ui("viz_meta_2")),
            
            div(class = "col-report-right no-padding-right",
                uiOutput(ns("end_meta")))
            
          ) # end of box, "repBox_meta"
          
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
    
    observe({
      req(rv$which_question)
      
      if (length(rv$which_question) == 1) {
        shinyjs::hide(id = "reportInput_target")
      } else {
        rv$set_analysis <- "hr"
        shinyjs::show(id = "reportInput_target")
      }
      
    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    observe({
      rv$set_analysis <- input$reportInput_target
    }) %>% bindEvent(input$reportInput_target)
    
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
      rv$report$schedule <- NULL
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
        p(style = "margin-top: 5px; margin-bottom: 0px"),
        span("Location error:", class = "txt-label"), ui_error,
        p(style = "margin-top: 5px; margin-bottom: 0px"),
        span("Storage limit:", class = "txt-label"), ui_limit)
      
      return(out)
      
    }) # end of renderUI, "report_device"
    
    ## Rendering if uncertainty was propagated or not: --------------------
    
    output$report_emulate <- renderUI({
      
      ui <- staticBlock("Without individual varation", active = FALSE)
      
      if (req(rv$is_emulate)) {
        ui <- staticBlock("With individual varation", active = TRUE)
      }
      
      out_ui <- tagList(ui)
      return(out_ui)
      
    }) # end of renderUI, "report_emulate"
    
    ## Rendering schedule comparison inputs: ------------------------------
    
    output$highlighting_reg <- renderUI({
      req(rv$which_question, rv$which_meta)
      req(rv$which_meta == "none")
      
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
          dplyr::mutate(tau_p = round("days" %#% .data$tau_p, 1)) %>%
          dplyr::mutate(duration = round("days" %#% .data$duration, 1))
        
        out_taup <- dat$tau_p[which.min(abs(dat$tau_p - input_taup))]
        out_dur <- dat$dur[which.min(abs(dat$dur - input_dur))]
        
        newdat <- dat %>%
          dplyr::filter(.data$tau_p == out_taup) %>%
          dplyr::filter(.data$duration == out_dur)
        
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
      
      rv$hr_coi <- data.frame(
        lci = min(newdat$error, na.rm = TRUE),
        est = mean(newdat$error, na.rm = TRUE),
        uci = max(newdat$error, na.rm = TRUE),
        ci = ci)
      
      # Credible intervals:
      rv$hr_cri <- .extract_cri(newdat$error, ci)
      
    }) # end of observe
    
    observe({ # For comparison with new duration:
      req(rv$highlight_dur > 0)
      shinyjs::show(id = "end_comparison")
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      
      input_taup <- "days" %#% rv$tau_p[[1]]$value[2] %#%
        rv$tau_p[[1]]$unit[2]
      
      dat <- movedesign::sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% .data$tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% .data$duration, 1))
      
      out_taup <- dat$tau_p[which.min(abs(dat$tau_p - input_taup))]
      out_dur <- as.numeric(rv$highlight_dur)
      
      newdat <- dat %>%
        dplyr::filter(.data$tau_p == out_taup) %>%
        dplyr::filter(.data$duration == out_dur)
      
      rv$hr_coi_new <- data.frame(
        lci = mean(newdat$error_lci, na.rm = TRUE),
        est = mean(newdat$error, na.rm = TRUE),
        uci = mean(newdat$error_uci, na.rm = TRUE),
        ci = ci)
      
      # Credible intervals:
      rv$hr_cri_new <- .extract_cri(newdat$error, ci)
      
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
          dplyr::mutate(dur = round("days" %#% .data$dur, 0))
        
        out_tauv <- dat$tau_v[which.min(abs(dat$tau_v - input_tauv))]
        out_dti <- dat$dti[which.min(abs(dat$dti - input_dti))]
        out_dur <- dat$dur[which.min(abs(dat$dur - input_dur))]
        
        newdat <- dat %>%
          dplyr::filter(.data$tau_v == out_tauv) %>%
          dplyr::filter(.data$dur == out_dur) %>%
          dplyr::filter(.data$dti == out_dti)
        
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
      
      rv$sd_coi <- data.frame(
        lci = mean(newdat$error_lci, na.rm = TRUE),
        est = mean(newdat$error, na.rm = TRUE),
        uci = mean(newdat$error_uci, na.rm = TRUE),
        ci = ci)
      
      # Credible intervals:
      rv$sd_cri <- .extract_cri(newdat$error, ci)
      
    }) # end of observe
    
    observe({ # For comparison with new interval:
      req(rv$highlight_dti > 0)
      shinyjs::show(id = "end_comparison")
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      
      input_tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
      
      dat <- movedesign::sims_speed[[1]]
      
      out_tauv <- dat$tau_v[which.min(abs(dat$tau_v - input_tauv))]
      opts <- movedesign::sims_speed[[1]] %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      out_dti <- fix_unit(
        opts$dti[match(rv$highlight_dti, opts$dti_notes)], "seconds")
      
      newdat <- dat %>%
        dplyr::filter(.data$tau_v == out_tauv) %>%
        dplyr::filter(.data$dti == out_dti$value)
      
      rv$sd_coi_new <- data.frame(
        lci = mean(newdat$error_lci, na.rm = TRUE),
        est = mean(newdat$error, na.rm = TRUE),
        uci = mean(newdat$error_uci, na.rm = TRUE),
        ci = ci)
      
      # Credible intervals:
      rv$sd_cri_new <- .extract_cri(newdat$error, ci)
      
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
      
      css_bold <- "font-weight: bold;"
      css_mono <- "font-family: var(--monosans);"
      
      if (length(rv$id) == 1) {
        out_id <- span(rv$id, class = "cl-grn")
      } else {
        out_id <- span(
          "multiple individuals",
          wrap_none("(", span(toString(rv$id),
                              class = "cl-grn"), ")"))
      }
      
      switch(rv$data_type,
             "selected" = {
               out_species <- span(
                 "These outputs are based on parameters",
                 "extracted from", out_id, "of the species", 
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
          style = paste(css_mono, css_bold),
          "However, due to ",
          span("very low effective sample sizes", class = "cl-dgr"),
          "of the", rv$data_type, "data, these parameters may not be",
          "accurate, and lead to negatively biased outputs.")
      }
      
      rv$report$species <- p(
        out_species, 
        span(style = paste(css_mono, css_bold),
             "Please see",
             icon("paw", class = "cl-sea"),
             span("Species", class = "cl-sea"),
             "parameters for more details."),
        out_bias)
      
    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    ### Reporting DESIGN: -------------------------------------------------
    
    observe({
      req(rv$which_question,
          rv$data_type,
          rv$is_analyses,
          rv$simList)
      
      N1 <- N2 <- NULL
      tau_p <- tau_p <- NULL
      tau_v <- tau_v <- NULL
      dur <- dur <- NULL
      dti <- dti <- NULL
      dur_unit <- dur_unit <- NULL
      dti_unit <- dti_unit <- NULL
      ideal_dti <- ideal_dti <- NULL
      ideal_dur <- ideal_dur <- NULL
      
      if ("Home range" %in% rv$which_question) req(rv$hr_completed)
      if ("Speed & distance" %in% rv$which_question) req(rv$sd_completed)
      
      pars <- .build_parameters(rv)
      list2env(pars, envir = environment())
      
      rv$hr_col <- rv$ctsd_col <- data.frame(
        hex = pal$sea,
        css = "cl-sea")
      
      if (dur$value <= ideal_dur$value) {
        rv$hr_col$hex <- pal$dgr
        rv$hr_col$css <- "cl-dgr"
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
          rv$ctsd_col$css <- "cl-dgr"
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
        req(rv$hr_cri)
        
        out_schedule <- out_reg_hr <-
          p("The minimum", span("sampling duration", class = "cl-sea"),
            "recommended for", span("home range", class = "cl-grn"),
            "estimation should be at least 30", icon(name = "xmark"),
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
        req(rv$sd_cri)
        
        out_schedule <- out_reg_ctsd <-
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
        out_schedule <- tagList(out_reg_hr, out_reg_ctsd)
      
      rv$report$schedule <- out_schedule
      
    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    ### Reporting OUTPUTS: ------------------------------------------------
    
    observe({
      req(rv$which_question,
          rv$data_type,
          rv$is_analyses,
          rv$simList)
      if (rv$which_meta != "none") req(rv$meta_tbl)
      
      N1 <- N2 <- NULL
      tau_p <- tau_p <- NULL
      tau_v <- tau_v <- NULL
      dur <- dur <- NULL
      dti <- dti <- NULL
      dur_unit <- dur_unit <- NULL
      dti_unit <- dti_unit <- NULL
      ideal_dti <- ideal_dti <- NULL
      ideal_dur <- ideal_dur <- NULL
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      
      if ("Home range" %in% rv$which_question) {
        req(rv$hr_cri, rv$hrErr)
      }
      if ("Speed & distance" %in% rv$which_question) {
        req(!is.null(rv$is_ctsd))
        if (rv$is_ctsd) req(rv$sd_cri, rv$speedErr)
      }
      
      css_bold <- "font-weight: bold;"
      css_mono <- "font-family: var(--monosans);"
      
      #### Initialize:
      
      out_analyses <- NULL
      rv$report$analyses <- NULL
      
      if ("Home range" %in% rv$which_question) 
        req(rv$hrEst, rv$hr_completed)
      if ("Speed & distance" %in% rv$which_question)
        req(rv$speedEst, rv$distEst, rv$sd_completed)
      
      pars <- .build_parameters(rv)
      list2env(pars, envir = environment())
      
      if (rv$which_meta == "none") {
        txt_single <- span(
          style = paste(css_mono, css_bold),
          "To obtain credible intervals from multiple",
          "simulations, select a different",
          span("analytical target", class = "cl-sea"),
          "(mean of sampled population, or comparing population means)",
          "in the", icon("house", class = "cl-mdn"),
          span("Home", class = "cl-mdn"), "tab.")
        
      } else {
        txt_meta_no_ci <- span(
          "The number of simulations was insufficient so credible",
          "intervals (CIs) could", span("not", class = "cl-dgr"),
          "be calculated.",
          span(
            style = paste(css_mono, css_bold),
            "Please run more simulations in the corresponding",
            shiny::icon("compass-drafting", class = "cl-sea"),
            span("Analyses", class = "cl-sea"), "tab(s) to obtain",
            wrap_none(span("valid CIs", class = "cl-dgr"), ".")))
      }
      
      #### Home range estimation:
      
      if ("Home range" %in% rv$which_question) {
        
        hr_cri <- c(.err_to_txt(rv$hr_cri$lci),
                    .err_to_txt(rv$hr_cri$est),
                    .err_to_txt(rv$hr_cri$uci))
        
        txt_hr_uncertainty <- "estimation."
        if (!is.na(rv$hr_cri$lci) && !is.na(rv$hr_cri$uci))
          txt_hr_uncertainty <- ifelse(
            rv$hr_cri$uci < .3 & rv$hr_cri$lci > -.3,
            "estimation, and with low uncertainty.", 
            "estimation, but with high uncertainty.")
        
        opts_dur <- 2^seq(1, 12, by = 1)
        plot_dur <- opts_dur[which.min(abs(
          opts_dur - ("days" %#% dur$value %#% dur$unit)))]
        
        if (dur$value >= ideal_dur$value) {
          rv$report$is_hr <- TRUE
          txt_hr <- span(
            style = css_bold,
            "Your current sampling duration appears sufficient",
            "for home range", txt_hr_uncertainty)
          
        } else {
          rv$report$is_hr <- FALSE
          txt_hr <- span(
            style = css_bold,
            "Your current sampling duration may be insufficient",
            "for home range estimation.")
        }
        
        txt_hr_extra <- NULL
        if (rv$which_meta == "none") txt_hr_extra <- txt_single
        else {
          
          if (!is.na(hr_cri[1]) && !is.na(hr_cri[3])) {
            txt_hr_extra <- span(
              "There is a", wrap_none(ci, "%", css = "cl-blk"),
              "probability the relative error will lie between",
              wrap_none(hr_cri[1], "%", css = "cl-blk"),
              "and", wrap_none(hr_cri[3], "%", end = ".", css = "cl-blk"))
          } else txt_hr_extra <- txt_meta_no_ci
        }
        
        txt_nsim <- .get_txt_nsim(rv, set_target = "hr")
        
        out_analyses <- out_hr <- p(
          txt_hr,
          txt_nsim,
          txt_hr_extra)
        
      } # end of 'Home range'
      
      ## Speed and distance estimation:
      
      if ("Speed & distance" %in% rv$which_question) {
        
        if (rv$is_ctsd) {
          
          sd_cri <- c(round(rv$sd_cri$lci * 100, 1),
                      round(rv$sd_cri$est * 100, 0),
                      round(rv$sd_cri$uci * 100, 1))
          
          txt_sd_uncertainty <- "estimation."
          if (!is.na(rv$sd_cri$lci) && !is.na(rv$sd_cri$uci))
            txt_sd_uncertainty <- ifelse(
              rv$sd_cri$uci < .3 & rv$sd_cri$lci > -.3,
              "estimation, and with low uncertainty.", 
              "estimation, but with high uncertainty.")
        }
        
        dti_options <- movedesign::sims_speed[[1]] %>%
          dplyr::select(.data$dti, .data$dti_notes) %>%
          unique()
        
        index_dti <- which.min(
          abs(dti_options$dti - (dti$value %#% dti$unit)))
        plotted_dti <- sub('^\\w+\\s\\w+\\s\\w+\\s', '',
                           dti_options[index_dti, 2])
        
        N2 <- rv$dev$N2
        if (is.list(rv$dev$N2)) N2 <- do.call(c, N2)
        N2 <- mean(N2, na.rm = TRUE)
        
        if (N2 >= 100) {
          rv$report$is_sd <- TRUE
          txt_sd <- span(
            style = paste(css_mono, css_bold),
            "Your current sampling interval appears sufficient",
            "for speed & distance", txt_sd_uncertainty)
        } else if (N2 >= 30) {
          rv$report$is_sd <- FALSE
          txt_sd <- span(
            style = paste(css_mono, css_bold),
            "Your current sampling interval may be sufficient",
            "for speed & distance estimation.")
        } else if (N2 > 0) {
          rv$report$is_sd <- FALSE
          txt_sd <- span(
            style = paste(css_mono, css_bold),
            "Your current sampling interval may be insufficient",
            "for speed & distance estimation.")
        } else {
          rv$report$is_sd <- FALSE
          txt_sd <- span(
            style = paste(css_mono, css_bold),
            "Your current sampling interval was too coarse",
            "for speed & distance estimation.")
        }
        
        if (rv$is_ctsd) {
          txt_sd_extra <- NULL
          
          if (rv$which_meta == "none") txt_sd_extra <- txt_single
          else {
            if (!is.na(sd_cri[1]) && !is.na(sd_cri[3]))
              txt_sd_extra <- span(
                "There is a", wrap_none(ci, "%", css = "cl-blk"),
                "probability that the relative error will lie within",
                wrap_none(sd_cri[1], "%", css = "cl-blk"), "and",
                wrap_none(sd_cri[3], "%", end = ".", css = "cl-blk"))
            else txt_sd_extra <- txt_meta_no_ci
          }
          
          txt_nsim <- .get_txt_nsim(rv, set_target = "ctsd")
          
          out_analyses <- p(
            txt_sd,
            txt_nsim,
            txt_sd_extra)
          
        } else {
          
          txt_sd_extra <- NULL
          if (is.na(hr_cri[1]) || 
              is.na(hr_cri[3])) txt_sd_extra <- txt_meta_no_ci
          
          out_analyses <- p(
            txt_sd, 
            txt_sd_extra)
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
      if (rv$which_meta != "none") {
        req(rv$metaErr, rv$meta_tbl) }

      #### Initialize:

      is_hr <- rv$report$is_hr
      is_sd <- rv$report$is_sd
      req(!is.null(is_hr), !is.null(is_sd))
      req(rv$hr_completed, rv$sd_completed)

      txt_hr_uncertainty <- NULL
      txt_sd_uncertainty <- NULL
      is_hr_ci <- FALSE
      is_sd_ci <- FALSE

      #### Styles:

      css_bold <- "font-weight: bold;"
      css_mono <- "font-family: var(--monosans);"

      #### For number of simulations:

      sufficient <- span("sufficient", class = "cl-sea")
      insufficient <- span("insufficient", class = "cl-dgr")

      if (length(rv$simList) > 1) {
        txt_nsim <- span(length(rv$simList), "simulations",
                          style = css_bold)
      } else {
        txt_nsim <- span("a", span("single", style = css_bold),
                          "simulation")
      }

      #### For home range estimation:

      if (rv$which_meta == "none") {
        hrErr_lci <- .err_to_txt(rv[["hrErr"]]$lci)
        hrErr_est <- .err_to_txt(rv[["hrErr"]]$est)
        hrErr_uci <- .err_to_txt(rv[["hrErr"]]$uci)
      } else {
        tmp <- rv$meta_tbl %>%
          dplyr::filter(.data$group == "All") %>%
          dplyr::filter(.data$type == "hr") %>%
          dplyr::slice(which.max(.data$m))
        hrErr_lci <- tmp[["error_lci"]]
        hrErr_est <- tmp[["error"]]
        hrErr_uci <- tmp[["error_uci"]]
      }

      if (length(rv$simList) > 1) {
        hrErr_lci <- .err_to_txt(rv$hr_cri$lci)
        hrErr_uci <- .err_to_txt(rv$hr_cri$uci)

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
        sdErr_lci <- .err_to_txt(rv[["speedErr"]]$lci)
        sdErr_est <- .err_to_txt(rv[["speedErr"]]$est)
        sdErr_uci <- .err_to_txt(rv[["speedErr"]]$uci)

        if (length(rv$simList) > 1) {
          req(rv$sd_cri)
          sdErr_lci <- round(rv$sd_cri$lci * 100, 1)
          sdErr_uci <- round(rv$sd_cri$uci * 100, 1)

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

      if (is_hr & !is_sd) {
        out <- span(
          style = paste(css_mono, css_bold),
          
          "Your current sampling schedule appears",
          sufficient, "for home range",
          ifelse(is_hr_ci,
                 wrap_none("estimation ", txt_hr_uncertainty, ","),
                 "estimation,"),
          "but", insufficient, "for speed & distance",
          ifelse(is_sd_ci,
                 wrap_none("estimation ", txt_sd_uncertainty, "."),
                 "estimation."))

        if (any(rv$dev$N2 == 0))
          out <- span(
            style = paste(css_mono, css_bold),

            "Your current sampling schedule appears",
            sufficient, "for home range",
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

      if (!is_hr & is_sd) {
        out <- span(
          style = paste(css_mono, css_bold),

          "Your current sampling schedule may be",
          insufficient, "for home range",
          ifelse(is_hr_ci,
                 wrap_none("estimation ", txt_hr_uncertainty, ","),
                 "estimation,"),
          "but appears", sufficient, "for speed & distance",
          ifelse(is_sd_ci,
                 wrap_none("estimation ", txt_sd_uncertainty, "."),
                 "estimation."))
      }

      if (is_hr & is_sd) {
        out <- span(
          style = paste(css_mono, css_bold),

          "Your current sampling schedule appears",
          sufficient, "for both home range",
          ifelse(is_hr_ci,
                 wrap_none("estimation ", txt_hr_uncertainty, ","),
                 "estimation,"),
          "and for speed & distance",
          ifelse(is_sd_ci,
                 wrap_none("estimation ", txt_sd_uncertainty, "."),
                 "estimation."))
      }

      if (!is_hr & !is_sd) {
        out <- span(
          style = paste(css_mono, css_bold),

          "Your current sampling schedule may be",
          insufficient, "for both home range",
          ifelse(is_hr_ci,
                 wrap_none("estimation ", txt_hr_uncertainty, ","),
                 "estimation,"),
          "and for speed & distance",
          ifelse(is_sd_ci,
                 wrap_none("estimation ", txt_sd_uncertainty, "."),
                 "estimation."))

        if (any(rv$dev$N2 == 0))
          out <- span(
            style = paste(css_mono, css_bold),

            "Your current sampling schedule may be",
            insufficient, "for home range",
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

      if (rv$which_meta == "none") {
        if (is.na(hrErr_lci) || is.na(hrErr_uci)) {
          out_hr_err <- span(
            ifelse(hrErr_est == 0, "less than 0.01%",
                   paste0(round(hrErr_est * 100, 1), "%")),
            "for home range estimation,")
        } else {
          out_hr_err <- span(
            ifelse(hrErr_est == 0, "less than 0.01%",
                   paste0(hrErr_est, "%")),
            paste0("[", hrErr_lci,
                   ", ", hrErr_uci, "%]"),
            "for home range estimation,")
        }
        
        if (is.na(sdErr_lci) || is.na(sdErr_uci)) {
          out_sd_err <- span(
            "and", ifelse(sdErr_est == 0, "less than 0.01%",
                          paste0(sdErr_est, "%")),
            "for speed estimation.")
        } else {
          out_sd_err <- span(
            "and", ifelse(sdErr_est == 0, "less than 0.01%",
                          paste0(sdErr_est, "%")),
            paste0("[", sdErr_lci,
                   ", ", sdErr_uci, "%]"),
            "for speed estimation.")
        }
        
      } else  {
        hrmetaErr <- dplyr::filter(rv$metaErr, .data$type == "hr")
        sdmetaErr <- dplyr::filter(rv$metaErr, .data$type == "ctsd")
        
        if (is.na(hrmetaErr$lci) || is.na(hrmetaErr$uci)) {
          out_hr_err <- span(
            ifelse(hrmetaErr$est == 0, "less than 0.01%",
                   paste0(round(hrmetaErr$est * 100, 1), "%")),
            "for home range estimation,")
        } else {
          out_hr_err <- span(
            ifelse(hrmetaErr$est == 0, "less than 0.01%",
                   paste0(hrmetaErr$est, "%")),
            paste0("[", hrmetaErr$lci,
                   ", ", hrmetaErr$uci, "%]"),
            "for home range estimation,")
        }
        
        if (is.na(sdmetaErr$lci) || is.na(sdmetaErr$uci)) {
          out_sd_err <- span(
            "and", ifelse(sdmetaErr$est == 0, "less than 0.01%",
                          paste0(sdmetaErr$est, "%")),
            "for speed estimation.")
        } else {
          out_sd_err <- span(
            "and", ifelse(sdmetaErr$est == 0, "less than 0.01%",
                          paste0(sdmetaErr$est, "%")),
            paste0("[", sdmetaErr$lci,
                   ", ", sdmetaErr$uci, "%]"),
            "for speed estimation.")
        }
      }
      
      out_extra <- ""
      if (is.null(txt_hr_uncertainty)) {
        if (rv$which_meta == "none") {
          out_extra <- span(
            style = paste(css_mono, css_bold),
            "To obtain credible intervals from multiple",
            "simulations, select a different",
            span("analytical target", class = "cl-sea"),
            "in the", icon("house", class = "cl-mdn"),
            span("Home", class = "cl-mdn"), "tab.")
        } else {
          out_extra <- span(
            "The number of simulations was insufficient so credible",
            "intervals (CIs) could", span("not", class = "cl-dgr"),
            "be calculated.", 
            span(
              style = paste(css_mono, css_bold),
              "Please run more simulations in the corresponding",
              shiny::icon("compass-drafting", class = "cl-sea"),
              span("Analyses", class = "cl-sea"), "tab to obtain",
              wrap_none(span("valid CIs", class = "cl-dgr"), ".")))
        }
      }

      out_analyses <- p(
        out,
        "Your error estimate based on",
        txt_nsim, "was", out_hr_err,
        if (any(rv$dev$N2 > 0)) { out_sd_err
        } else { span("but the sampling interval was",
                      "too coarse to estimate speed.") },
        out_extra
      )

      rv$report$analyses <- out_analyses

    }) %>% # end of observe,
      bindEvent(input$build_report)
    
    ## Reporting META-ANALYSES: -------------------------------------------
    
    observe({
      req(rv$which_meta)
      
      if (rv$which_meta == "none") {
        shinyjs::hide(id = "end_meta")
      } else { shinyjs::show(id = "end_meta") }
      
    }) # end of observe
    
    output$end_meta <- renderUI({
      req(rv$which_meta != "none")
      req(rv$report$meta, !is.null(rv$grouped))
      
      div(id = "report_meta",
          style = paste("padding-left: 14px;",
                        "padding-right: 14px;"),
          rv$report$meta)
      
    }) # end of renderUI, "end_meta"
    
    observe({
      req(rv$active_tab == "report", 
          rv$which_meta != "none")
      rv$report$meta <- out_meta <- span("")
      
      set_target <- NULL
      txt_target <- NULL
      txt_title <- NULL
      get_truth <- NULL
      get_coi <- NULL
      get_cri <- NULL
      set_style_title <- NULL
      txt_link_meta <- NULL
      txt_ratio_order <- NULL
  
      if (rv$which_meta == "none") {
        req(!rv$grouped)
      }
      
      if (rv$which_meta == "compare") {
        req(rv$grouped, rv$groups)
        req(rv$metaErr, rv$metaList, rv$metaList_groups[["is_final"]])
      }
      
      req(!is.null(rv$is_emulate),
          rv$metaList)
      
      css_bold <- "font-weight: bold;"
      css_mono <- "font-family: var(--monosans);"
      
      list2env(.build_outputs(rv), envir = environment())
      
      i <- 0
      for (target in set_target) {
        i <- i + 1
        
        meta <- as.data.frame(rv$metaList[[target]]$meta)
        tmpunit <- extract_units(rownames(
          meta[grep("mean", rownames(meta)), ]))
        est <- meta[grep("mean", rownames(meta)), ]$est
        lci <- meta[grep("mean", rownames(meta)), ]$low
        uci <- meta[grep("mean", rownames(meta)), ]$high
        
        truth <- tmpunit %#% get_truth[[target]]
        
        meta_dt <- meta[1, ] %>% 
          dplyr::mutate(
            error_est = (.data$est - truth)/truth,
            error_lci = (.data$low - truth)/truth,
            error_uci = (.data$high - truth)/truth) %>% 
          dplyr::select(.data$error_est,
                        .data$error_lci,
                        .data$error_uci) %>% 
          dplyr::rowwise() %>%
          dplyr::mutate(
            within_threshold = 
              (.data$error_est >= -rv$error_threshold &
                 .data$error_est <= rv$error_threshold),
            overlaps_with_threshold = 
              (.data$error_lci <= rv$error_threshold & 
                 .data$error_uci >= -rv$error_threshold),
            status = dplyr::case_when(
              within_threshold ~ "Yes",
              !within_threshold & overlaps_with_threshold ~ "Near",
              TRUE ~ "No"))
        
        txt_meta_groups <- NULL
        if (rv$which_meta == "compare") {
          
          meta_group_truth <- rv$metaList_groups[["intro"]][[target]]
          meta_group <- rv$metaList_groups[["final"]][[target]]
          
          is_subpop <- meta_group_truth$logs$subpop_detected
          is_subpop_detected <- meta_group$logs$subpop_detected
          
          txt_subpop_cont <- if (is_subpop == is_subpop_detected) 
            "As expected, sub-populations were" else
              "However, sub-populations were"
          
          if (meta_group_truth$mods$subpop_detected[[2,2]] < 2) {
            txt_subpop <- span(
              "There was insufficient evidence in the", rv$data_type,
              "dataset to detect sub-populations.")
            txt_subpop_cont <- "With the simulations, sub-populations were"
          }  else if (is_subpop) {
            txt_subpop <- span(
              "We expected to detect a sub-population",
              "through meta_group-analyses.")
          } else {
            txt_subpop <- span(
              "We expected no sub-populations to be detected",
              "through meta_group-analyses.")
          }
          
          col_subpop <- dplyr::case_when(
            is_subpop == is_subpop_detected ~ "cl-sea-d",
            meta_group_truth$mods$subpop_detected[[2,2]] < 2 ~ "cl-gld",
            TRUE ~ "cl-dgr"
          )
          
          txt_subpop_detected <- span(
            txt_subpop_cont, span(
              if (is_subpop_detected) "detected" else "not detected",
              class = col_subpop),
            ifelse(meta_group$mods$subpop_detected[[2,2]] < 2,
                   "(though with \u0394AICc \uFF1C 2).",
                   "(\u0394AICc \uFF1E 2).")
          )
          
          expected_ratio <- .get_ratios(meta_group_truth)
          observed_ratio <- .get_ratios(meta_group)
          
          ratio <- paste0(round(observed_ratio$est, 2), ":1")
          status_ratio <- list(
            "truth" = dplyr::between(observed_ratio$est,
                                     expected_ratio$lci, 
                                     expected_ratio$uci),
            "one_expected" = expected_ratio$lci <= 1 &
              expected_ratio$uci >= 1,
            "one_observed" = observed_ratio$lci <= 1 &
              observed_ratio$uci >= 1)
          
          txt_ratio <- span(
            "The", txt_target[[target]], "ratio", txt_ratio_order,
            ifelse(
              status_ratio$one_observed,
              paste0("overlapped with one (i.e., ",
                     "no difference between groups)."),
              paste0("did not overlap with one (ratio point estimate of ",
                     wrap_none(ratio, ")."))))
          
          sufficient_simulations <- status_ratio$truth && 
            (status_ratio$one_expected == status_ratio$one_observed) &&
            !is.na(get_cri[[target]][["lci"]]) && 
            !is.na(get_cri[[target]][["uci"]])
          
          txt_simulations <- span(
            style = paste(css_mono, css_bold),
            
            if (sufficient_simulations) {
              tagList("The number of simulations appears sufficient", 
                      "to obtain valid", wrap_none(
                        txt_target[[target]], color = pal$sea, " ratios."))
            } else {
              tagList("The number of simulations appears insufficient", 
                      "to obtain valid", wrap_none(
                        txt_target[[target]], color = pal$dgr, " ratios."))
            }
          )
          
          txt_meta_groups <- p(txt_subpop,
                               txt_subpop_detected,
                               txt_ratio, txt_simulations)
          
        } # end of if (rv$which_meta == "compare")
        
        txt_nsims <- ifelse(
          length(rv$simList) == 1,
          "for a single simulation",
          paste("for", length(rv$simList), "simulations"))
        
        switch(meta_dt$status,
               "Yes" = {
                 txt_mean <- span(
                   style = css_mono,
                   "The mean", txt_target[[target]], txt_nsims,
                   "is", span("within", class = "cl-sea"), "the",
                   paste0("\u00B1", rv$error_threshold * 100, "%"),
                   "error threshold.")
               },
               "Near" = {
                 txt_mean <- span(
                   style = css_mono,
                   "The mean", txt_target[[target]], txt_nsims,
                   "is", span("near", class = "cl-grn"), "the",
                   paste0("\u00B1", rv$error_threshold * 100, "%"),
                   "error threshold.")
               },
               "No" = {
                 txt_mean <- span(
                   style = css_mono,
                   "The mean", txt_target[[target]], txt_nsims,
                   span("falls outside", class = "cl-dgr"), "the",
                   paste0("\u00B1", rv$error_threshold * 100, "%"),
                   "error threshold.")
               })
        
        if (is.na(get_coi[[target]][["lci"]]) &&
            is.na(get_coi[[target]][["uci"]])) {
          txt_uncertainty <- "however, run more simulations to confirm."
        } else {
          txt_uncertainty <- ifelse(
            get_coi[[target]][["uci"]] < .3 && 
              get_coi[[target]][["lci"]] > -.3,
            "with low uncertainty.",
            "with high uncertainty.")
        }
        
        switch(
          meta_dt$status,
          "Yes" = {
            txt_final <- span(
              style = paste(css_mono, css_bold),
              "The number of simulations appears",
              wrap_none("sufficient", color = pal$sea),
              "to accurately estimate mean", 
              wrap_none(txt_target[[target]], end = ","),
              txt_uncertainty)
          },
          "Near" = {
            txt_final <- span(
              style = paste(css_mono, css_bold),
              "The number of simulations appears",
              wrap_none("insufficient", color = pal$grn),
              "to accurately estimate man", 
              wrap_none(txt_target[[target]], end = ","),
              txt_uncertainty)
          },
          "No" = {
            txt_final <- span(
              style = paste(css_mono, css_bold),
              "The number of simulations appears",
              wrap_none("insufficient", color = pal$dgr),
              "to accurately estimate mean", 
              wrap_none(txt_target[[target]], end = ","),
              txt_uncertainty)
          })
        
        if (length(get_cri) > 0) {
          if (!is.na(get_cri[[target]][["lci"]]) &&
              !is.na(get_cri[[target]][["uci"]])) {
            
            txt_final <- tagList(
              txt_final,
              span(
                style = paste("font-weight: 800;",
                              "font-family: var(--monosans);"),
                "Please run more simulations in the corresponding",
                shiny::icon("compass-drafting", class = "cl-sea"),
                span("Analyses", class = "cl-sea"), "tab to confirm."))
          }
        }
        
        index <- which(set_target == target)
        
        if (index == 1) {
          out_meta <- tagList(
            span(txt_title[[target]],
                 style = set_style_title),
            br(),
            p(txt_mean,
              txt_final,
              txt_meta_groups))
          
        } else {
          out_meta <- tagList(
            out_meta,
            br(),
            tagList(
              span(txt_title[[target]],
                   style = set_style_title),
              br(),
              p(txt_mean,
                txt_final,
                txt_meta_groups)))
        }
        
      } # end of [target] loop
      
      rv$report$meta <- tagList(
        out_meta,
        br(), txt_link_meta)
      
    }) # end of observe
    
    ## Rendering complete report: -----------------------------------------
    
    observe({
      req(rv$which_question,
          rv$report$species,
          rv$report$schedule)
      
      if (is.null(rv$report$analyses)) {
        shinyjs::hide(id = "repBox_analyses")
        shinyjs::hide(id = "section-comparison")
        
        m <- length(rv$simList)
        m <- ifelse(m == 1, "one simulation", "two simulations")
        
        shinyalert::shinyalert(
          type = "warning",
          title = "Warning",
          text = tagList(span(
            "Only", m, "currently available.",
            "Run more", span("simulations", class = "cl-grn"), 
            "in one of the previous analyses tabs."
          )),
          html = TRUE,
          size = "xs")
        
        msg_log(
          style = "error",
          message = paste(
            msg_danger("Insufficient simulations"),
            "to generate a report."),
          detail = "Return to the analyses tab(s).")
      }
      
      req(rv$report$analyses)
      
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
                rv$report$schedule,
                
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
                rv$report$schedule,
                
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
              rv$report$schedule,
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
                
                # div(id = "report_meta",
                #     style = paste0("background-color: #f4f4f4;",
                #                    "padding: 20px;",
                #                    "margin-top: 20px;"),
                #     rv$report$meta)
                
              ) # end of tagList
            ) # end of div
            
          }) # end of renderUI, "end_report_both"
          
        }) # end of switch
      
      if (rv$which_meta == "none") {
        shinyjs::show(id = "section-highlight_dur")
        shinyjs::show(id = "section-highlight_dti")
        shinyjs::hide(id = "section-repBox_meta")
      } else {
        shinyjs::hide(id = "section-highlight_dur")
        shinyjs::hide(id = "section-highlight_dti")
        shinyjs::show(id = "section-repBox_meta")
      }
      
    }) %>% bindEvent(input$build_report)
    
    ## Reporting COMPARISON (if available): ------------------------------
    
    observe({
      out_comp <- out_comp_hr <- span("")
      
      ci <- ifelse(is.null(input$ci), .95, input$ci/100)
      
      if (length(rv$which_question) == 1 &
          "Home range" %in% rv$which_question) {
        req(rv$highlight_dur)
        
        highlighted_dur <- as.numeric(rv$highlight_dur)
        
        est <- .err_to_txt(rv$hr_cri_new$est)
        lci <- .err_to_txt(rv$hr_cri_new$lci)
        uci <- .err_to_txt(rv$hr_cri_new$uci)
        
        txt_level <- ifelse(
          rv$hr_cri_new$uci < .3 & rv$hr_cri_new$lci > -.3,
          "and with low", "but with high")
        
        ideal_dur <- fix_unit(
          ("days" %#% rv$tau_p[[1]]$value[2] %#%
             rv$tau_p[[1]]$unit[2]) * 10,
          "days")
        
        if (highlighted_dur >= ideal_dur$value) {
          out_comp <- out_comp_hr <- p(
            "Your new sampling duration would likely be sufficient",
            "for", span("home range", class = "cl-grn"),
            "estimation,", txt_level, "uncertainty:",
            "for a duration of",
            highlighted_dur, "days, there is a",
            wrap_none(ci, "%", css = "cl-blk"),
            "probability that the relative error will lie within",
            wrap_none(lci, "%", css = "cl-blk"),
            "and", wrap_none(uci, "%", end = ".", css = "cl-blk"))
          
        } else {
          out_comp <- out_comp_hr <- p(
            "Your new sampling duration would likely be insufficient",
            "for", span("home range", class = "cl-grn"),
            "estimation.", br(),
            "For a duration of", highlighted_dur,
            "days, there is high uncertainty",
            wrap_none("(", ci, "%", css = "cl-blk"),
            "probability that the relative error will lie within",
            wrap_none(lci, "%", css = "cl-blk"),
            "and", wrap_none(uci, "%", end = ").", css = "cl-blk"))
        }
      } # end of 'Home range'
      
      ## Speed and distance estimation:
      
      if (length(rv$which_question) == 1 &
          "Speed & distance" %in% rv$which_question) {
        req(rv$highlight_dti)
        
        opts <- movedesign::sims_speed[[1]] %>%
          dplyr::select(.data$dti, .data$dti_notes) %>%
          unique()
        
        highlighted_dti <- opts$dti[
          match(rv$highlight_dti, opts$dti_notes)]
        out_dti <- fix_unit(highlighted_dti, "seconds", convert = TRUE)
        
        est <- .err_to_txt(rv$sd_cri_new$est)
        lci <- .err_to_txt(rv$sd_cri_new$lci)
        uci <- .err_to_txt(rv$sd_cri_new$uci)
        
        txt_level <- ifelse(
          rv$sd_cri_new$uci < .3 & rv$sd_cri_new$lci > -.3,
          "and with low", "but with high")
        
        ideal_dti <- fix_unit(
          (rv$tau_v[[1]]$value[2] %#% 
             rv$tau_v[[1]]$unit[2]) / 3, "seconds")
        
        if (highlighted_dti <= ideal_dti$value) {
          out_comp <- out_comp_sd <- p(
            "Your new sampling interval would likely be sufficient",
            "for", span("speed & distance", class = "cl-grn"),
            "estimation,", txt_level, "uncertainty:",
            "for a sampling interval of",
            wrap_none(out_dti$value, " ", out_dti$unit, ", there"),
            "is a", wrap_none(ci, "%", css = "cl-blk"),
            "probability that the relative error will lie within",
            wrap_none(lci, "%", css = "cl-blk"),
            "and", wrap_none(uci, "%", end = ".", css = "cl-blk"))
          
        } else {
          out_comp <- out_comp_sd <- p(
            "Your new sampling interval would likely be insufficient",
            "for", span("speed & distance", class = "cl-grn"),
            "estimation. For a sampling interval of",
            wrap_none(out_dti$value, " ", out_dti$unit, ", there is"),
            "high uncertainty",
            wrap_none("(", ci, "%", css = "cl-blk"),
            "probability that the relative error will lie within",
            wrap_none(lci, "%", css = "cl-blk"),
            "and", wrap_none(uci, "%", end = ").", css = "cl-blk"))
        }
        
      } # end of "Speed & distance"
      
      ### Both home range and speed & distance:
      
      if (length(rv$which_question) > 1) {
        out_analyses <-
          span("Your new sampling schedule (...)",
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
        dplyr::mutate(tau_p = round("days" %#% .data$tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% .data$duration, 1))
      
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
        dplyr::mutate(dur = round("days" %#% .data$dur, 1))
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
          dplyr::mutate(tau_p = round("days" %#% .data$tau_p, 1)) %>%
          dplyr::mutate(duration = round("days" %#% .data$duration, 1))
        
        out_taup <- dt_hr$tau_p[which.min(abs(dt_hr$tau_p - input_taup))]
        dur_for_hr <- dt_hr$dur[which.min(abs(dt_hr$dur - input_dur))]
        
        # Create density data frames:
        ds1_hr <- dt_hr %>%
          dplyr::filter(.data$tau_p == out_taup) %>%
          dplyr::filter(.data$duration == dur_for_hr) %>%
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
        ds1_hr, x >= rv$hr_cri$lci & x <= rv$hr_cri$uci)
      
      if (is_dur) {
        req(rv$hr_cri_new)
        
        out_dur_new <- dt_hr$dur[
          abs(dt_hr$dur - as.numeric(rv$highlight_dur)) %>%
            which.min()]
        
        ds2_hr <- dt_hr %>%
          dplyr::filter(.data$tau_p == out_taup) %>%
          dplyr::filter(.data$duration == out_dur_new) %>%
          stats::na.omit()
        med <- stats::median(ds2_hr$error)
        
        ds2_hr <- stats::density(ds2_hr$error)
        ds2_hr <- data.frame(x = ds2_hr$x, y = ds2_hr$y)
        rv$report$ds2_hr <- data.frame(
          "median" = med,
          "max" = max(ds2_hr$x),
          "min" = min(ds2_hr$x))
        
        rv$hr_cri_new <- suppressWarnings(
          .extract_cri(ds2_hr$x, ci = input_ci))
        if (is_log) ds2_hr$y <- ds2_hr$y / max(ds2_hr$y)
        
        ci2_hr <- subset(
          ds2_hr, x >= rv$hr_cri_new$lci & x <= rv$hr_cri_new$uci)
        
        hr_p1 <- ggplot2::geom_line(
          data = ds2_hr, mapping = ggplot2::aes(
            x = .data$x, y = .data$y),
          col = pal$mdn, linetype = "dotted")
        
        hr_p2 <- ggplot2::geom_area(
          data = ci2_hr,
          mapping = ggplot2::aes(x = .data$x, y = .data$y),
          alpha = 0.2, fill = pal$mdn)
        
        hr_p3 <- ggplot2::geom_segment(
          data = rv$hr_cri_new,
          mapping = ggplot2::aes(
            x = .data$lci,
            xend = .data$uci,
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
        paste0("Median AKDE error + ", rv$hr_cri$ci * 100,
               "% HDI for ", rv$report$dur_for_hr))
      brk <- c("now", "est")
      
      val_fill <- val_col <- c("now" = pal$sea_d, "est" = pal$sea)
      val_linetype <- c("now" = "blank", "est" = "solid")
      val_shape <- c("now" = 19, "est" = 18)
      
      override_size <- c(.8, .8)
      override_stroke <- c(4, 4)
      
      if (is_dur) {
        lbl <- c(
          lbl, paste0("Median AKDE error + ", rv$hr_cri$ci * 100,
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
        ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_vline(xintercept = 0, alpha = 1) +
        
        { if (is_dur) hr_p1 } +
        { if (is_dur) hr_p2 } +
        
        ggplot2::geom_line(
          ggplot2::aes(col = "est"),
          linetype = "dotted") +
        
        { if (!is.na(rv$hr_cri$lci) && !is.na(rv$hr_cri$lci))
          ggplot2::geom_area(
            data = ci1_hr,
            mapping = ggplot2::aes(x = .data$x, y = .data$y, fill = "est"),
            alpha = 0.4) } +
        
        { if (is_dur) hr_p3 } +
        
        { if (!is.na(rv$hr_cri$lci) && !is.na(rv$hr_cri$lci))
          ggplot2::geom_segment(
            data = ~ head(.x, 1),
            mapping = ggplot2::aes(
              x = rv$hr_cri$lci,
              xend = rv$hr_cri$uci,
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
        { if (!is.na(rv$hr_cri$lci) && !is.na(rv$hr_cri$lci))
          ggplot2::scale_fill_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_fill) } +
        { if (!is.na(rv$hr_cri$lci) && !is.na(rv$hr_cri$lci))
          ggplot2::scale_linetype_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_linetype) } +
        ggplot2::scale_shape_manual(
          name = "", labels = lbl, breaks = brk,
          values = val_shape) +
        
        ggplot2::labs(x = "Estimate error (%)",
                      y = y_lab) +
        
        theme_movedesign(font_available = rv$is_font,
                         ft_size = rv$ft_size,
                         title_y = FALSE) +
        ggplot2::theme(
          legend.position = "none",
          axis.title.x = ggplot2::element_blank())
      p
      
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
        dplyr::mutate(dur = round("days" %#% .data$dur, 0)) %>%
        dplyr::mutate(tau_p = round("days" %#% .data$tau_p, 0))
      sd_opts <- movedesign::sims_speed[[1]] %>%
        dplyr::mutate(dur = round("days" %#% .data$dur, 0)) %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      
      out_tauv <- dt_sd$tau_v[which.min(abs(dt_sd$tau_v - input_tauv))]
      dur_for_sd <- dt_sd$dur[which.min(abs(dt_sd$dur - input_dur))]
      
      out_dti <- dt_sd$dti[which.min(abs(dt_sd$dti - input_dti))]
      txt_dti <- sd_opts$dti_notes[match(out_dti, sd_opts$dti)]
      rv$report$txt_dti <- txt_dti
      
      # Create density data frames:
      
      ds1_sd <- dt_sd %>%
        dplyr::filter(.data$tau_v == out_tauv) %>%
        dplyr::filter(.data$dur == dur_for_sd) %>%
        dplyr::filter(.data$dti == out_dti) %>% 
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
        ds1_sd, x >= rv$sd_cri$lci & x <= rv$sd_cri$uci)
      
      if (is_dti) {
        req(rv$sd_cri_new)
        
        dti_new <- sd_opts$dti[match(rv$highlight_dti,
                                     sd_opts$dti_notes)]
        out_dti_new <- dt_sd$dti[which.min(abs(dt_sd$dti - dti_new))]
        txt_dti_new <- sd_opts$dti_notes[match(out_dti_new,
                                               sd_opts$dti)]
        rv$report$txt_dti_new <- txt_dti_new
        
        ds2_sd <- dt_sd %>%
          dplyr::filter(.data$tau_v == out_tauv) %>%
          dplyr::filter(.data$dur == dur_for_sd) %>%
          dplyr::filter(.data$dti == out_dti_new) %>%
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
        
        rv$sd_cri_new <- suppressWarnings(
          .extract_cri(ds2_sd$x, ci = input_ci))
        
        if (is_log) ds2_sd$y <- ds2_sd$y / max(ds2_sd$y)
        
        ci2_sd <- subset(
          ds2_sd, x >= rv$sd_cri_new$lci & x <= rv$sd_cri_new$uci)
        
        sd_p1 <- ggplot2::geom_line(
          data = ds2_sd, mapping = ggplot2::aes(x = .data$x, y = .data$y),
          col = pal$mdn, linetype = "dotted")
        
        sd_p2 <- ggplot2::geom_area(
          data = ci2_sd,
          mapping = ggplot2::aes(x = .data$x, y = .data$y),
          alpha = 0.2, fill = pal$mdn)
        
        sd_p3 <- ggplot2::geom_segment(
          data = rv$sd_cri_new,
          mapping = ggplot2::aes(
            x = .data$lci,
            xend = .data$uci,
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
        paste0("Median CTSD error + ", rv$sd_cri$ci * 100,
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
          lbl, paste0("Median CTSD error + ", rv$sd_cri$ci * 100,
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
          ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y)) +
          ggplot2::geom_vline(xintercept = 0, alpha = 1) +
          
          {if (is_dti) sd_p1 } +
          {if (is_dti) sd_p2 } +
          
          ggplot2::geom_line(
            ggplot2::aes(col = "est"),
            linetype = "dotted") +
          
          { if (!is.na(rv$sd_cri$lci) && !is.na(rv$sd_cri$lci))
            ggplot2::geom_area(
              data = ci1_sd,
              mapping = ggplot2::aes(x = .data$x,
                                     y = .data$y,
                                     fill = "est"),
              alpha = 0.4) } +
          
          { if (is_dti) sd_p3 } +
          
          { if (!is.na(rv$sd_cri$lci) && !is.na(rv$sd_cri$lci))
            ggplot2::geom_segment(
              data = ~ head(.x, 1),
              mapping = ggplot2::aes(
                x = rv$sd_cri$lci,
                xend = rv$sd_cri$uci,
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
          { if (!is.na(rv$sd_cri$lci) && !is.na(rv$sd_cri$lci))
            ggplot2::scale_fill_manual(
              name = "", labels = lbl, breaks = brk,
              values = val_fill) } +
          { if (!is.na(rv$sd_cri$lci) && !is.na(rv$sd_cri$lci))
            ggplot2::scale_linetype_manual(
              name = "", labels = lbl, breaks = brk,
              values = val_linetype) } +
          ggplot2::scale_shape_manual(
            name = "", labels = lbl, breaks = brk,
            values = val_shape) +
          
          ggplot2::labs(x = "Estimate error (%)",
                        y = y_lab) +
          
          theme_movedesign(font_available = rv$is_font,
                           ft_size = rv$ft_size,
                           title_y = FALSE) +
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
        dplyr::mutate(tau_p = round("days" %#% .data$tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% .data$duration, 1))
      
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
          dplyr::mutate(dur = round("days" %#% .data$dur, 1))
        out_tauv <- dt_sd$tau_v[which.min(abs(dt_sd$tau_v - input_tauv))]
        out_tauv <- fix_unit(out_tauv, "seconds", convert = TRUE)
      }
      
      dt_sd <- movedesign::sims_speed[[1]] %>%
        dplyr::mutate(dur = round(.data$input_dur, 0)) %>%
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
      req(rv$hr_cri, rv$sd_cri)
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
        req(rv$report$ds2_hr, rv$hr_cri_new)

        details <- details %>%
          dplyr::add_row(
            question = "Home range",
            group = "est_new",
            type = "hr_est_new",
            value = rv$report$ds2_hr[["median"]],
            lci = rv$hr_cri_new$lci,
            uci = rv$hr_cri_new$uci,
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
        req(rv$report$ds2_sd, rv$sd_cri_new)

        details <- details %>%
          dplyr::add_row(
            question = "Speed & distance",
            group = "est_new",
            type = "sd_est_new",
            value = rv$report$ds2_sd[["median"]],
            lci = rv$sd_cri_new$lci,
            uci = rv$sd_cri_new$uci,
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
            lci = rv$hr_cri$lci,
            uci = rv$hr_cri$uci,
            label = paste0("AKDE error for ",
                           rv$report$dur_for_hr),
            fill = pal$sea,
            col = pal$sea,
            linetype = "solid",
            shape = 18)
        
        if (!is.null(rv$simList)) {
          ci <- ifelse(is.null(input$ci), .95, input$ci/100)
          err <- suppressWarnings(.extract_cri(rv$hrErr$est, ci))
          err <- data.frame(
            lci = ifelse(is.null(err$lci), NA, err$lci),
            mean = mean(rv$hrErr$est, na.rm = TRUE),
            uci = ifelse(is.null(err$uci), NA, err$uci))
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
            lci = rv$sd_cri$lci,
            uci = rv$sd_cri$uci,
            label = paste0("CTSD error for ",
                           rv$report$txt_dti),
            fill = ifelse(is_both, pal$grn, pal$sea),
            col = ifelse(is_both, pal$grn, pal$sea),
            linetype = "solid",
            shape = 18)
        
        if (!is.null(rv$simList)) {
          ci <- ifelse(is.null(input$ci), .95, input$ci/100)
          err <- suppressWarnings(
            .extract_cri(rv$speedErr$est, ci = ci))
          err <- data.frame(
            lci = err$lci,
            mean = mean(rv$speedErr$est, na.rm = TRUE),
            uci =  err$uci)
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
        dplyr::mutate(group = factor(.data$group,
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
        dplyr::mutate(tau_p = round("days" %#% .data$tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% .data$duration, 1))
      
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
            dplyr::mutate(duration = round("days" %#% .data$dur, 1)) %>%
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
        dplyr::mutate(tau_p = round("days" %#% .data$tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% .data$duration, 1))
      
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
          dplyr::filter(.data$duration == dur_NEW)
        y_start <- dplyr::pull(newdat, .data$error_lci)
        y_end <- dplyr::pull(newdat, .data$error_uci)
        
        p1 <- ggplot2::geom_segment(
          ggplot2::aes(x = .data$dur_NEW,
                       xend = .data$dur_NEW,
                       y = .data$y_start,
                       yend = .data$y_end),
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
        dplyr::mutate(dur = round("days" %#% .data$dur, 0))
      dat$id <- 1:nrow(dat)
      
      opts <- sims %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
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
        dplyr::filter(.data$tau_v == filtering_tauv) %>%
        dplyr::filter(.data$dti == filtering_dti) %>%
        stats::na.omit()
      
      pd <- ggplot2::position_dodge(width = 0.6)
      
      if (reveal_if) {
        newdat <- dat %>%
          dplyr::filter(.data$tau_v == filtering_tauv) %>%
          dplyr::filter(.data$dti == dti_new) %>%
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
    
    build_tbl_report <- reactive({
      n_sims <- length(rv$simList)
      
      dt_dv <- rv$dev$tbl
      dt_dv <- dt_dv %>% dplyr::slice_tail(n = n_sims)
      
      if ("Home range" %in% rv$which_question) {
        req(rv$hr$tbl)
        dt_hr <- rv$hr$tbl
        dt_hr <- dplyr::filter(dt_hr, .data$data == "Initial") 
        dt_hr <- dplyr::slice_tail(dt_hr, n = n_sims)
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        req(rv$sd$tbl)
        dt_sd <- rv$sd$tbl
        dt_sd <- dplyr::filter(dt_sd, .data$data == "Initial") 
        dt_sd <- dplyr::slice_tail(dt_sd, n = n_sims)
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
      
      tmpdat <- suppressMessages(dplyr::full_join(dat, dt_dv))
      
      if ("Home range" %in% rv$which_question) {
        tmphr <- dt_hr %>% dplyr::select(-c(.data$device))
        tmpdat <- suppressMessages(
          tmpdat %>%
            dplyr::group_by(.data$seed) %>% 
            dplyr::full_join(dt_hr))
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        tmpdat <- suppressMessages(
          tmpdat %>%
            dplyr::full_join(dt_sd))
      }
      
      tmpdat <- tmpdat %>%
        dplyr::distinct() %>% 
        dplyr::group_by(.data$seed) %>%
        dplyr::summarize(dplyr::across(
          dplyr::everything(), 
          ~ifelse(all(is.na(.)), NA, .[!is.na(.)][1])))
      return(tmpdat)
      
    }) # end of reactive
    
    observe({
      req(rv$active_tab == 'report',
          rv$which_question,
          rv$dev$tbl,
          rv$simList,
          !rv$is_report)
      
      rv$report$tbl <- build_tbl_report()
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
        "group",
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
        group = "Group",
        taup = "\u03C4\u209A",
        tauv = "\u03C4\u1D65",
        sigma = "\u03C3\u209A",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = "N (area)",
        N2 = "N (speed)",
        area = "Area",
        area_err = "Error",
        area_err_min = "95% LCI",
        area_err_max = "95% UCI",
        ctsd = "Speed",
        ctsd_err = "Error",
        ctsd_err_min = "95% LCI",
        ctsd_err_max = "95% UCI",
        dist = "Distance",
        dist_err = "Error")
      
      dat <- dplyr::select(rv$report$tbl, 
                           -c(.data$device, .data$seed))
      if (!rv$grouped) {
        dat <- dplyr::select(dat, -.data$group)
        choices_subset <- choices_subset[-1]
      }
      
      if (!is.null(choices_subset)) {
        dat <- dplyr::select(dat, .data$choices_subset)
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
        group = if ("group" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "group"]) },
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
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 0)) },
        N1 = if ("N1" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "N1"],
            style = format_num,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 1)) },
        N2 = if ("N2" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "N2"],
            style = format_num,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 1)) },
        area = if ("area" %in% choices_subset) {
          reactable::colDef(
            minWidth = 100, name = nms[1, "area"]) },
        area_err = if ("area_err" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err"],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)) },
        area_err_min = if ("area_err_min" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err_min"],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)) },
        area_err_max = if ("area_err_max" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err_max"],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)) },
        ctsd = if ("ctsd" %in% choices_subset) {
          reactable::colDef(
            minWidth = 100, name = nms[1, "ctsd"]) },
        ctsd_err = if ("ctsd_err" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd_err"],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)) },
        ctsd_err_min = if ("ctsd_err_min" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd_err_min"],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)) },
        ctsd_err_max = if ("ctsd_err_max" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd_err_max"],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)) },
        dist = if ("dist" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 100, name = nms[1, "dist"]) },
        dist_err = if ("dist_err" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "dist_err"],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)) }
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
    
    ## Sampling schedule: -------------------------------------------------
    
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
        rv = rv, data = rv$simList, obj = rv$simfitList,
        type = "N", name = "area")
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'report',
          rv$ctsdList, rv$simList, rv$simfitList)
      
      mod_blocks_server(
        id = "repBlock_Nspeed", 
        rv = rv, data = rv$simList, obj = rv$ctsdList,
        type = "N", name = "speed")
      
    }) # end of observe
    
    ## Outputs: -----------------------------------------------------------
    
    observe({
      req(!is.null(rv$simList), rv$hrErr, rv$which_meta)
      req(nrow(rv$hrErr) == length(rv$simList))
      
      if ("Home range" %in% rv$which_question) {
        shinyjs::show(id = "repBox_hr_err") 
      } else { shinyjs::hide(id = "repBox_hr_err") }
      
      if (rv$which_meta == "none") {
        mod_blocks_server(
          id = "repBlock_hrErr",
          rv = rv, type = "hr", name = "hrErr")
        
      } else {
        req(rv$metaErr)
        mod_blocks_server(
          id = "repBlock_hrErr",
          rv = rv, type = "hr", name = "metaErr")
      }
      
    }) # end of observe
    
    observe({
      req(rv$simList, rv$speedErr, rv$which_meta)
      req(nrow(rv$speedErr) == length(rv$simList))
      
      if ("Speed & distance" %in% rv$which_question)
        shinyjs::show(id = "repBox_speed_err") else
          shinyjs::hide(id = "repBox_speed_err")
      
      if (rv$which_meta == "none") {
        mod_blocks_server(
          id = "repBlock_speedErr",
          rv = rv, type = "ctsd", name = "speedErr")
        
      } else {
        req(rv$metaErr)
        mod_blocks_server(
          id = "repBlock_speedErr",
          rv = rv, type = "ctsd", name = "metaErr")
      }
      
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
