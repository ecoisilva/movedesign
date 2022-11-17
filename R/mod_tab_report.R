#' tab_report UI Function
#'
#' @description A shiny Module.
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
                          inputId = ns("create_report"),
                          icon = icon("bookmark"),
                          label = "Build report",
                          width = "100%",
                          class = "btn-primary")
                        
                      ) # end of panel
                    )) # end of tabBox // repTabs_details
              ), # end of box // repBox_details
              
              shinydashboardPlus::box(
                title = NULL,
                width = NULL,
                headerBorder = FALSE,
                
                uiOutput(ns("repInfo_hrErr")),
                uiOutput(ns("repInfo_speedErr")),
                uiOutput(ns("repInfo_distErr")),
                
                br()
                
              )) # end of box
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
                             uiOutput(ns("repBlock_taup")),
                             uiOutput(ns("repBlock_tauv"))
                           ), uiOutput(ns("repBlock_sigma"))
                           
                  ), # end of panel (1 out of 2)
                  
                  tabPanel(title = "Tracking regime:",
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
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-10",
          
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
                
                div(id = ns("section-two_questions"),
                    uiOutput(ns("end_report_both"))),
                
                div(id = ns("section-comparison"),
                    
                    p("Quick comparison with other",
                      wrap_none(span("regimes", 
                                     class = "cl-sea"), "?")) %>%
                      tagAppendAttributes(class = "subheader"),
                    
                    column(width = 12, align = "center",
                           style = paste("z-index: 999;"),
                           uiOutput(ns("highlighting_reg"))),
                    
                    uiOutput(outputId = ns("reportPlots_error")),
                    
                    helpText(
                      "Note: This comparison is based on aggregated",
                      "information from 400 simulations, so mean",
                      "estimates will not match your current values."),
                    
                    uiOutput(ns("end_comparison"))
                    
                ) # end of div // section-comparison
                
            ) # end of div
          ) # end of box // repBox_analyses
          
      ), # end of div
      
      # [bottom column] ---------------------------------------------------
      
      div(class = div_column_main,
          
          ## Tables: ------------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Tables:", class = "ttl-box"),
            id = ns("repBox_tables"),
            width = NULL,
            solidHeader = FALSE,
            
            reactable::reactableOutput(ns("endTable"))
            
            # footer = tagList(
            #   uiOutput(outputId = ns("reportInput_vars")))
            
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
    
    vals$report <- reactiveValues()
    pal <- load_pal()
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    observe({
      req(vals$active_tab == 'report')
      shinyjs::hide(id = "end_comparison")

      boxnames <- c("analyses", "tables")
      for(i in 1:length(boxnames)) {
        shinyjs::hide(id = paste0("repBox_", boxnames[i])) }
    })
    
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
      
    }) # end of renderUI // report_question
    
    ## Rendering device limitations: --------------------------------------
    
    output$report_device <- renderUI({
      req(vals$which_limitations)
      
      if ("loss" %in% vals$which_limitations) {
        ui_loss <- staticBlock(paste0(vals$loss, "%"),
                               active = TRUE)
      } else if (!("loss" %in% vals$which_limitations)) {
        ui_loss <- staticBlock("No data loss",
                               active = FALSE)
      }
      
      if ("error" %in% vals$which_limitations) {
        ui_error <- staticBlock(paste(vals$error, "meters"),
                                active = TRUE)
      } else if (!("error" %in% vals$which_limitations)) {
        ui_error <- staticBlock("No error",
                                active = FALSE)
      }
      
      if ("limit" %in% vals$which_limitations) {
        ui_limit <- staticBlock(paste(vals$storage, "locations"),
                                type = "max", active = TRUE)
      } else if (!("limit" %in% vals$which_limitations)) {
        ui_limit <- staticBlock("No limit",
                                active = FALSE)
      }
      
      out <- tagList(
        span("Data loss", class = "txt-label"),
        ui_loss,
        span("Location error:", class = "txt-label"),
        ui_error,
        span("Storage limit:", class = "txt-label"),
        ui_limit
      )
      
      return(out)
      
    }) # end of renderUI // report_question
    
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
      
    }) # end of renderUI // highlighting_reg
    
    # BLOCKS --------------------------------------------------------------
    ## Species: -----------------------------------------------------------
    
    output$repBlock_taup <- shiny::renderUI({
      req(vals$tau_p0)
      
      parBlock(
        header = span(
          HTML(paste0("Position autocorrelation ",
                      "(\u03C4", tags$sub("p"), ")"))),
        value =
          paste(scales::label_comma(
            accuracy = .1)(vals$tau_p0$value[2]), vals$tau_p0$unit[2]),
        subtitle = if (vals$data_type != "simulated") {
          paste(
            ifelse(vals$tau_p0$value[1] == 0,
                   "0", scales::label_comma(accuracy = .1)
                   (vals$tau_p0$value[1])),
            "\u2014",
            scales::label_comma(accuracy = .1)(vals$tau_p0$value[3]))
        } else { "" })
      
    }) # end of renderUI // repBlock_taup
    
    output$repBlock_tauv <- shiny::renderUI({
      req(vals$tau_v0)
      
      parBlock(
        header = span(
          HTML(paste0("Velocity autocorrelation ",
                      "(\u03C4", tags$sub("v"), ")"))),
        value =
          paste(scales::label_comma(
            accuracy = .1)(vals$tau_v0$value[2]), vals$tau_v0$unit[2]),
        subtitle = if (vals$data_type != "simulated") {
          paste(
            ifelse(vals$tau_v0$value[1] == 0,
                   "0", scales::label_comma(accuracy = .1)
                   (vals$tau_v0$value[1])),
            "\u2014",
            scales::label_comma(accuracy = .1)(vals$tau_v0$value[3]))
        } else { "" })
      
    }) # end of renderUI // repBlock_tauv
    
    output$repBlock_sigma <- shiny::renderUI({
      req(vals$sigma0)
      
      sig <- fix_unit(vals$sigma0$value[2],
                      unit = vals$sigma0$unit[2],
                      convert = TRUE)
      
      sig_lci <- fix_unit(sig$unit %#% 
                            vals$sigma0$value[1] %#% 
                            vals$sigma0$unit[1], 
                          unit = sig$unit,
                          ui = TRUE)

      sig_uci <- fix_unit(sig$unit %#% 
                            vals$sigma0$value[3] %#% 
                            vals$sigma0$unit[3], 
                          unit = sig$unit,
                          ui = TRUE)
      parBlock(
        header = span(HTML("Semi-variance (\u03C3)")),
        value = span(HTML("&nbsp;", sig$value, sig_lci$unit)),
        subtitle = if (vals$data_type != "simulated") {
          paste(ifelse(sig_lci$value == 0,
                       "0", sig_lci$value),
                "\u2014", sig_uci$value) } else { "" })
      
    }) # end of renderUI // repBlock_sigma
    
    ## Tracking regime: ---------------------------------------------------
    
    output$repBlock_dur <- renderUI({
      req(vals$reg$dur)
      
      out <- fix_unit(vals$reg$dur, vals$reg$dur_unit,
                      convert = TRUE)
      
      if (out$unit == "months" || out$unit == "month") {
        out_days <- ("days" %#% 
                       (vals$reg$dur %#% vals$reg$dur_unit)) %>% 
          fix_unit(., "days")
        
        txt <- paste0("(or ", out_days[1], " ", out_days[2], ")")
      } else {
        txt <- ""        
      }
      
      parBlock(
        header = "Sampling duration",
        value = paste(out[1], out[2]),
        subtitle = txt)
      
    }) # end of renderUI // repBlock_dur
    
    output$repBlock_dti <- renderUI({
      req(vals$reg$dti, vals$reg$dti_unit)
      
      out <- fix_unit(vals$reg$dti, vals$reg$dti_unit,
                      convert = TRUE)
      
      parBlock(
        header = "Sampling interval",
        value = paste(out[1], out[2]),
        subtitle = "between fixes")
      
    }) # end of renderUI // repBlock_dti
    
    ## Analyses: --------------------------------------------------------
    ### Sample sizes: ---------------------------------------------------
    
    output$repUI_sizes <- renderUI({
      req(vals$is_reg_valid)
      
      if (is.null(vals$which_question) ||
          length(vals$which_question) > 1) {
        out <- tagList(
          uiOutput(ns("repBlock_n")),
          splitLayout(
            uiOutput(ns("repBlock_Narea")),
            uiOutput(ns("repBlock_Nspeed"))))
      }
      
      if (length(vals$which_question) == 1 &&
          "Home range" %in% vals$which_question) {
        out <- splitLayout(
          uiOutput(ns("repBlock_n")),
          uiOutput(ns("repBlock_Narea"))) }
      
      if (length(vals$which_question) == 1 &&
          "Speed & distance" %in% vals$which_question) {
        out <- splitLayout(
          uiOutput(ns("repBlock_n")),
          uiOutput(ns("repBlock_Nspeed"))) }
      
      return(out)
    }) # end of renderUI // repUI_sizes
    
    output$repBlock_n <- renderUI({
      req(vals$device_n)
      
      n <- vals$device_n
      
      number_n <- ""
      icon_n <- FALSE
      
      if (!is.null(isolate(input$deviceInput_loss))) {
        n_loss <- isolate(input$deviceInput_loss)
        number_n <- paste0(n_loss, "%")
        icon_n <- TRUE
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
    
    output$repBlock_Narea <- renderUI({
      req(vals$device_N1)
      
      diff1 <- paste0("-", round((100 - ((
        vals$device_N1 * 100)/vals$device_n)), 1), "%")
      
      sampleBlock(
        number = diff1,
        numberIcon = TRUE,
        header = round(vals$device_N1, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)
      
    }) # end of renderUI // repBlock_Narea
    
    output$repBlock_Nspeed <- renderUI({
      req(vals$device_N2)
      
      diff <- paste0("-", round((100 - ((
        vals$device_N2 * 100)/vals$device_n)), 1), "%")
      
      sampleBlock(
        number = diff,
        numberIcon = TRUE,
        header = round(vals$device_N2, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)
      
    }) # end of renderUI // repBlock_Nspeed
    
    ### Relative error: ---------------------------------------------------
    
    output$repInfo_hrErr <- shiny::renderUI({
      req(vals$hrErr, "Home range" %in% vals$which_question)
      
      errorBlock(icon = "radiation",
                 text = "Home range error",
                 value = vals$hrErr[[2]],
                 min = vals$hrErr[[1]],
                 max = vals$hrErr[[3]],
                 rightBorder = FALSE)
      
    }) # end of renderUI // repInfo_hrErr
    
    output$repInfo_speedErr <- shiny::renderUI({
      req(vals$speedErr, "Speed & distance" %in% vals$which_question)
      
      errorBlock(icon = "radiation",
                 text = "Speed error",
                 value = vals$speedErr[[2]],
                 min = vals$speedErr[[1]],
                 max = vals$speedErr[[3]],
                 rightBorder = FALSE)
      
    }) # end of renderUI // repInfo_speedErr
    
    output$repInfo_distErr <- shiny::renderUI({
      req(vals$distErr, "Speed & distance" %in% vals$which_question)
      
      errorBlock(icon = "radiation",
                 text = "Distance error",
                 value = vals$distErr[[2]],
                 min = vals$distErr[[1]],
                 max = vals$distErr[[3]],
                 rightBorder = FALSE)
      
    }) # end of renderUI // repInfo_distErr
    
    # VALIDATION ----------------------------------------------------------
    
    observe({
      vals$ci <- input$ci
    }) %>% # end of observe
      bindEvent(input$ci)
    
    observe({
      req(vals$active_tab == 'report')
      
      if (!is.null(vals$is_analyses)) {
        if (!vals$is_analyses) {
          
          shinyalert::shinyalert(
            type = "error",
            title = "Regime does not match analyses",
            text = span(
              "You have changed the regime without re-running",
              "estimations. Please go back to the",
              icon("compass-drafting", class = "cl-mdn"),
              span("Analyses", class = "cl-mdn"), "tab",
              "and make sure to click the",
              icon("paper-plane", class = "cl-mdn"),
              span("'Run estimation'", class = "cl-mdn"), "button."
            ),
            html = TRUE,
            size = "s")
        }
      }
      
    }) # end of observer
    
    # CALCULATIONS ------------------------------------------------------
    ## Credible intervals for HR estimation: ----------------------------
    
    observe({
      req(vals$active_tab == 'report')
      req(vals$tau_p0, vals$reg$dur, vals$reg$dur_unit)
      
      input_taup <- "days" %#% 
        vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      input_dur <- "days" %#% vals$reg$dur %#% vals$reg$dur_unit
      
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
      
      CI <- ifelse(is.null(vals$ci), .95, vals$ci/100)
      vals$hrCI <- bayestestR::ci(newdat$error, ci = CI, method = "HDI")
      
    }) # end of observe
    
    observe({
      req(vals$active_tab == 'report')
      req(vals$tau_v0, vals$reg$dti, vals$reg$dti_unit)
      
      input_tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      input_dur <- "days" %#% vals$reg$dur %#% vals$reg$dur_unit
      input_dti <- vals$reg$dti %#% vals$reg$dti_unit
      
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
      
      CI <- ifelse(is.null(vals$ci), .95, vals$ci/100)
      vals$sdCI <- bayestestR::ci(newdat$error, ci = CI,
                                  method = "HDI")
      
    }) # end of observe
    
    observe({ # For new/modified regimes:
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
      
      CI <- ifelse(is.null(vals$ci), .95, vals$ci/100)
      vals$hrCI_new <- bayestestR::ci(newdat$error,
                                      ci = CI, method = "HDI")
      
    }) %>% # end of observe,
      bindEvent(input$highlight_dur)
    
    observe({ # For new/modified regimes:
      req(input$highlight_dti > 0)
      shinyjs::show(id = "end_comparison")
      
      input_tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      
      dat <- movedesign::sims_speed[[1]]
      index_tauv <- which.min(abs(dat$tau_v - input_tauv))
      
      out_tauv <- dat$tau_v[index_tauv]
      opts <- movedesign::sims_speed[[1]] %>%
        dplyr::select(.data$dti, .data$dti_notes) %>%
        unique()
      out_dti <- fix_unit(opts$dti[match(input$highlight_dti,
                                         opts$dti_notes)],
                          "seconds")
      
      newdat <- dat %>%
        dplyr::filter(tau_v == out_tauv) %>%
        dplyr::filter(dti == out_dti$value)
      
      CI <- ifelse(is.null(vals$ci), .95, vals$ci/100)
      vals$sdCI_new <- bayestestR::ci(newdat$error,
                                      ci = CI, method = "HDI")
      
    }) %>% # end of observe,
      bindEvent(input$highlight_dti)
    
    # BLOCKS ------------------------------------------------------------
    ## Species & individual: --------------------------------------------
    
    output$report_plot <- ggiraph::renderGirafe({
      req(vals$data1)
      
      newdat <- vals$data1
      yrange <- diff(range(newdat$y))
      xrange <- diff(range(newdat$x))
      
      if (yrange < 1.5 * xrange) {
        ymin <- min(newdat$y) - yrange * .3
        ymax <- max(newdat$y) + yrange * .3
      } else if (yrange < 2 * xrange) {
        ymin <- min(newdat$y) - yrange * .5
        ymax <- max(newdat$y) + yrange * .5
      } else {
        ymin <- min(newdat$y)
        ymax <- max(newdat$y)
      }
      
      if (xrange < 2 * yrange) {
        xmin <- min(newdat$x) - xrange * .5
        xmax <- max(newdat$x) + xrange * .5
      } else {
        xmin <- min(newdat$x)
        xmax <- max(newdat$x)
      }
      
      p <- ggplot2::ggplot() +
        
        ggplot2::geom_path(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = timestamp),
          alpha = .9) +
        ggiraph::geom_point_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = timestamp),
          size = 1.2) +
        
        ggplot2::labs(x = "x coordinate",
                      y = "y coordinate") +
        
        ggplot2::scale_x_continuous(
          labels = scales::comma,
          limits = c(xmin, xmax)) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(ymin, ymax)) +
        viridis::scale_color_viridis(
          name = "Tracking time:",
          option = "D", trans = "time",
          breaks = c(min(newdat$timestamp),
                     max(newdat$timestamp)),
          labels = c("Start", "End")) +
        
        theme_movedesign() +
        ggplot2::theme(legend.position = "none")
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 5, height_svg = 5,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .5),
          ggiraph::opts_toolbar(saveaspng = FALSE)))
      
    }) # end of renderGirafe // report_plot
    
    # PLOTS ---------------------------------------------------------------
    
    observe({
      req(vals$active_tab == 'report')
      req(vals$tau_p0,
          vals$reg$dur, vals$reg$dur_unit,
          vals$reg$dti, vals$reg$dti_unit,
          vals$ci)
      
      input_taup <- "days" %#% 
        vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      input_dur <- "days" %#% vals$reg$dur %#% vals$reg$dur_unit
      
      input_tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      input_dti <- vals$reg$dti %#% vals$reg$dti_unit
      
      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")
      
      ## Rendering density plots: -----------------------------------------
      
      observe({
        req(vals$active_tab == 'report')
        req(vals$tau_p0,
            vals$reg$dur, vals$reg$dur_unit,
            vals$reg$dti, vals$reg$dti_unit,
            vals$ci,
            vals$hrCI)
        
        input_ci <- ifelse(is.null(vals$ci), .95, vals$ci/100)
        
        input_taup <- "days" %#% 
          vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
        
        input_dur <- "days" %#% vals$reg$dur %#% vals$reg$dur_unit
        
        tooltip_css <- paste(
          "font-family: 'Roboto Condensed', sans-serif;",
          "background-color: #222d32;",
          "font-size: 14px;",
          "padding: 5px;",
          "color: #fff;")
        
        # Preparing ifelse statements:
        
        is_both <- length(vals$which_question) > 2
        is_dur <- FALSE
        if (!is.null(input$highlight_dur)) {
          if (!is.na(as.numeric(input$highlight_dur))) 
            is_dur <- TRUE
        }
        
        is_log <- FALSE
        if (!is.null(input$repInput_scaled)) {
          if (input$repInput_scaled) 
            is_log <- TRUE
        }
        
        # Prepare datasets:
        
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
        
        ds1_hr <- stats::density(ds1_hr$error)
        ds1_hr <- data.frame(x = ds1_hr$x, y = ds1_hr$y)
        
        if (is_log) ds1_hr$y <- ds1_hr$y / max(ds1_hr$y)
       
        ci1_hr <- subset(
          ds1_hr, x >= vals$hrCI$CI_low & x <= vals$hrCI$CI_high)
   
        if (is_dur) {
          out_dur_new <- dt_hr$dur[
            abs(dt_hr$dur - as.numeric(input$highlight_dur)) %>%
              which.min()]
          
          ds2_hr <- dt_hr %>%
            dplyr::filter(tau_p == out_taup) %>%
            dplyr::filter(duration == out_dur_new) %>%
            stats::na.omit()
          
          ds2_hr <- stats::density(ds2_hr$error)
          ds2_hr <- data.frame(x = ds2_hr$x, y = ds2_hr$y)
          
          hrCI_new <- bayestestR::ci(ds2_hr$x, ci = input_ci, method = "HDI")
          
          if (is_log) ds2_hr$y <- ds2_hr$y / max(ds2_hr$y)
          
          ci2_hr <- subset(
            ds2_hr, x >= hrCI_new$CI_low & x <= hrCI_new$CI_high)
          
          hr_p1 <- ggplot2::geom_line(
            data = ds2_hr, mapping = ggplot2::aes(x = x, y = y),
            col = pal$mdn, linetype = "dotted")
          
          hr_p2 <- ggplot2::geom_area(
            data = ci2_hr,
            mapping = ggplot2::aes(x = x, y = y),
            alpha = 0.2, fill = pal$mdn)
          
          hr_p3 <- ggplot2::geom_segment(
            data = hrCI_new,
            mapping = ggplot2::aes(
              x = .data$CI_low,
              xend = .data$CI_high,
              y = 0, yend = 0,
              col = "est_new", linetype = "est_new"),
            size = .8)
          
          hr_p4 <- ggplot2::geom_point(
            data = ds2_hr,
            mapping = ggplot2::aes(
              x = stats::median(x), y = 0,
              col = "est_new", shape = "est_new"),
            size = 6)
        }
        
        lbl <- c(
          paste0("Current error"),
          paste0("Median error + ", vals$hrCI$CI * 100,
                 "% HDI for ", dur_for_hr, " days"))
        brk <- c("now", "est")
        
        val_fill <- val_col <- c("now" = pal$sea_d, "est" = pal$sea)
        val_linetype <- c("now" = "blank", "est" = "solid")
        val_shape <- c("now" = 19, "est" = 18)
        
        override_size <- c(.8, .8)
        override_stroke <- c(4, 4)
        
        if (is_dur) {
          lbl <- c(
            lbl, paste0("Median error + ", vals$hrCI$CI * 100,
                        "% HDI for ", input$highlight_dur, " days"))
          brk <- c(brk, "est_new")
          
          val_fill <- val_col <- c(val_fill, "est_new" = pal$mdn)
          val_linetype <- c(val_linetype, "est_new" = "solid")
          val_shape <- c(val_shape, "est_new" = 18)
          
          override_size <- c(override_size, .8)
          override_stroke <- c(override_stroke, 4)
        }
        
        y_lab <- ifelse(input$repInput_scaled,
                        "Probability density", "Density")
        
        #### Home range simulations: -------------------------------------
        
        output$plot_hr_density <- ggiraph::renderGirafe({
          
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
                x = vals$hrCI$CI_low,
                xend = vals$hrCI$CI_high,
                y = 0, yend = 0, col = "est",
                linetype = "est"),
              size = .8) +
            
            { if (is_dur) hr_p4 } +
            
            ggplot2::geom_point(
              data = ds1_hr,
              mapping = ggplot2::aes(
                x = stats::median(x), y = 0,
                col = "est", shape = "est"),
              size = 6) +
            
            { if (!is.null(vals$hrErr)) 
              ggplot2::geom_point(
                ggplot2::aes(x = vals$hrErr[[2]], y =  0,
                             col = "now", shape = "now"),
                size = 6, alpha = .7)
            } +
            ggplot2::scale_x_continuous(labels = scales::percent) +
            
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
            
            theme_movedesign() +
            ggplot2::theme(
              legend.position = "none",
              axis.title.x = ggplot2::element_blank())
          
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
                            "stroke: #004647;"))))
          
        }) # end of renderGirafe // plot_hr_density
      }) %>% # end of observe,
        bindEvent(input$create_report)
      
      observe({
        req(vals$active_tab == 'report')
        req(vals$tau_p0, vals$tau_v0,
            vals$reg$dur, vals$reg$dur_unit,
            vals$reg$dti, vals$reg$dti_unit,
            vals$ci,
            vals$hrCI,
            vals$sdCI)

        input_ci <- ifelse(is.null(vals$ci), .95, vals$ci/100)

        input_taup <- "days" %#%
          vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
        input_tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]

        input_dur <- "days" %#% vals$reg$dur %#% vals$reg$dur_unit
        input_dti <- vals$reg$dti %#% vals$reg$dti_unit

        tooltip_css <- paste(
          "font-family: 'Roboto Condensed', sans-serif;",
          "background-color: #222d32;",
          "font-size: 14px;",
          "padding: 5px;",
          "color: #fff;")

        # Preparing ifelse statements:

        is_both <- length(vals$which_question) > 2
        is_dti <- FALSE
        if (!is.null(input$highlight_dti)) {
          if (input$highlight_dti != "")
            is_dti <- TRUE
        }

        is_log <- FALSE
        if (!is.null(input$repInput_scaled)) {
          if (input$repInput_scaled)
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

        # Create density data frames:

        ds1_sd <- dt_sd %>%
          dplyr::filter(tau_v == out_tauv) %>%
          dplyr::filter(dur == dur_for_sd) %>%
          dplyr::filter(dti == out_dti) %>% 
          stats::na.omit()
        
        ds1_sd <- stats::density(ds1_sd$error)
        
        ds1_sd <- data.frame(x = ds1_sd$x, y = ds1_sd$y)
        if (is_log) ds1_sd$y <- ds1_sd$y / max(ds1_sd$y)
        
        ci1_sd <- subset(
          ds1_sd, x >= vals$sdCI$CI_low & x <= vals$sdCI$CI_high)

        if (is_dti) {
          dti_new <- opts$dti[match(input$highlight_dti,
                                    sd_opts$dti_notes)]
          out_dti_new <- dt_sd$dti[which.min(abs(dt_sd$dti - dti_new))]
          txt_dti_new <- sd_opts$dti_notes[match(out_dti_new,
                                                 sd_opts$dti)]

          ds2_sd <- dt_sd %>%
            dplyr::filter(tau_v == out_tauv) %>%
            dplyr::filter(dur == dur_for_sd) %>%
            dplyr::filter(dti == out_dti_new) %>%
            stats::na.omit()

          ds2_sd <- stats::density(ds2_sd$error)
          ds2_sd <- data.frame(x = ds2_sd$x, y = ds2_sd$y)

          sdCI_new <- bayestestR::ci(ds2_sd$x, 
                                     ci = input_ci,
                                     method = "HDI")

          if (is_log) ds2_sd$y <- ds2_sd$y / max(ds2_sd$y)

          ci2_sd <- subset(
            ds2_sd, x >= sdCI_new$CI_low & x <= sdCI_new$CI_high)

          sd_p1 <- ggplot2::geom_line(
            data = ds2_sd, mapping = ggplot2::aes(x = x, y = y),
            col = pal$mdn, linetype = "dotted")

          sd_p2 <- ggplot2::geom_area(
            data = ci2_sd,
            mapping = ggplot2::aes(x = x, y = y),
            alpha = 0.2, fill = pal$mdn)

          sd_p3 <- ggplot2::geom_segment(
            data = sdCI_new,
            mapping = ggplot2::aes(
              x = .data$CI_low,
              xend = .data$CI_high,
              y = 0, yend = 0,
              col = "est_new", linetype = "est_new"),
            size = .8)

          sd_p4 <- ggplot2::geom_point(
            data = ds2_sd,
            mapping = ggplot2::aes(
              x = stats::median(x), y = 0,
              col = "est_new", shape = "est_new"),
            size = 6)
        }

        lbl <- c(
          paste0("Current error"),
          paste0("Median error + ", vals$sdCI$CI * 100,
                 "% HDI for ", txt_dti))
        brk <- c("now", "est")

        val_fill <- val_col <- c("now" = pal$sea_d, "est" = pal$sea)
        val_linetype <- c("now" = "blank", "est" = "solid")
        val_shape <- c("now" = 19, "est" = 18)

        override_size <- c(.8, .8)
        override_stroke <- c(4, 4)

        if (is_dti) {
          lbl <- c(
            lbl, paste0("Median error + ", vals$sdCI$CI * 100,
                        "% HDI for ", txt_dti_new))
          brk <- c(brk, "est_new")

          val_fill <- val_col <- c(val_fill, "est_new" = pal$mdn)
          val_linetype <- c(val_linetype, "est_new" = "solid")
          val_shape <- c(val_shape, "est_new" = 18)

          override_size <- c(override_size, .8)
          override_stroke <- c(override_stroke, 4)
        }

        y_lab <- ifelse(input$repInput_scaled,
                        "Probability density", "Density")

        #### Speed simulations: -------------------------------------

        output$plot_sd_density <- ggiraph::renderGirafe({
          
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
              alpha = 0.4, fill = pal$sea) +

            {if (is_dti) sd_p3 } +

            ggplot2::geom_segment(
              mapping = ggplot2::aes(
                x = vals$sdCI$CI_low,
                xend = vals$sdCI$CI_high,
                y = 0, yend = 0, col = "est",
                linetype = "est"),
              size = .8) +

            {if (is_dti) sd_p4 } +

            ggplot2::geom_point(
              data = ds1_sd,
              mapping = ggplot2::aes(
                x = stats::median(x), y = 0,
                col = "est", shape = "est"),
              size = 6) +

            { if (!is.null(vals$sdErr))
              ggplot2::geom_point(
                ggplot2::aes(x = vals$sdErr[[2]], y =  0,
                             col = "now", shape = "now"),
                size = 6, alpha = .7)
            } +

            ggplot2::scale_x_continuous(labels = scales::percent) +

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

            theme_movedesign() +
            ggplot2::theme(
              legend.position = "none",
              axis.title.x = ggplot2::element_blank())

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
                            "stroke: #004647;"))))

        }) # end of renderGirafe // plot_sd_density
      }) %>% # end of observe,
      bindEvent(input$create_report) # end of observe

      #### Details: -------------------------------------------------------

      # output$plot_density_details <- ggiraph::renderGirafe({
      #   req(vals$hrCI, vals$sdCI, vals$hrErr)
      #
      #   girafe_height <- 2
      #
      #   if ("Home range" %in% vals$which_question) {
      #     hr_details <- data.frame(
      #       group = c("now", "est"),
      #       labels = c(
      #         paste0("Current AKDE error"),
      #         paste0("Median AKDE error + ", vals$hrCI$CI * 100,
      #                "% HDI for ", vals$report$dur_for_hr, " days")))
      #
      #     hr_details$est <- c(vals$hrErr[[2]],
      #                         vals$report$ds1_hr_median)
      #     hr_details$CI_low <-
      #       c(vals$hrErr[[2]], # vals$hrErr[[1]],
      #         vals$hrCI$CI_low)
      #     hr_details$CI_high <-
      #       c(vals$hrErr[[2]], #vals$hrErr[[3]],
      #         vals$hrCI$CI_high)
      #   }
      #
      #   if ("Speed & distance" %in% vals$which_question) {
      #     sd_details <- data.frame(
      #       group = c("now", "est"),
      #       labels = c(
      #         paste0("Current CTSD error"),
      #         paste0("Median CTSD error + ", vals$sdCI$CI * 100,
      #                "% HDI for ", txt_dti)))
      #
      #     if (vals$is_ctsd) {
      #       req(vals$sdErr)
      #       sd_details$est <- c(vals$sdErr[[2]], median(ds1_sd$x))
      #       sd_details$CI_low <- c(vals$sdErr[[2]], vals$sdCI$CI_low)
      #       sd_details$CI_high <- c(vals$sdErr[[2]], vals$sdCI$CI_high)
      #     } else {
      #       sd_details$est <- c(NA, median(ds1_sd$x))
      #       sd_details$CI_low <- c(NA, vals$sdCI$CI_low)
      #       sd_details$CI_high <- c(NA, vals$sdCI$CI_high)
      #     }
      #   }
      #
      #   if (is_dur) {
      #     hr_details <- hr_details %>%
      #       dplyr::add_row(group = "est_new",
      #                      labels = lbl[3],
      #                      est = median(ds2_hr$x),
      #                      CI_low = vals$hrCI_new$CI_low,
      #                      CI_high = vals$hrCI_new$CI_high)
      #     girafe_height <- 2.5
      #   }
      #
      #   if (is_dti) {
      #     sd_details <- sd_details %>%
      #       dplyr::add_row(group = "est_new",
      #                      labels = lbl[3],
      #                      est = median(ds2_sd$x),
      #                      CI_low = vals$hrCI_new$CI_low,
      #                      CI_high = vals$hrCI_new$CI_high)
      #     girafe_height <- 2.5
      #   }
      #
      #   hr_details <- hr_details %>%
      #     dplyr::mutate(
      #       group = factor(group,
      #                      levels = c("est_new", "est", "now")))
      #
      #   sd_details <- sd_details %>%
      #     dplyr::mutate(
      #       group = factor(group,
      #                      levels = c("est_new", "est", "now")))
      #
      #   p <- ggplot2::ggplot() +
      #     ggplot2::geom_vline(xintercept = 0) +
      #
      #     { if ("Home range" %in% vals$which_question) {
      #       ggplot2::geom_point(
      #         data = hr_details,
      #         mapping = ggplot2::aes(
      #           x = .data$est,
      #           y = .data$group,
      #           group = .data$group,
      #           col = .data$group,
      #           fill = .data$group,
      #           shape = .data$group),
      #         size = 6) }} +
      #
      #     { if ("Home range" %in% vals$which_question) {
      #       ggplot2::geom_segment(
      #         data = hr_details,
      #         mapping = ggplot2::aes(
      #           x = .data$CI_low,
      #           y = .data$group,
      #           group = .data$group,
      #           col = .data$group,
      #           fill = .data$group,
      #           shape = .data$group,
      #           xend = .data$CI_high,
      #           yend = .data$group),
      #         size = .8) }} +
      #
      #     { if ("Speed & distance" %in% vals$which_question) {
      #       ggplot2::geom_point(
      #         data = sd_details,
      #         mapping = ggplot2::aes(
      #           x = .data$est,
      #           y = .data$group,
      #           group = .data$group,
      #           col = .data$group,
      #           fill = .data$group,
      #           shape = .data$group),
      #         size = 6) }} +
      #
      #     { if ("Speed & distance" %in% vals$which_question) {
      #       ggplot2::geom_segment(
      #         data = sd_details,
      #         mapping = ggplot2::aes(
      #           x = .data$CI_low,
      #           y = .data$group,
      #           group = .data$group,
      #           col = .data$group,
      #           fill = .data$group,
      #           shape = .data$group,
      #           xend = .data$CI_high,
      #           yend = .data$group),
      #         size = .8) }} +
      #
      #     ggplot2::scale_x_continuous(
      #       labels = scales::percent) +
      #     ggplot2::scale_y_discrete(
      #       labels = c("___","___","___")) +
      #
      #     ggplot2::scale_color_manual(
      #       name = "", labels = lbl, breaks = brk,
      #       values = val_col) +
      #     ggplot2::scale_fill_manual(
      #       name = "", labels = lbl, breaks = brk,
      #       values = val_fill) +
      #     ggplot2::scale_shape_manual(
      #       name = "", labels = lbl, breaks = brk,
      #       values = val_shape) +
      #
      #     ggplot2::labs(x = "Estimate error (%)",
      #                   y = "") +
      #
      #     theme_movedesign() +
      #     ggplot2::theme(
      #       axis.text.y = ggplot2::element_text(color = "#ffffff"),
      #       legend.position = "bottom",
      #       legend.direction = "vertical",
      #       legend.title = ggplot2::element_blank()) +
      #     ggplot2::guides(
      #       shape = ggplot2::guide_legend(
      #         override.aes = list(
      #           alpha = 1,
      #           size = override_size,
      #           stroke = override_stroke)))
      #   p
      #
      #   ggiraph::girafe(
      #     ggobj = p,
      #     width_svg = 6, height_svg = girafe_height,
      #     options = list(
      #       ggiraph::opts_zoom(max = 5),
      #       ggiraph::opts_hover(
      #         css = paste("r: 4pt;",
      #                     "fill: #006263;",
      #                     "stroke: #006263;")),
      #       ggiraph::opts_selection(
      #         type = "single",
      #         css = paste("r: 4pt;",
      #                     "fill: #004647;",
      #                     "stroke: #004647;"))))
      #
      # }) # end of renderGirafe // plot_density_details
      
      ## Rendering error estimate plots: ----------------------------------
      
      output$plot_hr_error <- ggiraph::renderGirafe({
        
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
        
      }) # end of renderGirafe // plot_hr_error
      
      output$plot_sd_error <- ggiraph::renderGirafe({
        
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
        
      }) # end of renderGirafe // plot_sd_error
    }) # end of observe
    
    output$reportPlots_error <- renderUI({
      req(vals$which_question)
      
      if ("Home range" %in% vals$which_question) {
        out <- out_hr <- ggiraph::girafeOutput(
          outputId = ns("plot_hr_error"),
          width = "100%", height = "100%")
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        out <- out_sd <- ggiraph::girafeOutput(
          outputId = ns("plot_sd_error"),
          width = "100%", height = "100%")
      }
      
      # if (length(vals$which_question) > 1) {
      #   out <- tagList(out_hr, out_sd)
      # } # TODO
      
      return(out)
      
    }) # end of renderUI // reportPlots_error
    
    # REPORT --------------------------------------------------------------
    
    observe({
      req(vals$which_question,
          vals$data_type, 
          vals$is_analyses)

      questions <- NULL
      if ("Home range" %in% vals$which_question) {
        questions <- "Home range"
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        if (length(vals$which_question) > 1) {
          add_to <- ", " } else { add_to <- "" }
        questions <- paste0(questions, add_to, "Speed & distance")
      }
        
      msg_log(
        style = "warning",
        message = paste0("Building ",
                         msg_warning("report"), "..."),
        detail = paste("Current question(s):", questions))
      
      boxnames <- c("analyses", "tables")
      for(i in 1:length(boxnames)) {
        shinyjs::show(id = paste0("repBox_", boxnames[i])) }
      
      # Characteristic parameters:
      
      tau_p <- vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      tau_v <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      
      # Ideal regime:
      
      dur_times <- 30 # rule of thumb: 10 x tau_p
      ideal_dur <- fix_unit(tau_p * dur_times, "seconds", convert = TRUE)
      dur_units <- ideal_dur$unit
      
      ideal_dti <- fix_unit((tau_v/3), "seconds", convert = TRUE)
      dti_units <- ideal_dti$unit
      
      # Current regime:
      
      dur <- dur_units %#% vals$reg$dur %#% vals$reg$dur_unit
      dur <- fix_unit(dur, dur_units)
      dti <- dti_units %#% vals$reg$dti %#% vals$reg$dti_unit
      dti <- fix_unit(dti, dti_units)
      
      vals$hr_col <- vals$ctsd_col <- data.frame(
        hex = pal$sea, css = "var(--sea)")
      
      if ((dur$value %#% dur$unit) <= 
          (ideal_dur$value %#% ideal_dur$unit)) {
        vals$hr_col$hex <- pal$dgr
        vals$hr_col$css <- "var(--danger)"
      }
      
      if ((dti$value %#% dti$unit) <= 
          (ideal_dti$value %#% ideal_dti$unit)) {
        diff_dti <- tau_v / (vals$reg$dti %#% vals$reg$dti_unit)
        vals$ctsd_col$hex <- pal$dgr
        vals$ctsd_col$css <- "var(--danger)"
      } else {
        diff_dti <- 1 / (tau_v / (vals$reg$dti %#% vals$reg$dti_unit))
      }
      
      if ((dti$value %#% dti$unit) <= tau_v) {
        dti_text <-
          span(wrap_none("\u03C4", tags$sub("v"), "/",
                         round(diff_dti, 1)))
      } else {
        dti_text <-
          span(round(diff_dti, 1), icon(name = "xmark"),
               wrap_none("\u03C4", tags$sub("v")))
      }
      
      ## Reporting DATA: --------------------------------------------------
      
      if (vals$data_type == "selected") {
        
        out_species <-
          span("These outputs are based on parameters extracted",
               "from", span(vals$id, class = "cl-grn"),
               "and species", span(vals$species_common,
                                   class = "cl-grn"),
               wrap_none("(", em(vals$species_binom), ")."))
        
      } 
      
      if (vals$data_type == "upload") {
        
        out_species <-
          span("These outputs are based on parameters extracted",
               "from individual", span(vals$id, class = "cl-grn"),
               "and species", wrap_none(em(vals$species,
                                           class = "cl-grn"), "."))
        
      } 
      
      if (vals$data_type == "simulated") {
        
        out_species <-
          span("These outputs are based on a",
               span("simulated", class = "cl-grn"),
               "dataset.")
      }
      
      vals$report$species <- p(
        out_species,
        "Please see the",
        icon("paw", class = "cl-sea"),
        span("Species", class = "cl-sea"),
        "parameters above for more details.")
      
      ## Reporting REGIME: ------------------------------------------------
      
      ### Home range estimation:
      
      if ("Home range" %in% vals$which_question) {
        req(vals$hrCI)
        
        N1 <- scales::label_comma(accuracy = 1)(vals$device_N1)
        
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
        req(vals$sdCI)
        
        if(vals$is_ctsd) {
          N2 <- scales::label_comma(accuracy = 1)(vals$device_N2)
        } else { N2 <- vals$device_N2 }
        
        out_regime <- out_reg_ctsd <-
          p("The minimum", span("sampling interval", class = "cl-sea"),
            "for", span("speed & distance", class = "cl-grn"),
            "estimation should be less than the",
            span("velocity autocorrelation", class = "cl-sea"),
            wrap_none("parameter (", tags$sub("v"), "),"),
            "or \u2264", wrap_none(ideal_dti$value, " ", ideal_dti$unit,
                                   css = "cl-grn", end = "."),
            "Your current interval (\u0394t) is",
            dti_text,
            "\u2248", wrap_none(dti$value, " ", dti$unit,
                                color = vals$ctsd_col[1], end = ","),
            "resulting in an effective sample size equivalent to",
            N2, "independent locations.")
        
      } # end of "Speed & distance"
      
      ### Both home range and speed & distance:
      
      if (length(vals$which_question) > 1) {
        out_regime <- tagList(out_reg_hr, out_reg_ctsd)
      }

      vals$report$regime <- out_regime
      
      ## Reporting OUTPUTS: -----------------------------------------------
      
      if ("Home range" %in% vals$which_question) {
        req(vals$hrCI, vals$hrErr)
        
        hrCI <- c(round(vals$hrCI$CI_low * 100, 1),
                  round(vals$hrCI$CI * 100, 0),
                  round(vals$hrCI$CI_high * 100, 1))
        
        txt_level <- ifelse(
          vals$hrCI$CI_high < .3 &
            vals$hrCI$CI_low > -.3,
          "and with low", "but with high")
        
        dur_opts <- 2^seq(1, 12, by = 1)
        index_dur <- which.min(abs(
          dur_opts - ("days" %#% dur$value %#% dur$unit)))
        plotted_dur <- dur_opts[index_dur]
        
        out_analyses <- NULL
        
        if ((dur$value %#% dur$unit) >= 
            (ideal_dur$value %#% ideal_dur$unit)) {
          
          vals$report$is_hr <- TRUE
          out_hr1 <- 
            span("Your current sampling duration is likely sufficient",
                 "for", span("home range", class = "cl-grn"),
                 "estimation,", txt_level, "uncertainty.")
          
        } else {
          
          vals$report$is_hr <- FALSE
          out_hr1 <- 
            span("Your current sampling duration may be insufficient",
                 "for", span("home range", class = "cl-grn"),
                 "estimation.")
        }
        
        out_hr2 <- 
          span("Keep in mind that, for a similar duration of",
               plotted_dur, "days, there is a",
               wrap_none(hrCI[2], "%", css = "cl-blk"),
               "probability that the relative error will lie within",
               wrap_none(hrCI[1], "%", css = "cl-blk"),
               "and", wrap_none(hrCI[2], "%", end = ".", css = "cl-blk"))
        
        out_analyses <- out_hr <- p(
          out_hr1,
          "Your error estimate",
          "based on a single simulation was",
          wrap_none(round(vals$hrErr[[2]] * 100, 1), "%."),
          out_hr2)
        
      } # end of 'Home range'
      
      ## Speed and distance estimation:
      
      if ("Speed & distance" %in% vals$which_question) {
        
        if(vals$is_ctsd) {
          req(vals$sdCI, vals$speedErr)
          
          sdCI <- c(round(vals$sdCI$CI_low * 100, 1),
                    round(vals$sdCI$CI * 100, 0),
                    round(vals$sdCI$CI_high * 100, 1))
          
          txt_level <- ifelse(
            vals$sdCI$CI_high < .3 &
              vals$sdCI$CI_low > -.3,
            "and with low", "but with high")
          
          ctsd_err <- vals$speedErr[[2]]
        }
        
        dti_options <- movedesign::sims_speed[[1]] %>%
          dplyr::select(dti, dti_notes) %>%
          unique()
        
        index_dti <- which.min(
          abs(dti_options$dti - (dti$value %#% dti$unit)))
        plotted_dti <- sub('^\\w+\\s\\w+\\s\\w+\\s', '', 
                           dti_options[index_dti, 2])
        
        if ((dti$value %#% dti$unit) <= 
            (ideal_dti$value %#% ideal_dti$unit)) {
          
          vals$report$is_ctsd <- TRUE
          out_ctsd1 <- 
            span("Your current sampling interval is likely sufficient",
                 "for", span("speed & distance", class = "cl-grn"),
                 "estimation,", txt_level, "uncertainty.")
          
        } else {
          if (vals$device_N2 > 0) {
            vals$report$is_ctsd <- FALSE
            out_ctsd1 <-
              span("Your current sampling interval may be insufficient",
                   "for", span("speed & distance", class = "cl-grn"),
                   "estimation.")
            
          } else {
            vals$report$is_ctsd <- FALSE
            out_ctsd1 <-
              span("Your current sampling interval was too coarse",
                   "for", span("speed & distance", class = "cl-grn"),
                   "estimation.")
            
          }
        }

        if(vals$is_ctsd) {
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
            "Your error estimate",
            "based on a single simulation was",
            wrap_none(round(ctsd_err * 100, 1), "%."),
            out_ctsd2)
          
        } else {
          out_analyses <- out_ctsd <- p(out_ctsd1)
        }
        
      } # end of "Speed & distance"
      
      req(length(vals$which_question) == 1)
      vals$report$analyses <- out_analyses
      
    }) %>% # end of observe,
      bindEvent(input$create_report)
    
    observe({
      req(length(vals$which_question) > 1)
      
      ### Both home range and speed & distance:
      
      is_hr <- vals$report$is_hr
      is_ctsd <- vals$report$is_ctsd
      
      if(is_hr & !is_ctsd) {
        out <-
          span("Your current tracking regime",
               "is likely sufficient",
               "for", span("home range", class = "cl-grn"),
               "estimation, but insufficient",
               "for", span("speed & distance", class = "cl-dgr"),
               "estimation.")
        
        if (vals$device_N2 == 0)
          out <-
            span("Your current tracking regime",
                 "is likely sufficient",
                 "for", span("home range", class = "cl-grn"),
                 "estimation, and is inappropriate for",
                 span("speed & distance", class = "cl-dgr"),
                 "estimation.")
      }

      if (!is_hr & is_ctsd) {
        out <-
          span("Your current tracking regime",
               "is likely insufficient",
               "for", span("home range", class = "cl-dgr"),
               "estimation, but sufficient",
               "for", span("speed & distance", class = "cl-grn"),
               "estimation.")
      }

      if (is_hr & is_ctsd) {
        out <-
          span("Your current tracking regime",
               "is likely sufficient for both",
               span("home range", class = "cl-grn"),
               "estimation and for",
               span("speed & distance", class = "cl-grn"),
               "estimation.")

      }

      if (!is_hr & !is_ctsd) {

        out <-
          span("Your current tracking regime",
               "may be insufficient for both",
               span("home range", class = "cl-dgr"),
               "and", span("speed & distance", class = "cl-dgr"),
               "estimation.")
        
        if (vals$device_N2 == 0)
          out <-
            span("Your current tracking regime",
                 "may be insufficient for",
                 span("home range", class = "cl-dgr"),
                 "estimation, and is inappropriate for",
                 span("speed & distance", class = "cl-dgr"),
                 "estimation.")
        
      }
      
      out_analyses <- p(
        out,
        "For home range estimation, your error estimate",
        "based on a single simulation was",
        wrap_none(round(vals$hrErr[[2]] * 100, 1), "%"),
        wrap_none("[", round(vals$hrErr[[1]] * 100, 1),
                  ", ", round(vals$hrErr[[3]] * 100, 1),
                  "%]", css = "cl-blk", end = "."),
        
        if(vals$device_N2 > 0) {
          span("For speed estimation, your error estimate",
               "based on a single simulation was",
               wrap_none(round(vals$speedErr[[2]] * 100, 1), "%"),
               wrap_none("[", round(vals$speedErr[[1]] * 100, 1),
                         ", ", round(vals$speedErr[[3]] * 100, 1),
                         "%]", css = "cl-blk", end = "."))
        } else {
          span("Your sampling interval was",
               "too coarse, so it was not possible to estimate speed.")
        }
      )

      vals$report$analyses <- out_analyses
      
    }) %>% # end of observe,
      bindEvent(input$create_report)
    
    # Rendering complete report: ----------------------------------------
    
    observe({
      req(vals$which_question)
      
      if (length(vals$which_question) > 1) {
        shinyjs::hide(id = "section-comparison")
        shinyjs::show(id = "end_report_both")
      } else {
        shinyjs::show(id = "section-comparison")
        shinyjs::hide(id = "end_report_both")
      }
      
    }) %>% bindEvent(input$create_report)
    
    output$end_report <- renderUI({
      
      out <- tagList(
        vals$report$species,
        vals$report$regime,
        
        div(width = 12, align = "center",
            style = "z-index: 999;",
            
            shinyWidgets::switchInput(
              inputId = ns("repInput_scaled"),
              label = span(icon("wrench"),
                           "Scaled to 1"),
              labelWidth = "100px")),
        
        if (length(vals$which_question) == 1 &
            "Home range" %in% vals$which_question) {
          ggiraph::girafeOutput(
            outputId = ns("plot_hr_density"),
            width = "100%", height = "100%") },
        
        if (length(vals$which_question) == 1 &
            "Speed & distance" %in% vals$which_question) {
          ggiraph::girafeOutput(
            outputId = ns("plot_sd_density"),
            width = "100%", height = "100%") },
        
        if (length(vals$which_question) == 1) {
          ggiraph::girafeOutput(
            outputId = ns("plot_density_details"),
            width = "100%", height = "100%") },
        
        if (length(vals$which_question) == 1) {
          vals$report$analyses
        }
        
      ) # end of tagList
    }) %>% # end of renderUI // end_report
      bindEvent(input$create_report)
    
    output$end_report_both <- renderUI({
      req(length(vals$which_question) > 1)
      
      out <- tagList(
        splitLayout(
          cellWidths = c("50%", "50%"), 
          tagList(
            p(span("Home range", class = "cl-sea")) %>%
              tagAppendAttributes(class = "subheader"),
            ggiraph::girafeOutput(
              outputId = ns("plot_hr_density"),
              width = "100%", height = "100%")), 
          tagList(
            p(span("Speed & distance", class = "cl-sea")) %>%
              tagAppendAttributes(class = "subheader"),
            ggiraph::girafeOutput(
              outputId = ns("plot_sd_density"),
              width = "100%", height = "100%"))),
        
        ggiraph::girafeOutput(
          outputId = ns("plot_density_details"),
          width = "100%", height = "100%"),
        
        vals$report$analyses
        
      ) # end of tagList
    }) %>% # end of renderUI // end_report_both
      bindEvent(input$create_report)
    
    ## Reporting COMPARISON (if available): -------------------------------
    
    observe({
      out_comp <- out_comp_hr <- span("")
      
      if (length(vals$which_question) == 1 &
          "Home range" %in% vals$which_question) {
        req(input$highlight_dur)
        
        highlighted_dur <- as.numeric(input$highlight_dur)
        
        CI <- round(vals$hrCI_new$CI * 100, 0)
        LCI <- round(vals$hrCI_new$CI_low * 100, 1)
        UCI <- round(vals$hrCI_new$CI_high * 100, 1)
        
        txt_level <- ifelse(
          vals$hrCI_new$CI_high < .3 & vals$hrCI_new$CI_low > -.3,
          "and with low", "but with high")
        
        ideal_dur <- fix_unit(
          ("days" %#% vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]) * 10,
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
        
        CI <- round(vals$sdCI_new$CI * 100, 0)
        LCI <- round(vals$sdCI_new$CI_low * 100, 1)
        UCI <- round(vals$sdCI_new$CI_high * 100, 1)
        
        txt_level <- ifelse(
          vals$sdCI_new$CI_high < .3 & vals$sdCI_new$CI_low > -.3,
          "and with low", "but with high")
        
        ideal_dti <- fix_unit(
          (vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]) / 3, "seconds")
        
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
              "estimation.", "For a sampling interval of",
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
    
    # TABLE ---------------------------------------------------------------
    ## Final report table (combining previous results): -------------------
    
    reportRow <- reactive({
      
      dt_regs <- vals$dt_regs
      dt_regs <- dt_regs[nrow(dt_regs), ]
      
      if ("Home range" %in% vals$which_question) {
        req(vals$dt_hr)
        dt_hr <- vals$dt_hr
        dt_hr <- dt_hr[nrow(dt_hr), ]
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        req(vals$dt_sd)
        dt_sd <- vals$dt_sd
        dt_sd <- dt_sd[nrow(dt_sd), ]
      }
      
      dat <- data.frame(
        device = character(0),
        taup = character(0),
        tauv = character(0),
        sigma = character(0),
        dur = character(0),
        dti = character(0),
        n = numeric(0),
        N1 = numeric(0),
        N2 = numeric(0),
        area = character(0),
        area_err = numeric(0),
        area_err_min = numeric(0),
        area_err_max = numeric(0),
        ctsd = character(0),
        ctsd_err = numeric(0),
        ctsd_err_min = numeric(0),
        ctsd_err_max = numeric(0),
        dist = character(0),
        dist_err = numeric(0))
      
      tmpdat <- dt_regs %>% dplyr::select(.data$device:.data$N2)
      tmpdat <- suppressMessages(dplyr::full_join(dat, tmpdat))
      
      tmpdat$taup <- paste(
        scales::label_comma(accuracy = .1)(vals$tau_p0$value[2]),
        abbrv_unit(vals$tau_p0$unit[2]))
      
      tmpdat$tauv <- paste(
        scales::label_comma(accuracy = .1)(vals$tau_v0$value[2]),
        abbrv_unit(vals$tau_v0$unit[2]))
      
      out <- fix_unit(vals$sigma0$value[2],
                      vals$sigma0$unit[2], convert = T)
      tmpdat$sigma <- paste(out$value, abbrv_unit(out$unit))
      
      if ("Home range" %in% vals$which_question) {
        tmphr <- dt_hr %>% dplyr::select(.data$taup:.data$area_err_max)
        tmpdat <- suppressMessages(dplyr::full_join(tmpdat, tmphr))
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        tmpsd <- dt_sd %>% dplyr::select(.data$tauv:.data$dist_err)
        tmpdat <- suppressMessages(dplyr::full_join(tmpdat, tmpsd))
      }
      
      if (nrow(tmpdat == 2)) {
        tmpdat <- dplyr::coalesce(tmpdat[1,], tmpdat[2,])
      }
      
      return(tmpdat)
      
    }) # end of reactive
    
    observe({
      req(vals$active_tab == 'report', 
          vals$dt_regs)
      
      vals$report_full <<- rbind(vals$report_full, reportRow())
      dat <- vals$report_full
      
      if (nrow(dat) >= 2) {
        if (all(dat[nrow(dat)-1,2:6] == dat[nrow(dat),2:6])) {
          dat <- dplyr::coalesce(dat[1, ], dat[2, ])
        }}
      
      vals$report_full <- dplyr::distinct(dat)
      
    }) %>% # end of observe
      bindEvent(input$create_report)
    
    
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
      
      if ("Home range" %in% vals$which_question) {
        choices_subset <- choices[c(1:8, 10:13)]
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        choices_subset <- choices[c(1:7, 9, 14:19)]
      }
      
      nms <- data.frame(
        device = "Type",
        taup = "\u03C4\u209A",
        tauv = "\u03C4\u1D65",
        sigma = "\u03C3",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = "N (area)",
        N2 = "N (speed)",
        area = "HR area",
        area_err = "Error",
        area_err_min = "Error (min)",
        area_err_max = "Error (max)",
        ctsd = "CTSD",
        ctsd_err = "Error",
        ctsd_err_min = "Error (min)",
        ctsd_err_max = "Error (max)",
        dist = "Distance",
        dist_err = "Error")
      
      dat <- vals$report_full
      if (!is.null(choices_subset)) {
        dat <- dat %>% dplyr::select(choices_subset)
      }
      
      nms_sizes <- c("n", "N1", "N2")
      if ("Home range" %in% vals$which_question) {
        nms_sizes <- c("n", "N1")
        nms_hr <- choices[c(10:13)]
        colgroups <- list(
          reactable::colGroup(
            name = "Home range",
            columns = nms_hr),
          reactable::colGroup(
            name = "Sample sizes", 
            columns = nms_sizes))
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        nms_sizes <- c("n", "N2")
        nms_ctsd <- choices[c(14:17)]
        nms_dist <- choices[c(18:19)]
        colgroups <- list(
          reactable::colGroup(
            name = "Speed",
            columns = nms_ctsd),
          reactable::colGroup(
            name = "Distance",
            columns = nms_dist),
          reactable::colGroup(
            name = "Sample sizes", 
            columns = nms_sizes))
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
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 0)) },
        N1 = if ("N1" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "N1"],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)) },
        N2 = if ("N2" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "N2"],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)) },
        area = if ("area" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area"]) },
        area_err = if ("area_err" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err"],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        area_err_min = if ("area_err_min" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err_min"],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        area_err_max = if ("area_err_max" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err_max"],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        ctsd = if ("ctsd" %in% choices_subset) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd"]) },
        ctsd_err = if ("ctsd_err" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd_err"],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        ctsd_err_min = if ("ctsd_err_min" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd_err_min"],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        ctsd_err_max = if ("ctsd_err_max" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "ctsd_err_max"],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        dist = if ("dist" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "dist"]) },
        dist_err = if ("dist_err" %in% choices_subset) { 
          reactable::colDef(
            minWidth = 80, name = nms[1, "dist_err"],
            style = list(color = format_perc),
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
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_report_ui("tab_report_1")

## To be copied in the server
# mod_tab_report_server("tab_report_1")
