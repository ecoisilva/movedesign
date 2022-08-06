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

          shinydashboardPlus::box(
            id = "intro_report",
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = FALSE, closable = FALSE,

            actionButton(
              inputId = ns("create_report"),
              icon = icon("bookmark"),
              label = span("Build", span("report", style = "var(--sea)")),
              width = "100%")

          ), # end of box // intro_report

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
                        # p(style = "padding-top: 0px"),

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

                        # shinyWidgets::radioGroupButtons(
                        #   inputId = ns("reportInput_scenarios"),
                        #   label = "Operating life:",
                        #   choices = c("Best case" = "best",
                        #               "Average case" = "average",
                        #               "Worst case" = "worst"),
                        #   justified = TRUE,
                        #   direction = "vertical",
                        #   checkIcon = list(
                        #     yes = icon("ok", lib = "glyphicon"))),
                        #
                        # helpText("These are the three case scenarios for",
                        #   "your tracking device operating life.")

                      ) # end of panel
                    )) # end of tabBox // repTabs_details
              ), # end of box // repBox_details

              shinydashboardPlus::box(
                title = NULL,
                width = NULL,
                headerBorder = FALSE,

                uiOutput(ns("repInfo_hrErr")),
                uiOutput(ns("repInfo_ctsdErr")),
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

                  ), # end of panel (1 out of 3)

                  tabPanel(title = "Tracking regime:",
                           value = ns("repPanel_regime"),
                           icon = icon("stopwatch"),

                           splitLayout(
                             uiOutput(ns("repBlock_dur")),
                             uiOutput(ns("repBlock_dti")))

                  ), # end of panel (2 out of 3)

                  NULL

                  # tabPanel(title = "Sample sizes:",
                  #          value = ns("repPanel_sizes"),
                  #          icon = icon("calculator"),
                  #
                  #          uiOutput(ns("repUI_sizes"))
                  #
                  # )  # end of panel (3 out of 3)

                ) # end of tabBox
              )), # end of box // regBox_pars

          div(class = "col-lg-6 no-padding-left tabBox_noheaders",
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

                           uiOutput(ns("repUI_sizes"))

                  )  # end of panel (3 out of 3)

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

                p("Quick comparison with other",
                  wrap_none(span("regimes", class = "cl-sea"), "?")) %>%
                  tagAppendAttributes(class = "subheader"),

                column(width = 12, align = "center",
                       style = paste("z-index: 999;"),
                #                      "margin-bottom: -58px;"),
                       uiOutput(ns("highlighting_reg"))),

                uiOutput(outputId = ns("reportPlots_error")),

                helpText(
                  style = "text-align: right;",

                  HTML("Tooltip reports mean estimate",
                       "(low \u2014 high 95% CIs).")),

                helpText(
                  "Note: This comparison is based on aggregated",
                  "information from 400 simulations, so mean",
                  "estimates will not match your current values."),

                uiOutput(ns("end_comparison"))

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

            reactable::reactableOutput(ns("endTable")),

            footer = tagList(
              uiOutput(outputId = ns("reportInput_vars"))

            )) # end of box // tables

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

    # shinyjs::hide(id = "report_comparison")

    observe({
      boxnames <- c("analyses", "tables")

      if (is.null(vals$which_question)) {
        for(i in 1:length(boxnames)) {
          shinyjs::hide(id = paste0("repBox_", boxnames[i])) }
      } else {
        req(input$create_report)
        for(i in 1:length(boxnames)) {
          shinyjs::show(id = paste0("repBox_", boxnames[i])) }
      }

    }) # end of observe

    ## Rendering research questions: --------------------------------------

    output$report_question <- renderUI({

      ui_hr <- staticBlock("Home range", active = FALSE)
      ui_sd <- staticBlock("Speed & distance", active = FALSE)

      if (!is.null(vals$which_question)) {
        if ("Home range" %in% vals$which_question) {
          ui_hr <- staticBlock("Home range", active = TRUE)
        }

        if ("Speed & distance" %in% vals$which_question) {
          ui_sd <- staticBlock("Speed & distance", active = TRUE)
        }
        out <- tagList(ui_hr, ui_sd)
      } else {
        out <- tagList(ui_hr, ui_sd)
      }

      return(out)

    }) # end of renderUI // report_question

    ## Rendering device limitations: --------------------------------------

    output$report_device <- renderUI({
      req(vals$which_limitations)

      if("loss" %in% vals$which_limitations) {
        ui_loss <- staticBlock(paste0(vals$loss, "%"),
                               active = TRUE)
      } else if (!("loss" %in% vals$which_limitations)) {
        ui_loss <- staticBlock("No data loss",
                               active = FALSE)
      }

      if("error" %in% vals$which_limitations) {
        ui_error <- staticBlock(paste(vals$error, "meters"),
                                active = TRUE)
      } else if (!("error" %in% vals$which_limitations)) {
        ui_error <- staticBlock("No error",
                                active = FALSE)
      }

      if("limit" %in% vals$which_limitations) {
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
        dat <- sims_speed[[2]]
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
            accuracy = .1)(vals$tau_p0), vals$tau_p0_units),
        subtitle = if(vals$data_type != "simulated") {
          paste(
            ifelse(vals$tau_p0_min == 0,
                   "0",
                   scales::label_comma(accuracy = .1)(vals$tau_p0_min)),
            "—",
            scales::label_comma(accuracy = .1)(vals$tau_p0_max))
          } else { ""})

    }) # end of renderUI // repBlock_taup

    output$repBlock_tauv <- shiny::renderUI({
      req(vals$tau_v0)

      parBlock(
        header = span(
            HTML(paste0("Velocity autocorrelation ",
                        "(\u03C4", tags$sub("v"), ")"))),
        value =
          paste(scales::label_comma(
            accuracy = .1)(vals$tau_v0), vals$tau_v0_units),
        subtitle = if(vals$data_type != "simulated") {
          paste(
            ifelse(vals$tau_v0_min == 0,
                   "0",
                   scales::label_comma(accuracy = .1)(vals$tau_v0_min)),
            "—",
            scales::label_comma(accuracy = .1)(vals$tau_v0_max))
          } else { "" })

    }) # end of renderUI // repBlock_tauv

    output$repBlock_sigma <- shiny::renderUI({
      req(vals$sigma0)

      sig <- "km^2" %#% vals$sigma0 %#% "m^2"
      sig <- fix_unit(sig, unit = "km^2", ui = TRUE)

      parBlock(
        header = span(HTML("Semi-variance (\u03C3)")),
        value = span(HTML("&nbsp;", sig$value, sig$unit)),
        subtitle = if(vals$data_type != "simulated") {
          paste(
            ifelse(vals$sigma0_min == 0,
                   "0",
                   scales::label_comma(
                     accuracy = .1)(vals$sigma0_min)),
            "—",
            scales::label_comma(
              accuracy = .1)(vals$sigma0_max)) } else { "" })

    }) # end of renderUI // repBlock_sigma

    ## Tracking regime: ---------------------------------------------------

    output$repBlock_dur <- renderUI({
      req(vals$dur0_dev)

      out <- fix_unit(vals$dur0_dev, vals$dur0_units_dev,
                      convert = TRUE)
      parBlock(
        header = "Sampling duration",
        value = paste(out[1], out[2]))

    }) # end of renderUI // repBlock_dur

    output$repBlock_dti <- renderUI({
      req(vals$dti0_dev, vals$dti0_units_dev)

      out <- fix_unit(vals$dti0_dev, vals$dti0_units_dev,
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
                 value = vals$hrErr,
                 min = vals$hrErr_min,
                 max = vals$hrErr_max,
                 rightBorder = FALSE)

    }) # end of renderUI // repInfo_hrErr

    output$repInfo_ctsdErr <- shiny::renderUI({
      req(vals$ctsdErr, "Speed & distance" %in% vals$which_question)

      errorBlock(icon = "radiation",
                 text = "Speed error",
                 value = vals$ctsdErr,
                 min = vals$ctsdErr_min,
                 max = vals$ctsdErr_max,
                 rightBorder = FALSE)

    }) # end of renderUI // repInfo_ctsdErr
    
    
    # VALIDATION ----------------------------------------------------------

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
            size = "xs")
        }
      }

    }) # end of observer

    # CALCULATIONS ------------------------------------------------------
    ## Credible intervals for HR estimation: ----------------------------

    observe({
      req(vals$active_tab == 'report')
      req(vals$tau_p0, vals$tau_p0_units,
          vals$dur0_dev, vals$dur0_units_dev)

      input_taup <- "days" %#% vals$tau_p0 %#% vals$tau_p0_units
      input_dur <- "days" %#% vals$dur0_dev %#% vals$dur0_units_dev

      dat <- sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))

      index_taup <- which.min(abs(dat$tau_p - input_taup))
      out_taup <- dat$tau_p[index_taup]

      index_dur <- which.min(abs(dat$dur - input_dur))
      out_dur <- dat$dur[index_dur]

      newdat <- dat %>%
        dplyr::filter(tau_p == out_taup) %>%
        dplyr::filter(duration == out_dur)

      CI <- ifelse(is.null(input$ci), .95, input$ci/100)
      vals$hrCI <- bayestestR::ci(newdat$error, ci = CI, method = "HDI")

    }) # end of observe

    observe({
      req(vals$active_tab == 'report')
      req(vals$tau_v0, vals$tau_v0_units,
          vals$dti0_dev, vals$dti0_units_dev)

      input_tauv <- vals$tau_v0 %#% vals$tau_v0_units
      input_dur <- "days" %#% vals$dur0_dev %#% vals$dur0_units_dev
      input_dti <- vals$dti0_dev %#% vals$dti0_units_dev

      dat <- sims_speed[[1]] %>%
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

      CI <- ifelse(is.null(input$ci), .95, input$ci/100)
      vals$sdCI <- bayestestR::ci(newdat$error, ci = CI,
                                  method = "HDI")

    }) # end of observe

    observe({ # For new/modified regimes:
      req(input$highlight_dur > 0)
      # shinyjs::show(id = "report_comparison")

      input_taup <- "days" %#% vals$tau_p0 %#% vals$tau_p0_units

      dat <- sims_hrange[[1]] %>%
        dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
        dplyr::mutate(duration = round("days" %#% duration, 1))

      index_taup <- which.min(abs(dat$tau_p - input_taup))
      out_taup <- dat$tau_p[index_taup]
      out_dur <- as.numeric(input$highlight_dur)

      newdat <- dat %>%
        dplyr::filter(tau_p == out_taup) %>%
        dplyr::filter(duration == out_dur)

      CI <- ifelse(is.null(input$ci), .95, input$ci/100)
      vals$hrCI_new <- bayestestR::ci(newdat$error,
                                      ci = CI, method = "HDI")

    }) %>% # end of observe,
      bindEvent(input$highlight_dur)

    # BLOCKS ------------------------------------------------------------
    ## Species & individual: --------------------------------------------

    output$report_plot <- ggiraph::renderGirafe({
      req(vals$data1)

      newdat <- vals$data1
      yrange <- diff(range(newdat$y))
      xrange <- diff(range(newdat$x))

      if(yrange < 1.5 * xrange) {
        ymin <- min(newdat$y) - yrange * .3
        ymax <- max(newdat$y) + yrange * .3
      } else if(yrange < 2 * xrange) {
        ymin <- min(newdat$y) - yrange * .5
        ymax <- max(newdat$y) + yrange * .5
      } else {
        ymin <- min(newdat$y)
        ymax <- max(newdat$y)
      }

      if(xrange < 2 * yrange) {
        xmin <- min(newdat$x) - xrange * .5
        xmax <- max(newdat$x) + xrange * .5
      } else {
        xmin <- min(newdat$x)
        xmax <- max(newdat$x)
      }

      p <- ggplot2::ggplot() +

        ggiraph::geom_path_interactive(
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

      tmpdat <- dt_regs %>% dplyr::select(device:N2)
      tmpdat <- suppressMessages(dplyr::full_join(dat, tmpdat))

      if ("Home range" %in% vals$which_question) {
        tmphr <- dt_hr %>% dplyr::select(taup:area_err_max)
        tmpdat <- suppressMessages(dplyr::full_join(tmpdat, tmphr))
      }

      if (nrow(tmpdat == 2)) {
        tmpdat <- dplyr::coalesce(tmpdat[1,], tmpdat[2,])
      }

      return(tmpdat)

    }) # end of reactive

    observe({
      req(vals$active_tab == 'report', vals$dt_regs)

      vals$report_full <<- rbind(vals$report_full,
                                 reportRow())

    }) %>% # end of observe
      bindEvent(input$create_report)

    observe({
      req(vals$is_analyses, vals$which_question)

      nms <- nms_subset <- c(
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

      choices <- choices_subset <- names(nms)
      names(choices) <- names(choices_subset) <- nms %>% as.vector()

      if ("Home range" %in% vals$which_question) {

        nms_subset <- c("device",
                        "taup",
                        "dur",
                        "n",
                        "N1",
                        "area",
                        "area_err",
                        "area_err_min",
                        "area_err_max")
        nms_subset <- nms[nms_subset]

        choices_subset <- names(nms_subset)
        names(choices_subset) <- nms_subset %>% as.vector()
      }

      output$reportInput_vars <- renderUI({

        div(style = paste("width: 30%;",
                          "float: right;",
                          "display: flex;"),

            shinyWidgets::pickerInput(
              inputId = ns("report_vars"),
              width = "120px",
              label = span("Select columns:",
                           class = "txt-label",
                           style = paste("text-align: right;",
                                         "margin: 0 9px 0 0;",
                                         "line-height: 2.4;")),
              choices = choices,
              selected = choices_subset,
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE))

      }) # end of renderUI
    }) # end of observe

    output$endTable <- reactable::renderReactable({
      req(vals$report_full, input$report_vars)

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
      if (!is.null(input$report_vars)) {
        dat <- dat %>% dplyr::select(input$report_vars)
      }

      namedcolumns <- list(
        device = if("device" %in% input$report_vars) {
          reactable::colDef(
            name = nms[1, "device"]) },
        taup = if("taup" %in% input$report_vars) {
          reactable::colDef(
            name = nms[1, "taup"],
            style = list(fontWeight = "bold")) },
        tauv = if("tauv" %in% input$report_vars) {
          reactable::colDef(
            name = nms[1, "tauv"],
            style = list(fontWeight = "bold")) },
        sigma = if("sigma" %in% input$report_vars) {
          reactable::colDef(
            minWidth = 60, name = nms[1, "sigma"]) },
        dur = if("dur" %in% input$report_vars) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "dur"],
            style = list(fontWeight = "bold")) },
        dti = if("dti" %in% input$report_vars) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "dti"],
            style = list(fontWeight = "bold")) },
        n = if("n" %in% input$report_vars) {
          reactable::colDef(
            name = nms[1, "n"],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 0)) },
        N1 = if("N1" %in% input$report_vars) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "N1"],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)) },
        N2 = if("N2" %in% input$report_vars) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "N2"],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)) },
        area = if("area" %in% input$report_vars) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area"]) },
        area_err = if("area_err" %in% input$report_vars) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err"],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        area_err_min = if("area_err_min" %in% input$report_vars) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err_min"],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)) },
        area_err_max = if("area_err_max" %in% input$report_vars) {
          reactable::colDef(
            minWidth = 80, name = nms[1, "area_err_max"],
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

        columns = namedcolumns

      ) # end of reactable

      return(dt)

    }) # end of renderReactable // endTable

    # PLOTS ---------------------------------------------------------------

    observe({
      req(vals$active_tab == 'report')
      req(vals$tau_p0, vals$tau_p0_units,
          vals$dur0_dev, vals$dur0_units_dev,
          vals$dti0_dev, vals$dti0_units_dev,
          input$ci)

      input_taup <- "days" %#% vals$tau_p0 %#% vals$tau_p0_units
      input_dur <- "days" %#% vals$dur0_dev %#% vals$dur0_units_dev

      input_tauv <- vals$tau_v0 %#% vals$tau_v0_units
      input_dti <- vals$dti0_dev %#% vals$dti0_units_dev

      tooltip_css <- paste(
        "font-family: 'Roboto Condensed', sans-serif;",
        "background-color: #222d32;",
        "font-size: 14px;",
        "padding: 5px;",
        "color: #fff;")

      ## Rendering density plots: -----------------------------------------

      output$plot_hr_density <- ggiraph::renderGirafe({

        reveal_if <- FALSE
        if (!is.null(input$highlight_dur)) {
          if (!is.na(as.numeric(input$highlight_dur))) reveal_if <- TRUE
        }

        dat <- sims_hrange[[1]] %>%
          dplyr::mutate(tau_p = round("days" %#% tau_p, 1)) %>%
          dplyr::mutate(duration = round("days" %#% duration, 1))

        index_taup <- which.min(abs(dat$tau_p - input_taup))
        out_taup <- dat$tau_p[index_taup]

        index_dur <- which.min(abs(dat$dur - input_dur))
        out_dur <- dat$dur[index_dur]

        df1 <- dat %>%
          dplyr::filter(tau_p == out_taup) %>%
          dplyr::filter(duration == out_dur) %>%
          stats::na.omit()

        ds1 <- stats::density(df1$error)
        ds1 <- data.frame(x = ds1$x, y = ds1$y)

        if(input$repInput_scaled) ds1$y <- ds1$y / max(ds1$y)
        ds1_ci <- subset(ds1, x >= vals$hrCI$CI_low &
                           x <= vals$hrCI$CI_high)

        if (reveal_if) {
          dur_new <- as.numeric(input$highlight_dur)
          index_dur_new <- which.min(abs(dat$dur - dur_new))
          out_dur_new <- dat$dur[index_dur_new]


          df2 <- dat %>%
            dplyr::filter(tau_p == out_taup) %>%
            dplyr::filter(duration == out_dur_new) %>%
            stats::na.omit()

          ds2 <- stats::density(df2$error)
          ds2 <- data.frame(x = ds2$x, y = ds2$y)

          CI <- ifelse(is.null(input$ci), .95, input$ci/100)
          ci2 <- bayestestR::ci(df2$error, ci = CI, method = "HDI")
          if(input$repInput_scaled) ds2$y <- ds2$y / max(ds2$y)
          ds2_ci <- subset(ds2, x >= ci2$CI_low &
                             x <= ci2$CI_high)
          p1 <- ggplot2::geom_line(
            data = ds2, mapping = ggplot2::aes(x = x, y = y),
            col = pal$mdn, linetype = "dotted")

          p2 <- ggplot2::geom_area(
            data = ds2_ci,
            mapping = ggplot2::aes(x = x, y = y),
            alpha = 0.2, fill = pal$mdn)

          p3 <- ggplot2::geom_segment(
            data = ci2,
            mapping = ggplot2::aes(
              x = CI_low,
              xend = CI_high,
              y = 0, yend = 0,
              col = "est_new", linetype = "est_new"),
            size = .8)
          p4 <- ggplot2::geom_point(
            data = df2,
            mapping = ggplot2::aes(
              x = stats::median(error), y = 0,
              col = "est_new", shape = "est_new"),
            size = 6)
        }

        lbl <- c(
          paste0("Current error"),
          paste0("Median error + ", vals$hrCI$CI * 100,
                 "% HDI for ", out_dur, " days"))
        brk <- c("now", "est")

        val_fill <- val_col <- c("now" = pal$sea_d, "est" = pal$sea)
        val_linetype <- c("now" = "blank", "est" = "solid")
        val_shape <- c("now" = 19, "est" = 18)

        override_size <- c(.8, .8)
        override_stroke <- c(4, 4)

        if (reveal_if) {
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

        p <- ds1 %>%
          ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_vline(xintercept = 0, alpha = 1) +

          {if (reveal_if) p1 } +
          {if (reveal_if) p2 } +

          ggplot2::geom_line(
            ggplot2::aes(col = "est"),
            linetype = "dotted") +

          ggplot2::geom_area(
            data = ds1_ci,
            mapping = ggplot2::aes(x = x, y = y),
            alpha = 0.4, fill = pal$sea) +

          {if (reveal_if) p3 } +

          ggplot2::geom_segment(
            mapping = ggplot2::aes(
              x = vals$hrCI$CI_low,
              xend = vals$hrCI$CI_high,
              y = 0, yend = 0, col = "est",
              linetype = "est"),
            size = .8) +

          {if (reveal_if) p4 } +

          ggplot2::geom_point(
            data = df1,
            mapping = ggplot2::aes(
              x = stats::median(error), y = 0,
              col = "est", shape = "est"),
            size = 6) +

          ggplot2::geom_point(
            ggplot2::aes(x = vals$hrErr, y =  0,
                         col = "now", shape = "now"),
            size = 6, alpha = .7) +

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

      output$plot_sd_density <- ggiraph::renderGirafe({
        # req(vals$ctsdErr, vals$sdCI)

        reveal_if <- FALSE
        if (!is.null(input$highlight_dti)) {
          if (input$highlight_dti != "") reveal_if <- TRUE
        }

        dat <- sims_speed[[1]] %>%
          dplyr::mutate(dur = round("days" %#% dur, 0)) %>%
          dplyr::mutate(tau_p = round("days" %#% tau_p, 0))
        opts <- sims_speed[[1]] %>%
          dplyr::mutate(dur = round("days" %#% dur, 0)) %>%
          dplyr::select(dti, dti_notes) %>%
          unique()

        index_tauv <- which.min(abs(dat$tau_v - input_tauv))
        out_tauv <- dat$tau_v[index_tauv]

        index_dur <- which.min(abs(dat$dur - input_dur))
        out_dur <- dat$dur[index_dur]

        index_dti <- which.min(abs(dat$dti - input_dti))
        out_dti <- dat$dti[index_dti]

        txt_dti <- opts$dti_notes[match(out_dti, opts$dti)]

        df1 <- dat %>%
          dplyr::filter(tau_v == out_tauv) %>%
          dplyr::filter(dur == out_dur) %>%
          dplyr::filter(dti == out_dti)

        ds1 <- stats::density(df1$error)
        ds1 <- data.frame(x = ds1$x, y = ds1$y)

        if(input$repInput_scaled) ds1$y <- ds1$y / max(ds1$y)
        ds1_ci <- subset(ds1, x >= vals$sdCI$CI_low &
                           x <= vals$sdCI$CI_high)

        if (reveal_if) {
          dti_new <- opts$dti[match(input$highlight_dti,
                                    opts$dti_notes)]
          index_dti_new <- which.min(abs(dat$dti - dti_new))
          out_dti_new <- dat$dti[index_dti_new]
          txt_dti_new <- opts$dti_notes[match(out_dti_new,
                                              opts$dti)]

          df2 <- dat %>%
            dplyr::filter(tau_v == out_tauv) %>%
            dplyr::filter(dur == out_dur) %>%
            dplyr::filter(dti == out_dti_new) %>%
            stats::na.omit()
          ds2 <- stats::density(df2$error)
          ds2 <- data.frame(x = ds2$x, y = ds2$y)

          CI <- ifelse(is.null(input$ci), .95, input$ci/100)
          ci2 <- bayestestR::ci(df2$error, ci = CI, method = "HDI")
          if(input$repInput_scaled) ds2$y <- ds2$y / max(ds2$y)
          ds2_ci <- subset(ds2, x >= ci2$CI_low &
                             x <= ci2$CI_high)

          p1 <- ggplot2::geom_line(
            data = ds2, mapping = ggplot2::aes(x = x, y = y),
            col = pal$mdn, linetype = "dotted")
          p2 <- ggplot2::geom_area(
            data = ds2_ci,
            mapping = ggplot2::aes(x = x, y = y),
            alpha = 0.2, fill = pal$mdn)
          p3 <- ggplot2::geom_segment(
            data = ci2,
            mapping = ggplot2::aes(
              x = CI_low,
              xend = CI_high,
              y = 0, yend = 0,
              col = "est_new", linetype = "est_new"),
            size = .8)
          p4 <- ggplot2::geom_point(
            data = df2,
            mapping = ggplot2::aes(
              x = stats::median(error), y = 0,
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

        if (reveal_if) {
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

        p <- ds1 %>%
          ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_vline(xintercept = 0, alpha = 1) +

          {if (reveal_if) p1 } +
          {if (reveal_if) p2 } +

          ggplot2::geom_line(
            ggplot2::aes(col = "est"),
            linetype = "dotted") +

          ggplot2::geom_area(
            data = ds1_ci,
            mapping = ggplot2::aes(x = x, y = y),
            alpha = 0.4, fill = pal$sea) +

          {if (reveal_if) p3 } +

          ggplot2::geom_segment(
            mapping = ggplot2::aes(
              x = vals$sdCI$CI_low,
              xend = vals$sdCI$CI_high,
              y = 0, yend = 0, col = "est",
              linetype = "est"),
            size = .8) +

          {if (reveal_if) p4 } +

          ggplot2::geom_point(
            data = df1,
            mapping = ggplot2::aes(
              x = stats::median(error), y = 0,
              col = "est", shape = "est"),
            size = 6) +

          ggplot2::geom_point(
            ggplot2::aes(x = vals$ctsdErr, y =  0,
                         col = "now", shape = "now"),
            size = 6, alpha = .7) +

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

      ## Rendering error estimate plots: ----------------------------------

      output$plot_hr_error <- ggiraph::renderGirafe({

        dat <- sims_hrange[[2]] %>%
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
            mapping = ggplot2::aes(
              x = duration,
              y = error),
            size = 3, col = pal$mdn)
        }

        p <- ggplot2::ggplot() +

          ggplot2::geom_ribbon(
            data = dat_filtered,
            mapping = ggplot2::aes(
              x = duration,
              y = error,
              ymin = error_lci,
              ymax = error_uci),
            col = NA, fill = "grey90",
            alpha = .5) +

          ggplot2::geom_line(
            data = dat_filtered,
            mapping = ggplot2::aes(x = duration,
                                   y = error,
                                   group = tau_p),
            col = "grey20", linetype = "dotted",
            size = 0.5) +

          ggplot2::geom_point(
            data = dat_filtered,
            mapping = ggplot2::aes(x = duration,
                                   y = error,
                                   group = tau_p),
            size = 2.5, shape = 18, col = "grey40") +

          ggplot2::geom_segment(
            data = dat_filtered[index_dur,],
            ggplot2::aes(x = duration,
                         xend = duration,
                         y = error_lci,
                         yend = error_uci),
            col = pal$sea,
            linetype = "solid",
            size = 1.5, alpha = .8) +

          ggplot2::geom_point(
            data = dat_filtered[index_dur,],
            mapping = ggplot2::aes(
              x = duration, y = error, group = tau_p),
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

        # dat <- movedesign::sims_speed[[2]]
        dat <- sims_speed[[2]] %>%
          dplyr::mutate(dur = round("days" %#% dur, 0))
        dat$id <- 1:nrow(dat)

        opts <- sims_speed[[2]] %>%
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
            x = dur,
            y = error,
            group = as.factor(dti),
            ymin = error - ci,
            ymax = error + ci)) +

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
              ggplot2::aes(x = dur,
                           y = error,
                           group = as.factor(dti),
                           ymin = error - ci,
                           ymax = error + ci),
              alpha = .1, fill = pal$mdn,
              position = ggplot2::position_dodge(width = 0.3)) } +

          { if (reveal_if)
            ggplot2::geom_line(
              data = newdat,
              ggplot2::aes(x = dur,
                           y = error,
                           group = as.factor(dti)),
              size = .5, alpha = .8,
              linetype = "solid", col = pal$mdn,
              position = ggplot2::position_dodge(
                width = 0.3)) } +

          { if (reveal_if)
            ggiraph::geom_point_interactive(
              data = newdat,
              ggplot2::aes(x = dur,
                           y = error,
                           group = as.factor(dti)),
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

      if (length(vals$which_question) > 1) {
        out <- tagList(out_hr, our_sd)
      }

      return(out)

    }) # end of renderUI // reportPlots_error

    # REPORT --------------------------------------------------------------

    observe({
      req(vals$which_question)

      msg_log(
        style = "warning",
        message = paste0("Building ",
                         msg_warning("report"), "..."),
        detail = paste("Current question(s):", vals$which_question))

      # Timescale parameters:

      tau_p <- vals$tau_p0 %#% vals$tau_p0_units
      tau_v <- vals$tau_v0 %#% vals$tau_v0_units

      # Ideal regime:

      dur_times <- 10 # rule of thumb: 10 x tau_p
      dur_units <- "days"
      ideal_dur <- fix_unit((dur_units %#% tau_p) * dur_times,
                            dur_units)
      ideal_dti <- fix_unit(vals$tau_v0,
                            vals$tau_v0_units)

      # Current regime:

      dur <- dur_units %#% vals$dur0_dev %#% vals$dur0_units_dev
      dur <- fix_unit(dur, dur_units)
      dti_units <- ideal_dti$unit
      dti <- dti_units %#% vals$dti0_dev %#% vals$dti0_units_dev
      dti <- fix_unit(dti, dti_units)

      vals$hr_col <- vals$ctsd_col <- data.frame(
        hex = pal$sea, css = "var(--sea)")

      if (dur$value <= ideal_dur$value) {
        vals$hr_col$hex <- pal$dgr
        vals$hr_col$css <- "var(--danger)"
      }

      if (dti$value <= ideal_dti$value) {
        diff_dti <- (dti$unit %#% tau_v) / dti$value
        dti_text <-
          span(diff_dti, icon(name = "xmark"),
               wrap_none("\u03C4", tags$sub("v")))
      } else {
        diff_dti <- 1 / ((dti$unit %#% tau_v) / dti$value)
        dti_text <-
          span(wrap_none("\u03C4", tags$sub("v")), "/",
               diff_dti)

        vals$ctsd_col$hex <- pal$dgr
        vals$ctsd_col$css <- "var(--danger)"
      }

      ## Reporting DATA: --------------------------------------------------

      if (vals$data_type == "selected") {

        out_species <-
          span("These outputs are based on parameters extracted",
               "from individual", span(vals$id, class = "cl-grn"),
               "and species", span(vals$species_common,
                                   class = "cl-grn"),
               wrap_none("(", em(vals$species_binom), ")."))

      } else if (vals$data_type == "upload") {

        out_species <-
          span("These outputs are based on parameters extracted",
               "from individual", span(vals$id, class = "cl-grn"),
               "and species", wrap_none(em(vals$species,
                                           class = "cl-grn"), "."))

      } else if (vals$data_type == "simulated") {

        out_species <-
          span("These outputs are based on a",
               span("simulated", class = "cl-grn"),
               "dataset.")
      }

      out_species <- p(
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
          p("The minimum", span("sampling duration", class = "cl-sea"),
            "for", span("home range", class = "cl-grn"), "estimation",
            "is at least 10", icon(name = "xmark"),
            span("position autocorrelation", class = "cl-sea"),
            wrap_none("parameter (\u03C4", tags$sub("p"), "),"),
            "or \u2248", wrap_none(ideal_dur$value, " ", ideal_dur$unit,
                                   css = "cl-grn", end = "."),
            "Your current duration is",
            round(dur$value / (dur$unit %#% tau_p), 0),
            icon(name = "xmark"), wrap_none("\u03C4", tags$sub("p")),
            "\u2248", wrap_none(dur$value, " ", dur$unit,
                                color = vals$hr_col[1], ","),
            "resulting in an",
            "effective sample size of",
            span(N1, style = vals$hr_col[1]), "locations.")

      } # end of "Home range"

      ## Speed and distance estimation:

      if ("Speed & distance" %in% vals$which_question) {
        req(vals$sdCI)

        N2 <- scales::label_comma(accuracy = 1)(vals$device_N2)

        out_regime <- out_reg_ctsd <-
          p("The minimum", span("sampling interval", class = "cl-sea"),
            "(fix rate) for", span("speed & distance", class = "cl-grn"),
            "estimation should be less than the",
            span("velocity autocorrelation", class = "cl-sea"),
            wrap_none("parameter (", tags$sub("v"), "),"),
            "or \u2264", wrap_none(ideal_dti$value, " ", ideal_dti$unit,
                                   css = "cl-grn", end = "."),
            "Your current interval (\u0394t) is",
            dti_text,
            "\u2248", wrap_none(dti$value, " ", dti$unit,
                                color = vals$ctsd_col[1], ","),
            "resulting in an",
            "effective sample size of",
            span(N2, style = vals$ctsd_col[1]), "locations.")

      } # end of "Speed & distance"

      ### Both home range and speed & distance:

      # if (length(vals$which_question) > 1) {
      #   out_regime <- tagList(out_reg_hr, out_reg_ctsd)
      # }

      ## Reporting OUTPUTS: -----------------------------------------------

      if ("Home range" %in% vals$which_question) {
        req(vals$hrCI, vals$hrErr)

        CI <- round(vals$hrCI$CI * 100, 0)
        LCI <- round(vals$hrCI$CI_low * 100, 1)
        UCI <- round(vals$hrCI$CI_high * 100, 1)

        txt_level <- ifelse(
          vals$hrCI$CI_high < .3 &
            vals$hrCI$CI_low > -.3,
          "and with low", "but with high")

        dur_options <- 2^seq(1, 12, by = 1)
        index_dur <- which.min(abs(dur_options - dur$value))
        plotted_dur <- dur_options[index_dur]

        if (dur$value >= ideal_dur$value) {

          out_hr <-
            span("Your current tracking regime is likely sufficient",
                 "for", span("home range", class = "cl-grn"),
                 "estimation,", txt_level, "uncertainty.", br(),
                 "Keep in mind that, for a duration of",
                 plotted_dur, "days, there is a",
                 wrap_none(CI, "%", css = "cl-blk"),
                 "probability that the relative error will lie within",
                 wrap_none(LCI, "%", css = "cl-blk"),
                 "and", wrap_none(UCI, "%", end = ".", css = "cl-blk"))

        } else {

          out_hr <-
            span("Your current tracking regime is likely insufficient",
                 "for", span("home range", class = "cl-grn"),
                 "estimation.", br(),
                 "For a duration of",
                 plotted_dur, "days, there is high uncertainty",
                 wrap_none("(", CI, "%", css = "cl-blk"),
                 "probability that the relative error will lie within",
                 wrap_none(LCI, "%", css = "cl-blk"),
                 "and", wrap_none(UCI, "%", end = ").", css = "cl-blk"))
        }

        out_analyses <- out_hr <- p(
          out_hr,
          "Your error estimate",
          "based on a single simulation was",
          wrap_none(round(vals$hrErr * 100, 1), "%",
                    color = vals$hr_col[1], end = "."))

      } # end of 'Home range'

      ## Speed and distance estimation:

      if ("Speed & distance" %in% vals$which_question) {
        req(vals$ctsdCI, vals$ctsdErr)

        CI <- round(vals$sdCI$CI * 100, 0)
        LCI <- round(vals$sdCI$CI_low * 100, 1)
        UCI <- round(vals$sdCI$CI_high * 100, 1)

        txt_level <- ifelse(
          vals$ctsdCI$CI_high < .3 &
            vals$ctsdCI$CI_low > -.3,
          "and with low", "but with high")

        dti_options <- 2^seq(1, 12, by = 1)
        index_dti <- which.min(abs(dti_options - dti$value))
        plotted_dti <- dti_options[index_dti]

        if (dur$value >= ideal_dur$value) {

          out_ctsd <-
            span("Your current tracking regime is likely sufficient",
                 "for", span("home range", class = "cl-grn"),
                 "estimation,", txt_level, "uncertainty.",
                 "Keep in mind that, for a duration of",
                 plotted_dti, "days, there is a",
                 wrap_none(CI, "%", css = "cl-blk"),
                 "probability that the relative error will lie within",
                 wrap_none(LCI, "%", css = "cl-blk"),
                 "and", wrap_none(UCI, "%", end = ".", css = "cl-blk"))

        } else {

          out_ctsd <-
            span("Your current tracking regime is likely insufficient",
                 "for", span("home range", class = "cl-grn"),
                 "estimation.",
                 "For a duration of",
                 plotted_dti, "days, there is high uncertainty",
                 wrap_none("(", CI, "%", css = "cl-blk"),
                 "probability that the relative error will lie within",
                 wrap_none(LCI, "%", css = "cl-blk"),
                 "and", wrap_none(UCI, "%", end = ").", css = "cl-blk"))
        }

        out_analyses <- out_ctsd <- p(
          out_ctsd,
          "Your error estimate",
          "based on a single simulation was",
          wrap_none(round(vals$ctsdErr * 100, 1), "%",
                    color = vals$ctsd_col[1], end = "."))

      } # end of "Speed & distance"

      ### Both home range and speed & distance:

      # if (length(vals$which_question) > 1) {
      #   #TODO
      # }

      # Rendering complete report: ----------------------------------------

      output$end_report <- renderUI({

        out <- tagList(
          out_species,
          out_regime,

          div(width = 12, align = "center",
              style = "z-index: 999;",

              shinyWidgets::switchInput(
                inputId = ns("repInput_scaled"),
                label = span(icon("wrench"),
                             "Scaled to 1"),
                labelWidth = "100px")),

          if ("Home range" %in% vals$which_question) {
            ggiraph::girafeOutput(
              outputId = ns("plot_hr_density"),
              width = "100%", height = "100%") },

          if ("Speed & distance" %in% vals$which_question) {
            ggiraph::girafeOutput(
              outputId = ns("plot_sd_density"),
              width = "100%", height = "100%") },

          out_analyses

        ) # end of tagList
      }) # end of renderUI // end_report

    }) %>% bindEvent(input$create_report)

    ## Reporting COMPARISON (if available): -------------------------------

    observe({
      out_comp <- out_comp_hr <- span("")

      if ("Home range" %in% vals$which_question) {
        req(input$highlight_dur)

        highlighted_dur <- as.numeric(input$highlight_dur)

        CI <- round(vals$hrCI_new$CI * 100, 0)
        LCI <- round(vals$hrCI_new$CI_low * 100, 1)
        UCI <- round(vals$hrCI_new$CI_high * 100, 1)

        txt_level <- ifelse(
          vals$hrCI_new$CI_high < .3 &
            vals$hrCI_new$CI_low > -.3,
          "and with low", "but with high")

        ideal_dur <- fix_unit(
          ("days" %#% vals$tau_p0 %#% vals$tau_p0_units) * 10,
          "days")

        if (highlighted_dur >= ideal_dur$value) {
          out_comp <- out_comp_hr <-
            p("Your new tracking regime would likely be sufficient",
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
            p("Your new tracking regime would likely be insufficient",
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

      if ("Speed & distance" %in% vals$which_question) {
        #TODO
      } # end of "Speed & distance"

      ### Both home range and speed & distance:

      if (length(vals$which_question) > 1) {
        #TODO
      }

      output$end_comparison <- renderUI({
        div(id = "report_comparison",
          style = paste0("background-color: #f4f4f4;",
                           "padding: 20px;",
                           "margin-top: 20px;"),
            out_comp)

      }) # end of renderUI // end_comparison

    }) # end of observe

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_report_ui("tab_report_1")

## To be copied in the server
# mod_tab_report_server("tab_report_1")
