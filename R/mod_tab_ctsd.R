#' tab_ctsd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters/ for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_ctsd_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(

      ## Introduction: ----------------------------------------------------

      div(class = div_column_main,

          shinydashboardPlus::box(

            title = span("Speed & distance estimation:",
                         class = "ttl-tab"),
            icon = fontawesome::fa(name = "gauge-high",
                                   height = "21px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "var(--sea-dark)"),
            id = ns("sd_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = TRUE, closable = FALSE,

            column(
              align = "center", width = 12,
              
              p("Movement metrics, such as",
                span("speed and distance", class = "cl-sea-d"),
                "traveled, provide quantifiable links between behavior",
                "and energetics. For example, how far must animals",
                "travel to meet their nutritional and/or reproductive",
                "requirements."),

              p(style = "text-align: center;",
                "If estimating speed or distance traveled",
                "is your goal,", br(), "then click the",
                icon("paper-plane", class = "cl-mdn"),
                wrap_none(span("Run estimation", class = "cl-mdn")),
                "button."),

              splitLayout(
                cellWidths = c("38px", "10px", "200px"),
                cellArgs = list(style = 'align: center;'),

                shiny::actionButton(
                  inputId = ns("sdHelp_method"),
                  label = NULL,
                  width = "100%",
                  icon = icon("circle-question"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("run_sd"),
                  label = "Run estimation",
                  icon =  icon("paper-plane"),
                  width = "100%",
                  class = "btn-primary")
              ),
              br()

            ) # end of column (for text)
          ), # end of box // sd_intro

          shinyWidgets::radioGroupButtons(
            inputId = ns("ctsdInput_show"),
            label = NULL,
            choices = c("Show initial tracking regime" = 1,
                        "Show modified tracking regime" = 2),
            checkIcon = list(yes = icon("circle-check")),
            selected = 2,
            justified = TRUE),
          br()

      ), # end of div (top row)

      # [left column] -----------------------------------------------------

      div(class = div_column_left,

          ## Tracking regime: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Tracking regime", class = "ttl-box_solid"),
            id = ns("sdBox_regime"),
            status = "info",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("sdTabs_regime"),

              tabPanel(
                value = ns("sdPanel_regime"),
                title = icon("stopwatch", class = "cl-sea"),
                p(),
                fluidRow(
                  column(width = 12, uiOutput(ns("sdInfo_dur"))),
                  column(width = 12, uiOutput(ns("sdInfo_dti")))
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("sdPanel_regime_new"),
                title = icon("bolt", class = "cl-mdn"),
                p(),
                fluidRow(
                  column(width = 12, uiOutput(ns("sdInfo_dur_new"))),
                  column(width = 12, uiOutput(ns("sdInfo_dti_new")))
                ), # end of fluidRow

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12, align = "center",

              splitLayout(
                cellWidths = c("29%", "1%", "70%"),
                cellArgs = list(style = "align: center;"),

                shiny::actionButton(
                  inputId = ns("sdHelp_regime"),
                  label = NULL,
                  width = "100%",
                  icon = icon("circle-question"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("ctsd_adjRegime"),
                  label = "Modify",
                  icon = icon("wrench"),
                  class = "btn-info",
                  width = "100%")

              ) # end of splitLayout

            ) # end of column (footer)
          ), # end of box // sdBox_regime
          
          ## Sample sizes: ------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Sample sizes:", class = "ttl-box"),
            id = ns("sdBox_sizes"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            tabsetPanel(
              id = ns("sdTabs_sizes"),
              
              tabPanel(
                value = ns("sdPanel_sizes"),
                title = icon("stopwatch", class = "cl-sea"),
                
                fluidRow(
                  column(width = 12, uiOutput(ns("sdBlock_n"))),
                  column(width = 12, uiOutput(ns("sdBlock_N2"))),
                ) # end of fluidRow
                
              ), # end of panels (1 out of 2)
              
              tabPanel(
                value = ns("sdPanel_sizes_new"),
                title = icon("bolt", class = "cl-mdn"),
                
                fluidRow(
                  column(width = 12, uiOutput(ns("sdBlock_n_new"))),
                  column(width = 12, uiOutput(ns("sdBlock_N2_new"))),
                ) # end of fluidRow
                
              ) # end of panels (2 out of 2)
            ) # end of tabs
          ) # end of box // sdBox_sizes
          
      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(class = div_column_right,

          ## Speed & distance plots: --------------------------------------

          shinydashboardPlus::box(
            title = span("Estimates:", class = "ttl-box"),
            id = ns("sdBox_outputs"),
            width = NULL,
            headerBorder = FALSE,
            
            tabsetPanel(
              id = ns("sdTabs_viz"),
           
              tabPanel(
                title = "Distance",
                value = ns("regPanel_trajectory"),
                icon = icon("route"),
                
                div(
                  class = "col-xs-12 col-sm-12 col-md-12 col-lg-8",
                  p(), uiOutput(ns("ctsdVar_reveal")),
                  
                  ggiraph::girafeOutput(
                    outputId = ns("sdPlot_path"),
                    width = "100%", height = "100%")),
                
                div(
                  class = "col-xs-12 col-sm-12 col-md-12 col-lg-4",
                  p(class = "fluid-padding"),
                  uiOutput(ns("distInfo_est")),
                  uiOutput(ns("distInfo_est_new")),
                  uiOutput(ns("distInfo_err")),
                  uiOutput(ns("distInfo_err_new")))
                
              ), # end of panel (1 out of 2)
              
              tabPanel(
                title = "Speed",
                value = ns("regPanel_speed"),
                icon = icon("gauge-high"),
                
                div(
                  class = "col-xs-12 col-sm-12 col-md-12 col-lg-9",
                  p(), ggiraph::girafeOutput(
                    outputId = ns("sdPlot_main"),
                    width = "100%", height = "100%"),
                  uiOutput(ns("ctsdVar_group"))),
                
                div(
                  class = "col-xs-12 col-sm-12 col-md-12 col-lg-3",
                  p(class = "fluid-padding"),
                  uiOutput(ns("sdInfo_est")),
                  uiOutput(ns("sdInfo_est_new")),
                  uiOutput(ns("sdInfo_err")),
                  uiOutput(ns("sdInfo_err_new")))
                
              ) # end of panel (2 out of 2)
            ), # end of tabBox // sdBox_viz
            
            footer =  column(
              width = 12, align = "center",
              
              splitLayout(
                cellWidths = c("29%", "1%", "70%"),
                cellArgs = list(style = "align: center;"),
                
                shiny::actionButton(
                  inputId = ns("sdHelp_bias"),
                  label = NULL,
                  width = "100%",
                  icon = icon("circle-question"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("show_sdTable"),
                  label = span("Add to",
                               span("table", class = "cl-sea")),
                  icon = icon("bookmark"),
                  width = "100%",
                  class = "btn-primary")
                
              ) # end of splitLayout
              
            ) # end of column (footer)
          ) # end of sdBox_outputs
          
      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          ## Table: -------------------------------------------------------

          shinydashboardPlus::box(
            title = span("Summary table:", class = "ttl-box"),
            id = ns("sdBox_summary"),
            width = NULL,
            solidHeader = FALSE,

            reactable::reactableOutput(ns("sdTable")),
            br(),
            div(style = "display:inline-block; float:right",
                shiny::actionButton(
                  inputId = ns("sdTable_clear"),
                  label = "Clear table",
                  icon =  icon("trash"),
                  width = "110px")), br()

          ), # end of box // sdBox_summary

          ## Additional information: --------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("sdBox_misc"),
            width = NULL,
            solidHeader = FALSE,

            verbatimTextOutput(outputId = ns("time_ctsd")),
            verbatimTextOutput(outputId = ns("time_ctsd_new"))

          ) # end of box
      ) # end of column (bottom)

    ) # end of fluidRow
  ) # end of tagList
}

#' tab_ctsd Server Functions
#'
#' @noRd
mod_tab_ctsd_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals$sd <- reactiveValues()
    pal <- load_pal()

    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide secondary elements at start: ----------------------------------

    tmpnames <- c("regime",
                  "sizes",
                  "outputs",
                  "summary",
                  "misc")

    for(i in 1:length(tmpnames)) {
      shinyjs::hide(id = paste0("sdBox_", tmpnames[i]))
    }

    vec <- c("regime", "sizes", "dist")
    for(i in 1:length(vec)) {
      tmp_id <- paste0("sdTabs_", vec[i])
      tmp_target <- paste0("sdPanel_", vec[i], "_new")
      hideTab(inputId = tmp_id, target = ns(tmp_target))
    }

    shinyjs::hide(id = "ctsdInput_show")
    
    ## Changing grouping options for plot: --------------------------------

    output$ctsdVar_group <- renderUI({
      req(vals$reg$dur, vals$reg$dti)

      eval_dur <- vals$reg$dur %#% vals$reg$dur_unit
      eval_dti <- vals$reg$dti %#% vals$reg$dti_unit

      group_choices <- c(
        "Minutes" = "minutes",
        "Hours" = "hours",
        "Days" = "days",
        "Weeks" = "weeks",
        "Months" = "months")

      if (eval_dur <= (5 %#% "days")) {
        group_selected <- "hours"
        group_min <- "minutes"
        group_max <- "days"

      } else if (eval_dur > (5 %#% "days") &&
                eval_dur <= (1 %#% "month")) {
        group_selected <- "days"
        group_min <- "minutes"
        group_max <- "days"

      } else {
        group_selected <- "days"
        group_min <- "minutes"
        group_max <- "months"
      }

      if (eval_dti >= 1 %#% "day") {
        group_min <- "days"
      } else if (eval_dti >= 1 %#% "hour") {
        group_min <- "hours"
      }

      shinyWidgets::sliderTextInput(
        inputId = ns("ctsdVar_group_units"),
        label = span("Averaging time unit above:",
                     class = "txt-label",
                     style = "text-align: center;"),
        choices = group_choices,
        from_min = group_min,
        from_max = group_max,
        selected = group_selected,
        grid = TRUE,
        force_edges = TRUE,
        width = "100%")

    }) # end of renderUI // ctsdVar_group

    ## Checking if data is available: -----------------------------------

    observe({
      req(vals$active_tab == 'ctsd',
          vals$tmpid, vals$id)

      # Check if data is available:
      if (!is.null(vals$data1)) {
        if (vals$tmpid != vals$id) {

          shinyalert::shinyalert(
            title = "Oops!",
            text = span(
              "Data selected is from individual",
              HTML(paste0(span(vals$id, class = "cl-dgr"),
                          ",")), "but parameters are from",
              HTML(paste0(span(vals$tmpid, class = "cl-dgr"), ".")),
              br(),

              "Please extract parameters in the",
              icon("paw", class = "cl-mdn"),
              span("Data", class = "cl-mdn"), "tab",
              "for the appropriate individual before",
              "estimating speed & distance traveled."),
            html = TRUE,
            size = "xs")
        }

      } else {
        shinyalert::shinyalert(
          type = "error",
          title = "No data found",
          text = span(
            'Please upload, select or simulate an', br(),
            span('movement dataset', class = "cl-dgr"),
            'first in the',
            icon("paw", class = "cl-mdn"),
            span('Data', class = "cl-mdn"), "tabs."),
          html = TRUE,
          size = "xs")

      } # end of if (), checking for data
    }) # end of observe

    ## Checking if tau v available for initial dataset: -----------------

    observe({
      req(vals$valid_tauv, vals$active_tab == "ctsd")
      if (vals$valid_tauv == "No") {

        shinyalert::shinyalert(
          type = "error",

          title = "Speed & distance invalid",
          text = span(
            "No statistically significant signature of the animal's",
            span("velocity", class = "cl-dgr"),
            "remains in this dataset.",
            "Please select a different individual or dataset to",
            "proceed with", span("distance/speed", class = "cl-dgr"),
            "estimation"),

          confirmButtonText = "Dismiss",
          html = TRUE)

        msg_log(
          style = "danger",
          message = paste0("Initial dataset has no remaining ",
                           msg_danger("velocity"), " signature."),
          detail = paste0("Select a different individual",
                          "or dataset to proceed."))

        shinyjs::disable("run_sd")
      }

    }) # end of observe

    ## Adding different datasets to trajectory plot: --------------------

    output$ctsdVar_reveal <- renderUI({

      if (is.null(vals$sd$newdata)) {
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("ctsdShow_datasets"),
          label = "Show trajectories:",
          choices =
            c("Fine-scale" = "full",
              "Initial tracking regime" = "subset"),
          selected = c("full", "subset"),
          checkIcon = list(yes = icon("circle-check")),
          justified = TRUE)

      } else {
        
        opts <- c("full", "subset", "new")
        if (!is.null(input$ctsdInput_show)) {
          if (input$ctsdInput_show == 1) {
            opts <- c("full", "subset")
          }
        }
        
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("ctsdShow_datasets"),
          label = "Show trajectories:",
          choices =
            c("Fine-scale" = "full",
              "Initial regime" = "subset",
              "New regime" = "new"),
          selected = opts,
          checkIcon = list(yes = icon("circle-check")),
          justified = TRUE) }

    }) # end of renderUI // ctsdVar_reveal

    ## Quick select all boxes after adjusting tracking regime: ----------

    observe({

      tmp <- ifelse(input$ctsdInput_show == 1, "", "_new")
      tabs <- paste0("sdTabs_", vec)
      panels <- paste0("sdPanel_", vec)

      for(v in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[v]),
          selected = paste0("tab_ctsd_1-", panels[v], tmp)) }

      vals$paths_selected <- c("full", "subset", "new")
      
      if (input$ctsdInput_show == 1) {
        shinyjs::show(id = "sdInfo_est")
        shinyjs::show(id = "sdInfo_err")
        shinyjs::hide(id = "sdInfo_est_new")
        shinyjs::hide(id = "sdInfo_err_new")

        shinyjs::show(id = "distInfo_est")
        shinyjs::show(id = "distInfo_err")
        shinyjs::hide(id = "distInfo_est_new")
        shinyjs::hide(id = "distInfo_err_new")
      } else if (input$ctsdInput_show == 2) {
        shinyjs::hide(id = "sdInfo_est")
        shinyjs::hide(id = "sdInfo_err")
        shinyjs::show(id = "sdInfo_est_new")
        shinyjs::show(id = "sdInfo_err_new")

        shinyjs::hide(id = "distInfo_est")
        shinyjs::hide(id = "distInfo_err")
        shinyjs::show(id = "distInfo_est_new")
        shinyjs::show(id = "distInfo_err_new")
      }

    }) %>% # end of observe.
      bindEvent(input$ctsdInput_show, ignoreInit = TRUE)
    
    # PROCESSING ----------------------------------------------------------
    ## Fitting movement model (if needed): --------------------------------

    observe({
      req(vals$active_tab == 'ctsd')
      
      if (is.null(vals$data1)) {
        
        shinyalert::shinyalert(
          type = "error",
          title = "No tracking regime set",
          text = span(
            "Please go to the",
            icon("stopwatch", class = "cl-mdn"),
            span("Tracking regime", class = "cl-mdn"), "tab",
            "and make sure to both (1) set a tracking regime, and",
            "(2) run a new simulation by pressing the",
            icon("bolt", class = "cl-dgr"),
            span("'Run'", class = "cl-mdn"), "button."
          ),
          html = TRUE,
          size = "xs")
        
      } else {
        if (is.null(vals$fit1)) {
          
          msg_log(
            style = "danger",
            message = paste0("Model fit ",
                             msg_danger("not found"), "."),
            detail = "Please wait for model selection to finish.")
          
          expt <- estimate_time(vals$data1, parallel = vals$parallel)
          vals$expt_max <- expt$max
          vals$expt_min <- expt$min
          vals$expt_units <- expt$units
          
          if ((vals$expt_max %#% vals$expt_units) > 900) {
            
            vals$hr$check_time <- FALSE
            shinyalert::shinyalert(
              className = "modal_warning",
              title = "Do you wish to proceed?",
              callbackR = function(x) {
                vals$hr$check_time <- x
              },
              text = span(
                "Expected run time for the next phase", br(),
                "is approximately",
                span(vals$expt_min, "\u2013", vals$expt_max,
                     class = "cl-dgr"),
                wrap_none(span(vals$expt_units,
                               class = "cl-dgr"), ".")
              ),
              type = "warning",
              showCancelButton = TRUE,
              cancelButtonText = "Stop",
              confirmButtonCol = pal$mdn,
              confirmButtonText = "Proceed",
              html = TRUE
            )
          } else {
            vals$hr$check_time <- TRUE
          }
          
          req(vals$hr$check_time)
          if (vals$expt_max == vals$expt_min) {
            tmptxt <- paste("\u2264", vals$expt_max, vals$expt_units)
          } else {
            tmptxt <- paste(vals$expt_min, "\u2013",
                            vals$expt_max, vals$expt_units)
          }
          
          shinybusy::show_modal_spinner(
            spin = "fading-circle",
            color = "var(--sea)",
            
            text = span(
              style = "font-size: 18px;",
              span("!Selecting", style = "color: #797979;"),
              HTML(paste0(span("movement model", class = "cl-sea"),
                          span(".", style = "color: #797979;"))),
              p(),
              p("Expected run time:",
                style = paste("background-color: #eaeaea;",
                              "color: #797979;",
                              "font-size: 16px;",
                              "text-align: center;")), br(),
              p(tmptxt,
                style = paste("background-color: #eaeaea;",
                              "color: #009da0;",
                              "font-size: 16px;",
                              "text-align: center;",
                              "margin-top: -40px;")),
              p()
              
            ) # end of text
          ) # end of modal
          
          start <- Sys.time()
          guess1 <- ctmm::ctmm.guess(vals$data1, interactive = FALSE)
          
          inputList <- list(list(vals$data1, guess1))
          fit1 <- par_ctmm.select(inputList, parallel = vals$parallel)
          time_fit1 <- difftime(Sys.time(), start, units = "mins")
          
          if (round(time_fit1, 1) < 1) {
            tmpdetail <- paste("This step took less than one minute.")
          } else {
            tmpdetail <- paste("This step took approximately",
                               round(difftime(Sys.time(), start,
                                              units = 'min'), 0),
                               "minutes.")
          }
          
          if (!is.null(fit1)) {
            msg_log(
              style = 'success',
              message = paste0("Model fit ",
                               msg_success("completed"), "."),
              detail = tmpdetail)
            
            vals$guess <- NULL
            vals$needs_fit <- FALSE
            vals$fit1 <- fit1
            
            nms <- names(summary(vals$fit1)$DOF)
            N1 <- summary(vals$fit1)$DOF[grep('area', nms)][[1]]
            vals$N1 <- N1
            N2 <- summary(vals$fit1)$DOF[grep('speed', nms)][[1]]
            vals$N2 <- N2
            
          } # end of if (), !is.null(fit1)
          
          shinybusy::remove_modal_spinner()
          
        } # end of if (vals$needs_fit)
      } # end of if (is.null(vals$data1))
      
    }) # %>% # end of observe,
    # bindEvent(input$run_sd)
    
    observe({
      shinyjs::show(id = "sdBox_regime")
      shinyjs::show(id = "sdBox_sizes")
    }) %>% bindEvent(vals$fit1)

    ## Calculating total and mean distances: ----------------------------

    observe({
      req(vals$ctsd)
      
      # dat1 <- data.frame(
      #   x = vals$data1$x,
      #   y = vals$data1$y)
      
      # tmpdist <- list()
      # for(i in 2:nrow(dat1)) {
      #   tmpdist[[i]] <-
      #     sqrt((dat1$x[i] - dat1$x[i-1])^2 +
      #            (dat1$y[i] - dat1$y[i-1])^2)
      # }
      # vals$data1$dist <- c(0, do.call("rbind", tmpdist))
      
      dat_full <- data.frame(
        x = vals$data_full$x,
        y = vals$data_full$y)

      if (!is.null(vals$vals$sd$fit)) {
        dur <- vals$sd$dur %#% vals$sd$dur_unit
      } else { dur <- vals$reg$dur %#% vals$reg$dur_unit }
      dur_days <- "days" %#% dur
      
      tmpdist_full <- list()
      for(i in 2:nrow(dat_full)) {
        tmpdist_full[[i]] <-
          sqrt((dat_full$x[i] - dat_full$x[i-1])^2 +
                 (dat_full$y[i] - dat_full$y[i-1])^2)
      }
      vals$data_full$dist <- c(0, do.call("rbind", tmpdist_full))

      truth <- sum(vals$data_full$dist, na.rm = TRUE)
      vals$distTruth <- truth # in meters
      
      if (vals$ctsd_units == "kilometers/day") {
        dist <- vals$ctsd[2] * dur_days
        dist_lci <- vals$ctsd[1] * dur_days
        dist_uci <- vals$ctsd[3] * dur_days
        
      } else {
        dist <- ("kilometers/day" %#% 
                   vals$ctsd[2] %#% vals$ctsd_units) * dur_days
        dist_lci <- ("kilometers/day" %#% 
                       vals$ctsd[1] %#% vals$ctsd_units) * dur_days
        dist_uci <- ("kilometers/day" %#% 
                       vals$ctsd[3] %#% vals$ctsd_units) * dur_days
      }
      
      unit <- "kilometers"
      truth <- unit %#% truth
      
      vals$distEst <- data.frame("lci" = dist_lci,
                                 "est" = dist,
                                 "uci" = dist_uci,
                                 "unit" = unit)
      

      err <- (dist - truth) / truth
      err_lci <- (dist_lci - truth) / truth
      err_uci <- (dist_uci - truth) / truth
      vals$distErr <- data.frame("lci" = err_lci,
                                 "est" = err,
                                 "uci" = err_uci)
      
    }) # end of observe
    
    observe({
      req(vals$reg$dur, vals$distTruth,
          vals$ctsd_new)
      
      if (!is.null(vals$vals$sd$fit)) {
        dur <- vals$sd$dur %#% vals$sd$dur_unit
      } else { dur <- vals$reg$dur %#% vals$reg$dur_unit }
      dur_days <- "days" %#% dur
      
      if (vals$ctsd_units_new == "kilometers/day") {
        dist <- vals$ctsd_new[2] * dur_days
        dist_lci <- vals$ctsd_new[1] * dur_days
        dist_uci <- vals$ctsd_new[3] * dur_days
        
      } else {
        dist <- ("kilometers/day" %#% 
                   vals$ctsd_new[2] %#% vals$ctsd_units) * dur_days
        dist_lci <- ("kilometers/day" %#% 
                       vals$ctsd_new[1] %#% vals$ctsd_units) * dur_days
        dist_uci <- ("kilometers/day" %#% 
                       vals$ctsd_new[3] %#% vals$ctsd_units) * dur_days
      }
      
      unit <- "kilometers"
      truth <- unit %#% vals$distTruth
      
      vals$distEst_new <- data.frame("lci" = dist_lci,
                                     "est" = dist,
                                     "uci" = dist_uci,
                                     "unit" = unit)
      
      err <- (dist - truth) / truth
      err_lci <- (dist_lci - truth) / truth
      err_uci <- (dist_uci - truth) / truth
      vals$distErr_new <- data.frame("lci" = err_lci,
                                     "est" = err,
                                     "uci" = err_uci)
      
    }) # end of observe

    # ADJUSTING TRACKING REGIME -------------------------------------------
    # Adjust sampling parameters necessary for simulation:

    observe({
      req(vals$reg$dur, vals$reg$dti)

      if (!is.null(vals$ctsd)) {

        # Sampling duration:

        dur <- round("days" %#% vals$reg$dur %#% vals$reg$dur_unit, 0)
        tau_p0 <- round("days" %#% vals$tau_p0 %#% vals$tau_p0_units, 0)

        # Sampling interval:

        fixrate <- movedesign::gps_fixrate
        df_fixrate <- dplyr::arrange(fixrate, dplyr::desc(freq))
        dti_choices <- df_fixrate$nu_notes

        tau_v0 <- vals$tau_v0 %#% vals$tau_v0_units

        maxvalue <- 1 %#% "day" # tau_v0 * 3 OR dti
        maxindex <- which.min(abs(df_fixrate$nu - maxvalue))
        newvalue <- tau_v0 * 1/3
        newindex <- which.min(abs(df_fixrate$nu - newvalue))

        if (vals$data_type == "simulated") {
          tmprange <- NULL
        } else { tmprange <-
          paste(ifelse(vals$tau_v0_min == 0, "0",
                       scales::label_comma(
                         accuracy = .1)(vals$tau_v0_min)),
                "—", scales::label_comma(
                  accuracy = .1)(vals$tau_v0_max))
        }

        shiny::showModal(
          shiny::modalDialog(
            title = h4("Adjusting tracking regime:"),

            fluidRow(
              style = paste("margin-right: 25px;",
                            "margin-left: 25px;"),

              # p("Here you can adjust",
              #   span("sampling parameters", class = "cl-sea-d"),
              #   "to predict or simulate additional data from the",
              #   "same", span("movement model", class = "cl-sea-d"),
              #   "of the species provided."),

              p("If speed & distance estimation is your goal,",
                "we recommend that",
                span("sampling interval", class = "cl-sea"),
                "be equal (if not at least 4 times shorter) than the",
                span(HTML(paste0("velocity autocorrelation ",
                                 "(\u03C4", tags$sub("v"), ")")),
                     class = "cl-sea"), "value, to reduce bias",
                "of the conditional simulation.",
                "The", span("sampling duration", HTML("(\u0394t)"),
                            class = "cl-sea"),
                "is not directly related to the accuracy",
                "of home range estimation, so you can",
                "increase it if the total number of locations",
                "is a concern.",
              ),

              parBlock(
                header = span(
                  HTML(paste0("Velocity autocorrelation ",
                              "(\u03C4", tags$sub("v"), ")"))),
                value =
                  paste(scales::label_comma(
                    accuracy = .1)(vals$tau_v0),
                    vals$tau_v0_units),
                subtitle = tmprange),

              shiny::sliderInput(
                inputId = ns("ctsd_dur"),
                label = "Sampling duration (in days):",
                width = "100%",
                min = 1, max = tau_p0 * 100, value = tau_p0 * 10
              ),

              shinyWidgets::sliderTextInput(
                inputId = ns("ctsd_dti"),
                label = "Sampling interval:",
                width = "100%",

                choices = dti_choices,
                selected = dti_choices[newindex],
                from_min = dti_choices[1],
                from_max = dti_choices[maxindex]),

              uiOutput(ns("ctsdText_sampling")),

              p(span("Proceed with caution!", class = "cl-dgr"),
                "Longer sampling durations + lower sampling",
                "intervals will add run time to simulation, model",
                "fitting, and estimation functions."),

            ), # end of fluidRow

            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                inputId = ns("run_sd_new"),
                label = "Run simulation",
                icon =  icon("bolt"),
                class = "btn-danger")
            ),

            size = "m")) # end of modal

      } else {

        shinyalert::shinyalert(
          title = "Error",
          text = span(

            "First, estimate the speed & distance",
            "traveled based on the original dataset",
            "by clicking the",
            icon("paper-plane", class = "cl-mdn"),
            HTML(paste0(span("Run estimation", class = "cl-mdn"))),
            "button."),

          html = TRUE,
          size = "xs")

      } # end of ifelse statement
    }) %>% bindEvent(input$ctsd_adjRegime)

    output$ctsdText_sampling <- renderUI({

      fixrate <- movedesign::gps_fixrate
      dti <- fixrate$nu[match(input$ctsd_dti,
                              fixrate$nu_notes)]

      n_new <- length(
        seq(from = 1,
            to = input$ctsd_dur %#% "days",
            by = dti))

      splitLayout(

        parBlock(header = "Initial tracking regime:",
                 value = scales::label_comma(
                   accuracy = 1)(nrow(vals$data1)),
                 subtitle = span("locations", class = "cl-mdn")),

        parBlock(header =  "Modified tracking regime:",
                 value = span(scales::label_comma(
                   accuracy = 1)(n_new),
                   class = "cl-dgr"),
                 subtitle = span("locations", style = "cl-dgr"))

      ) # end of splitLayout
    }) # end of renderUI // ctsdText_sampling

    # CTSD ESTIMATION -----------------------------------------------------

    finescale_speed <- shiny::reactive({
      
      if (vals$data_type == "simulated") {
        dat <- vals$data0
      } else { dat <- vals$data1 }
      
      fit <- vals$fit0
      
      dur <- vals$reg$dur %#% vals$reg$dur_unit
      tauv <- vals$tau_v0 %#% vals$tau_v0_units
      
      sim_full <- ctmm::simulate(
        dat, fit, t = seq(0, dur, by = tauv/3))
      vals$data_full <- sim_full
      
      inputList <- ctmmweb::align_lists(
        list(sim_full[which(sim_full$t <= tauv * 10), ]),
        list(fit))
      
      ctsd_truth <- par_speed(inputList,
                              parallel = vals$parallel)

      return(ctsd_truth)
      
    }) %>% # end of reactive, data_sim
      bindCache(c(vals$id,
                  vals$reg$dur, vals$reg$dur_unit,
                  vals$reg$dti, vals$reg$dti_unit))
    
    ## Estimating for initial tracking regime: ----------------------------
    # Estimate speed & distance after pressing the "run_sd" button:

    observe({
      req(vals$is_valid)

      # Check if data is available:
      if (!is.null(vals$data1)) {

        tmplist <- list("sdBox_speed",
                        "sdBox_dist",
                        "sdBox_outputs",
                        "sdBox_misc")

        for(i in 1:length(tmplist)) {
          shinyjs::show(id = tmplist[i])
        }

        req(vals$fit1, vals$tmpid, vals$id)
        if (vals$tmpid != vals$id) {

          shinyalert::shinyalert(
            title = "Oops!",
            text = span(
              "Data selected is from individual",
              HTML(paste0(span(vals$id, class = "cl-dgr"),
                          ",")), "but parameters are from",
              HTML(paste0(span(vals$tmpid, class = "cl-dgr"), ".")),
              br(), "Please extract parameters in the",
              icon("paw", class = "cl-mdn"),
              span("Data", class = "cl-mdn"), "tab",
              "for the appropriate individual before",
              "estimating home range."),
            html = TRUE,
            size = "xs")

        } else {

          sumnames <- rownames(summary(vals$fit1)$CI)
          N <- summary(vals$fit1)$DOF

          if (is.null(grep('speed', sumnames))) {

            shinyalert::shinyalert(
              type = "error",
              title = "Warning",
              text = span(
                "No velocity signature remains in the data.",
                "Go back to the",
                icon("stopwatch", class = "cl-mdn"),
                span('Tracking regime', class = "cl-mdn"), "tab,",
                "and set a shorter",
                span("sampling interval", class = "cl-sea-d"),
                "(reducing the time between new locations)."
              ),

              html = TRUE,
              size = "xs")

          } else {
            if (N["speed"] > 0) {

              start <- Sys.time()

              msg_log(
                style = "warning",
                message = paste0(
                  "Simulating for ",
                  msg_warning("current trajectory"),
                  msg_step(1, 2, style = "warning")),
                detail = "This may take a while...")

              shinybusy::show_modal_spinner(
                spin = "fading-circle",
                color = "var(--sea)",
                text = span(
                  span("Estimating", style = "color: #797979;"),
                  HTML(paste0(span("speed & distance", class = "cl-sea"),
                              span("...", style = "color: #797979;")))
                )
              )

              if ((vals$reg$dur %#% vals$reg$dur_unit) > 10 %#% "days") {
                dat <- vals$data1
                dat <- dat[which(dat$t <= (10 %#% "day")), ]
              } else { dat <- vals$data1 }

              inputList <- ctmmweb::align_lists(list(vals$fit1),
                                                list(dat))

              vals$ctsd <- par_speed(inputList,
                                     parallel = vals$parallel)

              if ("CI" %in% names(vals$ctsd)) {
                vals$ctsd <- vals$ctsd$CI
              }
              
              # vals$sdEst <- vals$ctsd[2]
              # vals$sdEst_min <- vals$ctsd[1]
              # vals$sdEst_max <- vals$ctsd[3]

              vals$time_ctsd <- difftime(Sys.time(), start, units = "mins")

              nms <- rownames(vals$ctsd)
              vals$ctsd_units <- nms[grep('speed', nms)] %>%
                extract_units()

              msg_log(
                style = "success",
                message = paste0(
                  "Estimation ", msg_success("completed"),
                  msg_step(1, 2, style = "success")),
                detail = paste(
                  "This step took approximately",
                  round(vals$time_ctsd, 1), "minutes."))

              vals$data_speed <- ctmm::speeds(vals$data1,
                                              vals$fit1,
                                              units = FALSE)
              
              shinyFeedback::showToast(
                type = "success",
                message = "Current trajectory completed!",
                .options = list(
                  timeOut = 3000,
                  extendedTimeOut = 3500,
                  progressBar = FALSE,
                  closeButton = TRUE,
                  preventDuplicates = TRUE,
                  positionClass = "toast-bottom-right"
                )
              )

              ### Simulating fine-scale trajectory: -----------------------

              msg_log(
                style = "warning",
                message = paste0(
                  "Simulating for ",
                  msg_warning("fine-scale trajectory"),
                  msg_step(2, 2, style = "warning")),
                detail = "This may take a while...")

              # if (vals$data_type == "simulated") {
              #   dat <- vals$data0
              #   fit <- vals$fit0
              # } else {
              #   dat <- vals$data1
              #   fit <- vals$fit1
              # }
              # 
              # start_truth <- Sys.time()
              # dur0 <- vals$reg$dur %#% vals$reg$dur_unit
              # tauv <- vals$tau_v0 %#% vals$tau_v0_units
              # 
              # sim_full <- ctmm::simulate(
              #   dat, fit,
              #   t = seq(0, dur0, by = tauv/4))
              # vals$data_full <- sim_full
              #
              # inputList <- ctmmweb::align_lists(
              #   list(sim_full[which(sim_full$t <= (10 %#% "day")), ]),
              #   list(vals$fit0))
              # 
              # vals$ctsd_truth <- vals$ctsd
              # vals$ctsd_truth <- par_speed(inputList,
              #                              parallel = vals$parallel)
              
              start_truth <- Sys.time()
              vals$ctsd_truth <- finescale_speed()
          
              if ("CI" %in% names(vals$ctsd_truth)) {
                vals$ctsd_truth <- vals$ctsd_truth$CI
              }

              tempnames <- rownames(vals$ctsd_truth)
              vals$ctsd_truth_units <-
                tempnames[grep("speed", tempnames)] %>%
                extract_units()

              truth <- vals$ctsd_truth[2]
              
              sdErr <- (vals$ctsd[2] - truth) / truth
              sdErr_min <- (vals$ctsd[1] - truth) / truth
              sdErr_max <- (vals$ctsd[3] - truth) / truth
              vals$sdErr <- data.frame("lci" = sdErr_min,
                                       "est" = sdErr,
                                       "uci" = sdErr_max)
            
              vals$time_ctsd_truth <- difftime(
                Sys.time(), start_truth, units = "mins")

              shinyjs::show(id = "sdInfo_est")
              shinyjs::show(id = "sdInfo_err")
              
              msg_log(
                style = "success",
                message = paste0(
                  "Estimation ", msg_success("completed"),
                  msg_step(2, 2, style = "success")),
                detail = paste(
                  "This step took approximately",
                  round(vals$time_ctsd_truth, 1),
                  "minutes."))

              vals$time_ctsd <- difftime(Sys.time(), start,
                                         units = 'mins')
              vals$is_analyses <- TRUE
              
              shinyFeedback::showToast(
                type = "success",
                message = "Fine-scale trajectory completed!",
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
              
            } # end of N["speed"] > 0
          } # end of is.null(grep('speed', sumnames))
        } # end of vals$tmpid != vals$id

      } else {

        shinyalert::shinyalert(
          type = "error",
          title = "No regime set",
          text = span(
            "Please select a",
            span("tracking regime", class = "cl-sea-d"),
            "(sampling duration and interval) from the",
            icon("stopwatch", class = "cl-mdn"),
            span('Tracking regime', class = "cl-mdn"), "tab."),
          html = TRUE,
          size = "xs")

      } # end of !is.null(vals$data1)

    }) %>% # end of observe,
      bindEvent(input$run_sd)

    ## Estimating for new tracking regime: --------------------------------

    observe({
      req(vals$data0, vals$fit0)

      # Show "conditional simulation" panels:
      for(i in 1:length(vec)) {
        tmp_id <- paste0("sdTabs_", vec[i])
        tmp_target <- paste0("sdPanel_", vec[i], "_new")
        showTab(inputId = tmp_id, target = ns(tmp_target))
      }

      tabs <- paste0("sdTabs_", vec)
      panels <- paste0("sdPanel_", vec)
      for(v in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[v]),
          selected = paste0("tab_ctsd_1-",
                            panels[v], "_new")) }

      # Capture new sampling duration and interval:
      vals$sd$dti_unit <- sub('^.* ([[:alnum:]]+)$',
                              '\\1', input$ctsd_dti)
      
      fixrate <- movedesign::gps_fixrate
      tmp <- fixrate$nu[match(input$ctsd_dti, fixrate$nu_notes)]
      vals$sd$dti <- round((vals$sd$dti_unit %#% tmp), 0)
      vals$sd$dur <- input$ctsd_dur
      vals$sd$dur_unit <- "days"
      
      # Check if storage limitations:

      proceed <- TRUE
      if (!is.null(vals$storage)) {
        t0 <- seq(0,
                  vals$sd$dur %#% vals$sd$dur_unit,
                  by = vals$sd$dti %#% vals$sd$dti_unit)

        if (length(t0) >= vals$storage) {

          shinyalert::shinyalert(
            type = "warning",
            title = "Warning",
            text = span(
              "Many GPS units can only store a maximum of",
              wrap_none(
                span("32,000—64,000 locations", class = "cl-dgr"),
                "."), "You set a limit of", vals$storage,
              "making this tracking regime invalid.",

              "Please select a different combination of",
              span("sampling duration", class = "cl-dgr"), "and",
              HTML(paste0(span("frequency", class = "cl-dgr"), "."))
            ),
            html = TRUE,
            size = "xs")

          msg_log(
            style = "danger",
            message = paste0("Reached storage limit of",
                             msg_danger(vals$storage), " locations."),
            detail = "Please set a different tracking regime.")
          
          proceed <- FALSE

        } # end of length(t0) >= vals$storage)
      } # end of !is.null(vals$storage)

      ### 1. Simulate new dataset:
      # Fill in the gaps of original dataset + new duration:
      
      req(proceed)
      vals$conditional <- TRUE
      removeModal()

      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = span(
          span("Estimating new", style = "color: #797979;"),
          HTML(paste0(span("speed & distance", class = "cl-sea"),
                      span("...", style = "color: #797979;")))
        )
      )
      
      msg_log(
        style = "warning",
        message = paste0("Simulating ",
                         msg_warning("modified tracking regime"), "."),
        detail = "This may take a while...")

      start <- Sys.time()
      if (vals$data_type == "simulated") {
        input_dat <- vals$data0
      } else { input_dat <- vals$data1 }
      
      if ((vals$sd$dur %#% vals$sd$dur_unit) > 10 %#% "days") {
        input_dat <- input_dat[which(input_dat$t <= (10 %#% "day")), ]
      } else { input_dat <- input_dat }
      
      dur <- vals$sd$dur %#% vals$sd$dur_unit
      tauv <- vals$tau_v0 %#% vals$tau_v0_units
      
      vals$data_full <- ctmm::simulate(
        input_dat, vals$fit0, t = seq(0, dur, by = 1 %#% "minute"))
      
      dat <- ctmm::simulate(
        vals$data_full, vals$fit0,
        t = seq(0, vals$sd$dur %#% vals$sd$dur_unit,
                by = vals$sd$dti %#% vals$sd$dti_unit),
        seed = vals$seed0)

      dat <- ctmm:::pseudonymize(dat)
      dat$index <- 1:nrow(dat)
      vals$sd$newdata <- dat

      msg_log(
        style = "success",
        message = "Simulation completed.",
        detail = paste(
          "This step took approximately",
          round(difftime(Sys.time(), start, units = 'secs'), 1),
          "seconds."))

      ### 2. Fit models to simulation:

      msg_log(
        style = "warning",
        message = "Fitting models to simulation.",
        detail = "This may take a while...")
      
      start_fit <- Sys.time()
      
      guess1 <- reactive({
        ctmm::ctmm.guess(vals$sd$newdata, interactive = FALSE)
      }) %>% bindCache(vals$sd$dti,
                       vals$sd$dur)

      vals$guess_new <- guess1()
      
      if (vals$data_type == "simulated") {
        mod2 <- vals$ctmm_mod
      } else {
        mod2 <- prepare_mod(
          tau_p = vals$tau_p0, tau_p_units = vals$tau_p0_units,
          tau_v = vals$tau_v0, tau_v_units = vals$tau_v0_units,
          sigma = vals$sigma0, sigma_units = vals$sigma0_units)
      }
      
      newfit <- reactive({
        inputList <- list(list(vals$sd$newdata, mod2))
        fit <- par_ctmm.fit(inputList, parallel = TRUE)
        return(fit)
      }) %>% bindCache(vals$sd$dti,
                       vals$sd$dti_unit,
                       vals$sd$dur,
                       vals$sd$dur_unit)

      vals$sd$fit <- newfit()
      
      msg_log(
        style = "success",
        message = "Model fit completed.",
        detail = paste(
          "This step took approximately",
          round(difftime(Sys.time(), start_fit, units = 'mins'), 1),
          "minutes."))

      vec <- c("regime", "speed", "size", "viz")
      for(i in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0("sdTabs_", vec[i]),
          selected = paste0("tab_ctsd_1-sdPanel_",
                            vec[i], "_new"))
      }
      
      ### 3. Run the speed/distance estimator (CTSD):

      nms <- names(summary(vals$sd$fit)$DOF)
      N <- summary(vals$sd$fit)$DOF[grep('speed', nms)][[1]]
      vals$N2_new <- N
      
      if (N < 5) {
        
        shinyalert::shinyalert(
          title = "Low sample size!",
          text = span(
            "Effective sample size for speed estimation",
            "is too low."
          ),
          html = TRUE,
          size = "xs")
        
        msg_log(
          style = "error",
          message = "Effective sample size too low.",
          detail = "Please select a different tracking regime.")
        
      } else {
        
        start_est <- Sys.time()
        msg_log(
          style = "warning",
          message = paste0(
            "Estimating speed for ",
            msg_warning("newly simulated trajectory.")),
          detail = "This may take a while...")
        
        inputList <- ctmmweb::align_lists(list(vals$sd$fit),
                                          list(vals$sd$newdata))
        vals$ctsd_new <- par_speed(inputList,
                                   parallel = vals$parallel)
        
        if ("CI" %in% names(vals$ctsd_new)) {
          vals$ctsd_new <- vals$ctsd_new$CI
        }
        
        nms <- rownames(vals$ctsd_new)
        vals$ctsd_units_new <- extract_units(nms[grep('speed', nms)])
        
        truth <- vals$ctsd_truth[2]
        sdErr <- (vals$ctsd_new[2] - truth) / truth
        sdErr_min <- (vals$ctsd_new[1] - truth) / truth
        sdErr_max <- (vals$ctsd_new[3] - truth) / truth
        vals$sdErr_new <- data.frame("lci" = sdErr_min,
                                     "est" = sdErr,
                                     "uci" = sdErr_max)
        
        # Show buttons to change panels:
        shinyjs::show(id = "ctsdInput_show")
        
        vals$sd$time_new <- difftime(Sys.time(), start, units = 'mins')
        
        msg_log(
          style = "success",
          message = "Speed estimation completed.",
          detail = paste(
            "This step took approximately",
            round(vals$sd$time_new, 1), "minutes."))
      }
      
      shinybusy::remove_modal_spinner()

    }) %>% # end of observe,
      shiny::bindEvent(input$run_sd_new)

    # BLOCKS --------------------------------------------------------------
    ## Tracking regime: ---------------------------------------------------

    output$sdInfo_dur <- shiny::renderUI({
      req(vals$reg$dur, vals$reg$dur_unit)

      out_dur <- fix_unit(vals$reg$dur, vals$reg$dur_unit)

      parBlock(header = "Sampling duration",
               value = paste(out_dur[1], out_dur[2]))

    }) # end of renderUI // sdInfo_dur

    output$sdInfo_dti <- shiny::renderUI({
      req(vals$reg$dti, vals$reg$dti_unit)

      out_dti <- fix_unit(vals$reg$dti, vals$reg$dti_unit)

      parBlock(header = "Sampling interval",
               value = paste(out_dti[1], out_dti[2]),
               subtitle = "between fixes")

    }) # end of renderUI // sdInfo_dti

    observe({
      req(vals$sd$dti, vals$sd$dur)

      output$ctsdText_sims <- renderUI({

        dti2 <- vals$sd$dti
        dti2_txt <- vals$sd$dti_unit
        dur2 <- vals$sd$dur
        dur2_mth <- "months" %#% vals$sd$dur %#% vals$sd$dur_unit

        if (dur2 == 1) {
          dur2_txt <- HTML(paste0(
            "for ",  span("1 day", class = "cl-sea-d"), "."))
        } else { if (dur2 == 365) {
          dur2_txt <- HTML(paste0(
            "for ",  span("1 year", class = "cl-sea-d"), "."))
        } else {
          dur2_txt <- HTML(paste(
            "for a duration of",
            span(round(dur2, 1),
                 "days", class = "cl-dgr"), "(or",
            HTML(paste0(
              span(paste0(round(dur2_mth, 1),
                          " months"), class = "cl-dgr"), ")."))
          )) }
        }

        p(br(),
          "This new tracking regime is equal to",
          "a new location every",
          span(round(dti2, 1), dti2_txt,
               class = "cl-sea-d"),
          dur2_txt)

      }) # end of renderUI // ctsdText_sims

      output$sdInfo_dur_new <- shiny::renderUI({
        req(vals$sd$dur, vals$sd$dur_unit)

        dur <- vals$sd$dur
        tempunits <- vals$sd$dur_unit
        out_dur <- fix_unit(dur, tempunits)

        parBlock(header = "Sampling duration",
                 value = span(paste(out_dur[1], out_dur[2]),
                              class = "cl-mdn"))

      }) # ender of renderUI // sdInfo_dur_new

      output$sdInfo_dti_new <- shiny::renderUI({
        req(vals$sd$dti, vals$sd$dti_unit)

        dti <- vals$sd$dti
        tempunits <- vals$sd$dti_unit

        out_dti <- fix_unit(dti, tempunits)

        parBlock(header = "Sampling interval",
                 value = span(paste(out_dti[1], out_dti[2]),
                              class = "cl-mdn"),
                 subtitle = span("between fixes",
                                 class = "cl-mdn"))

      }) # ender of renderUI // sdInfo_dti_new
    }) # end of observe

    ## Sample sizes: ----------------------------------------------------

    output$sdBlock_n <- shiny::renderUI({
      req(vals$data1)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data1),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)

    }) # end of renderUI // sdBlock_n (absolute sample size)

    output$sdBlock_N2 <- shiny::renderUI({
      req(vals$fit1)

      n <- nrow(vals$data1)
      N <- vals$N2
      
      value <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      sampleBlock(
        number = value,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // sdBlock_N2 (effective)

    output$sdBlock_n_new <- shiny::renderUI({
      req(vals$sd$newdata)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$sd$newdata),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // sdBlock_n_new (absolute)

    output$sdBlock_N2_new <- shiny::renderUI({
      req(vals$sd$newdata, vals$N2_new)

      n <- nrow(vals$sd$newdata)
      N <- vals$N2_new
      
      value <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      sampleBlock(
        number = value,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // sdBlock_N2_new (effective)

    ## Outputs: ---------------------------------------------------------
    ### Speed & distance estimates: -------------------------------------

    output$sdInfo_est <- shiny::renderUI({
      req(vals$ctsd, vals$ctsd_units)

      speed_units <- vals$ctsd_units
      if (speed_units == "kilometers/day") {
        speed_units <- "km/day" }

      est <- vals$ctsd[2]
      est_min <- vals$ctsd[1]
      est_max <- vals$ctsd[3]

      parBlock(
        icon = "gauge-high",
        header = span("Movement speed from", br(),
                      "initial regime:"),
        value = paste(round(est, 1), speed_units),
        subtitle = paste(
          ifelse(est_min == 0,
                 "0", round(est_min, 3)),
          "—", round(est_max, 3)))

    }) # end of renderUI // sdInfo_est

    output$sdInfo_err <- shiny::renderUI({
      req(vals$sdErr)

      errorBlock(
        icon = "radiation",
        text = span("Expected error from", br(),
                    "initial regime:"),
        value = vals$sdErr[[2]],
        min = vals$sdErr[[1]],
        max = vals$sdErr[[3]],
        rightBorder = FALSE)

    }) # end of renderUI // sdInfo_err

    output$sdInfo_est_new <- shiny::renderUI({
      req(vals$ctsd_new)

      speed_units <- vals$ctsd_units_new
      if (speed_units == "kilometers/day") {
        speed_units <- "km/day" }

      est <- vals$ctsd_new[2]
      est_min <- vals$ctsd_new[1]
      est_max <- vals$ctsd_new[3]

      parBlock(
        icon = "gauge-high",
        header = span("Movement speed from", br(),
                    "modified regime:"),
        value = span(round(est, 1), speed_units, class = "cl-mdn"),
        subtitle = span(
          ifelse(est_min == 0,
                 "0", round(est_min, 3)),
          "—", round(est_max, 3), class = "cl-mdn"))

    }) # end of renderUI // sdInfo_est

    output$sdInfo_err_new <- shiny::renderUI({
      req(vals$ctsd_new)

      errorBlock(
        icon = "radiation",
        text = span("Expected error from", br(),
                    "modified regime:"),
        value = vals$sdErr_new[[2]],
        min = vals$sdErr_new[[1]],
        max = vals$sdErr_new[[3]],
        rightBorder = FALSE)

    }) # end of renderUI // sdInfo_err_new

    ### Movement metrics: -----------------------------------------------

    output$distInfo_est <- shiny::renderUI({
      req(vals$data1, vals$distEst)

      est <- fix_unit(vals$distEst[[2]], 
                      vals$distEst[[4]], convert = TRUE)
      lci <- fix_unit(vals$distEst[[1]], est$unit)
      uci <- fix_unit(vals$distEst[[3]], est$unit)
      
      parBlock(
        icon = "map-location-dot",
        header = span("Total distance traveled from", br(),
                       "initial regime:"),
        value = paste(
          scales::label_comma()(est$value), est$unit),
        subtitle = paste(
          scales::label_comma()(lci$value), "—", 
          scales::label_comma()(uci$value)))

    }) # end of renderUI // distInfo_est

    output$distInfo_err <- shiny::renderUI({
      req(vals$distErr)

      errorBlock(
        icon = "radiation",
        text = span("Expected error from", br(),
                    "initial regime:"),
        value = vals$distErr[[2]],
        min = vals$distErr[[1]],
        max = vals$distErr[[3]],
        rightBorder = FALSE)

    }) # end of renderUI // distInfo_err

    output$distInfo_est_new <- shiny::renderUI({
      req(vals$sd$newdata, vals$distEst_new)
      
      est <- fix_unit(vals$distEst_new[[2]],
                      vals$distEst_new[[4]], convert = TRUE)
      lci <- fix_unit(vals$distEst_new[[1]], est$unit)
      uci <- fix_unit(vals$distEst_new[[3]], est$unit)
      
      parBlock(
        icon = "map-location-dot",
        header = span("Total distance traveled from", br(),
                       "modified regime:"),
        value = span(scales::label_comma()(est$value), 
                     est$unit, class = "cl-mdn"),
        subtitle = span(scales::label_comma()(lci$value), "—",
                        scales::label_comma()(uci$value),
                        class = "cl-mdn"))

    }) # end of renderUI // distInfo_est_new

    output$distInfo_err_new <- shiny::renderUI({
      req(vals$distErr_new)

      errorBlock(
        icon = "radiation",
        text = span("Expected error from", br(),
                    "modified regime:"),
        value = vals$distErr_new[[2]],
        min = vals$distErr_new[[1]],
        max = vals$distErr_new[[3]],
        rightBorder = FALSE)

    }) # end of renderUI // distInfo_err_new

    # PLOTS -------------------------------------------------------------
    ## Rendering speed plot (estimate vs. time): ------------------------

    observe({
      req(vals$data_speed, vals$ctsd)

      unit <- input$ctsdVar_group_units
      dat <- vals$data_speed

      t_origin <- "1111-11-10 23:06:32"
      dat$timestamp <- as.POSIXct(dat$t, origin = t_origin)

      if (unit == "minutes") {
        dat$t_new <- round.POSIXt(as.POSIXct(dat$timestamp),
                           units = "mins")
        dat <- dat %>%
          dplyr::mutate(t_new = as.POSIXct(
            t_new, format = "%Y-%m-%d %H:%M:%OS"))
      } else if (unit == "hours") {
        dat$t_new <- round.POSIXt(as.POSIXct(dat$timestamp),
                           units = "hours")
        dat <- dat %>%
          dplyr::mutate(t_new = as.POSIXct(
            t_new, format = "%Y-%m-%d %H:%M:%OS"))
      } else if (unit == "days") {
        dat$t_new <- round.POSIXt(as.POSIXct(dat$timestamp),
                           units = "days")
        dat <- dat %>%
          dplyr::mutate(t_new = as.POSIXct(
            t_new, format = "%Y-%m-%d %H:%M:%OS"))
      } else if (unit == "weeks") {
        dat <- dat %>% dplyr::mutate(
          t_new = format(timestamp, "%U"))
      } else if (unit == "months") {
        dat$t_new <- round.POSIXt(as.POSIXct(dat$timestamp),
                           units = "months")
        dat <- dat %>%
          dplyr::mutate(t_new = as.POSIXct(
            t_new, format = "%Y-%m-%d %H:%M:%OS"))
      }

      dat <- dat %>%
        dplyr::group_by(t_new) %>%
        dplyr::summarise(est = mean(est),
                         low = mean(low),
                         high = mean(high))

      yline <- "day" %#% vals$ctsd[2] %#% "kilometers"

      if (!is.null(vals$ctsd_new)) {
        req(vals$ctsd_new)
        yline_new <- "day" %#% vals$ctsd_new[2] %#% "kilometers"
      }

      label_x <- paste0("Time lag (in ", unit, ")")
      output$sdPlot_main <- ggiraph::renderGirafe({

        p <- dat %>%

          ggplot2::ggplot(
            ggplot2::aes(x = t_new, y = est, group = 1)) +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = low, ymax = high),
            fill = "grey70", alpha = .6) +
          ggplot2::geom_line(color = "black") +

          ggplot2::geom_hline(
            yintercept = yline, size = 2, col = pal$sea) +

          { if (!is.null(vals$ctsd_new))
            ggplot2::geom_hline(
              yintercept = yline_new, size = 2, col = pal$dgr)
          } +

          ggplot2::labs(
            x = label_x,
            y = "Speed estimate (meters/second)") +
          theme_movedesign()

        ggiraph::girafe(ggobj = p)

      }) # end of renderGirafe // sdPlot_main

    }) %>% # end of observe,
      bindEvent(input$ctsdVar_group_units)

    ## Rendering trajectory: --------------------------------------------

    output$sdPlot_path <- ggiraph::renderGirafe({
      req(vals$data1, vals$data_full)

      newdat <- vals$data1
      alldat <- vals$data_full
      datasets <- input$ctsdShow_datasets

      lims <- calc_limit(newdat, alldat)
      if (!is.null(vals$sd$newdata)) {
        lims <- calc_limit(newdat, alldat, vals$sd$newdata)
      }
      
      if ("full" %in% datasets) {
        p1 <- ggplot2::geom_path(
          alldat, mapping = ggplot2::aes(
            x = x, y = y),
          col = "grey80", size = 1.4)
      }

      if ("subset" %in% datasets) {
        p2 <- ggiraph::geom_path_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y),
          size = 0.8, col = pal$sea)
        p2_points <- ggiraph::geom_point_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            tooltip = timestamp),
          size = 2.5, col = pal$sea)
      }

      if ("new" %in% datasets) {
        req(vals$sd$newdata)

        p3 <- ggiraph::geom_path_interactive(
          vals$sd$newdata, mapping = ggplot2::aes(
            x = x, y = y),
          size = 0.8, col = pal$dgr)
        p3_points <- ggiraph::geom_point_interactive(
          vals$sd$newdata, mapping = ggplot2::aes(
            x = x, y = y,
            tooltip = timestamp),
          size = 2.5, col = pal$dgr)
      }

      p <- ggplot2::ggplot() +

        { if ("full" %in% datasets) p1 } +
        
        { if ("subset" %in% datasets) p2 } +
        { if ("subset" %in% datasets) p2_points } +

        { if ("new" %in% datasets) p3 } +
        { if ("new" %in% datasets) p3_points } +

        ggplot2::labs(
          x = "x coordinate",
          y = "y coordinate") +

        ggplot2::scale_x_continuous(
          labels = scales::comma,
          limits = c(lims[["xmin"]], lims[["xmax"]])) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(lims[["ymin"]], lims[["ymax"]])) +
        viridis::scale_color_viridis(
          name = "Tracking time:",
          option = "D", trans = "time",
          breaks = c(min(newdat$time),
                     max(newdat$time)),
          labels = c("Start", "End")) +

        theme_movedesign() +
        ggplot2::guides(
          color = ggplot2::guide_colorbar(
            title.vjust = 1.02)) +
        ggplot2::theme(
          legend.position = c(0.76, 0.08),
          legend.direction = "horizontal",
          legend.title = ggplot2::element_text(
            size = 11, face = "bold.italic"),
          legend.key.height = ggplot2::unit(0.3, "cm"),
          legend.key.width = ggplot2::unit(0.6, "cm")
        )

      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 6,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .1),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_tooltip(use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill:#1279BF;",
                        "stroke:#1279BF;",
                        "cursor:pointer;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))

    }) # end of renderGirafe // sdPlot_path

    # TABLES --------------------------------------------------------------
    ## Initial tracking regime: -------------------------------------------

    sdRow <- reactive({
      
      out <- data.frame(
        data = "Initial",
        tauv = NA,
        dur = NA,
        dti = NA,
        n = nrow(vals$data1),
        N2 = vals$N2,
        ctsd = NA,
        ctsd_err = vals$sdErr[[2]],
        ctsd_err_min = vals$sdErr[[1]],
        ctsd_err_max = vals$sdErr[[3]],
        dist = NA,
        dist_err = vals$distErr[[2]])
      
      out$tauv <- paste(scales::label_comma(
        accuracy = .1)(vals$tau_v0),
        abbrv_unit(vals$tau_v0_units))
      
      out_dur <- fix_unit(vals$reg$dur, vals$reg$dur_unit)
      out$dur <- paste(out_dur$value, abbrv_unit(out_dur$unit))
      
      out_dti <- fix_unit(vals$reg$dti, vals$reg$dti_unit)
      out$dti <- paste(out_dti$value, abbrv_unit(out_dti$unit))

      out_ctsd <- fix_unit(vals$ctsd[2], vals$ctsd_units)
      out$ctsd <- paste(out_ctsd$value, abbrv_unit(out_ctsd$unit))

      out_dist <- fix_unit(vals$distEst[[2]], 
                           vals$distEst[[4]], convert = TRUE)
      out$dist <- paste(out_dist$value, out_dist$unit)
      
      return(out)

    }) %>%
      bindCache(vals$reg$dur, vals$reg$dur_unit,
                vals$reg$dti, vals$reg$dti_unit)

    observe({
      req(vals$data1,  
          vals$ctsd, vals$distErr, vals$distTruth)

      shinyjs::show(id = "sdBox_summary")

      vals$dt_sd <<- rbind(vals$dt_sd, sdRow())
      vals$dt_sd <- dplyr::distinct(vals$dt_sd)
      vals$report_sd_yn <- TRUE

    }) %>% # end of observe
      bindEvent(input$show_sdTable)

    ## Modified tracking regime: ------------------------------------------

    sdRow_new <- reactive({

      out <- data.frame(
        data = "Modified",
        tauv = NA,
        dur = NA,
        dti = NA,
        n = nrow(vals$sd$newdata),
        N2 = vals$N2_new,
        ctsd = NA,
        ctsd_err = vals$sdErr_new[[2]],
        ctsd_err_min = vals$sdErr_new[[1]],
        ctsd_err_max = vals$sdErr_new[[3]],
        dist = NA,
        dist_err = vals$distErr_new[[2]])
      
      out$tauv <- paste(scales::label_comma(
        accuracy = .1)(vals$tau_v0),
        abbrv_unit(vals$tau_v0_units))

      out_dur <- fix_unit(vals$sd$dur, vals$sd$dur_unit)
      out$dur <- paste(out_dur$value, abbrv_unit(out_dur$unit))
      
      out_dti <- fix_unit(vals$sd$dti, vals$sd$dti_unit)
      out$dti <- paste(out_dti$value, abbrv_unit(out_dti$unit))

      out_ctsd <- fix_unit(vals$ctsd_new[2], vals$ctsd_units_new)
      out$ctsd <- paste(out_ctsd$value, abbrv_unit(out_ctsd$unit))

      out_dist <- fix_unit(vals$distEst_new[[2]], 
                           vals$distEst_new[[4]], convert = TRUE)
      out$dist <- paste(out_dist$value, out_dist$unit)
      
      return(out)

    }) %>%
      bindCache(vals$sd$dur, vals$sd$dur_unit,
                vals$sd$dti, vals$sd$dti_unit)

    observe({
      req(vals$distErr_new)
      
      vals$dt_sd <<- rbind(vals$dt_sd, sdRow_new())
      vals$dt_sd <- dplyr::distinct(vals$dt_sd)
      vals$report_sd_yn <- TRUE

    }) %>% # end of observe
      bindEvent(input$show_sdTable)

    ## Rendering output table: --------------------------------------------

    output$sdTable <- reactable::renderReactable({
      req(vals$dt_sd)

      columnNames <- list(
        data = "Data:",
        tauv = "\u03C4\u1D65",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N2 = "N (speed)",
        ctsd = "Speed",
        ctsd_err = "Error",
        ctsd_err_min = "Error (min)",
        ctsd_err_max = "Error (max)",
        dist = "Distance",
        dist_err = "Error")
      
      reactable::reactable(
        data = vals$dt_sd,
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
            minWidth = 60),

        columns = list(
          data = reactable::colDef(
            name = columnNames[["data"]]),
          tauv = reactable::colDef(
            minWidth = 80, name = columnNames[["tauv"]],
            style = list(fontWeight = "bold")),
          dur = reactable::colDef(
            minWidth = 80, name = columnNames[["dur"]],
            style = list(fontWeight = "bold")),
          dti = reactable::colDef(
            minWidth = 80, name = columnNames[["dti"]],
            style = list(fontWeight = "bold")),
          n = reactable::colDef(
            name = columnNames[["n"]],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 0)),
          N2 = reactable::colDef(
            minWidth = 80, name = columnNames[["N2"]],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)),
          ctsd = reactable::colDef(
            minWidth = 80, name = columnNames[["ctsd"]]),
          ctsd_err = reactable::colDef(
            minWidth = 80, name = columnNames[["ctsd_err"]],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)),
          ctsd_err_min = reactable::colDef(
            minWidth = 80, name = columnNames[["ctsd_err_min"]],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)),
          ctsd_err_max = reactable::colDef(
            minWidth = 80, name = columnNames[["ctsd_err_max"]],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)),
          dist = reactable::colDef(
            minWidth = 80, name = columnNames[["dist"]]),
          dist_err = reactable::colDef(
            minWidth = 80, name = columnNames[["dist_err"]],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1))
        ))

    }) # end of renderReactable // sdTable

    observe({
      vals$dt_sd <- NULL
    }) %>% # end of observe,
      bindEvent(input$sdTable_clear)

    # Additional information: -------------------------------------------

    output$time_ctsd <- renderText({
      req(vals$time_ctsd)

      paste("Initial tracking regime took approximately",
            round(vals$time_ctsd, 1), "minutes.")
    })

    output$time_ctsd_new <- renderText({
      req(vals$sd$time)

      paste("Modified tracking regime took approximately",
            round(vals$sd$time, 1), "minutes.")
    })

    # Save information for report if table is not requested:

    observe({
      req(vals$is_analyses,
          vals$active_tab == 'ctsd')

      req(is.null(vals$dt_sd))
      vals$report_sd_yn <- FALSE

      if (is.null(vals$sdErr_new)) {
        req(vals$distErr)
        vals$report_sd_yn <- TRUE
        vals$dt_sd <- sdRow()
      } else {
        req(vals$distErr_new)
        vals$report_sd_yn <- TRUE
        vals$dt_sd <- sdRow_new()
      }
      
    })

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_ctsd_ui("tab_ctsd_1")

## To be copied in the server
# mod_tab_ctsd_server("tab_ctsd_1")
