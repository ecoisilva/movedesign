#' tab_ctsd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
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

            title = span("Speed & distance estimation:", style =
                           paste("padding-top: 14px;", ttl_main)),
            icon = fontawesome::fa(name = "tachometer-alt",
                                   height = "22px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "#e3e3e3"),
            id = ns("ctsd_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = TRUE, closable = FALSE,

            column(
              align = "center", width = 12,

              p(span("Continuous-time speed and distance estimation",
                     "(CTSD)", style = txt_key),
                "overcomes the limitations of straight-line displacement",
                "(SLD) estimation, as", span("CTSD", style = txt_key),
                "provides accurate,",
                "scale-insensitive estimates with reliable confidence",
                "intervals;", em("i.e.,"), "do not vary with the",
                "measurement scale (or error), sampling frequency, or",
                "the tortuosity of the animal’s movement."),

              p(style = ft_center,

                "If estimating speed or distance traveled",
                "is your goal, then click the",
                fontawesome::fa(name = "paper-plane", fill = hex_main),
                HTML(paste0(span("Run estimation", style = btn_primary))),
                "button."),

              splitLayout(
                cellWidths = c("38px", "10px", "200px"),
                cellArgs = list(style = 'align: center;'),

                shiny::actionButton(
                  inputId = ns("ctsdHelp_main"),
                  label = NULL,
                  width = "100%",
                  icon = icon("question-circle"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("run_ctsd"),
                  label = "Run estimation",
                  icon =  icon("paper-plane"),
                  width = "100%",
                  class = "btn-primary")
              ),
              br()

            ) # end of column (for text)
          ), # end of box // ctsd_intro

          uiOutput(ns("ctsdInput_show_all"))

      ), # end of div (top row)

      # [right column] ----------------------------------------------------

      div(class = div1_column_right,

          ## Tracking regime: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Tracking regime", style = ttl_box.solid),
            id = ns("ctsdBox_regime"),
            status = "info",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("ctsdTabs_regime"),

              tabPanel(
                value = ns("ctsdPanel_regime"),
                title = fontawesome::fa(name = "map-marker-alt",
                                        fill = hex_border),
                p(),
                fluidRow(
                  column(width = 12, uiOutput(ns("ctsdInfo_dur"))),
                  column(width = 12, uiOutput(ns("ctsdInfo_dti")))
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("ctsdPanel_regime_new"),
                title = fontawesome::fa(name = "bolt",
                                        fill = hex_caution),
                p(),
                fluidRow(
                  column(width = 12, uiOutput(ns("ctsdInfo_dur_new"))),
                  column(width = 12, uiOutput(ns("ctsdInfo_dti_new")))
                ), # end of fluidRow

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12, align = "center",

              splitLayout(
                cellWidths = c("29%", "1%", "70%"),
                cellArgs = list(style = "align: center;"),

                shiny::actionButton(
                  inputId = ns("ctsdHelp_regime"),
                  label = NULL,
                  width = "100%",
                  icon = icon("question-circle"),
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
          ), # end of box // ctsdBox_regime

          ## Speed and distance estimates: --------------------------------

          shinydashboardPlus::box(
            title = span("Speed estimate:", style = ttl_box),
            id = ns("ctsdBox_speed"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("ctsdTabs_speed"),

              tabPanel(
                value = ns("ctsdPanel_speed"),
                title = fontawesome::fa(name = "map-marker-alt",
                                        fill = hex_border),

                uiOutput(ns("ctsdInfo_speed")),
                uiOutput(ns("ctsdInfo_err")),
                p()

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("ctsdPanel_speed_new"),
                title = fontawesome::fa(name = "bolt",
                                        fill = hex_caution),

                uiOutput(ns("ctsdInfo_speed_new")),
                uiOutput(ns("ctsdInfo_err_new")),

                p()

              ) # end of panels (2 out of 2)
            ) # end of tabs
          ), # end of box // ctsdBox_speed

          shinydashboardPlus::box(
            title = span("Distance estimate:", style = ttl_box),
            id = ns("ctsdBox_dist"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("ctsdTabs_dist"),

              tabPanel(
                value = ns("ctsdPanel_dist"),
                title = fontawesome::fa(name = "map-marker-alt",
                                        fill = hex_border),

                uiOutput(ns("distInfo_total")),
                uiOutput(ns("distInfo_err")),
                p()

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("ctsdPanel_dist_new"),
                title = fontawesome::fa(name = "bolt",
                                        fill = hex_caution),

                uiOutput(ns("distInfo_total_new")),
                uiOutput(ns("distInfo_err_new")),
                p()

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12, align = "center",

              splitLayout(
                cellWidths = c("29%", "1%", "70%"),
                cellArgs = list(style = "align: center;"),

                shiny::actionButton(
                  inputId = ns("ctsdHelp_bias"),
                  label = NULL,
                  width = "100%",
                  icon = icon("question-circle"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("show_ctsdTable"),
                  label = "Table",
                  icon = icon("list"),
                  width = "100%",
                  class = "btn-primary")
              ) # end of splitLayout

            ) # end of column (footer)
          ) # end of box // ctsdBox_speed


      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(class = div1_column_left,

          ## Sample sizes: ------------------------------------------------

          shinydashboardPlus::box(
            title = span("Sample sizes:", style = ttl_box),
            id = ns("ctsdBox_sizes"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("ctsdTabs_sizes"),

              tabPanel(
                value = ns("ctsdPanel_sizes"),
                title = fontawesome::fa(name = "map-marker-alt",
                                        fill = hex_border),

                fluidRow(
                  column(width = 6, uiOutput(ns("ctsdBlock_n"))),
                  column(width = 6, uiOutput(ns("ctsdBlock_Nspeed"))),
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("ctsdPanel_sizes_new"),
                title = tagList(
                  fontawesome::fa(name = "bolt", fill = hex_caution),
                  span("Conditional simulation", style =
                         paste0(ttl_panel, col_caution))
                ),

                fluidRow(
                  column(width = 6, uiOutput(ns("ctsdBlock_n_new"))),
                  column(width = 6, uiOutput(ns("ctsdBlock_Nspeed_new"))),
                ) # end of fluidRow

              ) # end of panels (2 out of 2)
            ) # end of tabs
          ), # end of box // ctsdBox_sizes

          ## Speed & distance plots: --------------------------------------

          uiOutput(ns("ctsdBox_outputs")),

      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          ## Table: -------------------------------------------------------

          shinydashboardPlus::box(
            title = span("Summary table:", style = ttl_box),
            id = ns("ctsdBox_summary"),
            width = NULL,
            solidHeader = FALSE,

            DT::dataTableOutput(ns("ctsdTable")),
            br(),
            div(style = "display:inline-block; float:right",
                shiny::actionButton(
                  inputId = ns("ctsdTable_clear"),
                  label = "Clear table",
                  icon =  icon("trash"),
                  width = "110px")), br()

          ), # end of box // ctsdBox_summary

          ## Additional information: --------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", style = ttl_box),
            id = ns("ctsdBox_misc"),
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

    # DYNAMIC UI ELEMENTS -----------------------------------------------
    ## Hide all boxes at start: -----------------------------------------

    tmpnames <- c("regime",
                  "speed",
                  "dist",
                  "sizes",
                  "viz",
                  "vizdist",
                  "summary",
                  "misc")

    for(i in 1:length(tmpnames)) {
      shinyjs::hide(id = paste0("ctsdBox_", tmpnames[i]))
    }

    ## Hide secondary tabs at start: ------------------------------------

    vec <- c("regime", "speed", "dist", "sizes")

    for(i in 1:length(vec)) {
      tmp_id <- paste0("ctsdTabs_", vec[i])
      tmp_target <- paste0("ctsdPanel_", vec[i], "_new")
      hideTab(inputId = tmp_id, target = ns(tmp_target))
    }

    ## Changing group options for plot: ---------------------------------

    output$ctsdVar_group <- renderUI({
      req(vals$dur0_dev, vals$dti0_dev)

      eval_dur <- vals$dur0_dev
      eval_dti <- vals$dti0_dev

      group_choices <- c(
        "Minutes" = "minutes",
        "Hours" = "hours",
        "Days" = "days",
        "Weeks" = "weeks",
        "Months" = "months")

      if(eval_dur <= (5 %#% "days")) {
        group_selected <- "hours"
        group_min <- "minutes"
        group_max <- "days"

      } else if(eval_dur > (5 %#% "days") &&
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
                     style = paste0(ft_center, txt_label_bold)),
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
      if(!is.null(vals$data1)) {
        if(vals$tmpid != vals$id) {

          shinyalert::shinyalert(
            title = "Oops!",
            text = span(
              "Data selected is from individual",
              HTML(paste0(span(vals$id, style = col_caution),
                          ",")), "but parameters are from",
              HTML(paste0(span(vals$tmpid, style = col_caution), ".")),
              br(),

              "Please extract parameters in the",
              fontawesome::fa(name = "paw", fill = hex_main),
              span("Data", style = col_main), "tab",
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
            span('movement dataset', style = col_caution),
            'first in the',
            fontawesome::fa(name = "paw", fill = hex_main),
            span('Data', style = col_main), "tabs."),
          html = TRUE,
          size = "xs")

      } # end of if(), checking for data
    }) # end of observe

    ## Checking if tau v available for initial dataset: -----------------

    observe({
      req(vals$valid_tauv, vals$active_tab == "ctsd")
      if(vals$valid_tauv == "No") {

        shinyalert::shinyalert(
          type = "error",

          title = "Speed & distance invalid",
          text = span(
            "No statistically significant signature of the animal's",
            span("velocity", style = txt_caution),
            "remains in this dataset.",
            "Please select a different individual or dataset to",
            "proceed with", span("distance/speed", style = txt_caution),
            "estimation"),

          confirmButtonText = "Dismiss",
          html = TRUE)

        msg_log(
          style = "danger",
          message = paste0("Initial dataset has no remaining ",
                           msg_danger("velocity"), " signature."),
          detail = paste0("Select a different individual",
                          "or dataset to proceed."))

        shinyjs::disable("run_ctsd")
      }

    }) # end of observe

    ## Adding speed & distance plot box: --------------------------------

    output$ctsdBox_outputs <- renderUI({
      req(vals$ctsd, vals$ctsd_truth)

      shinydashboard::tabBox(
        id = ns("ctsdTabs_outputs"),
        title = "Plots",
        width = NULL,

        tabPanel(
          title = "Trajectory",
          value = ns("regPanel_trajectory"),
          icon = icon("route"),

          ggiraph::girafeOutput(
            outputId = ns("ctsdPlot_path"),
            width = "100%", height = "100%"),

          uiOutput(ns("ctsdVar_reveal"))

        ), # end of panel (1 out of 2)

        tabPanel(
          title = "Speed estimates",
          value = ns("regPanel_speed"),
          icon = icon("tachometer-alt"),

          ggiraph::girafeOutput(
            outputId = ns("ctsdPlot_main"),
            width = "100%", height = "100%"),

          uiOutput(ns("ctsdVar_group"))

        ) # end of panel (2 out of 2)
      ) # end of tabBox
    }) # end of renderUI // regBox_pars

    ## Adding different datasets to trajectory plot: --------------------

    output$ctsdVar_reveal <- renderUI({

      if(is.null(vals$data2_ctsd)) {
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("ctsdShow_datasets"),
          label = "Show trajectories:",
          choices =
            c("Fine-scale" = "full",
              "Initial tracking regime" = "subset"),
          selected = c("full", "subset"),
          checkIcon = list(yes = icon("check-circle")),
          justified = TRUE)

      } else {
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("ctsdShow_datasets"),
          label = "Show trajectories:",
          choices =
            c("Fine-scale" = "full",
              "Initial regime" = "subset",
              "New regime" = "new"),
          selected = c("full", "subset", "new"),
          checkIcon = list(yes = icon("check-circle")),
          justified = TRUE) }

    }) # end of renderUI // ctsdVar_reveal

    ## Quick select all boxes after adjusting tracking regime: ----------

    observe({

      if(input$ctsdInput_show == 1) {
        tmp <- ""
      } else {
        tmp <- "_new"
      }

      tabs <- paste0("ctsdTabs_", vec)
      panels <- paste0("ctsdPanel_", vec)

      for(v in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[v]),
          selected = paste0("tab_ctsd_1-", panels[v], tmp)) }

      vals$paths_selected <- c("full", "subset", "new")

    }) %>% # end of observe.
      bindEvent(input$ctsdInput_show)


    # PROCESSING --------------------------------------------------------
    ## Fitting movement model (if needed): ------------------------------

    observe({
      req(vals$active_tab == "ctsd")

      if(is.null(vals$data1)) {

        shinyalert::shinyalert(
          type = "error",
          title = "No tracking regime set",
          text = span(
            "Please go to the",
            fontawesome::fa(name = "map-marker-alt", fill = hex_main),
            span("Tracking regime", style = col_main), "tab",
            "and make sure to both (1) set a tracking regime, and",
            "(2) run a new simulation by pressing the",
            fontawesome::fa(name = "bolt", fill = hex_caution),
            span("'Run'", style = col_main), "button."
          ),
          html = TRUE,
          size = "xs")

      } else {

        if(vals$needs_fit) {

          msg_log(
            style = "danger",
            message = paste0("Model fit ",
                             msg_danger("not found"), "."),
            detail = "Please wait for 'ctmm.select()' to finish.")

          start <- Sys.time()
          newmod <- prepare_pars(
            tau_p0 = vals$tau_p0, tau_p0_units = vals$tau_p0_units,
            tau_v0 = vals$tau_v0, tau_v0_units = vals$tau_v0_units,
            sigma0 = vals$sigma0, sigma0_units = vals$sigma0_units)

          shiny::withProgress({
            fit1 <- ctmm::ctmm.fit(vals$data1, newmod)
          },
          message = "Fitting movement model.",
          detail = "This may take a while...")

          msg_log(
            style = 'success',
            message = paste0("Model fit ",
                             msg_success("completed"), "."),
            detail = paste(
              "This step took approximately",
              round(difftime(Sys.time(), start, units = 'mins'), 1),
              "minutes."))

          vals$guess <- NULL
          vals$fit1 <- fit1
          vals$needs_fit <- FALSE

        } # end of if(vals$needs_fit)
      } # end of if(is.null(vals$data1))

    }) # end of observe

    observe({
      shinyjs::show(id = 'ctsdBox_regime')
      shinyjs::show(id = 'ctsdBox_sizes')
    }) %>% bindEvent(vals$fit1)

    ## Calculating relative errors for CTSD estimates: ------------------

    observe({
      req(vals$ctsd, vals$ctsd_truth)

      # print(vals$ctsd)

      tmpctsd_est <- vals$ctsd[2]
      tmpctsd_lci <- vals$ctsd[1]
      tmpctsd_uci <- vals$ctsd[3]
      tmptruth <- vals$ctsd_truth[2]

      vals$ctsdErr <- (tmpctsd_est - tmptruth) / tmptruth
      vals$ctsdErr_min <- (tmpctsd_lci - tmptruth) / tmptruth
      vals$ctsdErr_max <- (tmpctsd_uci - tmptruth) / tmptruth

    }) # end of observe

    observe({
      req(vals$ctsd_new)

      tmpctsd_est <- vals$ctsd_new[2]
      tmpctsd_lci <- vals$ctsd_new[1]
      tmpctsd_uci <- vals$ctsd_new[3]
      tmptruth <- vals$ctsd_truth[2]

      vals$ctsdErr_new <- (tmpctsd_est - tmptruth) / tmptruth
      vals$ctsdErr_min_new <- (tmpctsd_lci - tmptruth) / tmptruth
      vals$ctsdErr_max_new <- (tmpctsd_uci - tmptruth) / tmptruth

    }) # end of observe


    ## Calculating total and mean distances: ----------------------------

    observe({
      req(vals$data1, vals$data_full,
          vals$data_speed, vals$ctsd)

      # dat1 <- data.frame(
      #   x = vals$data1$x,
      #   y = vals$data1$y)

      dat_full <- data.frame(
        x = vals$data_full$x,
        y = vals$data_full$y)

      # tmpdist <- list()
      # for(i in 2:nrow(dat1)) {
      #   tmpdist[[i]] <-
      #     sqrt((dat1$x[i] - dat1$x[i-1])^2 +
      #            (dat1$y[i] - dat1$y[i-1])^2)
      # }
      # vals$data1$dist <- c(0, do.call("rbind", tmpdist))

      tmpdist_full <- list()
      for(i in 2:nrow(dat_full)) {
        tmpdist_full[[i]] <-
          sqrt((dat_full$x[i] - dat_full$x[i-1])^2 +
                 (dat_full$y[i] - dat_full$y[i-1])^2)
      }
      vals$data_full$dist <- c(0, do.call("rbind", tmpdist_full))

      dist_truth <- sum(vals$data_full$dist, na.rm = TRUE)
      vals$dist_truth <- dist_truth

      vals$dist_lci <- as.numeric(
        vals$ctsd[1] * ("days" %#% vals$dur0_dev)) %#% "kilometers"
      vals$dist_est <- as.numeric(
        vals$ctsd[2] * ("days" %#% vals$dur0_dev)) %#% "kilometers"
      vals$dist_uci <- as.numeric(
        vals$ctsd[3] * ("days" %#% vals$dur0_dev)) %#% "kilometers"

      vals$distErr <- (vals$dist_est - dist_truth) / dist_truth
      vals$distErr_min <- (vals$dist_lci - dist_truth) / dist_truth
      vals$distErr_max <- (vals$dist_uci - dist_truth) / dist_truth

    }) # end of observe

    observe({
      req(vals$ctsd_new)

      dur <- "days" %#% vals$dur0_dev

      vals$dist_lci_new <-
        as.numeric(vals$ctsd_new[1] * dur) %#% "kilometers"
      vals$dist_est_new <-
        as.numeric(vals$ctsd_new[2] * dur) %#% "kilometers"
      vals$dist_uci_new <-
        as.numeric(vals$ctsd_new[3] * dur) %#% "kilometers"

      vals$distErr_new <-
        (vals$dist_est_new - vals$dist_truth) / vals$dist_truth
      vals$distErr_min_new <-
        (vals$dist_lci_new - vals$dist_truth) / vals$dist_truth
      vals$distErr_max_new <-
        (vals$dist_uci_new - vals$dist_truth) / vals$dist_truth

    }) # end of observe

    # ADJUSTING TRACKING REGIME -----------------------------------------
    # Adjust sampling parameters necessary for simulation:

    observe({
      req(vals$dur0_dev, vals$dti0_dev)

      if(!is.null(vals$ctsd)) {

        # Sampling duration:

        dur <- round("days" %#% vals$dur0_dev, 0)
        tau_p0 <- round("days" %#% vals$tau_p0 %#%
                          vals$tau_p0_units, 0)

        # Sampling interval:

        fixrate <- movedesign::gps_fixrate
        df_fixrate <- dplyr::arrange(fixrate, dplyr::desc(freq))
        dti_choices <- df_fixrate$nu_notes

        tau_v0 <- vals$tau_v0 %#% vals$tau_v0_units

        maxvalue <- 1 %#% "day" # tau_v0 * 3 OR dti
        maxindex <- which.min(abs(df_fixrate$nu - maxvalue))
        newvalue <- tau_v0 * 1/3
        newindex <- which.min(abs(df_fixrate$nu - newvalue))

        if(vals$data_type == "simulated") {
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
              style = paste("margin-right: 40px;",
                            "margin-left: 40px;"),

              p("Here you can adjust",
                span("sampling parameters", style = txt_key),
                "to predict or simulate additional data from the",
                "same", span("movement model", style = txt_key),
                "of the species provided."),

              p("Based on your research question,",
                "we recommend that",
                span("sampling interval", style = txt_border),
                "be equal (if not at least 3 times shorter) than the",
                span(HTML(paste0("velocity autocorrelation ",
                                 "(\u03C4", tags$sub("v"), ")")),
                     style = txt_border), "value, to reduce bias",
                "of the conditional simulation."
              ),

              parBlock(
                text = span(
                  HTML(paste0("Velocity autocorrelation ",
                              "(\u03C4", tags$sub("v"), ")"))),
                header =
                  paste(scales::label_comma(
                    accuracy = .1)(vals$tau_v0),
                    vals$tau_v0_units),
                number = tmprange),

              shiny::sliderInput(
                inputId = ns('ctsd_dur0'),
                label = "Sampling duration (in days):",
                width = "100%",
                min = 1, max = tau_p0 * 100, value = tau_p0 * 10
              ),

              shinyWidgets::sliderTextInput(
                inputId = ns('ctsd_dti0'),
                label = "Sampling interval:",
                width = "100%",

                choices = dti_choices,
                selected = dti_choices[newindex],
                from_min = dti_choices[1],
                from_max = dti_choices[maxindex]),

              uiOutput(ns("ctsdText_sampling")),

              p(span("Proceed with caution!", style = txt_caution),
                "Longer sampling durations and lower sampling",
                "intervals will add run time to simulation, model",
                "fitting, and estimation functions."),

            ), # end of fluidRow

            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                inputId = ns("run_ctsd_new"),
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
            fontawesome::fa(name = "paper-plane", fill = hex_main),
            HTML(paste0(span("Run estimation", style = btn_primary))),
            "button."),

          html = TRUE,
          size = "xs")

      } # end of ifelse statement
    }) %>% bindEvent(input$ctsd_adjRegime)

    output$ctsdText_sampling <- renderUI({

      fixrate <- movedesign::gps_fixrate
      tmp <- fixrate$nu[match(input$ctsd_dti0,
                              fixrate$nu_notes)]

      vals$n_new <- length(
        seq(from = 1,
            to = input$ctsd_dur0 %#% "days",
            by = tmp))

      splitLayout(

        parBlock(text = "Number of original locations:",
                 header = scales::label_comma(
                   accuracy = 1)(nrow(vals$data1))),

        parBlock(text = "Number of new locations:",
                 header = span(scales::label_comma(
                   accuracy = 1)(vals$n_new),
                   style = col_caution))

      ) # end of splitLayout
    }) # end of renderUI // ctsdText_sampling

    # CTSD ESTIMATION ---------------------------------------------------
    ## Estimating for initial tracking regime: --------------------------

    observe({
      req(vals$is_valid, vals$data1)

      tmplist <- list("ctsdBox_speed",
                      "ctsdBox_dist",
                      "ctsdBox_viz",
                      "ctsdBox_vizdist",
                      "ctsdBox_misc")

      for(i in 1:length(tmplist)) {
        shinyjs::show(id = tmplist[i]) }

      eval_dti <- vals$dti0_dev
      eval_tauv <- vals$tau_v0 %#% vals$tau_v0_units

      if(eval_dti > 3 * eval_tauv) {

        shinyalert::shinyalert(
          title = "Warning",
          text = span(
            "Selected individual has a",
            span("sampling interval", style = txt_key),
            "3x greater than the",
            HTML(paste0(span("velocity autocorrelation",
                             style = txt_caution), ".")),
            "This means that the error associated with the",
            "CTSD estimate is too high to continue evaluating",
            "the study design.", br(),
            "Please select a different individual, with a smaller",
            HTML(paste0(span("sampling interval",
                             style = txt_key), "."))),

          html = TRUE,
          size = "xs")

      } else {

        msg_log(
          style = "warning",
          message = paste0(
            "Simulating for ",
            msg_warning("current trajectory"),
            msg_step(1, 2, style = "warning")),
          detail = "This may take a while...")

        shiny::withProgress({
          start_ctsd <- Sys.time()
          vals$ctsd <- ctmm::speed(vals$data1,
                                   vals$fit1)
        },
        message = "Estimating speed/distance.",
        detail = "This may take a while...")

        if("CI" %in% names(vals$ctsd)) {
          vals$ctsd <- vals$ctsd$CI
        }

        tempnames <- rownames(vals$ctsd)
        vals$ctsd_units <-
          tempnames[grep('speed', tempnames)] %>%
          extract_units()

        msg_log(
          style = 'success',
          message = paste0(
            "Estimation ", msg_success("completed"),
            msg_step(1, 2, style = "success")),
          detail = paste(
            "This step took approximately",
            round(difftime(Sys.time(), start_ctsd,
                           units = 'mins'), 1),
            "minutes."))

        vals$data_speed <- ctmm::speeds(vals$data1,
                                        vals$fit1,
                                        units = FALSE)

        ### Simulating fine-scale trajectory: ---------------------------

        msg_log(
          style = "warning",
          message = paste0(
            "Simulating for ",
            msg_warning("fine-scale trajectory"),
            msg_step(2, 2, style = "warning")),
          detail = "This may take a while...")

        if(vals$data_type == "simulated") {
          dat <- vals$data0
        } else {
          dat <- vals$data1
        }

        shiny::withProgress({
          start_truth <- Sys.time()
          sim_full <- ctmm::simulate(
            dat,
            vals$fit0,
            t = seq(0, vals$dur0_dev, by = 1 %#% "minute"))
          vals$data_full <- sim_full
          incProgress(0.25)

          vals$ctsd_truth <- ctmm::speed(
            sim_full[which(sim_full$t <= (1 %#% "day")), ],
            vals$fit0,
            cores = -1)

        },
        min = 0, max = 1, value = 0.25,
        message = "Estimating speed of fine-scale trajectory.",
        detail = "This may take a while...")

        if("CI" %in% names(vals$ctsd_truth)) {
          vals$ctsd_truth <- vals$ctsd_truth$CI
        }

        tempnames <- rownames(vals$ctsd_truth)
        vals$ctsd_truth_units <-
          tempnames[grep('speed', tempnames)] %>%
          extract_units()

        vals$time_ctsd_truth <- difftime(
          Sys.time(), start_truth, units = 'mins')

        msg_log(
          style = "success",
          message = paste0(
            "Estimation ", msg_success("completed"),
            msg_step(2, 2, style = "success")),
          detail = paste(
            "This step took approximately",
            round(vals$time_ctsd_truth, 1),
            "minutes."))

        vals$time_ctsd <- difftime(Sys.time(),
                                   start_ctsd,
                                   units = 'mins')

      } # end of if() dti & tau evaluation
    }) %>% # end of observe, then:
      bindEvent(input$run_ctsd)

    ## Estimating for new tracking regime: ------------------------------

    observe({
      req(vals$data1, vals$fit1)

      # Show "conditional simulation" panels:
      for(i in 1:length(vec)) {
        tmp_id <- paste0("ctsdTabs_", vec[i])
        tmp_target <- paste0("ctsdPanel_", vec[i], "_new")
        showTab(inputId = tmp_id, target = ns(tmp_target))
      }

      tabs <- paste0("ctsdTabs_", vec)
      panels <- paste0("ctsdPanel_", vec)
      for(v in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[v]),
          selected = paste0("tab_ctsd_1-",
                            panels[v], "_new")) }

      # Capture new sampling duration and interval:
      vals$dti2_units <- sub('^.* ([[:alnum:]]+)$',
                             '\\1', input$ctsd_dti0)

      fixrate <- movedesign::gps_fixrate
      tmp <- fixrate$nu[match(input$ctsd_dti0,
                              fixrate$nu_notes)]

      vals$dti2 <- round((vals$dti2_units %#% tmp), 0)
      vals$dur2 <- input$ctsd_dur0
      vals$dur2_units <- "days"

      ### 1. Simulate new dataset:
      # Fill in the gaps of original dataset + new duration:

      vals$conditional <- TRUE

      msg_log(
        style = "warning",
        message = paste0("Simulating ",
                         msg_warning("new tracking regime"), "."),
        detail = "This may take a while...")

      t0 <- seq(1,
                vals$dur2 %#% vals$dur2_units,
                by = vals$dti2 %#% vals$dti2_units)

      if(length(t0) <= 64000) {

        if(vals$data_type == "simulated") {
          dat <- vals$data0
        } else {
          dat <- vals$data1
        }

        start_sim <- Sys.time()
        df2 <- ctmm::simulate(
          dat,
          vals$fit0,
          t = seq(0,
                  vals$dur2 %#% vals$dur2_units,
                  by = vals$dti2 %#% vals$dti2_units))

        df2 <- ctmm:::pseudonymize(df2)
        df2$index <- 1:nrow(df2)
        vals$data2_ctsd <- df2

        msg_log(
          style = "success",
          message = "Simulation completed.",
          detail = paste(
            "This step took approximately",
            round(difftime(Sys.time(), start_sim,
                           units = 'secs'), 1),
            "seconds."))

        ### 2. Fit models to simulation:

        msg_log(
          style = "warning",
          message = "Fitting models to simulation.",
          detail = "This may take a while...")

        start <- Sys.time()
        if(vals$data_type != "simulated") {
          mod <- prepare_pars(
            tau_p0 = vals$tau_p0,
            tau_p0_units = vals$tau_p0_units,
            tau_v0 = vals$tau_v0,
            tau_v0_units = vals$tau_v0_units,
            sigma0 = vals$sigma0,
            sigma0_units = vals$sigma0_units)
        } else {
          mod <- vals$ctmm_mod
        }

        fit2 <- reactive({
          ctmm::ctmm.fit(
            vals$data2_ctsd,
            mod,
            method = "pHREML",
            control = list(method = "pNewton",
                           cores = -1))

        }) %>% bindCache(vals$dti2,
                         vals$dti2_units,
                         vals$dur2,
                         vals$dur2_units)

        start <- Sys.time()
        shiny::withProgress({
          vals$fit2 <- fit2()
        },
        message = "Fitting models to simulation.",
        detail = "This may take a while...")

        msg_log(
          style = "success",
          message = "Model fit completed.",
          detail = paste(
            "This step took approximately",
            round(difftime(Sys.time(), start, units = 'mins'), 1),
            "minutes."))

        vec <- c("regime", "speed", "size", "viz")
        for(i in 1:length(vec)) {
          updateTabsetPanel(
            session,
            inputId = paste0("ctsdTabs_", vec[i]),
            selected = paste0("tab_ctsd_1-ctsdPanel_",
                              vec[i], "_new"))
        }

        ### 3. Run the speed/distance estimator (CTSD):

        msg_log(
          style = "warning",
          message = paste0(
            "Estmating speed for ",
            msg_warning("newly simulated trajectory.")),
          detail = "This may take a while...")

        shiny::withProgress({
          start_ctsd_new <- Sys.time()
          vals$ctsd_new <- ctmm::speed(
            vals$data2_ctsd,
            CTMM = vals$fit0,
            fast = TRUE)
        },
        message = "Estimating speed of simulated trajectory.",
        detail = "This may take a while...")

        if("CI" %in% names(vals$ctsd_new)) {
          vals$ctsd_new <- vals$ctsd_new$CI
        }

        tempnames <- rownames(vals$ctsd_new)
        vals$ctsd_units_new <-
          tempnames[grep('speed', tempnames)] %>%
          extract_units()

        msg_log(
          style = "success",
          message = "Speed estimation completed.",
          detail = paste(
            "This step took approximately",
            round(vals$time_ctsd_new, 1),
            "minutes."))

        vals$time_ctsd_new <- difftime(
          Sys.time(),
          start_ctsd_new,
          units = 'mins')


      } else {

        shinyalert::shinyalert(
          title = "Warning",
          text = span(
            "Many GPS units can only hold a maximum of",
            HTML(paste0(
              span("32,000—64,000 new locations", style = txt_caution),
              ",")), "making this tracking regime unlikely.",
            "Please select a different combination of",
            span("sampling duration", style = txt_caution), "and",
            HTML(paste0(span("frequency", style = txt_caution), "."))
          ),

          html = TRUE,
          size = "xs")

        msg_log(
          style = "danger",
          message = paste0("Most GPS units may only hold ",
                           msg_danger("32,000—64,000"),
                           " locations."),
          detail = "Please select a different tracking regime.")
      }

      removeModal()

      # Show buttons to change panels:
      output$ctsdInput_show_all <- renderUI({
        shinyWidgets::radioGroupButtons(
          inputId = ns("ctsdInput_show"),
          label = NULL,
          choices = c("Show initial tracking regime" = 1,
                      "Show modified tracking regime" = 2),
          checkIcon = list(yes = icon("check-circle")),
          selected = 2,
          justified = TRUE)
      })

    }) %>% # end of observe, then:
      shiny::bindEvent(input$run_ctsd_new)

    # BLOCKS ------------------------------------------------------------
    ## Tracking regime: -------------------------------------------------

    output$ctsdInfo_dur <- shiny::renderUI({
      req(vals$dur0_dev)

      out_dur <- fix_time(vals$dur0_units_dev %#% vals$dur0_dev,
                               vals$dur0_units_dev)

      parBlock(text = "Sampling duration",
               header = paste(out_dur[1], out_dur[2]))

    }) # end of renderUI // ctsdInfo_dur

    output$ctsdInfo_dti <- shiny::renderUI({
      req(vals$dti0_dev)

      out_dti <- fix_time(vals$dti0_units_dev %#% vals$dti0_dev,
                               vals$dti0_units_dev)

      parBlock(text = "Sampling interval",
               header = paste(out_dti[1], out_dti[2]),
               number = "between fixes")

    }) # end of renderUI // ctsdInfo_dti

    observe({
      req(vals$dti2, vals$dur2)

      output$ctsdText_sims <- renderUI({

        dti2 <- vals$dti2
        dti2_txt <- vals$dti2_units
        dur2 <- vals$dur2
        dur2_mth <- "months" %#% vals$dur2 %#% vals$dur2_units

        if(dur2 == 1) {
          dur2_txt <- HTML(paste0(
            "for ",  span("1 day", style = txt_key), "."))
        } else { if(dur2 == 365) {
          dur2_txt <- HTML(paste0(
            "for ",  span("1 year", style = txt_key), "."))
        } else {
          dur2_txt <- HTML(paste(
            "for a duration of",
            span(round(dur2, 1),
                 "days", style = txt_caution), "(or",
            HTML(paste0(
              span(paste0(round(dur2_mth, 1),
                          " months"), style = txt_caution), ")."))
          )) }
        }

        p(br(),
          "This new tracking regime is equal to",
          "a new location every",
          span(round(dti2, 1), dti2_txt,
               style = txt_key),
          dur2_txt)

      }) # end of renderUI // ctsdText_sims

      output$ctsdInfo_dur_new <- shiny::renderUI({
        req(vals$dur2, vals$dur2_units)

        dur <- vals$dur2
        tempunits <- vals$dur2_units
        out_dur <- fix_time(dur, tempunits)

        parBlock(text = "Sampling duration",
                 header = span(paste(out_dur[1], out_dur[2]),
                               style = col_caution))

      }) # ender of renderUI // ctsdInfo_dur_new

      output$ctsdInfo_dti_new <- shiny::renderUI({
        req(vals$dti2, vals$dti2_units)

        dti <- vals$dti2
        tempunits <- vals$dti2_units

        out_dti <- fix_time(dti, tempunits)

        parBlock(text = "Sampling interval",
                 header = span(paste(out_dti[1], out_dti[2]),
                               style = col_caution),
                 number = span("between fixes",
                               style = col_caution))

      }) # ender of renderUI // ctsdInfo_dti_new
    }) # end of observe

    ## Sample sizes: ----------------------------------------------------

    output$ctsdBlock_n <- shiny::renderUI({
      req(vals$data1)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data1),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)

    }) # end of renderUI // ctsdBlock_n (absolute sample size)

    output$ctsdBlock_Nspeed <- shiny::renderUI({
      req(vals$fit1)

      tmpnames <- names(summary(vals$fit1)$DOF)
      N <- summary(vals$fit1)$DOF[grep('speed', tmpnames)][[1]]
      n <- nrow(vals$data1)

      diff_perc <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      vals$Nspeed <- N

      sampleBlock(
        number = diff_perc,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // ctsdBlock_Nspeed (effective)

    output$ctsdBlock_n_new <- shiny::renderUI({
      req(vals$data2_ctsd)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data2_ctsd),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // ctsdBlock_n_new (absolute sample size)

    output$ctsdBlock_Nspeed_new <- shiny::renderUI({
      req(vals$data2_ctsd, vals$fit2)

      tempnames <- names(summary(vals$fit2)$DOF)
      N <- summary(vals$fit2)$DOF[grep('speed', tempnames)][[1]]
      n <- nrow(vals$data2_ctsd)

      diff_perc <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      sampleBlock(
        number = diff_perc,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // ctsdBlock_Nspeed_new (effective)

    ## Outputs: ---------------------------------------------------------
    ### Estimates: ------------------------------------------------------

    output$ctsdInfo_speed <- shiny::renderUI({
      req(vals$ctsd)

      speed_units <- vals$ctsd_units
      if(speed_units == "kilometers/day") {
        speed_units <- "km/day" }

      est <- vals$ctsd[2]
      est_min <- vals$ctsd[1]
      est_max <- vals$ctsd[3]

      parBlock(
        icon = "tachometer-alt",
        text = "Movement speed",
        header = paste(round(est, 1), speed_units),
        number = paste(
          ifelse(est_min == 0,
                 "0", round(est_min, 3)),
          "—", round(est_max, 3)))

    }) # end of renderUI // ctsdInfo_speed

    output$ctsdInfo_err <- shiny::renderUI({
      req(vals$ctsd, vals$ctsdErr)

      errorBlock(
        icon = "radiation",
        text = "Expected error",
        value = vals$ctsdErr,
        min = vals$ctsdErr_min,
        max = vals$ctsdErr_max,
        rightBorder = FALSE)

    }) # end of renderUI // ctsdInfo_err

    output$ctsdInfo_speed_new <- shiny::renderUI({
      req(vals$ctsd_new)

      speed_units <- vals$ctsd_units_new
      if(speed_units == "kilometers/day") {
        speed_units <- "km/day" }

      est <- vals$ctsd_new[2]
      est_min <- vals$ctsd_new[1]
      est_max <- vals$ctsd_new[3]

      parBlock(
        icon = "tachometer-alt",
        text = "Movement speed",
        header = paste(round(est, 1), speed_units),
        number = paste(
          ifelse(est_min == 0,
                 "0", round(est_min, 3)),
          "—", round(est_max, 3)))

    }) # end of renderUI // ctsdInfo_speed

    output$ctsdInfo_err_new <- shiny::renderUI({
      req(vals$ctsd_new)

      errorBlock(
        icon = "radiation",
        text = "Expected error",
        value = vals$ctsdErr_new,
        min = vals$ctsdErr_min_new,
        max = vals$ctsdErr_max_new,
        rightBorder = FALSE)

    }) # end of renderUI // ctsdInfo_err_new

    ### Movement metrics: -----------------------------------------------

    output$distInfo_total <- shiny::renderUI({
      req(vals$data1)

      est <- ifelse(
        vals$dist_est <= 1 %#% "kilometer",
        paste(
          scales::label_comma(
            accuracy = .1)(vals$dist_est), "m"),
        paste(
          scales::label_comma(
            accuracy = .1)("km" %#% vals$dist_est), "km"))

      lci <- scales::label_comma(
        accuracy = .1)("km" %#% vals$dist_lci)

      uci <- scales::label_comma(
        accuracy = .1)("km" %#% vals$dist_uci)

      parBlock(
        icon = "map-marked-alt",
        text = "Total distance traveled",
        header = est,
        number = paste(lci, "—", uci))

    }) # end of renderUI // distInfo_total

    output$distInfo_err <- shiny::renderUI({
      req(vals$distErr)

      errorBlock(
        icon = "radiation",
        text = "Expected error",
        value = vals$distErr,
        min = vals$distErr_min,
        max = vals$distErr_max,
        rightBorder = FALSE)

    }) # end of renderUI // distInfo_err

    output$distInfo_total_new <- shiny::renderUI({
      req(vals$data2_ctsd)

      est <- ifelse(
        vals$dist_est_new <= 1,
        paste(
          scales::label_comma(
            accuracy = .1)(vals$dist_est_new), "m"),
        paste(
          scales::label_comma(
            accuracy = .1)("km" %#% vals$dist_est_new), "km"))

      lci <- scales::label_comma(
        accuracy = .1)("km" %#% vals$dist_lci_new)

      uci <- scales::label_comma(
        accuracy = .1)("km" %#% vals$dist_uci_new)

      parBlock(
        icon = "map-marked-alt",
        text = "Total distance traveled",
        header = est,
        number = paste(lci, "—", uci))

    }) # end of renderUI // distInfo_total_new

    output$distInfo_err_new <- shiny::renderUI({
      req(vals$distErr_new)

      errorBlock(
        icon = "radiation",
        text = "Expected error",
        value = vals$distErr_new,
        min = vals$distErr_min_new,
        max = vals$distErr_max_new,
        rightBorder = FALSE)

    }) # end of renderUI // distInfo_err_new

    # PLOTS -------------------------------------------------------------
    ## Rendering speed plot (estimate vs. time): ------------------------

    observe({
      req(vals$data_speed, vals$ctsd)

      tmpunits <- input$ctsdVar_group_units
      df0 <- vals$data_speed

      t_origin <- "1111-11-10 23:06:32"
      df0$timestamp <- as.POSIXct(df0$t, origin = t_origin)

      if(tmpunits == "minutes") {
        df0$t_new <- round.POSIXt(as.POSIXct(df0$timestamp),
                           units = "mins")
        df0 <- df0 %>%
          dplyr::mutate(t_new = as.POSIXct(
            t_new, format = "%Y-%m-%d %H:%M:%OS"))
      } else if(tmpunits == "hours") {
        df0$t_new <- round.POSIXt(as.POSIXct(df0$timestamp),
                           units = "hours")
        df0 <- df0 %>%
          dplyr::mutate(t_new = as.POSIXct(
            t_new, format = "%Y-%m-%d %H:%M:%OS"))
      } else if(tmpunits == "days") {
        df0$t_new <- round.POSIXt(as.POSIXct(df0$timestamp),
                           units = "days")
        df0 <- df0 %>%
          dplyr::mutate(t_new = as.POSIXct(
            t_new, format = "%Y-%m-%d %H:%M:%OS"))
      } else if(tmpunits == "weeks") {
        df0 <- df0 %>% dplyr::mutate(
          t_new = format(timestamp, "%U"))
      } else if(tmpunits == "months") {
        df0$t_new <- round.POSIXt(as.POSIXct(df0$timestamp),
                           units = "months")
        df0 <- df0 %>%
          dplyr::mutate(t_new = as.POSIXct(
            t_new, format = "%Y-%m-%d %H:%M:%OS"))
      }

      df0 <- df0 %>%
        dplyr::group_by(t_new) %>%
        dplyr::summarise(est = mean(est),
                         low = mean(low),
                         high = mean(high))

      yline <- "day" %#% vals$ctsd[2] %#% "kilometers"

      if(!is.null(vals$ctsd_new)) {
        yline_new <- "day" %#% vals$ctsd_new[2] %#% "kilometers"
      }

      label_x <- paste0("Time lag (in ", tmpunits, ")")
      output$ctsdPlot_main <- ggiraph::renderGirafe({

        p <- df0 %>%

          ggplot2::ggplot(
            ggplot2::aes(x = t_new, y = est, group = 1)) +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = low, ymax = high),
            fill = "grey70", alpha = .6) +
          ggplot2::geom_line(color = "black") +

          ggplot2::geom_hline(
            yintercept = yline, size = 2, col = hex_border) +

          { if(!is.null(vals$ctsd_new))
            ggplot2::geom_hline(
              yintercept = yline_new, size = 2, col = hex_caution)
          } +

          ggplot2::labs(
            x = label_x,
            y = "Speed estimate (meters/second)") +
          theme_movedesign()

        ggiraph::girafe(ggobj = p)

      }) # end of renderGirafe // ctsdPlot_main

    }) %>% # end of observe,
      bindEvent(input$ctsdVar_group_units)


    ## Rendering trajectory: --------------------------------------------

    output$ctsdPlot_path <- ggiraph::renderGirafe({
      req(vals$data1, vals$data_full)

      newdat <- vals$data1
      alldat <- vals$data_full

      xmin <- min(
        min(newdat$x) - diff(range(newdat$x)) * .1,
        min(alldat$x) - diff(range(alldat$x)) * .1)

      xmax <- max(
        max(newdat$x) + diff(range(newdat$x)) * .1,
        max(alldat$x) + diff(range(alldat$x)) * .1)

      ymin <- min(
        min(newdat$y) - diff(range(newdat$y)) * .1,
        min(alldat$y) - diff(range(alldat$y)) * .1)

      ymax <- max(
        max(newdat$y) + diff(range(newdat$y)) * .1,
        max(alldat$y) + diff(range(alldat$y)) * .1)

      datasets <- input$ctsdShow_datasets

      if("full" %in% datasets) {
        p1 <- ggplot2::geom_path(
          alldat, mapping = ggplot2::aes(
            x = x, y = y),
          col = "grey80", size = 1.4)
      }

      if("subset" %in% datasets) {
        p2 <- ggiraph::geom_path_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y),
          size = 0.8, col = hex_border)
        p2_points <- ggiraph::geom_point_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            tooltip = timestamp),
          size = 2.5, col = hex_border)
      }

      if("new" %in% datasets) {

        p3 <- ggiraph::geom_path_interactive(
          vals$data2_ctsd, mapping = ggplot2::aes(
            x = x, y = y),
          size = 0.8, col = hex_caution)
        p3_points <- ggiraph::geom_point_interactive(
          vals$data2_ctsd, mapping = ggplot2::aes(
            x = x, y = y,
            tooltip = timestamp),
          size = 2.5, col = hex_caution)
      }

      p <- ggplot2::ggplot() +

        { if("full" %in% datasets) p1 } +
        { if("subset" %in% datasets) p2 } +
        { if("subset" %in% datasets) p2_points } +

        { if("new" %in% datasets) p3 } +
        { if("new" %in% datasets) p3_points } +

        ggplot2::labs(
          x = "x coordinate",
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

    }) # end of renderGirafe // ctsdPlot_path

    # TABLES ------------------------------------------------------------
    # Save estimation outputs to data.frame:

    observe({
      req(vals$ctsd, vals$ctsd_truth, vals$ctsdErr)
      shinyjs::show(id = "ctsdBox_summary")

      ### Initial tracking regime: --------------------------------------

      originalrow <- data.frame(
        data = "Initial",
        tauv = NA,
        dti = NA,
        n = nrow(vals$data1),
        N = vals$Nspeed,
        speed = NA,
        speed_err = vals$ctsdErr,
        dist = NA,
        dist_err = vals$distErr)

      originalrow$tauv <-
        paste(scales::label_comma(
          accuracy = .1)(vals$tau_v0),
          abbreviate_time(vals$tau_v0_units))

      originalrow$dti <-
        paste(scales::label_comma(
          accuracy = .1)(vals$dti0),
          abbreviate_time(vals$dti0_units))

      tempunits <- vals$ctsd_units
      if(tempunits == "kilometers/day") {
        tempunits <- "km/day" }

      originalrow$speed <-
        paste(scales::label_comma(
          accuracy = .1)(vals$ctsd[2]),
          tempunits)

      originalrow$dist <-
        paste(scales::label_comma(
          accuracy = .1)("km" %#% vals$dist_est), "km")

      vals$df_speeds <<- rbind(vals$df_speeds, originalrow)

    }) %>% # end of observe, then:
      bindEvent(input$show_ctsdTable)

    observe({
      req(vals$data2_ctsd)

      ### New tracking regime: ------------------------------------------

      fitnames <- names(summary(vals$fit2)$DOF)
      N_new <- summary(vals$fit2)$DOF[grep('speed', fitnames)][[1]]

      newrow <- data.frame(
        data = "New",
        tauv = NA,
        dti = NA,
        n = nrow(vals$data2_ctsd),
        N = N_new,
        speed = NA,
        speed_err = vals$ctsdErr_new,
        dist = NA,
        dist_err = vals$distErr_new)

      newrow$tauv <-
        paste(scales::label_comma(
          accuracy = .1)(vals$tau_v0),
          abbreviate_time(vals$tau_v0_units))

      newrow$dti <-
        paste(scales::label_comma(
          accuracy = .1)(vals$dti2),
          abbreviate_time(vals$dti2_units))

      tempunits <- vals$ctsd_units_new
      if(tempunits == "kilometers/day") {
        tempunits <- "km/day" }

      newrow$speed <-
        paste(scales::label_comma(
          accuracy = .1)(vals$ctsd_new[2]),
          tempunits)

      newrow$dist <-
        paste(scales::label_comma(
          accuracy = .1)("km" %#% vals$dist_est_new), "km")

      df_speeds <- dplyr::bind_rows(vals$df_speeds, newrow)
      vals$df_speeds <- df_speeds

    }) %>%  # end of observe, then:
      bindEvent(vals$ctsd_new)

    ## Rendering output table: ------------------------------------------

    output$ctsdTable <- DT::renderDataTable({
      req(vals$df_speeds)

      columnNames <- list(
        data = "Type:",
        tauv = paste0("\u03C4","\u1D65"),
        dti = "Interval",
        n = "n",
        N = paste0("N", tags$sub("speed")),
        speed = "CTSD",
        speed_err = "Error",
        dist = "Distance",
        dist_err = "Error")

      DT::datatable(
        data = vals$df_speeds,
        colnames = as.vector(unlist(columnNames)),
        rownames = FALSE,
        escape = FALSE,
        options = list(
          paging = F, dom = "t",
          order = list(list(0, 'asc')),
          columnDefs = list(list(className = 'dt-center',
                                 targets = 0:8)))
      ) %>%
        sparkline::spk_add_deps() %>%
        DT::formatStyle(
          columns = c("n", "N"),
          fontWeight = "bold",
          color = DT::styleInterval(
            c(5, 30),
            c(hex_caution, '#f5b700', hex_main))
        ) %>%
        DT::formatCurrency(
          columns = c("n", "N"),
          currency = "",
          digits = 0
        ) %>%
        DT::formatStyle(
          columns = c("speed_err", "dist_err"),
          fontWeight = "bold",
          color = DT::styleInterval(
            c(-0.8, -0.2, 0.2, 0.8),
            c(hex_caution, '#f5b700',
              hex_border,
              '#f5b700', hex_caution))
        ) %>%
        DT::formatPercentage(
          columns = c("speed_err", "dist_err"),
          digits = 1
        )

    }) # end of renderDataTable // ctsdTable

    observe({
      vals$df_speeds <- NULL
    }) %>% # end of observe,
      bindEvent(input$ctsdTable_clear)

    # Additional information: -------------------------------------------

    output$time_ctsd <- renderText({
      req(vals$time_ctsd)

      paste("Initial tracking regime took approximately",
            round(vals$time_ctsd, 1), "minutes.")
    })

    output$time_ctsd_new <- renderText({
      req(vals$time_ctsd_new)

      paste("Newly set tracking regime took approximately",
            round(vals$time_ctsd_new, 1), "minutes.")
    })

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_ctsd_ui("tab_ctsd_1")

## To be copied in the server
# mod_tab_ctsd_server("tab_ctsd_1")
