#' tab_hrange UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats qt sd
#'
mod_tab_hrange_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(

      ## Introduction: ----------------------------------------------------

      div(class = div_column_main,

          shinydashboardPlus::box(

            title = span("Home range estimation:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "map-location-dot",
                                   height = "21px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "var(--sea-dark)"),
            id = ns("hr_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = TRUE, closable = FALSE,

            column(
              align = "center", width = 12,

              p(span("Home range", class = "cl-sea-d"),
                "is the area repeatedly used throughout an animal's",
                "lifetime for all its normal behaviors and activities,",
                "excluding occasional exploratory excursions."),

              p(style = "text-align: center;",
                "If home range estimation is is your goal,", br(),
                "then click the",
                icon("paper-plane", class = "cl-mdn"),
                HTML(paste0(span("Run estimation", class = "cl-mdn"))),
                "button."),

              splitLayout(
                cellWidths = c("38px", "1%", "200px"),
                cellArgs = list(style = 'align: center;'),

                shiny::actionButton(
                  inputId = ns("hrHelp_method"),
                  label = NULL,
                  width = "100%",
                  icon = icon("circle-question"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("run_hr"),
                  label = "Run estimation",
                  icon =  icon("paper-plane"),
                  width = "100%",
                  class = "btn-primary")
              ),
              br()

            ) # end of column (for text)
          ), # end of box // hr_intro

          uiOutput(ns("hrInput_show_all")),
          br()

      ), # end of div (top row)

      # [right column] ----------------------------------------------------

      div(class = div_column_left,

          ## Tracking regime: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Tracking regime", class = "ttl-box_solid"),
            id = ns("hrBox_regime"),
            status = "info",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("hrTabs_regime"),

              tabPanel(
                value = ns("hrPanel_regime"),
                title = icon("stopwatch", class = "cl-sea"),
                p(),
                fluidRow(
                  column(width = 12, uiOutput(ns("hrInfo_dur"))),
                  column(width = 12, uiOutput(ns("hrInfo_dti")))
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("hrPanel_regime_new"),
                title = icon("bolt", class = "cl-mdn"),
                p(),
                fluidRow(
                  column(width = 12, uiOutput(ns("hrInfo_dur_new"))),
                  column(width = 12, uiOutput(ns("hrInfo_dti_new")))
                ), # end of fluidRow

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12, align = "center",

              splitLayout(
                cellWidths = c("29%", "1%", "70%"),
                cellArgs = list(style = "align: center;"),

                shiny::actionButton(
                  inputId = ns("hrHelp_regime"),
                  label = NULL,
                  width = "100%",
                  icon = icon("circle-question"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("hr_adjRegime"),
                  label = "Modify",
                  icon = icon("rotate-right"),
                  class = "btn-info",
                  width = "100%")

              ) # end of splitLayout

            ) # end of column (footer)
          ), # end of box // hrBox_regime

          ## Sample sizes: ------------------------------------------------

          shinydashboardPlus::box(
            title = span("Sample sizes:", class = "ttl-box"),
            id = ns("hrBox_sizes"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("hrTabs_sizes"),

              tabPanel(
                value = ns("hrPanel_sizes"),
                title = icon("stopwatch", class = "cl-sea"),

                fluidRow(
                  column(width = 12, uiOutput(ns("hrBlock_n"))),
                  column(width = 12, uiOutput(ns("hrBlock_N1"))),
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("hrPanel_sizes_new"),
                title = icon("bolt", class = "cl-mdn"),

                fluidRow(
                  column(width = 12, uiOutput(ns("hrBlock_n_new"))),
                  column(width = 12, uiOutput(ns("hrBlock_N1_new"))),
                ) # end of fluidRow

              ) # end of panels (2 out of 2)
            ) # end of tabs
          ) # end of box // hrBox_sizes

      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(class = div_column_right,

          ## Home range plots: --------------------------------------------

          shinydashboardPlus::box(
            title = span("Home range estimates:", class = "ttl-box"),
            id = ns("hrBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,

            tabsetPanel(
              id = ns("hrTabs_viz"),

              tabPanel(
                value = ns("hrPanel_viz"),
                title = tagList(
                  icon("stopwatch", class = "cl-sea"),
                  span("Regime", class = "ttl-panel")
                ),

                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-9",
                    p(),
                    shinyWidgets::checkboxGroupButtons(
                      inputId = ns("hrShow_levels"),
                      label = "Show estimate levels:",
                      choices = c("95% low CI",
                                  "Estimate",
                                  "95% high CI"),
                      selected = "Estimate",
                      checkIcon = list(yes = icon("circle-check")),
                      justified = TRUE),

                    ggiraph::girafeOutput(
                      outputId = ns("hrPlot_initial"),
                      width = "100%", height = "100%")),

                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-3",
                    p(class = "fluid-padding"),
                    uiOutput(ns("hrInfo_est")),
                    uiOutput(ns("hrInfo_err")))

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("hrPanel_viz_new"),
                title = tagList(
                  icon("bolt", class = "cl-mdn"),
                  span("Modified regime") %>%
                    tagAppendAttributes(class = 'ttl-panel cl-mdn')
                ),

                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-9",
                    p(),
                    shinyWidgets::checkboxGroupButtons(
                      inputId = ns("hrhrShow_levels_sim"),
                      label = "Show estimate levels:",
                      choices = c("95% low CI",
                                  "Estimate",
                                  "95% high CI"),
                      selected = "Estimate",
                      checkIcon = list(yes = icon("circle-check")),
                      justified = TRUE),

                    ggiraph::girafeOutput(
                      outputId = ns("hrPlot_modified"),
                      width = "100%", height = "100%"),

                    column(
                      width = 12, align = "center",
                      span("Add the",
                           icon("stopwatch", class = "cl-sea"),
                           span("Data", class = "cl-sea"),
                           "locations to the plot above:"),

                      shinyWidgets::switchInput(
                        inputId = ns("hrShow_datasets"),
                        onLabel = "Yes",
                        offLabel = "No",
                        labelWidth = "25px"))
                    ),

                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-3",
                    p(class = "fluid-padding"),
                    uiOutput(ns("hrText_sims")),
                    uiOutput(ns("hrInfo_est_new")),
                    uiOutput(ns("hrInfo_err_new")))

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12, align = "center",

              splitLayout(
                cellWidths = c("29%", "1%", "70%"),
                cellArgs = list(style = "align: center;"),

                shiny::actionButton(
                  inputId = ns("hrHelp_bias"),
                  label = NULL,
                  width = "100%",
                  icon = icon("circle-question"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("show_hrTable"),
                  label = span("Add to",
                               span("table", class = "cl-sea")),
                  icon = icon("bookmark"),
                  width = "100%",
                  class = "btn-primary")

              )) # end of footer
          ) # end of box // hrBox_viz

      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          ## Table: -------------------------------------------------------

          shinydashboardPlus::box(
            title = span("Table:", class = "ttl-box"),
            id = ns("hrBox_summary"),
            width = NULL,
            solidHeader = FALSE,

            reactable::reactableOutput(ns("hrTable"))

          ), # end of box // hrBox_summary

          ## Additional information: --------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("hrBox_misc"),
            width = NULL,
            solidHeader = FALSE,

            verbatimTextOutput(outputId = ns("time_hr"))

          ) # end of box
      ) # end of column (bottom)

    ) # end of fluidRow
  ) # end of tagList
}

#' tab_hrange Server Functions
#'
#' @noRd
mod_tab_hrange_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals$hr <- reactiveValues()
    pal <- load_pal()

    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide secondary elements at start: ----------------------------------

    tmpnames <- c("regime",
                  "sizes",
                  "viz",
                  "areas",
                  "summary",
                  "misc")

    for(i in 1:length(tmpnames)) {
      shinyjs::hide(id = paste0("hrBox_", tmpnames[i]))
    }

    vec <- c("regime", "area", "sizes", "viz")
    for(i in 1:length(vec)) {
      tmp_id <- paste0("hrTabs_", vec[i])
      tmp_target <- paste0("hrPanel_", vec[i], "_new")
      hideTab(inputId = tmp_id, target = ns(tmp_target))
    }

    ## Quick select all boxes after adjusting tracking regime: ----------

    observe({

      tmp <- ifelse(input$hrInput_show == 1, "", "_new")
      tabs <- paste0("hrTabs_", vec)
      panels <- paste0("hrPanel_", vec)

      for(v in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[v]),
          selected = paste0("tab_hrange_1-", panels[v], tmp)) }

    }) %>% # end of observe.
      bindEvent(input$hrInput_show)

    # PROCESSING ----------------------------------------------------------
    ## Fitting movement model (if needed): --------------------------------

    observe({
      req(vals$active_tab == 'hr')

      if(is.null(vals$data1)) {

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
        if(is.null(vals$fit1)) {

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

          } # end of if(), !is.null(fit1)

          shinybusy::remove_modal_spinner()

        } # end of if(vals$needs_fit)
      } # end of if(is.null(vals$data1))

    }) # %>% # end of observe,
      # bindEvent(input$run_hr)

    observe({
      shinyjs::show(id = "hrBox_regime")
      shinyjs::show(id = "hrBox_sizes")
    }) %>% bindEvent(vals$fit1)

    # ADJUSTING TRACKING REGIME -------------------------------------------
    # Adjust sampling parameters necessary for simulation:

    observe({
      req(vals$dur0_dev, vals$dti0_dev)

      if(!is.null(vals$akde)) {

        # Sampling duration:

        dur <- round("days" %#% vals$dur0_dev %#% vals$dur0_units_dev, 0)
        tau_p0 <- round("days" %#% vals$tau_p0 %#% vals$tau_p0_units, 0)
        dur_choices <- c(
          10, dur, tau_p0, tau_p0 * 10,
          tau_p0 * 50, tau_p0 * 100, tau_p0 * 200,
          tau_p0 * 400, tau_p0 * 600, tau_p0 * 800,
          tau_p0 * 1000, tau_p0 * 2000
        ) %>% plyr::round_any(5, f = round) %>%
          unique() %>% sort()
        dur_choices <- dur_choices[dur_choices != 0]

        # Sampling interval:

        fixrate <- movedesign::gps_fixrate
        df_fixrate <- dplyr::arrange(fixrate, dplyr::desc(freq))
        value <- vals$dti0_dev %#% vals$dti0_units_dev
        index <- which.min(abs(df_fixrate$nu - value))
        dti_choices <- df_fixrate$nu_notes

        if(vals$data_type == "simulated") {
          tmprange <- NULL
        } else { tmprange <-
          paste(ifelse(vals$tau_p0_min == 0, "0",
                       scales::label_comma(
                         accuracy = .1)(vals$tau_p0_min)),
                "—", scales::label_comma(
                  accuracy = .1)(vals$tau_p0_max))
        }

        shiny::showModal(
          shiny::modalDialog(
            title = h4("Adjusting tracking regime:"),

            fluidRow(
              style = paste("margin-right: 25px;",
                            "margin-left: 25px;"),

              parBlock(
                header = span(
                  HTML(paste0("Position autocorrelation ",
                              "(\u03C4", tags$sub("p"), ")"))),
                value =
                  paste(scales::label_comma(
                    accuracy = .1)(vals$tau_p0),
                    vals$tau_p0_units),
                subtitle = tmprange),

              p("If home range estimation is your goal,",
                "we recommend that the",
                span("sampling duration", class = "cl-sea"),
                "is that least 10 times the",
                span(HTML(paste0("position autocorrelation ",
                                 "(\u03C4", tags$sub("p"), ")")),
                     class = "cl-sea"), "value (shown above).",
                "The", span("sampling interval", HTML("(\u0394t)"),
                            class = "cl-sea"),
                "is not directly related to the accuracy",
                "of home range estimation, so you can",
                "increase it if the total number of locations",
                "is a concern.",
                p(),
                span(class = "cl-blk",
                     "The recommended",
                     span("sampling duration", class = "cl-sea"),
                     "is already set below, so you can just click",
                     icon("bolt", class = "cl-dgr"),
                     span("Run simulation", class = "cl-dgr"),
                     "to proceed."),
                p()
              ),

              shinyWidgets::sliderTextInput(
                inputId = ns("hr_dur"),
                label = "Sampling duration (in days):",
                width = "100%",
                choices = dur_choices,
                selected = tau_p0 * 10 %>%
                  plyr::round_any(5, f = round),
                from_min = ifelse(dur > tau_p0 * 10,
                                  dur, tau_p0 * 10) %>%
                  plyr::round_any(5, f = round),
                from_max = ifelse(dur > tau_p0 * 800,
                                  dur, tau_p0 * 2000) %>%
                  plyr::round_any(5, f = round)
              ),

              shinyWidgets::sliderTextInput(
                inputId = ns("hr_dti"),
                label = "Sampling interval:",
                width = "100%",

                choices = dti_choices,
                selected = dti_choices[index],
                from_min = dti_choices[index],
                from_max = dti_choices[nrow(dti_choices)]),

              uiOutput(ns("hrText_sampling")),

              p(span("Proceed with caution!", class = "cl-dgr"),
                "Longer sampling durations + lower sampling",
                "intervals will add run time to simulation, model",
                "fitting, and estimation functions."),

            ), # end of fluidRow

            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                inputId = ns("run_hr_new"),
                label = "Run simulation",
                icon = icon("bolt"),
                class = "btn-danger")
            ),

            size = "m")) # end of modal

      } else {

        shinyalert::shinyalert(
          title = "Error",
          text = span(

            "First, estimate the home range",
            "based on the original dataset",
            "by clicking the",
            icon("paper-plane", class = "cl-mdn"),
            HTML(paste0(span("Run estimation", class = "cl-mdn"))),
            "button."),

          html = TRUE,
          size = "xs")

      } # end of ifelse statement
    }) %>% bindEvent(input$hr_adjRegime)

    output$hrText_sampling <- renderUI({
      req(vals$data1, input$hr_dur, input$hr_dti)

      fixrate <- movedesign::gps_fixrate
      tmp <- fixrate$nu[match(input$hr_dti,
                              fixrate$nu_notes)]

      n_new <- length(
        seq(from = 1,
            to = input$hr_dur %#% "days",
            by = tmp))

      splitLayout(

        parBlock(header = "Initial tracking regime:",
                 value = scales::label_comma(
                   accuracy = 1)(nrow(vals$data1)),
                 subtitle = span("locations", class = "cl-mdn")),

        parBlock(header =  "Modified tracking regime:",
                 value = span(scales::label_comma(
                   accuracy = 1)(n_new),
                   class = "cl-dgr"),
                 subtitle = span("locations", class = "cl-dgr"))

      ) # end of splitLayout
    }) # end of renderUI // hrText_sampling

    # HOME RANGE ESTIMATION ---------------------------------------------

    ## Estimating for initial tracking regime: --------------------------
    # Estimate home range after pressing the "run_hr" button:

    observe({

      # Check if data is available:
      if(!is.null(vals$data1)) {

        tmplist <- list("hrBox_areas",
                        "hrBox_viz",
                        "hrBox_misc")

        for(i in 1:length(tmplist)) {
          shinyjs::show(id = tmplist[i])
        }

        req(vals$fit1, vals$tmpid, vals$id)
        if(vals$tmpid != vals$id) {

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

          start <- Sys.time()

          msg_log(
            style = "warning",
            message = paste0(
              "Estimating ",
              msg_warning("home range"), "..."),
            detail = "This may take a while...")

          shinybusy::show_modal_spinner(
            spin = "fading-circle",
            color = "var(--sea)",
            text = span(
              span("Estimating", style = "color: #797979;"),
              HTML(paste0(span("home range", class = "cl-sea"),
                          span("...", style = "color: #797979;")))
            )
          )

          vals$akde <- ctmm::akde(data = vals$data1,
                                  CTMM = vals$fit1)

          vals$is_analyses <- TRUE
          vals$hrEst <- summary(vals$akde)$CI[2]
          vals$hrEst_min <- summary(vals$akde)$CI[1]
          vals$hrEst_max <- summary(vals$akde)$CI[3]

          truth <- -2 * log(0.05) * pi * (vals$sigma0 %#%
                                            vals$sigma0_units)

          tempnames <- rownames(summary(vals$akde)$CI)
          units <- extract_units(
            tempnames[grep('^area', tempnames)])
          vals$hr_units <- units

          err <- ((vals$hrEst %#% units) - truth) / truth

          vals$hrErr <- err
          vals$hrErr_min <- ((vals$hrEst_min %#% units) -
                               truth) / truth
          vals$hrErr_max <- ((vals$hrEst_max %#% units) -
                               truth) / truth

          vals$time_hr <- difftime(Sys.time(), start, units = "mins")

          msg_log(
            style = "success",
            message = paste0(
              "Estimation ", msg_success("completed"),
              msg_step(1, 3, style = "success")),
            detail = paste(
              "This step took approximately",
              round(difftime(Sys.time(), start,
                             units = 'mins'), 1),
              "minutes."))


          shinybusy::remove_modal_spinner()

        } # end of if() statement
      } else {

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

      } # end of !is.null(vals$data1)

    }) %>% # end of observe,
      bindEvent(input$run_hr)

    ## Estimating for modified tracking regime: ---------------------------

    observe({
      req(vals$data0, vals$fit0)

      # Show "conditional simulation" panels:
      for(i in 1:length(vec)) {
        tmp_id <- paste0("hrTabs_", vec[i])
        tmp_target <- paste0("hrPanel_", vec[i], "_new")
        showTab(inputId = tmp_id, target = ns(tmp_target))
      }

      tabs <- paste0("hrTabs_", vec)
      panels <- paste0("hrPanel_", vec)
      for(v in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[v]),
          selected = paste0("tab_hrange_1-",
                            panels[v], "_new")) }

      # Capture new sampling duration and interval:
      vals$hr$dti_unit <- sub('^.* ([[:alnum:]]+)$',
                             '\\1', input$hr_dti)

      fixrate <- movedesign::gps_fixrate
      tmp <- fixrate$nu[match(input$hr_dti, fixrate$nu_notes)]
      vals$hr$dti <- vals$hr$dti_unit %#% round(tmp, 0)
      vals$hr$dur <- input$hr_dur
      vals$hr$dur_unit <- "days"

      ### 1. Simulate new dataset:
      # Fill in the gaps of original dataset + new duration:

      vals$conditional <- TRUE
      removeModal()

      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = span(
          span("Estimating new", style = "color: #797979;"),
          HTML(paste0(span("home range", class = "cl-sea"),
                      span("...", style = "color: #797979;")))
        )
      )

      msg_log(
        style = "warning",
        message = paste0("Simulating ",
                         msg_warning("new tracking regime"), "."),
        detail = "This may take a while...")

      start <- Sys.time()
      dat <- ctmm::simulate(
        vals$data0, vals$fit0,
        t = seq(0, vals$hr$dur %#% vals$hr$dur_unit,
                by = vals$hr$dti %#% vals$hr$dti_unit))

      dat <- ctmm:::pseudonymize(dat)
      dat$index <- 1:nrow(dat)
      vals$hr$newdata <- dat

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
        ctmm::ctmm.guess(vals$hr$newdata, interactive = FALSE)
      }) %>% bindCache(vals$hr$dti,
                       vals$hr$dur)
      
      vals$guess_new <- guess1()

      if(vals$data_type == "simulated") {
        mod1 <- vals$ctmm_mod
      } else {
        mod1 <- prepare_mod(
          tau_p = vals$tau_p0, tau_p_units = vals$tau_p0_units,
          tau_v = vals$tau_v0, tau_v_units = vals$tau_v0_units,
          sigma = vals$sigma0, sigma_units = vals$sigma0_units)
      }

      newfit <- reactive({
        inputList <- list(list(vals$hr$newdata, mod1))
        fit <- par_ctmm.fit(inputList, parallel = TRUE)
        return(fit)
      }) %>% bindCache(vals$hr$dti,
                       vals$hr$dti_unit,
                       vals$hr$dur,
                       vals$hr$dur_unit)

      vals$hr$fit <- newfit()

      msg_log(
        style = "success",
        message = "Model fit completed.",
        detail = paste(
          "This step took approximately",
          round(difftime(Sys.time(), start_fit, units = 'mins'), 1),
          "minutes."))

      vec <- c("regime", "area", "sizes", "viz")
      for(i in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0("hrTabs_", vec[i]),
          selected = paste0("tab_hrange_1-hrPanel_",
                            vec[i], "_new"))
      }

      ### 3. Run the home range estimator (AKDE):

      nms <- names(summary(vals$hr$fit)$DOF)
      N <- summary(vals$hr$fit)$DOF[grep('area', nms)][[1]]
      vals$N1_new <- N

      if (N < 5) {

        shinyalert::shinyalert(
          title = "Low sample size!",
          text = span(
            "Effective sample size for area estimation",
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
            "Estimating ",
            msg_warning("home range"), "..."),
          detail = "This may take a while...")

        akde_new <- ctmm::akde(data = vals$hr$newdata,
                               CTMM = vals$hr$fit)

        vals$is_analyses <- TRUE

        vals$akde_new <- akde_new
        vals$hrEst_new <- summary(vals$akde_new)$CI[2]
        vals$hrEst_min_new <- summary(vals$akde_new)$CI[1]
        vals$hrEst_max_new <- summary(vals$akde_new)$CI[3]

        truth <- -2 * log(0.05) * pi * (vals$sigma0 %#%
                                          vals$sigma0_units)

        nms <- rownames(summary(vals$akde_new)$CI)
        units <- extract_units(nms[grep('^area', nms)])
        vals$hr_units_new <- units

        vals$hrErr_new <- ((vals$hrEst_new %#% units) -
                             truth) / truth
        vals$hrErr_min_new <- ((vals$hrEst_min_new %#% units) -
                                 truth) / truth
        vals$hrErr_max_new <- ((vals$hrEst_max_new %#% units) -
                                 truth) / truth

        # Show buttons to change panels:
        output$hrInput_show_all <- renderUI({
          shinyWidgets::radioGroupButtons(
            inputId = ns("hrInput_show"),
            label = NULL,
            choices = c("Show initial tracking regime" = 1,
                        "Show modified tracking regime" = 2),
            checkIcon = list(yes = icon("circle-check")),
            selected = 2,
            justified = TRUE)
        })

        vals$hr$time_new <- difftime(Sys.time(), start, units = 'mins')

        msg_log(
          style = "success",
          message = "Home range estimation completed.",
          detail = paste(
            "This step took approximately",
            round(vals$hr$time_new, 1), "minutes."))

      }

      shinybusy::remove_modal_spinner()

    }) %>% # end of observe,
      shiny::bindEvent(input$run_hr_new)

    # BLOCKS --------------------------------------------------------------
    ## Tracking regime: ---------------------------------------------------

    observe({
      req(vals$data1)

      sumdat <- summary(vals$data1)
      nms <- names(sumdat)

      output$hrInfo_dur <- shiny::renderUI({

        tmpunits <- nms[grep('sampling period', nms)] %>% extract_units()
        dur <- as.numeric(sumdat[grep('sampling period', nms)])
        out <- fix_unit(dur, tmpunits)

        parBlock(header = "Sampling duration",
                 value = paste(out$value, out$unit))

      }) # end of renderUI // hrInfo_dur

      output$hrInfo_dti <- shiny::renderUI({

        dti <- as.numeric(sumdat[grep('sampling interval', nms)])
        out <- fix_unit(dti, vals$dti0_units)

        parBlock(header = "Sampling interval",
                 value = paste(out$value, out$unit),
                 subtitle = "between fixes")

      }) # end of renderUI // hrInfo_dti
    }) # end of observe

    observe({
      req(vals$hr$dti, vals$hr$dur)

      output$hrText_sims <- renderUI({

        out_dti <- fix_unit(vals$hr$dti, vals$hr$dti_unit)
        txt_dti <- ifelse(out_dti$value == 1,
                          paste(out_dti$unit),
                          paste(out_dti$value, out_dti$unit))

        dur <- vals$hr$dur
        dur_mth <- "months" %#% vals$hr$dur %#% vals$hr$dur_unit

        if (dur == 1) {
          txt_dur <- wrap_none(
            "for ",  span("1 day", class = "cl-sea-d"), ".")
        } else if (dur == 365) {
          txt_dur <- wrap_none(
            "for ",  span("1 year", class = "cl-sea-d"), ".")
        } else {
          txt_dur <- HTML(paste(
            "for a duration of",
            span(round(dur, 1), "days", class = "cl-dgr"), "(or",
            wrap_none(round(dur_mth, 1), " months",
                      css = "cl-dgr", end = ").")))
        }

        p(br(),
          "This new tracking regime is equal to",
          "a new location every", span(txt_dti, class = "cl-sea-d"),
          txt_dur)

      }) # end of renderUI // hrText_sims

      output$hrInfo_dur_new <- shiny::renderUI({

        dur <- vals$dur0_units %#% vals$hr$dur %#% vals$hr$dur_unit
        dur_unit <- vals$dur0_units

        out <- fix_unit(dur, dur_unit)

        parBlock(
          header = "Sampling duration",
          value = span(out$value, out$unit, class = "cl-mdn"))

      }) # ender of renderUI // hrInfo_dur_new

      output$hrInfo_dti_new <- shiny::renderUI({

        out <- fix_unit(vals$hr$dti, vals$hr$dti_unit)

        parBlock(
          header = "Sampling interval",
          value = span(out$value, out$unit, class = "cl-mdn"),
          subtitle = span("between fixes", class = "cl-mdn"))

      }) # ender of renderUI // hrInfo_dti_new
    }) # end of observe

    ## Sample sizes: ----------------------------------------------------

    output$hrBlock_n <- shiny::renderUI({
      req(vals$data1)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data1),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)

    }) # end of renderUI // hrBlock_n (absolute sample size)

    output$hrBlock_N1 <- shiny::renderUI({
      req(vals$fit1, vals$N1)

      N <- vals$N1
      n <- nrow(vals$data1)

      value <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      vals$N1 <- N

      sampleBlock(
        number = value,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // hrBlock_N1 (effective)

    output$hrBlock_n_new <- shiny::renderUI({
      req(vals$hr$newdata)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$hr$newdata),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // hrBlock_n_new (absolute sample size)

    output$hrBlock_N1_new <- shiny::renderUI({
      req(vals$hr$newdata, vals$N1_new)

      n <- nrow(vals$hr$newdata)
      N <- vals$N1_new

      value <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      sampleBlock(
        number = value,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // hrBlock_N1_new (effective)

    ## Outputs: ---------------------------------------------------------
    ### Home range area: ------------------------------------------------

    output$hrInfo_est <- shiny::renderUI({
      req(vals$akde, vals$hrEst)

      est <- fix_unit(vals$hrEst, vals$hr_units, convert = TRUE)

      est_min <- fix_unit(est$unit %#% vals$hrEst_min %#% vals$hr_units,
                          est$unit, ui = TRUE)
      est_max <- fix_unit(est$unit %#% vals$hrEst_max %#% vals$hr_units,
                          est$unit, ui = TRUE)
      hr_unit <- est_max$unit

      parBlock(
        icon = "map-location-dot",
        header = "Estimate",
        value = span(HTML("&nbsp;", est$value, hr_unit),
                     class = "cl-mdn"),
        subtitle = span(paste(est_min$value, "—", est_max$value),
                        class = "cl-mdn"))


    }) # end of renderUI // hrInfo_est

    output$hrInfo_est_new <- shiny::renderUI({
      req(vals$akde_new, vals$hrEst_new)

      est <- fix_unit(vals$hrEst_new, vals$hr_units_new, convert = TRUE)

      est_min <- fix_unit(est$unit %#%
                            vals$hrEst_min_new %#% vals$hr_units_new,
                          est$unit, ui = TRUE)
      est_max <- fix_unit(est$unit %#%
                            vals$hrEst_max_new %#% vals$hr_units_new,
                          est$unit, ui = TRUE)
      hr_unit <- est_max$unit

      parBlock(
        icon = "map-location-dot",
        header = "Estimate",
        value = span(HTML("&nbsp;", est$value, hr_unit),
                     class = "cl-mdn"),
        subtitle = span(paste(est_min$value, "—", est_max$value),
                        class = "cl-mdn"))

    }) # end of renderUI // hrInfo_est_new

    ### Relative error: -------------------------------------------------

    output$hrInfo_err <- shiny::renderUI({
      req(vals$hrErr)

      errorBlock(
        icon = "radiation",
        text = "Expected error",
        value = vals$hrErr,
        min = vals$hrErr_min,
        max = vals$hrErr_max,
        rightBorder = FALSE)

    }) # end of renderUI // hrInfo_err

    output$hrInfo_err_new <- shiny::renderUI({
      req(vals$hrErr_new)

      errorBlock(
        icon = "radiation",
        text = "Expected error",
        value = vals$hrErr_new,
        min = vals$hrErr_min_new,
        max = vals$hrErr_max_new,
        rightBorder = FALSE)

    }) # end of renderUI // hrInfo_err_new

    # PLOTS -------------------------------------------------------------
    ## Rendering home range estimate plot (xy): -------------------------

    output$hrPlot_initial <- ggiraph::renderGirafe({
      req(vals$akde)

      ud <- plotting_hr(dat = vals$data1,
                        ud = vals$akde,
                        levels = input$hrShow_levels,
                        color = pal$sea, fill = pal$dgr)

      ggiraph::girafe(
        ggobj = ud,
        options = list(
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("fill:#dd4b39;",
                        "stroke:#eb5644;")))
      )

    }) # end of renderGirafe

    ## Rendering simulation plot (xy): --------------------------------

    output$hrPlot_modified <- ggiraph::renderGirafe({
      req(vals$hr$newdata, vals$akde_new)

      # Rendering home range estimate plot:

      ud_sim <- plotting_hr_new(
        data1 = vals$data1,
        data2 = vals$hr$newdata,
        ud = vals$akde_new,

        levels = input$hrhrShow_levels_sim,
        show = input$hrShow_datasets,

        color = pal$sea,
        sim_color = pal$mdn,
        fill = pal$dgr)

      ggiraph::girafe(
        ggobj = ud_sim,
        options = list(
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("fill:#dd4b39;",
                        "stroke:#eb5644;")))
      )

    }) # end of renderGirafe

    # TABLES --------------------------------------------------------------
    ## Initial tracking regime: -------------------------------------------

    hrRow <- reactive({

      out <- data.frame(
        data = "Initial",
        taup = NA,
        dur = NA,
        n = nrow(vals$data1),
        N1 = vals$N1,
        area = NA,
        area_err = vals$hrErr,
        area_err_min = vals$hrErr_min,
        area_err_max = vals$hrErr_max)

      out$taup <- paste(scales::label_comma(
        accuracy = .1)(vals$tau_p0),
        abbrv_unit(vals$tau_p0_units))

      out_dur <- fix_unit(vals$dur0_dev, vals$dur0_units_dev)
      out$dur <- paste(out_dur$value, abbrv_unit(out_dur$unit))

      area <- scales::label_comma(accuracy = .1)(vals$hrEst)
      out$area <- paste(area, abbrv_unit(vals$hr_units))

      return(out)

    }) %>%
      bindCache(vals$dur0_dev, vals$dur0_units_dev,
                vals$dti0_dev, vals$dti0_units_dev)

    observe({
      req(vals$data1, vals$N1, vals$hrErr)

      shinyjs::show(id = "hrBox_summary")

      vals$dt_hr <<- rbind(vals$dt_hr, hrRow())
      vals$dt_hr <- dplyr::distinct(vals$dt_hr)
      vals$report_hr_yn <- TRUE

    }) %>% # end of observe
      bindEvent(input$show_hrTable)

    ## Modified tracking regime: ------------------------------------------

    hrRow_new <- reactive({

      out <- data.frame(
        data = "Modified",
        taup = NA,
        dur = NA,
        n = nrow(vals$hr$newdata),
        N1 = vals$N1_new,
        area = NA,
        area_err = vals$hrErr_new,
        area_err_min = vals$hrErr_min_new,
        area_err_max = vals$hrErr_max_new)

      out$taup <-
        paste(scales::label_comma(
          accuracy = .1)(vals$tau_p0),
          abbrv_unit(vals$tau_p0_units))

      out_dur <- fix_unit(vals$hr$dur, vals$hr$dur_unit, convert = TRUE)
      out$dur <- paste(out_dur[1], abbrv_unit(out_dur[,2]))

      area <- scales::label_comma(accuracy = .1)(vals$hrEst_new)
      out$area <- paste(area, abbrv_unit(vals$hr_units_new))

      return(out)

    }) %>%
      bindCache(vals$hr$dur, vals$hr$dur_unit,
                vals$hr$dti, vals$hr$dti_unit)

    observe({
      req(vals$hr$fit, vals$hrEst_new)

      vals$dt_hr <<- rbind(vals$dt_hr, hrRow_new())
      vals$dt_hr <- dplyr::distinct(vals$dt_hr)
      vals$report_hr_yn <- TRUE

    }) %>% # end of observe
      bindEvent(vals$hr$fit)

    ## Rendering output table: --------------------------------------------

    output$hrTable <- reactable::renderReactable({
      req(vals$dt_hr)

      columnNames <- list(
        data = "Data:",
        taup = "\u03C4\u209A",
        dur = "Duration",
        n = "n",
        N1 = "N (area)",
        area = "Area",
        area_err = "Error",
        area_err_min = "Error (min)",
        area_err_max = "Error (max)")

      reactable::reactable(
        data = vals$dt_hr,
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
          taup = reactable::colDef(
            minWidth = 80, name = columnNames[["taup"]],
            style = list(fontWeight = "bold")),
          dur = reactable::colDef(
            minWidth = 80, name = columnNames[["dur"]],
            style = list(fontWeight = "bold")),
          n = reactable::colDef(
            name = columnNames[["n"]],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 0)),
          N1 = reactable::colDef(
            minWidth = 80, name = columnNames[["N1"]],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)),
          area = reactable::colDef(
            minWidth = 80, name = columnNames[["area"]]),

          area_err = reactable::colDef(
            minWidth = 80, name = columnNames[["area_err"]],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)),
          area_err_min = reactable::colDef(
            minWidth = 80, name = columnNames[["area_err_min"]],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1)),
          area_err_max = reactable::colDef(
            minWidth = 80, name = columnNames[["area_err_max"]],
            style = list(color = format_perc),
            format = reactable::colFormat(percent = TRUE,
                                          digits = 1))
        ))

    }) # end of renderReactable // hrTable

    # HELP TOUR & MODALS: -----------------------------------------------
    ## Help tour (tracking regime): -------------------------------------

    observe({

      element <- intro <- character(0)
      element <- c(element, "#Tour_main")

      intro <- c(
        intro,
        HTML(paste(
          "Click the ",
          icon("wrench", class = "cl-sea"),
          span("Modify", class = "cl-sea"), "button",
          "to adjust the tracking regime."))
      )

      tour <- data.frame(element = element,
                         intro = intro,
                         stringsAsFactors = FALSE)

      rintrojs::introjs(
        session = session,
        options = list(
          steps = tour,
          nextLabel = "Next",
          prevLabel = "Previous",
          showStepNumbers = F,
          showButtons = T,
          showBullets = T
        ),
        events = list(onbeforechange =
                        rintrojs::readCallback('switchTabs')))

    }) %>% # observe event, bound to:
      bindEvent(input$hrHelp_regime)

    ## Help modal (biases): ---------------------------------------------

    observe({

      shiny::showModal(
        shiny::modalDialog(
          title = h3("Home range:"),

          p("As animal movement",
            "is inherently", span("autocorrelated", class = "cl-sea-d"),
            "(locations are similar as a function of space and",
            " distance), the",
            span("Autocorrelated Kernel Density Estimators (AKDEs)",
                 class = "cl-sea"),
            "are the most appropriate method for",
            span("home range", class = "cl-sea-d"), "estimation."),

          footer = modalButton("Dismiss"),
          size = "m")) # end of modal

    }) %>% # observe event, bound to:
      bindEvent(input$hrHelp_method)

    observe({

      shiny::showModal(
        shiny::modalDialog(
          title = h3("Explaining biases:"),

          withMathJax(
            paste0("$$\\small{\\text{Relative error (%)}",
                   " = \\frac{\\text{estimate}-\\text{truth}}",
                   "{\\text{truth}}\\times 100}$$")
          ),

          p(class = "cl-mdn",
            style = "text-align: center;",
            "How much uncertainty is associated",
            "with an estimate?"),

          p("The", span("relative error (%)", class = "cl-dgr"),
            "of an", span("home range estimate", class = "cl-sea-d"),
            "decreases as",
            span("sampling duration", class = "cl-sea"),
            "increases, and is ultimately dependent on the",
            span("position autocorrelation",
                 wrap_none("(\u03C4", tags$sub("p"), ")"),
                 "timescale.", class = "cl-sea-d"),
          ),

          footer = modalButton("Dismiss"),
          size = "m")) # end of modal

    }) %>% # observe event, bound to:
      bindEvent(input$hrHelp_bias)

    # Additional information: -------------------------------------------

    output$time_hr <- renderText({
      req(vals$time_hr)

      time_total <- vals$time_hr

      if (round(time_total, 1) < 1) {
        tmptxt <- "less than one minute."
      } else {
        tmptxt <- paste("approximately",
                        round(time_total, 1), "minutes.") }

      out <- paste("Home range estimation took", tmptxt)
      return(out)

    }) # end of renderText // time_hr



    observe({
      # Save information for report if table is not requested:

      req(vals$is_analyses,
          vals$active_tab == 'hr')

      req(is.null(vals$dt_hr))
      vals$report_regs_yn <- FALSE

      if (!is.null(vals$hrErr_new)) {
        req(vals$hrErr_new)
        vals$report_regs_yn <- TRUE
        vals$dt_hr <- hrRow_new()
      } else {
        req(vals$hrErr)
        vals$report_regs_yn <- TRUE
        vals$dt_hr <- hrRow()
      }

    }) %>% bindEvent(list(input$run_hr, input$run_hr_new))


  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_hrange_ui("tab_hrange_1")

## To be copied in the server
# mod_tab_hrange_server("tab_hrange_1")
