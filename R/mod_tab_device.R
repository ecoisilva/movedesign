#' tab_device UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_device_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(

      # Introduction: -----------------------------------------------------

      shinydashboardPlus::box(

        title = span("Set tracking regime:", class = "ttl-tab"),
        icon = fontawesome::fa(name = "stopwatch",
                               height = "21px",
                               margin_left = "14px",
                               margin_right = "8px",
                               fill = "var(--sea-dark)"),

        id = ns("reg_intro"),
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = TRUE, closable = FALSE,

        column(
          align = "center", width = 12,

          p("Appropriate study design is dependent on",
            "the type of", span("tracking device", class = "cl-sea-d"),
            "(and, ultimately, its",
            HTML(paste0(span("battery life", class = "cl-sea"),
                        ")")), "and on two sampling parameters:",
            span("study duration", class = "cl-dgr"),
            "(how long to track each individual for), and",
            span("sampling interval", class = "cl-dgr"),
            "(time between which new locations are collected)."
          ),

          p("Which type are you evaluating?") %>%
            tagAppendAttributes(class = 'label_center'),

          shiny::selectizeInput(
            inputId = ns("device_type"),
            width = "260px",
            label = NULL,
            choices = c("GPS/Satellite logger" = 1,
                        "VHF transmitter" = 2),
            options = list(
              placeholder = 'Select an option here',
              onInitialize = I('function() { this.setValue(""); }'))),

          shinyWidgets::checkboxGroupButtons(
            inputId = ns("which_limitations"),
            label = p("What limitations do you want to consider?") %>%
                        tagAppendAttributes(class = 'label_center no-bottom'),
            choices = c("Storage limit" = "limit",
                        "Fail rate" = "loss",
                        "Location error" = "error"),
            selected = character(0),
            checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                             no = icon("remove", lib = "glyphicon"))),
          br()

        ) # end of column (text)
      ), # end of box // device_intro

      # [left column] -----------------------------------------------------

      div(class = div_column_left,

          ## SETTINGS -----------------------------------------------------
          ### Device settings: --------------------------------------------

          shinydashboardPlus::box(
            title = span("Device settings", class = "ttl-box_solid"),
            id = ns("regBox_gps_device"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            column(
              width = 12, align = "left",

              fluidRow(
                p(HTML("&nbsp;"), "GPS battery life:") %>%
                  tagAppendAttributes(class = 'label_split'),

                splitLayout(
                  cellWidths = c("40%", "60%"),

                  shiny::numericInput(
                    inputId = ns("gps_dur"),
                    label = NULL,
                    value = 4),

                  shiny::selectInput(
                    inputId = ns("gps_dur_units"),
                    label = NULL,
                    choices = c("Day(s)" = "days",
                                "Month(s)" = "months",
                                "Year(s)" = "years"),
                    selected = "months")

                ), # end of splitLayout

                shinyWidgets::pickerInput(
                  inputId = ns("gps_dti_max"),
                  label = "GPS fix rate (max):",
                  choices = NULL) %>%
                  help_tip(
                    text = paste("Maximum sampling interval",
                                 "(time between two fixes)",
                                 "available at the duration above."),
                    placement = "bottom"),

                shinyWidgets::pickerInput(
                  inputId = ns("gps_dti_min"),
                  label = "GPS fix rate (min):",
                  choices = NULL) %>%
                  help_tip(
                    text = paste("Minimum sampling interval",
                                 "(time between two fixes)",
                                 "available at the duration above."),
                    placement = "bottom"),


                # p("Slower decay") %>%
                #   tagAppendAttributes(class = 'new_irs'),

                div(id = ns("gps_decay"),
                    p(HTML("&nbsp;"), "Choose decay rate:",
                      class = "txt-label",
                      style = "margin: 8px 0 -16px -7px;"),
                    shinyWidgets::sliderTextInput(
                      inputId = ns("gps_k"),
                      label = NULL,
                      choices = seq(from = 1, to = 30),
                      select = 30) %>%
                      tagAppendAttributes(class = 'decayinput')
                ),

                shinyWidgets::autonumericInput(
                  inputId = ns("gps_maxlocs"),
                  label = span("Device storage limit:",
                               style = "align: left !important;"),
                  currencySymbol = " locations",
                  currencySymbolPlacement = "s",
                  decimalPlaces = 0,
                  minimumValue = 0,
                  value = 30000,
                  wheelStep = 1000)
              ) # end of fluidRow
            ), # end of column

            footer = div(style = "text-align: right;",

                         shinyWidgets::prettyToggle(
                           inputId = ns("eval_tradeoffs"),
                           label_on =
                             span("Select from",
                                  span("plot", class = "cl-sea")),
                           label_off = "Set regime manually",
                           value = TRUE))

          ), # end of box // regBox_gps_device

          shinydashboardPlus::box(
            title = span("Device settings", class = "ttl-box_solid"),
            id = ns("regBox_vhf_device"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            column(
              width = 12, align = "left",

              fluidRow(
                p(HTML("&nbsp;"), "VHF battery life:") %>%
                  tagAppendAttributes(class = 'label_split'),

                splitLayout(
                  cellWidths = c("40%", "60%"),

                  shiny::numericInput(
                    inputId = ns("vhf_dur"),
                    label = NULL,
                    min = 1, value = 12),

                  shiny::selectInput(
                    inputId = ns("vhf_dur_units"),
                    label = NULL,
                    choices = c("Days" = "days",
                                "Months" = "months",
                                "Weeks" = "weeks",
                                "Years" = "years"),
                    selected = "months")

                ), # end of splitLayout

                shinyWidgets::sliderTextInput(
                  inputId = ns("vhf_ppm"),
                  label = "Pulses per minute (ppm):",
                  choices = c(15, 20, 25, 30, 35, 40, 55, 120),
                  selected = 40,
                  grid = FALSE),

              ) # end of fluidRow
            ), # end of column

            footer = shiny::actionButton(
                inputId = ns("add_vhf_point"),
                label = span("Add to",
                             span("plot", class = "cl-sea")),
                icon = icon("bookmark"),
                width = "100%")

          ), # end of box // regBox_vhf_device

          ### Limitations: ------------------------------------------------

          shinydashboardPlus::box(
            id = ns("regBox_loss"),
            width = NULL,
            headerBorder = FALSE,

            splitLayout(
              cellWidths = c("92%", "15px"),

              p(HTML("&nbsp;"),
                "Simulate % data loss:") %>%
                tagAppendAttributes(class = 'label_split'),

              actionButton(
                inputId = ns("regHelp_loss"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_loss_device")
            ),

            shinyWidgets::sliderTextInput(
              inputId = ns("deviceInput_loss"),
              label = NULL,
              choices = c(0, 5, seq(10, 100, by = 10)),
              from_min = 0, from_max = 90, selected = 0,
              grid = FALSE,
              post = "%",
              width = "100%"),
            br(),
            uiOutput(ns("regBlock_loss"))

          ), # end of box // regBox_loss

          shinydashboardPlus::box(
            id = ns("regBox_error"),
            width = NULL,
            headerBorder = FALSE,

            splitLayout(
              cellWidths = c("92%", "15px"),

              p(HTML("&nbsp;"),
                "Location error:") %>%
                tagAppendAttributes(class = 'label_split'),

              actionButton(
                inputId = ns("regHelp_error"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_error_device")
            ),

            shinyWidgets::autonumericInput(
              inputId = ns("deviceInput_error"),
              label = NULL,
              currencySymbol = " meter(s)",
              currencySymbolPlacement = "s",
              decimalPlaces = 0,
              minimumValue = 1,
              value = 10,
              wheelStep = 1)

          ), # end of box // regBox_error

          ### Other settings: ---------------------------------------------

          shinydashboardPlus::box(
            id = ns("regBox_type"),
            width = NULL,
            headerBorder = FALSE,

            shinyWidgets::pickerInput(
              ns("est_type"),
              label = "Sample sizes are:",
              choices = c("Approximated" = 1,
                          "From model fit" = 2),
              choicesOpt = list(
                subtext = c("(faster, less precise)",
                            "(slower, more precise)")),
              selected = 2),

            uiOutput(ns("regText_sizes"))

          ), # end of box // regBox_submit

          # Sample sizes: -------------------------------------------------

          shinydashboardPlus::box(
            title = span("Sample sizes:", class = "ttl-box"),
            id = ns("regBox_sizes"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE, closable = FALSE,

            fluidRow(
              column(width = 12, uiOutput(ns("regBlock_n"))),
              column(width = 12, uiOutput(ns("regBlock_Narea")) %>%
                       add_spinner(height = "60px", type = 7)
              ),
              column(width = 12, uiOutput(ns("regBlock_Nspeed")) %>%
                       add_spinner(height = "60px", type = 7)
              ),
            ), # end of fluidRow

            br(),
            footer = shiny::actionButton(
              inputId = ns("regButton_save"),
              label = span("Add to",
                           span("table", class = "cl-sea")),
              icon = icon("bookmark"),
              width = "100%")

          ) # end of box // regBox_sizes

      ), # end of column (right)

      # [center column] ---------------------------------------------------

      div(class = div_column_right,

          # Display species & device parameters: --------------------------

          div(class = "tabBox_noheaders",
              shinydashboardPlus::box(
                id = ns("regBox_pars"),
                width = NULL,
                headerBorder = FALSE,

                shinydashboard::tabBox(
                  id = ns("regTabs_pars"),
                  width = NULL,

                  tabPanel(title = "Species",
                           value = ns("regPanel_species"),
                           icon = icon("paw"),

                           splitLayout(
                             uiOutput(ns("regBlock_tau_p")),
                             uiOutput(ns("regBlock_tau_v"))
                           )

                  ), # end of panel (1 out of 2)

                  tabPanel(title = "Device",
                           value = ns("regPanel_device"),
                           icon = icon("location-dot"),

                           uiOutput(ns("regUI_regime"))

                  ) # end of panel (2 out of 2)

                ) # end of tabBox
              ) # end of box // regBox_pars
          ), # end of div

          # Specify tracking regime parameters: ---------------------------

          div(class = "col-lg-6 no-padding-left",

              shinydashboardPlus::box(
                title = span("Sampling parameters",
                             class = "ttl-box_solid"),
                id = ns("regBox_sampling"),
                status = "primary",
                width = NULL,
                solidHeader = TRUE,
                collapsible = FALSE,

                uiOutput(ns("regUI_sampling")),

                footer = uiOutput(ns("regUI_sampling_footer"))

              )), # end of box // regBox_gpsViz

          div(class = "col-lg-6 no-padding-right",

              shinydashboardPlus::box(
                title = span("Visualizing new simulated data:",
                             class = "ttl-box"),
                id = ns("regBox_sims"),
                width = NULL,
                solidHeader = FALSE,
                collapsible = TRUE,

                tabsetPanel(
                  id = ns("regTabs_sims"),

                  tabPanel(
                    value = ns("regPanel_id"),
                    title = tagList(
                      icon("location-dot", class = "cl-sea"),
                      span("Data", class = "ttl-panel")),

                    br(),

                    ggiraph::girafeOutput(
                      outputId = ns("regPlot_id"),
                      width = "95%", height = "100%"),

                    column(
                      width = 12, align = "center",
                      uiOutput(ns("simsInput_subset"))
                    )

                  ), # end of panels (1 out of 2)

                  tabPanel(
                    value = ns("newsimsPanel_svf"),
                    title = tagList(
                      icon("chart-line", class = "cl-sea"),
                      span("Variogram", class = "ttl-panel")
                    ),
                    br(),

                    ggiraph::girafeOutput(
                      outputId = ns("regPlot_svf"),
                      width = "95%", height = "100%"),

                    column(
                      width = 12, align = "center",
                      shiny::sliderInput(
                        ns("regVar_timeframe"),
                        label = span(paste("Proportion of",
                                           "variogram plotted (in %):")) %>%
                          tagAppendAttributes(class = 'label_split'),
                        min = 0, max = 100, value = 65, step = 5,
                        width = "85%")
                    ) # end of column

                  ) # end of panels (3 out of 3)
                ) # end of tabs

              )), # end of box // regBox_sims

          # Table: --------------------------------------------------------

          div(class = "col-lg-12 no-padding",
              shinydashboardPlus::box(
                title = span("Summary table:", class = "ttl-box"),
                id = ns("regBox_summary"),
                width = NULL,
                solidHeader = FALSE,

                reactable::reactableOutput(ns("regTable")),
                br(),
                div(style = "display:inline-block; float:right",
                    shiny::actionButton(
                      inputId = ns("regTable_clear"),
                      label = "Clear table",
                      icon =  icon("trash"),
                      width = "110px")), br()

              )) # end of box // regBox_summary

      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          # Additional information: ---------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("regBox_misc"),
            width = NULL, solidHeader = FALSE,

            verbatimTextOutput(ns("console_device"))

          ) # end of box // regBox_misc
      ) # end of column (bottom)

    ), # end of fluidRow

    # MODALS: -------------------------------------------------------------

    create_modal(var = "loss",  id = "device"),
    create_modal(var = "error",  id = "device"),
    NULL

  ) # end of tagList
}

#' tab_device Server Functions
#'
#' @noRd
mod_tab_device_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals$reg <- reactiveValues()
    pal <- load_pal()

    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at start: --------------------------------------------

    boxnames <- c("gps_device",
                  "vhf_device",
                  "type",
                  "loss",
                  "sampling",
                  "sims",
                  "sizes",
                  "summary",
                  "misc")

    for(i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("regBox_", boxnames[i]))
    }
    shinyjs::hide(id = "which_limitations")

    ## Reveal boxes: ------------------------------------------------------

    observe({ ### Reveal species & device parameters box:
      req(vals$active_tab == 'regime')

      if (!is.null(vals$tau_p0) && !is.null(vals$tau_v0)) {
        shinyjs::show(id = "regBox_pars")
      } else { shinyjs::hide(id = "regBox_pars") }

    })

    observe({ ### Reveal sample sizes box:

      if(is.null(vals$is_reg_valid)) {
        shinyjs::hide(id = "regBox_sizes")
      } else {
        req(vals$is_reg_valid)
        shinyjs::show(id = "regBox_sizes") }
    })

    observe({ ## Reveal summary box:
      if (!is.null(vals$dt_regs)) {
        shinyjs::show(id = "regBox_summary")
      } else {
        shinyjs::hide(id = "regBox_summary")
      }
    }) %>% # end of observe,
      bindEvent(vals$dt_regs)

    ## Update device settings: --------------------------------------------

    observe({
      req(input$device_type)

      shinyjs::show(id = "regBox_sampling")
      shinyjs::show(id = "which_limitations")

      vals$device_type <- NULL
      if (input$device_type == 1) {
        vals$device_type <- "GPS"
        shinyjs::show(id = "regBox_gps_device")
        shinyjs::hide(id = "regBox_vhf_device")
      }

      if (input$device_type == 2) {
        vals$device_type <- "VHF"
        shinyjs::show(id = "regBox_vhf_device")
        shinyjs::hide(id = "regBox_gps_device")
      }

    }) %>% # end of observe,
      bindEvent(input$device_type)

    ## Update device limitation UI elements: ------------------------------

    observe({

      options_limit <- NULL
      if (input$device_type == 1) {
        options_limit <- c("Storage limit" = "limit",
                           "Fail rate" = "loss",
                           "Location error" = "error")
      }
      if (input$device_type == 2) {
        options_limit <- c("Data loss" = "loss",
                           "Location error" = "error")
      }

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "which_limitations",
        choices = options_limit,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                         no = icon("remove", lib = "glyphicon")))

    }) %>% bindEvent(input$device_type)

    observe({
      vals$which_limitations <- input$which_limitations

      if("loss" %in% input$which_limitations) {
        shinyjs::show(id = "regBox_loss")
      } else { shinyjs::hide(id = "regBox_loss") }

      if("error" %in% input$which_limitations) {
        shinyjs::show(id = "regBox_error")
      } else { shinyjs::hide(id = "regBox_error") }

      vals$is_limit <- FALSE
      if ("limit" %in% input$which_limitations) {
        vals$is_limit <- TRUE
        shinyjs::show(id = "gps_maxlocs")
      } else {
        shinyjs::hide(id = "gps_maxlocs")
      }

    }) %>% bindEvent(input$which_limitations)

    observe({
      if (is.null(input$which_limitations)) {
        shinyjs::hide(id = "regBox_loss")
        shinyjs::hide(id = "gps_maxlocs")
        shinyjs::hide(id = "regBox_error")
      }
    })

    ## Render validate buttons: ----------------------------------------

    output$regButton_gps <- renderUI({

      if (vals$is_reg_valid) {
        shiny::actionButton(
          inputId = ns("validate_gps"),
          icon =  icon("circle-check"),
          label = "Validated!",
          width = "120px",
          class = "btn-info")
      } else {
        shiny::actionButton(
          inputId = ns("validate_gps"),
          icon =  icon("wand-magic-sparkles"),
          label = "Validate",
          width = "120px",
          class = "btn-danger")
      }

    }) # end of renderUI // regButton_gps

    output$regButton_vhf <- renderUI({

      if (vals$is_reg_valid) {
        shiny::actionButton(
          inputId = ns("validate_vhf"),
          icon =  icon("circle-check"),
          label = "Validated!",
          width = "120px",
          class = "btn-info")
      } else {
        shiny::actionButton(
          inputId = ns("validate_vhf"),
          icon =  icon("wand-magic-sparkles"),
          label = "Validate",
          width = "120px",
          class = "btn-danger")
      }

    }) # end of renderUI // regButton_vhf

    ## Render regime text: ------------------------------------------------

    output$regText_gps <- renderUI({
      req(vals$reg$dur, vals$reg$dti)

      out_dur <- fix_unit(vals$reg$dur, vals$reg$dur_unit)
      dur <- out_dur[1]
      dur_units <- out_dur[2]

      dur_day <- "days" %#% vals$reg$dur %#% vals$reg$dur_unit
      dur_mth <- "months" %#% vals$reg$dur %#% vals$reg$dur_unit

      if (dur_day == 1) {
        text_dur <- span(
          HTML(paste0(span("1 day", class = "cl-sea"), ".")))

      } else {
        if (dur_mth == 1) { text_month <- "month"
        } else { text_month <- "months" }
        text_dur <- span(
          span(round(dur_mth, 1), text_month, class = "cl-sea"),
          "(\u2248", HTML(paste0(
            span(paste(round(dur_day, 1), "days"), class = "cl-sea"),
            ").")))
      }

      out_dti <- fix_unit(vals$reg$dti, vals$reg$dti_unit)
      dti <- out_dti[1]
      dti_units <- out_dti[2]

      tmp <- 1/("day" %#% vals$reg$dti %#% vals$reg$dti_unit)
      frq <- round(tmp, 1)
      frq_units <- "locations per day"

      if (frq > 24) {
        frq <- 1/("hours" %#% vals$reg$dti %#% vals$reg$dti_unit)
        frq_units <- "locations per hour"
      } else if (frq < 1) {
        frq <- 1/("month" %#% vals$reg$dti %#% vals$reg$dti_unit)
        frq_units <- "locations per month"
      }

      frq <- round(frq, 1)

      out <- p(style = "max-width: 700px;",

               "This tracking regime is equal to a new location every",
               span(dti, dti_units, class = "cl-sea"),
               "(\u2248", HTML(paste0(span(paste(frq, frq_units),
                                           class = "cl-sea"), ")")),
               "for a duration of", text_dur)

      return(out)

    }) # end of renderUI // regText_gps

    output$regText_vhf <- renderUI({
      req(input$vhf_dur,
          input$vhf_dti)

      validate(need(input$vhf_dur != '', "Requires a value."),
               need(input$vhf_dti != '', "Requires a value."))

      out_dti <- fix_unit(input$vhf_dti, input$vhf_dti_units)
      dti <- input$vhf_dti
      dti_units <- input$vhf_dti_units

      out_dur <- fix_unit(input$vhf_dur, input$vhf_dur_units)
      dur <- out_dur[1]
      dur_units <- out_dur[2]

      dur_day <- "days" %#% input$vhf_dur %#% input$vhf_dur_units
      dur_mth <- "months" %#% input$vhf_dur %#% input$vhf_dur_units

      if (dur_day == 1) {
        text_dur <- span(
          HTML(paste0(span("1 day", class = "cl-sea"), ".")))
      } else {
        text_dur <- span(
          span(round(dur_mth, 1), "months", class = "cl-sea"),
          "(\u2248", HTML(paste0(
            span(paste(round(dur_day, 1), "days"), class = "cl-sea"),
            ").")))
      }

      tmp <- 1/("day" %#% input$vhf_dti %#% input$vhf_dti_units)
      frq <- round(tmp, 1)
      frq_units <- "locations per day"

      if (frq > 24) {
        frq <- 1/("hours" %#% input$vhf_dti %#% input$vhf_dti_units)
        frq_units <- "locations per hour"
      } else if (frq < 1) {
        frq <- 1/("month" %#% input$vhf_dti %#% input$vhf_dti_units)
        frq_units <- "locations per month"
      }

      frq <- round(frq, 1)

      p(style = paste("max-width: 480px;"),

        "This tracking regime is equal to a new location every",
        span(dti, dti_units, class = "cl-sea"),
        "(\u2248", HTML(paste0(span(paste(frq, frq_units),
                                    class = "cl-sea"), ")")),
        "for a duration of", text_dur)

    }) # end of renderUI // regText_vhf

    ## Update GPS inputs: -------------------------------------------------

    ### Reveal correct inputs:

    observe({
      if (input$eval_tradeoffs) {
        shinyjs::show("gps_dti_max")
        shinyjs::show("gps_dti_min")
        shinyjs::show(id = "gps_decay")
      } else {
        shinyjs::hide("gps_dti_max")
        shinyjs::hide("gps_dti_min")
        shinyjs::hide(id = "gps_decay")
      }

    }) %>% bindEvent(input$eval_tradeoffs)

    ### Update max/min GPS fix rate inputs:

    observe({
      req(vals$active_tab == 'regime',
          input$device_type == 1)

      # utils::data(gps_fixrate, package = "movedesign")

      fixrate <- movedesign::gps_fixrate
      maxrate_choices <- fixrate %>%
        dplyr::filter(choices == "Y") %>%
        dplyr::pull(nu_notes)

      shinyWidgets::updatePickerInput(
        session,
        inputId = "gps_dti_max",
        choices = maxrate_choices,
        selected = "1 fix every 8 hours")
    })

    observe({

      fixrate <- movedesign::gps_fixrate
      dti_max <- fixrate$nu[match(input$gps_dti_max,
                                  fixrate$nu_notes)]

      minrate_choices <- fixrate %>%
        dplyr::filter(nu < dti_max) %>%
        dplyr::pull(nu_notes)

      shinyWidgets::updatePickerInput(
        session,
        inputId = "gps_dti_min",
        choices = minrate_choices,
        selected = "1 fix every 10 seconds")

    }) %>% bindEvent(input$gps_dti_max)

    ## Select fix rate inputs: ------------------------------------------

    output$gpsSelect_fix <- renderUI({

      splitLayout(
        cellWidths = c("75px", "180px"),
        cellArgs = list(style = 'align: center;'),

        shiny::numericInput(
          ns("gps_dti"),
          label = NULL,
          min = 1, value = 1),

        shiny::selectInput(
          ns("gps_dti_units"),
          label = NULL,
          choices = c("Day(s)" = "days",
                      "Hour(s)" = "hours",
                      "Minute(s)" = "minutes",
                      "Second(s)" = "seconds"),
          selected = "hours")

      ) # end of splitLayout
    }) # end of renderUI // gpsSelect_fix

    output$vhfSelect_fix <- renderUI({

      splitLayout(
        cellWidths = c("85px", "170px"),
        cellArgs = list(style = 'align: center;'),

        shiny::numericInput(
          ns("vhf_dti"),
          label = NULL,
          min = 1, value = 8),

        shiny::selectInput(
          ns("vhf_dti_units"),
          label = NULL,
          choices = c("Day(s)" = "days",
                      "Hour(s)" = "hours",
                      "Minute(s)" = "minutes"),
          selected = "hours")

      ) # end of splitLayout
    }) # end of renderUI // vhfSelect_fix

    ## Convert values/units: ----------------------------------------------

    observe({
      req(vals$gps_dur_units)
      req(input$gps_dur_units != vals$gps_dur_units)
      validate(need(input$gps_dur != '' && input$gps_dur > 0,
                    "Requires a value."))

      new_dur <- sigdigits(input$gps_dur_units %#%
                             input$gps_dur %#% vals$gps_dur_units, 3)

      updateNumericInput(
        session,
        inputId = "gps_dur",
        label = NULL,
        min = 1,
        value = new_dur)

    }) %>% bindEvent(input$gps_dur_units)

    observe({
      req(vals$gps_dti_units)
      req(input$gps_dti_units != vals$gps_dti_units)
      validate(need(input$gps_dti != '' && input$gps_dti > 0,
                    "Requires a value."))

      new_dti <- sigdigits(input$gps_dti_units %#%
                             input$gps_dti %#% vals$gps_dti_units, 3)

      updateNumericInput(
        session,
        inputId = "gps_dti",
        label = NULL,
        min = 1,
        value = new_dti)

    }) %>% bindEvent(input$gps_dti_units)

    observe({
      validate(need(input$vhf_dur != '', "Requires a value."))
      req(input$vhf_dur_units != vals$vhf_dur_units)

      new_dur <- sigdigits(input$vhf_dur_units %#%
                             input$vhf_dur %#% vals$vhf_dur_units, 3)

      updateNumericInput(
        session,
        inputId = "vhf_dur",
        label = NULL,
        min = 1,
        value = new_dur)

    }) %>% bindEvent(input$vhf_dur_units)

    observe({
      validate(need(input$vhf_dti != '', "Requires a value."))
      req(input$vhf_dti_units != vals$vhf_dti_units)

      new_dti <- sigdigits(input$vhf_dti_units %#%
                             input$vhf_dti %#% vals$vhf_dti_units, 3)

      updateNumericInput(
        session,
        inputId = "vhf_dti",
        label = NULL,
        min = 1, value = new_dti)

    }) %>% bindEvent(input$vhf_dti_units)

    ## Changing sampling parameters: ------------------------------------

    observe({
      vals$is_reg_valid <- FALSE

      output$regUI_sampling <- renderUI({
        if (input$device_type == 1) {

          ### 1. GPS & Satellite logger:

          column(
            align = "center", width = 12,

            #### 1.1. Select tracking regime:

            p("What", span("sampling interval", class = "col-hgl"),
              "will you evaluate?") %>%
              tagAppendAttributes(class = 'label_center'),

            if (!input$eval_tradeoffs) {
              uiOutput(ns("gpsSelect_fix")) },

            if (input$eval_tradeoffs) {
              p(style = "padding-bottom: 15px;",

                "Please select a",
                span("GPS fix rate", class = "col-hgl"),
                "from the", span("plot", class = "col-key"),
                "below to further evaluate that",
                HTML(paste0(span("tracking regime",
                                 class = "col-hgl"), "."))) },

            #### 1.2. Plotting GPS battery life decay:
            if (input$eval_tradeoffs) {

              ggiraph::girafeOutput(
                outputId = ns("regPlot_gps"),
                width = "100%", height = "50%")
            },

            #### 1.3. Plotting GPS battery life decay:
            if (input$eval_tradeoffs) {

              div(style = paste("position: absolute;",
                                "top: 135px;", "right: calc(5%);"),

                  shinyWidgets::switchInput(
                    inputId = ns("deviceInput_log"),
                    label = span(icon("wrench"),
                                 "Logarithmic"),
                    labelWidth = "100px")
              ) },

            uiOutput(ns("regText_gps"))

          ) # end of column (UI)

        } else {

          ### 2. VHF transmitter:

          column(
            align = "center", width = 12,

            p("Here, you can vizualize the impact of different",
              "tracking regimes on",
              span("sample sizes", class = "cl-sea"),
              "by selecting the",
              span("study duration", class = "cl-dgr"),
              "(based on VHF battery life), and",
              span("sampling frequency", class = "cl-dgr"),
              "(based on how many times you will collect new",
              "locations)."),

            ggiraph::girafeOutput(
              outputId = ns("regPlot_vhf"),
              width = "100%", height = "50%"),

            p("What", span("sampling interval", class = "col-hgl"),
              "will you evaluate?") %>%
              tagAppendAttributes(class = 'subheader'),

            uiOutput(ns("vhfSelect_fix")),
            uiOutput(ns("regText_vhf"))

          ) # end of column (UI)
        } # end of if (), VHF device

      }) # end of renderUI // regUI_sampling

    }) %>% # end of observe
      bindEvent(input$device_type)

    observe({ # Add footer to sampling box:

      output$regUI_sampling_footer <- renderUI({
        if (input$device_type == 1) {

          ### GPS & Satellite logger:

          tagList(column(
              width = 12, align = "right",
              style = "padding-right: 5px;",

              uiOutput(ns("regButton_gps"), inline = TRUE),
              HTML("&nbsp;"),
              shiny::actionButton(
                inputId = ns("run_sim_new"),
                icon =  icon("bolt"),
                label = "Run",
                width = "120px",
                class = "btn-primary")

          )) # end of tagList (footer)

        } else {

          ### VHF transmitter:

          tagList(
            column(
              width = 12, align = "right",
              style = "padding-right: 5px;",

              uiOutput(ns("regButton_vhf"), inline = TRUE),
              HTML("&nbsp;"),
              shiny::actionButton(
                inputId = ns("run_sim_new"),
                icon =  icon("bolt"),
                label = "Run",
                width = "120px",
                class = "btn-primary")
            )
            # , shiny::actionButton(
            #   inputId = ns("regHelp_vhf"),
            #   label = NULL,
            #   width = "39px",
            #   icon = icon("circle-question"),
            #   class = "btn-warning",
            #   style = "position: absolute; left: 15px;")

          ) # end of tagList (footer)

        } # end of if (), VHF device
      }) # end of renderUI // regUI_sampling_footer

    }) %>% # end of observe
      bindEvent(input$device_type, ignoreInit = TRUE)

    ## Change sample size text: -----------------------------------------

    observe({
      if (input$est_type == 1) {

        out_text <- helpText(
          "As", span("effective sample sizes", class = "cl-sea"),
          "are", HTML(paste0(
            span("roughly estimated", class = "cl-dgr"), ",")),
          "these values will update before validation.",
          "However, they should only be used for a quick evaluation,",
          "and will not always correspond to the real sample sizes.")

      } else {

        vals$device_n <- NULL
        vals$device_N1 <- NULL
        vals$device_N2 <- NULL

        out_text <- helpText(
          "As", span("effective sample sizes", class = "cl-sea"),
          "are extracted from the",
          HTML(paste0(span("model fit", class = "cl-dgr"), ",")),
          "these values will", span("not", style = "color: #000;"),
          "update automatically.", br(), "Click the",
          icon("bolt", class = "cl-mdn"),
          HTML(paste0(span("Run", class = "cl-mdn"))),
          "button to refresh.")
      }

      output$regText_sizes <- renderUI({ out_text })

    }) %>% # end of observe
      bindEvent(input$est_type)

    # VALIDATION ----------------------------------------------------------

    ## Regime... ----------------------------------------------------------
    ### ...GPS & Satellite loggers: ---------------------------------------

    observe({
      req(!is.null(input$eval_tradeoffs))

      # Check if inputs were selected:

      vals$is_reg_valid <- FALSE
      if ((!input$eval_tradeoffs && is.null(input$gps_dti)) ||
          (input$eval_tradeoffs && is.null(input$regPlot_gps_selected))
      ) {

        shinyalert::shinyalert(
          title = "No regime selected",
          text = span(
            "Please select a fix rate to set a",
            HTML(paste0(span("tracking regime", class = "cl-sea-d"),
                        ",")), "then click the",
            icon("bolt", class = "cl-sea"),
            span("Validate", class = "cl-sea"),
            "button again."),
          html = TRUE,
          size = "xs")

      } else {
        validate(need(vals$reg$dur != '' && vals$reg$dur > 0,
                      "Requires a non-zero value."),
                 need(vals$reg$dti != '' && vals$reg$dti > 0,
                      "Requires a non-zero value."))
        vals$is_reg_valid <- TRUE

        dur <- vals$reg$dur %#% vals$reg$dur_unit
        dti <- vals$reg$dti %#% vals$reg$dti_unit

        t0 <- seq(1, dur, by = dti)
        vals$device_n <- length(t0)

        # Check GPS storage limit (if selected):

        req(vals$is_reg_valid,
            vals$device_n,
            vals$is_limit)

        vals$storage <- input$gps_maxlocs

        if (vals$device_n > vals$storage) {

          shinyalert::shinyalert(
            title = "Not enough GPS storage",
            text = span(
              "GPS storage limit",
              HTML(paste0("(", scales::label_comma()(vals$storage),
                          " locations)")),
              "is lower than the absolute sample size",
              HTML(paste0("(",  scales::label_comma()(vals$device_n),
                          " locations)."))
            ),
            html = TRUE,
            size = "xs")

          vals$is_reg_valid <- FALSE

        } else {

          vals$is_reg_valid <- TRUE
          vals$is_analyses <- FALSE

        } # end of storage limit if() statement
      } # end of selection if() statement

    }) %>% # end of observer,
      bindEvent(input$validate_gps)

    ### ...VHF transmitter: -----------------------------------------------

    observe({

      # Check if inputs were selected:

      if (is.null(input$vhf_dti)) {

        vals$is_reg_valid <- FALSE
        shinyalert::shinyalert(
          title = "No regime selected",
          text = span(
            "Please select a fix rate",
            "to set a",
            HTML(paste0(span("tracking regime", class = "cl-sea-d"),
                        ",")), 'then click the',
            icon("bolt", class = "cl-mdn"),
            span('Validate', class = "cl-sea"),
            'button again.'),
          html = TRUE,
          size = "xs")

      } else {
        validate(need(vals$reg$dur != '' && vals$reg$dur > 0,
                      "Requires a non-zero value."),
                 need(vals$reg$dti != '' && vals$reg$dti > 0,
                      "Requires a non-zero value."))

        vals$is_reg_valid <- TRUE
        vals$is_analyses <- FALSE

        dur <- vals$reg$dur %#% vals$reg$dur_unit
        dti <- vals$reg$dti %#% vals$reg$dti_unit

        t0 <- seq(1, dur, by = dti)
        vals$device_n <- length(t0)

      } # end of if (is.null(input$vhf_dti))

    }) %>% # end of observer,
      bindEvent(input$validate_vhf)

    # Regime verified:
    observe({

      updateTabsetPanel(
        session,
        inputId = "regTabs_pars",
        selected = "tab_device_1-regPanel_device")

      shinyjs::show(id = "regBox_type")

      shinyFeedback::showToast(
        type = "success",
        message = "Regime validated!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"
        )
      )

    }) %>% bindEvent(req(vals$is_reg_valid))

    ## Sample sizes: ------------------------------------------------------

    observe({
      req(input$alert_n_small)
      vals$confirm_n <- TRUE
    })

    observe({
      req(vals$device_n)

      if(vals$device_n > 10000) {
        vals$confirm_n <- FALSE
        shinyalert::shinyalert(
          inputId = "alert_n_small",
          title = "Warning",
          text = span(
            "You are about to simulate a dataset with over",
            scales::label_comma()(vals$device_n), "locations.",
            "This can take a considerable amount of time to run",
            "on certain devices (> hours).",
            "Do you wish to proceed?"),
          showCancelButton = TRUE,
          html = TRUE,
          size = "xs")
      } else { vals$confirm_n <- TRUE }

    }) %>% # end of observe,
      bindEvent(input$run_sim_new)

    # CALCULATIONS --------------------------------------------------------
    ## Prepare parameters: ------------------------------------------------

    saved_gps <- reactive({
      list(input$gps_dur,
           input$gps_dur_units,
           input$gps_dti,
           input$gps_dti_units)
    })

    observe({
      req(vals$active_tab == 'regime')

      validate(
        need(input$gps_dur != '', "Select a value."),
        need(input$gps_dur_units != '', "Please choose a unit."),
        need(input$gps_dti != '', "Select a value."),
        need(input$gps_dur_units != '', "Please choose a unit."))

      vals$gps_dur <- input$gps_dur
      vals$gps_dur_units <- input$gps_dur_units
      if(!input$eval_tradeoffs) {
        vals$gps_dti <- input$gps_dti
        vals$gps_dti_units <- input$gps_dti_units
      }

    }) %>% bindEvent(saved_gps())

    saved_vhf <- reactive({
      list(input$vhf_dur,
           input$vhf_dur_units,
           input$vhf_dti,
           input$vhf_dti_units)
    })

    observe({
      req(vals$active_tab == 'regime')
      validate(
        need(input$vhf_dur != '', "Select a value."),
        need(input$vhf_dti != '', "Select a value."))

      vals$vhf_dur <- input$vhf_dur
      vals$vhf_dur_units <- input$vhf_dur_units

      vals$vhf_dti <- input$vhf_dti
      vals$vhf_dti_units <- input$vhf_dti_units

    }) %>% bindEvent(saved_vhf())

    ## Device — sampling duration & interval: ---------------------------

    observe({
      vals$reg$dur <- NULL
      vals$reg$dti <- NULL

      # GPS:
      if (input$device_type == 1) {

        if (input$eval_tradeoffs &&
            !is.null(input$regPlot_gps_selected)) {
          req(vals$df_gps)
          validate(need(input$gps_dur != '', "Requires a value."))

          df0 <- vals$df_gps
          selected <- reg_selected()

          vals$reg$dur <- input$gps_dur_units %#% df0[selected, ]$dur
          vals$reg$dur_unit <- input$gps_dur_units

          tmpdti_notes <- df0[selected, ]$nu_notes
          tmpdti_units <- sub('^.* ([[:alnum:]]+)$',
                              '\\1', tmpdti_notes)
          vals$reg$dti_unit <- tmpdti_units
          vals$reg$dti <- tmpdti_units %#% df0[selected, ]$nu

        } else {
          req(input$gps_dur, input$gps_dti)
          validate(need(input$gps_dur != '' && input$gps_dur > 0,
                        "Requires a non-zero value."),
                   need(input$gps_dti != '' && input$gps_dti > 0,
                        "Requires a non-zero value."))

          vals$reg$dur <- input$gps_dur
          vals$reg$dur_unit <- input$gps_dur_units
          vals$reg$dti <- input$gps_dti
          vals$reg$dti_unit <- input$gps_dti_units
        }

      } else if (input$device_type == 2) {

        if (!is.null(input$regPlot_vhf_selected)) {
          req(vals$df_vhf)

          selected <- reg_selected()
          vals$reg$dur <- vals$df_vhf[selected, ]$dur
          vals$reg$dur_unit <- vals$df_vhf[selected, ]$dur_units

          vals$reg$dti <- input$vhf_dti
          vals$reg$dti_unit <- input$vhf_dti_units

        } else {
          req(input$vhf_dur, input$vhf_dti)

          validate(need(input$vhf_dur != '', "Requires a value."),
                   need(input$vhf_dti != '', "Requires a value."))

          vals$reg$dur <- input$vhf_dur
          vals$reg$dur_unit <- input$vhf_dur_units
          vals$reg$dti <- input$vhf_dti
          vals$reg$dti_unit <- input$vhf_dti_units
        }

      } # end of if ()
    })

    ## Device — sample sizes: -------------------------------------------

    observe({
      req(vals$tau_p0, vals$tau_v0,
          vals$reg$dur, vals$reg$dti)

      validate(need(vals$reg$dur != '' && vals$reg$dur > 0,
                    "Requires a non-zero value."),
               need(vals$reg$dti != '' && vals$reg$dti > 0,
                    "Requires a non-zero value."))

      n <- NULL
      N1 <- NULL
      N2 <- NULL

      tauv <- vals$tau_v0 %#% vals$tau_v0_units
      taup <- vals$tau_p0 %#% vals$tau_p0_units

      dur <- vals$reg$dur %#% vals$reg$dur_unit
      dti <- vals$reg$dti %#% vals$reg$dti_unit

      t0 <- seq(1, dur, by = dti)
      n <- length(t0)
      n_loss <- round(n * (input$deviceInput_loss/100), 0)
      vals$n_lost <- n_loss
      r <- dti / tauv

      if (input$est_type == 1) {
        vals$is_fitted <- "No"

        N1 <- dur / taup
        N2 <- ifelse(
          dti > tauv,
          ifelse(dti > 3 * tauv, 0,
                 (n - n_loss) / (dti/tauv)^r * (dti/tauv)),
          (n - n_loss) / tauv * dti)
        vals$device_n <- ifelse(n_loss > 0, n - n_loss, n)

        if (N1 > (n - n_loss)) { N1 <- n }

      } else if (input$est_type == 2) {

        req(vals$fit1)
        vals$is_fitted <- "Yes"

        nms <- names(summary(vals$fit1)$DOF)
        N1 <- summary(vals$fit1)$DOF[grep('area', nms)][[1]]
        N2 <- summary(vals$fit1)$DOF[grep('speed', nms)][[1]]
        vals$device_n <- nrow(vals$data1)

      }

      vals$device_N1 <- N1
      vals$device_N2 <- N2

    }) # end of observe

    # SIMULATIONS -------------------------------------------------------
    ## Simulating GPS battery life: -------------------------------------

    df_decay <- shiny::reactive({

      is_simplified <- TRUE

      k <- seq(0.001, 8.046066, length.out = 30)
      k0 <- k[input$gps_k]

      dat <- simulate_gps(
        data = movedesign::gps_fixrate,
        k = k0,
        yrange = input$gps_dur,
        yunits = input$gps_dur_units,
        cutoff = 10 %#% "days",
        max_x = input$gps_dti_max,
        min_x = input$gps_dti_min,
        simplified = is_simplified)


      # if(!is_simplified) {
      # # Display only values with duration
      # # (plus three additional rows):
      #
      #   max_freq <- dat %>%
      #     dplyr::filter(dur < 10 %#% "days") %>%
      #     dplyr::pull(freq_hrs) %>% min()
      #
      #   tmpdf <- dat %>%
      #     dplyr::filter(freq_hrs >= 0) %>%
      #     dplyr::filter(freq_hrs < max_freq)
      #
      #   if (nrow(tmpdf) + 3 <= nrow(dat) - 3) {
      #     dat <- dat[1:(nrow(tmpdf) + 3),]
      #   } else { dat <- dat[1:nrow(tmpdf),] }
      # }

      return(dat)

    }) %>% # end of reactive, df_decay()
      bindCache(input$gps_dur,
                input$gps_dur_units,
                input$gps_dti_max,
                input$gps_dti_min,
                input$gps_k)

    ## Simulating new conditional data: ---------------------------------

    ### Prepare parameters and reactives:

    data_sim <- shiny::reactive({

      dur <- vals$reg$dur %#% vals$reg$dur_unit
      dti <- vals$reg$dti %#% vals$reg$dti_unit
      t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      
      ctmm::simulate(
        vals$data0,
        vals$fit0,
        t = t_new,
        seed = vals$seed0) %>%
        ctmm:::pseudonymize()

    }) %>% # end of reactive, data_sim
      bindCache(c(vals$id,
                  vals$reg$dur, vals$reg$dur_unit,
                  vals$reg$dti, vals$reg$dti_unit))

    runtime <- shiny::reactive({

      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = span(
          style = "font-size: 18px;",
          span("Calculating", style = "color: #797979;"),
          HTML(paste0(span("run time", class = "cl-sea"),
                      span("...", style = "color: #797979;")))
        )
      )

      out <- estimate_time(vals$data1, parallel = vals$parallel)

      shinybusy::remove_modal_spinner()
      return(out)

    }) %>% # end of reactive, data_sim
      bindCache(c(vals$reg$dur, vals$reg$dur_unit,
                  vals$reg$dti, vals$reg$dti_unit))

    # Run simulation:

    observe({
      req(vals$data0,
          vals$fit0,
          vals$reg$dur,
          vals$reg$dti,
          input$est_type,
          vals$is_reg_valid,
          vals$confirm_n)

      shinyjs::show(id = "regBox_sims")

      shinyFeedback::showToast(
        type = "info",
        message = "Setting up simulation...",
        .options = list(
          timeOut = 2500,
          extendedTimeOut = 3500,
          progressBar = TRUE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"
        )
      )

      start <- Sys.time()
      data1 <- data_sim()

      vals$needs_fit <- TRUE

      vals$data1 <- data1
      vals$data1_full <- data1

      if (!is.null(input$deviceInput_loss)) {
        if (input$deviceInput_loss > 0) {
          perc_loss <- 1 - input$deviceInput_loss/100
          n_row <- nrow(data1)
          n_cut <- round(n_row * perc_loss)

          rows_thin <- sort(sample(1:nrow(data1), n_cut))
          data1_thin <- data1[rows_thin,]
          vals$data1 <- data1_thin
          vals$data1_full <- data1
          vals$loss <- input$deviceInput_loss
        }
      }

      if (!is.null(input$deviceInput_error)) {
        if (input$deviceInput_error > 0) {
          error_x <- stats::rnorm(nrow(data1), mean = 0,
                                  sd = input$deviceInput_error)
          error_y <- stats::rnorm(nrow(data1), mean = 0,
                                  sd = input$deviceInput_error)
          data1[c(2,3)] <- data1[c(2,3)] + c(error_x, error_y)
          vals$data1 <- data1
          vals$error <- input$deviceInput_error
        }
      }

      tmptime <- difftime(Sys.time(), start, units = 'min')

      if (round(tmptime, 0) < 1) {
        tmpdetail <- paste("This step took less than one minute.")
      } else {
        tmpdetail <- paste("This step took approximately",
                           round(difftime(Sys.time(), start,
                                          units = 'min'), 0),
                           "minutes.")
      }

      msg_log(
        style = "success",
        message = paste0("Simulation ",
                         msg_success("completed"), "."),
        detail = tmpdetail)
      vals$time_sims <- difftime(Sys.time(), start, units = "mins")

      ### Run model fit (if set):

      if (input$est_type == 1) {
        vals$needs_fit <- TRUE
      } else {
        req(vals$data1)

        msg_log(
          style = "warning",
          message = paste0("Model fit ",
                           msg_warning("in progress"), "."),
          detail = "Please wait for model selection to finish:")

        expt <- runtime()

        vals$expt_max <- expt$max
        vals$expt_min <- expt$min
        vals$expt_units <- expt$units

        if ((vals$expt_max %#% vals$expt_units) > 900) {

          vals$confirm_time <- FALSE
          shinyalert::shinyalert(
            className = "modal_warning",
            title = "Do you wish to proceed?",
            callbackR = function(x) {
              vals$confirm_time <- x
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
          vals$confirm_time <- TRUE
        }
      }

      req(vals$confirm_time)

      if (input$est_type == 1) {
        vals$needs_fit <- TRUE
      } else {

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
            span("Selecting", style = "color: #797979;"),
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

        # newmod <- prepare_mod(
        #   tau_p = vals$tau_p0, tau_p_units = vals$tau_p0_units,
        #   tau_v = vals$tau_v0, tau_v_units = vals$tau_v0_units,
        #   sigma = vals$sigma0, sigma_units = vals$sigma0_units)
        # fit1 <- ctmm::ctmm.fit(vals$data1, newmod)

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

          shinyjs::enable("regButton_save")

        } # end of if(), !is.null(fit1)
      } # end of if(), est_type

      if (!vals$tour_active) {

        shinyalert::shinyalert(
          className = "modal_success",
          type = "success",
          title = "Success!",
          text = span(
            "Proceed to the", br(),
            icon("compass-drafting", class = "cl-mdn"),
            span('Analyses', class = "cl-mdn"), "tabs."),
          html = TRUE,
          size = "xs")

      }

      # if (!input$regBox_sampling$collapsed &&
      #    vals$tour_active) { NULL } else {
      #      shinydashboardPlus::updateBox(
      #        "regBox_sampling", action = "toggle") }

      shinyFeedback::showToast(
        type = "success",
        message = "Simulation completed!",
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

    }) %>% # end of observe,
      bindEvent(input$run_sim_new)

    # PLOTS ---------------------------------------------------------------
    ## Plotting VHF: ------------------------------------------------------

    observe({
      req(input$vhf_dur, input$vhf_ppm)

      vhfrow <- data.frame(
        dur = input$vhf_dur,
        dur_units = input$vhf_dur_units,
        dur_secs = input$vhf_dur %#% input$vhf_dur_units,
        ppm = input$vhf_ppm)

      vals$df_vhf <<- rbind(vals$df_vhf, vhfrow)

    }) %>% # end of observe
      bindEvent(input$add_vhf_point)

    # observe({
    #   vals$df_vhf <- NULL
    # }) %>% # end of observe,
    #   bindEvent(input$vhfDat_clear)

    output$regPlot_vhf <- ggiraph::renderGirafe({
      req(vals$df_vhf) # req(nrow(vals$df_vhf) > 1)

      df0 <- vals$df_vhf
      df0$id <- 1:nrow(df0)
      df0$ppm_notes <- paste(df0$ppm, "ppm")

      dur_units <- df0$dur_units[1]
      df0$dur_new <- dur_units %#% df0$dur_secs

      p <- df0 %>%
        ggplot2::ggplot(
          ggplot2::aes(x = ppm,
                       y = dur_new,
                       data_id = as.numeric(id))) +

        ggplot2::geom_smooth(
          method = lm,
          formula = y ~ x,
          color = "#f4f4f4", se = F,
          size = 1.5, alpha = .8) +

        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = ppm_notes),
          size = 2,
          col = pal$mdn) +

        ggplot2::coord_fixed(ratio = -3) +
        ggplot2::labs(
          x = "Pulse rate (ppm)",
          y = paste0("Durations (in ", dur_units, ")")) +
        theme_movedesign() +
        ggplot2::theme(legend.position = "none")

      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 5.5,
        options = list(
          ggiraph::opts_hover(
            css = paste("r:5pt;",
                        "fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("r:5pt;",
                        "fill:#dd4b39;",
                        "stroke:#eb5644;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))

    }) # end of renderGirafe // regPlot_vhf

    ## Plotting GPS battery decay: --------------------------------------

    reg_selected <- reactive({
      if (input$device_type == 1) return(input$regPlot_gps_selected)
      if (input$device_type == 2) return(input$regPlot_vhf_selected)
    })

    observe({
      req(vals$active_tab == 'regime',
          input$device_type == 1,
          input$eval_tradeoffs,
          input$gps_k,
          input$gps_dur,
          input$gps_dur_units,
          input$gps_dti_max,
          input$gps_dti_min)

      dat <- movedesign::gps_fixrate
      dti_max <- dat$freq_hrs[match(
        input$gps_dti_max, dat$nu_notes)]
      dti_min <- dat$freq_hrs[match(
        input$gps_dti_min, dat$nu_notes)]

      req(dti_max < dti_min)

      df0 <- df_decay()
      vals$df_gps <- df0

      if (!("blue" %in% df0$color)) {
        pal_values <- pal$dgr
      } else { pal_values <- c(pal$mdn, pal$dgr) }

      output$regPlot_gps <- ggiraph::renderGirafe({

        if (input$deviceInput_log) {

          df0$x <- log(vals$df_gps$freq_hrs)
          x_label <- "Log of frequency (fixes per hour)"

          p1 <- ggplot2::geom_vline(xintercept = 0, alpha = 0)

        } else {
          df0$x <- vals$df_gps$freq_hrs
          x_label <- "Frequency (fixes per hour)"

          p1 <- ggplot2::geom_vline(xintercept = 0,
                                    color = "grey80",
                                    size = 0.2)
        }

        df0$dur <- input$gps_dur_units %#% df0$dur
        dur_units <- input$gps_dur_units

        ymax <- max(df0$dur) + diff(range(df0$dur)) * .2


        p <- ggplot2::ggplot(
          df0, ggplot2::aes(x = x,
                            y = dur,
                            col = color,
                            tooltip = nu_notes,
                            data_id = as.numeric(id))) +

          p1 +
          ggplot2::geom_hline(yintercept = 0,
                              color = "grey80",
                              size = 0.2) +

          ggplot2::geom_smooth(
            method = minpack.lm::nlsLM,
            formula = "y ~ y0/(1+exp(log(x)-log(k0)))",
            method.args = list(
              start = c(y0 = input$gps_dur %#% input$gps_dur_units,
                        k0 = input$gps_k)),
            se = F, color = "grey30",
            size = 2, alpha = .8) +

          ggiraph::geom_point_interactive(size = 1.5) +
          ggplot2::scale_color_manual(values = pal_values) +

          # ggplot2::scale_x_continuous(
          #   labels = scales::comma,
          #   expand = c(0, 0),
          #   limits = c(0, NA)) +
          ggplot2::scale_y_continuous(
            labels = scales::comma,
            limits = c(0, ymax)) +

          ggplot2::labs(
            x = x_label,
            y = paste0("Durations (in ", dur_units, ")")) +
          theme_movedesign() +
          # ggplot2::theme(axis.line = ggplot2::element_line(
          #   colour = "grey80", size = 0.2, linetype = "solid"),
          #   legend.position = "none") +
          ggplot2::theme(legend.position = "none")

        # if (!is.null(input$gps_selected_dti)) {
        #   tempid <- match(input$gps_selected_dti, df0$nu_notes)
        #   preselection <- as.character(tempid)
        # } else {
        #   preselection <- character(0)
        # }

        ggiraph::girafe(
          ggobj = p,
          width_svg = 5, height_svg = 4,
          options = list(
            ggiraph::opts_hover(
              css = paste("r:4pt;",
                          "fill: #ffbf00;",
                          "stroke: #ffbf00;")),
            ggiraph::opts_selection(
              # selected = preselection,
              type = "single",
              css = paste("r:4pt;",
                          "fill: #009da0;",
                          "stroke: #09da0;")),
            ggiraph::opts_toolbar(saveaspng = FALSE)))

      }) # end of renderGirafe // regPlot_gps
    }) # end of observe

    ## Render new simulated data plot (xy): -------------------------------

    output$regPlot_id <- ggiraph::renderGirafe({
      req(vals$data0, vals$data1)

      newdat <- vals$data1
      if (vals$data_type == "simulated") {
        dat <- vals$data0[which(vals$data0$t <= max(newdat$t)), ]
      } else {
        dat <- vals$data0
      }

      ymin <- min(
        min(newdat$y) - diff(range(newdat$y)) * .2,
        min(dat$y) - diff(range(dat$y)) * .2)

      ymax <- max(
        max(newdat$y) + diff(range(newdat$y)) * .2,
        max(dat$y) + diff(range(dat$y)) * .2)

      p <- ggplot2::ggplot() +
        ggplot2::geom_path(
          dat, mapping = ggplot2::aes(
            x = x, y = y),
          col = "grey90", size = 1.4) +

        ggiraph::geom_path_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = timestamp),
          size = .6, alpha = .8) +

        ggiraph::geom_point_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = timestamp,
            tooltip = timestamp),
          size = 2.5) +

        ggplot2::labs(
          x = "x coordinate",
          y = "y coordinate") +

        ggplot2::scale_x_continuous(
          labels = scales::comma) +
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
        ggplot2::guides(
          color = ggplot2::guide_colorbar(
            title.hjust = 4, title.vjust = 1.02)) +
        ggplot2::theme(
          legend.position = c(0.76, 0.08),
          legend.direction = "horizontal",
          legend.title = ggplot2::element_text(size = 11),
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

    }) # end of renderGirafe // regPlot_id

    ## Render variogram (svf): --------------------------------------------

    output$regPlot_svf <- ggiraph::renderGirafe({
      req(vals$data1)

      frac <- input$regVar_timeframe / 100
      svf <- prepare_svf(vals$data1, fraction = frac)
      p <- plotting_svf(svf, fill = pal$dgr)

      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 4,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .1),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill: #ffbf00;",
                        "stroke: #ffbf00;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))

    }) # end of renderGirafe // regPlot_svf

    # TABLES ------------------------------------------------------------
    ## Listing multiple tracking regimes: -------------------------------

    regRow <- reactive({

      out <- data.frame(
        device = NA,
        dur = NA,
        dti = NA,
        n = NA,
        N1 = NA,
        N2 = NA,
        fit = NA)

      out$device <- vals$device_type

      dur <- fix_unit(vals$reg$dur, vals$reg$dur_unit)
      dti <- fix_unit(vals$reg$dti, vals$reg$dti_unit)

      out$dur <- paste(dur[1], abbrv_unit(dur[,2]))
      out$dti <- paste(dti[1], abbrv_unit(dti[,2]))

      out$n <- vals$device_n
      out$N1 <- vals$device_N1
      out$N2 <- vals$device_N2
      out$fit <- vals$is_fitted

      return(out)

    })

    observe({
      req(vals$reg$dur, vals$reg$dur_unit,
          vals$reg$dti, vals$reg$dti_unit,
          vals$device_n, vals$device_N1, vals$device_N2)

      shinyjs::show(id = "regBox_summary")
      shinyjs::disable("regButton_save")

      vals$dt_regs <<- rbind(vals$dt_regs, regRow())
      vals$dt_regs <- dplyr::distinct(vals$dt_regs)
      vals$report_regs_yn <- TRUE

    }) %>% # end of observe,
      bindEvent(input$regButton_save)

    output$regTable <- reactable::renderReactable({
      req(vals$dt_regs)

      columnNames <- list(
        device = "Type",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = "N (area)",
        N2 = "N (speed)",
        fit = "Fitted?")

      reactable::reactable(
        vals$dt_regs,
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

        columns = list(
          device = reactable::colDef(
            name = columnNames[["device"]]),
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
          N1 = reactable::colDef(
            minWidth = 80, name = columnNames[["N1"]],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)),
          N2 = reactable::colDef(
            minWidth = 80, name = columnNames[["N2"]],
            style = list(color = format_num),
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)),
          fit = reactable::colDef(
            minWidth = 70, name = columnNames[["fit"]])
        ))

    }) # end of renderReactable // regTable

    observe({
      vals$dt_regs <- NULL
    }) %>% # end of observe,
      bindEvent(input$regTable_clear)

    # BLOCKS ------------------------------------------------------------
    ## Species parameters: ----------------------------------------------

    observe({
      req(vals$active_tab == 'regime',
          vals$tau_p0, vals$tau_v0, vals$data_type)

      tauv <- fix_unit(vals$tau_v0, vals$tau_v0_units)
      taup <- fix_unit(vals$tau_p0, vals$tau_p0_units)

      if (vals$data_type == "simulated") {
        taup_range <- tauv_range <- NULL
      } else {
        taup_min <- fix_unit(vals$tau_p0_min, vals$tau_p0_units)
        taup_max <- fix_unit(vals$tau_p0_max, vals$tau_p0_units)
        taup_range <- span(paste(taup_min[1], "—", taup_max[1]),
                           class = "cl-mdn")

        tauv_min <- fix_unit(vals$tau_v0_min, vals$tau_v0_units)
        tauv_max <- fix_unit(vals$tau_v0_max, vals$tau_v0_units)
        tauv_range <- span(paste(tauv_min[1], "—", tauv_max[1]),
                           class = "cl-mdn")
      }

      output$regBlock_tau_p <- renderUI({

        parBlock(
          header = span(
            HTML(paste0("Position autocorrelation ",
                        "(\u03C4", tags$sub("p"), ")"))),
          value = span(paste(taup[1], taup[2]), class = "cl-mdn"),
          subtitle = taup_range)

      }) # end of renderUI // regBlock_tau_p

      output$regBlock_tau_v <- renderUI({

        parBlock(
          header = span(
            HTML(paste0("Velocity autocorrelation ",
                        "(\u03C4", tags$sub("v"), ")"))),
          value = span(paste(tauv[1], tauv[2]), class = "cl-mdn"),
          subtitle = tauv_range)

      }) # end of renderUI // regBlock_tau_v

    }) # end of observe

    ## Tracking regime: -------------------------------------------------

    output$regUI_regime <- renderUI({
      req(vals$is_reg_valid)
      splitLayout(
        uiOutput(ns("regBlock_dur0_dev")),
        uiOutput(ns("regBlock_dti0_dev")))
    })

    output$regBlock_dur0_dev <- renderUI({
      req(vals$reg$dur)

      out <- fix_unit(vals$reg$dur, vals$reg$dur_unit,
                      convert = TRUE)
      parBlock(
        header = "Sampling duration",
        value = paste(out[1], out[2]))

    }) # end of renderUI // regBlock_dur0_dev

    output$regBlock_dti0_dev <- renderUI({
      req(vals$reg$dti, vals$reg$dti_unit)

      out <- fix_unit(vals$reg$dti, vals$reg$dti_unit,
                      convert = TRUE)

      parBlock(
        header = "Sampling interval",
        value = paste(out[1], out[2]),
        subtitle = "between fixes")

    }) # end of renderUI // regBlock_dti0_dev

    ## Sample sizes: ----------------------------------------------------

    output$regBlock_n <- renderUI({
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

    }) # end of renderUI // regBlock_n

    output$regBlock_Narea <- renderUI({
      req(vals$device_N1)

      if (input$est_type == 1) {
        diff1 <- paste0("-", round((100 - ((
          vals$device_N1 * 100)/vals$device_n)), 1), "%")
      } else {
        diff1 <- paste0("-", round((100 - (
          vals$device_N1 * 100)/nrow(vals$data1)), 1), "%")
      }

      sampleBlock(
        number = diff1,
        numberIcon = TRUE,
        header = round(vals$device_N1, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // regBlock_Narea

    output$regBlock_Nspeed <- renderUI({
      req(vals$device_N2)

      if (input$est_type == 1) {
        diff2 <- paste0("-", round((100 - ((
          vals$device_N2 * 100)/vals$device_n)), 1), "%")
      } else {
        diff2 <- paste0("-", round((100 - (
          vals$device_N2 * 100)/nrow(vals$data1)), 1), "%")
      }

      sampleBlock(
        number = diff2,
        numberIcon = TRUE,
        header = round(vals$device_N2, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // regBlock_Nspeed

    ## Data loss: -------------------------------------------------------

    output$regBlock_loss <- renderUI({
      req(vals$n_lost, input$deviceInput_loss > 0)

      parBlock(
        header = "Fixes lost:",
        value = vals$n_lost)

    }) # end of renderUI // regBlock_loss

    # MODALS & HELP -------------------------------------------------------

    # # ## GPS & Satellite logger:
    # #
    # # observe({
    # #
    # #   shiny::showModal(
    # #     shiny::modalDialog(
    # #       title = "GPS & Satellite loggers:",
    # #
    # #       column(
    # #         align = "center", width = 12,
    # #
    # #         p("With", HTML(paste0(
    # #           span("GPS & satellite loggers", class = "cl-sea-d"), ",")),
    # #           "sampling is", span("automated", class = "cl-sea"),
    # #           "and conducted by satellite systems. Therefore,",
    # #           span("sampling frequency", class = "cl-dgr"),
    # #           "and", span("sampling duration", class = "cl-dgr"),
    # #           "are both inherently linked with",
    # #           HTML(paste0(span("GPS battery life",
    # #                            class = "cl-sea"), ".")),
    # #           "This tradeoff restricts the volume of data that",
    # #           "can be collected by GPS & satellite loggers."),
    # #
    # #         uiOutput(ns("help_design"))
    # #
    # #       ), # end of column (main)
    # #
    # #       footer = tagList(
    # #         modalButton("Dismiss")
    # #       ),
    # #       size = "l"))
    # #
    # # }) %>% bindEvent(input$regHelp_gps)
    # #
    # # ## VHF transmitter:
    # #
    # # observe({
    # #
    # #   shiny::showModal(
    # #     shiny::modalDialog(
    # #       title = "VHF transmitter:",
    # #
    # #       column(
    # #         align = "center", width = 12,
    # #
    # #         p("With", HTML(paste0(
    # #           span("Very-High-Frequency (VHF) transmitters",
    # #                class = "cl-sea-d"), ", ")),
    # #           "the interval between new locations/fixes",
    # #           "is subject only to the tracking regime employed",
    # #           span("manually", class = "cl-sea"),
    # #           "in the study site.",
    # #           "As such, battery life is decoupled from any set",
    # #           HTML(paste0(span("sampling frequency",
    # #                            class = "cl-dgr"), ",")),
    # #           "and is mainly linked to",
    # #           HTML(paste0(span("sampling duration",
    # #                            class = "cl-dgr"), "."))),
    # #
    # #         uiOutput(ns("help_design"))
    # #
    # #       ), # end of column (main)
    # #
    # #       footer = tagList(
    # #         modalButton("Dismiss")
    # #       ),
    # #       size = "l"))
    # #
    # # }) %>% bindEvent(input$regHelp_vhf)
    # #
    # # output$help_design <- renderUI({
    # #
    # #   fluidRow(
    # #     shiny::selectizeInput(
    # #       inputId = ns("which_var"),
    # #       width = "40%",
    # #       label = NULL,
    # #       choices = c("Sampling duration" = 1,
    # #                   "Sampling interval" = 2),
    # #       options = list(
    # #         placeholder = 'Select an option below',
    # #         onInitialize = I('function() { this.setValue(""); }'))
    # #     ),
    # #
    # #     ggiraph::girafeOutput(
    # #       outputId = ns("regPlot_tradeoffs"), width = "90%"),
    # #
    # #   ) # end of fluidRow
    # # }) # end of renderUI // help_design
    # #
    # # output$regPlot_tradeoffs <- ggiraph::renderGirafe({
    # #   req(input$which_var)
    # #
    # #   df <- movedesign::gps_tradeoffs[1:27, ]
    # #
    # #   ylim.prim <- c(0, 233)
    # #   ylim.sec <- c(0, 13733.87)
    # #
    # #   diff(ylim.prim)
    # #   diff(ylim.sec)
    # #
    # #   b <- diff(ylim.prim)/diff(ylim.sec)
    # #   a <- b*(ylim.prim[1] - ylim.sec[1])
    # #
    # #   df$DOF_speed2 <- a + df$DOF_speed*b
    # #
    # #   # Sampling duration:
    # #
    # #   if (input$which_var == 1) {
    # #
    # #     p <- ggplot2::ggplot(df) +
    # #       ggplot2::geom_point(
    # #         ggplot2::aes(x = duration, DOF_area,
    # #                      color = pal$dg),
    # #         alpha = .5) +
    # #       ggplot2::geom_point(
    # #         ggplot2::aes(x = duration,
    # #                      y = a + DOF_speed * b,
    # #                      color = "var(--sea)"),
    # #         alpha = .5) +
    # #
    # #       ggplot2::geom_smooth(
    # #         method = 'loess', formula = 'y ~ x',
    # #         ggplot2::aes(x = duration, DOF_area,
    # #                      color = pal$dg),
    # #         span = 1, se = FALSE) +
    # #       ggplot2::geom_smooth(
    # #         method = 'loess', formula = 'y ~ x',
    # #         ggplot2::aes(x = duration, y = a + DOF_speed * b,
    # #                      color = "var(--sea)"),
    # #         span = 0.6, se = FALSE) +
    # #       ggplot2::scale_y_continuous(
    # #         expression(N[area]),
    # #         sec.axis = ggplot2::sec_axis(
    # #           ~ (. - a)/b,
    # #           name = expression(N[speed]))) +
    # #       ggplot2::scale_x_log10(
    # #         "Sampling duration (in months)",
    # #         expand = c(0,0.01)) +
    # #       ggplot2::scale_color_manual(
    # #         labels = c(expression(N[speed]),
    # #                    expression(N[area])),
    # #         values = c("var(--sea)", pal$dg)) +
    # #       theme_movedesign() +
    # #       ggplot2::theme(
    # #         legend.position = c(0.15,0.25),
    # #         legend.title = ggplot2::element_blank(),
    # #         legend.text = ggplot2::element_text(size = 13),
    # #         legend.key.size = ggplot2::unit(0.5, "cm"),
    # #         legend.key.width = ggplot2::unit(0.5, "cm"),
    # #         legend.background = ggplot2::element_blank(),
    # #         legend.key = ggplot2::element_blank(),
    # #         legend.text.align = 0)
    # #
    # #     p_end <- ggiraph::girafe(
    # #       ggobj = p,
    # #       width_svg = 6, height_svg = 4,
    # #       options = list(
    # #         ggiraph::opts_toolbar(saveaspng = FALSE)))
    # #
    # #   }
    # #
    # #   # Sampling interval:
    # #
    # #   if (input$which_var == 2) {
    # #
    # #     p <- ggplot2::ggplot(df) +
    # #       ggplot2::geom_point(
    # #         ggplot2::aes(x = frequency, DOF_area,
    # #                      color = pal$dg),
    # #         alpha = .5) +
    # #       ggplot2::geom_point(
    # #         ggplot2::aes(x = frequency,  a + DOF_area * b,
    # #                      color = "var(--sea)"),
    # #         alpha = .5) +
    # #
    # #       ggplot2::geom_smooth(
    # #         method = 'loess', formula = 'y ~ x',
    # #         ggplot2::aes(x = frequency, DOF_area,
    # #                      color = pal$dg),
    # #         span = 1, se = FALSE) +
    # #       ggplot2::geom_smooth(
    # #         method = 'loess', formula = 'y ~ x',
    # #         ggplot2::aes(x = frequency, y = a + DOF_speed * b,
    # #                      color = "var(--sea)"),
    # #         span = 0.6, se = FALSE) +
    # #       ggplot2::scale_y_continuous(
    # #         expression(N[area]),
    # #         sec.axis = ggplot2::sec_axis(
    # #           ~ (. - a)/b,
    # #           name = expression(N[speed]))) +
    # #       ggplot2::scale_x_log10(
    # #         "Sampling frequency (fixes/hour)",
    # #         expand = c(0,0.01)) +
    # #       ggplot2::scale_color_manual(
    # #         labels = c(expression(N[speed]),
    # #                    expression(N[area])),
    # #         values = c("var(--sea)", pal$dg)) +
    # #       theme_movedesign() +
    # #       ggplot2::theme(
    # #         legend.position = c(0.15,0.25),
    # #         legend.title = ggplot2::element_blank(),
    # #         legend.text = ggplot2::element_text(size = 13),
    # #         legend.key.size = ggplot2::unit(0.5, "cm"),
    # #         legend.key.width = ggplot2::unit(0.5, "cm"),
    # #         legend.background = ggplot2::element_blank(),
    # #         legend.key = ggplot2::element_blank(),
    # #         legend.text.align = 0)
    # #
    # #     p_end <- ggiraph::girafe(
    # #       ggobj = p,
    # #       width_svg = 6, height_svg = 4,
    # #       options = list(
    # #         ggiraph::opts_toolbar(saveaspng = FALSE)))
    # #   }
    # #
    # #   return(p_end)
    # # }) # end of renderGirafe // regPlot_tradeoffs

    ## Additional information: --------------------------------------------

    # Save information for report if table is not requested:

    observe({
      req(vals$active_tab == 'regime',
          vals$device_n, vals$device_N1, vals$device_N2)

      req(is.null(vals$dt_regs))
      vals$report_regs_yn <- FALSE
      vals$dt_regs <- regRow()

    })

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_device_ui("tab_device_1")

## To be copied in the server
# mod_tab_device_server("tab_device_1")
