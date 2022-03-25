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

        title = span("Tracking regime:", style =
                       paste("padding-top: 14px;", ttl_main)),
        icon = fontawesome::fa(name = "stopwatch",
                               height = "22px",
                               margin_left = "14px",
                               margin_right = "8px",
                               fill = "#e3e3e3"),

        id = ns("reg_intro"),
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = TRUE, closable = FALSE,

        column(
          align = "center", width = 12,

          p("Approppriate study design is dependent on",
            "the", span("tracking device type", style = txt_key),
            "(and, ultimately, its",
            HTML(paste0(span("battery life", style = txt_border),
                        ")")), "and on two sampling parameters:",
            span("study duration", style = txt_caution),
            "(how long to track each individual for), and",
            span("sampling interval", style = txt_caution),
            "(time between which new locations are collected).",
            "Weight limits imposed on tracking devices",
            "(e.g., 5% rule) also impact battery life."
          ),

          p("Which type are you evaluating?",
            style = txt_label_bold),

          shiny::selectizeInput(
            inputId = ns("device_type"),
            width = "260px",
            label = NULL,
            choices = c("Option 1. GPS/Satellite logger" = 1,
                        "Option 2. VHF transmitter" = 2),
            options = list(
              placeholder = 'Select an option here',
              onInitialize = I('function() { this.setValue(""); }'))),

          # p(style = txt_label_bold,
          #   "Do you want to consider weight limitations?"),
          #
          # shinyWidgets::switchInput(
          #   inputId = ns("weight"),
          #   onLabel = "Yes",
          #   offLabel = "No",
          #   labelWidth = "25px"),
          # br()

        ), # end of column (text)

        uiOutput(ns("regText_intro"))

      ), # end of box // device_intro

      # [right column] ----------------------------------------------------

      div(class = div1_column_right,

          ## SETTINGS -----------------------------------------------------
          ### Device settings: --------------------------------------------

          shinydashboardPlus::box(
            title = span("Device settings", style = ttl_box.solid),
            id = ns("regBox_device"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            column(
              width = 12, align = "left",
              uiOutput(ns("regUI_device")))

          ), # end of box // regBox_device

          uiOutput(ns("regBox_pars")),

          ### Other settings: ---------------------------------------------

          shinydashboardPlus::box(
            id = ns("regBox_loss"),
            width = NULL,
            headerBorder = FALSE,

            shinyWidgets::sliderTextInput(
              inputId = ns("deviceInput_loss"),
              label = "Simulate % data loss:",
              choices = c(0, 5, seq(10, 100, by = 10)),
              from_min = 0, from_max = 90, selected = 0,
              grid = FALSE,
              post = "%",
              width = "100%") %>%
              help_modal(file = "modal_dataloss")

          ), # end of box // regBox_loss

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
              selected = 1)

          ) # end of box // regBox_submit

      ), # end of column (right)

      # [center column] ---------------------------------------------------

      div(class = div1_column_left,

          # Specify tracking regime parameters: ---------------------------

          shinydashboardPlus::box(
            title = span("Specify sampling parameters:", style = ttl_box),
            id = ns("regBox_sampling"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,

            uiOutput(ns("regUI_sampling")),

            footer = uiOutput(ns("regUI_sampling_footer"))

          ), # end of box // regBox_gpsViz

          shinydashboardPlus::box(
            title = span("Visualizing new simulated data:",
                         style = ttl_box),
            id = ns("regBox_sims"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,

            tabsetPanel(
              id = ns("regTabs_sims"),

              tabPanel(
                value = ns("regPanel_id"),
                title = tagList(
                  fontawesome::fa(name = "map-marker-alt",
                                  fill = hex_border),
                  span("Data", style = ttl_panel)),

                br(),

                ggiraph::girafeOutput(
                  outputId = ns("regPlot_id"),
                  width = "95%", height = "100%") %>%
                  shinycssloaders::withSpinner(
                    type = getOption("spinner.type", default = 7),
                    color = getOption("spinner.color",
                                      default = "#f4f4f4")),

                column(
                  width = 12, align = "center",
                  uiOutput(ns("simsInput_subset"))
                )

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("newsimsPanel_svf"),
                title = tagList(
                  fontawesome::fa(name = "chart-line",
                                  fill = hex_border),
                  span("Variogram", style = ttl_panel)
                ),
                br(),

                ggiraph::girafeOutput(
                  outputId = ns("regPlot_svf"),
                  width = "95%", height = "100%"),

                column(
                  width = 12, align = "center",
                  shiny::sliderInput(
                    ns("regVar_timeframe"),
                    label = span(paste("Proportion of the",
                                       "variogram plotted (in %):"),
                                 style = txt_label_bold),
                    min = 0, max = 100, value = 65, step = 5,
                    width = "85%")
                ) # end of column

              ) # end of panels (3 out of 3)
            ) # end of tabs

          ), # end of box // regBox_sims

      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          # Sample sizes: -------------------------------------------------

          shinydashboardPlus::box(
            title = span("Displaying sample sizes:", style = ttl_box),
            id = ns("regBox_sizes"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE, closable = FALSE,

            column(
              align = "center", width = 12,
              uiOutput(ns("regText_sizes"))
            ), # end of column (for text)

            fluidRow(
              column(width = 4, uiOutput(ns("regBlock_n"))),
              column(width = 4, uiOutput(ns("regBlock_Narea"))),
              column(width = 4, uiOutput(ns("regBlock_Nspeed"))),
            ), # end of fluidRow

            br(),
            footer = fluidRow(column(
              width = 12, align = "center",

              splitLayout(
                cellWidths = c("60%", "40%"),
                cellArgs = list(style = "align: right;"),

                br(),
                shiny::actionButton(
                  inputId = ns("regButton_save"),
                  label = span("Add to",
                               span("table", style = col_border),
                               "below"),
                  icon = icon("bookmark"),
                  width = "90%", height = "90%")

              ) # end of splitLayout
            )) # end of footer

          ), # end of box // regBox_sizes

          # Table: --------------------------------------------------------

          shinydashboardPlus::box(
            title = span("Summary table:", style = ttl_box),
            id = ns("regBox_summary"),
            width = NULL,
            solidHeader = FALSE,

            DT::dataTableOutput(ns("regTable")),
            br(),
            div(style = "display:inline-block; float:right",
                shiny::actionButton(
                  inputId = ns("regTable_clear"),
                  label = "Clear table",
                  icon =  icon("trash"),
                  width = "110px")), br()

          ), # end of box // regBox_summary

          # Additional information: ---------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", style = ttl_box),
            id = ns("regBox_misc"),
            width = NULL, solidHeader = FALSE,

            verbatimTextOutput(ns("console_device"))

          ) # end of box // regBox_misc
      ) # end of column (bottom)

    ), # end of fluidRow

    # MODALS: -------------------------------------------------------------

    modal_dataloss,
    NULL

  ) # end of tagList
}

#' tab_device Server Functions
#'
#' @noRd
mod_tab_device_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # DYNAMIC UI ELEMENTS -----------------------------------------------
    ## Hide all boxes at start: -----------------------------------------

    boxnames <- c("device",
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

    ## Add species & device parameters box: ----------------------------

    output$regBox_pars <- renderUI({
      req(vals$tau_p0, vals$tau_v0)

      ### Species & study parameters:

      shinydashboard::tabBox(
        id = ns("regTabs_pars"),
        width = NULL,

        tabPanel(title = "Species",
                 value = ns("regPanel_species"),
                 icon = icon("paw"),

                 uiOutput(ns("regBlock_tau_p")),
                 uiOutput(ns("regBlock_tau_v"))

        ), # end of panel (1 out of 2)

        tabPanel(title = "Device",
                 value = ns("regPanel_device"),
                 icon = icon("map-marker-alt"),

                 uiOutput(ns("regUI_regime"))

        ) # end of panel (2 out of 2)

      ) # end of tabBox
    }) # end of renderUI // regBox_pars

    ## Adding maximum GPS fix rate input: -------------------------------

    output$gpsInput_maxrate <- renderUI({

      rate_choices <- gps_fixrate %>%
        dplyr::filter(choices == "Y") %>%
        dplyr::pull(nu_notes)

      shinyWidgets::pickerInput(
        inputId = ns("gps_maxrate"),
        label = "GPS fix rate (max):",
        choices = rate_choices,
        selected = "1 fix every 8 hours") %>%
        help_tip(text = paste("Maximum sampling interval",
                              "(time between two fixes)",
                              "available at the duration above."),
                 placement = "bottom")

    }) # end of renderUI // gpsInput_maxrate

    ## Select fix rate inputs: ------------------------------------------

    output$gpsSelect_fix <- renderUI({
      req(df_decay())

      dti_choices <- df_decay() %>%
        dplyr::filter(dur > 0) %>%
        dplyr::pull(nu_notes)

      shinyWidgets::pickerInput(
        inputId = ns("gps_selected_dti"),
        label = NULL,
        choices = dti_choices,
        options = list(title = "Select regime"))

    }) # end of renderUI // gpsSelect_fix

    output$vhfSelect_fix <- renderUI({

      splitLayout(
        cellWidths = c("75px", "180px"),
        cellArgs = list(style = 'align: center;'),

        shiny::numericInput(
          ns('vhf_dti'),
          label = NULL,
          min = 1, value = 1),

        shiny::selectInput(
          ns("vhf_dti_units"),
          label = NULL,
          choices = c("Day(s)" = "days",
                      "Hour(s)" = "hours",
                      "Minute(s)" = "minutes"),
          selected = "hours")

      ) # end of splitLayout
    }) # end of renderUI // vhfSelect_fix

    output$vhfText_regime <- renderUI({
      req(input$vhf_dur,
          input$vhf_dti)

      tmpunits <- fix_timeunits(input$vhf_dti,
                                input$vhf_dti_units)[2]

      dur0 <- input$vhf_dur %#% input$vhf_dur_units
      dur0_day <- "days" %#% dur0
      dur0_mth <- "months" %#% dur0

      if(dur0_day == 1) {
        tmp <- "1 day"
      } else {
        tmp <- paste(round(dur0_day, 0), "days")
      }

      p(style = paste(ft_center), # "padding: 0 50px 0 50px"),

        "This tracking regime is equal to a new location every",
        span(round(input$vhf_dti, 1), tmpunits, style = txt_border),
        "for a duration of",
        span(paste0(round(dur0_mth, 1),
                    " months"), style = txt_border), "(or",
        "approximately", HTML(paste0(span(
          tmp, style = txt_border), ")."))
      )

    }) # end of renderUI

    ## Changing device settings: ----------------------------------------

    observe({
      req(input$device_type)
      shinyjs::show(id = "regBox_device")
      shinyjs::show(id = "regBox_sampling")

      if(input$device_type == 1) {
        vals$device_type <- "GPS"
      } else if(input$device_type == 2) {
        vals$device_type <- "VHF"
      }

    }) # end of observe

    output$regUI_device <- renderUI({
      req(input$device_type)

      shinyjs::show(id = "regTabs_pars")
      updateTabsetPanel(
        session,
        inputId = "regTabs_pars",
        selected = "tab_tradeoff_1-regPanel_species")

      if(input$device_type == 1) {

        ### GPS & Satellite logger:

        fluidRow(
          p(HTML("&nbsp;"), "GPS battery life (max):",
            style = txt_label),

          splitLayout(
            cellWidths = c("35%", "65%"),

            shiny::numericInput(
              inputId = ns("gps_dur"),
              label = NULL,
              min = 1, max = 48,
              value = 48),

            shiny::selectInput(
              inputId = ns("gps_dur_units"),
              label = NULL,
              choices = c("Days" = "days",
                          "Months" = "months",
                          "Years" = "years"),
              selected = "months")

          ), # end of splitLayout

          uiOutput(ns("gpsInput_maxrate")),

          shiny::sliderInput(
            inputId = ns("gps_k0"),
            label = "Decay rate:",
            min = 0.01, max = 5, value = 0.8,
            ticks = FALSE)

        ) # end of fluidRow

      } else {

        ### VHF transmitter:

        fluidRow(
          p(HTML("&nbsp;"), "VHF battery life (max):",
            style = paste0(ft, col_main,
                           "font-size: 14px;",
                           "letter-spacing: 0.5px;",
                           "margin: 0 0 5px -7px;")),

          splitLayout(
            cellWidths = c("35%", "65%"),

            shiny::numericInput(
              inputId = ns("vhf_dur"),
              label = NULL,
              min = 1, value = 24),

            shiny::selectInput(
              inputId = ns("vhf_dur_units"),
              label = NULL,
              choices = c("Days" = "days",
                          "Months" = "months",
                          "Years" = "years"),
              selected = "months")

          ) # end of splitLayout

          # shinyWidgets::sliderTextInput(
          #   inputId = ns("vhf_ppm"),
          #   label = "Pulses per minute (ppm):",
          #   choices = c(15, 25, 30, 35, 40, 55),
          #   grid = TRUE)

        ) # end of fluidRow

      } # end of if(), VHF device
    }) # end of renderUI // regUI_device

    ## Changing sampling parameters: ------------------------------------

    observe({

      output$regUI_sampling <- renderUI({
        if(input$device_type == 1) {

          ### GPS & Satellite logger:

          column(
            align = "center", width = 12,

            p("Here, you can vizualize the tradeoff between",
              span("sampling duration", style = txt_caution),
              "and", span("frequency", style = txt_caution),
              "for the selected", span("GPS", style = txt_key),
              "settings, and their impact on",
              HTML(paste0(span("sample sizes",
                               style = txt_border), "."))),

            # Select tracking regime:

            p("What sampling interval will you evaluate?",
              style = txt_label_bold),

            uiOutput(ns("gpsSelect_fix")),

            p("Please select a", span("fix rate", style = txt_border),
              "from the input above (or from a",
              span("point", style = txt_key),
              "in the plot below) to further evaluate that",
              HTML(paste0(span("tracking regime",
                               style = txt_border), "."))),

            # Plotting GPS battery life decay:

            ggiraph::girafeOutput(
              outputId = ns("regPlot_decay"),
              width = "100%", height = "50%"),

            shinyWidgets::switchInput(
              inputId = ns("deviceInput_log"),
              label = span(icon("wrench"),
                           "Logarithmic"),
              labelWidth = "100px")

          ) # end of column (UI)

        } else {

          ### VHF transmitter:

          column(
            align = "center", width = 12,

            p("Here, you can vizualize the impact of different",
              "tracking regimes on",
              span("sample sizes", style = txt_border),
              "by selecting the",
              span("study duration", style = txt_caution),
              "(based on VHF battery life), and",
              span("sampling frequency", style = txt_caution),
              "(based on how many times you will collect new",
              "locations)."),

            p("What sampling interval will you evaluate?",
              style = txt_label_bold),

            uiOutput(ns("vhfSelect_fix")),
            uiOutput(ns("vhfText_regime"))

          ) # end of column (UI)
        } # end of if(), VHF device

      }) # end of renderUI // regUI_device

    }) %>% # end of observe
      bindEvent(input$device_type)

    observe({ # Add footer to sampling box:

      output$regUI_sampling_footer <- renderUI({
        if(input$device_type == 1) {

          ### GPS & Satellite logger:

          column(
            width = 12, align = "right",

            splitLayout(
              cellWidths = c("50px", "25%", "30%", "30%"),
              cellArgs = list(style = paste("align: right;",
                                            "padding: 0 0 0 5px;")),

              shiny::actionButton(
                inputId = ns("regHelp_gps"),
                label = NULL,
                width = "100%",
                icon = icon("question-circle"),
                class = "btn-warning"),
              br(),
              shiny::actionButton(
                inputId = ns("validate_gps"),
                icon =  icon("check-circle"),
                label = "Validate",
                width = "100%",
                class = "btn-info"),
              shiny::actionButton(
                inputId = ns("run_sim_new"),
                icon =  icon("bolt"),
                label = "Run",
                width = "100%",
                class = "btn-primary")

            ) # end of splitLayout
          ) # end of column (footer)

        } else {

          ### VHF transmitter:

          column(
            width = 12, align = "center",

            splitLayout(
              cellWidths = c("50px", "25%", "30%", "30%"),
              cellArgs = list(style = paste("align: right;",
                                            "padding: 0 0 0 5px;")),

              shiny::actionButton(
                inputId = ns("regHelp_vhf"),
                label = NULL,
                width = "100%",
                icon = icon("question-circle"),
                class = "btn-warning"),
              br(),
              shiny::actionButton(
                inputId = ns("validate_vhf"),
                label = "Validate",
                icon =  icon("check-circle"),
                width = "100%",
                class = "btn-info"),
              shiny::actionButton(
                inputId = ns("run_sim_new"),
                icon =  icon("bolt"),
                label = "Run",
                width = "100%",
                class = "btn-primary")

            ) # end of splitLayout
          ) # end of column (footer)

        } # end of if(), VHF device
      }) # end of renderUI // regUI_device

    }) %>% # end of observe
      bindEvent(input$device_type, ignoreInit = TRUE)

    ## Changing regime: -------------------------------------------------

    observe({
      req(isTruthy(input$validate_vhf) || isTruthy(input$validate_gps))

      if(is.null(input$regPlot_decay_selected) &&
         input$device_type == 1 && input$validate_gps) {

        shinyalert::shinyalert(
          title = "No regime selected",
          text = span(
            "Please select a fix rate",
            "to set a",
            HTML(paste0(span("tracking regime",
                             style = txt_key), ",")),
            'then click the',
            fontawesome::fa(name = "bolt", fill = hex_border),
            span('Validate', style = col_border),
            'button again.'),
          html = TRUE,
          size = "xs")

      } else {
        updateTabsetPanel(
          session,
          inputId = "regTabs_pars",
          selected = "tab_tradeoff_1-regPanel_device")

        output$regUI_regime <- renderUI({
          fluidRow(
            column(width = 12, uiOutput(ns("regBlock_dur0_dev"))),
            column(width = 12, uiOutput(ns("regBlock_dti0_dev"))))
        }) # end of renderUI // regUI_regime

        shinyjs::show(id = "regBox_type")
        shinyjs::show(id = "regBox_loss")

      } # end of if()
    }) # end of observer
    # %>% bindEvent(to_validate(), ignoreInit = TRUE)

    ## Change sample size text: -----------------------------------------

    observe({
      if(input$est_type == 1) {

        temp_text <- p(
          style = ft_center,

          "As", span("effective sample sizes", style = txt_border),
          "are", HTML(paste0(
            span("roughly estimated", style = txt_caution), ",")),
          "these values will update before validation.",
          "However, they also correspond to a best-case scenario",
          "(real sample sizes may be lower).")

      } else {

        vals$device_n <- NULL
        vals$device_N1 <- NULL
        vals$device_N2 <- NULL

        temp_text <- p(
          style = ft_center,

          "As", span("effective sample sizes", style = txt_border),
          "are extracted from the",
          HTML(paste0(
            span("model fit",
                 style = col_caution), ",")),
          "these values will", span("not", style = "color: #000;"),
          "update automatically.", br(), "Click the",
          fontawesome::fa(name = "bolt", fill = "#dd4b39"),
          HTML(paste0(span("Run", style = btn_danger))),
          "button to refresh.")
      }

      output$regText_sizes <- renderUI({temp_text})

    }) %>% # end of observe
      bindEvent(input$est_type)

    ## Add species box (if data available): -----------------------------

    observe({
      if(!is.null(vals$tau_p0)) {
        shinyjs::show(id = "regBox_species")
      } else {
        shinyjs::hide(id = "regBox_species")
      }
    }) # end of observe

    ## Add summary box (if data available): -----------------------------

    observe({
      if(!is.null(vals$df_regs)) {
        shinyjs::show(id = "regBox_summary")
      } else {
        shinyjs::hide(id = "regBox_summary")
      }
    }) # end of observe

    # PARAMETER CALCULATIONS --------------------------------------------
    ## Device — sampling duration & interval: ---------------------------

    observe({
      vals$dur0_dev <- NULL
      vals$dti0_dev <- NULL

      if(input$device_type == 1) {
        req(input$regPlot_decay_selected)

        df0 <- df_decay()
        dur_dev <- df0[reg_selected(), ]$dur
        dti_dev <- df0[reg_selected(), ]$nu

        vals$dur0_dev <- round(dur_dev %#% "months", 0)
        vals$dur0_units_dev <- "months"
        vals$dti0_dev <- round(dti_dev, 0)

        tmpdti_notes <- df0[reg_selected(), ]$nu_notes
        tmpdti_units <- sub('^.* ([[:alnum:]]+)$',
                            '\\1', tmpdti_notes)
        vals$dti0_units_dev <- tmpdti_units

      } else if(input$device_type == 2) {
        req(input$vhf_dur, input$vhf_dti)

        dur_dev <- input$vhf_dur %#% input$vhf_dur_units
        dti_dev <-  input$vhf_dti %#% input$vhf_dti_units

        vals$dur0_dev <- round(dur_dev, 0)
        vals$dur0_units_dev <- input$vhf_dur_units
        vals$dti0_dev <- round(dti_dev, 0)
        vals$dti0_units_dev <- input$vhf_dti_units

      } # end of if()
    }) # end of observe

    ## Device — sample sizes: -------------------------------------------

    observe({
      req(vals$tau_p0, vals$tau_v0,
          vals$dur0_dev, vals$dti0_dev)

      shinyjs::show(id = "regBox_sizes")

      n <- NULL
      N1 <- NULL
      N2 <- NULL

      tauv <- vals$tau_v0 %#% vals$tau_v0_units
      taup <- vals$tau_p0 %#% vals$tau_p0_units

      dur <- vals$dur0_dev
      dti <- vals$dti0_dev

      t0 <- seq(1, dur, by = dti)
      n <- length(t0)
      n_loss <- round(n * (input$deviceInput_loss/100), 0)
      nlost <- n_loss
      r <- dti / tauv

      if(input$est_type == 1) {
        vals$is_fitted <- "No"

        N1 <- dur / taup
        N2 <- ifelse(dti > tauv,
                     ifelse(dti > 10 * tauv, 0,
                            (n - n_loss)/(r * (r/3))),
                     (n - n_loss) / tauv * dti)

        if(N1 > (n - n_loss)) { N1 <- n }

      } else if(input$est_type == 2) {

        vals$device_n_fit <- nrow(vals$data1)

        req(vals$newfit)
        vals$is_fitted <- "Yes"

        tmpnames <- names(summary(vals$newfit)$DOF)
        N1 <- summary(vals$newfit)$DOF[grep('area', tmpnames)][[1]]
        N2 <- summary(vals$newfit)$DOF[grep('speed', tmpnames)][[1]]
      }

      vals$device_n <- n - n_loss
      vals$device_nlost <- nlost
      vals$device_N1 <- N1
      vals$device_N2 <- N2

    }) # end of observe

    # SIMULATIONS -------------------------------------------------------
    ## Simulating GPS battery life: -------------------------------------

    df_decay <- shiny::reactive({
      req(input$gps_k0,
          input$gps_dur,
          input$gps_maxrate)

      df_decay <- simulate_gpsdecay(
        data = gps_fixrate,
        k0 = input$gps_k0,
        yrange0 = input$gps_dur,
        subset = 0.164383,
        minrate = input$gps_maxrate)

      max_freq <- df_decay %>%
        dplyr::filter(color == "red") %>%
        dplyr::pull(freq_hrs) %>%
        min()

      # Display only values with duration
      # (plus three additional rows):

      tmpdf <- df_decay %>%
        dplyr::filter(freq_hrs >= 0) %>%
        dplyr::filter(freq_hrs < max_freq)
      nrow(tmpdf)

      if(nrow(tmpdf) + 3 <= nrow(df_decay) - 3) {
        df_decay <- df_decay[1:(nrow(tmpdf) + 3),]
      } else {
        df_decay <- df_decay[1:nrow(tmpdf),]
      }

      vals$gpsdecay <- df_decay
      return(df_decay)

    }) # end of reactive

    ## Simulating new conditional data: ---------------------------------

    data_sim <- shiny::reactive({

      df0 <- ctmm::simulate(
        vals$data0,
        vals$fit,
        t = seq(0, vals$dur0_dev, by = vals$dti0_dev)) %>%
        ctmm:::pseudonymize()

      return(df0)

    }) %>% # end of reactive
      bindEvent(c(vals$dur0_dev, vals$dti0_dev))

    observe({
      shinyjs::show(id = "regBox_sims")

      start <- Sys.time()
      shiny::withProgress({
        vals$data1 <- data_sim()
      },
      message = "Simulating new tracking regime.",
      detail = "This may take a while...")
      vals$needs_fit <- TRUE

      msg_log(
        style = "success",
        message = paste0("Simulation ",
                         msg_success("completed"), "."),
        detail = paste(
          "This step took approximately",
          round(difftime(Sys.time(), start,
                         units = 'min'), 1),
          "minutes."))

      ### Run model fit (if set):

      if(input$est_type == 1) {
        vals$needs_fit <- TRUE

      } else {

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
          fit0 <- ctmm::ctmm.fit(vals$data1, newmod)
        },
        message = "Fitting movement model.",
        detail = "This may take a while...")

        msg_log(
          style = "warning",
          message = paste0("...", msg_warning("Fitting"),
                           " movement model."),
          detail = "Please wait for model fit to finish.")

        vals$time_sims <- difftime(Sys.time(), start,
                                   units = "mins")
        msg_log(
          style = 'success',
          message = paste0("Model fit ",
                           msg_success("completed"), "."),
          detail = paste(
            "This step took approximately",
            round(vals$time_sims, 1), "minutes."))

        vals$guess <- NULL
        vals$newfit <- fit0
        vals$needs_fit <- FALSE

      } # end of if(), est_type

      # if(!input$regBox_sampling$collapsed &&
      #    vals$tour_active) { NULL } else {
      #      shinydashboardPlus::updateBox(
      #        "regBox_sampling", action = "toggle") }


    }) %>% # end of observe,
      bindEvent(input$run_sim_new)

    # PLOTS -------------------------------------------------------------
    ## Plotting GPS battery decay: --------------------------------------

    reg_selected <- reactive({
      input$regPlot_decay_selected
    })

    output$regPlot_decay <- ggiraph::renderGirafe({
      req(df_decay())

      df0 <- df_decay()

      if(input$deviceInput_log == TRUE) {
        df0$x <- log(vals$gpsdecay$freq_hrs)
      } else {
        df0$x <- vals$gpsdecay$freq_hrs
      }

      p <- ggplot2::ggplot(
        df0, ggplot2::aes(x = x,
                          y = dur,
                          col = color,
                          tooltip = nu_notes,
                          data_id = as.numeric(id))) +
        ggplot2::geom_smooth(
          method = minpack.lm::nlsLM,
          formula = "y ~ yrange0*exp(-k0*x)",
          method.args = list(
            start = c(yrange0 = input$gps_dur,
                      k0 = input$k0)),
          se = F, color = "#f4f4f4",
          size = 2, alpha = .8) +

        ggiraph::geom_point_interactive(size = 2) +
        ggplot2::scale_color_manual(
          values = c(hex_main, hex_caution)) +

        ggplot2::labs(x = "Frequency (fixes per hour)",
                      y = "Durations (in months)") +
        theme_movedesign() +
        ggplot2::theme(legend.position = "none")

      if(!is.null(input$gps_selected_dti)) {
        tempid <- match(input$gps_selected_dti, df0$nu_notes)
        preselection <- as.character(tempid)
      } else {
        preselection <- character(0)
      }

      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 5,
        options = list(
          ggiraph::opts_hover(
            css = paste("r:5pt;",
                        "fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_selection(
            selected = preselection,
            type = "single",
            css = paste("r:5pt;",
                        "fill:#dd4b39;",
                        "stroke:#eb5644;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))

    }) # end of renderGirafe // regPlot_decay

    ## Rendering new simulated data plot (xy): --------------------------

    output$regPlot_id <- ggiraph::renderGirafe({
      req(vals$data0, vals$data1)

      if(vals$data_type == "simulated") {
        dat <- vals$data0[which(vals$data0$t <= max(vals$data1$t)), ]
      } else {
        dat <- ctmm::simulate(
          vals$data1,
          vals$fit,
          dt = 20 %#% "seconds")
      }
      newdat <- vals$data1

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
          size = 0.8) +

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

    }) # end of renderGirafe // regPlot_id

    ## Rendering variogram (svf): ---------------------------------------

    output$regPlot_svf <- ggiraph::renderGirafe({
      req(vals$data1)

      frac <- input$regVar_timeframe / 100
      svf <- prepare_svf(vals$data1, fraction = frac)
      p <- plotting_svf(svf)

      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 4,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .1),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))

    }) # end of renderGirafe // regPlot_svf

    # TABLES ------------------------------------------------------------
    ## Listing multiple tracking regimes: -------------------------------

    observe({
      simrow <- data.frame(
        device = NA,
        taup = NA,
        tauv = NA,
        dur = NA,
        dti = NA,
        n = vals$device_n,
        N1 = vals$device_N1,
        N2 = vals$device_N2,
        fit = vals$is_fitted)

      simrow$device <- vals$device_type

      out_tp <- fix_timeunits(vals$tau_p0, vals$tau_p0_units)
      out_tv <- fix_timeunits(vals$tau_v0, vals$tau_v0_units)
      simrow$taup <- paste(out_tp[1], out_tp[2])
      simrow$tauv <- paste(out_tv[1], out_tv[2])

      out_dur <- fix_timeunits(vals$dur0_units_dev %#%
                                 vals$dur0_dev,
                               vals$dur0_units_dev)
      out_dti <- fix_timeunits(vals$dti0_units_dev %#%
                                 vals$dti0_dev,
                               vals$dti0_units_dev)

      simrow$dur <- paste(out_dur[1], out_dur[2])
      simrow$dti <- paste(out_dti[1], out_dti[2])

      vals$df_regs <<- rbind(vals$df_regs, simrow)
      shinyjs::disable("simsButton_save")

    }) %>% # end of observe
      bindEvent(input$regButton_save)

    output$regTable <- DT::renderDataTable({
      req(vals$df_regs)

      columnNames <- list(
        device = "Type",
        taup = paste0("\u03C4","\u209A"),
        tauv = paste0("\u03C4","\u1D65"),
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = paste0("N", tags$sub("area")),
        N2 = paste0("N", tags$sub("speed")),
        fit = "Fitted?")

      DT::datatable(
        data = vals$df_regs,
        colnames = as.vector(unlist(columnNames)),
        rownames = FALSE,
        escape = FALSE,
        # extensions = c("RowGroup", "Buttons"),
        # callback = htmlwidgets::JS(paste0(
        #   "table.rowGroup().",
        #   ifelse(input$regTable_group,
        #          "enable()", "disable()"),
        #   ".draw();")),
        options = list(
          paging = F, dom = "t",
          rowGroup = list(dataSrc = 1),
          columnDefs = list(list(className = 'dt-center',
                                 targets = 7)))) %>%
        DT::formatStyle(
          columns = c("n", "N1", "N2"),
          fontWeight = "bold",
          color = DT::styleInterval(
            c(5, 30),
            c(hex_caution, "#ffbf00", hex_main))
        ) %>%
        DT::formatCurrency(
          columns = c("n", "N1", "N2"),
          currency = "",
          digits = 0
        )

    }) # end of renderDataTable // regTable

    observe({
      vals$df_regs <- NULL
    }) %>% # end of observe,
      bindEvent(input$regTable_clear)

    # BLOCKS ------------------------------------------------------------
    ## Species parameters: ----------------------------------------------

    output$regBlock_tau_p <- renderUI({
      req(vals$tau_p0, vals$tau_v0)

      taup <- fix_timeunits(vals$tau_p0, vals$tau_p0_units)

      if(vals$data_type == "simulated") {
        tmprange <- NULL
      } else {
        taup_min <- fix_timeunits(vals$tau_p0_min, vals$tau_p0_units)
        taup_max <- fix_timeunits(vals$tau_p0_max, vals$tau_p0_units)
        tmprange <- span(paste(taup_min[1], "—", taup_max[1]),
                         style = col_main)
      }

      parBlock(
        text = span(
          HTML(paste0("Position autocorrelation ",
                      "(\u03C4", tags$sub("p"), ")"))),
        header = span(paste(taup[1], taup[2]), style = col_main),
        number = tmprange)

    }) # end of renderUI // regBlock_tau_p

    output$regBlock_tau_v <- renderUI({
      tauv <- fix_timeunits(vals$tau_v0, vals$tau_v0_units)

      if(vals$data_type == "simulated") {
        tmprange <- NULL
      } else {
        tauv_min <- fix_timeunits(vals$tau_v0_min, vals$tau_v0_units)
        tauv_max <- fix_timeunits(vals$tau_v0_max, vals$tau_v0_units)
        tmprange <- span(paste(tauv_min[1], "—", tauv_max[1]),
                         style = col_main)
      }

      parBlock(
        text = span(
          HTML(paste0("Velocity autocorrelation ",
                      "(\u03C4", tags$sub("v"), ")"))),
        header = span(paste(tauv[1], tauv[2]), style = col_main),
        number = tmprange)

    }) # end of renderUI // regBlock_tau_v

    ## Tracking regime: -------------------------------------------------

    output$regBlock_dur0_dev <- renderUI({
      req(vals$dur0_dev)

      if(vals$dur0_dev > (1 %#% "month")) {
        tmpunits <- "months"
      } else {
        tmpunits <- "days"
      }
      tmpheader <- round(tmpunits %#% vals$dur0_dev, 1)
      out <- fix_timeunits(tmpheader, tmpunits)

      parBlock(
        text = "Sampling duration",
        header = paste(out[1], out[2]))

    }) # end of renderUI // regBlock_dur0_dev

    output$regBlock_dti0_dev <- renderUI({
      req(vals$dti0_dev, vals$dti0_units_dev)

      tmp <- vals$dti0_units_dev %#% vals$dti0_dev
      out <- fix_timeunits(tmp, vals$dti0_units_dev)

      parBlock(
        text = "Sampling interval",
        header = paste(out[1], out[2]),
        number = "between fixes")

    }) # end of renderUI // regBlock_dti0_dev

    ## Sample sizes: ----------------------------------------------------

    output$regBlock_n <- renderUI({
      req(vals$device_n)

      n <- ifelse(input$est_type == 1,
                  vals$device_n,
                  vals$device_n_fit)

      sampleBlock(
        numberIcon = FALSE,
        header = n,
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)

    }) # end of renderUI // regBlock_n

    output$regBlock_Narea <- renderUI({
      req(vals$device_N1)

      if(input$est_type == 1) {
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

      if(input$est_type == 1) {
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

    # MODALS & HELP -----------------------------------------------------

    ## GPS & Satellite logger:

    observe({

      shiny::showModal(
        shiny::modalDialog(
          title = "GPS & Satellite loggers:",

          column(
            align = "center", width = 12,

            p("With", HTML(paste0(
              span("GPS & satellite loggers", style = txt_key), ",")),
              "sampling is", span("automated", style = txt_border),
              "and conducted by satellite systems. Therefore,",
              span("sampling frequency", style = txt_caution),
              "and", span("sampling duration", style = txt_caution),
              "are both inherently linked with",
              HTML(paste0(span("GPS battery life",
                               style = txt_border), ".")),
              "This tradeoff restricts the volume of data that",
              "can be collected by GPS & satellite loggers."),

            uiOutput(ns("help_design"))

          ), # end of column (main)

          footer = tagList(
            modalButton("Dismiss")
          ),
          size = "l"))

    }) %>% bindEvent(input$regHelp_gps)

    ## VHF transmitter:

    observe({

      shiny::showModal(
        shiny::modalDialog(
          title = "VHF transmitter:",

          column(
            align = "center", width = 12,

            p("With", HTML(paste0(
              span("Very-High-Frequency (VHF) transmitters",
                   style = txt_key), ", ")),
              "the interval between new locations/fixes",
              "is subject only to the tracking regime employed",
              span("manually", style = txt_border),
              "in the study site.",
              "As such, battery life is decoupled from any set",
              HTML(paste0(span("sampling frequency",
                               style = txt_caution), ",")),
              "and is mainly linked to",
              HTML(paste0(span("sampling duration",
                               style = txt_caution), "."))),

            uiOutput(ns("help_design"))

          ), # end of column (main)

          footer = tagList(
            modalButton("Dismiss")
          ),
          size = "l"))

    }) %>% bindEvent(input$regHelp_vhf)

    output$help_design <- renderUI({

      fluidRow(
        shiny::selectizeInput(
          inputId = ns("which_var"),
          width = "40%",
          label = NULL,
          choices = c("Sampling duration" = 1,
                      "Sampling interval" = 2),
          options = list(
            placeholder = 'Select an option below',
            onInitialize = I('function() { this.setValue(""); }'))
        ),

        ggiraph::girafeOutput(
          outputId = ns("regPlot_tradeoffs"), width = "90%"),

      ) # end of fluidRow
    }) # end of renderUI // help_design

    output$regPlot_tradeoffs <- ggiraph::renderGirafe({
      req(input$which_var)

      utils::globalVariables(c("gps_tradeoffs"))
      df <- gps_tradeoffs[1:27, ]

      ylim.prim <- c(0, 233)
      ylim.sec <- c(0, 13733.87)

      diff(ylim.prim)
      diff(ylim.sec)

      b <- diff(ylim.prim)/diff(ylim.sec)
      a <- b*(ylim.prim[1] - ylim.sec[1])

      df$DOF_speed2 <- a + df$DOF_speed*b

      # Sampling duration:

      if(input$which_var == 1) {

        p <- ggplot2::ggplot(df) +
          ggplot2::geom_point(
            ggplot2::aes(x = duration, DOF_area,
                         color = hex_caution),
            alpha = .5) +
          ggplot2::geom_point(
            ggplot2::aes(x = duration,
                         y = a + DOF_speed * b,
                         color = hex_border),
            alpha = .5) +

          ggplot2::geom_smooth(
            method = 'loess', formula = 'y ~ x',
            ggplot2::aes(x = duration, DOF_area,
                         color = hex_caution),
            span = 1, se = FALSE) +
          ggplot2::geom_smooth(
            method = 'loess', formula = 'y ~ x',
            ggplot2::aes(x = duration, y = a + DOF_speed * b,
                         color = hex_border),
            span = 0.6, se = FALSE) +
          ggplot2::scale_y_continuous(
            expression(N[area]),
            sec.axis = ggplot2::sec_axis(
              ~ (. - a)/b,
              name = expression(N[speed]))) +
          ggplot2::scale_x_log10(
            "Sampling duration (in months)",
            expand = c(0,0.01)) +
          ggplot2::scale_color_manual(
            labels = c(expression(N[speed]),
                       expression(N[area])),
            values = c(hex_border, hex_caution)) +
          theme_movedesign() +
          ggplot2::theme(
            legend.position = c(0.15,0.25),
            legend.title = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = 13),
            legend.key.size = ggplot2::unit(0.5, "cm"),
            legend.key.width = ggplot2::unit(0.5, "cm"),
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.text.align = 0)

        p_end <- ggiraph::girafe(
          ggobj = p,
          width_svg = 6, height_svg = 4,
          options = list(
            ggiraph::opts_toolbar(saveaspng = FALSE)))

      }

      # Sampling interval:

      if(input$which_var == 2) {

        p <- ggplot2::ggplot(df) +
          ggplot2::geom_point(
            ggplot2::aes(x = frequency, DOF_area,
                         color = hex_caution),
            alpha = .5) +
          ggplot2::geom_point(
            ggplot2::aes(x = frequency,  a + DOF_area * b,
                         color = hex_border),
            alpha = .5) +

          ggplot2::geom_smooth(
            method = 'loess', formula = 'y ~ x',
            ggplot2::aes(x = frequency, DOF_area,
                         color = hex_caution),
            span = 1, se = FALSE) +
          ggplot2::geom_smooth(
            method = 'loess', formula = 'y ~ x',
            ggplot2::aes(x = frequency, y = a + DOF_speed * b,
                         color = hex_border),
            span = 0.6, se = FALSE) +
          ggplot2::scale_y_continuous(
            expression(N[area]),
            sec.axis = ggplot2::sec_axis(
              ~ (. - a)/b,
              name = expression(N[speed]))) +
          ggplot2::scale_x_log10(
            "Sampling frequency (fixes/hour)",
            expand = c(0,0.01)) +
          ggplot2::scale_color_manual(
            labels = c(expression(N[speed]),
                       expression(N[area])),
            values = c(hex_border, hex_caution)) +
          theme_movedesign() +
          ggplot2::theme(
            legend.position = c(0.15,0.25),
            legend.title = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = 13),
            legend.key.size = ggplot2::unit(0.5, "cm"),
            legend.key.width = ggplot2::unit(0.5, "cm"),
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.text.align = 0)

        p_end <- ggiraph::girafe(
          ggobj = p,
          width_svg = 6, height_svg = 4,
          options = list(
            ggiraph::opts_toolbar(saveaspng = FALSE)))
      }

      return(p_end)

    }) # end of renderGirafe // regPlot_tradeoffs

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_device_ui("tab_device_1")

## To be copied in the server
# mod_tab_device_server("tab_device_1")
