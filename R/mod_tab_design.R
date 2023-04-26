#' tab_design UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_design_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      # Introduction: -----------------------------------------------------
      
      shinydashboardPlus::box(
        
        title = span("Sampling design:", class = "ttl-tab"),
        icon = fontawesome::fa(name = "stopwatch",
                               height = "21px",
                               margin_left = "14px",
                               margin_right = "8px",
                               fill = "var(--sea-dark)"),
        
        id = ns("dev_intro"),
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
          
          div(id = "content_device-type",
              p("Which type are you evaluating?") %>%
                tagAppendAttributes(class = 'label_center'),
              
              shiny::selectizeInput(
                inputId = ns("device_type"),
                width = "260px",
                label = NULL,
                choices = c("GPS/Satellite logger" = "GPS",
                            "VHF transmitter" = "VHF"),
                options = list(
                  placeholder = "Select an option here",
                  onInitialize = I('function() { this.setValue(""); }')))),
          
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("which_limits"),
            label = p("What limitations do you want to consider?") %>%
              tagAppendAttributes(
                class = 'label_center no-bottom'),
            choices = c("Storage limit" = "max",
                        "Fail rate" = "loss",
                        "Location error" = "error"),
            selected = character(0),
            checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                             no = icon("remove", lib = "glyphicon"))),
          br()
          
        ) # end of column (text)
      ), # end of box // device_intro
      
      # [left column] -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-4 col-md-4 col-lg-3",
          
          ## SETTINGS -----------------------------------------------------
          ### Device settings: --------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Device settings", class = "ttl-box_solid"),
            id = ns("devBox_gps_device"),
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
                  
                  shinyWidgets::autonumericInput(
                    inputId = ns("gps_dur"),
                    label = NULL,
                    minimumValue = 0,
                    value = 12,
                    allowDecimalPadding = FALSE,
                    wheelStep = 1),
                  
                  shiny::selectInput(
                    inputId = ns("gps_dur_unit"),
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
                
                fluidRow(
                  column(width = 12,
                         verbatimTextOutput(outputId = ns("min_frq"))
                  )), p(style = "padding: 0px;"),
                
                shinyWidgets::autonumericInput(
                  inputId = ns("device_max"),
                  label = span("Device storage max:",
                               style = "align: left !important;"),
                  currencySymbol = " locations",
                  currencySymbolPlacement = "s",
                  decimalPlaces = 0,
                  minimumValue = 0,
                  value = 30000,
                  wheelStep = 1000)
                
              ) # end of fluidRow
            ), # end of column
            
            footer = div(
              style = "text-align: right;",
              
              shinyWidgets::prettyToggle(
                inputId = ns("gps_from_plot"),
                label_on =
                  span("Select from",
                       span("plot", class = "cl-sea")),
                label_off = "Set regime manually",
                value = TRUE))
            
          ), # end of box // devBox_gps_device
          
          shinydashboardPlus::box(
            title = span("Device settings", class = "ttl-box_solid"),
            id = ns("devBox_vhf_device"),
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
                    inputId = ns("vhf_dur_unit"),
                    label = NULL,
                    choices = c("Days" = "days",
                                "Months" = "months",
                                "Weeks" = "weeks",
                                "Years" = "years"),
                    selected = "months")
                  
                ) #, # end of splitLayout
                
                # shinyWidgets::sliderTextInput(
                #   inputId = ns("vhf_ppm"),
                #   label = "Pulses per minute (ppm):",
                #   choices = c(15, 20, 25, 30, 35, 40, 55, 120),
                #   selected = 40,
                #   grid = FALSE),
                
              ) # end of fluidRow
            ) #, # end of column
            
            # footer = shiny::actionButton(
            #     inputId = ns("add_vhf_point"),
            #     label = span("Add to",
            #                  span("plot", class = "cl-sea")),
            #     icon = icon("bookmark"),
            #     width = "100%") 
            
          ), # end of box // devBox_vhf_device
          
          ### Limitations: ------------------------------------------------
          
          shinydashboardPlus::box(
            id = ns("devBox_loss"),
            width = NULL,
            headerBorder = FALSE,
            
            splitLayout(
              cellWidths = c("92%", "15px"),
              
              p(HTML("&nbsp;"),
                "Simulate % data loss:") %>%
                tagAppendAttributes(class = 'label_split'),
              
              actionButton(
                inputId = ns("devHelp_loss"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_loss_device")
            ),
            
            shinyWidgets::sliderTextInput(
              inputId = ns("device_loss"),
              label = NULL,
              choices = c(0, 5, seq(10, 100, by = 10)),
              from_min = 0, from_max = 90, selected = 0,
              grid = FALSE,
              post = "%",
              width = "100%"),
            br(),
            uiOutput(ns("devBlock_loss"))
            
          ), # end of box // devBox_loss
          
          shinydashboardPlus::box(
            id = ns("devBox_error"),
            width = NULL,
            headerBorder = FALSE,
            
            splitLayout(
              cellWidths = c("92%", "15px"),
              
              p(HTML("&nbsp;"),
                "Location error:") %>%
                tagAppendAttributes(class = 'label_split'),
              
              actionButton(
                inputId = ns("devHelp_error"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_error_device")
            ),
            
            shinyWidgets::autonumericInput(
              inputId = ns("device_error"),
              label = NULL,
              currencySymbol = " meter(s)",
              currencySymbolPlacement = "s",
              decimalPlaces = 0,
              minimumValue = 0, # 1
              value = 10,
              wheelStep = 1)
            
          ), # end of box // devBox_error
          
          ### Other settings: ---------------------------------------------
          
          shinydashboardPlus::box(
            id = ns("devBox_type"),
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
            
            uiOutput(ns("devText_sizes"))
            
          ), # end of box // devBox_submit
          
          # Sample sizes: -------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Sample sizes:", class = "ttl-box"),
            id = ns("devBox_sizes"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE, closable = FALSE,
            
            fluidRow(
              column(width = 12, uiOutput(ns("devBlock_n"))),
              column(width = 12, uiOutput(ns("devBlock_Narea"))),
              column(width = 12, uiOutput(ns("devBlock_Nspeed"))),
            ), # end of fluidRow
            
            br(),
            footer = shiny::actionButton(
              inputId = ns("devButton_save"),
              label = span("Add to",
                           span("table", class = "cl-sea")),
              icon = icon("bookmark"),
              width = "100%")
            
          ) # end of box // devBox_sizes
          
      ), # end of column (right)
      
      # [center column] ---------------------------------------------------
      
      div(class = "col-xs-12 col-sm-8 col-md-8 col-lg-9",
          
          # Display species & device parameters: --------------------------
          
          div(class = "tabBox_noheaders",
              shinydashboardPlus::box(
                id = ns("devBox_pars"),
                width = NULL,
                headerBorder = FALSE,
                
                shinydashboard::tabBox(
                  id = ns("devTabs_pars"),
                  width = NULL,
                  
                  tabPanel(title = "Species",
                           value = ns("devPanel_species"),
                           icon = icon("paw"),
                           
                           splitLayout(
                             mod_blocks_ui(ns("devBlock_taup")),
                             mod_blocks_ui(ns("devBlock_tauv"))
                           )
                           
                  ), # end of panel (1 out of 2)
                  
                  tabPanel(title = "Device",
                           value = ns("devPanel_device"),
                           icon = icon("location-dot"),
                           
                           splitLayout(
                             uiOutput(ns("devBlock_dur")),
                             uiOutput(ns("devBlock_dti")))
                           
                  ) # end of panel (2 out of 2)
                  
                ) # end of tabBox
              ) # end of box // devBox_pars
          ), # end of div
          
          # Specify tracking regime parameters: ---------------------------
          
          div(class = "col-lg-6 no-padding-left",
              
              shinydashboardPlus::box(
                title = span("Sampling parameters",
                             class = "ttl-box_solid"),
                id = ns("devBox_sampling"),
                status = "primary",
                width = NULL,
                solidHeader = TRUE,
                collapsible = FALSE,
                
                uiOutput(ns("devUI_sampling")),
                
                footer = uiOutput(ns("devUI_sampling_footer"))
                
              )), # end of box // devBox_gpsViz
          
          div(class = "col-lg-6 no-padding-right",
              
              shinydashboardPlus::box(
                title = span("Visualizing new simulated data:",
                             class = "ttl-box"),
                id = ns("devBox_sims"),
                width = NULL,
                solidHeader = FALSE,
                collapsible = TRUE,
                
                tabsetPanel(
                  id = ns("devTabs_sims"),
                  
                  tabPanel(
                    value = ns("devPanel_id"),
                    title = tagList(
                      icon("location-dot", class = "cl-sea"),
                      span("Data", class = "ttl-panel")),
                    
                    br(),
                    
                    ggiraph::girafeOutput(
                      outputId = ns("devPlot_id"),
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
                      icon("chart-line", class = "cl-sea"),
                      span("Variogram", class = "ttl-panel")
                    ),
                    br(),
                    
                    ggiraph::girafeOutput(
                      outputId = ns("devPlot_svf"),
                      width = "95%", height = "100%"),
                    
                    column(
                      width = 12, align = "center",
                      shiny::sliderInput(
                        ns("dev_timeframe"),
                        label = span(
                          paste("Proportion of",
                                "variogram plotted (in %):")) %>%
                          tagAppendAttributes(class = 'label_split'),
                        min = 0, max = 100, value = 65, step = 5,
                        width = "85%")
                    ) # end of column
                    
                  ) # end of panels (3 out of 3)
                ) # end of tabs
                
              )), # end of box // devBox_sims
          
          # Table: --------------------------------------------------------
          
          div(class = "col-lg-12 no-padding",
              shinydashboardPlus::box(
                title = span("Summary table:", class = "ttl-box"),
                id = ns("devBox_summary"),
                width = NULL,
                solidHeader = FALSE,
                
                reactable::reactableOutput(ns("devTable")) #,
                # br(),
                # div(style = "display:inline-block; float:right",
                #     shiny::actionButton(
                #       inputId = ns("devTable_clear"),
                #       label = "Clear table",
                #       icon =  icon("trash"),
                #       width = "110px")), br()
                
              )) # end of box // devBox_summary
          
      ), # end of column (center)
      
      # [bottom column] ---------------------------------------------------
      
      div(class = div_column_main,
          
          # Additional information: ---------------------------------------
          
          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("devBox_misc"),
            width = NULL, solidHeader = FALSE,
            
            verbatimTextOutput(ns("console_device"))
            
          ) # end of box // devBox_misc
      ) # end of column (bottom)
      
    ), # end of fluidRow
    
    # MODALS: -------------------------------------------------------------
    
    create_modal(var = "loss", id = "device"),
    create_modal(var = "error", id = "device"),
    NULL
    
  ) # end of tagList
}
    
#' tab_design Server Functions
#'
#' @noRd
mod_tab_design_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    vals$dev <- reactiveValues(
      is_valid = FALSE,
      type = NULL,
      dur = NULL, dti = NULL,
      n = NULL, N1 = NULL, N2 = NULL,
      tbl = NULL
    )
    
    ## Sampling parameters: -----------------------------------------------
    
    ### Initial sampling parameters:
    
    observe({ # GPS duration
      req(input$gps_dur, input$gps_dur_unit)
      vals$dev$dur <- data.frame(
        value = input$gps_dur, unit = input$gps_dur_unit)
      
    }) %>% # end of observe,
      bindEvent(input$gps_dur)
    
    observe({ # GPS interval
      req(input$gps_dti, input$gps_dti_unit)
      vals$dev$dti <- data.frame(
        value = input$gps_dti, unit = input$gps_dti_unit)
      
    }) %>% # end of observe,
      bindEvent(input$gps_dti)
    
    observe({ # VHF duration
      req(input$vhf_dur, input$vhf_dur_unit)
      vals$dev$dur <- data.frame(
        value = input$vhf_dur, unit = input$vhf_dur_unit)
      
    }) %>% # end of observe,
      bindEvent(input$vhf_dur)
    
    observe({ # VHF interval
      req(input$vhf_dti, input$vhf_dti_unit)
      vals$dev$dti <- data.frame(
        value = input$vhf_dti, unit = input$vhf_dti_unit)
      
    }) %>% # end of observe,
      bindEvent(input$vhf_dti)
    
    
    ### Final sampling parameters:
    
    observe({
      req(input$device_type)
      vals$dur <- NULL
      vals$dti <- NULL
      
      # GPS:
      if (input$device_type == "GPS") {
        req(!is.null(input$gps_from_plot))
        
        if (input$gps_from_plot &&
            !is.null(input$devPlot_gps_selected)) {
          req(vals$gps,
              input$gps_dur,
              input$gps_dur_unit)
          
          validate(
            need(input$gps_dur %#%
                   input$gps_dur_unit > 2 %#% "days", 
                 "Duration cannot be less than 2 days."))
          
          gps <- vals$gps
          vals$dur <- data.frame(
            value = round("days" %#% gps[reg_selected(), ]$dur_sec, 1),
            unit = "days")
          
          tmpdti_notes <- gps[reg_selected(), ]$dti_notes
          tmpdti_unit <- sub('^.* ([[:alnum:]]+)$', '\\1', tmpdti_notes)
          vals$dti <- data.frame(
            value = tmpdti_unit %#% gps[reg_selected(), ]$dti,
            unit = tmpdti_unit)
          
        } else {
          req(vals$dev$dur, vals$dev$dti)
          
          vals$dur <- data.frame(value = vals$dev$dur$value,
                                 unit = vals$dev$dur$unit)
          vals$dti <- data.frame(value = vals$dev$dti$value,
                                 unit = vals$dev$dti$unit)
        }
        
      } else if (input$device_type == "VHF") {
        
        if (!is.null(input$devPlot_vhf_selected)) {
          req(vals$vhf,
              input$vhf_dti,
              input$vhf_dti_unit)
          
          vals$dur <- data.frame(
            value = vals$vhf[reg_selected(), ]$dur,
            unit = vals$vhf[reg_selected(), ]$dur_unit)
          
          vals$dti <- data.frame(
            value = input$vhf_dti,
            unit = input$vhf_dti_unit)
          
        } else {
          req(vals$dev$dur, vals$dev$dti)
          
          vals$dur <- data.frame(
            value = vals$dev$dur$value,
            unit = vals$dev$dur$unit)
          vals$dti <- data.frame(
            value = vals$dev$dti$value,
            unit = vals$dev$dti$unit)
        }
        
      } # end of if ()
    }) # end of observe
    
    ## Sample sizes: ------------------------------------------------------
    
    observe({
      req(vals$tau_p0, 
          vals$tau_v0,
          vals$dur,
          vals$dti,
          input$est_type)
      
      n <- NULL
      N1 <- NULL
      N2 <- NULL
      
      taup <- vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      
      dur <- vals$dur$value %#% vals$dur$unit
      dti <- vals$dti$value %#% vals$dti$unit
      
      t0 <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      n <- length(t0)
      vals$dev$n <- n
      
      if (!is.null(input$device_loss)) {
        req(input$device_loss)
        n_loss <- round(n * (input$device_loss/100), 0)
        n <- length(t0) - n_loss
        vals$lost <- data.frame(perc = input$device_loss,
                                n = n_loss)
      }
      r <- dti / tauv
      
      if (input$est_type == 1) {
        vals$is_fitted <- "No"
        
        N1 <- dur / taup
        N1 <- ifelse(N1 > n, n, N1)
        N2 <- ifelse(dti > tauv,
                     ifelse(dti > 3 * tauv, 0,
                            (n) / (r)^r * (r)),
                     (n) / tauv * dti)
        
      } else if (input$est_type == 2) {
        if (!is.null(vals$fit1)) {
          req(vals$fit1)
          vals$is_fitted <- "Yes"
          vals$dev$n <- nrow(vals$data1)
          tmpnms <- names(summary(vals$fit1)$DOF)
          N1 <- summary(vals$fit1)$DOF[grep('area', tmpnms)][[1]]
          N2 <- summary(vals$fit1)$DOF[grep('speed', tmpnms)][[1]]
        }
      }
      
      vals$dev$N1 <- N1
      vals$dev$N2 <- N2
      
    }) # end of observe
    
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

    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("devBox_", boxnames[i]))
    }
    shinyjs::hide(id = "which_limits")
    shinyjs::hide(id = "min_frq")

    ## Reveal boxes: ------------------------------------------------------

    observe({ ### Reveal species & device parameters box:
      req(vals$active_tab == 'device')

      if (!is.null(vals$tau_p0) & !is.null(vals$tau_v0)) {
        shinyjs::show(id = "devBox_pars")
      } else { shinyjs::hide(id = "devBox_pars") }

    }) # end of observe
    
    observe({ ### Reveal sample sizes box:

      if (vals$dev$is_valid) {
        shinyjs::show(id = "devBox_sizes")
      } else { shinyjs::hide(id = "devBox_sizes") }

    }) %>% # end of observe,
      bindEvent(vals$dev$is_valid)
    
    observe({ ## Reveal summary box:

      if (!is.null(vals$dev$tbl)) {
        shinyjs::show(id = "devBox_summary")
      } else { shinyjs::hide(id = "devBox_summary") }

    }) %>% # end of observe,
      bindEvent(input$devButton_save)

    ## Update device settings: --------------------------------------------
    
    observe({
      req(input$device_type)
      
      shinyjs::show(id = "devBox_sampling")
      shinyjs::show(id = "which_limits")
      
      switch(
        input$device_type,
        GPS = {
          shinyjs::show(id = "devBox_gps_device")
          shinyjs::hide(id = "devBox_vhf_device")
        },
        VHF = {
          shinyjs::show(id = "devBox_vhf_device")
          shinyjs::hide(id = "devBox_gps_device")
        },
        stop(paste0("No handler for ", input$device_type, "."))
      ) # end of switch
      
      # vals$dev$type <- input$device_type
      
    }) %>% # end of observe,
      bindEvent(input$device_type)
    
    ## Update device limitations: -----------------------------------------
    
    observe({
      req(input$device_type)

      opt_limits <- NULL
      switch(
        input$device_type,
        GPS = {
          opt_limits <- c("Storage limit" = "max",
                          "Fail rate" = "loss",
                          "Location error" = "error")
        },
        VHF = {
          opt_limits <- c("Data loss" = "loss",
                          "Location error" = "error")
        },
        stop(paste0("No handler for ", input$device_type, "."))
      ) # end of switch

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "which_limits",
        choices = opt_limits,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                         no = icon("remove", lib = "glyphicon")))

    }) %>% # end of observe,
      bindEvent(input$device_type)
    
    observe({
      vals$which_limits <- input$which_limits

      if ("loss" %in% input$which_limits) {
        shinyjs::show(id = "devBox_loss")
      } else { shinyjs::hide(id = "devBox_loss") }

      if ("error" %in% input$which_limits) {
        shinyjs::show(id = "devBox_error")
      } else { shinyjs::hide(id = "devBox_error") }

      if ("max" %in% input$which_limits) {
        shinyjs::show(id = "device_max")
      } else { shinyjs::hide(id = "device_max") }

    }) %>% # end of observe,
      bindEvent(input$which_limits)
    
    observe({
      if (is.null(input$which_limits)) {
        shinyjs::hide(id = "devBox_loss")
        shinyjs::hide(id = "devBox_error")
        shinyjs::hide(id = "device_max") }

    }) # end of observe

    ## Render validate buttons: -------------------------------------------
    
    output$devButton_gps <- renderUI({

      if (vals$dev$is_valid) {
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

    }) # end of renderUI // devButton_gps
    
    output$devButton_vhf <- renderUI({
      
      if (vals$dev$is_valid) {
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
      
    }) # end of renderUI // devButton_vhf
    
    ## Render regime text: ------------------------------------------------
    
    writing_regime <- reactive({
      req(vals$dur, vals$dti)
      
      out_dur <- fix_unit(vals$dur$value, vals$dur$unit, convert = TRUE)
      out_dti <- fix_unit(vals$dti$value, vals$dti$unit)
      
      dur <- out_dur$value
      dur_unit <- out_dur$unit

      dti <- out_dti$value
      dti_unit <- out_dti$unit
      
      dur_day <- fix_unit(vals$dur$value, vals$dur$unit)
      if (grepl("day", dur_unit)) {
        txt_dur <- wrap_none(
          dur_day$value, " ", dur_day$unit,
          css = "cl-sea", end = ".")
        
      } else if (grepl("month", dur_unit)) {
        txt_dur <- span(
          span(dur, dur_unit, class = "cl-sea"),
          "(\u2248", wrap_none(
            dur_day$value, " ", dur_day$unit,
            css = "cl-sea", end = ")."))
        
      } else if (grepl("year", dur_unit)) {
        dur_mth <- "months" %#% dur %#% dur_unit
        dur_mth <- fix_unit(dur_mth, "months")
        
        txt_dur <- span(
          span(dur, dur_unit, class = "cl-sea"),
          "(\u2248", wrap_none(dur_mth$value, " ", dur_mth$unit,
                               css = "cl-sea", end = ")."))
      } else {
        txt_dur <- wrap_none(
          ifelse(dur == 1, "", paste0(dur, " ")), 
          dur_unit, css = "cl-sea", end = ".")
      }
      
      if (dti == 1) {
        txt_dti <- dti_unit
      } else {
        txt_dti <- paste(dti, dti_unit)
      }
      
      if (dti_unit == "day") {
        txt_frq <- ""
      } else {
        frq <- round(1/("day" %#% dti %#% dti_unit), 1)
        frq_unit <- "fixes every day"
        
        if (frq > 24) {
          frq <- 1/("hours" %#% dti %#% dti_unit)
          frq_unit <- "fixes every hour"
        } else if (frq < 1) {
          frq <- 1/("month" %#% dti %#% dti_unit)
          frq_unit <- "fixes every month"
        } else if (frq == 1) {
          frq <- 1/("day" %#% dti %#% dti_unit)
          frq_unit <- "fix every day"
        }
        
        txt_frq <- span("(\u2248", wrap_none(
          round(frq, 1), " ", frq_unit,
          color = "var(--sea)", end = ")"))
      }
      
      out <- p(style = "max-width: 700px;",
               
               "This sampling design is equal to a new location",
               "every", span(txt_dti, class = "cl-sea"),
               txt_frq,
               "for a duration of", txt_dur)
      
      return(out)

    }) # end of reactive, writing_regime()

    output$devText <- renderUI(writing_regime())
    
    ## Update GPS inputs: -------------------------------------------------
    
    observe({
      if (input$gps_dti_max == "1 fix every day") {
        shinyjs::hide(id = "min_frq")
      } else { shinyjs::show(id = "min_frq") }
      
    }) %>% # end of observe,
      bindEvent(input$gps_dti_max)
    
    output$min_frq <- renderText({
      req(input$gps_from_plot, input$gps_dti_max)
      
      out <- ""
      if (input$gps_dti_max == "1 fix every 12 hours")
        out <- "2 fixes every day"
      if (input$gps_dti_max == "1 fix every 8 hours")
        out <- "3 fixes every day"
      if (input$gps_dti_max == "1 fix every 6 hours")
        out <- "4 fixes every day"
      if (input$gps_dti_max == "1 fix every 4 hours")
        out <- "6 fixes every day"
      
      return(out)
      
    }) # end of renderText // min_frq
    
    ### Reveal correct inputs:
    
    observe({
      if (input$gps_from_plot) {
        shinyjs::show("gps_dti_max")
      } else {  shinyjs::hide("gps_dti_max") }
      
    }) %>% bindEvent(input$gps_from_plot)
    
    ### Update max/min GPS fix rate inputs:
    
    observe({
      max_dti_choices <- movedesign::fixrates %>%
        dplyr::filter(.data$common == "Y") %>%
        dplyr::pull(.data$dti_notes)
      
      shinyWidgets::updatePickerInput(
        session,
        inputId = "gps_dti_max",
        choices = max_dti_choices,
        selected = "1 fix every day")
      
    }) %>% bindEvent(input$device_type)
    
    ## Convert values/units: ----------------------------------------------
    
    observe({
      req(input$gps_dur,
          input$gps_dur_unit != vals$dev$dur$unit)
      
      new_dur <- sigdigits(
        input$gps_dur_unit %#% input$gps_dur %#% vals$dev$dur$unit, 3)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "gps_dur",
        value = new_dur)
      
    }) %>% # end of observe,
      bindEvent(input$gps_dur_unit)
    
    
    observe({
      req(input$gps_dti,
          input$gps_dti_unit != vals$dev$dti$unit)
      
      new_dti <- sigdigits(
        input$gps_dti_unit %#% input$gps_dti %#% vals$dev$dti$unit, 3)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "gps_dti",
        value = new_dti)
      
    }) %>% # end of observe,
      bindEvent(input$gps_dti_unit)
    
    
    observe({
      req(input$vhf_dur,
          input$vhf_dur_unit != vals$dev$dur$unit)
      
      new_dur <- sigdigits(
        input$vhf_dur_unit %#% input$vhf_dur %#% vals$dev$dur$unit, 3)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "vhf_dur",
        value = new_dur)
      
    }) %>% # end of observe,
      bindEvent(input$vhf_dur_unit)
    
    
    observe({
      req(input$vhf_dti,
          input$vhf_dti_unit != vals$dev$dti$unit)
      
      new_dti <- sigdigits(
        input$vhf_dti_unit %#% input$vhf_dti %#% vals$dev$dti$unit, 3)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "vhf_dti",
        value = new_dti)
      
    }) %>% # end of observe,
      bindEvent(input$vhf_dti_unit)
    
    ## Changing sampling parameters: --------------------------------------
    
    output$gpsSelect_fix <- renderUI({
      
      splitLayout(
        cellWidths = c("75px", "180px"),
        cellArgs = list(style = 'align: center;'),
        
        shiny::numericInput(
          ns("gps_dti"),
          label = NULL,
          min = 1, value = 1),
        
        shiny::selectInput(
          ns("gps_dti_unit"),
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
          ns("vhf_dti_unit"),
          label = NULL,
          choices = c("Day(s)" = "days",
                      "Hour(s)" = "hours",
                      "Minute(s)" = "minutes"),
          selected = "hours")
        
      ) # end of splitLayout
    }) # end of renderUI // vhfSelect_fix
    
    output$devUI_sampling <- renderUI({
      
      switch(
        input$device_type,
        
        GPS = {  ### 1. GPS & Satellite logger:
          
          column(
            align = "center", width = 12,
            
            #### 1.1. Select tracking regime:
            
            p("What sampling interval will you evaluate?") %>%
              tagAppendAttributes(class = 'subheader'),
            
            if (!input$gps_from_plot) {
              uiOutput(ns("gpsSelect_fix")) },
            
            if (input$gps_from_plot) {
              p(style = "padding-bottom: 15px;",
                
                "Please select a",
                span("GPS fix rate", class = "label_intext"),
                "from the", span("plot", class = "cl-sea"), "below",
                "to further evaluate that sampling design.") },
            
            if (input$gps_from_plot) {
              column(
                width = 12, align = "center",
                style = "z-index: 9999;",
                
                shinyWidgets::awesomeCheckbox(
                  inputId = ns("device_log"),
                  label = span(
                    "Show in", span("Logarithmic", class = "cl-sea"),
                    "scale", icon("wrench")),
                  value = TRUE)) 
            },
            
            #### 1.2. Plotting GPS battery life decay:
            
            if (input$gps_from_plot) {
              
              ggiraph::girafeOutput(
                outputId = ns("devPlot_gps"),
                width = "100%", height = "50%") %>%
                shinycssloaders::withSpinner(
                  type = getOption("spinner.type", default = 4),
                  color = getOption("spinner.color",
                                    default = "#009da0"),
                  hide.ui	= TRUE)
            },
            
            uiOutput(ns("devPlotSubtitle")),
            uiOutput(ns("devText"))
            
          ) # end of column (UI)
          
        }, # end of GPS
        
        VHF = {  ### 2. VHF transmitter:
          
          column(
            align = "center", width = 12,
            
            # p("Here, you can vizualize the impact of different",
            #   "tracking regimes on",
            #   span("sample sizes", class = "cl-sea"),
            #   "by selecting the",
            #   span("study duration", class = "cl-dgr"),
            #   "(based on VHF battery life), and",
            #   span("sampling frequency", class = "cl-dgr"),
            #   "(based on how many times you will collect new",
            #   "locations)."),
            
            # ggiraph::girafeOutput(
            #   outputId = ns("devPlot_vhf"),
            #   width = "100%", height = "50%"),
            
            p("What sampling interval will you evaluate?") %>%
              tagAppendAttributes(class = 'subheader'),
            
            uiOutput(ns("vhfSelect_fix")),
            uiOutput(ns("devText"))
            
          ) # end of column (UI)
        }, # end of VHF
        
        stop(paste0("No handler for ", input$device_type, "."))
        
      ) # end of switch
      
    }) # end of renderUI // devUI_sampling
    
    # Add footer to sampling box:
    
    output$devUI_sampling_footer <- renderUI({
      
      switch(
        input$device_type,
        
        GPS = { ### GPS & Satellite logger:
          
          tagList(column(
            width = 12, align = "right",
            style = "padding-right: 5px;",
            
            uiOutput(ns("devButton_gps"), inline = TRUE),
            HTML("&nbsp;"),
            shiny::actionButton(
              inputId = ns("run_sim_new"),
              icon =  icon("bolt"),
              label = "Run",
              width = "120px",
              class = "btn-primary")
            
          )) # end of tagList (footer)
        },
      
        VHF = { ### VHF transmitter:
          
          tagList(column(
            width = 12, align = "right",
            style = "padding-right: 5px;",
            
            uiOutput(ns("devButton_vhf"), inline = TRUE),
            HTML("&nbsp;"),
            shiny::actionButton(
              inputId = ns("run_sim_new"),
              icon =  icon("bolt"),
              label = "Run",
              width = "120px",
              class = "btn-primary")
          )
          
          # , shiny::actionButton(
          #   inputId = ns("devHelp_vhf"),
          #   label = NULL,
          #   width = "39px",
          #   icon = icon("circle-question"),
          #   class = "btn-warning",
          #   style = "position: absolute; left: 15px;")
          
          ) # end of tagList (footer)
        },
        
        stop(paste0("No handler for ", input$device_type, "."))
        
      ) # end of switch
      
    }) # end of renderUI // devUI_sampling_footer
    
    ## Change sample size text: -------------------------------------------
    
    output$devPlotSubtitle <- renderUI({
      req(vals$which_question, input$gps_from_plot)
      if (length(vals$which_question) > 1) {
        req(vals$tau_p0, vals$tau_v0)
        
        ui <- tagList(
          span("Note:", class = "help-block-ttl"), 
          "The primary axis is the", 
          wrap_none(span("sampling duration", col = "black"), end = ","),
          "(points), and the secondary axis (lines) are",
          "the expected effective sample sizes (roughly estimated)",
          "\u2014", span("N[area]", class = "cl-sea"),
          "and", span("N[speed]", class = "cl-grn-d"), "\u2014",
          "for each sampling design; true N values may differ.")
      } else {
        
        switch(
          vals$which_question,
          "Home range" = {
            req(vals$tau_p0)
            ui <- tagList(
              span("Note:", class = "help-block-ttl"), 
              "The secondary axis (lines) are",
              "the expected effective sample sizes (roughly estimated)",
              "\u2014", span("N[area]", class = "cl-sea"), "\u2014",
              "for each sampling design; true N may differ.")
          },
          "Speed & distance" = {
            req(vals$tau_v0)
            ui <- tagList(
              span("Note:", style = "help-block-ttl"), 
              "The secondary axis (lines) are",
              "the expected effective sample sizes (roughly estimated)",
              "\u2014", span("N[speed]", class = "cl-dgr"), "\u2014",
              "for each sampling design; true N may differ.")
          },
          stop(paste0("No handler for ",
                      vals$which_question, "."))
        )
      }
      
      ui <- span(class = "help-block", ui,
                 "Sampling interval (x axis) is set to",
                 "logarithmic scale to show these",
                 "values more clearly.")
      
      return(ui)
      
    }) # end of renderUI, "devPlotSubtitle"
    
    info_sizes <- reactive({
      if (input$est_type == 1) {
        
        out_text <- helpText(
          "As", span("effective sample sizes", class = "cl-sea"),
          "are", HTML(paste0(
            span("roughly estimated", class = "cl-dgr"), ",")),
          "these values will update before validation.",
          "However, they should only be used for a quick evaluation,",
          "and will not always correspond to the real sample sizes.")
        
      } else {
        
        out_text <- helpText(
          "As", span("effective sample sizes", class = "cl-sea"),
          "are extracted from the",
          HTML(paste0(span("model fit", class = "cl-dgr"), ",")),
          "these values will", span("not", style = "color: #000;"),
          "update automatically.", br(), "Click the",
          icon("bolt", class = "cl-mdn"),
          HTML(paste0(span("Run", class = "cl-mdn"))),
          "button to recalculate them.")
      }
    }) # end of reactive, info_sizes

    output$devText_sizes <- renderUI({
      req(input$est_type)
      info_sizes()
    })
    
    # ALERTS --------------------------------------------------------------
    
    # Alert if GPS input values are not valid:
    
    observe({
      req(vals$active_tab == 'device',
          input$device_type == "GPS",
          vals$dev$dur,
          vals$alert_active)
      
      min_dur <- ifelse(
        vals$dev$dur$value == 0,
        TRUE, vals$dev$dur$value %#% 
          vals$dev$dur$unit <= 2 %#% "days")
      
      max_dur <- ifelse(
        vals$dev$dur$value == 0,
        TRUE, vals$dev$dur$value %#%
          vals$dev$dur$unit > 10 %#% "years")

       if ((min_dur || max_dur) && vals$alert_active) {
        shinyalert::shinyalert(
          type = "warning",
          title = "Invalid battery life",
          text = tagList(span(
            "Maximum duration cannot be shorter than",
            span("2 days", class = "cl-dgr"),
            "or greater than", span("10 years", class = "cl-dgr"),
            "for the simulation to succeed. In these conditions,",
            "the plot will not update.", p(),
            "Please choose a different",
            wrap_none("GPS battery life", css = "cl-blk",
                      end = "."))),
          callbackR = function(x) {
            vals$alert_active <- x
          },
          html = TRUE,
          showCancelButton = TRUE,
          cancelButtonText = "Dismiss forever",
          confirmButtonText = "OK",
          confirmButtonCol = pal$mdn,
          size = "s") }
      
    }) # end of observe
    
    ## Regime... ----------------------------------------------------------
    ### ...GPS & Satellite loggers: ---------------------------------------
    
    # Alert if research question(s)/data are NOT available:
    
    observe({
      req(vals$active_tab == 'device')
      next_step <- TRUE
  
      # Check if questions were set:
      if (is.null(vals$which_question)) {
        next_step <- FALSE
        shinyalert::shinyalert(
          title = "No research goal selected",
          text = tagList(span(
            "Please select a research question in the",
            icon("house", class = "cl-blk"),
            span("Home", class = "cl-blk"),
            "tab before proceeding.")),
          html = TRUE,
          size = "xs")
      }
      
      # Check if there is data:
      req(next_step)
      
      if (is.null(vals$data0))
        shinyalert::shinyalert(
          type = "error",
          title = "No data found",
          text = tagList(span(
            "Please", wrap_none(
              span("upload", class = "cl-dgr"), end = ","),
            span("select", class = "cl-dgr"), "or",
            span("simulate", class = "cl-dgr"), "data first",
            "in the", icon("paw", class = "cl-mdn"),
            span("Species", class = "cl-mdn"), "tabs."
          )),
          html = TRUE,
          size = "xs")
        
    }) # end of observe
    
    # Alert if user did NOT select fix rate before validation:
    
    observe({
      req(!is.null(input$gps_from_plot))
      
      # Check if inputs were selected:
      
      vals$dev$is_valid <- FALSE
      if ((!input$gps_from_plot && is.null(input$gps_dti)) ||
          (input$gps_from_plot && is.null(input$devPlot_gps_selected))
      ) {
        
        shinyalert::shinyalert(
          title = "No regime selected",
          text = tagList(span(
            "Please select a fix rate to set a",
            HTML(paste0(span("sampling design", class = "cl-sea-d"),
                        ",")), "then click the",
            icon("bolt", class = "cl-sea"),
            span("Validate", class = "cl-sea"),
            "button again."
          )),
          html = TRUE,
          size = "xs")
        
        
      } else {
        req(vals$dur, vals$dti)
        
        dur <- vals$dur$value %#% vals$dur$unit
        
        if (dur < (1 %#% "days")) {
          
          shinyalert::shinyalert(
            title = "Very short duration",
            type = "warning",
            text = tagList(span(
              "Sampling duration is currently less than",
              "one day.")),
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonText = "Proceed",
            cancelButtonText = "Cancel",
            callbackR = function(x) {
              vals$dev$is_valid <- x },
            html = TRUE,
            size = "xs")
          
        } else {
          
          vals$dev$is_valid <- TRUE
          vals$is_analyses <- FALSE
          
        } # end of if (vals$dur < 1 %#% "days")
      }
      
      
    }) %>% # end of observer,
      bindEvent(input$validate_gps)
    
    # Alert if number of locations is higher than GPS allows:
    
    observe({
      req(vals$dev$is_valid,
          vals$dev$n,
          vals$which_limits,
          input$device_max)
      
      # Check GPS storage limit (if selected):
      
      if (("max" %in% vals$which_limits) &&
          vals$dev$n > vals$storage) {
        
        shinyalert::shinyalert(
          title = "Not enough GPS storage",
          type = "warning",
          text = tagList(span(
            "GPS storage limit",
            wrap_none("(", scales::label_comma()(vals$storage),
                      " locations)"),
            "is lower than the absolute sample size",
            wrap_none("(", scales::label_comma()(vals$dev$n),
                      " locations).")
          )),
          html = TRUE,
          size = "xs")
        
        vals$dev$is_valid <- FALSE
        
      } else {
        
        vals$dev$is_valid <- TRUE
        vals$is_analyses <- FALSE
        
      } # end of storage limit if () statement
      
    }) %>% # end of observer,
      bindEvent(input$validate_gps)
    
    ### ...VHF transmitter: -----------------------------------------------
    
    # Alert if user did NOT select fix rate before validation:
    
    observe({
      
      # Check if inputs were selected:
      
      if (is.null(input$vhf_dti)) {
        
        shinyalert::shinyalert(
          title = "No regime selected",
          text = tagList(span(
            "Please select a fix rate to set a",
            HTML(paste0(span("sampling design", class = "cl-sea-d"),
                        ",")), "then click the",
            icon("bolt", class = "cl-sea"),
            span("Validate", class = "cl-sea"),
            "button again."
          )),
          html = TRUE,
          size = "xs")
        
        
      } else {
        req(vals$dur, vals$dti)
        
        dur <- vals$dur$value %#% vals$dur$unit
        
        if (dur < (1 %#% "days")) {
          
          shinyalert::shinyalert(
            title = "Very short duration",
            type = "warning",
            text = tagList(span(
              "Sampling duration is currently less than",
              "one day.")),
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonText = "Proceed",
            cancelButtonText = "Cancel",
            callbackR = function(x) {
              vals$dev$is_valid <- x },
            html = TRUE,
            size = "xs")
          
        } else {
          
          vals$dev$is_valid <- TRUE
          vals$is_analyses <- FALSE
          
        } # end of if (vals$dur < 1 %#% "days")
      }
      
    }) %>% # end of observer,
      bindEvent(input$validate_vhf)
    
    # Regime verified:
    
    observe({
      updateTabsetPanel(
        session,
        inputId = "devTabs_pars",
        selected = "tab_device_1-devPanel_device")
      
      shinyjs::show(id = "devBox_type")
      
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
      
    }) %>% bindEvent(req(vals$dev$is_valid))
    
    ## Sample sizes... ----------------------------------------------------
    
    observe({
      req(vals$dev$n)
      
      if (vals$dev$n > 20000)
        shinyalert::shinyalert(
          inputId = "alert_n_small",
          title = "Warning",
          text = tagList(span(
            "You are about to simulate a dataset with over",
            scales::label_comma()(vals$dev$n), "locations.",
            "This can take a considerable amount of time to run",
            "on certain devices (several hours or days).",
            "Do you wish to proceed?"
          )),
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Proceed",
          cancelButtonText = "Cancel",
          callbackR = function(x) {
            vals$dev$is_valid <- x },
          html = TRUE,
          size = "xs")
      
    }, priority = 0) %>% # end of observe,
      bindEvent(input$run_sim_new)
    
    # SIMULATIONS ---------------------------------------------------------
    ## Simulating GPS battery life: ---------------------------------------
    
    simulating_gps <- reactive({
      req(vals$dev$dur,
          input$gps_dti_max,
          vals$seed0)
      
      dur <- vals$dev$dur$value %#% vals$dev$dur$unit
      req(dur > 2 %#% "days")
      
      if ((dur) < 10 %#% "days") {
        input_cutoff <- .5 %#% "days"
      } else { input_cutoff <- 2 %#% "days" }
      
      dat <- simulate_gps(
        data = movedesign::fixrates,
        b_max = vals$dev$dur$value,
        b_unit = vals$dev$dur$unit,
        cutoff = input_cutoff,
        dti_max = input$gps_dti_max,
        seed = vals$seed0,
        set_seed = vals$overwrite_active)
      
      if (all(dat$dur_sec == 0)) return(NULL)
      
      # Display only values with duration
      # (plus three additional rows):
      
      if ("Y" %in% dat$cutoff) {
        
        tmp <- dat %>%
          dplyr::filter(.data$cutoff == "Y") %>%
          dplyr::pull(id)
        
        if (length(tmp) > 3)
          dat <- dat %>% dplyr::filter(id <= (min(tmp) + 3))
      }
      
      return(dat)
      
    }) # %>% # end of reactive, simulating_gps()
    # bindCache(list(input$gps_dur,
    #                input$gps_dur_unit,
    #                input$gps_dti_max,
    #                vals$seed0))
    
    ## Simulating new conditional data: -----------------------------------
   
    ### Prepare parameters:
    
    simulating_data <- reactive({
      
      dat <- vals$data0
      dur <- vals$dur$value %#% vals$dur$unit
      dti <- vals$dti$value %#% vals$dti$unit
      t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      
      if (vals$data_type == "simulated") {
        fit <- vals$ctmm_mod 
      } else {
        if (vals$fit0$isotropic == TRUE) { fit <- vals$fit0
        } else fit <- vals$ctmm_mod <- prepare_mod(
          tau_p = vals$tau_p0$value[2], 
          tau_p_units = vals$tau_p0$unit[2], 
          tau_v = vals$tau_v0$value[2], 
          tau_v_units = vals$tau_v0$unit[2], 
          sigma = vals$sigma0$value[2], 
          sigma_units = vals$sigma0$unit[2])
      }
      
      sim <- ctmm::simulate(dat, fit, t = t_new, seed = vals$seed0)
      sim <- pseudonymize(sim)
      return(sim)
      
    }) %>% # end of reactive, simulating_data()
      bindCache(c(vals$species, 
                  vals$id,
                  vals$dur, 
                  vals$dti))
    
    estimating_time <- reactive({
      
      loading_modal("Calculating run time")
      out_time <- guess_time(vals$data1, parallel = vals$parallel)
      
      shinybusy::remove_modal_spinner()
      return(out_time)
      
    }) %>% # end of reactive, estimating_time()
      bindCache(c(vals$tau_p0, 
                  vals$tau_v0,
                  vals$dur, 
                  vals$dti))
    
    fitting_model <- reactive({
      
      guess1 <- ctmm::ctmm.guess(vals$data1, interactive = FALSE)
      inputList <- list(list(vals$data1, guess1))
      out_fit <- par.ctmm.select(inputList, parallel = vals$parallel)
      
      return(out_fit)
      
    }) %>% # end of reactive, fitting_model()
      bindCache(vals$data0,
                vals$data1,
                vals$dur,
                vals$dti)
    
    ## Run simulation:
    
    observe({
      req(vals$data0,
          vals$fit0,
          vals$dur, 
          vals$dti,
          vals$dev$is_valid)
      
      shinyFeedback::showToast(
        type = "info",
        message = "Setting up simulation...",
        .options = list(
          timeOut = 2500,
          extendedTimeOut = 3500,
          progressBar = TRUE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right")
      )
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = tagList(span(
          style = "font-size: 18px;",
          span("Simulating", style = "color: #797979;"),
          HTML(paste0(span("data", class = "cl-sea"),
                      span("...", style = "color: #797979;")))
        ))
      )
      
      start <- Sys.time()
      data1 <- simulating_data()
      
      vals$data1 <- data1
      vals$data1_full <- data1
      vals$dev$n <- nrow(data1)
      vals$needs_fit <- TRUE
      vals$is_analyses <- NULL
      vals$hr_completed <- FALSE
      vals$sd_completed <- FALSE
      
      # If there is data loss:
      
      if (!is.null(input$device_loss)) {
        if (input$device_loss > 0) {
          
          vals$data1_full <- data1
          to_keep <- round(nrow(data1) * (1 - vals$lost$perc/100))
          to_keep_vec <- sort(sample(1:nrow(data1), to_keep))
          vals$data1 <- data1[to_keep_vec,]
        }
      } # end of input$device_loss
      
      # If there are errors associated with each location:
      
      if (!is.null(input$device_error)) {
        if (input$device_error > 0) {
          vals$data1_full <- data1
          error_x <- stats::rnorm(nrow(data1), mean = 0,
                                  sd = input$device_error)
          error_y <- stats::rnorm(nrow(data1), mean = 0,
                                  sd = input$device_error)
          data1[c(2,3)] <- data1[c(2,3)] + c(error_x, error_y)
          vals$data1 <- data1
          vals$error <- input$device_error
        }
      } # end of input$device_error
      
      msg_log(
        style = "success",
        message = paste0("Simulation ",
                         msg_success("completed"), "."),
        with_time = difftime(Sys.time(), start, units = "sec"))
      
      shinybusy::remove_modal_spinner()
      
      if (input$est_type == 1) {
        
        vals$needs_fit <- TRUE
        shinybusy::remove_modal_spinner()
        
      } else {
        req(vals$data1)
        shinybusy::remove_modal_spinner()
        
        expt <- estimating_time()
        vals$expt <- expt
        
        vals$dev$confirm_time <- NULL
        if ((as.numeric(expt$max) %#% expt$unit) > 900) {
          
          shinyalert::shinyalert(
            className = "modal_warning",
            title = "Do you wish to proceed?",
            callbackR = function(x) {
              vals$dev$confirm_time <- x
            },
            text = tagList(span(
              "Expected run time for the next phase", br(),
              "is approximately",
              wrap_none(span(
                expt$range, expt$unit, class = "cl-dgr"), ".")
            )),
            type = "warning",
            showCancelButton = TRUE,
            cancelButtonText = "Stop",
            confirmButtonCol = pal$mdn,
            confirmButtonText = "Proceed",
            html = TRUE
          )
        } else { vals$dev$confirm_time <- TRUE }
      }
      
    }, priority = 1) %>% # end of observe,
      bindEvent(input$run_sim_new)
    
    observe({
      req(vals$dev$confirm_time)
      
      shinyjs::show(id = "devBox_sims")
      
      if (input$est_type == 1) {
        vals$needs_fit <- TRUE
      } else {
        
        msg_log(
          style = "warning",
          message = paste0("Model fit ",
                           msg_warning("in progress"), ","),
          detail = "Please wait for model selection to finish:")
        
        shinybusy::show_modal_spinner(
          spin = "fading-circle",
          color = "var(--sea)",
          
          text = tagList(span(
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
            p(vals$expt$range,
              style = paste("background-color: #eaeaea;",
                            "color: #009da0;",
                            "font-size: 16px;",
                            "text-align: center;",
                            "margin-top: -40px;")),
            p()
          )) # end of text
        ) # end of modal
        
        start <- Sys.time()
        fit1 <- fitting_model()
        time_fit1 <- difftime(Sys.time(), start, units = "secs")
        
        if (!is.null(fit1)) {
          msg_log(
            style = 'success',
            message = paste0("Model fit ",
                             msg_success("completed"), "."),
            with_time = time_fit1)
          
          vals$guess <- NULL
          vals$needs_fit <- FALSE
          vals$fit1 <- fit1
          
          shinyjs::enable("devButton_save")
          
        } # end of if (), !is.null(fit1)
        
      } # end of if (), est_type
      
      if (!vals$tour_active) {
        shinyalert::shinyalert(
          className = "modal_success",
          type = "success",
          title = "Success!",
          text = tagList(span(
            "Proceed to the", br(),
            icon("compass-drafting", class = "cl-mdn"),
            span('Analyses', class = "cl-mdn"), "tabs."
          )),
          html = TRUE,
          size = "xs")
      }
      
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
      bindEvent(vals$dev$confirm_time)
    
    # PLOTS ---------------------------------------------------------------
    ## Plotting GPS battery life: -----------------------------------------
    
    reg_selected <- reactive({
      if (input$device_type == "GPS") return(input$devPlot_gps_selected)
      if (input$device_type == "VHF") return(input$devPlot_vhf_selected)
    })
    
    preparing_gps <- reactive({
      
      add_N1 <- FALSE
      add_N2 <- FALSE
      a1 <- b1 <- a2 <- b2 <- NULL
      
      device <- movedesign::fixrates
      gps_sim <- simulating_gps()
      
      req(gps_sim)
      vals$gps <- gps_sim
      
      if (!("N" %in% gps_sim$cutoff)) {
        pal_values <- pal$dgr
      } else { pal_values <- c(pal$mdn, pal$dgr) }
      
      gps_sim$dur <- vals$dev$dur$unit %#% gps_sim$dur_sec
      dur_unit <- vals$dev$dur$unit
      
      ymax <- max(gps_sim$dur) + diff(range(gps_sim$dur)) * .2
      
      # Add lines for absolute and effective sample sizes:
      
      gps_sim$n <- NA
      for (i in 1:nrow(gps_sim)) {
        t0 <- seq(0, round(gps_sim$dur_sec[i], 0), 
                  by = round(gps_sim$dti[i], 0))[-1]
        gps_sim$n[i] <- length(t0)
      }
      ylim.prim <- c(0, max(gps_sim$dur))
      ylim.sec <- c(0, max(gps_sim$n))
      b0 <- diff(ylim.prim)/diff(ylim.sec)
      a0 <- b0 * (ylim.prim[1] - ylim.sec[1])
      add_n <- TRUE
      
      if (!is.null(vals$which_question)) {
        
        add_N1 <- FALSE
        add_N2 <- FALSE
        
        if ("Home range" %in% vals$which_question &&
            !is.null(vals$tau_p0)) {
          
          req(vals$is_valid)
          taup <- vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
          gps_sim$N_area <- gps_sim$dur_sec / taup
          
          ylim.prim <- c(0, max(gps_sim$dur))
          ylim.sec <- c(0, max(gps_sim$N_area))
          b1 <- diff(ylim.prim)/diff(ylim.sec)
          a1 <- b1 * (ylim.prim[1] - ylim.sec[1])
          
          add_N1 <- TRUE
          
        } # end of N1
        
        if ("Speed & distance" %in% vals$which_question &&
            !is.null(vals$tau_v0)) {
          
          req(vals$is_valid)
          tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
          gps_sim$N_speed <- NA
          
          for (i in 1:nrow(gps_sim)) {
            dti <- gps_sim$dti[i]
            r <- dti / tauv
            
            n <- gps_sim$n[i]
            n_loss <- ifelse(is.null(vals$lost$n), 0, vals$lost$n)
            
            N2 <- ifelse(
              dti > tauv,
              ifelse(dti > 3 * tauv, 0,
                     (n - n_loss) / (dti/tauv)^r * (dti/tauv)),
              (n - n_loss) / tauv * dti)
            
            if (N2 < 1) N2 <- 0
            gps_sim$N_speed[i] <- N2
          }
          
          ylim.prim <- c(0, max(gps_sim$dur))
          ylim.sec <- c(0, max(gps_sim$N_speed))
          b2 <- diff(ylim.prim)/diff(ylim.sec)
          a2 <- b2 * (ylim.prim[1] - ylim.sec[1])
          
          add_N2 <- TRUE
          
        } # end of N2
        
        if (length(vals$which_question) > 1 &&
            !is.null(vals$tau_v0) &&
            !is.null(vals$tau_v0)) {
          req(vals$is_valid)
          
          ylim.prim <- c(0, max(gps_sim$dur))
          ylim.sec <- c(0, max(gps_sim$N_speed))
          b1 <- diff(ylim.prim)/diff(ylim.sec)
          a1 <- b1 * (ylim.prim[1] - ylim.sec[1])
          
        } # end of both N1 and N2
        
      } # end of !is.null(vals$which_question)
      
      return(list(
        gps = gps_sim,
        add_N1 = add_N1,
        add_N2 = add_N2,
        axis1 = data.frame(a1 = a1, b1 = b1),
        axis2 = data.frame(a2 = a2, b2 = b2),
        pal = pal_values
      ))
      
    }) # end of reactive, preparing_gps()
    
    output$devPlot_gps <- ggiraph::renderGirafe({
      req(vals$active_tab == 'device',
          input$gps_dur,
          input$gps_dur_unit,
          input$gps_dti_max,
          input$device_log)
      
      dti_scale <- dti_yn <- NULL
      device <- preparing_gps()
      device$gps$x <- log(device$gps$dti)

      x_label <- "Log of sampling interval (time between fixes)"
      pos <- c(0.25, 0.85)
      p_x <- ggplot2::geom_vline(
        xintercept = min(device$gps$x),
        alpha = 0)

      ymax <- max(device$gps$dur) +
        diff(range(device$gps$dur)) * .2

      if (!is.null(input$device_log)) {
        if (!input$device_log) {

          device$gps$x <- device$gps$dti
          x_label <- "Sampling interval (time between fixes)"
          pos <- c(0.75, 0.85)
          p_x <- ggplot2::geom_vline(
            xintercept = min(log(device$gps$x)),
            color = "grey80",
            size = 0.2)
        }
      }

      x_scale <- data.frame(
        brks = device$gps %>%
          dplyr::filter(dti_yn == "Y") %>%
          dplyr::pull(x),
        lbls = device$gps %>%
          dplyr::filter(dti_yn == "Y") %>%
          dplyr::pull(dti_scale))

      p_x_scale <- ggplot2::scale_x_continuous(
        breaks = x_scale$brks,
        labels = x_scale$lbls,
        guide = ggplot2::guide_axis(check.overlap = TRUE,
                                    n.dodge = 2))

      if (device$add_N1) {
        pN1 <- ggplot2::geom_line(
          data = device$gps,
          mapping = ggplot2::aes(
            x = .data$x,
            y = device$axis1$a1 + .data$N_area * device$axis1$b1,
            color = "Narea",
            group = 1),
          size = 3, alpha = .2)
      }
      if (device$add_N2) {
        pN2 <- ggplot2::geom_line(
          data = device$gps,
          mapping = ggplot2::aes(
            x = .data$x,
            y = device$axis2$a2 + .data$N_speed * device$axis2$b2,
            color = "Nspeed",
            group = 1),
          size = 3, alpha = .2)
      }

      p <- ggplot2::ggplot(
        device$gps, ggplot2::aes(
          x = .data$x,
          y = .data$dur,
          fill = .data$cutoff,
          tooltip = .data$dti_notes,
          data_id = as.numeric(.data$id))) +

        p_x +

        ggplot2::geom_hline(
          yintercept = 0,
          color = "grey80",
          size = 0.2) +

        { if (device$add_N1) pN1 } +
        { if (device$add_N2) pN2 } +

        { if (device$add_N1 & !device$add_N2) {
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(
              ~ (. - device$axis1$a1)/device$axis1$b1,
              name = expression(N[area])),
            labels = scales::comma,
            limits = c(0, ymax))
        } else if (device$add_N2 & !device$add_N1) {
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(
              ~ (. - device$axis2$a2)/device$axis2$b2,
              name = expression(N[speed])),
            labels = scales::comma,
            limits = c(0, ymax))
        } else if (!device$add_N1 && !device$add_N2) {
          ggplot2::scale_y_continuous(
            labels = scales::comma,
            limits = c(0, ymax))
        } else if (device$add_N1 && device$add_N2) {
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(
              ~ (. - device$axis2$a2)/device$axis2$b2,
              name = "Effective sample sizes (N)"),
            labels = scales::comma,
            limits = c(0, ymax)) }
        } +

        ggiraph::geom_point_interactive(shape = 21, size = 2) +
        p_x_scale +
        
        ggplot2::scale_fill_manual(values = device$pal) +
        ggplot2::scale_color_manual(
          name = "",
          labels = c(bquote(N[area]),
                     bquote(N[speed])),
          breaks = c("Narea", "Nspeed"),
          values = c(pal$sea, pal$grn)) +

        ggplot2::labs(
          x = x_label,
          y = paste0("Durations (in ", input$gps_dur_unit, ")")) +

        theme_movedesign() +
        ggplot2::guides(fill = "none") +
        ggplot2::theme(
          legend.position = pos,
          legend.text = ggplot2::element_text(size = 14),
          axis.title.y.right = ggplot2::element_text(angle = 90))
      
      # preselected <- character(0)
      # if (vals$tour_active) {
      #   tmp <- match("1 fix every 2 hours", device$gps$dti_notes)
      #   preselected <- as.character(tmp)
      # }

      ggiraph::girafe(
        ggobj = p,
        width_svg = 5, height_svg = 4,
        options = list(
          ggiraph::opts_tooltip(
            css = paste(
              "z-index: 99999999999999 !important;",
              "background-color: var(--midnight) !important;",
              "color: var(--white) !important;",
              "border: 1px var(--midnight) solid !important;",
              "border-radius: 4px;",
              "padding: 5px;")),
          ggiraph::opts_hover(
            css = paste(
              "r: 4pt;",
              "fill: #ffbf00;",
              "stroke: #ffbf00;")),
          ggiraph::opts_selection(
            # selected = preselected,
            type = "single",
            css = paste(
              "r: 4pt;",
              "fill: #2c3b41;",
              "stroke: #2c3b41;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))

    }) # %>% # end of renderGirafe, "devPlot_gps",
    # bindEvent(input$gps_dur,
    #           input$gps_dur_unit,
    #           input$gps_dti_max,
    #           input$device_log,
    #           vals$which_question,
    #           vals$overwrite_seed)
    
    ## Plotting new simulated data plot (xy): -----------------------------
    
    output$devPlot_id <- ggiraph::renderGirafe({
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
        
        ggplot2::geom_path(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = timestamp),
          linewidth = .6, alpha = .8) +
        
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
      
    }) # end of renderGirafe, "devPlot_id"
    
    ## Plotting variogram (svf): ------------------------------------------
    
    output$devPlot_svf <- ggiraph::renderGirafe({
      req(vals$data1)
      
      frac <- input$dev_timeframe / 100
      svf <- extract_svf(vals$data1, fraction = frac)
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
      
    }) # end of renderGirafe, "devPlot_svf"
    
    # BLOCKS --------------------------------------------------------------
    ## Species parameters: ------------------------------------------------
    
    observe({
      req(vals$tau_p0)
      
      mod_blocks_server(
        id = "devBlock_taup", 
        vals = vals, data = vals$data0, fit = vals$fit0,
        type = "tau", name = "tau_p0",
        input_name = list(
          chr = "dev_taup0",
          html = wrap_none("Position autocorrelation ",
                           "(\u03C4", tags$sub("p"), ")")),
        input_modal = "modal_taup_dev")
    }) # end of observe
    
    observe({
      req(vals$tau_v0)
      
      mod_blocks_server(
        id = "devBlock_tauv",
        vals = vals, data = vals$data0, fit = vals$fit0,
        type = "tau", name = "tau_v0",
        input_name = list(
          chr = "dev_tauv0",
          html = wrap_none("Velocity autocorrelation ",
                           "(\u03C4", tags$sub("v"), ")")),
        input_modal = "modal_tauv_dev")
    }) # end of observe
    
    ## Tracking regime: -------------------------------------------------
    
    output$devBlock_dur <- renderUI({
      req(vals$dur)
      
      out <- fix_unit(vals$dur$value, vals$dur$unit, convert = TRUE)
      if (grepl("month", out$unit)) {
        out_new <- convert_to(out, new_unit = "days")
        txt <- paste0("(or ", out_new[1], " ", out_new[2], ")")
      } else if (grepl("year", out$unit)) {
        out_new <- convert_to(out, new_unit = "months")
        txt <- paste0("(or ", out_new[1], " ", out_new[2], ")")
      } else if (grepl("days", out$unit)) {
        out <- fix_unit(vals$dur$value, vals$dur$unit)
        txt <- NULL
      } else {
        txt <- NULL
      }
      
      parBlock(
        header = "Sampling duration",
        value = paste(out[1], out[2]),
        subtitle = txt)
      
    }) # end of renderUI // devBlock_dur
    
    output$devBlock_dti <- renderUI({
      req(vals$dti)
      
      out <- fix_unit(vals$dti$value, vals$dti$unit, convert = TRUE)
      
      parBlock(
        header = "Sampling interval",
        value = paste(out[1], out[2]),
        subtitle = "between fixes")
      
    }) # end of renderUI // devBlock_dti
    
    ## Sample sizes: ----------------------------------------------------
    
    output$devBlock_n <- renderUI({
      req(vals$dev$n)
      
      n <- vals$dev$n
      
      number_n <- ""
      icon_n <- FALSE
      
      if (!is.null(vals$lost$perc)) {
        if (vals$lost$perc > 0) {
          number_n <- paste0("-", vals$lost$perc, "%")
          icon_n <- TRUE
        }
      }
      
      sampleBlock(
        number = span(number_n),
        numberIcon = icon_n,
        header = n,
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)
      
    }) # end of renderUI // devBlock_n
    
    output$devBlock_Narea <- renderUI({
      req(vals$dev$N1)
      
      diff1 <- paste0("-", round((100 - ((
        vals$dev$N1 * 100)/vals$dev$n)), 1), "%")
      
      sampleBlock(
        number = span(diff1),
        numberIcon = TRUE,
        header = round(vals$dev$N1, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)
      
    }) # end of renderUI // devBlock_Narea
    
    output$devBlock_Nspeed <- renderUI({
      req(vals$dev$N2)
      
      diff2 <- paste0("-", round((100 - ((
        vals$dev$N2 * 100)/vals$dev$n)), 1), "%")
      
      sampleBlock(
        number = span(diff2),
        numberIcon = TRUE,
        header = round(vals$dev$N2, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)
      
    }) # end of renderUI // devBlock_Nspeed
    
    ## Data loss: -------------------------------------------------------
    
    output$devBlock_loss <- renderUI({
      req(vals$lost$n, input$device_loss > 0)
      
      parBlock(
        header = "Fixes lost:",
        value = vals$lost$n)
      
    }) # end of renderUI // devBlock_loss

    # TABLES --------------------------------------------------------------
    # Listing multiple sampling designs: ----------------------------------
    
    devRow <- reactive({
      
      out <- data.frame(
        device = NA,
        dur = NA,
        dti = NA,
        n = NA,
        N1 = NA,
        N2 = NA,
        fit = NA)
      
      out$device <- input$device_type
      
      dur <- fix_unit(vals$dur$value, vals$dur$unit, convert = TRUE)
      dti <- fix_unit(vals$dti$value, vals$dti$unit)
      
      out$dur <- paste(dur[1], abbrv_unit(dur[,2]))
      out$dti <- paste(dti[1], abbrv_unit(dti[,2]))
      
      out$n <- vals$dev$n
      out$N1 <- vals$dev$N1
      out$N2 <- vals$dev$N2
      out$fit <- vals$is_fitted
      
      return(out)
      
    })
    
    observe({
      req(vals$dur, vals$dti,
          vals$dev$n, vals$dev$N1, vals$dev$N2)
      
      shinyjs::show(id = "devBox_summary")
      shinyjs::disable("devButton_save")
      
      vals$dev$tbl <<- rbind(vals$dev$tbl, devRow())
      vals$dev$tbl <- dplyr::distinct(vals$dev$tbl)
      vals$report_dev_yn <- TRUE
      
    }) %>% # end of observe,
      bindEvent(input$devButton_save)
    
    output$devTable <- reactable::renderReactable({
      req(vals$dev$tbl)
      
      nms <- list(
        device = "Type",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = "N (area)",
        N2 = "N (speed)",
        fit = "Fitted?")
      
      reactable::reactable(
        vals$dev$tbl,
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
            name = nms[["device"]]),
          dur = reactable::colDef(
            minWidth = 80, name = nms[["dur"]],
            style = list(fontWeight = "bold")),
          dti = reactable::colDef(
            minWidth = 80, name = nms[["dti"]],
            style = list(fontWeight = "bold")),
          n = reactable::colDef(
            name = nms[["n"]],
            style = format_num,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 0)),
          N1 = reactable::colDef(
            minWidth = 80, name = nms[["N1"]],
            style = format_num,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)),
          N2 = reactable::colDef(
            minWidth = 80, name = nms[["N2"]],
            style = format_num,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 1)),
          fit = reactable::colDef(
            minWidth = 70, name = nms[["fit"]])
        ))
      
    }) # end of renderReactable, "devTable"
    
    # observe({
    #   vals$dev$tbl <- NULL
    # }) %>% # end of observe,
    #   bindEvent(input$devTable_clear)
    
    ## Additional information: --------------------------------------------
    
    # Export values for tests:
    
    shiny::exportTestValues(
      data1 = simulating_data()
    )
    
    # Save information for report if table is not requested:
    
    observe({
      req(vals$active_tab == 'device',
          vals$dev$n, vals$dev$N1, vals$dev$N2, vals$fit1)
      
      req(is.null(vals$dev$tbl))
      vals$report_dev_yn <- FALSE
      vals$dev$tbl <- devRow()
      
    }) # end of observe
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_tab_design_ui("tab_design_1")
    
## To be copied in the server
# mod_tab_design_server("tab_design_1")
