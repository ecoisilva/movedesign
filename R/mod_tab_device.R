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
        
        title = span("Sampling design:", class = "ttl-tab"),
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
            inputId = ns("which_limitations"),
            label = p("What limitations do you want to consider?") %>%
              tagAppendAttributes(
                class = 'label_center no-bottom'),
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
                  
                  shinyWidgets::autonumericInput(
                    inputId = ns("gps_dur"),
                    label = NULL,
                    minimumValue = 0,
                    value = 12,
                    allowDecimalPadding = FALSE,
                    wheelStep = 1),
                  
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
                
                fluidRow(
                  column(width = 12,
                         verbatimTextOutput(outputId = ns("min_frq"))
                  )), p(style = "padding: 0px;"),
                
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
                  
                ) #, # end of splitLayout
                
                #TODO
                # shinyWidgets::sliderTextInput(
                #   inputId = ns("vhf_ppm"),
                #   label = "Pulses per minute (ppm):",
                #   choices = c(15, 20, 25, 30, 35, 40, 55, 120),
                #   selected = 40,
                #   grid = FALSE),
                
              ) # end of fluidRow
            ) #, # end of column
            
            #TODO
            # footer = shiny::actionButton(
            #     inputId = ns("add_vhf_point"),
            #     label = span("Add to",
            #                  span("plot", class = "cl-sea")),
            #     icon = icon("bookmark"),
            #     width = "100%") 
            
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
      
      if (!is.null(vals$tau_p0) & !is.null(vals$tau_v0)) {
        shinyjs::show(id = "regBox_pars")
      } else { shinyjs::hide(id = "regBox_pars") }
      
    })
    
    observe({ ### Reveal sample sizes box:
      
      if (is.null(vals$is_reg_valid)) {
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
      
      switch(input$device_type,
             GPS = {    
               shinyjs::show(id = "regBox_gps_device")
               shinyjs::hide(id = "regBox_vhf_device") 
             },
             VHF = { 
               shinyjs::show(id = "regBox_vhf_device")
               shinyjs::hide(id = "regBox_gps_device")  
             },
             stop(paste0("No handler for ", input$device_type, "."))
      )
      vals$device_type <- input$device_type
      
    }) %>% # end of observe,
      bindEvent(input$device_type)
    
    ## Update device limitation UI elements: ------------------------------
    
    observe({
      req(input$device_type)
      
      opt_limits <- NULL
      switch(input$device_type,
             GPS = {    
               opt_limits <- c("Storage limit" = "limit",
                               "Fail rate" = "loss",
                               "Location error" = "error")
             },
             VHF = { 
               opt_limits <- c("Data loss" = "loss",
                               "Location error" = "error")
             },
             stop(paste0("No handler for ", input$device_type, "."))
      )
      
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "which_limitations",
        choices = opt_limits,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                         no = icon("remove", lib = "glyphicon")))
      
    }) %>% bindEvent(input$device_type)
    
    observe({
      vals$which_limitations <- input$which_limitations
      
      if ("loss" %in% input$which_limitations) {
        shinyjs::show(id = "regBox_loss")
      } else { shinyjs::hide(id = "regBox_loss") }
      
      if ("error" %in% input$which_limitations) {
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
    
    observe({
      req(input$device_type)
      
      switch(input$device_type,
             GPS = {    
               req(vals$reg$dur, vals$reg$dti)
               
               out_dur <- fix_unit(vals$reg$dur, vals$reg$dur_unit)
               out_dti <- fix_unit(vals$reg$dti, vals$reg$dti_unit)
             },
             VHF = { 
               req(input$vhf_dur, input$vhf_dti)
               
               out_dur <- fix_unit(input$vhf_dur, input$vhf_dur_units)
               out_dti <- fix_unit(input$vhf_dti, input$vhf_dti_units)
             },
             stop(paste0("No handler for ", input$device_type, "."))
      )
      
      dur <- out_dur$value
      dur_unit <- out_dur$unit
      
      dti <- out_dti$value
      dti_unit <- out_dti$unit
      
      if (dur_unit == "day" | dur_unit == "days") {
        tmp <- ifelse(dur == 1, "", paste0(dur, " "))
        txt_dur <- wrap_none(tmp, dur_unit, 
                             css = "cl-sea", end = ".")
        
      } else if (dur_unit == "month" | dur_unit == "months") {
        dur_day <- "days" %#% dur %#% dur_unit
        dur_day <- fix_unit(dur_day, "days")
        
        txt_dur <- span(
          span(dur, dur_unit, class = "cl-sea"),
          "(\u2248", wrap_none(
            dur_day$value, " ", dur_day$unit,
            css = "cl-sea", end = ")."))
       
      } else if (dur_unit == "year" | dur_unit == "years") {
        dur_mth <- "months" %#% dur %#% dur_unit
        dur_mth <- fix_unit(dur_mth, "months")
        
        txt_dur <- span(
          span(dur, dur_unit, class = "cl-sea"),
          "(\u2248", wrap_none(dur_mth$value, " ", dur_mth$unit, 
                               css = "cl-sea", end = ")."))
      }
      
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
      
      frq <- round(frq, 1)
      
      out <- p(style = "max-width: 700px;",
               
               "This sampling design is equal to a new location",
               "every", span(dti, dti_unit, class = "cl-sea"),
               "(\u2248", HTML(paste0(span(paste(frq, frq_unit),
                                           class = "cl-sea"), ")")),
               "for a duration of", txt_dur)
      
      output$regText_gps <- renderUI(out) 
      output$regText_vhf <- renderUI(out)
      
    })
    
    ## Update GPS inputs: -------------------------------------------------
    
    observe({
      
      if (input$gps_dti_max == "1 fix every day") {
        shinyjs::hide(id = "min_frq")
      } else {
        shinyjs::show(id = "min_frq")
      }
    }) %>% bindEvent(input$gps_dti_max)
    
    output$min_frq <- renderText({
      
      out <- ""
      if (input$gps_dti_max ==
          "1 fix every 12 hours") out <- "2 fixes every day"
      if (input$gps_dti_max ==
          "1 fix every 8 hours")  out <- "3 fixes every day"
      if (input$gps_dti_max ==
          "1 fix every 6 hours")  out <- "4 fixes every day"
      if (input$gps_dti_max ==
          "1 fix every 4 hours")  out <- "6 fixes every day"
      
      return(out)
    }) %>% bindEvent(input$gps_dti_max)
    
    ### Reveal correct inputs:
    
    observe({
      if (input$eval_tradeoffs) {
        shinyjs::show("gps_dti_max")
        # shinyjs::show(id = "gps_decay")
      } else {
        shinyjs::hide("gps_dti_max")
        # shinyjs::hide(id = "gps_decay")
      }
      
    }) %>% bindEvent(input$eval_tradeoffs)
    
    ### Update max/min GPS fix rate inputs:
    
    observe({
      req(vals$active_tab == 'regime',
          input$device_type == "GPS")
      
      max_dti_choices <- movedesign::fixrates %>%
        dplyr::filter(.data$common == "Y") %>%
        dplyr::pull(.data$dti_notes)
      
      shinyWidgets::updatePickerInput(
        session,
        inputId = "gps_dti_max",
        choices = max_dti_choices,
        selected = "1 fix every day")
      
    }) %>% bindEvent(input$device_type)
    
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
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "gps_dur",
        value = new_dur,
        options = list(
          minimumValue = 0,
          allowDecimalPadding = FALSE))
      
      
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
        switch(
          input$device_type,
          
          ### 1. GPS & Satellite logger:
          GPS = {
            
            column(
              align = "center", width = 12,
              
              #### 1.1. Select tracking regime:
              
              p("What", span("sampling interval", class = "col-hgl"),
                "will you evaluate?") %>%
                tagAppendAttributes(class = 'subheader'),
              
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
              
              if (input$eval_tradeoffs) {
                
                column(
                  width = 12,
                  style = "z-index: 1000;",
                  
                  shinyWidgets::switchInput(
                    inputId = ns("deviceInput_log"),
                    label = span(icon("wrench"),
                                 "Logarithmic"),
                    labelWidth = "100px")
                  
                ) },
              
              #### 1.2. Plotting GPS battery life decay:
              
              if (input$eval_tradeoffs) {
                
                ggiraph::girafeOutput(
                  outputId = ns("regPlot_gps"),
                  width = "100%", height = "50%") %>%
                  shinycssloaders::withSpinner(
                    type = getOption("spinner.type", default = 4),
                    color = getOption("spinner.color",
                                      default = "#009da0"),
                    hide.ui	= TRUE)
              },
              
              uiOutput(ns("regPlotSubtitle")),
              uiOutput(ns("regText_gps"))
              
            ) # end of column (UI)
            
          },
          
          ### 2. VHF transmitter:
          VHF = { 
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
              
              #TODO
              # ggiraph::girafeOutput(
              #   outputId = ns("regPlot_vhf"),
              #   width = "100%", height = "50%"),
              
              p("What", span("sampling interval", class = "col-hgl"),
                "will you evaluate?") %>%
                tagAppendAttributes(class = 'subheader'),
              
              uiOutput(ns("vhfSelect_fix")),
              uiOutput(ns("regText_vhf"))
              
            ) # end of column (UI)
          }, # end of VHF device
          stop(paste0("No handler for ", input$device_type, "."))
        ) # end of switch
        
      }) # end of renderUI // regUI_sampling
      
    }) %>% # end of observe
      bindEvent(input$device_type, ignoreInit = TRUE)
    
    observe({ # Add footer to sampling box:
      
      output$regUI_sampling_footer <- renderUI({
          
        switch(input$device_type,
               
               ### GPS & Satellite logger:
               GPS = {    
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
               },
               
               ### VHF transmitter:
               VHF = { 
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
               },
               stop(paste0("No handler for ", input$device_type, "."))
        ) # end of switch
      }) # end of renderUI // regUI_sampling_footer
      
    }) %>% # end of observe
      bindEvent(input$device_type, ignoreInit = TRUE)
    
    ## Change sample size text: -----------------------------------------
    
    observe({
      req(vals$which_question)
      # vals$tau_p0, vals$tau_v0
      
      if (length(vals$which_question) > 1) {
        ui <- tagList(
          "Note: The secondary axis is showing an approximation of",
          "the effective sample sizes",
          span("N[area]", class = "cl-sea"),
          "and", span("N[speed]", class = "cl-dgr"),
          "for each sampling design;",
          "true N values may be slighty different.")
      } else {
        
        switch(vals$which_question,
               "Home range" = { 
                 ui <- tagList(
                   "Note: The secondary axis is showing",
                   "the effective sample size",
                   span("N[area]", class = "cl-sea"),
                   "for each sampling design.")
               },
               "Speed & distance" = {
                 ui <- tagList(
                   "Note: The secondary axis is showing",
                   "the effective sample size",
                   span("N[speed]", class = "cl-dgr"),
                   "for each sampling design.")
               },
               stop(paste0("No handler for ", 
                           vals$which_question, "."))
        )
      }
      
      ui <- span(class = "help-block",
                 ui, 
                 "Switch to logarithmic scale to show these",
                 "values more clearly at higher intervals.")
      
      output$regPlotSubtitle <- renderUI({ ui })
      
    }) %>% # end of observe
      bindEvent(vals$which_question)
    
    
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
        
        if (dur < (1 %#% "days")) {
          
          shinyalert::shinyalert(
            title = "Sampling duration too short",
            text = span(
              "Sampling duration is currently less than",
              "one day."
            ),
            html = TRUE,
            size = "xs")
          
          vals$is_reg_valid <- FALSE
          
        } else {
          
          vals$is_reg_valid <- TRUE
          vals$is_analyses <- FALSE
          
        } # end of if (vals$dur < 1 %#% "days")
        
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
          
        } # end of storage limit if () statement
      } # end of selection if () statement
      
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
      
      if (vals$device_n > 20000) {
        vals$confirm_n <- FALSE
        shinyalert::shinyalert(
          inputId = "alert_n_small",
          title = "Warning",
          text = span(
            "You are about to simulate a dataset with over",
            scales::label_comma()(vals$device_n), "locations.",
            "This can take a considerable amount of time to run",
            "on certain devices (several hours or days).",
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
      validate(need(input$gps_dur != '' && input$gps_dur > 0,
                    "Requires a positive value."))
      if(input$gps_dur_units == "days")
        validate(need(input$gps_dur > 2, "Requires a value."))
      
      vals$gps_dur <- input$gps_dur
      vals$gps_dur_units <- input$gps_dur_units

    }) %>% bindEvent(list(input$gps_dur,
                          input$gps_dur_units), 
                     ignoreNULL = TRUE)
    
    observe({
      vals$gps_dti <- input$gps_dti
      vals$gps_dti_units <- input$gps_dti_units
      
    }) %>% bindEvent(list(input$gps_dti,
                          input$gps_dti_units),
                     ignoreNULL = TRUE)
                     
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
    
    ## Device, sampling duration & interval: ---------------------------
    
    observe({
      vals$reg$dur <- NULL
      vals$reg$dti <- NULL
      
      # GPS:
      if (input$device_type == "GPS") {
        
        if (input$eval_tradeoffs &&
            !is.null(input$regPlot_gps_selected)) {
          req(vals$df_gps)
          validate(need(input$gps_dur != '', "Requires a value."))
          
          gps <- vals$df_gps
          selected <- reg_selected()
          
          vals$reg$dur <- input$gps_dur_units %#%
            gps[selected, ]$dur_sec
          vals$reg$dur_unit <- input$gps_dur_units
          
          tmpdti_notes <- gps[selected, ]$dti_notes
          tmpdti_units <- sub('^.* ([[:alnum:]]+)$',
                              '\\1', tmpdti_notes)
          vals$reg$dti_unit <- tmpdti_units
          vals$reg$dti <- tmpdti_units %#% gps[selected, ]$dti
          
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
        
      } else if (input$device_type == "VHF") {
        
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
    
    ## Device, sample sizes: ----------------------------------------------
    
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
      
      tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      taup <- vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      
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
    
    gps_simulation <- shiny::reactive({
      
      if ((input$gps_dur %#% input$gps_dur_units) < 10 %#% "days") {
        input_cutoff <- .5 %#% "days"
      } else { input_cutoff <- 2 %#% "days" }
      
      dat <- simulate_gps(
        data = movedesign::fixrates,
        yrange = input$gps_dur,
        yunits = input$gps_dur_units,
        cutoff = input_cutoff,
        max_dti = input$gps_dti_max)
      
      if (all(dat$dur_sec == 0)) return(NULL)
      
      # Display only values with duration
      # (plus three additional rows):
      
      if ("red" %in% dat$color) {
        
        tmpids <- dat %>%
          dplyr::filter(.data$color == "red") %>%
          dplyr::pull(id)

        if (length(tmpids) > 3) {
          dat <- dat %>% dplyr::filter(id <= (min(tmpids) + 3))
        }
      }
      
      return(dat)
      
    }) %>% # end of reactive, gps_simulation()
      bindCache(list(input$gps_dur,
                     input$gps_dur_units,
                     input$gps_dti_max))
    
    ## Simulating new conditional data: ---------------------------------
    
    ### Prepare parameters and reactive() functions:
    
    data_sim <- shiny::reactive({
      
      dur <- vals$reg$dur %#% vals$reg$dur_unit
      dti <- vals$reg$dti %#% vals$reg$dti_unit
      t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      
      data <- ctmm::simulate(
        vals$data0,
        vals$fit0,
        t = t_new,
        seed = vals$seed0)
      data <- pseudonymize(data)
      
      return(data)
      
    }) %>% # end of reactive, data_sim
      bindCache(c(vals$species, vals$id,
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
      bindCache(c(vals$tau_p0, vals$tau_v0,
                  vals$reg$dur, vals$reg$dur_unit,
                  vals$reg$dti, vals$reg$dti_unit))
    
    model_fit <- shiny::reactive({
      
      guess1 <- ctmm::ctmm.guess(vals$data1, interactive = FALSE)
      
      inputList <- list(list(vals$data1, guess1))
      out_fit <- par.ctmm.select(inputList, parallel = vals$parallel)
      
      return(out_fit)
      
    }) %>% # end of reactive, data_fit
      bindCache(vals$data0,
                vals$data1,
                vals$reg$dur,
                vals$reg$dti)
    
    # Run simulation:
    
    observe({
      req(vals$data0,
          vals$fit0,
          vals$reg$dur,
          vals$reg$dti,
          vals$is_reg_valid,
          vals$confirm_n)
      
      message("...Running simulation...")
      
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
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = span(
          style = "font-size: 18px;",
          span("Simulating", style = "color: #797979;"),
          HTML(paste0(span("data", class = "cl-sea"),
                      span("...", style = "color: #797979;")))
        )
      )
      start <- Sys.time()
      data1 <- data_sim()
      
      vals$needs_fit <- TRUE
      vals$is_analyses <- NULL
      
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
        shinybusy::remove_modal_spinner()
        
      } else {
        req(vals$data1)
        
        shinybusy::remove_modal_spinner()
        
        msg_log(
          style = "warning",
          message = paste0("Model fit ",
                           msg_warning("in progress"), "."),
          detail = "Please wait for model selection to finish:")
        
        expt <- runtime()
        vals$expt_max <- expt$max
        vals$expt_min <- expt$min
        vals$expt_units <- expt$units
        
        vals$reg$confirm_time <- NULL
        if ((as.numeric(expt$max) %#% expt$units) > 900) {
          
          shinyalert::shinyalert(
            className = "modal_warning",
            title = "Do you wish to proceed?",
            callbackR = function(x) {
              vals$reg$confirm_time <- x
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
        } else { vals$reg$confirm_time <- TRUE }
      }
      
    }) %>% # end of observe,
      bindEvent(input$run_sim_new)
      
    observe({
      req(vals$reg$confirm_time)
      
      shinyjs::show(id = "regBox_sims")
      
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
        fit1 <- model_fit()
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
          
          shinyjs::enable("regButton_save")
          
        } # end of if (), !is.null(fit1)
      } # end of if (), est_type
      
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
      bindEvent(vals$reg$confirm_time)
    
    # PLOTS ---------------------------------------------------------------
    ## Plotting VHF: ------------------------------------------------------
    
    # observe({
    #   req(input$vhf_dur, input$vhf_ppm)
    # 
    #   vhfrow <- data.frame(
    #     dur = input$vhf_dur,
    #     dur_units = input$vhf_dur_units,
    #     dur_secs = input$vhf_dur %#% input$vhf_dur_units,
    #     ppm = input$vhf_ppm)
    # 
    #   vals$df_vhf <<- rbind(vals$df_vhf, vhfrow)
    # 
    # }) %>% # end of observe
    #   bindEvent(input$add_vhf_point)
    # 
    # # observe({
    # #   vals$df_vhf <- NULL
    # # }) %>% # end of observe,
    # #   bindEvent(input$vhfDat_clear)
    # 
    # #TODO
    # # output$regPlot_vhf <- ggiraph::renderGirafe({
    # #   req(vals$df_vhf) # req(nrow(vals$df_vhf) > 1)
    # # 
    # #   df0 <- vals$df_vhf
    # #   df0$id <- 1:nrow(df0)
    # #   df0$ppm_notes <- paste(df0$ppm, "ppm")
    # # 
    # #   dur_units <- df0$dur_units[1]
    # #   df0$dur_new <- dur_units %#% df0$dur_secs
    # # 
    # #   p <- df0 %>%
    # #     ggplot2::ggplot(
    # #       ggplot2::aes(x = ppm,
    # #                    y = dur_new,
    # #                    data_id = as.numeric(id))) +
    # # 
    # #     ggplot2::geom_smooth(
    # #       method = lm,
    # #       formula = y ~ x,
    # #       color = "#f4f4f4", se = F,
    # #       size = 1.5, alpha = .8) +
    # # 
    # #     ggiraph::geom_point_interactive(
    # #       ggplot2::aes(tooltip = ppm_notes),
    # #       size = 2,
    # #       col = pal$mdn) +
    # # 
    # #     ggplot2::coord_fixed(ratio = -3) +
    # #     ggplot2::labs(
    # #       x = "Pulse rate (ppm)",
    # #       y = paste0("Durations (in ", dur_units, ")")) +
    # #     theme_movedesign() +
    # #     ggplot2::theme(legend.position = "none")
    # # 
    # #   ggiraph::girafe(
    # #     ggobj = p,
    # #     width_svg = 6, height_svg = 5.5,
    # #     options = list(
    # #       ggiraph::opts_hover(
    # #         css = paste("r:5pt;",
    # #                     "fill:#ffbf00;",
    # #                     "stroke:#ffbf00;")),
    # #       ggiraph::opts_selection(
    # #         type = "single",
    # #         css = paste("r:5pt;",
    # #                     "fill:#dd4b39;",
    # #                     "stroke:#eb5644;")),
    # #       ggiraph::opts_toolbar(saveaspng = FALSE)))
    # # 
    # # }) # end of renderGirafe // regPlot_vhf
    
    ## Plotting GPS battery life: -----------------------------------------
    
    reg_selected <- reactive({
      if (input$device_type == "GPS") return(input$regPlot_gps_selected)
      if (input$device_type == "VHF") return(input$regPlot_vhf_selected)
    })
    
    observe({
      req(vals$active_tab == 'regime', 
          input$device_type == "GPS",
          input$gps_dur, 
          input$gps_dur_units, 
          input$gps_dti_max) 
      
      proceed <- TRUE
      
      validate(need(input$gps_dur > 0, "Requires a non-zero value."))
      if ((input$gps_dur %#% input$gps_dur_units <= 2 %#% "days") ||
          (input$gps_dur %#% input$gps_dur_units > 10 %#% "years")) {
        
        proceed <- NULL
        
        if (vals$keep_alert)
          shinyalert::shinyalert(
            type = "warning",
            title = "Invalid battery life",
            text = tagList(
              "Maximum duration cannot be shorter than",
              span("2 days", class = "cl-dgr"),
              "or greater than", span("10 years", class = "cl-dgr"),
              "for the simulation to succeed. In these conditions,",
              "the plot will not update.", p(),
              "Please choose a different",
              wrap_none("GPS battery life", css = "cl-blk",
                        end = ".")),
            callbackR = function(x) {
              vals$keep_alert <- x
            },
            html = TRUE,
            showCancelButton = TRUE,
            cancelButtonText = "Dismiss forever",
            confirmButtonText = "OK",
            confirmButtonCol = pal$mdn,
            size = "s")
        
        req(proceed)
      }
      
      add_n <- FALSE
      add_N1 <- FALSE
      add_N2 <- FALSE
      
      device <- movedesign::fixrates
      
      gps_sim <- gps_simulation()
      req(gps_sim)
      vals$df_gps <- gps_sim
      
      if (!("blue" %in% gps_sim$color)) {
        pal_values <- pal$dgr
      } else { pal_values <- c(pal$mdn, pal$dgr) }
      
      gps_sim$dur <- input$gps_dur_units %#% gps_sim$dur_sec
      dur_units <- input$gps_dur_units
      
      ymax <- max(gps_sim$dur) + diff(range(gps_sim$dur)) * .2
      
      # Add lines for absolute and effective sample sizes:
      
      gps_sim$n <- NA
      for(i in 1:nrow(gps_sim)) {
        t0 <- seq(1, gps_sim$dur_sec[i], by = gps_sim$dti[i])
        gps_sim$n[i] <- length(t0)
      }
      ylim.prim <- c(0, max(gps_sim$dur))
      ylim.sec <- c(0, max(gps_sim$n))
      b0 <- diff(ylim.prim)/diff(ylim.sec)
      a0 <- b0 * (ylim.prim[1] - ylim.sec[1])
      add_n <- TRUE
      
      if (!is.null(vals$which_question)) {
        req(vals$which_question)
        req(vals$is_valid, vals$tau_p0, vals$tau_v0)

        tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
        taup <- vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]

        if ("Home range" %in% vals$which_question) {

          gps_sim$N_area <- gps_sim$dur_sec / taup

          ylim.prim <- c(0, max(gps_sim$dur))
          ylim.sec <- c(0, max(gps_sim$N_area))
          b1 <- diff(ylim.prim)/diff(ylim.sec)
          a1 <- b1 * (ylim.prim[1] - ylim.sec[1])

          add_N1 <- TRUE
        } # end of N1

        if ("Speed & distance" %in% vals$which_question) {

          gps_sim$N_speed <- NA

          for(i in 1:nrow(gps_sim)) {
            dti <- gps_sim$dti[i]
            r <- dti / tauv

            n <- gps_sim$n[i]
            n_loss <- ifelse(is.null(vals$n_lost), 0, vals$n_lost)

            N2 <- ifelse(
              dti > tauv,
              ifelse(dti > 3 * tauv, 0,
                     (n - n_loss) / (dti/tauv)^r * (dti/tauv)),
              (n - n_loss) / tauv * dti)

            if(N2 < 1) N2 <- 0
            gps_sim$N_speed[i] <- N2
          }

          ylim.prim <- c(0, max(gps_sim$dur))
          ylim.sec <- c(0, max(gps_sim$N_speed))
          b2 <- diff(ylim.prim)/diff(ylim.sec)
          a2 <- b2 * (ylim.prim[1] - ylim.sec[1])

          add_N2 <- TRUE
        } # end of N2
        
        if (length(vals$which_question) > 1) {

          ylim.prim <- c(0, max(gps_sim$dur))
          ylim.sec <- c(0, max(gps_sim$N_speed))
          b1 <- diff(ylim.prim)/diff(ylim.sec)
          a1 <- b1 * (ylim.prim[1] - ylim.sec[1])

        } # end of both N1 and N2

      } # end of !is.null(vals$which_question)
      
      output$regPlot_gps <- ggiraph::renderGirafe({
        
        gps_sim$x <- log(gps_sim$frq_hrs)
        x_label <- "Log of frequency (fixes per hour)"
        
        p_x <- ggplot2::geom_vline(xintercept = 0, alpha = 0)
        
        if (!is.null(input$deviceInput_log)) {
          if (!input$deviceInput_log) {
            
            gps_sim$x <- gps_sim$frq_hrs
            x_label <- "Frequency (fixes per hour)"
            
            p_x <- ggplot2::geom_vline(xintercept = 0,
                                      color = "grey80",
                                      size = 0.2)
          }
        }
        if (add_n) {
          p1 <- ggplot2::geom_line(
            data = gps_sim,
            mapping = ggplot2::aes(
              x = .data$x,
              y = a0 + .data$n * b0,
              group = 1),
            color = pal$mdn,
            size = 3, alpha = .2)
        }
        if (add_N1) {
          p2 <- ggplot2::geom_line(
            data = gps_sim,
            mapping = ggplot2::aes(
              x = .data$x,
              y = a1 + .data$N_area * b1,
              group = 1),
            color = pal$sea,
            size = 3, alpha = .2)
        }
        if (add_N2) {
          p3 <- ggplot2::geom_line(
            data = gps_sim,
            mapping = ggplot2::aes(
              x = .data$x,
              y = a2 + .data$N_speed * b2,
              group = 1),
            color = pal$dgr,
            size = 3, alpha = .2)
        }
        
        p <- ggplot2::ggplot(
          gps_sim, ggplot2::aes(
            x = .data$x,
            y = .data$dur,
            color = .data$color,
            tooltip = .data$dti_notes,
            # tooltip = paste(.data$dti_notes,
            #                 ",", .data$N_area),
            data_id = as.numeric(.data$id))) +
          
          p_x +
          
          ggplot2::geom_hline(yintercept = 0,
                              color = "grey80",
                              size = 0.2) +
          
          # { if (add_n)  p1 } +
          { if (add_N1) p2 } +
          { if (add_N2) p3 } +

          { if (add_N1 & !add_N2) {
            ggplot2::scale_y_continuous(
              sec.axis = ggplot2::sec_axis(
                ~ (. - a1)/b1,
                name = expression(N[area])),
              labels = scales::comma,
              limits = c(0, ymax))
          } else if (add_N2 & !add_N1) {
            ggplot2::scale_y_continuous(
              sec.axis = ggplot2::sec_axis(
                ~ (. - a2)/b2,
                name = expression(N[speed])),
              labels = scales::comma,
              limits = c(0, ymax))
          } else if (!add_N1 && !add_N2) {
            ggplot2::scale_y_continuous(
              labels = scales::comma,
              limits = c(0, ymax))
          } else if (add_N1 && add_N2) {
            ggplot2::scale_y_continuous(
              sec.axis = ggplot2::sec_axis(
                ~ (. - a2)/b2,
                name = expression(N)),
              labels = scales::comma,
              limits = c(0, ymax)) }
          } +
          
          ggiraph::geom_point_interactive(size = 1.5) +
          ggplot2::scale_color_manual(values = pal_values) +
          
          # ggplot2::scale_fill_manual(
          #   name = "",
          #   labels = c(expression(N[area],
          #              expression(N[speed])),
          #   values = c(pal$sea, pal$dgr))) +
          
          ggplot2::labs(
            x = x_label,
            y = paste0("Durations (in ", dur_units, ")")) +
          theme_movedesign() +
          ggplot2::guides(color = "none") +
          ggplot2::theme(legend.position = c(0.85, 0.85))
        
        if (vals$tour_active) {
          tmpid <- match("1 fix every hour", device$dti_notes)
          preselected <- as.character(tmpid)
        } else {
          preselected <- character(0)
        }
        
        ggiraph::girafe(
          ggobj = p,
          width_svg = 5, height_svg = 4,
          options = list(
            ggiraph::opts_hover(
              css = paste("r:4pt;",
                          "fill: #ffbf00;",
                          "stroke: #ffbf00;")),
            ggiraph::opts_selection(
              selected = preselected,
              type = "single",
              css = paste("r: 4pt;",
                          "fill: #2c3b41;",
                          "stroke: #2c3b41;")),
            ggiraph::opts_toolbar(saveaspng = FALSE)))
        
      }) # end of renderGirafe // regPlot_gps
      
    }) %>% # end of observe,
      bindEvent(list(input$gps_dur,
                     input$gps_dur_units,
                     input$gps_dti_max,
                     vals$which_question,
                     vals$active_tab == 'regime', 
                     input$device_type == "GPS"))
    
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

    }) # end of renderGirafe // regPlot_id

    ## Render variogram (svf): --------------------------------------------

    output$regPlot_svf <- ggiraph::renderGirafe({
      req(vals$data1)

      frac <- input$regVar_timeframe / 100
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
      
      taup <- fix_unit(vals$tau_p0$value[2], vals$tau_p0$unit[2])
      tauv <- fix_unit(vals$tau_v0$value[2], vals$tau_v0$unit[2])
      
      if (vals$data_type == "simulated") {
        taup_range <- tauv_range <- NULL
      } else {
        taup_min <- fix_unit(vals$tau_p0$value[1], vals$tau_p0$unit[1])
        taup_max <- fix_unit(vals$tau_p0$value[3], vals$tau_p0$unit[3])
        taup_range <- span(paste(taup_min[1], "\u2014",
                                 taup_max[1]), class = "cl-mdn")
        
        tauv_min <- fix_unit(vals$tau_v0$value[1], vals$tau_v0$unit[1])
        tauv_max <- fix_unit(vals$tau_v0$value[3], vals$tau_v0$unit[3])
        tauv_range <- span(paste(tauv_min[1], "\u2014",
                                 tauv_max[1]), class = "cl-mdn")
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
