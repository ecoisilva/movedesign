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
          
          div(id = "content_device-limitations",
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("which_limitations"),
            label = p("What limitations do you want to consider?") %>%
              tagAppendAttributes(
                class = 'label_center no-bottom'),
            choices = c("Fix success rate" = "loss",
                        "Deployment disruption" = "failure",
                        "Location error" = "error",
                        "Storage limits" = "max"),
            selected = character(0),
            checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                             no = icon("remove", lib = "glyphicon")))),
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
                  label = span("Device storage (max):",
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
                label_off = "Set schedule manually",
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
          
          div(id = "content_limitations",
              shinydashboardPlus::box(
                id = ns("devBox_loss"),
                width = NULL,
                headerBorder = FALSE,
                
                splitLayout(
                  cellWidths = c("92%", "15px"),
                  
                  p(HTML("&nbsp;"),
                    "Fix success rate (%):") %>%
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
                  inputId = ns("device_fixsuccess"),
                  label = NULL,
                  choices = seq(0, 100, by = 1),
                  from_min = 5, from_max = 100, selected = 100,
                  grid = FALSE,
                  post = "%",
                  width = "100%"),
                br(),
                uiOutput(ns("devBlock_loss"))
                
              ), # end of box // devBox_loss
              
              shinydashboardPlus::box(
                id = ns("devBox_failure"),
                width = NULL,
                headerBorder = FALSE,
                
                splitLayout(
                  cellWidths = c("92%", "15px"),
                  
                  p(HTML("&nbsp;"),
                    "Deployment disruption (%):") %>%
                    tagAppendAttributes(class = 'label_split'),
                  
                  actionButton(
                    inputId = ns("devHelp_failure"),
                    icon = icon("circle-question"),
                    label = NULL,
                    style = paste("background-color: #fff;",
                                  "color: black;",
                                  "padding: 0;",
                                  "float: right;")) %>%
                    bsplus::bs_attach_modal(
                      id_modal = "modal_failure_device")
                ),
                
                shinyWidgets::sliderTextInput(
                  inputId = ns("device_failure"),
                  label = NULL,
                  choices = seq(0, 100, by = 1),
                  from_min = 0, from_max = 90, selected = 0,
                  grid = FALSE,
                  post = "%",
                  width = "100%")
                
              ), # end of box // devBox_failure
              
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
                    bsplus::bs_attach_modal(
                      id_modal = "modal_error_device")
                ),
                
                shinyWidgets::autonumericInput(
                  inputId = ns("device_error"),
                  label = NULL,
                  currencySymbol = " meter(s)",
                  currencySymbolPlacement = "s",
                  decimalPlaces = 0,
                  minimumValue = 0,
                  value = 0,
                  wheelStep = 1)
                
              ) # end of box // devBox_error
          ), # end of div
          
          ### Other settings: ---------------------------------------------
          
          # shinydashboardPlus::box(
          #   id = ns("devBox_type"),
          #   width = NULL,
          #   headerBorder = FALSE,
          #   
          #   shinyWidgets::pickerInput(
          #     ns("est_type"),
          #     label = "Sample sizes are:",
          #     choices = c("Approximated" = 1,
          #                 "From model fit" = 2),
          #     choicesOpt = list(
          #       subtext = c("(faster, less precise)",
          #                   "(slower, more precise)")),
          #     selected = 2),
          #   
          #   uiOutput(ns("devText_sizes"))
          #   
          # ), # end of box // devBox_submit
          
          # Sample sizes: -------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Sample sizes:", class = "ttl-box"),
            id = ns("devBox_sizes"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE, closable = FALSE,
            
            fluidRow(
              column(width = 12, uiOutput(ns("devBlock_n"))),
              column(width = 12, mod_blocks_ui(ns("devBlock_Narea"))),
              column(width = 12, mod_blocks_ui(ns("devBlock_Nspeed")))
              
            ), # end of fluidRow
            
            br(),
            
            footer = tagList(
              column(
                width = 12, align = "center",
                style = "padding-left: 5px; padding-right: 5px;",
                
                shinyWidgets::pickerInput(
                  ns("est_type"),
                  label = "Sample sizes are:",
                  choices = c("Approximated" = 1,
                              "From model fit" = 2),
                  choicesOpt = list(
                    subtext = c("(faster, less precise)",
                                "(slower, more precise)")),
                  selected = 2, 
                  width = "100%"),
                
                uiOutput(ns("devText_sizes"))
                
              )) # end of footer
              
            # footer = column(
            #   width = 12, align = "right",
            #   style = "padding-left: 0px; padding-right: 0px;",
            #   
            #   shiny::actionButton(
            #     inputId = ns("devButton_save"),
            #     icon = icon("eye"),
            #     label = span("Show", span("table", class = "cl-sea")),
            #     width = "110px")) # end of footer
            
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
                             mod_blocks_ui(ns("devBlock_dur")),
                             mod_blocks_ui(ns("devBlock_dti")))
                           
                  ) # end of panel (2 out of 2)
                  
                ) # end of tabBox
              ) # end of box // devBox_pars
          ), # end of div
          
          # Specify sampling parameters: ----------------------------------
          
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
                    # div(class = "sims-irs",
                    #     shinyWidgets::sliderTextInput(
                    #       inputId = ns("dev_nsim"),
                    #       label = "Show simulation no.:",
                    #       choices = seq(1, 100, by = 1))),
                    
                    ggiraph::girafeOutput(
                      outputId = ns("devPlot_id"),
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
                      outputId = ns("devPlot_svf"),
                      width = "95%", height = "100%"),
                    
                    column(
                      width = 12, align = "center",
                      shiny::sliderInput(
                        ns("dev_fraction"),
                        label = span(
                          "Proportion of variogram plotted:") %>%
                          tagAppendAttributes(class = 'label_split'),
                        min = 0, max = 100, value = 50, step = 5,
                        post = "%",
                        width = "85%"),
                      p(),
                      shinyWidgets::awesomeCheckbox(
                        inputId = ns("dev_add_fit"),
                        label = span(
                          "Add", span("model fit", class = "cl-sea"),
                          "to variogram", icon("wrench")),
                        value = TRUE)
                      
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
                
                reactable::reactableOutput(ns("devTable")),
                uiOutput(ns("devUI_sumLegend"))
                
              )) # end of box // devBox_summary
          
      ), # end of column (center)
      
      # [bottom column] ---------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
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
    create_modal(var = "failure", id = "device"),
    create_modal(var = "error", id = "device"),
    NULL
    
  ) # end of tagList
}

#' tab_design Server Functions
#'
#' @noRd
mod_tab_design_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    rv$dev <- reactiveValues(
      is_valid = FALSE,
      type = NULL,
      dur = NULL, dti = NULL,
      n = NULL, N1 = NULL, N2 = NULL,
      tbl = NULL
    )
    
    ## If using simulated data (from scratch): ----------------------------
    
    observe({
      req(rv$active_tab == 'device')
      req(!rv$sims$grouped,
          rv$which_meta != "compare",
          rv$data_type == "simulated",
          rv$datList)
      
      if (!is.null(rv$modList0) && is.null(rv$modList)) {
        rv$modList <- rv$modList0[length(rv$modList0)]
        rv$seedList <- rv$seedList0[length(rv$modList0)]
        rv$is_isotropic <- TRUE
      }
      
    }) # end of observe
    
    ## Sampling parameters: -----------------------------------------------
    
    ### Initial sampling parameters:
    
    observe({ # GPS duration
      req(input$gps_dur, input$gps_dur_unit)
      rv$dev$dur <- data.frame(
        value = input$gps_dur, unit = input$gps_dur_unit)
      
    }) %>% # end of observe
      bindEvent(input$gps_dur)
    
    observe({ # GPS interval
      req(input$gps_dti, input$gps_dti_unit)
      rv$dev$dti <- data.frame(
        value = input$gps_dti, unit = input$gps_dti_unit)
      
    }) %>% # end of observe,
      bindEvent(input$gps_dti)
    
    observe({ # VHF duration
      req(input$vhf_dur, input$vhf_dur_unit)
      rv$dev$dur <- data.frame(
        value = input$vhf_dur, unit = input$vhf_dur_unit)
      
    }) %>% # end of observe,
      bindEvent(input$vhf_dur)
    
    observe({ # VHF interval
      req(input$vhf_dti, input$vhf_dti_unit)
      rv$dev$dti <- data.frame(
        value = input$vhf_dti, unit = input$vhf_dti_unit)
      
    }) %>% # end of observe,
      bindEvent(input$vhf_dti)
    
    
    ### Final sampling parameters:
    
    observe({
      req(input$device_type)
      rv$dur <- NULL
      rv$dti <- NULL
      
      # GPS:
      if (input$device_type == "GPS") {
        req(!is.null(input$gps_from_plot))
        
        if (input$gps_from_plot &&
            !is.null(input$devPlot_gps_selected)) {
          req(rv$gps,
              input$gps_dur,
              input$gps_dur_unit)
          
          validate(
            need(input$gps_dur %#%
                   input$gps_dur_unit > 2 %#% "days", 
                 "Duration cannot be less than 2 days."))
          
          gps <- rv$gps
          rv$dur <- data.frame(
            value = round("days" %#% gps[reg_selected(), ]$dur_sec, 1),
            unit = "days")
          
          tmpdti_notes <- gps[reg_selected(), ]$dti_notes
          tmpdti_unit <- sub('^.* ([[:alnum:]]+)$', '\\1', tmpdti_notes)
          rv$dti <- data.frame(
            value = tmpdti_unit %#% gps[reg_selected(), ]$dti,
            unit = tmpdti_unit)
          
        } else {
          req(rv$dev$dur, rv$dev$dti)
          
          rv$dur <- data.frame(value = rv$dev$dur$value,
                               unit = rv$dev$dur$unit)
          rv$dti <- data.frame(value = rv$dev$dti$value,
                               unit = rv$dev$dti$unit)
        }
        
      } else if (input$device_type == "VHF") {
        
        if (!is.null(input$devPlot_vhf_selected)) {
          req(rv$vhf,
              input$vhf_dti,
              input$vhf_dti_unit)
          
          rv$dur <- data.frame(
            value = rv$vhf[reg_selected(), ]$dur,
            unit = rv$vhf[reg_selected(), ]$dur_unit)
          
          rv$dti <- data.frame(
            value = input$vhf_dti,
            unit = input$vhf_dti_unit)
          
        } else {
          req(rv$dev$dur, rv$dev$dti)
          
          rv$dur <- data.frame(
            value = rv$dev$dur$value,
            unit = rv$dev$dur$unit)
          rv$dti <- data.frame(
            value = rv$dev$dti$value,
            unit = rv$dev$dti$unit)
        }
        
      } # end of if ()
    }) # end of observe
    
    ## Sample sizes: ------------------------------------------------------
    
    observe({
      req(rv$active_tab == 'device',
          rv$which_question,
          rv$tau_p[[1]],
          rv$dur,
          rv$dti,
          input$est_type)
      
      n <- NULL
      N1 <- NULL
      
      taup <- rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      
      dur <- rv$dur$value %#% rv$dur$unit
      dti <- rv$dti$value %#% rv$dti$unit
      
      t0 <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      n <- length(t0)
      rv$dev$n <- list(n)
      
      if (req(input$device_fixsuccess) < 100) {
        n_loss <- round(n * (1 - input$device_fixsuccess/100), 0)
        n <- length(t0) - n_loss
        rv$lost <- data.frame(
          perc = 1 - input$device_fixsuccess/100,
          n = n_loss)
      }
      
      if (input$est_type == 1) {
        rv$is_fitted <- "No"
        N1 <- min(dur/taup, n)
        
        rv$dev$N1 <- N1
        
      } else if (input$est_type == 2 && !is.null(rv$simfitList)) {
        req(rv$simList, rv$simfitList)
        req(length(rv$simList) == length(rv$simfitList))
        
        rv$is_fitted <- "Yes"
        
        rv$dev$n <- lapply(seq_along(rv$simList), function(x)
          n <- nrow(rv$simList[[x]]))
        
        N1 <- lapply(seq_along(rv$simList), function(x) {
          n <- nrow(rv$simList[[x]])
          tmpnms <- names(summary(rv$simfitList[[x]])$DOF)
          N1 <- summary(rv$simfitList[[x]])$
            DOF[grep('area', tmpnms)][[1]]
          return(N1)
        })
        
        rv$dev$N1 <- N1
      }
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'device',
          rv$which_question,
          rv$tau_v[[1]],
          rv$dur,
          rv$dti,
          input$est_type)
      
      n <- NULL
      N2 <- NULL
      
      tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
      
      dur <- rv$dur$value %#% rv$dur$unit
      dti <- rv$dti$value %#% rv$dti$unit
      
      t0 <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      n <- length(t0)
      rv$dev$n <- list(n)
      
      if (req(input$device_fixsuccess) < 100) {
        n_loss <- round(n * (1 - input$device_fixsuccess/100), 0)
        n <- length(t0) - n_loss
        rv$lost <- data.frame(
          perc = 1 - input$device_fixsuccess/100,
          n = n_loss)
      }
      
      r <- dti / tauv
      
      if (input$est_type == 1) {
        rv$is_fitted <- "No"
        N2 <- ifelse(dti > tauv,
                     ifelse(dti > 3 * tauv, 0,
                            (n) / (r)^r * (r)),
                     (n) / tauv * dti)
        
        rv$dev$N2 <- N2
        
      } else if (input$est_type == 2 && !is.null(rv$simfitList)) {
        req(rv$simList, rv$simfitList)
        req(length(rv$simList) == length(rv$simfitList))
        rv$is_fitted <- "Yes"
        
        rv$dev$n <- lapply(seq_along(rv$simList), function(x)
          n <- nrow(rv$simList[[x]]))
        
        N2 <- lapply(seq_along(rv$simList), function(x) {
          n <- nrow(rv$simList[[x]])
          tmpnms <- names(summary(rv$simfitList[[x]])$DOF)
          N2 <- summary(rv$simfitList[[x]])$DOF[grep('speed', tmpnms)][[1]]
          return(N2)
        })
        
        rv$dev$N2 <- N2
      }
      
    }) # end of observe
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at start: --------------------------------------------
    
    boxnames <- c("gps_device", "vhf_device", "type",
                  "loss",
                  "error",
                  "failure",
                  "sampling",
                  "sims",
                  "sizes",
                  "summary",
                  "misc")
    
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("devBox_", boxnames[i]))
    }
    
    shinyjs::hide(id = "which_limitations")
    shinyjs::hide(id = "min_frq")
    
    device_inputs <- reactive({
      list(input$gps_dur,
           input$gps_dur_unit,
           input$gps_dti_max,
           input$vhf_dur,
           input$vhf_dur_unit,
           input$vhf_dti,
           input$vhf_dti_unit,
           input$gps_from_plot,
           input$devPlot_gps_selected,
           input$devPlot_vhf_selected)
    })
    
    observe({
      req(rv$active_tab == 'device')
      shinyjs::enable("devButton_run")
    }) %>% bindEvent(device_inputs())
    
    ## Reveal boxes: ------------------------------------------------------
    
    observe({ ### Reveal species & device parameters box:
      req(rv$active_tab == 'device')
      
      if (!is.null(rv$tau_p[[1]]) & !is.null(rv$tau_v[[1]])) {
        shinyjs::show(id = "devBox_pars")
      } else { shinyjs::hide(id = "devBox_pars") }
      
    }) # end of observe
    
    observe({ ### Reveal sample sizes box:
      
      if (rv$dev$is_valid) {
        shinyjs::show(id = "devBox_sizes")
      } else { shinyjs::hide(id = "devBox_sizes") }
      
    }) %>% # end of observe,
      bindEvent(rv$dev$is_valid)
    
    observe({ ## Reveal summary box:
      
      if (!is.null(rv$dev$tbl)) {
        shinyjs::show(id = "devBox_summary")
      } else { shinyjs::hide(id = "devBox_summary") }
      
    }) # %>% # end of observe,
      # bindEvent(input$devButton_save)
    
    ## Update device settings: --------------------------------------------
    
    observe({
      req(input$device_type)
      rv$device_type <- input$device_type
      
      shinyjs::show(id = "devBox_sampling")
      shinyjs::show(id = "which_limitations")
      
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
      
      # rv$dev$type <- input$device_type
      
    }) %>% # end of observe,
      bindEvent(input$device_type)
    
    ## Update device limitations: -----------------------------------------
    
    observe({
      req(rv$which_meta, input$device_type)
      
      opt_limits <- NULL
      
      switch(
        input$device_type,
        GPS = {
          if (rv$which_meta == "none")
            opt_limits <- c("Fix success rate" = "loss",
                            "Location error" = "error",
                            "Storage limits" = "max")
          else
            opt_limits <- c("Fix success rate" = "loss",
                            "Deployment disruption" = "failure",
                            "Location error" = "error",
                            "Storage limits" = "max")
        },
        VHF = {
          if (rv$which_meta == "none")
            opt_limits <- c("Fix success rate" = "loss",
                            "Location error" = "error")
          else
            opt_limits <- c("Fix success rate" = "loss",
                            "Deployment disruption" = "failure",
                            "Location error" = "error")
        },
        stop(paste0("No handler for ", input$device_type, "."))
        
      ) # end of switch
      
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "which_limitations",
        choices = opt_limits,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                         no = icon("remove", lib = "glyphicon")))
      
    }) %>% # end of observe,
      bindEvent(input$device_type)
    
    observe({
      rv$which_limitations <- input$which_limitations
      
      if ("loss" %in% input$which_limitations) {
        shinyjs::show(id = "devBox_loss")
      } else { shinyjs::hide(id = "devBox_loss") }
      
      if ("failure" %in% input$which_limitations) {
        shinyjs::show(id = "devBox_failure")
      } else { shinyjs::hide(id = "devBox_failure") }
      
      if ("error" %in% input$which_limitations) {
        shinyjs::show(id = "devBox_error")
      } else { shinyjs::hide(id = "devBox_error") }
      
      if ("max" %in% input$which_limitations) {
        shinyjs::show(id = "device_max")
      } else { shinyjs::hide(id = "device_max") }
      
    }) %>% # end of observe,
      bindEvent(input$which_limitations)
    
    observe({
      req(rv$active_tab == 'device')
      
      if (is.null(input$which_limitations)) {
        shinyjs::hide(id = "devBox_loss")
        shinyjs::hide(id = "devBox_failure")
        shinyjs::hide(id = "devBox_error")
        shinyjs::hide(id = "device_max")
        
        shinyWidgets::updateSliderTextInput(
          session = session,
          inputId = "device_fixsuccess",
          selected = 100)
        
        shinyWidgets::updateSliderTextInput(
          session = session,
          inputId = "device_failure",
          selected = 0)
        
        shinyWidgets::updateAutonumericInput(
          session = session,
          inputId = "device_error",
          value = 0)
        
        shinyWidgets::updateAutonumericInput(
          session = session,
          inputId = "device_max",
          value = 1e6)
      } else {
        req(input$which_limitations)
        shinyjs::enable("devButton_run")
      }
      
    }) # end of observe
    
    ## Update based on number of simulations: -----------------------------
    
    # observe({
    #   req(rv$simList,
    #       rv$active_tab == 'device')
    #   rv$dev_nsim <- 1
    #   
    #   if (length(rv$simList) == 1) {
    #     shinyjs::hide(id = "dev_nsim")
    #     
    #   } else {
    #     shinyjs::show(id = "dev_nsim")
    #     div(class = "sims-irs",
    #         shinyWidgets::updateSliderTextInput(
    #           session = session,
    #           inputId = "dev_nsim",
    #           label = "Show simulation no.:",
    #           choices = seq(1, length(rv$simList), by = 1),
    #           selected = length(rv$simList)))
    #   }
    #   
    # }) %>% # end of observer
    #   bindEvent(rv$simList)
    # 
    # observe({
    #   req(rv$simList,
    #       input$hr_nsim >= 1,
    #       rv$active_tab == 'device')
    #   
    #   int <- round(input$dev_nsim, 0)
    #   if (int %in% seq(1, length(rv$simList), 1))
    #     rv$dev_nsim <- input$dev_nsim
    #   
    # }) %>% # end of observer,
    #   bindEvent(input$dev_nsim)
    
    ## Render validate buttons: -------------------------------------------
    
    output$devButton_gps <- renderUI({
      
      if (rv$dev$is_valid) {
        shiny::actionButton(
          inputId = ns("validate_gps"),
          icon =  icon("circle-check"),
          label = "Validated!",
          width = "100px",
          class = "btn-info")
      } else {
        shiny::actionButton(
          inputId = ns("validate_gps"),
          icon =  icon("wand-magic-sparkles"),
          label = "Validate",
          width = "100px",
          class = "btn-danger")
      }
      
    }) # end of renderUI // devButton_gps
    
    output$devButton_vhf <- renderUI({
      
      if (rv$dev$is_valid) {
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
    
    ## Render schedule text: ----------------------------------------------
    
    writing_schedule <- reactive({
      req(rv$dur, rv$dti)
      
      out_dur <- fix_unit(rv$dur$value, rv$dur$unit, convert = TRUE)
      out_dti <- fix_unit(rv$dti$value, rv$dti$unit)
      
      dur <- out_dur$value
      dur_unit <- out_dur$unit
      
      dti <- out_dti$value
      dti_unit <- out_dti$unit
      
      dur_day <- fix_unit(rv$dur$value, rv$dur$unit)
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
      
    }) %>% # end of reactive, writing_schedule(),
      debounce(50)
    
    output$devText <- renderUI(writing_schedule())
    
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
          input$gps_dur_unit != rv$dev$dur$unit)
      
      new_dur <- sigdigits(
        input$gps_dur_unit %#% input$gps_dur %#% rv$dev$dur$unit, 3)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "gps_dur",
        value = new_dur)
      
    }) %>% # end of observe,
      bindEvent(input$gps_dur_unit)
    
    
    observe({
      req(input$gps_dti,
          input$gps_dti_unit != rv$dev$dti$unit)
      
      new_dti <- sigdigits(
        input$gps_dti_unit %#% input$gps_dti %#% rv$dev$dti$unit, 3)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "gps_dti",
        value = new_dti)
      
    }) %>% # end of observe,
      bindEvent(input$gps_dti_unit)
    
    
    observe({
      req(input$vhf_dur,
          input$vhf_dur_unit != rv$dev$dur$unit)
      
      new_dur <- sigdigits(
        input$vhf_dur_unit %#% input$vhf_dur %#% rv$dev$dur$unit, 3)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "vhf_dur",
        value = new_dur)
      
    }) %>% # end of observe,
      bindEvent(input$vhf_dur_unit)
    
    
    observe({
      req(input$vhf_dti,
          input$vhf_dti_unit != rv$dev$dti$unit)
      
      new_dti <- sigdigits(
        input$vhf_dti_unit %#% input$vhf_dti %#% rv$dev$dti$unit, 3)
      
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
            
            #### 1.1. Select sampling schedule:
            
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
                width = "100%", height = "50%")
            },
            
            uiOutput(ns("devPlotLegend")),
            uiOutput(ns("devText"))
            
          ) # end of column (UI)
          
        }, # end of GPS
        
        VHF = {  ### 2. VHF transmitter:
          
          column(
            align = "center", width = 12,
            
            # p("Here, you can vizualize the impact of different",
            #   "tracking schedules on",
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
              inputId = ns("devButton_run"),
              icon =  icon("bolt"),
              label = "Run",
              width = "120px",
              class = "btn-primary")
            
          )) # end of tagList (footer)
          
          # tagList(
          #   column(width = 9, align = "left",
          #          style = "padding-left: 0px; padding-right: 0px;",
          #          div(id = "sims-footer",
          #              uiOutput(ns("devButton_gps"), inline = TRUE),
          #              HTML("&nbsp;"),
          #              shiny::actionButton(
          #                inputId = ns("devButton_run"),
          #                icon =  icon("bolt"),
          #                label = "Run",
          #                width = "80px",
          #                class = "btn-primary"))),
          #   
          #   column(width = 3, align = "right",
          #          style = "padding-left: 0px; padding-right: 0px;",
          #          
          #          shiny::actionButton(
          #            inputId = ns("devButton_repeat"),
          #            label = "Repeat",
          #            icon = icon("repeat"),
          #            class = "btn-info",
          #            width = "90px"))
          #   
          # ) # end of tagList (footer)
        },
        
        VHF = { ### VHF transmitter:
          
          tagList(column(
            width = 12, align = "right",
            style = "padding-right: 5px;",
            
            uiOutput(ns("devButton_vhf"), inline = TRUE),
            HTML("&nbsp;"),
            shiny::actionButton(
              inputId = ns("devButton_run"),
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
    
    output$devPlotLegend <- renderUI({
      req(rv$which_question, input$gps_from_plot)
      if (length(rv$which_question) > 1) {
        req(rv$tau_p[[1]], rv$tau_v[[1]])
        
        ui <- tagList(
          fontawesome::fa("circle-exclamation", fill = pal$dgr),
          span("Note:", class = "help-block-note"), 
          "The primary axis is the", 
          wrap_none(span("sampling duration", col = "black"), end = ","),
          "(points), and the secondary axis (lines) are",
          "the expected effective sample sizes (roughly estimated)",
          "\u2014", span("N[area]", class = "cl-sea"),
          "and", span("N[speed]", class = "cl-grn-d"), "\u2014",
          "for each sampling design;",
          wrap_none(
            "true N values may differ", color = pal$dgr, "."))
        
      } else {
        
        switch(
          rv$which_question,
          "Home range" = {
            req(rv$tau_p[[1]])
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "The secondary axis (lines) are",
              "the expected effective sample sizes (roughly estimated)",
              "\u2014", span("N[area]", class = "cl-sea"), "\u2014",
              "for each sampling design;",
              wrap_none(
                "true N may differ", color = pal$dgr, "."))
          },
          "Speed & distance" = {
            req(rv$tau_v[[1]])
            ui <- tagList(
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"), 
              "The secondary axis (lines) are",
              "the expected effective sample sizes (roughly estimated)",
              "\u2014", span("N[speed]", class = "cl-dgr"), "\u2014",
              "for each sampling design;",
              wrap_none(
                "true N may differ", color = pal$dgr, "."))
          },
          stop(paste0("No handler for ",
                      rv$which_question, "."))
        )
      }
      
      ui <- span(class = "help-block", ui,
                 "Sampling interval (x axis) is set to",
                 "logarithmic scale to show these",
                 "values more clearly.")
      
      return(ui)
      
    }) # end of renderUI, "devPlotLegend"
    
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
          "update automatically. Click the",
          icon("bolt", class = "cl-mdn"),
          wrap_none(span("Run", class = "cl-mdn")),
          "button to recalculate them.")
      }
    }) # end of reactive, info_sizes
    
    output$devText_sizes <- renderUI({
      req(input$est_type)
      info_sizes()
    })
    
    ## Add note to summary table: -----------------------------------------
    
    output$devUI_sumLegend <- renderUI({
      req(nrow(rv$dev$tbl) > 1)
      
      n_diff <- rv$dev$tbl %>% 
        dplyr::select(.data$dur, .data$dti) %>% 
        dplyr::distinct() %>% nrow()
      
      ui <- ""
      if (n_diff > 1) {
        ui <- tagList(
          p(style = "margin-top: 22px;"),
          span(class = "help-block",
               
               fontawesome::fa("circle-exclamation", fill = pal$dgr),
               span("Note:", class = "help-block-note"), 
               "If multiple combinations of sampling parameters were",
               "tested, only the last set selected will be used for",
               "further analyses."))
      }
      
      return(ui)
      
    }) %>% # end of renderUI, "devUI_sumLegend"
      bindEvent(rv$dev$tbl)
    
    # ALERTS --------------------------------------------------------------
    
    # Alert if GPS input values are not valid:
    
    observe({
      req(rv$active_tab == 'device',
          input$device_type == "GPS",
          rv$dev$dur,
          rv$alert_active)
      
      min_dur <- ifelse(
        rv$dev$dur$value == 0,
        TRUE, rv$dev$dur$value %#% 
          rv$dev$dur$unit <= 2 %#% "days")
      
      max_dur <- ifelse(
        rv$dev$dur$value == 0,
        TRUE, rv$dev$dur$value %#%
          rv$dev$dur$unit > 10 %#% "years")
      
      if ((min_dur || max_dur) && rv$alert_active) {
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
            rv$alert_active <- x
          },
          html = TRUE,
          showCancelButton = TRUE,
          cancelButtonText = "Dismiss forever",
          confirmButtonText = "OK",
          confirmButtonCol = pal$mdn,
          size = "s") }
      
    }) # end of observe
    
    ## Schedule... --------------------------------------------------------
    ### ...GPS & Satellite loggers: ---------------------------------------
    
    # Alert if research question(s)/data are NOT available:
    
    observe({
      req(rv$active_tab == 'device')
      next_step <- TRUE
      rv$lost <- NULL
      
      # Check if questions were set:
      if (is.null(rv$which_question)) {
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
      
      if (is.null(rv$datList))
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

      rv$dev$is_valid <- FALSE
      if ((!input$gps_from_plot && is.null(input$gps_dti)) ||
          (input$gps_from_plot && is.null(input$devPlot_gps_selected))
      ) {
        
        shinyalert::shinyalert(
          title = "No schedule selected",
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
        req(rv$dur, rv$dti)
        
        dur <- rv$dur$value %#% rv$dur$unit
        
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
              rv$dev$is_valid <- x },
            html = TRUE,
            size = "xs")
          
        } else {
          
          rv$dev$is_valid <- TRUE
          rv$is_analyses <- FALSE
          
        } # end of if (rv$dur < 1 %#% "days")
      }
      
    }) %>% # end of observer,
      bindEvent(input$validate_gps)
    
    # Alert if number of locations is higher than GPS allows:
    
    observe({
      req(rv$dev$is_valid,
          rv$dev$n,
          rv$which_limitations,
          input$device_max)
      
      # Check GPS storage limit (if selected):
      
      if (("max" %in% rv$which_limitations) &&
          rv$dev$n > rv$storage) {
        
        shinyalert::shinyalert(
          title = "Not enough GPS storage",
          type = "warning",
          text = tagList(span(
            "GPS storage limit",
            wrap_none("(", scales::label_comma()(rv$storage),
                      " locations)"),
            "is lower than the absolute sample size",
            wrap_none("(", scales::label_comma()(rv$dev$n),
                      " locations).")
          )),
          html = TRUE,
          size = "xs")
        
        rv$dev$is_valid <- FALSE
        
      } else {
        
        rv$dev$is_valid <- TRUE
        rv$is_analyses <- FALSE
        
      } # end of storage limit if () statement
      
    }) %>% # end of observer,
      bindEvent(input$validate_gps)
    
    ### ...VHF transmitter: -----------------------------------------------
    
    # Alert if user did NOT select fix rate before validation:
    
    observe({
      rv$lost <- NULL
      
      # Check if inputs were selected:
      
      if (is.null(input$vhf_dti)) {
        
        shinyalert::shinyalert(
          title = "No schedule selected",
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
        req(rv$dur, rv$dti)
        
        dur <- rv$dur$value %#% rv$dur$unit
        
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
              rv$dev$is_valid <- x },
            html = TRUE,
            size = "xs")
          
        } else {
          
          rv$dev$is_valid <- TRUE
          rv$is_analyses <- FALSE
          
        } # end of if (rv$dur < 1 %#% "days")
      }
      
    }) %>% # end of observer,
      bindEvent(input$validate_vhf)
    
    # Schedule verified:
    
    observe({
      updateTabsetPanel(
        session,
        inputId = "devTabs_pars",
        selected = "tab_device_1-devPanel_device")
      
      shinyjs::show(id = "devBox_type")
      
      shinyFeedback::showToast(
        type = "success",
        message = "Schedule validated!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"
        )
      )
      
    }) %>% bindEvent(req(rv$dev$is_valid))
    
    ## Sample sizes... ----------------------------------------------------
    
    # observe({
    #   req(rv$dev$n[[1]])
    #   
    #   if (rv$dev$n[[1]] > 20000)
    #     shinyalert::shinyalert(
    #       inputId = "alert_n_small",
    #       title = "Warning",
    #       text = tagList(span(
    #         "You are about to simulate a dataset with over",
    #         scales::label_comma()(rv$dev$n[[1]]), "locations.",
    #         "This can take a considerable amount of time to run",
    #         "on certain devices (several hours or days).",
    #         "Do you wish to proceed?"
    #       )),
    #       showConfirmButton = TRUE,
    #       showCancelButton = TRUE,
    #       confirmButtonText = "Proceed",
    #       cancelButtonText = "Cancel",
    #       callbackR = function(x) {
    #         rv$dev$is_valid <- x },
    #       html = TRUE,
    #       size = "xs")
    #   
    # }, priority = 0) %>% # end of observe,
    #   bindEvent(input$devButton_run)
    
    # SIMULATIONS ---------------------------------------------------------
    ## Simulating GPS battery life: ---------------------------------------
    
    simulating_gps <- reactive({
      req(rv$dev$dur,
          input$gps_dti_max,
          rv$seed0)
      
      add_N1 <- FALSE
      add_N2 <- FALSE
      a1 <- b1 <- a2 <- b2 <- NULL
      
      dur <- rv$dev$dur$value %#% rv$dev$dur$unit
      req(dur > 2 %#% "days")
      
      if ((dur) < 10 %#% "days") {
        input_cutoff <- .5 %#% "days"
      } else { input_cutoff <- 2 %#% "days" }
      
      gps_sim <- simulate_gps(
        data =  movedesign::fixrates,
        b_max = rv$dev$dur$value,
        b_unit = rv$dev$dur$unit,
        cutoff = input_cutoff,
        dti_max = input$gps_dti_max,
        seed = rv$seed0,
        set_seed = rv$overwrite_active)
      
      if (all(gps_sim$dur_sec == 0)) return(NULL)
      
      # Display only values with duration
      # (plus three additional rows):
      
      if ("Y" %in% gps_sim$cutoff) {
        
        tmp <- gps_sim %>%
          dplyr::filter(.data$cutoff == "Y") %>%
          dplyr::pull(.data$id)
        
        if (length(tmp) > 3)
          gps_sim <- gps_sim %>% 
            dplyr::filter(id <= (min(tmp) + 3))
      }
      
      req(gps_sim)
      
      
      if (!("N" %in% gps_sim$cutoff)) {
        pal_values <- pal$dgr
      } else { pal_values <- c(pal$mdn, pal$dgr) }
      
      gps_sim$dur <- rv$dev$dur$unit %#% gps_sim$dur_sec
      dur_unit <- rv$dev$dur$unit
      
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
      
      if (!is.null(rv$which_question)) {
        
        add_N1 <- FALSE
        add_N2 <- FALSE
        
        if ("Home range" %in% rv$which_question &&
            !is.null(rv$tau_p[[1]])) {
          
          req(rv$is_valid)
          taup <- rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
          gps_sim$N_area <- gps_sim$dur_sec / taup
          
          ylim.prim <- c(0, max(gps_sim$dur))
          ylim.sec <- c(0, max(gps_sim$N_area))
          b1 <- diff(ylim.prim)/diff(ylim.sec)
          a1 <- b1 * (ylim.prim[1] - ylim.sec[1])
          
          add_N1 <- TRUE
          
        } # end of N1
        
        if ("Speed & distance" %in% rv$which_question &&
            !is.null(rv$tau_v[[1]])) {
          
          req(rv$is_valid)
          tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
          gps_sim$N_speed <- NA
          
          for (i in 1:nrow(gps_sim)) {
            dti <- gps_sim$dti[i]
            r <- dti / tauv
            
            n <- gps_sim$n[i]
            n_loss <- ifelse(is.null(rv$lost$n), 0, rv$lost$n)
            
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
        
        if (length(rv$which_question) > 1 &&
            !is.null(rv$tau_v[[1]]) &&
            !is.null(rv$tau_v[[1]])) {
          req(rv$is_valid)
          
          ylim.prim <- c(0, max(gps_sim$dur))
          ylim.sec <- c(0, max(gps_sim$N_speed))
          b1 <- diff(ylim.prim)/diff(ylim.sec)
          a1 <- b1 * (ylim.prim[1] - ylim.sec[1])
          
        } # end of both N1 and N2
        
      } # end of !is.null(rv$which_question)
      
      rv$gps <- gps_sim
      
      return(list(
        gps = gps_sim,
        add_N1 = add_N1,
        add_N2 = add_N2,
        axis1 = data.frame(a1 = a1, b1 = b1),
        axis2 = data.frame(a2 = a2, b2 = b2),
        pal = pal_values
      ))
      
    }) # end of reactive, simulating_gps()
    
    ## Simulating new conditional data: -----------------------------------
    
    estimating_time <- reactive({
      
      loading_modal("Calculating run time")
      out_time <- guess_time(data = rv$simList,
                             error = rv$error,
                             parallel = rv$parallel)
      
      shinybusy::remove_modal_spinner()
      return(out_time)
      
    }) %>% # end of reactive, estimating_time()
      bindCache(c(rv$tau_p[[1]],
                  rv$tau_v[[1]],
                  rv$dur, 
                  rv$dti))
    
    fit_model <- reactive({
      
      simList <- rv$simList
      simfitList <- fitting_model(simList,
                                  .dur = rv$dur,
                                  .dti = rv$dti,
                                  .tau_p = rv$tau_p,
                                  .tau_v = rv$tau_v,
                                  .error_m = rv$error,
                                  .check_sampling = TRUE,
                                  .rerun = TRUE)
      
      if (is.null(simfitList)) {
        msg_log(
          style = "danger",
          message = paste0(
            "Model selection ", msg_danger("failed"), "."))
        return(NULL)
      }
      
      return(simfitList)
      
    }) %>% # end of reactive, fit_model()
      bindCache(rv$datList,
                rv$simList,
                rv$dur,
                rv$dti,
                rv$seed0)
    
    ### Run simulation (nsims = 1): ---------------------------------------
    
    observe({
      req(rv$which_question,
          rv$datList,
          rv$sigma,
          rv$dur, 
          rv$dti,
          rv$dev$is_valid)
      
      if (rv$data_type != "simulated") req(rv$fitList)
      else req(rv$modList)
      
      if ("Home range" %in% rv$which_question) req(rv$tau_p[[1]])
      if ("Speed & distance" %in% rv$which_question) req(rv$tau_v[[1]])
      
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
      
      if (rv$which_meta == "compare")
        req(length(rv$tau_p) == 3, rv$groups)
      if (rv$add_ind_var) req(rv$meanfitList)
      
      start <- Sys.time()
      simList <- simulating_data(rv, rv$seed0)
      
      if (!rv$grouped) {
        rv$seedList <- list(rv$seed0)
        names(simList) <- c(rv$seed0)
      } else {
        rv$seedList <- list(rv$seed0, rv$seed0 + 1)
        rv$groups[[2]] <- list(A = as.character(rv$seed0), 
                               B = as.character(rv$seed0 + 1))
        names(simList) <- c(rv$seed0, rv$seed0 + 1)
      }
      
      rv$nsims <- NULL # length(simList)
      rv$needs_fit <- TRUE
      rv$is_analyses <- FALSE
      rv$hr_completed <- FALSE
      rv$sd_completed <- FALSE
      
      class(simList) <- c(class(simList), "movedesign")
      
      # If there is data loss:
      
      if (!is.null(input$device_fixsuccess))
        if (req(input$device_fixsuccess) < 100) {
          
          simList <- lapply(simList, function(x) {
            
            to_keep <- round(nrow(x) * (1 - rv$lost$perc), 0)
            to_keep_vec <- sort(sample(seq_len(nrow(x)),
                                       to_keep, replace = FALSE))
            x[to_keep_vec, ] })
          
        } # end of input$device_fixsuccess
      
      # If there is tag failure:
      
      rv$fail_prob <- NULL
      rv$dev_failed <- c()
      if (!is.null(input$device_failure))
        if (req(input$device_failure) > 0) {
          rv$fail_prob <- input$device_failure/100
          rv$dev_failed <- rep(FALSE, length(simList))
        }
      
      # If there are errors associated with each location:
      
      rv$error <- NULL
      if (!is.null(input$device_error))
        if (req(input$device_error) > 0) {
          rv$error <- input$device_error
          simList <- lapply(simList, function(x) {
            
            x$error_x <- x$error_y <- stats::rnorm(
              nrow(x), mean = 0, sd = input$device_error)
            
            x$HDOP <- sqrt(2) * sqrt(x$error_x^2 + x$error_y^2) /
              sqrt(-2 * log(0.05))
            
            x$original_x <- x$x
            x$original_y <- x$y
            x[c("x", "y")] <- x[c("x", "y")] + c(x$error_x, x$error_y)
            
            ctmm::uere(x) <- 1
            
            # ext <- extent(x[1:15,])
            # ctmm::plot(x[1:15,], error = 1, ext = ext)
            # ctmm::plot(x[1:15,], error = 0,
            #            cex = 1, pch = 20, add = TRUE)
            # new_x <- x
            # new_x$x <- new_x$original_x
            # new_x$y <- new_x$original_y
            # ctmm::plot(new_x[1:15,], error = 0,
            #            cex = 1, pch = 20, col = "blue", add = TRUE)
            
            return(x) })
          
        } # end of input$device_error
      
      rv$simList <- simList
      rv$dev$n <- lapply(simList, function(x) nrow(x))
      
      if (is.null(rv$simList)) {
        msg_log(
          style = "danger",
          message = paste0(
            "Simulation ", msg_danger("failed"), "."))
      } else {
        msg_log(
          style = "success",
          message = paste0("Simulation ",
                           msg_success("completed"), "."),
          run_time = difftime(Sys.time(), start, units = "sec"))
      }
      shinybusy::remove_modal_spinner()
      req(rv$simList)
      
      expt <- estimating_time()
      rv$expt <- expt
      
      rv$dev$confirm_time <- NULL
      if ((as.numeric(expt$max) %#% expt$unit) > 900) {
        
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            rv$dev$confirm_time <- x
          },
          text = tagList(span(
            "Expected run time for the next phase", br(),
            "is approximately",
            wrap_none(span(
              expt$range, class = "cl-dgr"), ".")
          )),
          type = "warning",
          showCancelButton = TRUE,
          cancelButtonText = "Stop",
          confirmButtonCol = pal$mdn,
          confirmButtonText = "Proceed",
          html = TRUE
        )
      } else rv$dev$confirm_time <- TRUE
      
    }, priority = 1) %>% # end of observe,
      bindEvent(input$devButton_run)
    
    observe({
      req(rv$dev$confirm_time)
      fitList <- NULL
      
      if (is.null(rv$which_m) && rv$which_meta != "none") {
        proceed <- FALSE
        shinyalert::shinyalert(
          type = "error",
          title = "Deployment type unavailable",
          text = tagList(span(
            "You have not specified the deployment type",
            "in the", icon("house", class = "cl-blk"),
            span("Home", class = "cl-blk"), "tab.",
            " Please set either a fixed or",
            "minimum number of tags before clicking the", 
            icon("bolt", class = "cl-sea"),
            span("'Simulate'", class = "cl-sea"), "button."
          )),
          html = TRUE,
          size = "s")
        req(proceed)
      }
      
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
          p(rv$expt$range,
            style = paste("background-color: #eaeaea;",
                          "color: #009da0;",
                          "font-size: 16px;",
                          "text-align: center;",
                          "margin-top: -40px;")),
          p()
        )) # end of text
      ) # end of modal
      
      start <- Sys.time()
      fitList <- fit_model()
      time_fit <- difftime(Sys.time(), start, units = "secs")
      
      if (!is.null(fitList)) {
        
        shinyjs::show(id = "devBox_sims")
        
        msg_log(
          style = 'success',
          message = paste0("Model fit ",
                           msg_success("completed"), "."),
          run_time = time_fit)
        
        rv$needs_fit <- FALSE
        rv$simfitList <- fitList
        
        lapply(seq_along(rv$simfitList), function(x) {
          
          group <- 1
          if (rv$grouped) {
            
            get_group <- function(seed, groups) {
              if (as.character(seed) %in% groups[["A"]]) { 
                return("A") } else { return("B") }
            }
            
            group <- get_group(rv$seedList[[x]], rv$groups[[2]])
          }
          
          if (rv$add_ind_var) {
            tau_p <- extract_pars(
              emulate_seeded(rv$meanfitList[[group]], 
                             rv$seedList[[x]]),
              "position")[[1]]
            tau_v <- extract_pars(
              emulate_seeded(rv$meanfitList[[group]], 
                             rv$seedList[[x]]),
              "velocity")[[1]]
            sigma <- extract_pars(
              emulate_seeded(rv$meanfitList[[group]], 
                             rv$seedList[[x]]),
              "sigma")[[1]]
            
          } else {
            tau_p <- rv$tau_p[[group]]
            tau_v <- rv$tau_v[[group]]
            sigma <- rv$sigma[[group]]
          }
          
          rv$dev$tbl <<- rbind(
            rv$dev$tbl,
            .build_tbl(
              device = rv$device_type,
              group = if (rv$grouped) group else NA,
              data = rv$simList[[x]],
              seed = rv$seedList[[x]],
              obj = rv$simfitList[[x]],
              tau_p = tau_p,
              tau_v = tau_v,
              sigma = sigma))
        
        })
        
        rv$report_dev_yn <- TRUE
        
        # (Re)initialize for every new set of sampling parameters:
        
        rv$akdeList <- list()
        rv$ctsdList <- list()
        rv$pathList <- list()
        
        rv$meta_tbl <- NULL
        rv$hr$tbl <- NULL
        rv$sd$tbl <- NULL
        
        rv$err_prev <- list("hr" = rep(1, 10),
                            "ctsd" = rep(1, 10))
        
        shinyjs::disable("devButton_run")
        shinyjs::enable("devButton_save")
        
      } # end of if (), !is.null(fitList)
      
      if (!rv$tour_active) {
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
      bindEvent(rv$dev$confirm_time)
    
    # PLOTS ---------------------------------------------------------------
    ## Plotting GPS battery life: -----------------------------------------
    
    reg_selected <- reactive({
      if (input$device_type == "GPS") return(input$devPlot_gps_selected)
      if (input$device_type == "VHF") return(input$devPlot_vhf_selected)
      rv$dev$is_valid <- FALSE
    })
    
    output$devPlot_gps <- ggiraph::renderGirafe({
      req(rv$active_tab == 'device',
          rv$dev$dur$value,
          rv$dev$dur$unit,
          input$gps_dti_max)
      
      dti_scale <- dti_yn <- NULL
      device <- simulating_gps()
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
          dplyr::pull(.data$x),
        lbls = device$gps %>%
          dplyr::filter(dti_yn == "Y") %>%
          dplyr::pull(.data$dti_scale))
      
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
          linewidth = 3, alpha = .2)
      }
      if (device$add_N2) {
        pN2 <- ggplot2::geom_line(
          data = device$gps,
          mapping = ggplot2::aes(
            x = .data$x,
            y = device$axis2$a2 + .data$N_speed * device$axis2$b2,
            color = "Nspeed",
            group = 1),
          linewidth = 3, alpha = .2)
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
          linewidth = 0.2) +
        
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
        
        theme_movedesign(font_available = rv$is_font) +
        ggplot2::guides(fill = "none") +
        ggplot2::theme(
          legend.position = pos,
          legend.text = ggplot2::element_text(size = 14),
          axis.title.y.right = ggplot2::element_text(angle = 90))
      
      # preselected <- character(0)
      # if (rv$tour_active) {
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
          # ggiraph::opts_hover_inv(css = "opacity:0.4;"),
          ggiraph::opts_toolbar(saveaspng = FALSE)))
      
    }) %>% # end of renderGirafe, "devPlot_gps"
      bindEvent(list(rv$dev$dur$value,
                     rv$dev$dur$unit,
                     input$gps_dti_max,
                     input$device_log,
                     rv$active_tab,
                     rv$which_question,
                     rv$overwrite_seed)) %>%
      debounce(50)
    
    ## Plotting new simulated data plot (xy): -----------------------------
    
    output$devPlot_id <- ggiraph::renderGirafe({
      req(rv$datList, rv$simList, rv$id)
      
      sim <- rv$simList[[1]]
      if (rv$data_type == "simulated") {
        dat <- rv$datList[[1]]
        dat <- dat[which(dat$t <= max(sim$t)), ]
        dat$id <- as.factor(dat$id)
      } else {
        dat <- tele_to_dt(rv$datList[rv$id])
      }
      
      ymin <- min(
        min(sim$y) - diff(range(sim$y)) * .2,
        min(dat$y) - diff(range(dat$y)) * .2)
      
      ymax <- max(
        max(sim$y) + diff(range(sim$y)) * .2,
        max(dat$y) + diff(range(dat$y)) * .2)
      
      if (rv$grouped) {
        req(rv$groups, rv$simList[[2]])
        sim_new <- rv$simList[[2]]
        ymin <- min(ymin, min(sim_new$y) - diff(range(sim_new$y)) * .2)
        ymax <- max(ymax, max(sim_new$y) + diff(range(sim_new$y)) * .2)
        
        p <- ggplot2::ggplot() +
          ggplot2::geom_point(
            dat, mapping = ggplot2::aes(
              x = .data$x,
              y = .data$y,
              fill = .data$id), 
            color = "grey90", shape = 21, size = 1.4) +
          ggplot2::scale_fill_grey(start = .4) +
          
          ggplot2::geom_path(
            sim, mapping = ggplot2::aes(
              x = .data$x, 
              y = .data$y),
            color = pal$grn,
            linewidth = .6, alpha = .8) +
          
          ggplot2::geom_path(
            sim_new, mapping = ggplot2::aes(
              x = .data$x,
              y = .data$y),
            color = pal$sea,
            linewidth = .6, alpha = .8) +
          
          ggiraph::geom_point_interactive(
            sim, mapping = ggplot2::aes(
              x = .data$x, 
              y = .data$y,
              tooltip = .data$timestamp),
            color = pal$grn,
            size = 2.5) +
          ggiraph::geom_point_interactive(
            sim_new, mapping = ggplot2::aes(
              x = .data$x,
              y = .data$y,
              tooltip = .data$timestamp),
            color = pal$sea,
            size = 2.5)
        
      } else {
        
        p <- ggplot2::ggplot() +
          ggplot2::geom_point(
            dat, mapping = ggplot2::aes(
              x = .data$x,
              y = .data$y,
              fill = .data$id), 
            color = "grey90", shape = 21, size = 1.4) +
          ggplot2::scale_fill_grey(start = .4) +
          
            ggplot2::geom_path(
              sim, mapping = ggplot2::aes(
                x = .data$x,
                y = .data$y,
                color = .data$timestamp),
              linewidth = .6, alpha = .8) +
          
            ggiraph::geom_point_interactive(
              sim, mapping = ggplot2::aes(
                x = .data$x,
                y = .data$y,
                color = .data$timestamp,
                tooltip = .data$timestamp),
              size = 2.5)
        
      }
      
      p <- p +
        
        ggplot2::scale_x_continuous(
          labels = scales::comma) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(ymin, ymax)) +
        viridis::scale_color_viridis(
          name = "Tracking time:",
          option = "D", trans = "time",
          breaks = c(min(sim$timestamp),
                     max(sim$timestamp)),
          labels = c("Start", "End")) +
        
        theme_movedesign(font_available = rv$is_font) +
        ggplot2::guides(
          fill = "none",
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
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_tooltip(use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill:#1279BF;",
                        "stroke:#1279BF;",
                        "cursor:pointer;"))))
      
    }) # end of renderGirafe, "devPlot_id"
    
    ## Plotting variogram (svf): ------------------------------------------
    
    output$devPlot_svf <- ggiraph::renderGirafe({
      req(rv$simList, rv$simfitList)
      
      # set_id <- 1
      # if (!is.null(rv$dev_nsim)) set_id <- rv$dev_nsim
      
      rv$simsvfList <- extract_svf(rv$simList,
                                   rv$simfitList, fraction = 1)
      
      if (length(rv$simList) == 1) hex_fill <- pal$dgr
      else hex_fill <- c(pal$grn, pal$sea)
      p <- plotting_svf(rv$simsvfList,
                        fill = hex_fill,
                        add_fit = ifelse(is.null(input$dev_add_fit),
                                         FALSE, input$dev_add_fit),
                        fraction = input$dev_fraction / 100,
                        font_available = rv$is_font)
      
      ggiraph::girafe(
        ggobj = suppressWarnings(ggpubr::ggarrange(plotlist = p)),
        options = list(
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_sizing(rescale = TRUE, width = .5),
          ggiraph::opts_hover(css = paste("fill: #ffbf00;",
                                          "stroke: #ffbf00;"))
        ))
      
    }) # end of renderGirafe, "devPlot_svf"
    
    # BLOCKS --------------------------------------------------------------
    ## Species parameters: ------------------------------------------------
    
    observe({
      req(rv$tau_p)
      
      mod_blocks_server(
        id = "devBlock_taup", 
        rv = rv, type = "tau", name = "tau_p",
        input_name = list(
          chr = "dev_taup0",
          html = wrap_none("Position autocorrelation ",
                           "(\u03C4", tags$sub("p"), ")")),
        input_modal = "modal_taup_dev")
    }) # end of observe
    
    observe({
      req(rv$tau_v)
      
      mod_blocks_server(
        id = "devBlock_tauv",
        rv = rv, type = "tau", name = "tau_v",
        input_name = list(
          chr = "dev_tauv0",
          html = wrap_none("Velocity autocorrelation ",
                           "(\u03C4", tags$sub("v"), ")")),
        input_modal = "modal_tauv_dev")
    }) # end of observe
    
    ## Sampling schedule: -----------------------------------------------
    
    observe({
      req(rv$active_tab == 'device')
      req(rv$datList, rv$id)
      
      mod_blocks_server(
        id = "devBlock_dur",
        rv = rv, data = rv$datList[rv$id],
        type = "dur")
      
      mod_blocks_server(
        id = "devBlock_dti", 
        rv = rv, data = rv$datList[rv$id],
        type = "dti")
      
    }) # end of observe
    
    ## Sample sizes: ----------------------------------------------------
    
    output$devBlock_n <- renderUI({
      req(rv$dev$n)
      
      perc_n <- NULL
      icon_n <- FALSE
      if (!is.null(rv$lost$perc))
        if (rv$lost$perc < 1) {
          perc_n <- span(paste0("-", rv$lost$perc, "%"))
          icon_n <- TRUE
        }
      
      sizeBlock(
        type = "n",
        percentage = perc_n,
        icon = icon_n,
        value = rv$dev$n[[1]],
        rightBorder = FALSE,
        marginBottom = TRUE)
      
    }) # end of renderUI // devBlock_n
    
    observe({
      req(rv$active_tab == 'device', rv$dev$n, rv$dev$N1)
      
      mod_blocks_server(
        id = "devBlock_Narea", 
        rv = rv, n = rv$dev$n, N = rv$dev$N1,
        type = "N", name = "area")
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'device', rv$dev$n, rv$dev$N2)
      
      mod_blocks_server(
        id = "devBlock_Nspeed", 
        rv = rv, n = rv$dev$n, N = rv$dev$N2,
        type = "N", name = "speed")
      
    }) # end of observe
    
    
    ## Data loss: -------------------------------------------------------
    
    output$devBlock_loss <- renderUI({
      req(input$device_fixsuccess < 100)
      
      n_loss <- round(rv$dev$n[[1]] *
                        (1 - input$device_fixsuccess/100), 0)
    
      parBlock(
        header = "Fixes lost:",
        value = n_loss)
      
    }) # end of renderUI // devBlock_loss
    
    # TABLES --------------------------------------------------------------
    ## Listing sampling designs: ------------------------------------------
    
    observe({
      req(rv$dev$tbl)
      
      shinyjs::show(id = "devBox_summary")
      rv$dev$tbl <- dplyr::distinct(rv$dev$tbl)
      
    }) # %>% # end of observe,
      # bindEvent(input$devButton_save)
    
    output$devTable <- reactable::renderReactable({
      req(rv$which_question, rv$dev$tbl)
      
      dt_dv <- dplyr::select(rv$dev$tbl, -c(.data$seed, .data$data))
      if (!rv$grouped) {
        dt_dv <- dplyr::select(
          dt_dv, -c(.data$group, .data$area:.data$dist_err))
      } else {
        dt_dv <- dplyr::select(
          dt_dv, -c(.data$area:.data$dist_err))
      }
      
      nms <- list(
        device = "Type",
        group = "Group",
        taup = "\u03C4\u209A",
        tauv = "\u03C4\u1D65",
        sigma = "\u03C3\u209A",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = "N (area)",
        N2 = "N (speed)",
        fit = "Fitted?")
      
      reactable::reactable(
        dt_dv,
        compact = TRUE,
        highlight = TRUE,
        striped = TRUE,
        
        defaultPageSize = 5,
        paginationType = "jump",
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 20),
        showPageInfo = FALSE,
        
        defaultColDef = reactable::colDef(
            headerClass = "rtable_header",
            align = "center",
            minWidth = 50),
        
        columns = list(
          device = reactable::colDef(
            name = nms[["device"]]),
          group = if (rv$grouped) {
            reactable::colDef(
              minWidth = 80, name = nms[["group"]]) },
          taup = reactable::colDef(
              minWidth = 100, name = nms[["taup"]],
              style = list(fontWeight = "bold")),
          tauv = reactable::colDef(
              minWidth = 100, name = nms[["tauv"]],
              style = list(fontWeight = "bold")),
          sigma = reactable::colDef(
              minWidth = 100, name = nms[["sigma"]]),
          dur = reactable::colDef(
            minWidth = 80, name = nms[["dur"]],
            style = list(fontWeight = "bold")),
          dti = reactable::colDef(
            minWidth = 80, name = nms[["dti"]],
            style = list(fontWeight = "bold")),
          n = reactable::colDef(
            name = nms[["n"]],
            style = format_num,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 0)),
          N1 = reactable::colDef(
            minWidth = 80, name = nms[["N1"]],
            style = format_num,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 1)),
          N2 = reactable::colDef(
            minWidth = 80, name = nms[["N2"]],
            style = format_num,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 1)),
          fit = reactable::colDef(
            minWidth = 70, name = nms[["fit"]])
        ))
      
    }) # end of renderReactable, "devTable"
    
    # observe({
    #   rv$dev$tbl <- NULL
    # }) %>% # end of observe,
    #   bindEvent(input$devTable_clear)
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_design_ui("tab_design_1")

## To be copied in the server
# mod_tab_design_server("tab_design_1")
