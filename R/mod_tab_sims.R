#' tab_sims UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats runif
#'
mod_tab_sims_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      # Introduction: -----------------------------------------------------
      
      div(class = div_column_main,
          
          shinydashboardPlus::box(
            
            title = span("Simulate movement data:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "file-pen",
                                   height = "21px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "var(--sea-dark)"),
            id = ns("sims_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = FALSE, closable = FALSE,
            
            column(
              align = "center", width = 12,
              
              p("Choose parameters that reflect your intended",
                "study species, then click the",
                icon("seedling", class = "cl-sea"),
                span("Generate seed", class = "cl-sea"), "and",
                icon("bolt", class = "cl-mdn"),
                HTML(paste0(span("Run simulation", class = "cl-mdn"))),
                "buttons (in that order). If needed, re-adjust any",
                "value until you achieve a simulation that behaves",
                "similarly to your study species.")
              
            ) # end of column (for text)
          ) # end of box // sims_intro
      ), # end of div (top row)
      
      # [right column] ----------------------------------------------------
      
      div(id = "sim-parameters",
          class = div_column_left,
          
          # PARAMETERS: ---------------------------------------------------
          ## Timescale parameters -----------------------------------------
          
          shinydashboardPlus::box(
            title = span("Characteristic timescales:",
                         class = "ttl-box_solid"),
            id = ns("simBox_timescales"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            
            ### Position autocorrelation:
            
            splitLayout(
              cellWidths = c("92%", "15px"),
              
              p(HTML("&nbsp;"),
                HTML(paste0("Position autocorrelation ",
                            "(\u03C4", tags$sub("p"), "):"))) %>%
                tagAppendAttributes(class = 'label_split'),
              
              actionButton(
                inputId = ns("simsHelp_taup"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_taup_sim")
            ),
            
            splitLayout(
              cellWidths = c("40%", "60%"),
              
              numericInput(
                inputId = ns("tau_p0"),
                label = NULL,
                min = 1, value = 1,
                width =  "100%"),
              
              selectInput(
                inputId = ns("tau_p0_units"),
                label = NULL,
                choices = c("Month(s)" = "months",
                            "Weeks(s)" = "weeks",
                            "Day(s)" = "days",
                            "Hour(s)" = "hours"),
                selected = "days",
                width = "100%")
              
            ), # end of splitLayout // tau_p
            
            ### Velocity autocorrelation:
            
            splitLayout(
              cellWidths = c("92%", "15px"),
              
              p(HTML("&nbsp;"),
                HTML(paste0("Velocity autocorrelation ",
                            "(\u03C4", tags$sub("v"), "):"))) %>%
                tagAppendAttributes(class = 'label_split'),
              
              actionButton(
                inputId = ns("simsHelp_tauv"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_tauv_sim")
            ),
            
            splitLayout(
              cellWidths = c("40%", "60%"),
              
              numericInput(
                inputId = ns("tau_v0"),
                label = NULL,
                min = 1, max = 500, value = 1),
              
              selectInput(
                inputId = ns("tau_v0_units"),
                label = NULL,
                choices = c("Day(s)" = "days",
                            "Hour(s)" = "hours",
                            "Minute(s)" = "minutes"),
                selected = "hours")
              
            ) # end of splitLayout // tau_v
            
          ), # end of box // simBox_timescales
          
          ## Spatial parameters -------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Other parameters", class = "ttl-box_solid"),
            id = ns("simBox_spatialscales"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            
            splitLayout(
              cellWidths = c("92%", "15px"),
              
              p(HTML("&nbsp;"),
                HTML("Semi-variance (\u03C3):")) %>%
                tagAppendAttributes(class = 'label_split'),
              
              actionButton(
                inputId = ns("simsHelp_var"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_sigma_sim")
            ),
            
            splitLayout(
              cellWidths = c("40%", "60%"),
              
              numericInput(
                inputId = ns("sigma0"),
                label = NULL,
                min = 1, max = 500, value = 1),
              
              selectInput(
                inputId = ns("sigma0_units"),
                label = NULL,
                choices = c(
                  "Square kilometers" = "square kilometers",
                  "Square meters" = "square meters",
                  "Hectares" = "ha"),
                selected = "Square kilometers")
              
            ), # end of splitLayout
            
            p(HTML("&nbsp;"),
              HTML("Movement speed (\u03BD):")) %>%
              tagAppendAttributes(class = 'label_split'),
            verbatimTextOutput(outputId = ns("sims_speed"))
            
          ), # end of box // simBox_spatialscales
          
          ## Submit parameters --------------------------------------------
          
          shinydashboardPlus::box(
            id = ns("simBox_submit"),
            width = NULL,
            headerBorder = FALSE,
            
            actionButton(inputId = ns("generateSeed"),
                         icon = icon("seedling"),
                         label = "Generate seed",
                         width = "100%",
                         class = "btn-info"),
            
            fluidRow(
              column(width = 12,
                     verbatimTextOutput(outputId = ns("seedvalue"))
              )), p(style = "padding: 0px;"),
            
            actionButton(
              inputId = ns("run_sim"),
              icon =  icon("bolt"),
              label = "Run simulation",
              width = "100%",
              class = "btn-primary")
            
          ) # end of box // simBox_submit
      ), # end of div (right column)
      
      # [center column] ---------------------------------------------------
      
      div(class = div_column_right,
          
          # Visualization: ------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Visualizing simulated data:", class = "ttl-box"),
            id = ns("simBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-7",
                tabsetPanel(
                  id = ns("simTabs_viz"),
                  
                  tabPanel(
                    value = ns("simPanel_id"),
                    title = tagList(
                      icon("paw", class = "cl-sea"),
                      span("Data", class = "ttl-panel")),
                    br(),
                    
                    ggiraph::girafeOutput(
                      outputId = ns("simPlot_id"),
                      width = "100%", height = "100%") %>%
                      shinycssloaders::withSpinner(
                        type = getOption("spinner.type", default = 4),
                        size = getOption("spinner.size", default = 1.5),
                        color = getOption("spinner.color",
                                          default = "#f4f4f4"),
                        proxy.height = "300px"), p()
                    
                  ), # end of panels (1 out of 3)
                  
                  tabPanel(
                    value = ns("simPanel_animated"),
                    title = tagList(
                      icon("route", class = "cl-sea"),
                      span("Trajectory details", class = "ttl-panel")
                    ), br(),
                    
                    ggiraph::girafeOutput(
                      outputId = ns("simPlot_route"),
                      width = "95%", height = "100%"),
                    
                    column(width = 12, align = "center",
                           uiOutput(ns("simInput_timeline"))
                    ), br()
                    
                  ) # end of panels (2 out of 2)
                )), # end of tabs
            
            div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-5",
                
                p(class = "fluid-padding"),
                div(id = ns("help_sim_guide"),
                    p("Quick", wrap_none(span("guidelines",
                                              class = "cl-sea"), ":")) %>%
                      tagAppendAttributes(class = 'subheader'),
                    
                    helpText(
                      style = "text-align: justify",
                      "To change the home range crossing time, modify",
                      span("position autocorrelation", class = "cl-sea"),
                      HTML(paste0("(\u03C4", tags$sub("p"), ").")),
                      "To change directional persistence, modify",
                      span("velocity autocorrelation", class = "cl-sea"),
                      HTML(paste0("(\u03C4", tags$sub("v"), ").")),
                      "To change the area covered,",
                      "modify", span("semi-variance", class = "cl-sea"),
                      "(\u03C3).",
                      "Increasing either",
                      HTML(paste0("\u03C4", tags$sub("p"))), "or",
                      HTML(paste0("\u03C4", tags$sub("v"))),
                      "lowers", span("movement speed", class = "cl-sea"),
                      "(\u03BD), while increasing \u03C3 raises it."
                    ), p(style = "padding-bottom: 25px;")
                ),
                
                div(id = ns("sim_details"),
                    p("Do you wish to compare",
                      wrap_none(span("multiple simulations",
                                     class = "cl-sea"), "?")) %>%
                      tagAppendAttributes(class = 'subheader'),
                    
                    helpText(
                      style = "text-align: justify",
                      
                      "You can add all parameters",
                      "to a table for easy comparisons.",
                      "Click the",
                      fontawesome::fa("bookmark", fill = "var(--midgnight"),
                      span("Add to table", class = "cl-mdn"),
                      "button below after each run.")
                )
                
            ), # end of div (body)
            
            footer = tagList(
              column(
                width = 12, align = "right",
                style = "padding-right: 0px;",
                
                div(id = "sims-footer",
                    shiny::actionButton(
                      inputId = ns("repeat_sim"),
                      label = "Repeat",
                      icon = icon("rotate-right"),
                      class = "btn-info",
                      width = "120px"),
                    HTML("&nbsp;"),
                    shiny::actionButton(
                      inputId = ns("simButton_save"),
                      label = span("Add to",
                                   span("table", class = "cl-sea")),
                      icon = icon("bookmark"),
                      width = "120px"))
              ),
              
              div(
                style = "position: absolute; left: 10px;",
                shinyWidgets::checkboxGroupButtons(
                  inputId = ns("simHelp_guide"),
                  label = NULL,
                  choices =
                    c(`<i class='fa fa-circle-question'></i>` = "show"),
                  justified = TRUE,
                  width = "39px",
                  status = "warning"
                ))
              
            ) # end of tagList (footer)
          ), # end of box // simBox_viz
          
          # Table: --------------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Simulation details:", class = "ttl-box"),
            id = ns("simBox_summary"),
            width = NULL,
            solidHeader = FALSE,
            
            div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-4",
                
                p(style = "text-align: justify;",
                  "This information will be added to the",
                  icon("box-archive", class = "cl-mdn"),
                  span("Report", class = "cl-mdn"), "tab,",
                  "so it can be reviewed at any point.",
                  span("Note:", class = "cl-dgr"),
                  "only the last simulation will be used",
                  "for further analyses.")
                
            ), # end of div (left)
            
            div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-8",
                p(style = "padding-top: 5px;"),
                reactable::reactableOutput(ns("simTable")),
                
                br(),
                helpText(style = "padding: 0px 0px;",
                         "The", span("movement speed",
                                     style = "color: #000000;"),
                         "value returned here",
                         "assumes a Gaussian stochastic process for",
                         "a faster computation, but the true value may",
                         "not necessarily be normally distributed.")
                
            ), # end of div (right)
            
            footer = tagList(
              div(style = "display:inline-block; float:right",
                  
                  actionButton(
                    inputId = ns("simTable_clear"),
                    label = "Clear table",
                    icon =  icon("trash"),
                    width = "110px")
                  
              )) # end of tagList (footer)
          ) # end of box // simBox_summary
          
      ), # end of column (center)
      
      # [bottom column] ---------------------------------------------------
      
      div(class = div_column_main,
          
          # Information and R console: ------------------------------------
          
          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("simBox_misc"),
            width = NULL,
            solidHeader = FALSE,
            
            verbatimTextOutput(outputId = ns("console_sims"))
            
          ) # end of box
          
      ), # end of column (bottom)
      
      # FIXED PANELS: -----------------------------------------------------
      ## Help button: -----------------------------------------------------
      
      fixedPanel(
        actionButton(
          inputId = ns("help_sims"),
          label = "Help",
          icon = icon("compass"),
          style = paste("color: #fff;",
                        "background-color: #222d32;",
                        "border-color: #222d32")),

        right = 25, top = 75, width = "45px")
      
    ), # end of fluidRow
    
    # MODALS: -------------------------------------------------------------
    
    create_modal(var = "taup",  id = "sim"),
    create_modal(var = "tauv",  id = "sim"),
    create_modal(var = "sigma", id = "sim"),
    NULL
    
  ) # end of tagList
}

#' tab_sims Server Functions
#'
#' @noRd
mod_tab_sims_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    ## Hide boxes initially:
    
    shinyjs::hide(id = "simBox_viz")
    shinyjs::hide(id = "simBox_summary")
    shinyjs::hide(id = "sim_details")
    
    observe({
      shinyjs::hide(id = "help_sim_guide")
      
      req(input$simHelp_guide)
      if (input$simHelp_guide == "show") {
        shinyjs::show(id = "help_sim_guide")
      } else { shinyjs::hide(id = "help_sim_guide") }
    })
    
    ## Re-run simulations via multiple ways:
    
    to_run <- reactive({
      list(input$run_sim, input$repeat_sim)
    })
    
    to_rerun <- reactive({
      list(input$repeat_sim,
           input$generateSeed)
    })
    
    ## Stored parameters:
    
    saved_pars <- reactive({
      
      list(input$generateSeed,
           input$tau_p0,
           input$tau_p0_units,
           input$tau_v0,
           input$tau_v0_units,
           input$sigma0)
    })
    
    ## Reset values from import or select data tabs:
    
    observe({
      req(vals$active_tab == 'sims')
      reset_reactiveValues(vals) # reset vals between data tabs
      
    }) %>% # end of observe,
      bindEvent(input$generateSeed)
    
    ## Calculate deterministic speed: -------------------------------------
    
    output$sims_speed <- renderText({
      
      if (input$sigma0 == "" ||
          input$tau_p0 == "" ||
          input$tau_v0 == "") {
        
        v <- data.frame(value = "", unit = "")
        
      } else {
        v <- sqrt((input$sigma0 %#% input$sigma0_units) * pi/2) / 
          sqrt(prod((input$tau_v0 %#% input$tau_v0_units), 
                      (input$tau_p0 %#% input$tau_p0_units)))
        
        v <- fix_unit(v, "m/s", convert = TRUE)
      }
      
      return(paste(v$value, v$unit))
      
    }) # end of renderText
    
    ## Convert values/units: ----------------------------------------------
    
    observe({
      req(input$tau_p0_units != vals$tau_p0$unit[2])
      
      new_tau_p0 <- sigdigits(
        input$tau_p0_units %#%
          vals$tau_p0$value[2] %#% vals$tau_p0$unit[2], 3)
      
      updateNumericInput(
        session,
        inputId = "tau_p0",
        label = NULL,
        min = 1, value = new_tau_p0)
      
    }) %>% bindEvent(input$tau_p0_units)
    
    observe({
      req(input$tau_v0_units != vals$tau_v0$unit[2])
      
      new_tau_v0 <- sigdigits(
        input$tau_v0_units %#%
          vals$tau_v0$value[2] %#% vals$tau_v0$unit[2], 3)
      
      updateNumericInput(
        session,
        inputId = "tau_v0",
        label = NULL,
        min = 1, value = new_tau_v0)
      
    }) %>% bindEvent(input$tau_v0_units)
    
    observe({
      req(input$sigma0_units != vals$sigma0$unit[2])
      
      new_sigma0 <- sigdigits(
        input$sigma0_units %#%
          vals$sigma0$value[2] %#% vals$sigma0$unit[2], 3)
      
      updateNumericInput(
        session,
        inputId = "sigma0",
        label = NULL,
        min = 1, value = new_sigma0)
      
    }) %>% bindEvent(input$sigma0_units)
    
    # OPERATIONS ----------------------------------------------------------
    ## Generate random seed: ----------------------------------------------
    
    observe({
      vals$seed0 <- round(stats::runif(1, min = 1, max = 10000), 0)
    }) %>% bindEvent(to_rerun(), ignoreInit = TRUE)
    
    output$seedvalue <- renderPrint({
      req(vals$seed0)
      return(vals$seed0)
    })
    
    ## Prepare model and run simulation: ----------------------------------
    
    observe({
      req(vals$active_tab == 'sims')
      
      vals$tau_p0 <- data.frame(value = c(NA, input$tau_p0, NA),
                                unit = rep(input$tau_p0_units, 3))
      rownames(vals$tau_p0) <- c("low", "est", "high")
      
      vals$tau_v0 <- data.frame(value = c(NA, input$tau_v0, NA),
                                unit = rep(input$tau_v0_units, 3))
      rownames(vals$tau_v0) <- c("low", "est", "high")
      
      vals$sigma0 <- data.frame(value = c(NA, input$sigma0, NA),
                                unit = rep(input$sigma0_units, 3))
      rownames(vals$sigma0) <- c("low", "est", "high")
      
      vals$is_run <- FALSE
      
    }) %>% bindEvent(saved_pars())
    
    sim0 <- reactive({
      
      vals$ctmm_mod <- prepare_mod(
        tau_p = input$tau_p0, tau_p_unit = input$tau_p0_units,
        tau_v = input$tau_v0, tau_v_unit = input$tau_v0_units,
        sigma = input$sigma0, sigma_unit = input$sigma0_units)
      
      tmp_taup <- "days" %#% input$tau_p0 %#% input$tau_p0_units
      tmp_tauv <- input$tau_v0 %#% input$tau_v0_units
      
      vals$dur0 <- dplyr::case_when(
        tmp_taup >= ("days" %#% tmp_tauv) ~ 
          ifelse(tmp_taup > 1,
                 round(tmp_taup * 20, 1), 20),
        TRUE ~ 
          ifelse(("days" %#% tmp_tauv) > 1,
                 round("days" %#% tmp_tauv * 20, 1), 20)
      )
      vals$dur0_units <- "days"

      vals$dti0 <- dplyr::case_when(
        tmp_tauv <= 120 ~ 1,
        tmp_tauv <= 3600 ~ round("minutes" %#% tmp_tauv/4, 0),
        tmp_tauv <= 86400 ~ 1,
        tmp_tauv <= 10 %#% "days" ~ 2,
        TRUE ~ 12)
      vals$dti0_units <- dplyr::case_when(
        tmp_tauv <= 3600 ~ "minutes",
        TRUE ~ "hours")
      
      simulate_data(
        mod = vals$ctmm_mod,
        dur = vals$dur0, dur_units = vals$dur0_units,
        dti = vals$dti0, dti_units = vals$dti0_units,
        seed = vals$seed0)
      
    }) %>% # end of reactive, sim0()
      bindCache(input$tau_p0,
                input$tau_p0_units,
                input$tau_v0,
                input$tau_v0_units,
                input$sigma0,
                input$sigma0_units,
                vals$seed0, cache = "app")
    
    fit0 <- reactive({
      req(vals$data0, vals$ctmm_mod)
      
      inputList <- list(list(vals$data0, vals$ctmm_mod))
      fit <- par.ctmm.fit(inputList, parallel = TRUE)
      
      return(fit)
      
    }) %>%
      bindCache(input$tau_p0,
                input$tau_p0_units,
                input$tau_v0,
                input$tau_v0_units,
                input$sigma0,
                input$sigma0_units,
                vals$seed0, cache = "app")
    
    observe({
      req(vals$active_tab == 'sims')
      
      if (!is.null(vals$seed0)) {
        
        validate(
          need(input$tau_p0 != '', "Select a value."),
          need(input$tau_p0_units != '', "Please choose a unit."),
          need(input$tau_v0 != '', "Select a value."),
          need(input$tau_v0_units != '', "Please choose a unit."),
          need(input$sigma0 != '', "Select a value."),
          need(input$sigma0_units != '', "Please choose a unit."))
        
        ### Simulate full dataset: ----------------------------------------
        
        msg_log(
          style = "warning",
          message = paste0("Generating ",
                           msg_warning("simulated data"), "..."),
          detail = "Please wait for the simulation to finish."
        )
        
        shinybusy::show_modal_spinner(
          spin = "fading-circle",
          color = pal$sea,
          
          text = span(
            style = "font-size: 18px;",
            span("Simulating", style = "color: #797979;"),
            HTML(paste0(span("movement data", class = "cl-sea"),
                        span(".", style = "color: #797979;"))),
            p("This may take a while...",
              style = paste("color: #797979;",
                            "font-size: 16px;",
                            "text-align: center;")),
            p())
          
        ) # end of show_modal_spinner
        
        start_sim <- Sys.time()
        vals$data0 <- sim0()
        
        # Store relevant values:
        vals$data_type <- "simulated"
        vals$species_binom <- vals$species <- "Simulated"
        vals$id <- vals$tmpid <- as.character(vals$seed0)
  
        vals$is_run <- TRUE
        
        # Reset analyses from previous runs (if needed):
        vals$hr <- NULL
        vals$sd <- NULL
        
        time_sim0 <- difftime(Sys.time(), start_sim, units = "secs")

        msg_log(
          style = "success",
          message = paste0("Simulation ",
                           msg_success("completed"), "."),
          with_time = time_sim0)
        
        vals$is_valid <- TRUE
        shinybusy::remove_modal_spinner()
        
        ### Run model fit: ------------------------------------------------
        
        shinybusy::show_modal_spinner(
          spin = "fading-circle",
          color = pal$sea,
          
          text = span(
            style = "font-size: 18px;",
            span("Fitting", style = "color: #797979;"),
            HTML(paste0(span("movement model", class = "cl-sea"),
                        span(".", style = "color: #797979;"))),
            p("This may take a while...",
              style = paste(# "background-color: #eaeaea;",
                "color: #797979;",
                "font-size: 16px;",
                "text-align: center;")),
            p())
          
        ) # end of show_modal_spinner
        
        vals$guess <- NULL
        vals$needs_fit <- FALSE
        
        msg_log(
          style = "warning",
          message = paste0("...", msg_warning("Fitting"),
                           " movement model."),
          detail = "Please wait for model fit to finish.")
        
        vals$fit0 <- fit0()
        
        vals$time_sims <- difftime(Sys.time(), start_sim,
                                   units = "mins")
        
        if (round(vals$time_sims, 1) < 1) {
          tmpdetail <- paste("This step took less than one minute.")
        } else {
          tmpdetail <- paste("This step took approximately",
                             round(vals$time_sims, 1), "minutes.")
        }
        
        msg_log(
          style = "success",
          message = paste0("Model fitting ",
                           msg_success("completed"), "."),
          detail = tmpdetail)
        
        shinyjs::enable("simButton_save")
        shinyjs::show(id = "simBox_misc")
        shinyjs::show(id = "sim_details")
        shinyjs::show(id = "simBox_viz")
        
        shinybusy::remove_modal_spinner()
        
        
      } else {
        
        shinyalert::shinyalert(
          title = "No seed found",
          text = span(
            "Please generate a seed value first, by", br(),
            "clicking the",
            icon("seedling", class = "cl-mdn"),
            span("Generate seed", class = "cl-mdn"),
            "button."),
          html = TRUE,
          size = "xs")
        
      } # end of if () statement
    }) %>% # end of observe,
      bindEvent(to_run())
    
    # PLOTS ---------------------------------------------------------------
    ## Rendering simulated data plot (xy): --------------------------------
    
    output$simPlot_id <- ggiraph::renderGirafe({
      req(vals$data0, vals$is_run, vals$data_type == "simulated")
      
      tmp_taup <- input$tau_p0 %#% input$tau_p0_units
      tmp_tauv <- input$tau_v0 %#% input$tau_v0_units
      
      if (tmp_taup > tmp_tauv) { 
        tau <- vals$tau_p0 
        tau_html <- "\u03C4\u209A"
      } else { 
        tau <- vals$tau_v0 
        tau_html <- "\u03C4\u1D65"
      }
      
      newdat <- vals$data0
      newdat <- newdat[which(newdat$t <= (
        tau$value[2] %#% tau$unit[2])), ]
      
      out_tau <- fix_unit(tau$value[2], tau$unit[2])
      out_dur <- fix_unit(vals$dur0, vals$dur0_units)
      subtitle <- paste(
        "Highlighting one", tau_html, "cycle",
        paste0("(\u2248 ", out_tau[1], " ", out_tau[2], ")"),
        "out of ", out_dur[1], out_dur[2])
      
      # newdat <- newdat[which(newdat$t <= (1 %#% "day")), ]
      # out_dur <- fix_unit(vals$dur0, vals$dur0_units)
      # subtitle <- paste(
      #   "Highlighting 1 day",
      #   "out of ", out_dur[1], out_dur[2])
      
      ymin <- min(vals$data0$y) - diff(range(vals$data0$y)) * .2
      ymax <- max(vals$data0$y) + diff(range(vals$data0$y)) * .2
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_path(
          vals$data0, mapping = ggplot2::aes(
            x = x, y = y),
          col = "grey90", linewidth = 1) +
        ggplot2::geom_point(
          vals$data0, mapping = ggplot2::aes(
            x = x, y = y),
          col = "grey60", size = 1.2) +
        
        ggplot2::geom_path(
          newdat, mapping = ggplot2::aes(
            x = x, y = y, color = timestamp),
          linewidth = 0.5, alpha = .6) +
        ggiraph::geom_point_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = timestamp,
            tooltip = timestamp),
          size = 1.5) +
        
        ggplot2::labs(
          title = "Simulated individual:",
          subtitle = subtitle,
          x = "x coordinate",
          y = "y coordinate") +
        
        ggplot2::scale_x_continuous(
          labels = scales::comma) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(ymin, ymax)) +
        viridis::scale_color_viridis(
          name = "Time:",
          option = "D", trans = "time",
          breaks = c(min(newdat$time),
                     max(newdat$time)),
          labels = c("Start", "End")) +
        
        theme_movedesign() +
        ggplot2::guides(
          color = ggplot2::guide_colorbar(
            title.vjust = 1.02)) +
        ggplot2::theme(
          legend.position = c(0.78, 0.08),
          legend.direction = "horizontal",
          legend.title = ggplot2::element_text(
            size = 11, face = "italic"),
          legend.key.height = ggplot2::unit(0.3, "cm"),
          legend.key.width = ggplot2::unit(0.6, "cm")
        )
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 5.5, height_svg = 5,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .1),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_tooltip(use_fill = TRUE),
          # ggiraph::opts_hover(
          #   css = paste("fill:#1279BF;",
          #               "stroke:#1279BF;",
          #               "cursor:pointer;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))
      
    }) # end of renderGirafe // simPlot_id
    
    ## Preparing data for animation plot: ---------------------------------
    
    data_animated <- reactive({
      req(vals$data0, vals$fit0, vals$data_type == "simulated")
      
      dat <- ctmm::simulate(vals$data0, CTMM = vals$fit0,
                            dt = 15 %#% "minutes")
      
      t_origin <- "1111-10-31 23:06:32"
      dat$timestamp <- as.POSIXct(dat$t, origin = t_origin)
      data_animated <- dat[which(dat$t <= input$timeline), ]
      return(data_animated)
      
    })
    
    ## Rendering route (xyt), for 1-day of data: --------------------------
    
    output$simInput_timeline <- renderUI({
      req(vals$data0, vals$is_run, vals$data_type == "simulated")
      
      tags$div(
        class = "timelineinput",
        sliderInput(
          inputId = ns("timeline"),
          label = p("Rendering one full day,",
                    paste0(vals$dti0, "-", 
                           abbrv_unit(vals$dti0_units), 
                           " steps:")),
          
          value = 1 %#% "day",
          step = 15 %#% "minutes",
          min = 15 %#% "minutes",
          max = 1 %#% "day",
          # animate = animationOptions(interval = 500),
          ticks = FALSE,
          width = "85%"))
      
    }) # end of renderUI // simInput_timeline
    
    output$simPlot_route <- ggiraph::renderGirafe({
      req(input$timeline)
      
      # Time elapsed:
      
      dat <- data_animated()
      maxt <- 1 %#% "day"
      
      datfull <- vals$data0[which(vals$data0$t <= maxt), ]
      nday <- format(max(dat$timestamp), "%d")
      
      subtitle <- paste("Day", nday,
                        format(max(dat$timestamp), "%H:%M:%S"))
      
      thrs_elapsed <- paste("hours" %#% max(dat$t), "hours")
      tmin_elapsed <- paste("minutes" %#% max(dat$t), "minutes")
      
      # Distance traveled:
      dat$dist <- measure_distance(dat)
      dist <- paste(
        scales::label_comma(
          accuracy = 1)(sum(dat$dist, na.rm = TRUE)),
        "meters")
      
      ymin <- min(datfull$y) - diff(range(datfull$y)) * .2
      ymax <- max(datfull$y) + diff(range(datfull$y)) * .2
      p <- ggplot2::ggplot() +
        
        ggplot2::geom_path(
          data = datfull, mapping = ggplot2::aes(x = x, y = y),
          col = "grey90") +
        ggplot2::geom_point(
          data = datfull, mapping = ggplot2::aes(x = x, y = y),
          col = "grey90", size = 2) +
        
        ggplot2::geom_path(
          data = dat,
          mapping = ggplot2::aes(x = x, y = y, color = timestamp),
          size = 1.2) +
        ggplot2::geom_point(
          data = dat,
          mapping = ggplot2::aes(x = x, y = y, color = timestamp),
          size = 2.5) +
        
        # Time elapsed:
        
        ggplot2::annotate(
          "text", family = "Roboto Condensed",
          col = pal$mdn,
          x = min(datfull$x) + diff(range(datfull$x)) * .2,
          y = ymax - diff(range(datfull$y)) * .1,
          fontface = 2, size = 5, lineheight = 1.5,
          label = paste("Time elapsed:\n")) +
        ggplot2::annotate(
          "text", family = "Roboto Condensed",
          col = pal$mdn,
          x = min(datfull$x) + diff(range(datfull$x)) * .2,
          y = ymax - diff(range(datfull$y)) * .1,
          fontface = 1, size = 4, lineheight = 1.5,
          label = tmin_elapsed) +
        
        # Distance traveled:
        
        ggplot2::annotate(
          "text", family = "Roboto Condensed",
          col = pal$mdn,
          x = max(datfull$x) - diff(range(datfull$x)) * .2,
          y = ymax - diff(range(datfull$y)) * .1,
          fontface = 2, size = 5, lineheight = 1.5,
          label = paste("Distance traveled:\n")) +
        ggplot2::annotate(
          "text", family = "Roboto Condensed",
          col = pal$mdn,
          x = max(datfull$x) - diff(range(datfull$x)) * .2,
          y = ymax - diff(range(datfull$y)) * .1,
          fontface = 1, size = 4, lineheight = 1.5,
          label = paste(dist)) +
        
        ggplot2::labs(
          title = "Timestamp:",
          subtitle = subtitle,
          x = "x coordinate",
          y = "y coordinate") +
        
        ggplot2::scale_x_continuous(
          labels = scales::comma) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(ymin, ymax)) +
        viridis::scale_color_viridis(
          name = "Tracking time:",
          option = "mako", direction = -1, trans = "time") +
        
        theme_movedesign() +
        ggplot2::theme(legend.position = "none")
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 6,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .1),
          ggiraph::opts_toolbar(saveaspng = FALSE)))
      
    }) # end of renderUI // simPlot_route
    
    # TABLES --------------------------------------------------------------
    ## Listing multiple simulations: --------------------------------------
    
    simRow <- reactive({
      
      out <- data.frame(
        seed = NA,
        taup = NA,
        tauv = NA,
        sigma = NA,
        time_elapsed = NA,
        tdist = NA,
        mdist = NA,
        speed = NA)
      
      out$seed <- vals$seed0
      
      out$taup <- paste(
        scales::label_comma(accuracy = .1)(vals$tau_p0$value[2]),
        abbrv_unit(vals$tau_p0$unit[2]))
      
      out$tauv <- paste(
        scales::label_comma(accuracy = .1)(vals$tau_v0$value[2]),
        abbrv_unit(vals$tau_v0$unit[2]))
      
      sig <- fix_unit(vals$sigma0$value[2],
                      vals$sigma0$unit[2], convert = T)
      out$sigma <- paste(sig$value, abbrv_unit(sig$unit))
      
      out$time_elapsed <- paste(
        round("days" %#% max(vals$data0$t), 0), "days")
      
      distances <- measure_distance(vals$data0)
      tdist <- sum(distances, na.rm = TRUE)
      mdist <- mean(distances)
      
      if (tdist > 1000) {
        out$tdist <- paste(scales::label_comma(
          accuracy = 1)("km" %#% tdist), "km")
      } else {
        out$tdist <- paste(scales::label_comma(
          accuracy = 1)(tdist), "m")
      }
      
      out$mdist <- paste(scales::label_comma(
        accuracy = .1)(mdist), "m")
      
      tmpnames <- rownames(summary(vals$fit0)$CI)
      speed <- summary(vals$fit0)$CI[
        grep("speed", tmpnames), 2]
      
      speedunits <- tmpnames[grep("speed", tmpnames)] %>%
        extract_units() %>% abbrv_unit()
      
      out$speed <- paste(scales::label_comma(
          accuracy = .1)(speed), speedunits)
      
      return(out)
      
    }) %>% bindEvent(to_run())
    
    observe({
      shinyjs::show(id = "simBox_summary")
      shinyjs::disable("simButton_save")
      
      vals$dt_sims <<- rbind(vals$dt_sims, simRow())
      vals$dt_sims <- dplyr::distinct(vals$dt_sims)
      vals$report_sims_yn <- TRUE
      
    }) %>% # end of observe
      bindEvent(input$simButton_save)
    
    output$simTable <- reactable::renderReactable({
      req(vals$dt_sims)
      
      print(vals$dt_sims)
      dt_sims <- vals$dt_sims[, -1]
      
      columnNames <- list(
        taup = "\u03C4\u209A",
        tauv = "\u03C4\u1D65",
        sigma = "\u03C3",
        time_elapsed = "Time elapsed:",
        tdist = "Dist (total)",
        mdist = "Dist (mean)",
        speed = "Speed (mean)")
      
      reactable::reactable(
        dt_sims,
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
            align = "right",
            minWidth = 55),
        
        columns = list(
          taup = reactable::colDef(
            name = columnNames[["taup"]],
            style = list(fontWeight = "bold")),
          tauv = reactable::colDef(
            name = columnNames[["tauv"]],
            style = list(fontWeight = "bold")),
          sigma = reactable::colDef(
            minWidth = 60, name = columnNames[["sigma"]],
            style = list(fontWeight = "bold")),
          time_elapsed = reactable::colDef(
            minWidth = 100, name = columnNames[["time_elapsed"]]),
          tdist = reactable::colDef(
            minWidth = 85, name = columnNames[["tdist"]]),
          mdist = reactable::colDef(
            minWidth = 85, name = columnNames[["mdist"]]),
          speed = reactable::colDef(
            minWidth = 100, name = columnNames[["speed"]])
        ))
      
    }) # end of renderDataTable // simTable
    
    observe({
      vals$dt_sims <- NULL
    }) %>% # end of observe,
      bindEvent(input$simTable_clear)
    
    # HELP TOUR & MODALS: -------------------------------------------------
    
    build_simsTour <- function(ns, vals) {
      
      element <- intro <- character(0)
      tabinfo <- paste0("#tab_sims_1", "-")
      
      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          "This tab allows you to simulate a new dataset from scratch,",
          "if you do not have access to any real dataset for parameter",
          "extraction."
        )))
      
      element <- c(element, paste0(tabinfo, "simBox_timescales"))
      intro <- c(
        intro,
        HTML(paste(
          "First, you need to set the timescale parameters,",
          "which are:",
          "(1)", span("Position autocorrelation", class = "cl-sea"),
          wrap_none("(\u03C4", tags$sub("p"), "),"),
          "or home range crossing time, and", 
          "(2)", span("velocity autocorrelation", class = "cl-sea"),
          wrap_none("(\u03C4", tags$sub("v"), "),"),
          "or directional persistence.",
          p(),
          "For a more in-depth explanation on what these parameters",
          "mean, click the", fontawesome::fa("circle-question"),
          "help tips."
        )))
      
      element <- c(element, paste0(tabinfo, "simBox_spatialscales"))
      intro <- c(
        intro,
        HTML(paste(
          "Then, you set the",
          span("semi-variance", class = "cl-sea"), "(\u03C3)",
          "parameter, or the average spatial variability",
          "between any two locations.",
          p(),
          "These three variables",
          wrap_none("(\u03C4", tags$sub("p"), ", ",
                    "\u03C4", tags$sub("v"), ", and ",
                    "\u03C3)"),
          "determine the next relevant parameter:",
          span("velocity", class = "cl-sea"), "(\u03BD),",
          "or the directional speed of the simulated animal.",
          p(),
          "For an in-depth explanation of each parameter,",
          "click the", fontawesome::fa("circle-question"),
          "help tips."
        )))
      
      element <- c(element, "#sim-parameters")
      intro <- c(
        intro,
        HTML(paste(
          "Some guidelines:", br(),
          "To quickly modify the distance",
          "traveled within the same time period, you can change the",
          span(HTML(paste0("(\u03C4", tags$sub("p"))),
               class = "cl-sea"),
          "parameter.",
          "To quickly change directional persistence",
          "(and create a simulation that generally travels",
          "more linearly), you can increase the",
          span(HTML(paste0("(\u03C4", tags$sub("v"))),
               class = "cl-sea"),
          "parameter.",
          "To increase the overall area covered during travel,",
          "increase", span("semi-variance", class = "cl-sea"),
          "(\u03C3)."
        )))
      
      element <- c(element, paste0(tabinfo, "simBox_submit"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action",
            "Now you click the", fontawesome::fa("seedling"),
            "'Generate seed' button and then the",
            fontawesome::fa("bolt"), "'Run simulation' button.")
        ))
      )
      
      element <- c(element, paste0(tabinfo, "simBox_viz"))
      intro <- c(
        intro,
        HTML(paste(
          "This tab allows you to visualize the dataset you have",
          "just simulated. The",
          fontawesome::fa("paw", fill = pal$sea),
          span("Data", class = "cl-sea"), "tab",
          "plots, in color, a single",
          span("position autocorrelation", class = "cl-sea"),
          "cycle, out of 10", fontawesome::fa("xmark"),
          HTML(paste0("\u03C4", tags$sub("p"), "."))
        ))
      )
      
      element <- c(element, paste0(tabinfo, "simPlot_route"))
      intro <- c(
        intro,
        HTML(paste(
          "The", fontawesome::fa("route", fill = pal$sea),
          span("Trajectory details", class = "cl-sea"), "tab",
          "runs through a single day of the simulated animal's",
          "trajectory, showing time elapsed and distance covered."
        ))
      )
      
      element <- c(element, "#sims-footer")
      intro <- c(
        intro,
        HTML(paste(
          "If needed, you can use the",
          fontawesome::fa("rotate-right", fill = pal$sea),
          span("Repeat", fill = pal$sea), "button to quickly",
          "simulate a new individual.",
          p(),
          span(
            class = "tour_action",
            "Once you are satisfied with the current parameters",
            "and simulation,",
            "you can save all information in a table below by",
            "clicking the",
            fontawesome::fa("bookmark", fill = pal$sea),
            span("Add to table", fill = pal$sea), "button.")
        )))
      
      element <- c(element, paste0(tabinfo, "simBox_summary"))
      intro <- c(
        intro,
        HTML(paste(
          "You will be able to see other parameters such as",
          span("Tot. distance", class = "cl-grey"),
          "(total distance traveled within 10",
          fontawesome::fa("xmark"),
          span(HTML(paste0("(\u03C4", tags$sub("p"))),
               class = "cl-sea"),
          "cycles),", "the", span("Avg. distance", 
                                  class = "cl-grey"),
          "(average distance traveled)", "and the",
          span("Avg. Speed", class = "cl-grey"),
          "(average movement speed)."
        ))
      )
      
      data.frame(element = element,
                 intro = intro,
                 stringsAsFactors = FALSE)
      
    } # end of sims tour
    
    observe({
      tour_sims <- build_simsTour(ns, vals)
      
      rintrojs::introjs(
        session = session,
        options = list(
          steps = tour_sims,
          nextLabel = "Next",
          prevLabel = "Previous",
          showStepNumbers = F,
          showButtons = T,
          showBullets = T
        ),
        events = list(onbeforechange =
                        rintrojs::readCallback("switchTabs")))
      
    }) %>% bindEvent(input$help_sims)
    
    # SETTINGS ------------------------------------------------------------
    ## Restore state: -----------------------------------------------------
    
    observe({
      
      # Initial parameters:
      
      updateNumericInput(
        session = session,
        inputId = "tau_p0",
        value = vals$restored_vals$"tau_p0")
      
      updateSelectInput(
        session = session,
        inputId = "tau_p0_units",
        selected = vals$restored_vals$"tau_p0_units")
      
      updateNumericInput(
        session,
        inputId = "tau_v0",
        value = vals$restored_vals$"tau_v0")
      
      updateSelectInput(
        session = session,
        inputId = "tau_v0_units",
        selected = vals$restored_vals$"tau_v0_units")
      
      updateNumericInput(
        session,
        inputId = ns("sigma0"),
        value = vals$restored_vals$"sigma0")
      
      updateSelectInput(
        session = session,
        inputId = "sigma0_units",
        selected = vals$restored_vals$"sigma0_units")
      
      vals$tau_p0 <- vals$restored_vals$"tau_p0"
      vals$tau_v0 <- vals$restored_vals$"tau_v0"
      vals$sigma0 <- vals$restored_vals$"sigma0"
      
      vals$seed0 <- vals$restored_vals$"seed0"
      
      vals$dur0 <- vals$restored_vals$"dur0"
      vals$dur0_units <- vals$restored_vals$"dur0_units"
      vals$dti0 <- vals$restored_vals$"dti0"
      vals$dti0_units <- vals$restored_vals$"dti0_units"
      
      # Data and model fit:
      
      vals$data_type <- vals$restored_vals$"data_type"
      vals$data0 <- vals$restored_vals$"data0"
      vals$fit0 <- vals$restored_vals$"fit0"
      
      # Validation parameters:
      
      vals$is_run <- vals$restored_vals$"is_run"
      
    }) %>% bindEvent(vals$restored_vals)
    
    # observe({
    #   vals$seed0 <- vals$restored_vals$"seed0"
    # }) %>% bindEvent(vals$restored_vals, once = TRUE)
    
    ## Additional information: --------------------------------------------
    
    # Export values for tests:
    
    shiny::exportTestValues(
      data0 = vals$data0
    )
    
    # Save information for report if table is not requested:
    
    observe({
      req(vals$active_tab == 'sims', vals$data_type == "simulated")
      vals$report_sims_yn <- FALSE
      vals$report_sims <- simRow()
    })
    
    # Display time elapsed:
    
    output$console_sims <- renderText({
      req(vals$time_sims)
      paste0("The simulation took approximately ",
             round(vals$time_sims, 1), " minutes.")
    })
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_sims_ui("tab_sims_1")

## To be copied in the server
# mod_tab_sims_server("tab_sims_1")
