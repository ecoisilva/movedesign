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

          uiOutput(ns("sdUI_show")),
          br()
          
      ), # end of div (top row)

      # [left column] -----------------------------------------------------

      div(class = div_column_left,

          ## Tracking regime: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Sampling design", class = "ttl-box_solid"),
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
                  column(width = 12, mod_blocks_ui(ns("sdBlock_dur"))),
                  column(width = 12, mod_blocks_ui(ns("sdBlock_dti"))),
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("sdPanel_regime_new"),
                title = icon("bolt", class = "cl-mdn"),
                p(),
                fluidRow(
                  column(width = 12, mod_blocks_ui(ns("sdBlock_dur_new"))),
                  column(width = 12, mod_blocks_ui(ns("sdBlock_dti_new"))),
                ) # end of fluidRow

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12, align = "center",

              shiny::actionButton(
                inputId = ns("sdButton_compare"),
                label = "Compare",
                icon = icon("wrench"),
                class = "btn-info",
                width = "100%")
              
              # splitLayout(
              #   cellWidths = c("29%", "1%", "70%"),
              #   cellArgs = list(style = "align: center;"),
              # 
              #   shiny::actionButton(
              #     inputId = ns("sdHelp_regime"),
              #     label = NULL,
              #     width = "100%",
              #     icon = icon("circle-question"),
              #     class = "btn-warning"),
              #   br(),
              #   shiny::actionButton(
              #     inputId = ns("sdButton_compare"),
              #     label = "Modify",
              #     icon = icon("wrench"),
              #     class = "btn-info",
              #     width = "100%")
              # 
              # ) # end of splitLayout

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
                  column(width = 12, mod_blocks_ui(ns("sdBlock_N")))
                ) # end of fluidRow
                
              ), # end of panels (1 out of 2)
              
              tabPanel(
                value = ns("sdPanel_sizes_new"),
                title = icon("bolt", class = "cl-mdn"),
                
                fluidRow(
                  column(width = 12, uiOutput(ns("sdBlock_n_new"))),
                  column(width = 12, mod_blocks_ui(ns("sdBlock_N_new")))
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
                  p(), 
                  shinyWidgets::checkboxGroupButtons(
                    inputId = ns("show_paths"),
                    label = "Show trajectories:",
                    choices =
                      c("Fine-scale/truth" = "full",
                        "Current design" = "initial"),
                    selected = c("full", "initial"),
                    checkIcon = list(yes = icon("circle-check")),
                    justified = TRUE),
                  p(),
                  
                  ggiraph::girafeOutput(
                    outputId = ns("sdPlot_path"),
                    width = "100%", height = "100%")) %>%
                  shinycssloaders::withSpinner(
                    type = getOption("spinner.type", default = 7),
                    color = getOption("spinner.color",
                                      default = "#f4f4f4")),
                
                div(
                  class = "col-xs-12 col-sm-12 col-md-12 col-lg-4",
                  p(class = "fluid-padding"),
                  uiOutput(ns("sdText_new1")),
                  
                  fluidRow(
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("distBlock_est"))),
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("distBlock_err")))),
                  
                  fluidRow(
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("distBlock_est_new"))),
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("distBlock_err_new"))))
                  
                ) # end of div
                
              ), # end of panel (1 out of 2)
              
              tabPanel(
                title = "Speed",
                value = ns("regPanel_speed"),
                icon = icon("gauge-high"),
                
                div(
                  class = "col-xs-12 col-sm-12 col-md-12 col-lg-8",
                  p(), 
                  shinyWidgets::checkboxGroupButtons(
                    inputId = ns("show_speeds"),
                    label = "Show speed estimates:",
                    choices =
                      c("Fine-scale/truth" = "full",
                        "Current design" = "initial"),
                    selected = c("full", "initial"),
                    checkIcon = list(yes = icon("circle-check")),
                    justified = TRUE),
                  p(),
                  
                  ggiraph::girafeOutput(
                    outputId = ns("sdPlot_speed"),
                    width = "100%", height = "100%"),
                  uiOutput(ns("sdUI_unit"))),
                
                div(
                  class = "col-xs-12 col-sm-12 col-md-12 col-lg-4",
                  p(class = "fluid-padding"),
                  uiOutput(ns("sdText_new2")),

                  fluidRow(
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("sdBlock_est"))),
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("sdBlock_err")))),
                  
                  fluidRow(
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("sdBlock_est_new"))),
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("sdBlock_err_new"))))
                
                ) # end of div

              ) # end of panel (2 out of 2)
            ), # end of tabBox // sdBox_viz
            
            footer =  column(
              width = 12,
              style = "max-width: 250px; float: right;",
              
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
                  inputId = ns("add_sd_table"),
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

            verbatimTextOutput(outputId = ns("out_time_sd")),
            verbatimTextOutput(outputId = ns("out_time_sd_new")),
            verbatimTextOutput(outputId = ns("out_time_sd_total"))
            
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
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    vals$sd <- reactiveValues(time = c(0, 0))
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at start: --------------------------------------------
    
    boxnames <- c("regime",
                  "sizes",
                  "outputs",
                  "summary",
                  "misc")

    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("sdBox_", boxnames[i]))
    }

    tabnames <- c("regime", "speed", "size", "dist", "viz")
    for (i in 1:length(tabnames)) {
      tmp_id <- paste0("sdTabs_", tabnames[i])
      tmp_target <- paste0("sdPanel_", tabnames[i], "_new")
      hideTab(inputId = tmp_id, target = ns(tmp_target))
    }
    
    ## Select all tabs after new sampling design: -------------------------
    
    observe({
      tmp <- ifelse(input$sdInput_show == 1, "", "_new")
      tabs <- paste0("sdTabs_", tabnames)
      panels <- paste0("sdPanel_", tabnames)
      
      for (i in 1:length(tabnames)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[i]),
          selected = paste0("tab_ctsd_1-", panels[i], tmp)) 
      }
      
      vals$paths_selected <- c("full", "initial", "new")
      
      tmp <- c(paste("sdInfo", c("est", "err"), sep = "_"),
               paste("distInfo", c("est", "err"), sep = "_"))
      tmp_new <- paste0(tmp, "_new")
      
      for (i in 1:length(tmp)) {
        
        if (input$sdInput_show == 1) {
          shinyjs::show(id = tmp[i])
          shinyjs::hide(id = tmp_new[i])
        } else if (input$sdInput_show == 2) {
          shinyjs::show(id = tmp_new[i])
          shinyjs::hide(id = tmp[i])
        }
      }
      
    }) %>% # end of observe.
      bindEvent(input$sdInput_show) # ignoreInit = TRUE
    
    ## Sample size boxes for comparing sampling designs: ------------------
    
    output$sdUI_compare_n <- renderUI({
      req(vals$data1, input$sd_dur, input$sd_dti)
      
      device <- movedesign::fixrates
      dti <- device$dti[match(input$sd_dti, device$dti_notes)]
      
      n_new <- length(
        seq(0, round(input$sd_dur %#% "days", 0),
            by = round(dti, 0))[-1])
      
      splitLayout(
        parBlock(header = "Initial sampling design:",
                 value = span(scales::label_comma(
                   accuracy = 1)(nrow(vals$data1)),
                   "locations", class = "cl-mdn")),
        
        parBlock(header = "Modified sampling design:",
                 value = span(scales::label_comma(
                   accuracy = 1)(n_new),
                   "locations", class = "cl-dgr"))
        
      ) # end of splitLayout
      
    }) # end of renderUI // sdUI_compare_n
    
    ## Adding different datasets to trajectory plot: ----------------------
    
    observe({
      req(vals$sd$data)
      
      opts <- c("full", "initial", "new")
      
      if (!is.null(input$sdInput_show)) {
        if (input$sdInput_show == 1) {
          opts <- c("full", "initial")
        }
      }
      
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_paths",
        choices =
          c("Fine-scale" = "full",
            "Initial design" = "initial",
            "New design" = "new"),
        selected = opts,
        checkIcon = list(yes = icon("circle-check")),
        justified = TRUE)
      
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_speeds",
        choices =
          c("Fine-scale" = "full",
            "Initial design" = "initial",
            "New design" = "new"),
        selected = opts,
        checkIcon = list(yes = icon("circle-check")),
        justified = TRUE)
      
    }) # end of observe
    
    ## Changing grouping options for plot: --------------------------------
    
    output$sdUI_unit <- renderUI({
      req(vals$dur, vals$dti)
      
      eval_dur <- vals$dur$value %#% vals$dur$unit
      eval_dti <- vals$dti$value %#% vals$dti$unit
      
      opts_unit <- c("Minutes" = "minutes",
                     "Hours" = "hours",
                     "Days" = "days",
                     "Weeks" = "weeks",
                     "Months" = "months")
      
      if (eval_dur <= (5 %#% "days")) {
        unit_selected <- "hours"
        unit_min <- "minutes"
        unit_max <- "days"
        
      } else if (eval_dur > (5 %#% "days") &&
                 eval_dur <= (1 %#% "month")) {
        unit_selected <- "days"
        unit_min <- "minutes"
        unit_max <- "days"
        
      } else {
        unit_selected <- "days"
        unit_min <- "minutes"
        unit_max <- "months"
      }
      
      if (eval_dti >= 1 %#% "day") {
        unit_min <- "days"
      } else if (eval_dti >= 1 %#% "hour") {
        unit_min <- "hours"
      }
      
      shinyWidgets::sliderTextInput(
        inputId = ns("sd_unit"),
        label = span("Averaging time unit above:",
                     class = "txt-label",
                     style = "text-align: center;"),
        choices = opts_unit,
        from_min = unit_min,
        from_max = unit_max,
        selected = unit_selected,
        grid = TRUE,
        force_edges = TRUE,
        width = "100%")
      
    }) # end of renderUI, "sdUI_unit"
    
    ## Show new conditional panels if comparing: --------------------------
    
    observe({
      req(vals$sd$data)
      req(vals$sd$fit)
      
      # Show new conditional simulation panels:
      for (i in 1:length(tabnames)) {
        tmp_id <- paste0("sdTabs_", tabnames[i])
        tmp_target <- paste0("sdPanel_", tabnames[i], "_new")
        showTab(inputId = tmp_id, target = ns(tmp_target))
      }
      
      # Select new conditional simulation panels:
      tabs <- paste0("sdTabs_", tabnames)
      panels <- paste0("sdPanel_", tabnames)
      for (i in 1:length(tabnames))
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[i]),
          selected = paste0("tab_ctsd_1-", panels[i], "_new"))
      
    }) %>% # end of observe,
      bindEvent(list(vals$sd$data, vals$sd$fit))
    
    ## Show buttons to change all panels: ---------------------------------
    
    output$sdUI_show <- renderUI({
      req(vals$sd$data)
      
      shinyWidgets::radioGroupButtons(
        inputId = ns("sdInput_show"),
        label = NULL,
        choices = c("Show initial sampling design" = 1,
                    "Show modified sampling design" = 2),
        checkIcon = list(yes = icon("circle-check")),
        selected = 2,
        justified = TRUE)
      
    }) # end of renderUI, "sdUI_show"
    
    ## Writing new regime text: -------------------------------------------
    
    writing_regime_new <- reactive({
      req(vals$sd$dur, vals$sd$dti)
      
      out_dti <- fix_unit(vals$sd$dti$value, vals$sd$dti$unit)
      txt_dti <- ifelse(out_dti$value == 1,
                        paste(out_dti$unit),
                        paste(out_dti$value, out_dti$unit))
      
      dur <- vals$sd$dur$value
      dur_mth <- "months" %#% vals$sd$dur$value %#% vals$sd$dur$unit
      
      if (dur == 1) {
        txt_dur <- wrap_none(
          "for ",  span("1 day", class = "cl-dgr"), ".")
      } else if (dur == 365) {
        txt_dur <- wrap_none(
          "for ",  span("1 year", class = "cl-dgr"), ".")
      } else {
        txt_dur <- HTML(paste(
          "for a duration of",
          span(round(dur, 1), "days", class = "cl-dgr"), "(or",
          wrap_none(round(dur_mth, 1), " months",
                    css = "cl-dgr", end = ").")))
      }
      
      p(br(),
        "This new sampling design is equal to",
        "a new location every", span(txt_dti, class = "cl-sea-d"),
        txt_dur)
      
    }) # end of reactive, "writing_regime_new"
    
    output$sdText_new1 <- renderUI(writing_regime_new())
    output$sdText_new2 <- renderUI(writing_regime_new())
    
    # ALERTS --------------------------------------------------------------
    
    ## If no initial data uploaded, selected or simulated:
    
    observe({
      req(vals$active_tab == 'ctsd')
      
      if (is.null(vals$data0))
        shinyalert::shinyalert(
          type = "error",
          title = "No data found",
          text = tagList(span(
            "Please upload, select or simulate an", br(),
            span("movement dataset", class = "cl-dgr"),
            "first in the",
            icon("paw", class = "cl-mdn"),
            span("Species", class = "cl-mdn"), "tabs."
          )),
          html = TRUE,
          size = "xs")
      
    }) # end of observe
    
    ## If no sampling design was set:
    
    observe({
      req(vals$active_tab == 'ctsd')
      
      if (is.null(vals$data1)) {
        
        shinyalert::shinyalert(
          type = "error",
          title = "No sampling design set",
          text = tagList(span(
            "Please go to the",
            icon("stopwatch", class = "cl-mdn"),
            span("Sampling design", class = "cl-mdn"), "tab",
            "and make sure to both (1) set the sampling design, and",
            "(2) run a new simulation by pressing the",
            icon("bolt", class = "cl-dgr"),
            span("'Run'", class = "cl-mdn"), "button."
          )),
          html = TRUE,
          size = "xs")
        
      } else {
        
        # Check if ids match:
        
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
              span("Species", class = "cl-mdn"), "tab",
              "for the appropriate individual before",
              "estimating home range."),
            html = TRUE,
            size = "xs")
        }
        
        ## If no signature of velocity persists in data:
        
        if (is.null(vals$tau_v0)) {
          
          shinyalert::shinyalert(
            type = "error",
            title = "Speed & distance invalid",
            text = tagList(span(
              "No statistically significant signature of the animal's",
              span("velocity", class = "cl-dgr"),
              "remains in this dataset.",
              "Please select a different individual or dataset to",
              "proceed with", span("distance/speed", class = "cl-dgr"),
              "estimation"
            )),
            confirmButtonText = "Dismiss",
            html = TRUE)
          
          msg_log(
            style = "danger",
            message = paste("Initial dataset has no remaining",
                            msg_danger("velocity"), "signature."),
            detail = paste("Select a different individual",
                           "or dataset to proceed."))
          
          shinyjs::disable("run_sd")
        }
        
      } # end of if ()
      
    }) # end of observe
    
    # If comparing sampling designs before running it once:
    
    observe({
      
      if (is.null(vals$ctsd))
        shinyalert::shinyalert(
          title = "Error",
          text = tagList(span(
            
            "First, estimate speed & distance",
            "traveled based on the original dataset",
            "by clicking the",
            icon("paper-plane", class = "cl-mdn"),
            wrap_none(span("Run estimation", class = "cl-mdn")),
            "button."
          )),
          html = TRUE,
          size = "xs")
      
    }) %>% # end of observe,
      bindEvent(input$sdButton_compare)
    
    # If sampling design is too coarse:
    
    observe({
      req(vals$active_tab == 'ctsd',
          vals$fit1, vals$which_question)
      
      nms <- rownames(summary(vals$fit1)$CI)
      N_speed <- summary(vals$fit1)$DOF[["speed"]]
      
      is_valid <- NULL
      if (length(grep("velocity", nms)) == 0 | N_speed == 0) {

        if (length(vals$which_question) == 1) {
          alert_type <- "error"
          txt <- span(
            "The data is too coarsely sampled to",
            "estimate speed and distance. Go back to the",
            icon("stopwatch", class = "cl-mdn"),
            span("Sampling design", class = "cl-mdn"), "tab,",
            "and set a shorter",
            span("sampling interval", class = "cl-sea-d"),
            "(reducing the time between locations).")
          
          msg_log(
            style = "danger",
            message = paste(
              "Data too coarsely sampled for",
              msg_danger("speed & distance"), "estimation"),
            detail = paste("Select a different individual",
                           "or dataset to proceed."))

          shinyjs::disable("run_sd")
          
        } else {
          alert_type <- "info"
          txt <- span(
            "The data was too coarsely sampled to estimate",
            "speed and distance. Please proceed to the",
            icon("box-archive", class = "cl-mdn"),
            span("Report", class = "cl-mdn"), "tab",
            "for a review of both home range",
            "and speed/distance estimation.", br(),
            "You can also go back to the",
            icon("stopwatch", class = "cl-mdn"),
            span("Sampling design", class = "cl-mdn"), "tab,",
            "and set a shorter",
            span("sampling interval", class = "cl-sea-d"),
            "(reducing the time between locations),",
            "before re-running both estimators.")
          vals$sd_completed <- TRUE
        }

        shinyalert::shinyalert(
          type = alert_type,
          title = "Warning",
          text = tagList(txt),
          html = TRUE,
          size = "s")

        is_valid <- FALSE

      } else {
        shinyjs::enable("run_sd")
        is_valid <- TRUE
      }
      
      vals$sd$is_valid <- is_valid
      
    }) %>% # end of observe,
      bindEvent(vals$active_tab)
    
    # OPERATIONS ----------------------------------------------------------
    ## Fitting movement model (if needed): --------------------------------
    
    timing_fit <- reactive({
      out_time <- guess_time(data = vals$data1, parallel = vals$parallel)
      return(out_time)
      
    }) %>% # end of reactive, calculating_time()
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
    
    ## Run model fit:
    
    observe({
      req(vals$active_tab == 'ctsd',
          vals$data1,
          vals$dev$is_valid,
          vals$needs_fit)
      
      vals$fit1 <- NULL
      vals$is_analyses <- NULL
      
      loading_modal("Calculating run time")
      expt <- timing_fit()
      confirm_time <- NULL
      
      if ((expt$max %#% expt$unit) > (15 %#% "minutes")) {
        
        out <- fix_unit(expt$max, expt$unit, convert = TRUE)
        
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            confirm_time <- x
          },
          text = tagList(span(
            "Expected run time for the next phase", br(),
            "is approximately",
            wrap_none(span(out$value, out$unit,
                           class = "cl-dgr"), ".")
          )),
          type = "warning",
          showCancelButton = TRUE,
          cancelButtonText = "Stop",
          confirmButtonCol = pal$mdn,
          confirmButtonText = "Proceed",
          html = TRUE)
        
      } else { confirm_time <- TRUE }
      
      shinybusy::remove_modal_spinner()
      
      req(confirm_time)
      
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("in progress"), "."),
        detail = "Please wait for model selection to finish:")
      
      loading_modal("Selecting movement model",
                    runtime = expt$range, for_time = TRUE)
      
      start <- Sys.time()
      fit1 <- fitting_model()
      time_fit1 <- difftime(Sys.time(), start, units = "sec")
      vals$sd$time[1] <- vals$sd$time[1] + time_fit1[[1]]
      
      msg_log(
        style = 'success',
        message = paste0("Model fit ",
                         msg_success("completed"), "."),
        with_time = time_fit1)
      
      vals$needs_fit <- FALSE
      vals$fit1 <- fit1
      
      nms <- names(summary(vals$fit1)$DOF)
      N1 <- summary(vals$fit1)$DOF[grep('area', nms)][[1]]
      N2 <- summary(vals$fit1)$DOF[grep('speed', nms)][[1]]
      vals$dev$N1 <- N1
      vals$dev$N2 <- N2
      
      shinybusy::remove_modal_spinner()
      
      shinyFeedback::showToast(
        type = "success",
        message = "Model fit completed!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"
        )
      )
      
    }) # end of observe
    
    observe({
      shinyjs::show(id = "sdBox_regime")
      shinyjs::show(id = "sdBox_sizes")
    }) %>% bindEvent(vals$fit1)
    
    ## Comparing sampling designs: ----------------------------------------
    # Adjust sampling parameters necessary for simulation:
    
    observe({
      req(vals$tau_p0, vals$tau_v0,
          vals$dur, vals$dti,
          vals$ctsd)
      
      taup <- "days" %#% vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      
      # Sampling duration:
      
      dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      dur <- round(dur, 0)
      taup <- round(taup, 0)
      
      ideal_dur <- "days" %#% (tauv/0.05^2) # error ~ 0.05
      opts_dur <- c(dur, ideal_dur, 
                    5, 10, 15, 25, 50, 
                    100, 250, 500, 1000) %>%
        round_any(5, f = round)
      opts_dur <- c(1, 2, 3, 4, opts_dur) %>% sort() %>% unique()
      opts_dur <- opts_dur[opts_dur != 0]
      
      max_dur <- max(
        opts_dur[which.min(abs(opts_dur - ideal_dur))], dur)
      
      # Sampling interval:
      
      device <- movedesign::fixrates %>% 
        dplyr::arrange(dplyr::desc(.data$frq))
      opts_dti <- device$dti_notes
      
      dti_min <- tauv/10
      minindex <- which.min(abs(device$dti - dti_min))
      dti_max <- 1 %#% "day" # OR tau_v0 * 3 OR dti
      maxindex <- which.min(abs(device$dti - dti_max))
      dti_new <- tauv * 1/3
      newindex <- which.min(abs(device$dti - dti_new))
      
      tmprange <- NULL
      
      if (vals$data_type != "simulated") {
        tmprange <- paste(ifelse(
          vals$tau_p0$value[1] == 0, "0",
          scales::label_comma(accuracy = .1)(vals$tau_p0$value[1])),
          "\u2014",
          scales::label_comma(accuracy = .1)(vals$tau_p0$value[3]))
      }
      
      shiny::showModal(
        shiny::modalDialog(
          title = h4("Adjusting sampling design:"),
          
          fluidRow(
            style = paste("margin-right: 25px;",
                          "margin-left: 25px;"),
            
            p("If speed & distance estimation is your goal,",
              "we recommend that",
              span("sampling interval", class = "cl-sea"),
              "be equal (if not at least 3 times shorter) than the",
              span(HTML(paste0("velocity autocorrelation ",
                               "(\u03C4", tags$sub("v"), ")")),
                   class = "cl-sea"), "timescale, to reduce bias",
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
                  accuracy = .1)(vals$tau_v0$value[2]),
                  vals$tau_v0$unit[2]),
              subtitle = tmprange),
            
            shinyWidgets::sliderTextInput(
              inputId = ns("sd_dur"),
              label = "Sampling duration (in days):",
              width = "100%",
              choices = opts_dur,
              selected = round_any(ideal_dur, 5, f = round),
              from_max = max_dur,
              from_min = 1
            ),
            
            shinyWidgets::sliderTextInput(
              inputId = ns("sd_dti"),
              label = "Sampling interval:",
              width = "100%",
              
              choices = opts_dti,
              selected = opts_dti[newindex],
              from_min = opts_dti[minindex],
              from_max = opts_dti[maxindex]),
            
            uiOutput(ns("sdUI_compare_n")),
            
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
          
          size = "l")) # end of modal
      
    }) %>% # end of observe,
      bindEvent(input$sdButton_compare)
    
    # ESTIMATIONS ---------------------------------------------------------
    
    # Estimate fine-scale trajectory:
    estimating_truth <- reactive({
      
      fit <- vals$fit0
      dat <- if (vals$data_type == "simulated") { vals$data0
      } else { vals$data1 }
      
      dur <- vals$dur$value %#% vals$dur$unit
      taup <- vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      
      dti <- ifelse(tauv <= 1 %#% "minute", 1 %#% "minute", tauv/10)
      t_new <- seq(0, round(dur, 0), by = dti)[-1]
      dat_full <- ctmm::simulate(
        dat, vals$fit0, t = t_new, seed = vals$seed0)
      vals$data_full <- dat_full
      
      # inputList <- align_lists(
      #   list(dat_full[which(dat_full$t <= tauv * 10), ]),
      #   list(vals$fit0))
      
      # ctsd_truth <- par.speed(inputList, parallel = vals$parallel)
      
      if (fit$mean == "stationary") {
      
        sig <- vals$sigma0$value[2] %#% vals$sigma0$unit[2]
        v <- sqrt(sig * pi/2)
        v <- v/sqrt(prod(taup, tauv)) # error ~ 0.01
        
      } else {
        
        err <- 0.01
        cor.min <- 0.5
        dt.max <- -log(cor.min) * tauv
        
        dt <- tauv * (err/10)^(1/3) # O(error/10) inst error
        t <- seq(0, tauv/err^2, dt) # O(error) est error
        dat <- ctmm::simulate(fit, t = t, precompute = F)
        v <- sqrt(dat$vx^2 + dat$vy^2)
        
        w <- diff(t)
        w <- w * (w <= dt.max)
        w <- c(0,w) + c(w,0)
        w <- w * (t >= range(dat$t)[1] & t <= range(dat$t)[2])
        v <- sum(w * v)/sum(w) # weighted average speed
      }
      
      ctsd_truth <- data.frame(low = NA, est = v, high = NA,
                               row.names = "speed (meters/second)")
      return(ctsd_truth)
      
    }) %>% # end of reactive, estimating_truth()
      bindCache(c(vals$id,
                  vals$dur,
                  vals$dti))
    
    ## Estimating for initial sampling design: ----------------------------
    
    timing_speed <- reactive({
      
      loading_modal("Calculating run time")
      msg_log(
        style = "warning",
        message = paste0("Estimating ", msg_warning("run time"), ":"))
      
      out_time <- guess_time(data = vals$data1, 
                             fit = vals$fit1, 
                             seed = vals$seed0,
                             type = "speed",
                             with_truth = TRUE,
                             trace = TRUE,
                             parallel = vals$parallel)
      
      shinybusy::remove_modal_spinner()
      return(out_time)
      
    }) %>% # end of reactivee, timing_speed()
      bindCache(vals$data0,
                vals$data1,
                vals$dur,
                vals$dti)
    
    estimating_speed <- reactive({
      
      dat <- vals$data1
      # tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      # 
      # dti <- vals$dti$value %#% vals$dti$unit
      # dur <- vals$dur$value %#% vals$dur$unit
      # 
      # if (dur >= 20 * tauv && dti < 3 * tauv)
      #   dat <- dat[which(dat$t <= 20 * tauv), ]
      
      inputList <- align_lists(list(dat), list(vals$fit1))
      if (vals$overwrite_active) set.seed(vals$seed0)
      ctsd <- par.speed(inputList, parallel = vals$parallel)
      if (vals$overwrite_active) set.seed(NULL)
      return(ctsd)
      
    }) %>% # end of reactive, estimating_speed()
      bindCache(vals$dur,
                vals$dti,
                vals$data1)
    
    # First, estimate run time:
    
    observe({
      req(vals$data1,
          vals$fit1,
          vals$tmpid == vals$id,
          vals$sd$is_valid)
      
      tmplist <- list("sdBox_speed",
                      "sdBox_dist",
                      "sdBox_outputs")
      
      for (i in 1:length(tmplist)) shinyjs::show(id = tmplist[i])
      
      expt <- timing_speed()
      vals$sd$expt_time <- expt
      
      tmpnms <- names(summary(vals$fit1)$DOF)
      N <- summary(vals$fit1)$DOF[grep("speed", tmpnms)][[1]]
      
      req(N)
      if ((as.numeric(expt$max) %#% expt$unit) > 900) {
        
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            vals$sd$confirm_time <- x
          },
          text = tagList(span(
            "Expected run time for estimation",
            "could be on average", span(expt$range, class = "cl-dgr"),
            "but may take \u003E", wrap_none(
              span(expt$max, expt$unit, class = "cl-dgr"), ".")
          )),
          type = "warning",
          showCancelButton = TRUE,
          cancelButtonText = "Stop",
          confirmButtonCol = pal$mdn,
          confirmButtonText = "Proceed",
          html = TRUE
        )
      } else { vals$sd$confirm_time <- TRUE }
      
    }) %>% # end of observe,
      bindEvent(input$run_sd)
    
    observe({
      req(vals$sd$confirm_time)
      req(vals$sd_completed == FALSE)
      
      msg_log(
        style = "warning",
        message = paste0("Estimating ",
                         msg_warning("speed & distance"), ":"))
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = span(
          span("Estimating", style = "color: #797979;"),
          HTML(paste0(span("speed & distance", class = "cl-sea"),
                      span("...", style = "color: #797979;"))),
          p(),
          p("Expected run time:",
            style = paste("background-color: #eaeaea;",
                          "color: #797979;",
                          "font-size: 16px;",
                          "text-align: center;")), br(),
          p(vals$sd$expt_time$range,
            style = paste("background-color: #eaeaea;",
                          "color: #009da0;",
                          "font-size: 16px;",
                          "text-align: center;",
                          "margin-top: -40px;")),
          p()
        ) # end of text
      ) # end of modal
      
      ### Current trajectory: ---------------------------------------------
      
      start <- Sys.time()
      
      msg_log(
        style = "warning",
        message = paste0(
          "Simulating for ",
          msg_warning("current trajectory"),
          msg_step(1, 2, style = "warning")),
        detail = "This may take a while...")
      
      ctsd <- estimating_speed()
      ctsd_data <- ctmm::speeds(vals$data1, vals$fit1, units = FALSE)
      
      if ("CI" %in% names(ctsd)) ctsd <- ctsd$CI
      tmpnms <- rownames(ctsd)
      tmpunit <- extract_units(tmpnms[grep("speed", tmpnms)])
      
      out_est <- c("lci" = ctsd[1],
                   "est" = ctsd[2],
                   "uci" = ctsd[3])
      
      vals$ctsd <- ctsd
      vals$ctsd_data <- ctsd_data
      vals$speedEst <- data.frame(value = out_est, "unit" = tmpunit)
      time_sd <- difftime(Sys.time(), start, units = "sec")
      
      msg_log(
        style = "success",
        message = paste0(
          "Estimation ", msg_success("completed"),
          msg_step(1, 2, style = "success")),
        with_time = time_sd)
      
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
      
      ### Fine-scale trajectory: ------------------------------------------
      
      start_truth <- Sys.time()
      
      msg_log(
        style = "warning",
        message = paste0(
          "Simulating for ",
          msg_warning("fine-scale trajectory"),
          msg_step(2, 2, style = "warning")),
        detail = "This may take a while...")
      
      ctsd_truth <- estimating_truth()
      
      if ("CI" %in% names(ctsd_truth)) ctsd_truth <- ctsd_truth$CI
      tmpnms <- rownames(ctsd_truth)
      tmpunit_truth <- extract_units(tmpnms[grep("speed", tmpnms)])
      truth <- ctsd_truth[[2]] %#% tmpunit_truth
      
      out_err <- c(
        "lci" = ((out_est[[1]] %#% tmpunit) - truth) / truth,
        "est" = ((out_est[[2]] %#% tmpunit) - truth) / truth,
        "uci" = ((out_est[[3]] %#% tmpunit) - truth) / truth)
      
      vals$is_analyses <- TRUE
      vals$is_ctsd <- TRUE
      vals$ctsd_truth <- ctsd_truth
      vals$speedTruth <- data.frame(
        value = ctsd_truth[[2]], unit = tmpunit_truth)
      vals$speedErr <- data.frame(value = out_err)
      time_sd <- difftime(Sys.time(), start_truth, units = "sec")
      
      vals$sd$time[1] <- vals$sd$time[1] + 
        difftime(Sys.time(), start, units = "sec")[[1]]
      
      shinyjs::show(id = "sdBlock_est")
      shinyjs::show(id = "sdBlock_err")
      
      msg_log(
        style = "success",
        message = paste0(
          "Estimation ", msg_success("completed"),
          msg_step(2, 2, style = "success")),
        with_time = time_sd)
      
      shinyFeedback::showToast(
        type = "success",
        message = "Fine-scale trajectory completed!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"))
      
      vals$sd_completed <- TRUE
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      bindEvent(vals$sd$confirm_time)
    
    ## Estimating for new sampling design: --------------------------------
    
    timing_speed_new <- reactive({
      
      loading_modal("Calculating run time")
      msg_log(
        style = "warning",
        message = paste0("Estimating ", msg_warning("run time"), ":"))
      
      out_time <- guess_time(data = vals$sd$data, 
                             fit = vals$sd$fit, 
                             seed = vals$seed0,
                             type = "speed",
                             trace = TRUE,
                             parallel = vals$parallel)
                             
      shinybusy::remove_modal_spinner()
      return(out_time)
      
    }) %>% # end of reactive, timing_speed_new()
      bindCache(vals$sd$data,
                vals$sd$dur,
                vals$sd$dti)
    
    simulating_data_new <- reactive({
      
      dat <- vals$data1
      if (vals$fit0$isotropic == TRUE) { fit <- vals$fit0
      } else fit <- vals$ctmm_mod
      
      dur <- vals$sd$dur$value %#% vals$sd$dur$unit
      dti <- vals$sd$dti$value %#% vals$sd$dti$unit
      t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      
      # Fill in the gaps of original dataset + new duration:
      sim <- ctmm::simulate(dat, fit, t = t_new, seed = vals$seed0)
      sim <- pseudonymize(sim)
      sim$index <- 1:nrow(sim)
      return(sim)
      
    }) %>% # end of reactive, simulating_data_new()
      bindCache(vals$sd$dur,
                vals$sd$dti,
                vals$data1)
    
    fitting_model_new <- reactive({
      
      guess <- ctmm::ctmm.guess(vals$sd$data, interactive = FALSE)
      vals$guess_new <- guess
      
      if (vals$data_type == "simulated") {
        mod1 <- vals$ctmm_mod
      } else {
        mod1 <- prepare_mod(
          tau_p = vals$tau_p0$value[2], tau_p_units = vals$tau_p0$unit[2],
          tau_v = vals$tau_v0$value[2], tau_v_units = vals$tau_v0$unit[2],
          sigma = vals$sigma0$value[2], sigma_units = vals$sigma0$unit[2])
      }
      
      inputList <- list(list(vals$sd$data, mod1))
      fit <- par.ctmm.fit(inputList, parallel = TRUE)
      return(fit)
      
    }) %>% # end of reactive, fitting_model_new()
      bindCache(vals$sd$dur,
                vals$sd$dti,
                vals$sd$data)
    
    estimating_speed_new <- reactive({
      
      dat <- vals$sd$data
      
      # tauv <- vals$tau_v0$value[2] %#% vals$tau_v0$unit[2]
      # dti <- vals$dti$value %#% vals$dti$unit
      # dur <- vals$dur$value %#% vals$dur$unit
      # 
      # if (dur >= 20 * tauv && dti < 3 * tauv)
      #   dat <- dat[which(dat$t <= 20 * tauv), ]
      
      inputList <- align_lists(list(dat), list(vals$sd$fit))
      ctsd_new <- par.speed(inputList, parallel = vals$parallel)
      return(ctsd_new)
      
    }) %>% # end of reactive, estimating_speed_new()
      bindCache(vals$sd$dur,
                vals$sd$dti,
                vals$sd$data)
    
    observe({
      req(vals$data1, vals$fit1)
      
      # Capture new sampling duration and interval:
      
      device <- movedesign::fixrates
      dti_unit <- sub('^.* ([[:alnum:]]+)$', '\\1', input$sd_dti)
      dti <- dti_unit %#% round(
        device$dti[match(input$sd_dti, device$dti_notes)], 0)
      
      vals$sd$dti <- data.frame(
        value = dti,
        unit = dti_unit)
      
      vals$sd$dur <- data.frame(
        value = input$sd_dur,
        unit = "days")
      
      # Check if storage limitations:
      
      proceed <- TRUE
      if (!is.null(vals$storage)) {
        
        n_new <- length(
          seq(0, round(input$sd_dur %#% "days", 0),
              by = round(dti %#% dti_unit, 0))[-1])
        
        if (n_new >= vals$storage) {
          
          shinyalert::shinyalert(
            type = "warning",
            title = "Warning",
            text = tagList(span(
              "Many GPS units can only store a maximum of",
              wrap_none(
                span("32,000\u201464,000 locations", class = "cl-dgr"),
                "."), "You set a limit of", vals$storage,
              "making this sampling design invalid.",
              
              "Please select a different combination of",
              span("sampling duration", class = "cl-dgr"), "and",
              HTML(paste0(span("frequency", class = "cl-dgr"), "."))
            )),
            html = TRUE,
            size = "xs")
          
          msg_log(
            style = "danger",
            message = paste0("Reached storage limit of",
                             msg_danger(vals$storage), " locations."),
            detail = "Please set a different sampling design.")
          
          proceed <- FALSE
          
        } # end of length(t0) >= vals$storage)
      } # end of !is.null(vals$storage)
      
      req(proceed)
      vals$conditional <- TRUE
      removeModal()
      
      ### 1. Simulate new dataset:
      
      loading_modal("Simulating new design")
      msg_log(
        style = "warning",
        message = paste0("Simulating ",
                         msg_warning("new sampling design"), "."),
        detail = "This may take a while...")
      
      start <- Sys.time()
      dat <- simulating_data_new()
      vals$sd$data <- dat
      time_sd <- difftime(Sys.time(), start, units = "sec")
      
      msg_log(
        style = "success",
        message = paste0("Simulation ", msg_success("completed"), "."),
        with_time = time_sd)
      
      ### 2. Fit models to simulation:
      
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("in progress"), "."),
        detail = "Please wait for model selection to finish:")
      
      start_fit <- Sys.time()
      vals$sd$fit <- fitting_model_new()
      time_sd <- difftime(Sys.time(), start_fit, units = "sec")
      
      msg_log(
        style = "success",
        message = paste0("Model fit ", msg_success("completed"), "."),
        with_time = time_sd)
      
      shinybusy::remove_modal_spinner()
      
      ### 3. Estimate runnning time:
      
      expt <- timing_speed_new()
      vals$sd$expt2_range <- expt$range

      vals$sd$confirm_time2 <- NULL
      if ((as.numeric(expt$max) %#% expt$unit) > 900) {
        
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            vals$sd$confirm_time2 <- x
          },
          text = tagList(span(
            "Expected run time for estimation", br(),
            "is approximately",
            wrap_none(
            span(expt$range, class = "cl-dgr"), end = ".")
          )),
          type = "warning",
          showCancelButton = TRUE,
          cancelButtonText = "Stop",
          confirmButtonCol = pal$mdn,
          confirmButtonText = "Proceed",
          html = TRUE
        )
      } else { vals$sd$confirm_time2 <- TRUE }
      
    }, priority = 1) %>% # end of observe,
      bindEvent(input$run_sd_new)
    
    observe({
      req(vals$sd$data,
          vals$sd$fit,
          vals$sd$confirm_time2)
      
      ### 4. Run the speed/distance estimator (CTSD):
      
      tmpnms <- names(summary(vals$sd$fit)$DOF)
      N <- summary(vals$sd$fit)$DOF[grep("speed", tmpnms)][[1]]
      vals$N2_new <- N
      
      req(N)
      if (N < 5) {
        set_col <- "color: #dd4b39;"
        p_extra <- tagList(
          p("(high uncertainty due to low sample size)",
            style = set_col), p())
      } else {
        p_extra <- NULL
        set_col <- "color: #009da0;"
      }
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = span(
          span("Estimating new", style = "color: #797979;"),
          HTML(paste0(span("speed & distance", class = "cl-sea"),
                      span("...", style = "color: #797979;"))),
          p(),
          p("Expected run time:",
            style = paste("background-color: #eaeaea;",
                          "color: #797979;",
                          "font-size: 16px;",
                          "text-align: center;")), br(),
          p(vals$sd$expt2_range,
            style = paste("background-color: #eaeaea;",
                          set_col,
                          "font-size: 16px;",
                          "text-align: center;",
                          "margin-top: -40px;")), p(),
          p_extra
          
        ) # end of text
      ) # end of modal
      
      if (N < 5) {
        
        shinyalert::shinyalert(
          title = "Low sample size!",
          text = tagList(span(
            "Effective sample size for speed estimation",
            "is too low."
          )),
          html = TRUE,
          size = "xs")
        
        msg_log(
          style = "error",
          message = "Effective sample size too low.",
          detail = "Please select a different sampling design.")
        
      } else {
        
        start_est <- Sys.time()
        msg_log(
          style = "warning",
          message = paste0(
            "Estimating speed for ",
            msg_warning("new trajectory.")),
          detail = paste0("This should take around ",
                          vals$sd$expt2_range, "..."))
        
        ctsd_new <- estimating_speed_new()
        ctsd_data_new <- ctmm::speeds(vals$sd$data,
                                      vals$sd$fit,
                                      units = FALSE)
        
        if ("CI" %in% names(ctsd_new)) ctsd_new <- ctsd_new$CI
        
        tmpnms <- rownames(ctsd_new)
        tmpunit <- extract_units(tmpnms[grep("speed", tmpnms)])
        truth <- vals$speedTruth$value[1] %#% vals$speedTruth$unit[1]
        
        out_est <- c("lci" = ctsd_new[1],
                     "est" = ctsd_new[2],
                     "uci" = ctsd_new[3])
        
        out_err <- c(
          "lci" = ((out_est[[1]] %#% tmpunit) - truth) / truth,
          "est" = ((out_est[[2]] %#% tmpunit) - truth) / truth,
          "uci" = ((out_est[[3]] %#% tmpunit) - truth) / truth)
        
        vals$ctsd_new <- ctsd_new
        vals$ctsd_data_new <- ctsd_data_new
        vals$speedEst_new <- data.frame(value = out_est, "unit" = tmpunit)
        vals$speedErr_new <- data.frame(value = out_err)
        
        time_sd <- difftime(Sys.time(), start_est, units = "sec")
        vals$sd$time[2] <- vals$sd$time[2] + time_sd[[1]]
        
        msg_log(
          style = "success",
          message =  paste0(
            "Estimation ", msg_success("completed"), "."),
          with_time = time_sd)
      }
      
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      bindEvent(vals$sd$confirm_time2)
    
    ## Calculating total and mean distances: ------------------------------
    
    observe({
      req(vals$dur,
          vals$data_full, vals$ctsd, vals$speedEst)
      
      dist_full <- measure_distance(vals$data_full)
      dist <- measure_distance(vals$data1)
      
      truth <- sum(dist_full, na.rm = TRUE)
      vals$data_full$dist <- dist_full
      vals$distTruth <- truth # in meters
      
      # if (!is.null(vals$sd$fit)) {
      #   dur <- vals$sd$dur$value %#% vals$sd$dur$unit
      # } else { dur <- vals$dur$value %#% vals$dur$unit }
      dur <- vals$dur$value %#% vals$dur$unit
      dur_days <- "days" %#% dur
      
      oldunit <- vals$speedEst$unit[[1]]
      newunit <- "kilometers/day"
      
      sd_lci <- vals$speedEst$value[[1]]
      sd_est <- vals$speedEst$value[[2]]
      sd_uci <- vals$speedEst$value[[3]]
      
      if (oldunit == newunit) {
        dist_lci <- sd_lci * dur_days
        dist_est <- sd_est * dur_days
        dist_uci <- sd_uci * dur_days
      } else {
        dist_lci <- (newunit %#% sd_lci %#% oldunit) * dur_days
        dist_est <- (newunit %#% sd_est %#% oldunit) * dur_days
        dist_uci <- (newunit %#% sd_uci %#% oldunit) * dur_days
      }
      
      dist_unit <- "kilometers"
      truth <- dist_unit %#% truth
      
      out_est <- c("lci" = dist_lci,
                   "est" = dist_est,
                   "uci" = dist_uci)
      
      out_err <- c(
        "lci" = ((out_est[[1]]) - truth) / truth,
        "est" = ((out_est[[2]]) - truth) / truth,
        "uci" = ((out_est[[3]]) - truth) / truth)
      
      vals$distEst <- data.frame(value = out_est, "unit" = dist_unit)
      vals$distErr <- data.frame(value = out_err)
      
    }) # end of observe
    
    observe({
      req(vals$dur, 
          vals$distTruth, 
          vals$ctsd_new,
          vals$speedEst_new)
      
      # if (!is.null(vals$speedEst_new)) {
      #   dur <- vals$sd$dur$value %#% vals$sd$dur$unit
      # } else { dur <- vals$dur$value %#% vals$dur$unit }
      dur <- vals$dur$value %#% vals$dur$unit
      dur_days <- "days" %#% dur # TO VERIFY
      
      oldunit <- vals$speedEst_new$unit[[1]]
      newunit <- "kilometers/day"
      
      sd_lci <- vals$speedEst_new$value[[1]]
      sd_est <- vals$speedEst_new$value[[2]]
      sd_uci <- vals$speedEst_new$value[[3]]
      
      if (oldunit == newunit) {
        dist_lci <- sd_lci * dur_days
        dist_est <- sd_est * dur_days
        dist_uci <- sd_uci * dur_days
      } else {
        dist_lci <- (newunit %#% sd_lci %#% oldunit) * dur_days
        dist_est <- (newunit %#% sd_est %#% oldunit) * dur_days
        dist_uci <- (newunit %#% sd_uci %#% oldunit) * dur_days
      }
      
      dist_unit <- "kilometers"
      truth <- dist_unit %#% vals$distTruth
      
      out_est <- c("lci" = dist_lci,
                   "est" = dist_est,
                   "uci" = dist_uci)
      
      out_err <- c(
        "lci" = ((out_est[[1]]) - truth) / truth,
        "est" = ((out_est[[2]]) - truth) / truth,
        "uci" = ((out_est[[3]]) - truth) / truth)
      
      vals$distEst_new <- data.frame(value = out_est, "unit" = dist_unit)
      vals$distErr_new <- data.frame(value = out_err)
      
    }) # end of observe
    
    # PLOTS ---------------------------------------------------------------
    ## Plotting trajectory: -----------------------------------------------
    
    output$sdPlot_path <- ggiraph::renderGirafe({
      req(vals$data1, vals$data_full, input$show_paths)
      
      newdat <- vals$data1
      alldat <- vals$data_full
      
      datasets <- input$show_paths

      lims <- extract_limits(newdat, alldat)
      
      if (!is.null(vals$sd$data)) {
        lims <- extract_limits(newdat, alldat, vals$sd$data)
      }
      
      if ("full" %in% datasets) {
        p1 <- ggplot2::geom_path(
          alldat, mapping = ggplot2::aes(
            x = x, y = y),
          col = "grey70", size = 1.5)
        p1_main <- ggplot2::geom_path(
          alldat, mapping = ggplot2::aes(
            x = x, y = y),
          col = "grey30", size = 0.4)
      }

      if ("initial" %in% datasets) {
        p2 <- ggplot2::geom_path(
          newdat, mapping = ggplot2::aes(
            x = x, y = y),
          linewidth = 0.2, col = pal$sea)
        p2_points <- ggiraph::geom_point_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            tooltip = timestamp),
          size = 0.4, col = pal$sea)
      }

      if ("new" %in% datasets) {
        req(vals$sd$data)

        p3 <- ggplot2::geom_path(
          vals$sd$data, mapping = ggplot2::aes(
            x = x, y = y),
          linewidth = 0.2, col = pal$dgr)
        p3_points <- ggiraph::geom_point_interactive(
          vals$sd$data, mapping = ggplot2::aes(
            x = x, y = y,
            tooltip = timestamp),
          size = 0.4, col = pal$dgr)
      }

      p <- ggplot2::ggplot() +

        { if ("full" %in% datasets) p1 } +
        { if ("full" %in% datasets) p1_main } +
        
        { if ("initial" %in% datasets) p2 } +
        { if ("initial" %in% datasets) p2_points } +

        { if ("new" %in% datasets) p3 } +
        { if ("new" %in% datasets) p3_points } +

        ggplot2::labs(
          x = "X coordinate",
          y = "Y coordinate") +

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

    }) # end of renderGirafe, "sdPlot_path"
    
    ## Plotting speed (estimate vs. time): --------------------------------
    
    preparing_speed <- reactive({
      
      unit <- ifelse(input$sd_unit == "minutes", "mins", input$sd_unit)
      dat <- vals$ctsd_data
      
      t_origin <- "1111-11-10 23:06:32"
      dat$timestamp <- as.POSIXct(dat$t, origin = t_origin)
      
      if (unit != "weeks") {
        dat$t_new <- round.POSIXt(as.POSIXct(dat$timestamp), units = unit)
        dat <- dat %>% dplyr::mutate(
          t_new = as.POSIXct(t_new, format = "%Y-%m-%d %H:%M:%OS"))
        
      } else {
        dat <- dat %>% dplyr::mutate(t_new = format(timestamp, "%U"))
      }
      
      dat <- dat %>%
        dplyr::group_by(.data$t_new) %>%
        dplyr::summarise(est = mean(.data$est),
                         low = mean(.data$low),
                         high = mean(.data$high))
      
      unit <- vals$speedEst$unit[[2]]
      yline <- vals$speedEst$value[[2]] %#% unit
      yline_truth <- vals$speedTruth$value %#% 
        vals$speedTruth$unit
      
      yline_new <- NULL
      
      if (!is.null(vals$speedEst_new)) {
        yline_new <- vals$speedEst_new$value[[2]] %#% 
          vals$speedEst_new$unit[[2]]
      }
      
      return(list(
        data = dat,
        yline = yline,
        yline_truth = yline_truth,
        yline_new = yline_new))
      
    }) # end of reactive, preparing_speed()
    
    output$sdPlot_speed <- ggiraph::renderGirafe({
      req(vals$speedEst, vals$speedTruth,
          input$show_speeds,
          input$sd_unit)
      
      datasets <- input$show_speeds
      
      p <- preparing_speed()[["data"]] %>%
        
        ggplot2::ggplot(
          ggplot2::aes(x = .data$t_new, 
                       y = .data$est, 
                       group = 1)) +
        
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data$low, 
                       ymax = .data$high),
          fill = pal$sea, alpha = .3) +
        
        ggplot2::geom_line(
          color = pal$sea_m) +
        
        { if ("full" %in% datasets)
          ggplot2::geom_hline(
            yintercept = preparing_speed()[["yline_truth"]], 
            linewidth = 1, linetype = "solid") } +
        
        { if ("initial" %in% datasets)
          ggplot2::geom_hline(
            yintercept = preparing_speed()[["yline"]], 
            linewidth = 1.5, linetype = "dashed",
            col = pal$sea) } +
        
        { if (!is.null(vals$ctsd_new) && "new" %in% datasets)
          ggplot2::geom_hline(
            yintercept = preparing_speed()[["yline_new"]],
            linewidth = 1.5, linetype = "dashed",
            col = pal$dgr)  } +
        
        ggplot2::labs(
          x = "Time lag",
          y = "Speed estimate (meters/second)") +
        theme_movedesign()
      
      ggiraph::girafe(ggobj = p)
      
    }) # end of renderGirafe, "sdPlot_speed"
    
    # TABLES --------------------------------------------------------------
    ## Initial sampling design: -------------------------------------------

    sdRow <- reactive({

      out <- data.frame(
        data = "Initial",
        tauv = NA,
        dur = NA,
        dti = NA,
        n = nrow(vals$data1),
        N2 = vals$dev$N2,
        ctsd = NA,
        ctsd_err = vals$speedErr$value[[2]],
        ctsd_err_min = vals$speedErr$value[[1]],
        ctsd_err_max = vals$speedErr$value[[3]],
        dist = NA,
        dist_err = vals$distErr$value[[2]])

      out$tauv <- paste(
        scales::label_comma(.1)(vals$tau_v0$value[2]),
        abbrv_unit(vals$tau_v0$unit[2]))

      out_dur <- fix_unit(vals$dur$value, vals$dur$unit, convert = TRUE)
      out$dur <- paste(out_dur$value, abbrv_unit(out_dur$unit))

      out_dti <- fix_unit(vals$dti$value, vals$dti$unit)
      out$dti <- paste(out_dti$value, abbrv_unit(out_dti$unit))

      out_ctsd <- fix_unit(vals$speedEst$value[[2]],
                           vals$speedEst$unit[[2]])
      out$ctsd <- paste(out_ctsd$value, abbrv_unit(out_ctsd$unit))

      out_dist <- fix_unit(vals$distEst$value[[2]],
                           vals$distEst$unit[[2]], convert = TRUE)
      out$dist <- paste(out_dist$value, out_dist$unit)

      return(out)

    }) %>%
      bindCache(vals$dur$value, vals$dur$unit,
                vals$dti$value, vals$dti$unit)

    observe({
      req(vals$data1, vals$ctsd, vals$distErr, vals$distTruth)

      shinyjs::show(id = "sdBox_summary")

      vals$sd$tbl <<- rbind(vals$sd$tbl, sdRow())
      vals$sd$tbl <- dplyr::distinct(vals$sd$tbl)
      vals$report_sd_yn <- TRUE

    }) %>% # end of observe
      bindEvent(input$add_sd_table)

    ## New sampling design: -----------------------------------------------

    sdRow_new <- reactive({

      out <- data.frame(
        data = "Modified",
        tauv = NA,
        dur = NA,
        dti = NA,
        n = nrow(vals$sd$data),
        N2 = vals$N2_new,
        ctsd = NA,
        ctsd_err = vals$speedErr_new$value[[2]],
        ctsd_err_min = vals$speedErr_new$value[[1]],
        ctsd_err_max = vals$speedErr_new$value[[3]],
        dist = NA,
        dist_err = vals$distErr_new$value[[2]])
      
      out$tauv <- paste(
        scales::label_comma(.1)(vals$tau_v0$value[2]),
        abbrv_unit(vals$tau_v0$unit[2]))
      
      out_dur <- fix_unit(vals$sd$dur$value, vals$sd$dur$unit)
      out$dur <- paste(out_dur$value, abbrv_unit(out_dur$unit))

      out_dti <- fix_unit(vals$sd$dti$value, vals$sd$dti$unit)
      out$dti <- paste(out_dti$value, abbrv_unit(out_dti$unit))
      
      out_ctsd <- fix_unit(vals$speedEst_new$value[[2]],
                           vals$speedEst_new$unit[[2]])
      out$ctsd <- paste(out_ctsd$value, abbrv_unit(out_ctsd$unit))
      
      out_dist <- fix_unit(vals$distEst_new$value[[2]],
                           vals$distEst_new$unit[[2]], convert = TRUE)
      out$dist <- paste(out_dist$value, out_dist$unit)
      
      return(out)

    }) %>%
      bindCache(vals$sd$dur,
                vals$sd$dti)

    observe({
      req(vals$distErr_new)

      vals$sd$tbl <<- rbind(vals$sd$tbl, sdRow_new())
      vals$sd$tbl <- dplyr::distinct(vals$sd$tbl)
      vals$report_sd_yn <- TRUE

    }) %>% # end of observe
      bindEvent(input$add_sd_table)

    ## Rendering output table: --------------------------------------------

    output$sdTable <- reactable::renderReactable({
      req(vals$sd$tbl)

      nms <- list(
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
      
      nms_sizes <- reactable::colGroup(
        name = "Sample sizes", 
        columns = c("n", "N2"))
      nms_ctsd <- reactable::colGroup(
        name = "Speed",
        columns = c("ctsd",
                    "ctsd_err",
                    "ctsd_err_min",
                    "ctsd_err_max"))
      nms_dist <- reactable::colGroup(
        name = "Distance",
        columns = c("dist", "dist_err"))
      
      colgroups <- list(nms_sizes,
                        nms_ctsd,
                        nms_dist)
      
      namedcolumns <- list(
        data = reactable::colDef(
          name = nms[["data"]]),
        tauv = reactable::colDef(
          minWidth = 80, name = nms[["tauv"]],
          style = list(fontWeight = "bold")),
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
        N2 = reactable::colDef(
          minWidth = 80, name = nms[["N2"]],
          style = format_num,
          format = reactable::colFormat(separators = TRUE,
                                        digits = 1)),
        ctsd = reactable::colDef(
          minWidth = 80, name = nms[["ctsd"]]),
        ctsd_err = reactable::colDef(
          minWidth = 80, name = nms[["ctsd_err"]],
          style = format_perc,
          format = reactable::colFormat(percent = TRUE,
                                        digits = 1)),
        ctsd_err_min = reactable::colDef(
          minWidth = 80, name = nms[["ctsd_err_min"]],
          style = format_perc,
          format = reactable::colFormat(percent = TRUE,
                                        digits = 1)),
        ctsd_err_max = reactable::colDef(
          minWidth = 80, name = nms[["ctsd_err_max"]],
          style = format_perc,
          format = reactable::colFormat(percent = TRUE,
                                        digits = 1)),
        dist = reactable::colDef(
          minWidth = 80, name = nms[["dist"]]),
        dist_err = reactable::colDef(
          minWidth = 80, name = nms[["dist_err"]],
          style = format_perc,
          format = reactable::colFormat(percent = TRUE,
                                        digits = 1))
      )
      
      reactable::reactable(
        data = vals$sd$tbl,
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
        
        columns = namedcolumns,
        columnGroups = colgroups
        
      ) # end of reactable

    }) # end of renderReactable // sdTable

    observe({
      vals$sd$tbl <- NULL
    }) %>% # end of observe,
      bindEvent(input$sdTable_clear)

    # BLOCKS --------------------------------------------------------------

    ## Tracking device: ---------------------------------------------------
    ### Initial sampling design: ------------------------------------------

    observe({
      req(vals$data1)

      mod_blocks_server(
        id = "sdBlock_dur",
        vals = vals, data = vals$data1,
        type = "dur", input_name = "Sampling duration")

      mod_blocks_server(
        id = "sdBlock_dti",
        vals = vals, data = vals$data1,
        type = "dti", input_name = "Sampling interval")

    }) # end of observe

    ### New sampling design: ----------------------------------------------

    observe({
      req(vals$sd$data)

      mod_blocks_server(
        id = "sdBlock_dur_new",
        vals = vals, data = vals$sd$data,
        type = "dur", input_name = "Sampling duration",
        class = "cl-mdn")

      mod_blocks_server(
        id = "sdBlock_dti_new",
        vals = vals, data = vals$sd$data,
        type = "dti", input_name = "Sampling interval",
        class = "cl-mdn")

    }) # end of observe

    ## Sample sizes: ------------------------------------------------------

    output$sdBlock_n <- shiny::renderUI({
      req(vals$data1)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data1),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)

    }) # end of renderUI, "sdBlock_n"

    observe({
      req(vals$fit1)

      mod_blocks_server(
        id = "sdBlock_N",
        vals = vals, data = vals$data1, fit = vals$fit1,
        type = "N", name = "speed")

    }) # end of observe

    output$sdBlock_n_new <- shiny::renderUI({
      req(vals$sd$data)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$sd$data),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI, "sdBlock_n_new"

    observe({
      req(vals$hr$fit)

      mod_blocks_server(
        id = "sdBlock_N_new",
        vals = vals, data = vals$sd$data, fit = vals$sd$fit,
        type = "N", name = "speed", class = "cl-mdn")

    }) # end of observe

    ## Outputs: -----------------------------------------------------------
    ### Speed & distance estimates: ---------------------------------------

    observe({
      req(vals$ctsd, vals$speedEst, vals$speedErr)

      mod_blocks_server(
        id = "sdBlock_est",
        vals = vals, type = "ctsd", name = "speedEst")

      mod_blocks_server(
        id = "sdBlock_err",
        vals = vals, type = "ctsd", name = "speedErr")

    }) # end of observe

    output$sdBlock_err <- shiny::renderUI({
      req(vals$speedErr)

      errorBlock(
        icon = "radiation",
        text = span("Expected error from", br(),
                    "initial regime:"),
        value = vals$speedErr[[2]],
        min = vals$speedErr[[1]],
        max = vals$speedErr[[3]],
        rightBorder = FALSE)

    }) # end of renderUI // sdBlock_err

    observe({
      req(vals$ctsd_new, vals$speedEst_new, vals$speedErr_new)

      mod_blocks_server(
        id = "sdBlock_est_new",
        vals = vals, type = "ctsd", name = "speedEst_new",
        class = "cl-mdn")
      
      mod_blocks_server(
        id = "sdBlock_err_new",
        vals = vals, type = "ctsd", name = "speedErr_new",
        class = "cl-mdn")

    }) # end of observe

    ### Movement metrics: -------------------------------------------------

    observe({
      req(vals$ctsd, vals$distEst)

      mod_blocks_server(
        id = "distBlock_est",
        vals = vals, type = "dist", name = "distEst")

      mod_blocks_server(
        id = "distBlock_err",
        vals = vals, type = "dist", name = "distErr")

    }) # end of observe

    observe({
      req(vals$ctsd_new, vals$distEst_new)

      mod_blocks_server(
        id = "distBlock_est_new",
        vals = vals, type = "dist", name = "distEst_new")

      mod_blocks_server(
        id = "distBlock_err_new",
        vals = vals, type = "dist", name = "distErr_new")

    }) # end of observe

    # MISC ----------------------------------------------------------------

    observe({
      shinyjs::show(id = "sdBox_misc")  
    }) %>% bindEvent(vals$sd$time[1] > 0)
    
    output$out_time_sd <- renderText({
      req(vals$sd$time[1] > 0)
      
      out <- fix_unit(vals$sd$time[1], "seconds", convert = TRUE)
      paste0("Initial sampling design took approximately ",
             out$value, " ", out$unit, ".")

    }) # end of renderText, "time_sd"

    output$out_time_sd_new <- renderText({
      req(vals$sd$time[2] > 0)

      out <- fix_unit(vals$sd$time[2], "seconds", convert = TRUE)
      paste0("New sampling design took approximately ",
             out$value, " ", out$unit, ".")

    }) # end of renderText, "time_sd_new"

    output$out_time_sd_new <- renderText({
      req(vals$sd$time[1] > 0, vals$ctsd_new)

      total_time <- vals$sd$time[1] + vals$sd$time[2]

      out <- fix_unit(total_time, "seconds", convert = TRUE)
      paste0("... In total, this section took approximately",
             out$value, " ", out$unit, ".")

    }) # end of renderText, "time_sd_total"


    # Save information for report if table is not requested:

    observe({
      req(vals$is_analyses,
          vals$active_tab == 'ctsd')

      req(is.null(vals$sd$tbl))
      vals$report_sd_yn <- FALSE

      if (!is.null(vals$speedErr) &&
          !is.null(vals$distErr) ) {
        req(vals$speedErr)
        vals$report_sd_yn <- TRUE
        vals$sd$tbl <- sdRow()

      } else if (!is.null(vals$speedErr_new) &&
                 !is.null(vals$distErr_new) ) {
        req(vals$speedErr_new)
        vals$report_sd_yn <- TRUE
        vals$sd$tbl <- sdRow_new()
      }
      

    }) # end of observe
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_ctsd_ui("tab_ctsd_1")

## To be copied in the server
# mod_tab_ctsd_server("tab_ctsd_1")
