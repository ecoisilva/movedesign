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
                wrap_none(span("Run estimation", class = "cl-mdn")),
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

          uiOutput(ns("hrUI_show")),
          br()

      ), # end of div (top row)

      # [right column] ----------------------------------------------------

      div(class = div_column_left,

          ## sampling design: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Sampling design", class = "ttl-box_solid"),
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
                  column(width = 12, mod_blocks_ui(ns("hrBlock_dur"))),
                  column(width = 12, mod_blocks_ui(ns("hrBlock_dti"))),
                ) # end of fluidRow
                
              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("hrPanel_regime_new"),
                title = icon("bolt", class = "cl-mdn"),
                p(),
                
                fluidRow(
                  column(width = 12, mod_blocks_ui(ns("hrBlock_dur_new"))),
                  column(width = 12, mod_blocks_ui(ns("hrBlock_dti_new"))),
                ) # end of fluidRow
                
              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12, align = "center",
              
                shiny::actionButton(
                  inputId = ns("hrButton_compare"),
                  label = "Compare",
                  icon = icon("rotate-right"),
                  class = "btn-info",
                  width = "100%")
              
              # splitLayout(
              #   cellWidths = c("29%", "1%", "70%"),
              #   cellArgs = list(style = "align: center;"),
              # 
              #   shiny::actionButton(
              #     inputId = ns("hrHelp_regime"),
              #     label = NULL,
              #     width = "100%",
              #     icon = icon("circle-question"),
              #     class = "btn-warning"),
              #   br(),
              #   shiny::actionButton(
              #     inputId = ns("hrButton_compare"),
              #     label = "Modify",
              #     icon = icon("rotate-right"),
              #     class = "btn-info",
              #     width = "100%")
              # 
              # ) # end of splitLayout

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
                  column(width = 12, mod_blocks_ui(ns("hrBlock_N")))
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("hrPanel_sizes_new"),
                title = icon("bolt", class = "cl-mdn"),

                fluidRow(
                  column(width = 12, uiOutput(ns("hrBlock_n_new"))),
                  column(width = 12, mod_blocks_ui(ns("hrBlock_N_new")))
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
                      inputId = ns("hr_contours"),
                      label = "Show estimate levels:",
                      choices = c("95% low CI" = "lci",
                                  "Estimate" = "est",
                                  "95% high CI" = "uci"),
                      selected = "est",
                      checkIcon = list(yes = icon("circle-check")),
                      justified = TRUE),
                    p(),
                    
                    column(
                      width = 12, align = "center",
                      style = "display: contents;",
                      shinyWidgets::awesomeCheckbox(
                        inputId = ns("hr_truth"),
                        label = span("Show", span("true", 
                                                  class = "cl-sea"), 
                                     "home range"),
                        value = TRUE)), p(),
                    
                    ggiraph::girafeOutput(
                      outputId = ns("hrPlot"),
                      width = "100%", height = "100%")
                ),

                div(id = "content_hr-areas",
                    class = "col-xs-12 col-sm-12 col-md-12 col-lg-3",
                    p(class = "fluid-padding"),
                    
                    mod_blocks_ui(ns("hrBlock_est")),
                    mod_blocks_ui(ns("hrBlock_err")))

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
                      inputId = ns("hr_contours_new"),
                      label = "Show estimate levels:",
                      choices = c("95% low CI" = "lci",
                                  "Estimate" = "est",
                                  "95% high CI" = "uci"),
                      selected = "est",
                      checkIcon = list(yes = icon("circle-check")),
                      justified = TRUE),
                    p(),
                    
                    column(
                      width = 12, align = "center",
                      style = "display: contents;",
                      shinyWidgets::awesomeCheckbox(
                        inputId = ns("hr_truth_new"),
                        label = span("Show", span("true", 
                                                  class = "cl-sea"), 
                                     "home range"),
                        value = TRUE)), p(),
                    
                    ggiraph::girafeOutput(
                      outputId = ns("hrPlot_new"),
                      width = "100%", height = "100%"),
                    
                    column(
                      width = 12, align = "center",
                      shinyWidgets::awesomeCheckbox(
                        inputId = ns("hr_datasets"),
                        label = span("Add the",
                                     icon("stopwatch", class = "cl-sea"),
                                     span("Data", class = "cl-sea"),
                                     "locations to the plot"),
                        value = TRUE))
                    
                ), # end of div()
                
                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-3",
                    p(class = "fluid-padding"),
                    uiOutput(ns("hrText_new")),
                    
                    mod_blocks_ui(ns("hrBlock_est_new")),
                    mod_blocks_ui(ns("hrBlock_err_new")))

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12,
              style = "max-width: 250px; float: right;",
              
              splitLayout(
                cellWidths = c("20%", "1%", "79%"),
                cellArgs = list(style = "align: center;"),
                
                shiny::actionButton(
                  inputId = ns("hrHelp_bias"),
                  label = NULL,
                  width = "100%",
                  icon = icon("circle-question"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("show_hr_table"),
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

            verbatimTextOutput(outputId = ns("time_hr")),
            verbatimTextOutput(outputId = ns("time_hr_new")),
            verbatimTextOutput(outputId = ns("time_hr_total"))
            
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
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    vals$hr <- reactiveValues(time = c(0,0), completed = FALSE)
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at start: --------------------------------------------
    
    boxnames <- c("regime",
                  "sizes",
                  "viz",
                  "areas",
                  "summary",
                  "misc")
    
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("hrBox_", boxnames[i]))
    }
    
    tabnames <- c("regime", "area", "sizes", "viz")
    for (i in 1:length(tabnames)) {
      tmp_id <- paste0("hrTabs_", tabnames[i])
      tmp_target <- paste0("hrPanel_", tabnames[i], "_new")
      hideTab(inputId = tmp_id, target = ns(tmp_target))
    }
    
    ## Select all tabs after new sampling design: -------------------------
    
    observe({
      tmp <- ifelse(input$hrInput_show == 1, "", "_new")
      tabs <- paste0("hrTabs_", tabnames)
      panels <- paste0("hrPanel_", tabnames)
      
      for (i in 1:length(tabnames)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[i]),
          selected = paste0("tab_hrange_1-", panels[i], tmp)) }
      
    }) %>% # end of observe.
      bindEvent(input$hrInput_show)
    
    ## Sample size boxes for comparing sampling designs: ------------------
    
    output$hrUI_compare_n <- renderUI({
      req(vals$data1, input$hr_dur, input$hr_dti)
      
      device <- movedesign::fixrates
      dti <- device$dti[match(input$hr_dti, device$dti_notes)]
      
      n_new <- length(
        seq(0, round(input$hr_dur %#% "days", 0),
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
      
    }) # end of renderUI // hrUI_compare_n
    
    ## Show new conditional panels if comparing: --------------------------
    
    observe({
      req(vals$hr$data)
      req(vals$hr$fit)
      
      # Show new conditional simulation panels:
      for (i in 1:length(tabnames)) {
        tmp_id <- paste0("hrTabs_", tabnames[i])
        tmp_target <- paste0("hrPanel_", tabnames[i], "_new")
        showTab(inputId = tmp_id, target = ns(tmp_target))
      }
      
      # Select new conditional simulation panels:
      tabs <- paste0("hrTabs_", tabnames)
      panels <- paste0("hrPanel_", tabnames)
      for (i in 1:length(tabnames))
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[i]),
          selected = paste0("tab_hrange_1-",
                            panels[i], "_new"))
      
    }) %>% # end of observe,
      bindEvent(list(vals$hr$data, vals$hr$fit))
    
    ## Show buttons to change all panels: ---------------------------------
    
    output$hrUI_show <- renderUI({
      req(vals$hr$data)
      
      shinyWidgets::radioGroupButtons(
        inputId = ns("hrInput_show"),
        label = NULL,
        choices = c("Show initial sampling design" = 1,
                    "Show modified sampling design" = 2),
        checkIcon = list(yes = icon("circle-check")),
        selected = 2,
        justified = TRUE)
      
    }) # end of renderUI, "hrUI_show"
    
    ## Writing new regime text: -------------------------------------------
    
    writing_regime_new <- reactive({
      req(vals$hr$dur, vals$hr$dti)
      
      out_dti <- fix_unit(vals$hr$dti$value, vals$hr$dti$unit)
      txt_dti <- ifelse(out_dti$value == 1,
                        paste(out_dti$unit),
                        paste(out_dti$value, out_dti$unit))
      
      dur <- vals$hr$dur$value
      dur_mth <- "months" %#% vals$hr$dur$value %#% vals$hr$dur$unit
      
      
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
    
    output$hrText_new <- renderUI(writing_regime_new())
    
    ## Toggle checkbox for true home range: -------------------------------
    
    observe({
      if (!is.null(input$hr_truth_new))
        shinyWidgets::updateAwesomeCheckbox(
          session = session, 
          inputId = "hr_truth_new",
          value = input$hr_truth)
      
    }) %>% # end of observe,
      bindEvent(input$hr_truth)
    
    observe({
      shinyWidgets::updateAwesomeCheckbox(
        session = session, 
        inputId = "hr_truth",
        value = input$hr_truth_new)
      
    }) %>% # end of observe,
      bindEvent(input$hr_truth_new)
    
    # ALERTS --------------------------------------------------------------
    
    ## If no initial data uploaded, selected or simulated:
    
    observe({
      req(vals$active_tab == 'hr')
      
      if (is.null(vals$data0))
        shinyalert::shinyalert(
          type = "error",
          title = "No data found",
          text = tagList(span(
            "Please upload, select or simulate an", br(),
            span("movement dataset", class = "cl-dgr"),
            "first in the",
            icon("paw", class = "cl-mdn"),
            span("Data", class = "cl-mdn"), "tabs."
          )),
          html = TRUE,
          size = "xs")
      
    }) # end of observe
    
    ## If no sampling design was set:
    
    observe({
      req(vals$active_tab == 'hr')
      
      if (is.null(vals$data1))
        shinyalert::shinyalert(
          type = "error",
          title = "No sampling design set",
          text = tagList(span(
            "Please go to the",
            icon("stopwatch", class = "cl-mdn"),
            span("Sampling design", class = "cl-mdn"), "tab",
            "and make sure to both (1) set and validate",
            "a sampling duration and interval, and",
            "(2) run a new simulation by pressing the",
            icon("bolt", class = "cl-dgr"),
            span("'Run'", class = "cl-mdn"), "button."
          )),
          html = TRUE,
          size = "xs")
      
    }) # end of observe
    
    # If comparing sampling designs before running it once:
    
    observe({
      if (is.null(vals$akde))
        shinyalert::shinyalert(
          title = "Error",
          text = tagList(span(
            
            "First, estimate the home range",
            "based on the original dataset",
            "by clicking the",
            icon("paper-plane", class = "cl-mdn"),
            wrap_none(span("Run estimation", class = "cl-mdn")),
            "button."
          )),
          html = TRUE,
          size = "xs")
      
    }) %>% # end of observe,
      bindEvent(input$hrButton_compare)
    
    ## If no data available after clicking run_hr:
    
    observe({
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
        
        if (vals$tmpid != vals$id)
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
        
      } # end of if ()
      
    }) %>% # end of observe,
      bindEvent(input$run_hr)
    
    # OPERATIONS ----------------------------------------------------------
    ## Fitting movement model (if needed): --------------------------------
    
    estimating_time <- reactive({
      out_time <- guesstimate_time(vals$data1, 
                                   parallel = vals$parallel)
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
      req(vals$active_tab == 'hr',
          vals$data1,
          vals$dev$is_valid,
          vals$needs_fit)
      
      vals$fit1 <- NULL
      vals$is_analyses <- NULL
      
      loading_modal("Calculating run time")
      expt <- estimating_time()
      
      confirm_time <- NULL
      if ((as.numeric(expt$max) %#% expt$unit) > 900) {
        
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            confirm_time <- x
          },
          text = tagList(span(
            "Expected run time for the next phase", br(),
            "is approximately",
            span(expt$min, "\u2013", expt$max,
                 class = "cl-dgr"),
            wrap_none(span(expt$unit,
                           class = "cl-dgr"), ".")
          )),
          type = "warning",
          showCancelButton = TRUE,
          cancelButtonText = "Stop",
          confirmButtonCol = pal$mdn,
          confirmButtonText = "Proceed",
          html = TRUE
        )
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
      
      start_fit <- Sys.time()
      fit1 <- fitting_model()
      time_fit1 <- difftime(Sys.time(), start, units = "sec")
      vals$hr$time[1] <- vals$hr$time[1] + time_fit1[[1]]
      
      msg_log(
        style = 'success',
        message = paste0("Model fit ",
                         msg_success("completed"), "."),
        with_time = time_fit1)
      
      vals$needs_fit <- FALSE
      vals$fit1 <- fit1
      
      tmpnms <- names(summary(vals$fit1)$DOF)
      N1 <- summary(vals$fit1)$DOF[grep('area', tmpnms)][[1]]
      N2 <- summary(vals$fit1)$DOF[grep('speed', tmpnms)][[1]]
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
      shinyjs::show(id = "hrBox_regime")
      shinyjs::show(id = "hrBox_sizes")
    }) %>% bindEvent(vals$fit1)
    
    ## Comparing sampling designs: ----------------------------------------
    # Adjust sampling parameters necessary for simulation:
    
    observe({
      req(vals$tau_p0, vals$tau_v0,
          vals$dur, vals$dti,
          vals$akde)
      
      # Sampling duration:
      
      dur <- "days" %#% vals$dur$value %#% vals$dur$unit
      taup <- "days" %#% vals$tau_p0$value[2] %#% vals$tau_p0$unit[2]
      dur <- round(dur, 0)
      taup <- round(taup, 0)
      
      opts_dur <- c(10, 
                    dur, 
                    taup, 
                    taup * c(10, 50,
                             100, 200, 400, 600, 800, 
                             1000, 2000)
      ) %>% round_any(5, f = round) %>%
        unique() %>% sort()
      opts_dur <- opts_dur[opts_dur != 0]
      
      # Sampling interval:
      
      device <- movedesign::fixrates %>% 
        dplyr::arrange(dplyr::desc(.data$frq))
      dti <- vals$dti$value %#% vals$dti$unit
      index <- which.min(abs(device$dti - dti))
      
      opts_dti <- device$dti_notes
      
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
            
            parBlock(
              header = span(
                wrap_none("Position autocorrelation ",
                          "(\u03C4", tags$sub("p"), ")")),
              value =
                paste(scales::label_comma(
                  accuracy = .1)(vals$tau_p0$value[2]),
                  vals$tau_p0$unit[2]),
              subtitle = tmprange),
            
            p("If home range estimation is your goal,",
              "we recommend that the",
              span("sampling duration", class = "cl-sea"),
              "is that least 10 times the",
              span(wrap_none("position autocorrelation ",
                             "(\u03C4", tags$sub("p"), ")"),
                   class = "cl-sea"), "value (shown above).",
              "The", span("sampling interval (\u0394t)",
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
              choices = opts_dur,
              selected = taup * 10 %>%
                round_any(5, f = round),
              from_min = ifelse(dur > taup * 10,
                                dur, taup * 10) %>%
                round_any(5, f = round),
              from_max = ifelse(dur > taup * 800,
                                dur, taup * 2000) %>%
                round_any(5, f = round)
            ),
            
            shinyWidgets::sliderTextInput(
              inputId = ns("hr_dti"),
              label = "Sampling interval:",
              width = "100%",
              
              choices = opts_dti,
              
              selected = opts_dti[index],
              from_min = opts_dti[index],
              from_max = opts_dti[nrow(opts_dti)]),
            
            uiOutput(ns("hrUI_compare_n")),
            
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
      
    }) %>% # end of observe,
      bindEvent(input$hrButton_compare)
    
    # ESTIMATIONS ---------------------------------------------------------
    
    # Calculate and create true home range:
    estimating_truth <- reactive({
      
      sig <- vals$sigma0$value[2] %#% vals$sigma0$unit[2]
      area <- -2 * log(0.05) * pi * sig
      radius_x <- radius_y <- sqrt(-2 * log(0.05) * sig)
      truth <- data.frame(
        id = rep(1, each = 100),
        angle = seq(0, 2 * pi, length.out = 100))
      truth$x <- unlist(lapply(
        mean(vals$data1$x), function(x) x + radius_x * cos(truth$angle)))
      truth$y <- unlist(lapply(
        mean(vals$data1$y), function(x) x + radius_y * sin(truth$angle)))
      
      # xy <- truth %>% dplyr::select(long, lat)
      # truth <- sp::SpatialPointsDataFrame(
      #   coords = xy, data = truth,
      #   proj4string = sp::CRS(vals$crs))
      
      return(list(area = area, data = truth))
      
    }) # end of reactive, "estimating_truth"
    
    ## Estimating for initial sampling design: ----------------------------
    
    estimating_hr <- reactive({
      ctmm::akde(data = vals$data1, CTMM = vals$fit1)
      
    }) %>% # end of reactive, "estimating_hr"
      bindCache(vals$dur,
                vals$dti,
                vals$data1)
    
    observe({
      req(vals$data1,
          vals$fit1,
          vals$tmpid == vals$id)
      
      tmplist <- list("hrBox_areas",
                      "hrBox_viz",
                      "hrBox_misc")
      
      for (i in 1:length(tmplist)) shinyjs::show(id = tmplist[i])
      
      start <- Sys.time()
      
      msg_log(
        style = "warning",
        message = paste0("Estimating ",
                         msg_warning("home range"), ":"))
      
      loading_modal("Estimating home range")
      
      akde <- estimating_hr()
      truth <- estimating_truth()[["area"]]
      
      tmpnms <- rownames(summary(akde)$CI)
      tmpunit <- extract_units(tmpnms[grep('^area', tmpnms)])
      
      out_est <- c(
        "lci" = summary(akde)$CI[1],
        "est" = summary(akde)$CI[2],
        "uci" = summary(akde)$CI[3])
      
      out_err <- c(
        "lci" = ((out_est[[1]] %#% tmpunit) - truth) / truth,
        "est" = ((out_est[[2]] %#% tmpunit) - truth) / truth,
        "uci" = ((out_est[[3]] %#% tmpunit) - truth) / truth)
      
      vals$is_analyses <- TRUE
      vals$akde <- akde
      vals$hrErr <- data.frame(value = out_err)
      vals$hrEst <- data.frame(value = out_est, "unit" = tmpunit)
      time_hr <- difftime(Sys.time(), start, units = "sec")
      vals$hr$time[1] <- vals$hr$time[1] + time_hr[[1]]
      
      msg_log(
        style = "success",
        message = paste0("Estimation ",
                         msg_success("completed"), "."),
        with_time = vals$time_hr)
      
      vals$hr$completed <- TRUE
      
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      bindEvent(input$run_hr)
    
    ## Estimating for new sampling design: --------------------------------
    
    simulating_data_new <- reactive({
      
      dur <- vals$hr$dur$value %#% vals$hr$dur$unit
      dti <- vals$hr$dti$value %#% vals$hr$dti$unit
      t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      
      # Fill in the gaps of original dataset + new duration:
      dat <- ctmm::simulate(
        vals$data0, vals$fit0, t = t_new, seed = vals$seed0)
      
      dat <- pseudonymize(dat)
      dat$index <- 1:nrow(dat)
      return(dat)
      
    }) %>% # end of reactive, simulating_data_new()
      bindCache(vals$hr$dur,
                vals$hr$dti,
                vals$data0,
                vals$fit0)
    
    fitting_model_new <- reactive({
      
      guess <- ctmm::ctmm.guess(vals$hr$data, interactive = FALSE)
      vals$guess_new <- guess
      
      if (vals$data_type == "simulated") {
        mod1 <- vals$ctmm_mod
      } else {
        mod1 <- prepare_mod(
          tau_p = vals$tau_p0$value[2], tau_p_units = vals$tau_p0$unit[2],
          tau_v = vals$tau_v0$value[2], tau_v_units = vals$tau_v0$unit[2],
          sigma = vals$sigma0$value[2], sigma_units = vals$sigma0$unit[2])
      }
      
      inputList <- list(list(vals$hr$data, mod1))
      fit <- par.ctmm.fit(inputList, parallel = TRUE)
      return(fit)
      
    }) %>% # end of reactive, fitting_model_new()
      bindCache(vals$hr$dur,
                vals$hr$dti,
                vals$hr$data)
    
    estimating_hr_new <- reactive({
      ctmm::akde(data = vals$hr$data, CTMM = vals$hr$fit)
      
    }) %>% # end of reactive, estimating_hr_new()
      bindCache(vals$hr$dur,
                vals$hr$dti,
                vals$hr$data)
    
    observe({
      req(vals$data1, vals$fit1)
      
      # Capture new sampling duration and interval:
      
      device <- movedesign::fixrates
      dti_unit <- sub('^.* ([[:alnum:]]+)$', '\\1', input$hr_dti)
      dti <- dti_unit %#% round(
        device$dti[match(input$hr_dti, device$dti_notes)], 0)
      
      vals$hr$dti <- data.frame(
        value = dti,
        unit = dti_unit)
      
      vals$hr$dur <- data.frame(
        value = input$hr_dur,
        unit = "days")
      
      # Check if storage limitations:
      
      proceed <- TRUE
      if (!is.null(vals$storage)) {
        
        n_new <- length(
          seq(0, round(input$hr_dur %#% "days", 0),
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
      vals$hr$data <- dat
      time_hr <- difftime(Sys.time(), start, units = "sec")
      
      msg_log(
        style = "success",
        message = paste0("Simulation ", msg_success("completed"), "."),
        with_time = time_hr)
      
      ### 2. Fit models to simulation:
      
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("in progress"), "."),
        detail = "Please wait for model selection to finish:")
      
      start_fit <- Sys.time()
      vals$hr$fit <- fitting_model_new()
      time_sd <- difftime(Sys.time(), start_fit, units = "sec")
      
      msg_log(
        style = "success",
        message = paste0("Model fit ", msg_success("completed"), "."),
        with_time = time_sd)
      
      shinybusy::remove_modal_spinner()
      
      ### 3. Run the home range estimator (AKDE):
      
      loading_modal("Estimating new home range")
      
      tmpnms <- names(summary(vals$hr$fit)$DOF)
      N <- summary(vals$hr$fit)$DOF[grep('area', tmpnms)][[1]]
      vals$N1_new <- N
      
      if (N < 5) {
        
        shinyalert::shinyalert(
          title = "Low sample size!",
          text = tagList(span(
            "Effective sample size for area estimation",
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
            "Estimating new ", msg_warning("home range"), "..."),
          detail = "This may take a while...")
        
        akde_new <- estimating_hr_new()
        truth <- estimating_truth()[["area"]]
        
        tmpnms <- rownames(summary(akde_new)$CI)
        tmpunit <- extract_units(tmpnms[grep('^area', tmpnms)])
        
        out_est <- c(
          "lci" = summary(akde_new)$CI[1],
          "est" = summary(akde_new)$CI[2],
          "uci" = summary(akde_new)$CI[3])
        
        out_err <- c(
          "lci" = ((out_est[[1]] %#% tmpunit) - truth) / truth,
          "est" = ((out_est[[2]] %#% tmpunit) - truth) / truth,
          "uci" = ((out_est[[3]] %#% tmpunit) - truth) / truth)
        
        vals$is_analyses <- TRUE
        vals$akde_new <- akde_new
        vals$hrErr_new <- data.frame(value = out_err)
        vals$hrEst_new <- data.frame(value = out_est, "unit" = tmpunit)
        time_hr <- difftime(Sys.time(), start_est, units = "sec")
        vals$hr$time[2] <- vals$hr$time[2] + 
          difftime(Sys.time(), start, units = "sec")[[1]]
        
        msg_log(
          style = "success",
          message = paste0("Estimation ",
                           msg_success("completed"), "."),
          with_time = time_hr)
        
      } # end of if ()
      
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      shiny::bindEvent(input$run_hr_new)
    
    # PLOTS ---------------------------------------------------------------
    ## Plotting home range: -----------------------------------------------
    
    output$hrPlot <- ggiraph::renderGirafe({
      req(vals$is_analyses, vals$akde)
      
      if (!is.null(input$hr_truth)) {
        show_truth <- ifelse(input$hr_truth, TRUE, FALSE)
      } else { show_truth <- FALSE }
      
      truth <- estimating_truth()[["data"]]
      
      if (is.null(vals$hr$data)) {
        ext <- ctmm::extent(list(vals$data1,
                                 vals$akde))
      } else {
        ext <- ctmm::extent(list(vals$data1, 
                                 vals$hr$data,
                                 vals$akde, 
                                 vals$akde_new))
      }
      
      ud <- plotting_hr(
        input1 = list(data = vals$data1, ud = vals$akde),
        truth = truth,
        show_truth = show_truth,
        contours = input$hr_contours,
        color = pal$sea,
        extent = ext)
      
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
      
    }) # end of renderGirafe, "hrPlot"
    
    ## Plotting new home range: -------------------------------------------
    
    output$hrPlot_new <- ggiraph::renderGirafe({
      req(vals$is_analyses, vals$hr$data, vals$akde_new)
      
      if (!is.null(input$hr_truth_new)) {
        show_truth <- ifelse(input$hr_truth_new, TRUE, FALSE)
      } else { show_truth <- FALSE }
      
      # Rendering home range estimate plot:
      
      truth <- estimating_truth()[["data"]]
      ext <- ctmm::extent(list(vals$data1, 
                               vals$hr$data,
                               vals$akde, 
                               vals$akde_new))
      
      ud_sim <- plotting_hr(
        input1 = list(data = vals$data1, ud = vals$akde),
        input2 = list(data = vals$hr$data, ud = vals$akde_new),
        truth = truth,
        show_truth = show_truth,
        show_both = input$hr_datasets,
        contours = input$hr_contours_new,
        color = pal$dgr,
        extent = ext)
      
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
    ## Initial sampling design: -------------------------------------------
    
    hrRow <- reactive({
      
      out <- data.frame(
        data = "Initial",
        taup = NA,
        dur = NA,
        dti = NA,
        n = nrow(vals$data1),
        N1 = vals$dev$N1,
        area = NA,
        area_err = vals$hrErr$value[[2]],
        area_err_min = vals$hrErr$value[[1]],
        area_err_max = vals$hrErr$value[[3]])
      
      out$taup <- paste(
        scales::label_comma(.1)(vals$tau_p0$value[2]),
        abbrv_unit(vals$tau_p0$unit[2]))
      
      out_dur <- fix_unit(vals$dur$value, vals$dur$unit)
      out$dur <- paste(out_dur$value, abbrv_unit(out_dur$unit))
      
      out_dti <- fix_unit(vals$dti$value, vals$dti$unit)
      out$dti <- paste(out_dti$value, abbrv_unit(out_dti$unit))
      
      area <- scales::label_comma(.1)(vals$hrEst$value[[2]])
      out$area <- paste(area, abbrv_unit(vals$hrEst$unit[[2]]))
      
      return(out)
      
    }) %>% # end of reactive, hrRow()
      bindCache(vals$dur,
                vals$dti)
    
    observe({
      req(vals$data1, vals$dev$N1, vals$hrErr)
      
      shinyjs::show(id = "hrBox_summary")
      
      vals$hr$tbl <<- rbind(vals$hr$tbl, hrRow())
      vals$hr$tbl <- dplyr::distinct(vals$hr$tbl)
      vals$report_hr_yn <- TRUE
      
    }) %>% # end of observe
      bindEvent(input$show_hr_table)
    
    ## New sampling design: -----------------------------------------------
    
    hrRow_new <- reactive({
      
      out <- data.frame(
        data = "Modified",
        taup = NA,
        dur = NA,
        dti = NA,
        n = nrow(vals$hr$data),
        N1 = vals$N1_new,
        area = NA,
        area_err = vals$hrErr_new$value[[2]],
        area_err_min = vals$hrErr_new$value[[1]],
        area_err_max = vals$hrErr_new$value[[3]])
      
      out$taup <- paste(
        scales::label_comma(.1)(vals$tau_p0$value[2]),
        abbrv_unit(vals$tau_p0$unit[2]))
      
      out_dur <- fix_unit(vals$hr$dur$value, vals$hr$dur$unit, 
                          convert = TRUE)
      out$dur <- paste(out_dur[1], abbrv_unit(out_dur[,2]))
      
      out_dti <- fix_unit(vals$hr$dti$value, vals$hr$dti$unit)
      out$dti <- paste(out_dti$value, abbrv_unit(out_dti$unit))
      
      area <- scales::label_comma(.1)(vals$hrEst_new$value[[2]])
      out$area <- paste(area, abbrv_unit(vals$hrEst_new$unit[[2]]))
      
      return(out)
      
    }) %>%
      bindCache(vals$hr$dur,
                vals$hr$dti)
    
    observe({
      req(vals$hr$fit, vals$hrEst_new)
      
      vals$hr$tbl <<- rbind(vals$hr$tbl, hrRow_new())
      vals$hr$tbl <- dplyr::distinct(vals$hr$tbl)
      vals$report_hr_yn <- TRUE
      
    }) %>% # end of observe
      bindEvent(vals$hr$fit)
    
    ## Rendering output table: --------------------------------------------
    
    output$hrTable <- reactable::renderReactable({
      req(vals$hr$tbl)
      
      columnNames <- list(
        data = "Data:",
        taup = "\u03C4\u209A",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = "N (area)",
        area = "Area",
        area_err = "Error",
        area_err_min = "Error (min)",
        area_err_max = "Error (max)")
      
      reactable::reactable(
        data = vals$hr$tbl,
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
      
    }) # end of renderReactable, "hrTable"
    
    # BLOCKS --------------------------------------------------------------
    
    ## Tracking device: ---------------------------------------------------
    ### Initial sampling design: ------------------------------------------
    
    observe({
      req(vals$data1)
      
      mod_blocks_server(
        id = "hrBlock_dur", 
        vals = vals, data = vals$data1,
        type = "dur", input_name = "Sampling duration")
      
      mod_blocks_server(
        id = "hrBlock_dti", 
        vals = vals, data = vals$data1,
        type = "dti", input_name = "Sampling interval")
      
    }) # end of observe
    
    ### Modified sampling design: -----------------------------------------
    
    observe({
      req(vals$hr$data)
      
      mod_blocks_server(
        id = "hrBlock_dur_new", 
        vals = vals, data = vals$hr$data,
        type = "dur", input_name = "Sampling duration",
        class = "cl-mdn")
      
      mod_blocks_server(
        id = "hrBlock_dti_new", 
        vals = vals, data = vals$hr$data,
        type = "dti", input_name = "Sampling interval",
        class = "cl-mdn")
      
    }) # end of observe
    
    ## Sample sizes: ------------------------------------------------------
    
    output$hrBlock_n <- shiny::renderUI({
      req(vals$data1)
      
      sampleBlock(
        number = NULL,
        numberIcon = FALSE,
        header = nrow(vals$data1),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)
      
    }) # end of renderUI, "hrBlock_n"
    
    observe({
      req(vals$fit1)
      
      mod_blocks_server(
        id = "hrBlock_N", 
        vals = vals, data = vals$data1, fit = vals$fit1,
        type = "N", name = "area")
      
    }) # end of observe
    
    output$hrBlock_n_new <- shiny::renderUI({
      req(vals$hr$data)
      
      sampleBlock(
        number = NULL,
        numberIcon = FALSE,
        header = nrow(vals$hr$data),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = FALSE)
      
    }) # end of renderUI, 'hrBlock_n_new"
    
    observe({
      req(vals$hr$fit)
      
      mod_blocks_server(
        id = "hrBlock_N_new", 
        vals = vals, data = vals$hr$data, fit = vals$hr$fit,
        type = "N", name = "area", class = "cl-mdn")
      
    }) # end of observe
    
    ## Outputs: -----------------------------------------------------------
    ### Home range area: --------------------------------------------------
    
    observe({
      req(vals$akde, vals$hrEst, vals$hrErr)
      
      mod_blocks_server(
        id = "hrBlock_est",
        vals = vals, type = "hr", name = "hrEst")
      
      mod_blocks_server(
        id = "hrBlock_err", 
        vals = vals, type = "hr", name = "hrErr")
      
    }) # end of observe
    
    observe({
      req(vals$akde_new, vals$hrEst_new, vals$hrErr_new)
      
      mod_blocks_server(
        id = "hrBlock_est_new", 
        vals = vals, type = "hr", name = "hrEst_new",
        class = "cl-mdn")
      
      mod_blocks_server(
        id = "hrBlock_err_new", 
        vals = vals, type = "hr", name = "hrErr_new",
        class = "cl-mdn")
      
    }) # end of observe
    
    # HELP TOUR & MODALS --------------------------------------------------
    ## Help tour (sampling design): ---------------------------------------
    
    observe({
      
      element <- intro <- character(0)
      element <- c(element, "#Tour_main")
      
      intro <- c(
        intro,
        HTML(paste(
          "Click the ",
          fontawesome::fa("wrench", fill = pal$sea),
          span("Modify", class = "cl-sea"), "button",
          "to adjust the sampling design."))
      )
      
      # element <- c(element, "#hr_intro")
      # 
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     "hr_intro."))
      # )
      
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
    
    ## Help modal (biases): -----------------------------------------------
    
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
    
    # MISC ----------------------------------------------------------------
    
    output$time_hr <- renderText({
      req(vals$hr$time[1] > 0)
      
      out <- fix_unit(vals$hr$time[1], "seconds", convert = TRUE)
      paste0("Initial sampling design took approximately ",
             out$value, " ", out$unit, ".")
      
    }) # end of renderText, "time_hr"
    
    output$time_hr_new <- renderText({
      req(vals$hr$time[2] > 0)
      
      out <- fix_unit(vals$hr$time[2], "seconds", convert = TRUE)
      paste0("New sampling design took approximately ",
             out$value, " ", out$unit, ".")
      
    }) # end of renderText, "time_hr_new"
    
    output$time_hr_new <- renderText({
      req(vals$hr$time[1] > 0, vals$cthr_new)
      
      total_time <- vals$hr$time[1] + vals$hr$time[2]
      
      out <- fix_unit(total_time, "seconds", convert = TRUE)
      paste0("... In total, this section took approximately",
             out$value, " ", out$unit, ".")
      
    }) # end of renderText, "time_hr_total"
    
    
    # Save information for report if table is not requested:
    
    observe({
      req(vals$active_tab == 'hr',
          vals$is_analyses)
      
      req(is.null(vals$hr$tbl))
      vals$report_hr_yn <- FALSE
      
      if (is.null(vals$hrErr_new)) {
        req(vals$hrErr)
        vals$report_hr_yn <- TRUE
        vals$hr$tbl <- hrRow()
      } else {
        req(vals$hrErr_new)
        vals$report_hr_yn <- TRUE
        vals$hr$tbl <- hrRow_new()
      }
      
    }) %>% # end of observe,
      bindEvent(list(input$run_hr, input$run_hr_new))
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_hrange_ui("tab_hrange_1")

## To be copied in the server
# mod_tab_hrange_server("tab_hrange_1")
