#' tab_ctsd UI Function
#'
#' @description Estimate speed & distance.
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

      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",

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

      div(class = "col-xs-12 col-sm-4 col-md-4 col-lg-3",
          
          ## Number of simulations: ---------------------------------------
          
          mod_comp_m_ui("comp_m_in_ctsd"),
          
          ## Tracking schedule: -------------------------------------------

          shinydashboardPlus::box(
            title = span("Sampling design", class = "ttl-box_solid"),
            id = ns("sdBox_schedule"),
            status = "info",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("sdTabs_schedule"),

              tabPanel(
                value = ns("sdPanel_schedule"),
                title = icon("stopwatch", class = "cl-sea"),
                p(),
                fluidRow(
                  column(width = 12, mod_blocks_ui(ns("sdBlock_dur"))),
                  column(width = 12, mod_blocks_ui(ns("sdBlock_dti"))),
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("sdPanel_schedule_new"),
                title = icon("bolt", class = "cl-mdn"),
                p(),
                fluidRow(
                  column(width = 12, mod_blocks_ui(ns("sdBlock_dur_new"))),
                  column(width = 12, mod_blocks_ui(ns("sdBlock_dti_new"))),
                ) # end of fluidRow

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = div(
              id = "sdBox_schedule_footer",
              column(
                width = 12, align = "right",
                style = "padding-left: 0px; padding-right: 0px;",
                
                shiny::actionButton(
                  inputId = ns("sdButton_compare"),
                  icon = icon("code-compare"),
                  label = "Compare",
                  class = "btn-info",
                  width = "125px")
                
              )) # end of column, div (footer)
          ), # end of box // sdBox_schedule
          
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
                  column(width = 12, mod_blocks_ui(ns("sdBlock_n"))),
                  column(width = 12, mod_blocks_ui(ns("sdBlock_N")))
                ) # end of fluidRow
                
              ), # end of panels (1 out of 2)
              
              tabPanel(
                value = ns("sdPanel_sizes_new"),
                title = icon("bolt", class = "cl-mdn"),
                
                fluidRow(
                  column(width = 12, mod_blocks_ui(ns("sdBlock_n_new"))),
                  column(width = 12, mod_blocks_ui(ns("sdBlock_N_new")))
                ) # end of fluidRow
                
              ) # end of panels (2 out of 2)
            ) # end of tabs // sdTabs_sizes
          ) # end of box // sdBox_sizes
          
      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(class = "col-xs-12 col-sm-8 col-md-8 col-lg-9",

          ## Speed & distance plots: --------------------------------------

          shinydashboardPlus::box(
            title = span("Estimates:", class = "ttl-box"),
            id = ns("sdBox_outputs"),
            width = NULL,
            headerBorder = FALSE,
            
            p(),
            column(width = 12, align = "center",
                   div(class = "sims-irs",
                       shinyWidgets::sliderTextInput(
                         inputId = ns("sd_nsim"),
                         label = "Show simulation no.:",
                         choices = seq(1, 100, by = 1)))),
            
            tabsetPanel(
              id = ns("sdTabs_viz"),
              
              tabPanel(
                title = "Distance",
                value = ns("sdPanel_trajectory"),
                icon = icon("route"),
                
                div(id = "sd_outputs",
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
                        mod_blocks_ui(ns("distBlock_err")))
                  ),
                  
                  fluidRow(
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("distBlock_est_new"))),
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("distBlock_err_new")))
                  ),
                  
                  uiOutput(ns("sdUI_distLegend")),
                  p(style = "margin-top: 35px;"),
                  uiOutput(ns("sdBlock_group_dist"))
                  
                ) # end of div
                
              ), # end of panel (1 out of 2)
              
              tabPanel(
                title = "Speed",
                value = ns("sdPanel_speed"),
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
                        mod_blocks_ui(ns("sdBlock_err")))
                  ),
                  
                  fluidRow(
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("sdBlock_est_new"))),
                    div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12",
                        mod_blocks_ui(ns("sdBlock_err_new")))
                  ),
                  
                  uiOutput(ns("sdUI_sdLegend")),
                  p(style = "margin-top: 35px;"),
                  uiOutput(ns("sdBlock_group_speed"))
                  
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
                  label = span("Show", span("table", class = "cl-sea")),
                  icon = icon("eye"),
                  width = "100%")
                
              ) # end of splitLayout
              
            ) # end of column (footer)
          ), # end of sdBox_outputs
          
          ## Table: -------------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Summary table:", class = "ttl-box"),
            id = ns("sdBox_summary"),
            width = NULL,
            solidHeader = FALSE,
            
            reactable::reactableOutput(ns("sdTable")) #,
            # br(),
            # div(style = "display:inline-block; float:right",
            #     shiny::actionButton(
            #       inputId = ns("sdTable_clear"),
            #       label = "Clear table",
            #       icon =  icon("trash"),
            #       width = "110px")), br()
            
          ), # end of box, "sdBox_summary"
          
      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          ## Additional information: --------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("sdBox_misc"),
            width = NULL,
            solidHeader = FALSE,

            verbatimTextOutput(outputId = ns("out_time_sd")),
            verbatimTextOutput(outputId = ns("out_time_sd_new")),
            div(class = "pre-main",
                verbatimTextOutput(outputId = ns("out_time_sd_total")))

          ) # end of box
          
      ) # end of column (bottom)

    ) # end of fluidRow
  ) # end of tagList
}

#' tab_ctsd Server Functions
#'
#' @noRd
mod_tab_ctsd_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    rv$sd <- reactiveValues(ctsdList = NULL)
    
    ## Calculate ratio (effect size) if groups: ---------------------------
    
    observe({
      req(rv$active_tab == 'ctsd')
      req("compare" %in% rv$which_meta,
          rv$modList, rv$fitList,
          rv$mu, rv$tau_v,
          rv$grouped)
      req(length(rv$mu) == 3,
          length(rv$sigma) == 3)

      if (rv$grouped && rv$data_type != "simulated") {
        req(rv$modList_groups)
      }
      
      shinyjs::hide(id = "sdBox_schedule_footer")
      
    }) # end of observe
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    observe({
      if (req(rv$nsims) != length(req(rv$simfitList)))
        shinyjs::hide(id = "sdBox_viz")
    })

    observe({
      shinyjs::show(id = "sdBox_schedule")
      shinyjs::show(id = "sdBox_sizes")
      shinyjs::show(id = "sdBox_viz")
    }) %>% bindEvent(rv$simfitList)
    
    ## Hide elements at start: --------------------------------------------
    
    boxnames <- c("schedule",
                  "sizes",
                  "outputs",
                  "summary",
                  "misc")

    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("sdBox_", boxnames[i]))
    }

    tabnames <- c("schedule", "speed", "sizes", "dist", "viz")
    for (i in 1:length(tabnames)) {
      tmp_id <- paste0("sdTabs_", tabnames[i])
      tmp_target <- paste0("sdPanel_", tabnames[i], "_new")
      hideTab(inputId = tmp_id, target = ns(tmp_target))
    }
    
    observe({
      req(!is.null(rv$grouped))
      if (!rv$grouped) shinyjs::hide(id = "sdBlock_group_speed")
      else shinyjs::show(id = "sdBlock_group_speed")
      
      if (!rv$grouped) shinyjs::hide(id = "sdBlock_group_dist")
      else shinyjs::show(id = "sdBlock_group_dist")
      
    }) %>% bindEvent(rv$grouped)
    
    ## Update based on number of simulations: -----------------------------
    
    observe({
      req(rv$active_tab == 'ctsd',
          rv$simList, rv$ctsdList)
      rv$sd_nsim <- 1
      
      if (length(rv$simList) == 1) {
        shinyjs::hide(id = "sd_nsim")
        div(class = "sims-irs",
            shinyWidgets::updateSliderTextInput(
              session = session,
              inputId = "sd_nsim",
              label = "Show simulation no.:",
              choices = seq(1, length(rv$simList), by = 1),
              selected = 1))
        
      } else {
        req(length(rv$simList) == length(rv$ctsdList))
        
        shinyjs::show(id = "sd_nsim")
        div(class = "sims-irs",
            shinyWidgets::updateSliderTextInput(
              session = session,
              inputId = "sd_nsim",
              label = "Show simulation no.:",
              choices = seq(1, length(rv$simList), by = 1),
              selected = length(rv$simList)))
      }
      
    }) # end of observer
    
    observe({
      req(rv$simList,
          input$sd_nsim >= 1)
      
      int <- round(input$sd_nsim, 0)
      if (int %in% seq(1, length(rv$simList), 1))
        rv$sd_nsim <- input$sd_nsim
      
    }) %>% # end of observer,
      bindEvent(input$sd_nsim)
    
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
      
      rv$paths_selected <- c("full", "initial", "new")
      
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
      bindEvent(input$sdInput_show)
    
    ## Sample size boxes for comparing sampling designs: ------------------
    
    output$sdUI_compare_n <- renderUI({
      req(rv$simList, input$sd_dur, input$sd_dti, rv$sd_nsim)
      
      device <- movedesign::fixrates
      dti <- device$dti[match(input$sd_dti, device$dti_notes)]
      
      n_new <- length(
        seq(0, round(input$sd_dur %#% "days", 0),
            by = round(dti, 0))[-1])
      
      splitLayout(
        parBlock(header = "Initial sampling design:",
                 value = span(scales::label_comma(
                   accuracy = 1)(nrow(rv$simList[[rv$sd_nsim]])),
                   "locations", class = "cl-mdn")),
        
        parBlock(header = "Modified sampling design:",
                 value = span(scales::label_comma(
                   accuracy = 1)(n_new),
                   "locations", class = "cl-dgr"))
        
      ) # end of splitLayout
      
    }) # end of renderUI // sdUI_compare_n
    
    ## Show new conditional panels if comparing: --------------------------
    
    observe({
      req(rv$sd$simList)
      req(rv$sd$fitList)
      
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
      bindEvent(list(rv$sd$simList, rv$sd$fitList))
    
    ## Show buttons to change all panels: ---------------------------------
    
    output$sdUI_show <- renderUI({
      req(rv$sd$simList)
      
      shinyWidgets::radioGroupButtons(
        inputId = ns("sdInput_show"),
        label = NULL,
        choices = c("Show initial sampling design" = 1,
                    "Show modified sampling design" = 2),
        checkIcon = list(yes = icon("circle-check")),
        selected = 2,
        justified = TRUE)
      
    }) # end of renderUI, "sdUI_show"
    
    ## Writing new schedule text: -----------------------------------------
    
    writing_schedule_new <- reactive({
      req(rv$sd$dur, rv$sd$dti)
      
      out_dti <- fix_unit(rv$sd$dti$value, rv$sd$dti$unit)
      txt_dti <- ifelse(out_dti$value == 1,
                        paste(out_dti$unit),
                        paste(out_dti$value, out_dti$unit))
      
      dur <- rv$sd$dur$value
      dur_mth <- "months" %#% rv$sd$dur$value %#% rv$sd$dur$unit
      
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
      
    }) # end of reactive, "writing_schedule_new"
    
    output$sdText_new1 <- renderUI(writing_schedule_new())
    output$sdText_new2 <- renderUI(writing_schedule_new())
    
    ## Adding different datasets to trajectory plot: ----------------------
    
    observe({
      req(rv$sd$simList)
      
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
      req(rv$dur, rv$dti)
      
      eval_dur <- rv$dur$value %#% rv$dur$unit
      eval_dti <- rv$dti$value %#% rv$dti$unit
      
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
    
    ## Add note if CIs cannot be calculated: ------------------------------
    
    output$sdUI_distLegend <- output$sdUI_sdLegend <- renderUI({
      req(rv$speedErr,
          rv$simList)

      ci <- suppressWarnings(bayestestR::ci(
        rv$speedErr$est, ci = .95, method = "HDI"))
      lci <- ci$CI_low
      uci <- ci$CI_high

      ui <- ""
      if (length(rv$simList) > 1) {
        if (is.na(lci) || is.na(uci))
          ui <- tagList(
            p(style = "margin-top: 35px;"),
            span(class = "help-block",
                 style = "text-align: center !important;",

                 fontawesome::fa("circle-exclamation", fill = pal$dgr),
                 span("Note:", class = "help-block-note"),
                 "Credible intervals (CIs) were too large or the",
                 "number of simulations insufficient, returning ",
                 wrap_none(span("NAs", class = "cl-dgr"), "."),
                 "Run more simulations to obtain valid CIs."))
      } else {
        ui <- tagList(
          p(style = "margin-top: 22px;"),
          span(class = "help-block",
               style = "text-align: center !important;",

               fontawesome::fa("circle-exclamation", fill = pal$dgr),
               span("Note:", class = "help-block-note"),
               "This relative error is based on a single simulation.",
               "To obtain valid credible intervals, run more",
               "simulations in the",
               fontawesome::fa("stopwatch", fill = pal$sea),
               span("Sampling design", class = "cl-sea"), "tab."))
      }

      return(ui)

    }) %>% # end of renderUI, "sdUI_errLegend"
      bindEvent(rv$speedErr)
    
    # ALERTS --------------------------------------------------------------
    
    ## If no sampling design was set:
    
    observe({
      req(rv$active_tab == 'ctsd')
      
      rv$set_analysis <- NULL
      if (is.null(rv$simList)) {
        
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
        
        if (all(rv$tmp$id != rv$id)) {
          shinyalert::shinyalert(
            title = "Oops!",
            text = span(
              "Data selected is from different individual(s).",
              "Please extract parameters in the",
              icon("paw", class = "cl-mdn"),
              span("Species", class = "cl-mdn"), "tab",
              "for the appropriate individual before",
              "estimating home range."),
            html = TRUE,
            size = "xs")
          # shinyalert::shinyalert(
          #   title = "Oops!",
          #   text = span(
          #     "Data selected is from individual",
          #     HTML(paste0(span(rv$id, class = "cl-dgr"),
          #                 ",")), "but parameters are from",
          #     HTML(paste0(span(rv$tmp$id, class = "cl-dgr"), ".")),
          #     br(), "Please extract parameters in the",
          #     icon("paw", class = "cl-mdn"),
          #     span("Species", class = "cl-mdn"), "tab",
          #     "for the appropriate individual before",
          #     "estimating home range."),
          #   html = TRUE,
          #   size = "xs")
        }
        
        ## If no signature of velocity persists in data:
        
        if (is.null(rv$tau_v[[1]])) {
          
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
        
        req(all(rv$tmp$id == rv$id), !is.null(rv$tau_v[[1]]))
        rv$set_analysis <- "ctsd"
        
      } # end of if ()
      
    }) # end of observe
    
    # If comparing sampling designs before running it once:
    
    observe({
      
      if (is.null(rv$ctsdList))
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
      req(rv$active_tab == 'ctsd',
          rv$simfitList, rv$which_question)
      
      N_speed <- extract_dof(rv$simfitList, "speed")

      is_valid <- NULL
      if (all(N_speed == 0)) {

        if (length(rv$which_question) == 1) {
          alert_type <- "error"
          txt <- span(
            "Data are too coarsely sampled to",
            "estimate speed and distance. Go back to the",
            icon("stopwatch", class = "cl-mdn"),
            span("Sampling design", class = "cl-mdn"), "tab,",
            "and set a shorter",
            span("sampling interval", class = "cl-sea-d"),
            "(reducing the time between locations).")
          
          msg_log(
            style = "danger",
            message = paste(
              "Data are too coarsely sampled for",
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
          rv$sd_completed <- TRUE
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
      
      rv$sd$is_valid <- is_valid
      
    }) %>% # end of observe,
      bindEvent(rv$active_tab)
    
    # OPERATIONS ----------------------------------------------------------
    ## Comparing sampling designs: ----------------------------------------
    # Adjust sampling parameters necessary for simulation:
    
    observe({
      req(rv$tau_p[[1]], rv$tau_v[[1]],
          rv$dur, rv$dti,
          rv$ctsdList)
      
      # Species parameters:
      
      taup <- rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      taup <- round("days" %#% taup, 0)
      tauv <- rv$tau_v[[1]]$value[2] %#% rv$tau_v[[1]]$unit[2]
      
      # Sampling duration:
      
      dur <- "days" %#% rv$dur$value %#% rv$dur$unit
      dur <- round(dur, 0)
      
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
      
      if (rv$data_type != "simulated") {
        tmprange <- paste(ifelse(
          rv$tau_p[[1]]$value[1] == 0, "0",
          scales::label_comma(accuracy = .1)(rv$tau_p[[1]]$value[1])),
          "\u2014",
          scales::label_comma(accuracy = .1)(rv$tau_p[[1]]$value[3]))
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
                  accuracy = .1)(rv$tau_v[[1]]$value[2]),
                  rv$tau_v[[1]]$unit[2]),
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
            
            p(span(class = "help-block",
                   style = "text-align: center !important;",
                   
                   fontawesome::fa("circle-exclamation", fill = pal$dgr),
                   span("Note:", class = "help-block-note"),
                   "Longer sampling durations + lower sampling",
                   "intervals will add run time to simulation, model",
                   "fitting, and estimation functions. Proceed with",
                   wrap_none(span("caution", class = "cl-dgr"), ".")))
            
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
    ## Estimating for initial sampling design: ----------------------------
    
    timing_speed <- reactive({
      
      loading_modal("Calculating run time")
      msg_log(
        style = "warning",
        message = paste0("Estimating ", msg_warning("run time"), "..."))
      
      out_time <- guess_time(type = "speed",
                             fit = rv$simfitList, 
                             dti = rv$dti,
                             dur = rv$dur,
                             seed = rv$seedList,
                             trace = TRUE,
                             parallel = rv$parallel)
      
      shinybusy::remove_modal_spinner()
      return(out_time)
      
    }) %>% # end of reactivee, timing_speed()
      bindCache(rv$datList,
                rv$simList,
                rv$dur,
                rv$dti)
    
    estimating_speeds <- reactive({
      
      out_speeds <- list()
      for (i in seq_along(rv$simList)) {
        out_speeds[[i]] <- tryCatch({
          ctmm::speeds(
            rv$simList[[i]], 
            rv$simfitList[[i]], 
            units = FALSE)
        }, error = function(e) {
          warning(paste("ctmm::speeds() failed",
                        "for simulation no.", i))
          return(NULL)
        })
      }
      
      out_speeds[sapply(out_speeds, is.null)] <- NULL
      return(out_speeds)
      
    }) %>% # end of reactive, estimating_speeds()
      bindCache(rv$dur,
                rv$dti,
                rv$simList,
                rv$seedList)
    
    estimating_speed <- reactive({
      
      start_sd <- Sys.time()
      if (length(rv$simList) == 1) {
        
        ctsd <- par.speed(rv$simList[1],
                          rv$simfitList[1],
                          seed = rv$seedList[1],
                          parallel = rv$parallel)
        
        rv$ctsdList <- ctsd
        names(rv$ctsdList) <- rv$seedList[1]
        
      } else {
        
        n_sims <- length(rv$simList) - length(rv$ctsdList)
        if (length(rv$ctsdList) == 0) {
          seq_for_est <- 1:length(rv$simList)
        } else {
          seq_for_est <- (length(rv$ctsdList) + 1):length(rv$simList)
        }
        
        msg_log(
          style = "warning",
          message = paste0(
            "Speed estimation for ", n_sims,
            " simulation(s) (out of ", max(seq_for_est), ") ",
            msg_warning("in progress"), ","),
          detail = "This may take a while...")
        
        if (rv$parallel) {
          
          ctsd <- par.speed(
            rv$simList[seq_for_est],
            rv$simfitList[seq_for_est],
            seed = rv$seedList[seq_for_est],
            parallel = rv$parallel)
          
          rv$ctsdList <<- c(rv$ctsdList, ctsd)
          length(rv$ctsdList)
          names(rv$ctsdList) <- do.call(c, rv$seedList)
          
          msg_log(
            style = 'success',
            message = paste0("Speed estimation ",
                             msg_success("completed"), "."),
            run_time = difftime(Sys.time(), start_sd, units = "secs"))
          
        } else {
          
          ctsd <- list()
          for (i in seq_for_est) {
              
            shinyFeedback::showToast(
              type = "info",
              message = paste0("Estimation ", i, " out of ",
                               length(seq_for_est), "..."),
              .options = list(
                progressBar = FALSE,
                closeButton = TRUE,
                preventDuplicates = TRUE,
                positionClass = "toast-bottom-right"))
            
            msg_log(
              style = "warning",
              message = paste0("Estimation for sim no. ", i, " ",
                               msg_warning("in progress"), ","),
              detail = "This may take a while...")
            
            ctsd[[i]] <- par.speed(
              rv$simList[i],
              rv$simfitList[i],
              seed = rv$seedList[i],
              parallel = rv$parallel)
            rv$ctsdList[[length(rv$ctsdList) + 1]] <- ctsd[[i]]
            
            msg_log(
              style = 'warning',
              message = paste0("Estimation for sim no. ", i, " ",
                               msg_success("completed"), "..."),
              run_time = difftime(Sys.time(), start_sd, units = "secs"))
            
            shinyFeedback::showToast(
              type = "success",
              message = paste0("Estimation ", i, 
                               " out of ", length(rv$simList),
                               " completed."),
              .options = list(
                progressBar = FALSE,
                closeButton = TRUE,
                preventDuplicates = TRUE,
                positionClass = "toast-bottom-right"))
            
          } # end of for loop
        } # end of if [rv$parallel]
      }
      
      names(rv$ctsdList) <- names(rv$simList)
      if ("DOF" %in% names(ctsd)) ctsd <- list(ctsd)
      
      time_sd <- difftime(Sys.time(), start_sd, units = "sec")
      rv$time[["ctsd"]][[1]] <- rv$time[["ctsd"]][[1]] + time_sd[[1]]
      
      return(ctsd)
      
    }) %>% # end of reactive, estimating_speed()
      bindCache(rv$simList,
                rv$seedList,
                rv$dur,
                rv$dti)
    
    # First, estimate run time:
    
    observe({
      req(rv$active_tab == 'ctsd',
          rv$simList,
          rv$simfitList,
          rv$which_meta,
          rv$tmp$id == rv$id,
          rv$sd$is_valid)
      req(length(rv$simList) == length(rv$simfitList))
      
      expt <- timing_speed()
      rv$sd$expt_time <- expt
      max_time <- as.numeric(expt$max) %#% expt$unit
      num_cores <- parallel::detectCores(logical = FALSE)
      
      if (rv$parallel) 
        ttl_time <- max_time / 
        ifelse(length(rv$simList) < num_cores,
               length(rv$simList), num_cores)
      else ttl_time <- max_time
      
      if (length(rv$ctsdList) == 0) {
        N <- mean(unlist(extract_dof(rv$simfitList, "speed")),
                  na.rm = TRUE)
      } else {
        N <- mean(sapply(rv$ctsdList, function(y) y$DOF[["speed"]]),
                  na.rm = TRUE)
      }
      
      if (is.na(N)) N <- NULL
      req(N)
      
      rv$sd$proceed_to_ctsd <- NULL
      if (ttl_time > 15 %#% "minutes" || N < 15) {
        
        ttl_time <- fix_unit(expt$unit %#% ttl_time,
                             expt$unit, convert = TRUE)
        min_time <- fix_unit(ttl_time$unit %#% expt$min %#% expt$unit,
                             ttl_time$unit)
        
        # dti <- rv$dti$value %#% rv$dti$unit
        # tau_v <- rv$tau_v[[1]][2, "value"] %#% rv$tau_v[[1]][2, "unit"]
        # if (N > 5 && (dti/tau_v > 1)) ...
        
        if (N > 15) out_txt <- tagList(span(
          "Expected run time for estimation",
          "could be on average", span(
            paste0(min_time$value, "\u2013",
                   ttl_time$value, " ", ttl_time$unit),
            class = "cl-dgr"),
          "but may take \u003E", 
          wrap_none(
            ttl_time$value, " ", ttl_time$unit,
            css = "cl-dgr", end = ".")))
        else out_txt <- tagList(span(
          "Expected run time for estimation",
          "cannot be obtained due to",
          wrap_none("low effective sample sizes", 
                    color = pal$dgr, end = ".")))
        
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            rv$sd$proceed_to_ctsd <- x
          },
          text = out_txt,
          type = "warning",
          showCancelButton = TRUE,
          cancelButtonText = "Stop",
          confirmButtonCol = pal$mdn,
          confirmButtonText = "Proceed",
          html = TRUE)
        
      } else { rv$sd$proceed_to_ctsd <- TRUE }
      
    }) %>% # end of observe,
      bindEvent(list(input$run_sd,
                     rv$m$proceed))
    
    observe({
      req(rv$sd$proceed_to_ctsd,
          length(rv$simList) == length(rv$simfitList))
      req(rv$sd_completed == FALSE)
      
      boxes <- list("sdBox_speed",
                    "sdBox_dist",
                    "sdBox_outputs")
      
      for (b in seq_along(boxes)) shinyjs::show(id = boxes[b])
      
      start <- Sys.time()
      
      num_sims <- length(rv$simList) - length(rv$ctsdList)
      if (length(rv$ctsdList) == 0) {
        seq_for <- 1:length(rv$simList)
      } else {
        seq_for <- (length(rv$ctsdList) + 1):length(rv$simList)
      }
      
      msg_log(
        style = "warning",
        message = paste0("Estimating ",
                         msg_warning("speed & distance"), "..."))
      
      if (length(rv$ctsdList) == 0) {
        N <- mean(unlist(extract_dof(rv$simfitList, "area")),
                  na.rm = TRUE)
      } else {
        N <- mean(sapply(rv$ctsdList, function(y) y$DOF[["speed"]]),
                  na.rm = TRUE)
      }
      
      if (is.na(N)) N <- NULL
      req(N)
      
      if (N < 15)
        loading_modal("Estimating speed & distance")
      else
        loading_modal("Estimating speed & distance", 
                      exp_time = rv$sd$expt_time,
                      n = num_sims)
      
      # msg_log(
      #   style = "warning",
      #   message = paste0("Simulating for ",
      #                    msg_warning("current trajectory"), ","),
      #   detail = "This may take a while...")
      
      sdList <- estimating_speed()
      dataList <- estimating_speeds()
      
      truthList <- get_true_speed(
        data = rv$simList,
        seed = rv$seedList,
        
        tau_p = rv$tau_p,
        tau_v = rv$tau_v,
        sigma = rv$sigma,
        
        emulated = rv$is_emulate,
        fit = if (rv$is_emulate) rv$meanfitList else NULL,
        
        grouped = rv$grouped,
        groups = if (rv$grouped) rv$groups[[2]] else NULL)
      rv$truth$ctsd <- truthList
      
      # If speed() returns NULL (1 simulation only)
      if (is.null(sdList) && length(rv$simList) == 1) {
        msg_log(
          style = "danger",
          message = paste0(
            "Speed estimation", msg_danger("failed"), "."))
        shinybusy::remove_modal_spinner()
        req(sdList)
      }
      
      out_est_df <- data.frame(seed = numeric(0),
                               lci = numeric(0), 
                               est = numeric(0),
                               uci = numeric(0),
                               unit = character(0))
      out_err_df <- data.frame(seed = numeric(0),
                               lci = numeric(0),
                               est = numeric(0),
                               uci = numeric(0))
      
      for (i in seq_along(sdList)) {
        sim_no <- seq_for[i]
        
        # If speed() returns NULL (multiple simulation)
        if (is.null(sdList[[i]])) {
          out_est_df <- out_est_df %>%
            dplyr::add_row(seed = rv$seedList[[sim_no]],
                           lci = NA, est = NA, uci = NA, unit = NA)
          out_err_df <- out_err_df %>%
            dplyr::add_row(seed = rv$seedList[[sim_no]],
                           lci = NA, est = NA, uci = NA)
          
          rv$pathList <<- c(rv$pathList, list(NULL))
          next
        }
        
        # [quickfix for older ctmm versions]
        if ("CI" %in% names(sdList[[i]]))
          sdList[[i]] <- sdList[[i]]$CI
        
        # If speed() returns Inf
        to_check <- sdList[[i]][1, "est"]
        
        if (is.infinite(to_check)) {
          out_est_df <- out_est_df %>%
            dplyr::add_row(seed = rv$seedList[[sim_no]],
                           lci = NA, est = NA, uci = NA, unit = NA)
          out_err_df <- out_err_df %>%
            dplyr::add_row(seed = rv$seedList[[sim_no]],
                           lci = NA, est = NA, uci = NA)
          
          msg_log(
            style = "danger",
            message = paste(
              "Data are too coarsely sampled for",
              msg_danger("speed"), "estimation."))
          
          if (length(sdList) == 1) 
            shinyalert::shinyalert(
              type = "error",
              title = "Warning",
              text = tagList(p(
                style = "text-align: justify !important;",
                "Data are too coarsely sampled to",
                "estimate speed and distance. Go back to the",
                icon("stopwatch", class = "cl-sea-d"),
                span("Sampling design", class = "cl-sea-d"),
                "tab, and set a shorter",
                span("sampling interval", class = "cl-dgr"),
                "(less time between recorded locations).")),
              html = TRUE, size = "s")
          
          rv$pathList <<- c(rv$pathList, list(NULL))
          next
        }
        
        tmpname <- rownames(sdList[[i]])
        tmpunit <- extract_units(tmpname[grep("speed", tmpname)])
        
        group <- 1
        if (rv$grouped) {
          nm <- names(rv$simList)[[sim_no]]
          group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
        }
        
        seed <- as.character(rv$seedList[[sim_no]])
        sd_truth <- rv$truth$ctsd[[seed]]
        
        out_est_df <- out_est_df %>%
          dplyr::add_row(seed = rv$seedList[[sim_no]],
                         lci = sdList[[i]][1], 
                         est = sdList[[i]][2],
                         uci = sdList[[i]][3],
                         unit = tmpunit)
        
        out_err_df <- out_err_df %>%
          dplyr::add_row(
            seed = rv$seedList[[sim_no]],
            lci = ((sdList[[i]][[1]] %#% tmpunit) - sd_truth) / sd_truth,
            est = ((sdList[[i]][[2]] %#% tmpunit) - sd_truth) / sd_truth,
            uci = ((sdList[[i]][[3]] %#% tmpunit) - sd_truth) / sd_truth)
      }
      
      rv$speedDatList <- dataList
      rv$speedEst <<- rbind(rv$speedEst, out_est_df)
      rv$speedErr <<- rbind(rv$speedErr, out_err_df)
      rv$is_analyses <- TRUE
      rv$is_report <- FALSE
      rv$is_meta <- FALSE
      rv$is_ctsd <- TRUE
      
      ### Calculating total and mean distance: ----------------------------
      
      out_dist_est_df <- data.frame(seed = numeric(0),
                                    lci = numeric(0),
                                    est = numeric(0),
                                    uci = numeric(0),
                                    unit = character(0))
      out_dist_err_df <- data.frame(seed = numeric(0),
                                    lci = numeric(0),
                                    est = numeric(0),
                                    uci = numeric(0))
      
      dur_days <- "days" %#% rv$dur$value %#% rv$dur$unit
      unit_new <- "kilometers/day"
      
      for (i in seq_along(sdList)) {
        sim_no <- seq_for[i]
        
        pathList <- estimate_trajectory(
          data = rv$simList[sim_no],
          fit = rv$simfitList[sim_no],
          groups = if (rv$grouped) rv$groups[[2]] else NULL,
          dur = rv$dur,
          tau_v = rv$tau_v,
          seed = rv$seedList[sim_no])
        
        rv$pathList <<- c(rv$pathList, pathList)
        
        if (is.null(sdList[[i]]) ||
            is.null(rv$pathList[[sim_no]])) {
          out_dist_est_df <- out_dist_est_df %>%
            dplyr::add_row(seed = rv$seedList[[sim_no]],
                           lci = NA, est = NA, uci = NA, unit = NA)
          out_dist_err_df <- out_dist_err_df %>%
            dplyr::add_row(seed = rv$seedList[[sim_no]],
                           lci = NA, est = NA, uci = NA)
          next
        }
        
        truth <- sum(rv$pathList[[sim_no]]$dist, na.rm = TRUE)
        unit_old <- rv$speedEst$unit[sim_no]
        
       if (!is.na(rv$speedEst$est[sim_no])) {
         
         dist_lci <- (unit_new %#% rv$speedEst$lci[sim_no]
                      %#% unit_old) * dur_days
         dist_est <- (unit_new %#% rv$speedEst$est[sim_no]
                      %#% unit_old) * dur_days
         dist_uci <- (unit_new %#% rv$speedEst$uci[sim_no]
                      %#% unit_old) * dur_days
         
         dist_unit <- "kilometers"
         truth <- dist_unit %#% truth
         
         out_dist_est_df <- out_dist_est_df %>%
           dplyr::add_row(seed = rv$seedList[[sim_no]],
                          lci = dist_lci, 
                          est = dist_est, 
                          uci = dist_uci, 
                          unit = dist_unit)
         
         out_dist_err_df <- out_dist_err_df %>%
           dplyr::add_row(seed = rv$seedList[[sim_no]],
                          lci = (dist_lci - truth) / truth,
                          est = (dist_est - truth) / truth,
                          uci = (dist_uci - truth) / truth)
       } else {
         out_dist_est_df <- out_dist_est_df %>%
           dplyr::add_row(seed = rv$seedList[[sim_no]],
                          lci = NA, est = NA, uci = NA, unit = NA)
         out_dist_err_df <- out_dist_err_df %>%
           dplyr::add_row(seed = rv$seedList[[sim_no]],
                          lci = NA, est = NA, uci = NA)
       }
      }
      
      msg_log(
        style = 'success',
        message = paste0("Path estimation ",
                         msg_success("completed"), "."),
        run_time = difftime(Sys.time(), start, units = "sec"))
      
      rv$distEst <<- rbind(rv$distEst, out_dist_est_df)
      rv$distErr <<- rbind(rv$distErr, out_dist_err_df)
      
      # Save outputs to table:
      for (i in seq_along(sdList)) {
        sim_no <- seq_for[i]
        
        group <- 1
        if (rv$grouped) {
          group <- ifelse(
            names(rv$simList)[[sim_no]] %in% rv$groups[[2]]$A,
            "A", "B")
        }
        
        if (rv$is_emulate) {
          tau_p <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           rv$seedList[[sim_no]]),
            "position")[[1]]
          tau_v <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           rv$seedList[[sim_no]]),
            "velocity")[[1]]
          sigma <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           rv$seedList[[sim_no]]),
            "sigma")[[1]]
        } else {
          tau_p <- rv$tau_p[[group]]
          tau_v <- rv$tau_v[[group]]
          sigma <- rv$sigma[[group]]
        }
        
        rv$sd$tbl <<- rbind(
          rv$sd$tbl, 
          .build_tbl(
            target = "ctsd",
            group = if (rv$grouped) group else NA,
            data = rv$simList[[sim_no]],
            seed = names(rv$simList)[[sim_no]],
            obj = rv$ctsdList[[sim_no]],
            tau_p = tau_p,
            tau_v = tau_v,
            sigma = sigma,
            speed = rv$speedEst[sim_no, ],
            speed_error = rv$speedErr[sim_no, ],
            distance = rv$distEst[sim_no, ],
            distance_error = rv$distErr[sim_no, ]))
      }
      
      shinyjs::show(id = "sdBlock_est")
      shinyjs::show(id = "sdBlock_err")
      
      time_sd <- difftime(Sys.time(), start, units = "sec")
      rv$time[["ctsd"]][[1]] <- rv$time[["ctsd"]][[1]] + time_sd[[1]]
      
      msg_log(
        style = "success",
        message = paste0(
          "Estimation ", msg_success("completed"), "."),
        run_time = time_sd)
      
      shinyFeedback::showToast(
        type = "success",
        message = "Speed estimation completed!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"))
      
      rv$sd_completed <- TRUE
      rv$sd$proceed_to_ctsd <- FALSE
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      bindEvent(rv$sd$proceed_to_ctsd)
    
    ## Estimating for new sampling design: --------------------------------
    
    timing_speed_new <- reactive({
      
      loading_modal("Calculating run time")
      msg_log(
        style = "warning",
        message = paste0("Estimating ", msg_warning("run time"), "..."))
      
      out_time <- guess_time(data = rv$sd$simList,
                             fit = rv$sd$fitList,
                             seed = rv$seedList,
                             type = "speed",
                             trace = TRUE,
                             parallel = rv$parallel)
                             
      shinybusy::remove_modal_spinner()
      return(out_time)
      
    }) %>% # end of reactive, timing_speed_new()
      bindCache(rv$sd$simList,
                rv$sd$dur,
                rv$sd$dti)
    
    comparing_data <- reactive({
      
      set_id <- 1
      if (!is.null(rv$sd_nsim)) set_id <- rv$sd_nsim
      
      dat <- rv$simList[[set_id]]
      seed <- rv$seedList[[set_id]]
      
      dur <- rv$sd$dur$value %#% rv$sd$dur$unit
      dti <- rv$sd$dti$value %#% rv$sd$dti$unit
      t_new <- seq(0, round(dur, 0), by = round(dti, 0))[-1]
      
      if (rv$data_type == "simulated") {
        fit <- fitA <- rv$modList[[1]]
        if (rv$grouped) fitB <- rv$modList[[2]]
      }
      
      if (rv$is_emulate) {
        req(rv$meanfitList)
        fit <- emulate_seeded(rv$meanfitList[["All"]], seed)
        if (length(fit$isotropic) > 1)
          fit$isotropic <- fit$isotropic[["sigma"]]
        
        # Recenter to 0,0:
        fit$mu[["x"]] <- 0
        fit$mu[["y"]] <- 0
        
      } else {
        fit <- prepare_mod(
          tau_p = rv$tau_p[[1]][2, ],
          tau_v = rv$tau_v[[1]][2, ],
          sigma = rv$sigma[[1]][2, ],
          mu = rv$mu[[1]])
        
      }
      
      if ("compare" %in% rv$which_meta) {
        req(rv$groups)
        
        get_group <- function(seed, groups) {
          if (as.character(seed) %in% groups[["A"]]) { 
            return("A") } else { return("B") }
        }
        
        group <- get_group(seed, rv$groups[[2]])
        rv$sd$groups <- stats::setNames(list(seed), group)
        
        if (rv$is_emulate) {
          fit <- emulate_seeded(rv$meanfitList[[group]], seed)
          if (length(fit$isotropic) > 1)
            fit$isotropic <- fit$isotropic[["sigma"]]
          
          # Recenter to 0,0:
          fit$mu[["x"]] <- 0
          fit$mu[["y"]] <- 0
          
        } else {
          fit <- prepare_mod(
            tau_p = rv$tau_p[[group]][2, ],
            tau_v = rv$tau_v[[group]][2, ],
            sigma = rv$sigma[[group]][2, ],
            mu = rv$mu[[group]])
        }
      }
      
      # Fill in the gaps of original dataset + new duration:
      sim <- ctmm::simulate(dat, fit, t = t_new, seed = seed)
      sim <- pseudonymize(sim)
      sim$index <- 1:nrow(sim)
      return(list(sim))
      
    }) %>% # end of reactive, comparing_data()
      bindCache(rv$sd$dur,
                rv$sd$dti,
                rv$simList)
    
    fitting_new_model <- reactive({
      
      newsimList <- rv$sd$simList
      guessList <- tryCatch(
        lapply(seq_along(newsimList), function (x)
          ctmm::ctmm.guess(newsimList[[x]],
                           interactive = FALSE)),
        error = function(e) e)
      
      if (inherits(guessList, "error")) {
        msg_log(
          style = "danger",
          message = paste0(
            "Parameter guesstimation ", msg_danger("failed"), "."))
        return(NULL)
      }
      
      out_fit <- tryCatch(
        par.ctmm.fit(newsimList, guessList, parallel = rv$parallel),
        error = function(e) e)
      
      return(list(out_fit))
      
    }) %>% # end of reactive, fitting_new_model()
      bindCache(rv$sd$dur,
                rv$sd$dti,
                rv$sd$simList)
    
    estimating_speed_new <- reactive({
      
      ctsd_new <- list()
      ctsd_new[[1]] <- par.speed(rv$simList[1],
                                 rv$simfitList[1],
                                 seed = rv$seedList[1],
                                 parallel = rv$parallel)
      return(ctsd_new)
      
    }) %>% # end of reactive, estimating_speed_new()
      bindCache(rv$sd$dur,
                rv$sd$dti,
                rv$sd$simList)
    
    observe({
      req(rv$simList, rv$simfitList)
      
      set_id <- 1
      if (!is.null(rv$sd_nsim)) set_id <- rv$sd_nsim
      
      # Capture new sampling duration and interval:
      
      device <- movedesign::fixrates
      dti_unit <- sub('^.* ([[:alnum:]]+)$', '\\1', input$sd_dti)
      dti <- dti_unit %#% round(
        device$dti[match(input$sd_dti, device$dti_notes)], 0)
      
      rv$sd$dti <- data.frame(
        value = dti,
        unit = dti_unit)
      
      rv$sd$dur <- data.frame(
        value = input$sd_dur,
        unit = "days")
      
      # Check if storage limitations:
      
      proceed <- TRUE
      if (!is.null(rv$storage)) {
        
        n_new <- length(
          seq(0, round(input$sd_dur %#% "days", 0),
              by = round(dti %#% dti_unit, 0))[-1])
        
        if (n_new >= rv$storage) {
          
          shinyalert::shinyalert(
            type = "warning",
            title = "Warning",
            text = tagList(span(
              "Many GPS units can only store a maximum of",
              wrap_none(
                span("32,000\u201464,000 locations", class = "cl-dgr"),
                "."), "You set a limit of", rv$storage,
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
                             msg_danger(rv$storage), " locations."),
            detail = "Please set a different sampling design.")
          
          proceed <- FALSE
          
        } # end of length(t0) >= rv$storage)
      } # end of !is.null(rv$storage)
      
      req(proceed)
      rv$conditional <- TRUE
      removeModal()
      
      ### 1. Simulate new dataset:
      
      loading_modal("Simulating new design")
      msg_log(
        style = "warning",
        message = paste0("Simulating ",
                         msg_warning("new sampling design"), "."),
        detail = "This may take a while...")
      
      start <- Sys.time()
      dat <- comparing_data()
      rv$sd$simList <- dat
      time_sd <- difftime(Sys.time(), start, units = "sec")
      
      msg_log(
        style = "success",
        message = paste0("Simulation ", msg_success("completed"), "."),
        run_time = time_sd)
      
      ### 2. Fit models to simulation:
      
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("in progress"), "."),
        detail = "Please wait for model selection to finish:")
      
      start_fit <- Sys.time()
      rv$sd$fitList <- fitting_new_model()
      time_sd <- difftime(Sys.time(), start_fit, units = "sec")
      
      msg_log(
        style = "success",
        message = paste0("Model fit ", msg_success("completed"), "."),
        run_time = time_sd)
      
      shinybusy::remove_modal_spinner()
      
      ### 3. Estimate runnning time:
      
      expt <- timing_speed_new()
      rv$sd$expt2_range <- expt$range

      rv$sd$proceed_to_ctsd_new <- NULL
      rv$sd$proceed_to_dist_new <- NULL
      if ((as.numeric(expt$max) %#% expt$unit) > 900) {
        
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            rv$sd$proceed_to_ctsd_new <- x
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
      } else { rv$sd$proceed_to_ctsd_new <- TRUE }
      
    }, priority = 1) %>% # end of observe,
      bindEvent(input$run_sd_new)
    
    observe({
      req(rv$sd$simList,
          rv$sd$fitList)
      
      set_id <- 1
      if (!is.null(rv$sd_nsim)) set_id <- rv$sd_nsim
      
      ### 4. Run the speed/distance estimator (CTSD):
      
      fit <- rv$sd$fitList[[1]]
      tmpnms <- names(summary(fit)$DOF)
      N <- summary(fit)$DOF[grep("speed", tmpnms)][[1]]
      rv$N2_new <- N
      
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
          p(rv$sd$expt2_range,
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
          message = paste0(
            "Effective sample size ", msg_danger("too low"), ","),
          detail = paste("Please select a different sampling design."))
        
      } else {
        
        start_est <- Sys.time()
        msg_log(
          style = "warning",
          message = paste0(
            "Estimating speed for ",
            msg_warning("new trajectory.")),
          detail = paste0("This should take around ",
                          rv$sd$expt2_range, "..."))
        
        ctsd_new <- estimating_speed_new()
        ctsd_data_new <- ctmm::speeds(rv$sd$simList[[1]],
                                      rv$sd$fitList[[1]],
                                      units = FALSE)
        
        if ("CI" %in% names(ctsd_new[[1]])) 
          ctsd_new[[1]] <- ctsd_new[[1]]$CI
        
        tmpname <- rownames(ctsd_new[[1]])
        tmpunit <- extract_units(tmpname[grep("speed", tmpname)])
        
        truthList <- get_true_speed(
          data = rv$sd$simList[[1]],
          seed = rv$seedList[[set_id]],
          
          tau_p = rv$tau_p,
          tau_v = rv$tau_v,
          sigma = rv$sigma,
          
          emulated = rv$is_emulate,
          fit = if (rv$is_emulate) rv$meanfitList else NULL,
          
          grouped = rv$grouped,
          groups = if (rv$grouped) rv$sd$groups else NULL)
        truth <- truthList[[1]]
        
        out_est <- data.frame(
          seed = rv$seedList[[1]],
          lci = ctsd_new[[1]][1],
          est = ctsd_new[[1]][2],
          uci = ctsd_new[[1]][3],
          unit = tmpunit)
        
        out_err <- data.frame(
          seed = rv$seedList[[1]],
          lci = ((out_est[[2]] %#% tmpunit) - truth) / truth,
          est = ((out_est[[3]] %#% tmpunit) - truth) / truth,
          uci = ((out_est[[4]] %#% tmpunit) - truth) / truth)
        
        rv$sd$ctsdList <- ctsd_new
        rv$sd$speedDatList <- ctsd_data_new
        rv$speedEst_new <- out_est
        rv$speedErr_new <- out_err
        
        time_sd <- difftime(Sys.time(), start_est, units = "sec")
        rv$time[["ctsd"]][[2]] <- rv$time[["ctsd"]][[2]] + time_sd[[1]]
        
        ### Calculating total and mean distance: --------------------------
        
        dur_days <- "days" %#% rv$dur$value %#% rv$dur$unit
        
        oldunit <- rv$speedEst_new$unit[[1]]
        newunit <- "kilometers/day"
        
        sd_lci <- rv$speedEst_new[1, "lci"]
        sd_est <- rv$speedEst_new[1, "est"]
        sd_uci <- rv$speedEst_new[1, "uci"]
        
        dist_lci <- (newunit %#% sd_lci %#% oldunit) * dur_days
        dist_est <- (newunit %#% sd_est %#% oldunit) * dur_days
        dist_uci <- (newunit %#% sd_uci %#% oldunit) * dur_days
        
        dist_unit <- "kilometers"
        truth <- dist_unit %#%
          sum(rv$pathList[[1]]$dist, na.rm = TRUE)
        
        out_est <- c("lci" = dist_lci,
                     "est" = dist_est,
                     "uci" = dist_uci)
        
        out_err <- c(
          "lci" = ((out_est[[1]]) - truth) / truth,
          "est" = ((out_est[[2]]) - truth) / truth,
          "uci" = ((out_est[[3]]) - truth) / truth)
        
        rv$distEst_new <- data.frame(
          seed = rv$seedList[[1]],
          lci = out_est[[1]], 
          est = out_est[[2]], 
          uci = out_est[[3]], 
          unit = dist_unit)
        
        rv$distErr_new <- data.frame(
          seed = rv$seedList[[1]],
          lci = out_err[[1]], 
          est = out_err[[2]], 
          uci = out_err[[3]])
        
        msg_log(
          style = "success",
          message =  paste0(
            "Estimation ", msg_success("completed"), "."),
          run_time = time_sd)
      }
      
      rv$sd$proceed_to_dist_new <- TRUE
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      bindEvent(rv$sd$proceed_to_ctsd_new)
    
    # PLOTS ---------------------------------------------------------------
    ## Plotting trajectory: -----------------------------------------------
    
    output$sdPlot_path <- ggiraph::renderGirafe({
      req(rv$ctsdList, rv$pathList,
          input$show_paths, 
          rv$sd_nsim)
      req(length(rv$ctsdList) == length(rv$simList))
      req(length(rv$pathList) == length(rv$simList))
      
      newdat <- rv$simList[[rv$sd_nsim]]
      alldat <- rv$pathList[[rv$sd_nsim]]
      
      if (is.na(rv$distEst[rv$sd_nsim, ]$est)) {
        p <- ggplot2::ggplot() +
          ggtext::geom_richtext(
            mapping = ggplot2::aes(x = 1, y = 1),
            size = 5,
            color = "grey50",
            fill = NA, label.color = NA,
            label = paste0("[Estimation for individual ",
                           rv$sd_nsim,
                           " <span style='color: #DB4545;'>",
                           "failed</span>]")) +
          ggplot2::theme_void()
        
      } else {
        
        datasets <- input$show_paths
        lims <- extract_limits(newdat, alldat)
        
        if (!is.null(rv$sd$simList)) {
          lims <- extract_limits(newdat, alldat, 
                                 rv$sd$simList[[1]])
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
            size = 0.8, col = pal$sea)
        }
        
        if ("new" %in% datasets) {
          req(rv$sd$simList)
          
          p3 <- ggplot2::geom_path(
            rv$sd$simList[[1]], mapping = ggplot2::aes(
              x = x, y = y),
            linewidth = 0.2, col = pal$dgr)
          p3_points <- ggiraph::geom_point_interactive(
            rv$sd$simList[[1]], mapping = ggplot2::aes(
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
          
          theme_movedesign(font_available = rv$is_font) +
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
      }
     
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
      dat <- rv$speedDatList[[rv$sd_nsim]]
      ctsd <- rv$speedEst[rv$sd_nsim, ]
      req(!is.na(ctsd$est))
      
      t_origin <- "1111-11-10 23:06:32"
      dat <- dat %>%
        dplyr::mutate(timestamp = as.POSIXct(t, origin = t_origin),
               t_new = if (unit != "weeks") {
                 as.POSIXct(round.POSIXt(timestamp, units = unit), 
                            format = "%Y-%m-%d %H:%M:%OS")
               } else { format(timestamp, "%U") })
      
      dat <- dat %>%
        dplyr::group_by(.data$t_new) %>%
        dplyr::summarise(est = mean(.data$est),
                         low = mean(.data$low),
                         high = mean(.data$high))
      
      unit <- ctsd$unit
      yline <- ctsd$est %#% unit
      
      group <- 1
      if (rv$grouped) group <- ifelse(
        names(rv$simList)[[rv$sd_nsim]] %in% rv$groups[[2]]$A, "A", "B")
      
      yline_truth <- rv$truth$ctsd[[group]]
      yline_new <- NULL
      
      if (!is.null(rv$speedEst_new)) {
        yline_new <- rv$speedEst_new$est[1] %#% 
          rv$speedEst$unit[1]
      }
      
      return(list(
        data = dat,
        yline = yline,
        yline_truth = yline_truth,
        yline_new = yline_new))
      
    }) # end of reactive, preparing_speed()
    
    output$sdPlot_speed <- ggiraph::renderGirafe({
      req(rv$speedEst,
          rv$truth,
          rv$sd_nsim,
          input$show_speeds,
          input$sd_unit)
      req(nrow(rv$speedEst) == length(rv$simList))
      
      datasets <- input$show_speeds
      
      # if (rv$sd_nsim > 1) {
      #   sims_ci <- colMeans(rv$speedEst[,2:4]) %>% as.vector()
      #   sims_ci <- data.frame(
      #     low = sims_ci[[1]] %#% rv$speedEst$unit[1],
      #     high = sims_ci[[1]] %#% rv$speedEst$unit[3])
      # }
      
      if (is.na(rv$speedEst[rv$sd_nsim, ]$est)) {
        p <- ggplot2::ggplot() +
          ggtext::geom_richtext(
            mapping = ggplot2::aes(x = 1, y = 1),
            size = 5,
            color = "grey50",
            fill = NA, label.color = NA,
            label = paste0("[Estimation for individual ",
                           rv$sd_nsim,
                           " <span style='color: #DB4545;'>",
                           "failed</span>]")) +
          ggplot2::theme_void()
      } else {
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
          
          # { if (rv$sd_nsim > 1)
          #   ggplot2::geom_ribbon(
          #     data = sims_ci,
          #     mapping = ggplot2::aes(ymin = low, ymax = high), 
          #     fill = pal$sea_d, alpha = .5)
          # } +
          
          { if ("full" %in% datasets)
            ggplot2::geom_hline(
              yintercept = preparing_speed()[["yline_truth"]], 
              linewidth = 1, linetype = "solid") } +
          
          { if ("initial" %in% datasets)
            ggplot2::geom_hline(
              yintercept = preparing_speed()[["yline"]], 
              linewidth = 1.1, linetype = "dashed",
              col = pal$sea) } +
          
          { if (!is.null(rv$sd$ctsdList) && "new" %in% datasets)
            ggplot2::geom_hline(
              yintercept = preparing_speed()[["yline_new"]],
              linewidth = 1.1, linetype = "dashed",
              col = pal$dgr)  } +
          
          ggplot2::labs(
            x = "Time lag",
            y = "Speed estimate (meters/second)") +
          theme_movedesign(font_available = rv$is_font)
      }
      
      ggiraph::girafe(ggobj = p)
      
    }) # end of renderGirafe, "sdPlot_speed"
    
    # TABLES --------------------------------------------------------------
    ## Initial sampling design: -------------------------------------------
    
    # observe({
    #   req(rv$simList,
    #       rv$pathList,
    #       rv$ctsdList,
    #       rv$distErr,
    #       rv$sd$tbl)
    # 
    #   shinyjs::show(id = "sdBox_summary")
    #   rv$sd$tbl <- dplyr::distinct(rv$sd$tbl)
    #   rv$report_sd_yn <- TRUE
    # 
    # }) %>% # end of observe
    #   bindEvent(input$add_sd_table)
    
    observe({
      shinyjs::toggle(id = "sdBox_summary")
    }) %>% # end of observe,
      bindEvent(input$add_sd_table)

    ## New sampling design: -----------------------------------------------

    observe({
      req(rv$sd$fitList, rv$speedEst_new, rv$distErr_new)
      
      set_id <- 1
      if (!is.null(rv$sd_nsim)) set_id <- isolate(rv$sd_nsim)
      
      group <- "All"
      if (rv$grouped) {
        get_group <- function(seed, groups) {
          if (as.character(seed) %in% groups[["A"]]) { 
            return("A") } else { return("B") }
        }
        group <- get_group(rv$seedList[[set_id]], rv$groups[[2]])
      }
      
      if (rv$is_emulate) {
        fit <- emulate_seeded(
          rv$meanfitList[[group]], rv$seedList[[set_id]])
        tau_p <- extract_pars(fit, "position")[[1]]
        tau_v <- extract_pars(fit, "velocity")[[1]]
        sigma <- extract_pars(fit, "sigma")[[1]]
        
      } else {
        tau_p <- rv$tau_p[[group]]
        tau_v <- rv$tau_v[[group]]
        sigma <- rv$sigma[[group]]
      }
      
      rv$sd$tbl <<- rbind(
        rv$sd$tbl, 
        .build_tbl(
          data_type = "Modified",
          target = "ctsd",
          group = group,
          data = rv$sd$simList[[1]],
          seed = names(rv$simList)[[set_id]],
          obj = rv$sd$ctsdList[[1]],
          tau_p = tau_p,
          tau_v = tau_v,
          sigma = sigma,
          speed = rv$speedEst_new[1, ],
          speed_error = rv$speedErr_new[1, ],
          distance = rv$distEst_new[1, ],
          distance_error = rv$distErr_new[1, ]))
      
      rv$sd$tbl <- dplyr::distinct(rv$sd$tbl)
      
    }) %>% # end of observe
      bindEvent(input$add_sd_table)

    ## Rendering output table: --------------------------------------------

    output$sdTable <- reactable::renderReactable({
      req(rv$sd$tbl, rv$ctsdList)
      
      dt_sd <- dplyr::select(rv$sd$tbl, -seed)
      
      if (!rv$grouped) {
        dt_sd <- dplyr::select(
          dt_sd, -c(device, group, taup, sigma, N1, area:area_err_max))
      } else {
        dt_sd <- dplyr::select(
          dt_sd, -c(device, taup, sigma, N1, area:area_err_max))
      }
      
      nms <- list(
        data = "Data",
        group = "Group",
        tauv = "\u03C4\u1D65",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N2 = "N (speed)",
        ctsd = "Speed",
        ctsd_err = "Error",
        ctsd_err_min = "95% LCI",
        ctsd_err_max = "95% UCI",
        dist = "Distance",
        dist_err = "Error")
      
      nms_sizes <- reactable::colGroup(
        name = "Sample sizes", 
        columns = c("n", "N2"))
      nms_ctsd <- reactable::colGroup(
        name = "Speed",
        columns = c("ctsd_err",
                    "ctsd_err_min",
                    "ctsd_err_max"))
      nms_dist <- reactable::colGroup(
        name = "& Distance",
        columns = c("dist", "dist_err"))
      
      colgroups <- list(nms_sizes,
                        nms_ctsd,
                        nms_dist)
      
      if (length(unique(dt_sd$data)) == 1) {
        dt_sd <- dplyr::select(dt_sd, -data)
        nms <- nms[-1]
        colgroups <- colgroups[-1]
      }
      
      if (rv$grouped) dt_sd <- dplyr::rename(dt_sd,  Group = group)
      
      reactable::reactable(
        data = dt_sd,
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
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 0)),
          N2 = reactable::colDef(
            minWidth = 80, name = nms[["N2"]],
            style = format_num,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 1)),
          ctsd = reactable::colDef(
            minWidth = 120, name = nms[["ctsd"]]),
          ctsd_err = reactable::colDef(
            minWidth = 80, name = nms[["ctsd_err"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)),
          ctsd_err_min = reactable::colDef(
            minWidth = 80, name = nms[["ctsd_err_min"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)),
          ctsd_err_max = reactable::colDef(
            minWidth = 80, name = nms[["ctsd_err_max"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)),
          dist = reactable::colDef(
            minWidth = 80, name = nms[["dist"]]),
          dist_err = reactable::colDef(
            minWidth = 80, name = nms[["dist_err"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1))
        ),
        columnGroups = colgroups)

    }) %>% # end of renderReactable, "sdTable"
      bindEvent(list(input$add_sd_table, rv$ctsdList))
    
    # BLOCKS --------------------------------------------------------------
    ## Tracking device: ---------------------------------------------------
    ### Initial sampling design: ------------------------------------------

    observe({
      req(rv$active_tab == 'ctsd')
      req(rv$simList)
      
      mod_blocks_server(
        id = "sdBlock_dur",
        rv = rv, data = rv$simList,
        type = "dur")

      mod_blocks_server(
        id = "sdBlock_dti",
        rv = rv, data = rv$simList,
        type = "dti")
      
    }) # end of observe

    ### New sampling design: ----------------------------------------------

    observe({
      req(rv$sd$simList)

      mod_blocks_server(
        id = "sdBlock_dur_new",
        rv = rv, data = rv$sd$simList,
        type = "dur",
        class = "cl-mdn")

      mod_blocks_server(
        id = "sdBlock_dti_new",
        rv = rv, data = rv$sd$simList,
        type = "dti",
        class = "cl-mdn")

    }) # end of observe

    ## Sample sizes: ------------------------------------------------------
    
    observe({
      req(rv$active_tab == 'ctsd', rv$simList)
      
      mod_blocks_server(
        id = "sdBlock_n", 
        rv = rv, data = rv$simList,
        type = "n",
        options = list(rightBorder = FALSE,
                       marginBottom = TRUE))
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'ctsd', rv$simList, rv$simfitList, rv$ctsdList)
      req(length(rv$ctsdList) >= 1)
      
      mod_blocks_server(
        id = "sdBlock_N",
        rv = rv, data = rv$simList, obj = rv$ctsdList,
        type = "N", name = "speed")

    }) # end of observe
    
    observe({
      req(rv$active_tab == 'ctsd', rv$hr$simList)
      
      mod_blocks_server(
        id = "sdBlock_n_new", 
        rv = rv, data = rv$sd$simList,
        type = "n",
        options = list(rightBorder = FALSE,
                       marginBottom = FALSE))
      
    }) # end of observe
    
    observe({
      req(rv$sd$simList, rv$sd$fitList)

      mod_blocks_server(
        id = "sdBlock_N_new",
        rv = rv, data = rv$sd$simList, obj = rv$sd$fitList,
        type = "N", name = "speed", class = "cl-mdn")

    }) # end of observe
    
    ## Groups: ------------------------------------------------------------
    
    observe({
      req(rv$which_meta == "compare", rv$grouped)
      req(rv$simList, rv$akdeList, rv$sd_nsim)
      
      shinyjs::show(id = "sdBlock_group_speed")
      
      set_id <- rv$sd_nsim
      set_group <- names(rv$groups[[2]])[
        sapply(rv$groups[[2]], function(x)
          names(rv$simList)[[set_id]] %in% x)]
      
      output$sdBlock_group_speed <- renderUI({
        
        parBlock(
          icon = "object-ungroup",
          header = "Group",
          value = set_group)
        
      }) # end of renderUI, "sdBlock_group_speed"
      
    }) %>% # end of observe,
      bindEvent(rv$sd_nsim)
    
    observe({
      req(rv$which_meta == "compare", rv$grouped)
      req(rv$simList, rv$akdeList, rv$sd_nsim)
      
      shinyjs::show(id = "sdBlock_group_dist")
      
      set_id <- rv$sd_nsim
      set_group <- names(rv$groups[[2]])[
        sapply(rv$groups[[2]], function(x)
          names(rv$simList)[[set_id]] %in% x)]
      
      output$sdBlock_group_dist <- renderUI({
        
        parBlock(
          icon = "object-ungroup",
          header = "Group",
          value = set_group)
        
      }) # end of renderUI, "sdBlock_group_dist"
      
    }) %>% # end of observe,
      bindEvent(list(rv$sd_nsim, rv$active_tab == 'ctsd'))
    
    ## Outputs: -----------------------------------------------------------
    ### Speed & distance estimates: ---------------------------------------

    observe({
      req(rv$ctsdList, rv$speedEst, rv$speedErr)
      req(nrow(rv$speedEst) == length(rv$simList),
          nrow(rv$speedErr) == length(rv$simList))
      
      mod_blocks_server(
        id = "sdBlock_est",
        rv = rv, type = "ctsd", name = "speedEst")
      mod_blocks_server(
        id = "sdBlock_err",
        rv = rv, type = "ctsd", name = "speedErr")
      
    }) # end of observe
    
    observe({
      req(rv$sd$ctsdList, rv$speedEst_new, rv$speedErr_new)

      mod_blocks_server(
        id = "sdBlock_est_new",
        rv = rv, type = "ctsd", name = "speedEst_new",
        class = "cl-mdn")
      
      mod_blocks_server(
        id = "sdBlock_err_new",
        rv = rv, type = "ctsd", name = "speedErr_new",
        class = "cl-mdn")

    }) # end of observe

    ### Movement metrics: -------------------------------------------------
    
    observe({
      req(rv$ctsdList, rv$speedEst, rv$distEst)
      
      mod_blocks_server(
        id = "distBlock_est",
        rv = rv, type = "dist", name = "distEst")
      mod_blocks_server(
        id = "distBlock_err",
        rv = rv, type = "dist", name = "distErr")
      
    }) # end of observe

    observe({
      req(rv$sd$ctsdList, rv$speedEst_new, rv$distEst_new)
      req(any(!is.na(rv$speedEst_new$est)))
      
      mod_blocks_server(
        id = "distBlock_est_new",
        rv = rv, type = "dist", name = "distEst_new")

      mod_blocks_server(
        id = "distBlock_err_new",
        rv = rv, type = "dist", name = "distErr_new")

    }) # end of observe
    
    # HELP TOUR & MODALS --------------------------------------------------
    ## Help modal (biases): -----------------------------------------------
    
    observe({
      
      shiny::showModal(
        shiny::modalDialog(
          title = h4(span("Speed & distance", class = "cl-sea"),
                     "estimation:"),
          
          fluidRow(
            style = paste("margin-right: 20px;",
                          "margin-left: 20px;"),
            
            p("As straight-line displacements (SLDs) have known",
              "limitations, here we use the",
             span("Continuous-time Speed and Distance (CTSD)",
                   class = "cl-sea"), "estimation method.",
             "CTSD provides accurate, scale-insensitive estimates",
             "of both speed and distance with reliable confidence",
             "intervals."),
            
            h4(style = "margin-top: 30px;", "For more information:"),
            
            p(style = "font-family: var(--monosans);",
              "Noonan, M. J., Fleming, C. H., Akre, T. S.,",
              "Drescher-Lehman, J., Gurarie, E., Harrison, A. L.,",
              "Kays, R. & Calabrese, J. M. (2019). Scale-insensitive",
              "estimation of speed and distance traveled from animal",
              "tracking data. Movement Ecology, 7(1), 1-15.")
            
          ), # end of fluidRow
          
          footer = modalButton("Dismiss"),
          size = "m")) # end of modal
      
    }) %>% # end of observe,
      bindEvent(input$sdHelp_method)
    
    # MISC ----------------------------------------------------------------

    observe({
      shinyjs::show(id = "sdBox_misc")  
    }) %>% bindEvent(rv$time[["sd"]] > 0)
    
    output$out_time_sd <- renderText({
      req(rv$time[["sd"]][[1]] > 0)
      
      out <- fix_unit(rv$time[["ctsd"]][[1]], "seconds", convert = TRUE)
      paste0("Initial sampling design took approximately ",
             out$value, " ", out$unit, ".")

    }) # end of renderText, "time_sd"

    output$out_time_sd_new <- renderText({
      req(rv$time[["sd"]][[2]] > 0)

      out <- fix_unit(rv$time[["ctsd"]][[2]], "seconds", convert = TRUE)
      paste0("New sampling design took approximately ",
             out$value, " ", out$unit, ".")

    }) # end of renderText, "time_sd_new"

    output$out_time_sd_new <- renderText({
      req(rv$time[["sd"]][[2]] > 0, rv$sd$ctsdList)

      total_time <- rv$time[["sd"]][[1]] + rv$time[["sd"]][[2]]

      out <- fix_unit(total_time, "seconds", convert = TRUE)
      paste0("... In total, this section took approximately",
             out$value, " ", out$unit, ".")

    }) # end of renderText, "time_sd_total"
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_ctsd_ui("tab_ctsd_1")

## To be copied in the server
# mod_tab_ctsd_server("tab_ctsd_1")
