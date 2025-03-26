#' tab_hrange UI Function
#'
#' @description Estimating home range area.
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
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
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
      
      div(class = "col-xs-12 col-sm-4 col-md-4 col-lg-3",
          
          ## Number of simulations: ---------------------------------------
          
          mod_comp_m_ui("comp_m_in_hr"),
          
          ## Sampling design: ---------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Sampling design", class = "ttl-box_solid"),
            id = ns("hrBox_schedule"),
            status = "info",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,
            
            tabsetPanel(
              id = ns("hrTabs_schedule"),
              
              tabPanel(
                value = ns("hrPanel_schedule"),
                title = icon("stopwatch", class = "cl-sea"),
                p(),
                
                fluidRow(
                  column(width = 12, mod_blocks_ui(ns("hrBlock_dur"))),
                  column(width = 12, mod_blocks_ui(ns("hrBlock_dti"))),
                ) # end of fluidRow
                
              ), # end of panels (1 out of 2)
              
              tabPanel(
                value = ns("hrPanel_schedule_new"),
                title = icon("bolt", class = "cl-mdn"),
                p(),
                
                fluidRow(
                  column(width = 12, mod_blocks_ui(ns("hrBlock_dur_new"))),
                  column(width = 12, mod_blocks_ui(ns("hrBlock_dti_new"))),
                ) # end of fluidRow
                
              ) # end of panels (2 out of 2)
            ), # end of tabs
            
            footer = div(
              id = "hrBox_schedule_footer",
              column(
                width = 12, align = "right",
                style = "padding-left: 0px; padding-right: 0px;",
                
                shiny::actionButton(
                  inputId = ns("hrButton_compare"),
                  icon = icon("code-compare"),
                  label = "Compare",
                  class = "btn-info",
                  width = "125px")
                
              )) # end of column, div (footer)
          ), # end of box // hrBox_schedule
          
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
                  column(width = 12, mod_blocks_ui(ns("hrBlock_n"))),
                  column(width = 12, mod_blocks_ui(ns("hrBlock_N")))
                ) # end of fluidRow
                
              ), # end of panels (1 out of 2)
              
              tabPanel(
                value = ns("hrPanel_sizes_new"),
                title = icon("bolt", class = "cl-mdn"),
                
                fluidRow(
                  column(width = 12, mod_blocks_ui(ns("hrBlock_n_new"))),
                  column(width = 12, mod_blocks_ui(ns("hrBlock_N_new")))
                ) # end of fluidRow
                
              ) # end of panels (2 out of 2)
            ) # end of tabs // hrTabs_sizes
          ) # end of box // hrBox_sizes
          
      ), # end of div (right column)
      
      # [center column] ---------------------------------------------------
      
      div(class = "col-xs-12 col-sm-8 col-md-8 col-lg-9",
          
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
                  span("Design", class = "ttl-panel")
                ),
                
                div(id = "hr_outputs",
                    class = "col-xs-12 col-sm-12 col-md-12 col-lg-9",
                    p(),
                    
                    div(class = "sims-irs",
                        shinyWidgets::sliderTextInput(
                          inputId = ns("hr_nsim"),
                          label = "Show simulation no.:",
                          choices = seq(1, 100, by = 1))),
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
                        value = TRUE),
                      
                      shinyWidgets::awesomeCheckbox(
                        inputId = ns("hr_locations"),
                        label = span("Show", span("locations", 
                                                  class = "cl-sea")),
                        value = FALSE)), p(),
                    
                    ggiraph::girafeOutput(
                      outputId = ns("hrPlot"),
                      width = "100%", height = "100%")
                    
                ), # end of div()
                
                div(id = "content_hr-areas",
                    class = "col-xs-12 col-sm-12 col-md-12 col-lg-3",
                    p(class = "fluid-padding"),
                    
                    mod_blocks_ui(ns("hrBlock_est")),
                    p(style = "margin-top: 35px;"),
                    mod_blocks_ui(ns("hrBlock_err")),
                    uiOutput(ns("hrUI_errLegend")),
                    
                    p(style = "margin-top: 35px;"),
                    uiOutput(ns("hrBlock_group"))
                    
                ) # end of div()
                
              ), # end of panels (1 out of 2)
              
              tabPanel(
                value = ns("hrPanel_viz_new"),
                title = tagList(
                  icon("bolt", class = "cl-mdn"),
                  span("Modified design") %>%
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
                        value = TRUE),
                      
                      shinyWidgets::awesomeCheckbox(
                        inputId = ns("hr_locations_new"),
                        label = span("Show", span("locations", 
                                                  class = "cl-sea")),
                        value = FALSE)), p(),
                    
                    ggiraph::girafeOutput(
                      outputId = ns("hrPlot_new"),
                      width = "100%", height = "100%"),
                    
                    column(
                      width = 12, align = "center",
                      shinyWidgets::awesomeCheckbox(
                        inputId = ns("hr_datasets"),
                        label = span("Add the initial",
                                     icon("stopwatch", class = "cl-sea"),
                                     span("Design", class = "cl-sea"),
                                     "outputs to the plot"),
                        value = TRUE))
                    
                ), # end of div()
                
                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-3",
                    p(class = "fluid-padding"),
                    uiOutput(ns("hrText_new")),
                    
                    mod_blocks_ui(ns("hrBlock_est_new")),
                    p(style = "margin-top: 35px;"),
                    mod_blocks_ui(ns("hrBlock_err_new"))
                    
                ) # end of div()
                
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
                  inputId = ns("add_hr_table"),
                  label = span("Show", span("table", class = "cl-sea")),
                  icon = icon("eye"),
                  width = "100%")
                
              )) # end of footer
          ), # end of box // hrBox_viz
          
          ## Table: -------------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Summary table:", class = "ttl-box"),
            id = ns("hrBox_summary"),
            width = NULL,
            solidHeader = FALSE,
            
            reactable::reactableOutput(ns("hrTable"))
            
          ), # end of box, "hrBox_summary"
          
      ), # end of column (center)
      
      # [bottom column] ---------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          ## Additional information: --------------------------------------
          
          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("hrBox_misc"),
            width = NULL,
            solidHeader = FALSE,

            verbatimTextOutput(outputId = ns("out_time_hr")),
            verbatimTextOutput(outputId = ns("out_time_hr_new")),
            div(class = "pre-main",
                verbatimTextOutput(outputId = ns("out_time_hr_total")))

          ) # end of box
          
      ) # end of column (bottom)
      
    ) # end of fluidRow
  ) # end of tagList
}

#' tab_hrange Server Functions
#'
#' @noRd
mod_tab_hrange_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    rv$hr <- reactiveValues(akdeList = list())
    
    ## Calculate ratio (effect size) if groups: ---------------------------
    
    observe({
      req(rv$active_tab == 'hr')
      req("compare" %in% rv$which_meta,
          rv$mu, rv$sigma,
          rv$grouped)
      req(length(rv$mu) == 3,
          length(rv$sigma) == 3)
      
      shinyjs::hide(id = "hrBox_schedule_footer")
      
    }) # end of observe
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    # observe({
    #   if (req(rv$nsims) != length(req(rv$simfitList)))
    #     shinyjs::hide(id = "hrBox_viz")
    # })
    
    observe({
      shinyjs::show(id = "hrBox_schedule")
      shinyjs::show(id = "hrBox_sizes")
      shinyjs::show(id = "hrBox_viz")
    }) %>% bindEvent(rv$simfitList)
    
    ## Hide elements at start: --------------------------------------------
    
    boxnames <- c("schedule",
                  "sizes",
                  "viz",
                  "areas",
                  "summary",
                  "misc")
    
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("hrBox_", boxnames[i]))
    }
    
    tabnames <- c("schedule", "area", "sizes", "viz")
    for (i in 1:length(tabnames)) {
      tmp_id <- paste0("hrTabs_", tabnames[i])
      tmp_target <- paste0("hrPanel_", tabnames[i], "_new")
      hideTab(inputId = tmp_id, target = ns(tmp_target))
    }
    
    shinyjs::hide(id = "hr_nsim")
    
    observe({
      req(!is.null(rv$grouped))
      if (!rv$grouped) shinyjs::hide(id = "hrBlock_group")
      else shinyjs::show(id = "hrBlock_group")
      
    }) %>% bindEvent(rv$grouped)
    
    ## Update based on number of simulations: -----------------------------
    
    observe({
      req(rv$active_tab == 'hr',
          rv$simList, rv$akdeList)
      rv$hr_nsim <- 1
      
      if (length(rv$simList) == 1) {
        shinyjs::hide(id = "hr_nsim")
        div(class = "sims-irs",
            shinyWidgets::updateSliderTextInput(
              session = session,
              inputId = "hr_nsim",
              label = "Show simulation no.:",
              choices = seq(1, length(rv$simList), by = 1),
              selected = 1))
        
      } else {
        req(length(rv$simList) == length(rv$akdeList))
        
        shinyjs::show(id = "hr_nsim")
        div(class = "sims-irs",
            shinyWidgets::updateSliderTextInput(
              session = session,
              inputId = "hr_nsim",
              label = "Show simulation no.:",
              choices = seq(1, length(rv$simList), by = 1),
              selected = length(rv$simList)))
      }
      
    }) # end of observer
    
    observe({
      req(rv$simList, rv$is_analyses,
          input$hr_nsim >= 1,
          rv$active_tab == 'hr')
      
      int <- round(input$hr_nsim, 0)
      if (int %in% seq(1, length(rv$simList), 1))
        rv$hr_nsim <- input$hr_nsim
      
    }) %>% # end of observer,
      bindEvent(input$hr_nsim)
    
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
    
    # Change tabs if more simulations are run:
    observe({
      req(rv$hr_nsim > 1)
      tabs <- paste0("hrTabs_", tabnames)
      panels <- paste0("hrPanel_", tabnames)
      
      for (i in 1:length(tabnames)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[i]),
          selected = paste0("tab_hrange_1-", panels[i])) }
      
    }) %>% # end of observe.
      bindEvent(rv$hr_nsim)
    
    ## Sample size boxes for comparing sampling designs: ------------------
    
    output$hrUI_compare_n <- renderUI({
      req(rv$simList, input$hr_dur, input$hr_dti, rv$sd_nsim)
      
      device <- movedesign::fixrates
      dti <- device$dti[match(input$hr_dti, device$dti_notes)]
      
      n_new <- length(
        seq(0, round(input$hr_dur %#% "days", 0),
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
      
    }) # end of renderUI // hrUI_compare_n
    
    ## Show new conditional panels if comparing: --------------------------
    
    observe({
      req(rv$hr$simList)
      req(rv$hr$fitList)
      
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
      bindEvent(list(rv$hr$simList, rv$hr$fitList))
    
    ## Show buttons to change all panels: ---------------------------------
    
    output$hrUI_show <- renderUI({
      req(rv$hr$simList)
      
      shinyWidgets::radioGroupButtons(
        inputId = ns("hrInput_show"),
        label = NULL,
        choices = c("Show initial sampling design" = 1,
                    "Show modified sampling design" = 2),
        checkIcon = list(yes = icon("circle-check")),
        selected = 2,
        justified = TRUE)
      
    }) # end of renderUI, "hrUI_show"
    
    ## Writing new schedule text: -----------------------------------------
    
    writing_schedule_new <- reactive({
      req(rv$hr$dur, rv$hr$dti)
      
      out_dti <- fix_unit(rv$hr$dti$value, rv$hr$dti$unit)
      txt_dti <- ifelse(out_dti$value == 1,
                        paste(out_dti$unit),
                        paste(out_dti$value, out_dti$unit))
      
      dur <- rv$hr$dur$value
      dur_mth <- "months" %#% rv$hr$dur$value %#% rv$hr$dur$unit
      
      
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
    
    output$hrText_new <- renderUI(writing_schedule_new())
    
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
    
    ## Add note if CIs cannot be calculated: ------------------------------
    
    output$hrUI_errLegend <- renderUI({
      req(rv$hrErr,
          rv$simList,
          rv$which_meta)
      
      ci <- suppressWarnings(bayestestR::ci(
        rv$hrErr$est, ci = .95, method = "HDI"))
      lci <- ci$CI_low
      uci <- ci$CI_high
      
      if (rv$which_meta != "none") extra_ui <- tagList(
        "To obtain valid credible intervals, run more simulations",
        "through the", span("Simulations", class = "cl-grn"), "box.")
      else extra_ui <- tagList(
        "To obtain valid credible intervals, select a different",
        span("analytical target", class = "cl-sea"), "in the",
        icon("house", class = "cl-mdn"),
        span("Home", class = "cl-mdn"), "tab.")
      
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
                 "Run more", span("simulations", class = "cl-grn"), 
                 "to obtain valid CIs."))
      } else {
        ui <- tagList(
          p(style = "margin-top: 22px;"),
          span(class = "help-block",
               style = "text-align: center !important;",
               
               fontawesome::fa("circle-exclamation", fill = pal$dgr),
               span("Note:", class = "help-block-note"), 
               "This relative error is based on a single simulation,",
               "and the error range is calculated from the 95%",
               "confidence intervals [low\u2014high CI].",
               extra_ui))
      }
      
      return(ui)
      
    }) %>% # end of renderUI, "hrUI_errLegend"
      bindEvent(rv$hrErr)
    
    # ALERTS --------------------------------------------------------------
    
    ## If no sampling design was set:
    
    observe({
      req(rv$active_tab == 'hr')
      
      rv$set_analysis <- NULL
      if (is.null(rv$simList)) {
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
      } else {
        rv$set_analysis <- "hr"
      }
      
    }) # end of observe
    
    # If comparing sampling designs before running it once:
    
    observe({
      if (is.null(rv$akdeList))
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
        
        req(rv$tmp$id, rv$id)
        if (all(rv$tmp$id != rv$id))
          shinyalert::shinyalert(
            title = "Oops!",
            text = span(
              "Data selected is from individual",
              HTML(paste0(span(rv$id, class = "cl-dgr"),
                          ",")), "but parameters are from",
              HTML(paste0(span(rv$tmp$id, class = "cl-dgr"), ".")),
              br(), "Please extract parameters in the",
              icon("paw", class = "cl-mdn"),
              span("Species", class = "cl-mdn"), "tab",
              "for the appropriate individual before",
              "estimating home range."),
            html = TRUE,
            size = "xs")
        
      } # end of if ()
      
    }) %>% # end of observe,
      bindEvent(input$run_hr)
    
    # OPERATIONS ----------------------------------------------------------
    ## Comparing sampling designs: ----------------------------------------
    # Adjust sampling parameters necessary for simulation:
    
    observe({
      req(rv$tau_p, rv$tau_v,
          rv$dur, rv$dti,
          rv$akdeList)
      
      # Species parameters:
      
      taup <- "days" %#% rv$tau_p[[1]]$value[2] %#% rv$tau_p[[1]]$unit[2]
      
      # Sampling duration:
      
      dur <- "days" %#% rv$dur$value %#% rv$dur$unit
      
      opts_dur <- c(1:4, 
                    round_any(c(10, dur, taup, 
                                taup * c(10, 25, 50, 75, 100)),
                              5, f = round),
                    365, 730, 1096, 1461, 1826) %>%
        unique() %>% sort()
      opts_dur <- opts_dur[opts_dur != 0]
      selected_dur <- opts_dur[which.min(abs(opts_dur - taup * 10))]
      
      # Sampling interval:
      
      device <- movedesign::fixrates %>% 
        dplyr::arrange(dplyr::desc(.data$frq))
      dti <- rv$dti$value %#% rv$dti$unit
      index <- which.min(abs(device$dti - dti))
      
      opts_dti <- device$dti_notes
      
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
            
            mod_blocks_server(
              id = "hrBlock_taup", 
              rv = rv, type = "tau", name = "tau_p0",
              input_name = list(
                chr = "data_taup0",
                html = wrap_none("Position autocorrelation ",
                                 "(\u03C4", tags$sub("p"), ")"))),
            
            # parBlock(
            #   header = span(
            #     wrap_none("Position autocorrelation ",
            #               "(\u03C4", tags$sub("p"), ")")),
            #   value =
            #     paste(scales::label_comma(
            #       accuracy = .1)(rv$tau_p[[1]]$value[2]),
            #       rv$tau_p[[1]]$unit[2]),
            #   subtitle = tmprange),
            
            p("If home range estimation is your goal,",
              "we recommend that the",
              span("sampling duration", class = "cl-sea"),
              "is at least 10 times the",
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
              selected = selected_dur,
              from_min = selected_dur,
              from_max = max(opts_dur)
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
              inputId = ns("run_hr_new"),
              label = "Run simulation",
              icon = icon("bolt"),
              class = "btn-danger")
          ),
          
          size = "m")) # end of modal
      
    }) %>% # end of observe,
      bindEvent(input$hrButton_compare)
    
    # ESTIMATIONS ---------------------------------------------------------
    ## Estimating for initial sampling design: ----------------------------
    
    estimating_hr <- reactive({
      
      start_hr <- Sys.time()
      if ((length(rv$simList) == 1) ||
          (rv$grouped && length(rv$simList) == 2)) {
        
        hr <- tryCatch(
          ctmm::akde(rv$simList[[1]], rv$simfitList[[1]]),
          warning = function(w) NULL,
          error = function(e) NULL)
        hr <- list(hr)
        
        if (rv$grouped) {
          hr2 <- tryCatch(
            ctmm::akde(rv$simList[[2]], rv$simfitList[[2]]),
            warning = function(w) NULL,
            error = function(e) NULL)
          hr <- list(hr[[1]], hr2)
        }
        
        rv$akdeList <- hr
        
      } else {
        
        n_sims <- length(rv$simList) - length(rv$akdeList)
        if (length(rv$akdeList) == 0) {
          seq_for_est <- 1:length(rv$simList)
        } else { 
          seq_for_est <- (length(rv$akdeList) + 1):length(rv$simList)
        }
        
        if (rv$parallel && n_sims >= 4) {
          
          hr <- par.akde(
            rv$simList[seq_for_est],
            rv$simfitList[seq_for_est],
            parallel = rv$parallel)
          
          rv$akdeList <<- c(rv$akdeList, hr)
          
        } else {
          
          j <- 0
          hr <- list()
          for (i in seq_for_est) {
            j <- j + 1
            
            # shinyFeedback::showToast(
            #   type = "info",
            #   message = paste0("Estimation ", i, " out of ",
            #                    length(seq_for_est), "..."),
            #   .options = list(
            #     progressBar = FALSE,
            #     closeButton = TRUE,
            #     preventDuplicates = TRUE,
            #     positionClass = "toast-bottom-right"))
            
            msg_log(
              style = "warning",
              message = paste0("Estimation for sim no. ", i, " ",
                               msg_warning("in progress"), ","),
              detail = "This may take a while...")
            
            hr[[j]] <- tryCatch(
              ctmm::akde(rv$simList[[i]], rv$simfitList[[i]]),
              warning = function(w) { w; return(NULL) },
              error = function(e) return(NULL))
            rv$akdeList[[length(rv$akdeList) + 1]] <- hr[[j]]
            
            msg_log(
              style = 'warning',
              message = paste0("Estimation for sim no. ", i, " ",
                               msg_success("completed"), "..."),
              run_time = difftime(Sys.time(), start_hr, units = "secs"))
            
            # shinyFeedback::showToast(
            #   type = "success",
            #   message = paste0("Estimation ", i, 
            #                    " out of ", length(rv$simList),
            #                    " completed."),
            #   .options = list(
            #     progressBar = FALSE,
            #     closeButton = TRUE,
            #     preventDuplicates = TRUE,
            #     positionClass = "toast-bottom-right"))
            
          } # end of for loop
        } # end of if [rv$parallel]
      }
      
      names(rv$akdeList) <- names(rv$simList)
      return(hr)
      
    }) %>% # end of reactive, estimating_hr()
      bindCache(rv$simList,
                rv$seedList,
                rv$dur,
                rv$dti)
    
    observe({
      req("Home range" %in% rv$which_question,
          rv$is_analyses == FALSE)
      req(rv$simList,
          rv$simfitList,
          rv$tmp$id == rv$id,
          length(rv$simList) == length(rv$simfitList))
      
      boxes <- list("hrBox_areas",
                    "hrBox_viz",
                    "hrBox_misc")
      
      for (b in 1:length(boxes)) shinyjs::show(id = boxes[b])
      
      start <- Sys.time()
      
      num_sims <- length(rv$simList) - length(rv$akdeList)
      if (length(rv$akdeList) == 0) {
        seq_for <- 1:length(rv$simList)
      } else {
        seq_for <- (length(rv$akdeList) + 1):length(rv$simList)
      }
      
      msg_log(
        style = "warning",
        message = paste0("Estimating ",
                         msg_warning("home range"), "..."))
      
      loading_modal("Estimating home range")
      
      hrList <- estimating_hr()
      
      truthList <- get_true_hr(
        data = rv$simList,
        seed = rv$seedList,
        sigma = rv$sigma,
        
        emulated = rv$is_emulate,
        fit = if (rv$is_emulate) rv$meanfitList else NULL,
        
        grouped = rv$grouped,
        groups = if (rv$grouped) rv$groups[[2]] else NULL)
      rv$truth$hr <- truthList
      req(hrList)
      
      # If tiny DOF[area]:
      N1 <- extract_dof(rv$simfitList, "area")
      if (all(N1 < 0.001)) {
        proceed <- NULL
        shinybusy::remove_modal_spinner()
        
        shinyalert::shinyalert(
          type = "error",
          title = "Warning",
          text = tagList(p(
            style = "text-align: justify !important;",
            "Current model fit has an",
            span("effective sample size", class = "cl-dgr"),
            "for area below the error threshold (0.0001).",
            "Home range area cannot be estimated.")),
          html = TRUE, size = "s")
        
        msg_log(
          style = "danger",
          message = paste0(
            "Model fit has a very low ",
            msg_danger("effective sample size"), ","),
          detail = paste("Choose a different sampling design",
                         "or seed to proceed."))
        req(proceed)
      }
      
      # If home range estimation failed:
      if (is.null(hrList)) {
        proceed <- NULL
        shinybusy::remove_modal_spinner()

        shinyalert::shinyalert(
          type = "error",
          title = "Error",
          text = tagList(p(
            style = "text-align: justify !important;",
            "Home range estimation failed.")),
          html = TRUE, size = "s")

        msg_log(
          style = "danger",
          message = paste(
            "Home range estimation ",
            msg_danger("failed"), ","),
          detail = paste("Report bug."))
        req(proceed)
      }
      
      req(length(rv$simList) == length(rv$akdeList))
      
      out_est_df <- data.frame(seed = numeric(0),
                               lci = numeric(0), 
                               est = numeric(0),
                               uci = numeric(0),
                               unit = character(0))
      out_err_df <- data.frame(seed = numeric(0),
                               lci = numeric(0),
                               est = numeric(0),
                               uci = numeric(0))
      
      for (i in seq_along(hrList)) {
        sim_no <- seq_for[i]
        
        group <- 1
        if (rv$grouped) {
          nm <- names(rv$simList)[[sim_no]]
          group <- ifelse(nm %in% rv$groups[[2]]$A, "A", "B")
        }
        
        if (rv$is_emulate) {
          tau_p <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]], rv$seedList[[sim_no]]),
            "position")[[1]]
          tau_v <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]], rv$seedList[[sim_no]]),
            "velocity")[[1]]
          sigma <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]], rv$seedList[[sim_no]]),
            "sigma")[[1]]
        } else {
          tau_p <- rv$tau_p[[group]]
          tau_v <- rv$tau_v[[group]]
          sigma <- rv$sigma[[group]]
        }
        
        seed <- as.character(rv$seedList[[sim_no]])
        hr_truth <- rv$truth$hr[[seed]]$area
        N1 <- extract_dof(rv$simfitList[[sim_no]], "area")[[1]]
        
        tmpsum <- tryCatch(
          summary(rv$akdeList[[i]]),
          error = function(e) e)
        
        if (is.null(rv$akdeList[[sim_no]]) || 
            is.null(tmpsum) || length(tmpsum) == 0 ||
            any(tmpsum[[1]] == 0) ||
            inherits(tmpsum, "error") || N1 < 0.001) {
          
          out_est_df <- out_est_df %>%
            dplyr::add_row(
              seed = rv$seedList[[sim_no]],
              lci = NA, est = NA, uci = NA, unit = NA)
          out_err_df <- out_err_df %>%
            dplyr::add_row(
              seed = rv$seedList[[sim_no]],
              lci = NA, est = NA, uci = NA)
          
          rv$hr$tbl <<- rbind(
            rv$hr$tbl, 
            .build_tbl(
              target = "hr",
              group = if (rv$grouped) group else NA,
              data = rv$simList[[sim_no]], 
              seed = names(rv$simList)[[sim_no]],
              obj = rv$akdeList[[sim_no]],
              tau_p = tau_p,
              tau_v = tau_v,
              sigma = sigma,
              area = out_est_df[i, ],
              area_error = out_err_df[i, ]))
          next
        }
        
        tmpname <- rownames(summary(rv$akdeList[[sim_no]])$CI)
        tmpunit <- extract_units(tmpname[grep('^area', tmpname)])
        
        out_est_df <- out_est_df %>%
          dplyr::add_row(
            seed = rv$seedList[[sim_no]],
            lci = tmpsum$CI[1], 
            est = tmpsum$CI[2], 
            uci = tmpsum$CI[3], 
            unit = tmpunit)
        out_err_df <- out_err_df %>%
          dplyr::add_row(
            seed = rv$seedList[[sim_no]],
            lci = ((tmpsum$CI[1] %#% tmpunit) - hr_truth) / hr_truth, 
            est = ((tmpsum$CI[2] %#% tmpunit) - hr_truth) / hr_truth, 
            uci = ((tmpsum$CI[3] %#% tmpunit) - hr_truth) / hr_truth) 
        
        rv$hr$tbl <<- rbind(
          rv$hr$tbl, 
          .build_tbl(
            target = "hr",
            group = if (rv$grouped) group else NA,
            data = rv$simList[[sim_no]], 
            seed = rv$seedList[[sim_no]],
            obj = rv$akdeList[[sim_no]],
            tau_p = tau_p,
            tau_v = tau_v,
            sigma = sigma,
            area = out_est_df[i, ],
            area_error = out_err_df[i, ]))
      }
      
      rv$hrEst <<- rbind(rv$hrEst, out_est_df)
      rv$hrErr <<- rbind(rv$hrErr, out_err_df)
      
      time_hr <- difftime(Sys.time(), start, units = "sec")
      rv$time[["hr"]][[1]] <- rv$time[["hr"]][[1]] + time_hr[[1]]
      
      msg_log(
        style = "success",
        message = paste0("Estimation ",
                         msg_success("completed"), "."),
        run_time = time_hr)
      
      rv$hr_completed <- TRUE
      rv$is_analyses <- TRUE
      rv$is_report <- FALSE
      rv$is_meta <- FALSE
      
      # UI elements:
      
      shinyjs::show(id = "hrBox_misc")
      tabs <- paste0("hrTabs_", tabnames)
      panels <- paste0("hrPanel_", tabnames)
      for (i in 1:length(tabnames))
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[i]),
          selected = paste0("tab_hrange_1-", panels[i]))
      
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      bindEvent(list(input$run_hr,
                     rv$m$proceed))
    
    ## Estimating for new sampling design: --------------------------------
    
    comparing_data <- reactive({
      
      set_id <- 1
      if (!is.null(rv$hr_nsim)) set_id <- rv$hr_nsim
      
      dat <- rv$simList[[set_id]]
      seed <- rv$seedList[[set_id]]
      
      dur <- rv$hr$dur$value %#% rv$hr$dur$unit
      dti <- rv$hr$dti$value %#% rv$hr$dti$unit
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
        rv$hr$groups <- stats::setNames(list(seed), group)
        
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
      bindCache(rv$hr$dur,
                rv$hr$dti,
                rv$simList)
    
    timing_fit <- reactive({
      
      out_time <- guess_time(data = rv$hr$simList, parallel = rv$parallel)
      return(out_time)
      
    }) %>% # end of reactive, timing_fit()
      bindCache(c(rv$hr$dur,
                  rv$hr$dti,
                  rv$hr$simList))
    
    fitting_new_model <- reactive({
      
      newsimList <- rv$hr$simList
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
      
      return(out_fit)
      
    }) %>% # end of reactive, fitting_new_model()
      bindCache(c(rv$hr$dur,
                  rv$hr$dti,
                  rv$hr$simList))
    
    estimating_hr_new <- reactive({
      tryCatch(
        ctmm::akde(rv$hr$simList, rv$hr$fitList),
        warning = function(w) NULL,
        error = function(e) NULL)
      
    }) %>% # end of reactive, estimating_hr_new()
      bindCache(rv$hr$dur,
                rv$hr$dti,
                rv$hr$simList)
    
    observe({
      req(rv$simList, rv$simfitList)
      
      set_id <- 1
      if (!is.null(rv$hr_nsim)) set_id <- rv$hr_nsim
      
      # Capture new sampling duration and interval:
      
      device <- movedesign::fixrates
      dti_unit <- sub('^.* ([[:alnum:]]+)$', '\\1', input$hr_dti)
      dti <- dti_unit %#% round(
        device$dti[match(input$hr_dti, device$dti_notes)], 0)
      
      rv$hr$dti <- data.frame(
        value = dti,
        unit = dti_unit)
      
      rv$hr$dur <- data.frame(
        value = input$hr_dur,
        unit = "days")
      
      # Check if storage limitations:
      
      proceed <- TRUE
      if (!is.null(rv$storage)) {
        
        n_new <- length(
          seq(0, round(input$hr_dur %#% "days", 0),
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
      rv$hr$simList <- dat
      time_hr <- difftime(Sys.time(), start, units = "sec")
      
      msg_log(
        style = "success",
        message = paste0("Simulation ", msg_success("completed"), "."),
        run_time = time_hr)
      
      ### 2. Fit models to simulation:
      
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("in progress"), "."),
        detail = "Please wait for model selection to finish:")
      
      start_fit <- Sys.time()
      expt <- timing_fit()
      shinybusy::remove_modal_spinner()
      
      loading_modal("Selecting new movement model", exp_time = expt)
      
      rv$hr$fitList <- fitting_new_model()
      time_sd <- difftime(Sys.time(), start_fit, units = "sec")
      
      msg_log(
        style = "success",
        message = paste0("Model fit ", msg_success("completed"), "."),
        run_time = time_sd)
      shinybusy::remove_modal_spinner()
      
      ### 3. Run the home range estimator (AKDE):
      
      loading_modal("Estimating new home range")
      
      rv$N1_new <- N <- extract_dof(rv$hr$fitList, "area")
      
      if (all(N < 5)) {
        
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
        truthList <- get_true_hr(
          data = rv$hr$simList[[1]],
          seed = rv$seedList[[set_id]],
          sigma = rv$sigma,
          
          emulated = rv$is_emulate,
          fit = if (rv$is_emulate) rv$meanfitList else NULL,
          
          grouped = rv$grouped,
          groups = if (rv$grouped) rv$hr$groups else NULL)
        truth <- truthList[[1]]$area
        
        tmpsumm <- summary(akde_new[[1]])
        tmpname <- rownames(tmpsumm$CI)
        tmpunit <- extract_units(tmpname[grep('^area', tmpname)])
        
        out_est <- c(
          "lci" = tmpsumm$CI[1], 
          "est" = tmpsumm$CI[2], 
          "uci" = tmpsumm$CI[3]) 
        
        out_err <- c( 
          "lci" = ((out_est[[1]] %#% tmpunit) - truth) / truth, 
          "est" = ((out_est[[2]] %#% tmpunit) - truth) / truth, 
          "uci" = ((out_est[[3]] %#% tmpunit) - truth) / truth) 
        
        if (is.null(rv$hr_nsim)) set_id <- 1
        else set_id <- rv$hr_nsim
        
        out_est <- data.frame(seed = rv$seedList[[set_id]],
                              lci = out_est[[1]], 
                              est = out_est[[2]], 
                              uci = out_est[[3]], 
                              unit = tmpunit)
        out_err <- data.frame(seed = rv$seedList[[set_id]],
                              lci = out_err[[1]], 
                              est = out_err[[2]], 
                              uci = out_err[[3]])
        
        group <- "All"
        if (rv$grouped) {
          get_group <- function(seed, groups) {
            if (as.character(seed) %in% groups[["A"]]) { 
              return("A") } else { return("B") }
          }
          group <- get_group(rv$seedList[[set_id]], rv$groups[[2]])
        }
        
        if (rv$is_emulate) {
          tau_p <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           rv$seedList[[set_id]]),
            "position")[[1]]
          tau_v <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           rv$seedList[[set_id]]),
            "velocity")[[1]]
          sigma <- extract_pars(
            emulate_seeded(rv$meanfitList[[group]],
                           rv$seedList[[set_id]]),
            "sigma")[[1]]
        } else {
          tau_p <- rv$tau_p[[group]]
          tau_v <- rv$tau_v[[group]]
          sigma <- rv$sigma[[group]]
        }
        
        rv$hr$tbl <<- rbind(
          rv$hr$tbl, 
          .build_tbl(
            data_type = "Modified",
            target = "hr",
            group = group,
            data = rv$hr$simList[[1]],
            seed = rv$seedList[[set_id]],
            obj = akde_new[[1]],
            tau_p = tau_p,
            tau_v = tau_v,
            sigma = sigma,
            area = out_est[1, ],
            area_error = out_err[1, ]))
        
        rv$hrEst_new <- out_est
        rv$hrErr_new <- out_err
        
        rv$is_analyses <- TRUE
        rv$hr$akdeList <- akde_new
        time_hr <- difftime(Sys.time(), start_est, units = "sec")
        rv$time[["hr"]][[2]] <- rv$time[["hr"]][[2]] + 
          difftime(Sys.time(), start, units = "sec")[[1]]
        
        msg_log(
          style = "success",
          message = paste0("Estimation ",
                           msg_success("completed"), "."),
          run_time = time_hr)
        
      } # end of if ()
      
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      shiny::bindEvent(input$run_hr_new)
    
    # PLOTS ---------------------------------------------------------------
    ## Plotting home range: -----------------------------------------------
    
    output$hrPlot <- ggiraph::renderGirafe({
      req(rv$is_analyses, 
          rv$simList, length(rv$akdeList) > 0)
      
      req(length(rv$simList) == length(rv$akdeList))
      
      nsim <- 1
      if (!is.null(input$hr_nsim)) {
        req(input$hr_nsim)
        req(input$hr_nsim <= length(rv$simList))
        nsim <- as.integer(input$hr_nsim)
      }
      
      show_truth <- FALSE
      show_locations <- FALSE
      
      if (!is.null(input$hr_truth)) {
        show_truth <- ifelse(input$hr_truth, TRUE, FALSE)
      }
      
      if (!is.null(input$hr_locations)) {
        show_locations <- ifelse(input$hr_locations, TRUE, FALSE)
      }
      
      if (is.null(rv$akdeList[[nsim]])) {
        ud <- ggplot2::ggplot() +
            ggtext::geom_richtext(
              mapping = ggplot2::aes(x = 1, y = 1),
              size = 5,
              color = "grey50",
              fill = NA, label.color = NA,
              label = paste0("[Effective sample size *too low*]")) + 
            ggplot2::theme_void()
        
      } else {
        
        truth <- rv$truth$hr[[nsim]]$data
        ext <- ctmm::extent(list(rv$simList[[nsim]],
                                 rv$akdeList[[nsim]],
                                 truth))
        
        if (!is.null(rv$hr$akdeList)) {
          req(rv$hr$simList, rv$hr$akdeList)
          ext <- ctmm::extent(list(rv$simList[[nsim]],
                                   rv$akdeList[[nsim]],
                                   rv$hr$simList,
                                   rv$hr$akdeList[[1]],
                                   truth))
        }
        
        req(rv$simList[[nsim]],
            rv$akdeList[[nsim]])
        
        ud <- plotting_hr(
          input1 = list(data = rv$simList[[nsim]],
                        ud = rv$akdeList[[nsim]]),
          truth = truth,
          show_truth = show_truth,
          show_locations = show_locations,
          contours = input$hr_contours,
          color = pal$sea,
          extent = ext,
          font_available = rv$is_font)
      }
      
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
      req(length(rv$simList) == length(rv$akdeList))
      req(rv$hr$simList, rv$hr$akdeList, rv$hr_nsim)
      
      show_truth <- FALSE
      show_locations <- FALSE
      
      if (!is.null(input$hr_truth_new)) {
        show_truth <- ifelse(input$hr_truth_new, TRUE, FALSE)
      }
      
      if (!is.null(input$hr_locations_new)) {
        show_locations <- ifelse(input$hr_locations_new, TRUE, FALSE)
      }
      
      # Rendering home range estimate plot:
      truthList <- get_true_hr(
        data = rv$hr$simList[[1]],
        seed = rv$seedList[[rv$hr_nsim]],
        sigma = rv$sigma,
        
        emulated = rv$is_emulate,
        fit = if (rv$is_emulate) rv$meanfitList else NULL,
        
        grouped = rv$grouped,
        groups = if (rv$grouped) rv$hr$groups else NULL)
      truth <- truthList[[1]]$data
      
      ext <- ctmm::extent(list(rv$simList[[rv$hr_nsim]], 
                               rv$hr$simList[[1]],
                               rv$akdeList[[rv$hr_nsim]], 
                               rv$hr$akdeList[[1]]))
      
      ud_sim <- plotting_hr(
        input1 = list(data = rv$simList[[rv$hr_nsim]],
                      ud = rv$akdeList[[rv$hr_nsim]]),
        input2 = list(data = rv$hr$simList[[1]], 
                      ud = rv$hr$akdeList[[1]]),
        truth = truth,
        show_truth = show_truth,
        show_locations = show_locations,
        show_both = input$hr_datasets,
        contours = input$hr_contours_new,
        color = pal$dgr,
        extent = ext,
        font_available = rv$is_font)
      
      ggiraph::girafe(
        ggobj = ud_sim,
        options = list(
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill: #ffbf00;",
                        "stroke: #ffbf00;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("fill: #dd4b39;",
                        "stroke: #eb5644;")))
      )
      
    }) # end of renderGirafe
    
    # TABLES --------------------------------------------------------------
    ## Initial sampling design: -------------------------------------------
    
    observe({
      req(rv$simList, rv$hrErr, input$add_hr_table)
      
      shinyjs::show(id = "hrBox_summary")
      rv$hr$tbl <- dplyr::distinct(rv$hr$tbl)
      
    }) %>% # end of observe
      bindEvent(list(input$add_hr_table, rv$hrErr))
    
    ## New sampling design: -----------------------------------------------
    
    observe({
      req(rv$hr$tbl, rv$hr$fitList, rv$hrEst_new)
      rv$hr$tbl <- dplyr::distinct(rv$hr$tbl)
      
    }) %>% # end of observe
      bindEvent(list(input$add_hr_table, rv$hr$fitList))
    
    ## Rendering output table: --------------------------------------------
    
    output$hrTable <- reactable::renderReactable({
      req(rv$hr$tbl, rv$akdeList)
      
      dt_hr <- dplyr::select(rv$hr$tbl, -seed)
      
      if (!rv$grouped) {
        dt_hr <- dplyr::select(
          dt_hr, -c(device, group, tauv, sigma, N2, ctsd:dist_err))
      } else {
        dt_hr <- dplyr::select(
          dt_hr, -c(device, tauv, sigma, N2, ctsd:dist_err))
      }
      
      nms <- list(
        data = "Data",
        group = "Group",
        taup = "\u03C4\u209A",
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = "N (area)",
        area = "Area",
        area_err = "Error",
        area_err_min = "95% LCI",
        area_err_max = "95% UCI")
      
      nms_sizes <- reactable::colGroup(
        name = "Sample sizes", 
        columns = c("n", "N1"))
      nms_error <- reactable::colGroup(
        name = "Home range",
        columns = c("area",
                    "area_err",
                    "area_err_min",
                    "area_err_max"))
      
      colgroups <- list(nms_sizes, nms_error)
      
      if (length(unique(dt_hr$data)) == 1) {
        dt_hr <- dplyr::select(dt_hr, -data)
        nms <- nms[-1]
        colgroups <- colgroups[-1]
      }
      
      reactable::reactable(
        data = dt_hr,
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
          group = reactable::colDef(
            name = nms[["group"]]),
          taup = reactable::colDef(
            minWidth = 80, name = nms[["taup"]],
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
          N1 = reactable::colDef(
            minWidth = 80, name = nms[["N1"]],
            style = format_num,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 1)),
          area = reactable::colDef(
            minWidth = 80, name = nms[["area"]]),
          area_err = reactable::colDef(
            minWidth = 80, name = nms[["area_err"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)),
          area_err_min = reactable::colDef(
            minWidth = 80, name = nms[["area_err_min"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1)),
          area_err_max = reactable::colDef(
            minWidth = 80, name = nms[["area_err_max"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US",
              percent = TRUE, digits = 1))
        ),
        columnGroups = colgroups)
      
    }) %>% # end of renderReactable, "hrTable"
      bindEvent(list(input$add_hr_table, rv$hr$tbl))
    
    # BLOCKS --------------------------------------------------------------
    ## Tracking device: ---------------------------------------------------
    ### Initial sampling design: ------------------------------------------
    
    observe({
      req(rv$active_tab == 'hr')
      req(rv$simList)
      
      mod_blocks_server(
        id = "hrBlock_dur",
        rv = rv, data = rv$simList,
        type = "dur")
      
      mod_blocks_server(
        id = "hrBlock_dti",
        rv = rv, data = rv$simList,
        type = "dti")
      
    }) # end of observe
    
    ### Modified sampling design: -----------------------------------------
    
    observe({
      req(rv$hr$simList)
      
      mod_blocks_server(
        id = "hrBlock_dur_new",
        rv = rv, data = rv$hr$simList,
        type = "dur",
        class = "cl-mdn")
      
      mod_blocks_server(
        id = "hrBlock_dti_new",
        rv = rv, data = rv$hr$simList,
        type = "dti",
        class = "cl-mdn")
      
    }) # end of observe
    
    ## Sample sizes: ------------------------------------------------------
    
    observe({
      req(rv$active_tab == 'hr', rv$simList)
      
      mod_blocks_server(
        id = "hrBlock_n", 
        rv = rv, data = rv$simList,
        type = "n",
        options = list(rightBorder = FALSE,
                       marginBottom = TRUE))
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'hr',
          rv$simList, rv$simfitList)

      mod_blocks_server(
        id = "hrBlock_N",
        rv = rv, data = rv$simList, obj = rv$simfitList,
        type = "N", name = "area")

    }) # end of observe

    observe({
      req(rv$active_tab == 'hr', rv$hr$simList)
      
      mod_blocks_server(
        id = "hrBlock_n_new", 
        rv = rv, data = rv$hr$simList,
        type = "n",
        options = list(rightBorder = FALSE,
                       marginBottom = TRUE))
      
    }) # end of observe
    
    observe({
      req(rv$hr$simList, rv$hr$fitList)
      
      mod_blocks_server(
        id = "hrBlock_N_new",
        rv = rv, data = rv$hr$simList, obj = rv$hr$fitList,
        type = "N", name = "area", class = "cl-mdn")
      
    }) # end of observe
    
    ## Groups: ------------------------------------------------------------
    
    observe({
      req(rv$which_meta == "compare", rv$grouped)
      req(rv$simList, rv$akdeList, rv$hr_nsim)
      
      shinyjs::show(id = "hrBlock_group")
      
      set_id <- rv$hr_nsim
      set_group <- names(rv$groups[[2]])[
        sapply(rv$groups[[2]], function(x)
          names(rv$simList)[[set_id]] %in% x)]
      
      output$hrBlock_group <- renderUI({
        
        parBlock(
          icon = "object-ungroup",
          header = "Group",
          value = set_group)
        
      }) # end of renderUI, "hrBlock_group"
      
    }) %>% # end of observe,
      bindEvent(list(rv$hr_nsim, rv$active_tab == 'sd'))
  
    ## Home range outputs: ------------------------------------------------
    
    observe({
      req(rv$active_tab == 'hr')
      req(rv$simList, rv$akdeList, rv$hr_nsim)
      req(nrow(rv$hrEst) == length(rv$simList),
          nrow(rv$hrErr) == length(rv$simList))
      
      mod_blocks_server(
        id = "hrBlock_est",
        rv = rv, type = "hr", name = "hrEst", get_id = rv$hr_nsim)
      mod_blocks_server(
        id = "hrBlock_err",
        rv = rv, type = "hr", name = "hrErr", get_id = rv$hr_nsim)
    
    }) # end of observe
    
    observe({
      req(rv$hr$simList, rv$hr$akdeList, 
          rv$hrEst_new,
          rv$hrErr_new)
      
      mod_blocks_server(
        id = "hrBlock_est_new",
        rv = rv, type = "hr", name = "hrEst_new",
        class = "cl-mdn")

      mod_blocks_server(
        id = "hrBlock_err_new",
        rv = rv, type = "hr", name = "hrErr_new",
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
          showStepNumbers = FALSE,
          showButtons = TRUE,
          showBullets = TRUE
        ),
        events = list(onbeforechange =
                        rintrojs::readCallback('switchTabs')))

    }) %>% # end of observe,
      bindEvent(input$hrHelp_schedule)

    ## Help modal (biases): -----------------------------------------------

    observe({

      shiny::showModal(
        shiny::modalDialog(
          title = h4(span("Home range", class = "cl-sea"),
                     "estimation:"),

          fluidRow(
            style = paste("margin-right: 20px;",
                          "margin-left: 20px;"),

            p("As animal movement",
              "is inherently", span("autocorrelated", class = "cl-sea-d"),
              "(locations are similar as a function of space and",
              " distance), the",
              span("Autocorrelated Kernel Density Estimators (AKDEs)",
                   class = "cl-sea"),
              "are the most appropriate method for",
              span("home range", class = "cl-sea-d"), "estimation."),

            h4(style = "margin-top: 30px;", "For more information:"),

            p(style = "font-family: var(--monosans);",
              "- Silva, I., Fleming, C. H., Noonan, M. J., Alston, J.,",
              "Folta, C., Fagan, W. F., & Calabrese, J. M. (2022).",
              "Autocorrelation-informed home range estimation: A review",
              "and practical guide. Methods in Ecology and Evolution,",
              "13(3), 534-544."),

            p(style = "font-family: var(--monosans);",
              "- Calabrese, J. M., Fleming, C. H., & Gurarie, E. (2016).",
              "ctmm: An R package for analyzing animal relocation data",
              "as a continuous-time stochastic process. Methods in",
              "Ecology and Evolution, 7(9), 1124-1132.")

          ), # end of fluidRow

          footer = modalButton("Dismiss"),
          size = "m")) # end of modal

    }) %>% # end of observe,
      bindEvent(input$hrHelp_method)

    observe({

      out_sims <- ""

      shiny::showModal(
        shiny::modalDialog(
          title = h4(
            "How much", span("uncertainty", class = "cl-sea"),
            "is associated with our estimate(s)?"),

          fluidRow(
            style = paste("margin-right: 20px;",
                          "margin-left: 20px;"),

            p("Estimating an home range with AKDEs",
              "allows for the calculation of the 95% confidence",
              "intervals on the area estimate.",
              "Ideally, we want narrow confidence intervals."),

            p("However, the quality of these estimates and CIs",
              "depend on the movement model chosen,",
              "emphasizing the significance of visual analysis",
              "and evaluation of the effective sample sizes."),

            p("Here, for a single simulation, we calculate the",
              "relative error of both the",
              span("point estimate", class = "cl-sea-d"), "and",
              span("95% confidence intervals", class = "cl-sea-d"),
              "in relation to the truth:"),

            withMathJax(
              paste0("$$\\small{\\text{Error (%)}",
                     " = \\frac{\\text{estimate}-\\text{truth}}",
                     "{\\text{truth}}\\times 100}$$")
            ),

            if (!is.null(rv$simList)) {
              if (length(rv$simList) > 1) {
                p("For multiple simulations, we calculate the",
                  span("mean", class = "cl-sea-d"), "and the",
                  span("95% credible intervals", class = "cl-sea-d"),
                  "of all point estimates. Credible intervals allow",
                  "for an easier interpretation: for instance, the",
                  "relative error of any simulation has a 95%",
                  "probability of falling within the reported",
                  "range (shown within brackets below the point",
                  "estimate).")
              }
            },

            p("This expected error decreases as",
              span("sampling duration", class = "cl-sea"),
              "increases, and is ultimately dependent on the",
              span("position autocorrelation",
                   wrap_none("(\u03C4", tags$sub("p"), ")"),
                   "timescale.", class = "cl-sea-d"))

          ), # end of fluidRow

          footer = modalButton("Dismiss"),
          size = "m")) # end of modal

    }) %>% # end of observe,
      bindEvent(input$hrHelp_bias)

    # MISC ----------------------------------------------------------------

    output$out_time_hr <- renderText({
      req(rv$time[["hr"]][[1]] > 0)
      
      out <- fix_unit(rv$time[["hr"]][[1]], "seconds", convert = TRUE)
      out_txt <- paste0("Initial sampling design took approximately ",
                        out$value, " ", out$unit, ".")
      out_txt

    }) # end of renderText, "time_hr"

    output$out_time_hr_new <- renderText({
      req(rv$time[["hr"]][[2]] > 0, rv$hr$akdeList)

      out <- fix_unit(rv$time[["hr"]][[2]], "seconds", convert = TRUE)
      out_txt <- paste0("New sampling design took approximately ",
                        out$value, " ", out$unit, ".")
      out_txt

    }) # end of renderText, "time_hr_new"

    output$out_time_hr_total <- renderText({
      req(rv$time[["hr"]][[2]] > 0, rv$hr$akdeList)

      total_time <- rv$time[["hr"]][[1]] + rv$time[["hr"]][[2]]

      out <- fix_unit(total_time, "seconds", convert = TRUE)
      out_txt <- paste0("... In total, this section took ",
                        out$value, " ", out$unit, ".")
      out_txt

    }) # end of renderText, "time_hr_total"
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_hrange_ui("tab_hrange_1")

## To be copied in the server
# mod_tab_hrange_server("tab_hrange_1")
