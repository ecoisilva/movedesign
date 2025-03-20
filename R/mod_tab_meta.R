#' tab_meta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_tab_meta_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      # Introduction: -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          shinydashboardPlus::box(
            title = span("Meta-analyses:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "layer-group",
                                   height = "21px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "var(--sea-dark)"),
            id = ns("meta_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = TRUE, closable = FALSE,
            
            column(
              align = "center", width = 12,
              p(style = "max-width: 1000px;",
                
                "The main goal in this tab is to conduct",
                "population-level inference of key metrics, where",
                "multiple individual estimates are treated as samples",
                "from a broader population. This is crucial to evaluate",
                wrap_none(span("changes over time", 
                               class = "cl-jgl"), ","),
                span("space", class = "cl-jgl"), "or",
                span("covariates", class = "cl-jgl"), "(e.g.",
                "habitat quality or fragmentation) and for",
                "comparative analyses of",
                wrap_none(span("species averages", 
                               class = "cl-jgl"), ".")),
              
              p(style = "text-align: center;",
                "If population-level inferences are your goal,", br(), 
                "then click the", icon("paper-plane", class = "cl-mdn"),
                wrap_none(span("Run meta-analyses", class = "cl-mdn")),
                "button."),
              
              splitLayout(
                cellWidths = c("38px", "1%", "200px"),
                cellArgs = list(style = 'align: center;'),
                
                shiny::actionButton(
                  inputId = ns("metaHelp_method"),
                  label = NULL,
                  width = "100%",
                  icon = icon("circle-question"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("run_meta"),
                  label = "Run meta-analyses",
                  icon =  icon("paper-plane"),
                  width = "100%",
                  class = "btn-primary")
              ),
              
              br(),
              shinyWidgets::radioGroupButtons(
                inputId = ns("metaOutput_target"),
                width = "260px",
                label = "Show outputs for:",
                choices = c(
                  "Home range" = "hr",
                  "Speed & distance" = "ctsd"),
                selected = "hr",
                checkIcon = list(yes = icon("circle-check")),
                direction = "vertical",
                justified = TRUE),
              br()
              
            ) # end of column (for text)
            
          ) # end of box // meta_intro
      ), # end of div (top row)
      
      # [left column] -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-4 col-md-3 col-lg-2",
          
          ## Number of simulations: ---------------------------------------
          
          shinydashboardPlus::box(
            title = tagList(
              icon("bolt", class = "cl-jgl"),
              span("Simulations:", class = "ttl-box cl-jgl")),
            id = ns("metaBox_simulations"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            p(style = "text-align: right !important;",
              HTML("&nbsp;"), "No. simulations (total):") %>%
              tagAppendAttributes(class = 'label_split'),
            
            fluidRow(
              column(width = 12,
                     verbatimTextOutput(outputId = ns("nsims_total"))
              )), p(style = "padding: 0px;"),
            
            div(id = ns("txt_hr_ratio_label"),
                p(style = "text-align: right !important;",
                  HTML("&nbsp;"), "Home range ratio:") %>%
                  tagAppendAttributes(class = 'label_split')),
            fluidRow(
              column(width = 12,
                     verbatimTextOutput(outputId = ns("txt_hr_ratio"))
              )),
            
            br(),
            div(id = ns("txt_sd_ratio_label"),
                p(style = "text-align: right !important;",
                  HTML("&nbsp;"), "Speed ratio:") %>%
                  tagAppendAttributes(class = 'label_split')),
            fluidRow(
              column(width = 12,
                     verbatimTextOutput(outputId = ns("txt_sd_ratio"))
              ))
            
          ), # end of box // hrBox_sizes
          
          # Error: --------------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Mean home range:", class = "ttl-box"),
            id = ns("metaBox_err_hr"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            mod_blocks_ui(ns("metaBlock_hr")),
            uiOutput(ns("metaBlock_hr_ratio"))
            
          ), # end of box // metaBox_err_hr
          
          shinydashboardPlus::box(
            title = span("Mean speed:", class = "ttl-box"),
            id = ns("metaBox_err_speed"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            mod_blocks_ui(ns("metaBlock_speed")),
            uiOutput(ns("metaBlock_speed_ratio"))
            
          ) # end of box // metaBox_err_speed
          
      ), # end of div (left column)
      
      # [right column] ----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-8 col-md-9 col-lg-10",
          
          # Outputs: ------------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Outputs:", class = "ttl-box"),
            id = ns("metaBox_outputs"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            p(style = "margin-top: 10px;"),
            tabsetPanel(
              id = ns("metaTabs_outputs"),
              
              tabPanel(
                value = ns("metaPanel_all"),
                title = tagList(
                  icon("paw", class = "cl-sea"),
                  span("Dataset", class = "ttl-panel")
                ),
                
                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-5",
                    reactable::reactableOutput(ns("metaTable_all")),
                    uiOutput(ns("metaUI_legend_all"))),
                
                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-7",
                    fluidRow(column(
                      width = 12,
                      
                      ggiraph::girafeOutput(
                        outputId = ns("metaPlot_all"),
                        width = "100%", height = "100%")
                      
                    ))) # end of column, fluidRow, div
              ), # end of panels (1 out of 2)
              
              tabPanel(
                value = ns("metaPanel_groups"),
                title = tagList(
                  icon("object-ungroup", class = "cl-jgl"),
                  span("Group level", class = "ttl-panel cl-jgl")
                ),
                
                p(style = "margin-top: 10px;"),
                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-5",
                    shiny::selectizeInput(
                      inputId = ns("metaInput_mod"),
                      label = "Type:",
                      choices = "",
                      selected = NULL,
                      multiple = FALSE,
                      options = list(
                        onInitialize = 
                          I('function() { this.setValue(""); }'))),
                    reactable::reactableOutput(ns("metaTable_groups")),
                    uiOutput(ns("metaUI_legend"))
                ),
                
                div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-7",
                    ggiraph::girafeOutput(
                      outputId = ns("metaPlot_groups"),
                      width = "100%", height = "100%")),
                
              ) # end of panels (1 out of 2)
            ), # end of tabs
            
            footer = uiOutput("metaUI_footer")
            
          ), # end of box // metaBox_viz
          
          # Table: --------------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Optimal number of tracked individuals:",
                         class = "ttl-box"),
            id = ns("metaBox_summary"),
            width = NULL,
            solidHeader = FALSE,
            
            p(style = "margin-top: 10px;"),
            div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-6",
                mod_viz_meta_ui("viz_meta_1")),
                
            div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-6",
                reactable::reactableOutput(
                  ns("metaTable_m_optimal"))),
            
            footer = column(
              width = 12,
              style = "max-width: 390px; float: right;",
              
              splitLayout(
                cellWidths = c("20%", "1%",
                               "41.5%", "1%",
                               "38.5%"),
                cellArgs = list(style = "align: center;"),
                
                shiny::actionButton(
                  inputId = ns("metaHelp_resample"),
                  label = NULL,
                  width = "100%",
                  icon = icon("circle-question"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("run_meta_resample"),
                  label = span("Resample"),
                  icon =  icon("wand-sparkles"),
                  width = "100%",
                  class = "btn-info"),
                br(),
                shinyWidgets::autonumericInput(
                  inputId = ns("n_resamples"),
                  label = NULL,
                  currencySymbol = " resamples(s)",
                  currencySymbolPlacement = "s",
                  decimalPlaces = 0,
                  minimumValue = 1,
                  maximumValue = 1000,
                  value = 5, wheelStep = 5),
                
              )) # end of footer
            
          ) # end of box // metaBox_summary
          
      ) # end of column (right)
      
    ), # end of fluidRow
    
    # MODALS: -------------------------------------------------------------
    
    NULL
    
  ) # end of tagList
}
    
#' tab_meta Server Functions
#'
#' @noRd 
mod_tab_meta_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
      
    rv$meta <- reactiveValues(n_resample = 1)
    
    # Run meta-analyses multiple ways:
    
    observe({
      rv$run_meta <- input$run_meta
    }) %>% bindEvent(input$run_meta)
    
    observe({
      rv$run_meta_resample <- input$run_meta_resample
    }) %>% bindEvent(input$run_meta_resample)
    
    to_run_meta <- reactive({
      list(input$run_meta, input$run_meta_resample)
    })
    
    ## Switch if both research targets are available: ---------------------
    
    observe({
      
      if (length(rv$which_question) == 2) {
        req(input$metaOutput_target)
        rv$set_analysis <- input$metaOutput_target
      }
      
    }) %>% # end of observe,
      bindEvent(list(rv$which_question,
                     input$metaOutput_target,
                     input$run_meta))
    
    ## Update number of resamples: ----------------------------------------
    
    # observe({
    #   req(rv$meta$n_resample > 1)
    #   
    #   # if (!is.null(rv$meta_tbl_resample)) {
    #   #   rv$meta$n_resample <- max(unique(rv$meta_tbl_resample$sample))
    #   # }
    #   
    #   shinyWidgets::updateAutonumericInput(
    #     session = session,
    #     inputId = "n_resamples",
    #     value = rv$meta$n_resample)
    #   
    # }) %>% # end of observe,
    #   bindEvent(rv$meta$n_resample)
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at the start: ----------------------------------------
    
    shinyjs::hide(id = "txt_ratio")
    
    boxnames <- c("simulations",
                  "outputs",
                  # "summary",
                  "err_hr",
                  "err_speed")
    
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("metaBox_", boxnames[i]))
    }
    
    ## Update ratio text based on workflow: -------------------------------
    
    observe({
      req(rv$active_tab == 'meta')
      req(rv$which_meta, rv$which_question)
      
      shinyjs::hide(id = "txt_hr_ratio")
      shinyjs::hide(id = "txt_hr_ratio_label")
      shinyjs::hide(id = "txt_sd_ratio")
      shinyjs::hide(id = "txt_sd_ratio_label")
      
      if ("compare" %in% rv$which_meta) {
        if ("Home range" %in% rv$which_question) {
          shinyjs::show(id = "txt_hr_ratio")
          shinyjs::show(id = "txt_hr_ratio_label")
        } 
        
        if ("Speed & distance" %in% rv$which_question) {
          shinyjs::show(id = "txt_sd_ratio")
          shinyjs::show(id = "txt_sd_ratio_label")
        }
      }
      
    }) # end of observe
    
    ## Update based on outputs: -------------------------------------------
    
    observe({
      req(rv$is_valid)
      
      if (rv$is_valid)
        shinyjs::show(id = "metaBox_simulations")
       else shinyjs::hide(id = "metaBox_simulations")
      
    }) # end of observe
    
    observe({
      req(rv$grouped)
      req(rv$metaList_groups[[3]], rv$set_analysis)
      
      out <- rv$metaList_groups[[2]][[rv$set_analysis]]$mods
      req(length(out) == 4)
      
      mod_choices <- as.list(names(out))
      names(mod_choices) <- c("Variation in Sub-population A",
                              "Variation in Sub-population B",
                              "Variation in Joint population",
                              "Joint versus sub-populations")
      
      shiny::updateSelectizeInput(
        session,
        inputId = "metaInput_mod",
        label = "Outputs to show:",
        choices = mod_choices,
        selected = mod_choices[[1]])
      
    }) # end of observe
    
    observe({
      hideTab(inputId = "metaTabs_outputs",
              target = ns("metaPanel_groups"))
      req(rv$metaList_groups)
      if (rv$metaList_groups[[3]])
        showTab(inputId = "metaTabs_outputs",
                target = ns("metaPanel_groups"))
      
    }) # end of observe
    
    output$nsims_total <- renderText({
      if (!is.null(rv$simList)) return(length(rv$simList))
      else return(0)
    }) # end of renderText, "nsims_total"
    
    
    ## Render new text (for effect size): ---------------------------------
    
    output$txt_hr_ratio <- renderText({
      req("compare" %in% rv$which_meta, rv$metaList_groups[[2]])
      
      out_txt <- NULL
      var <- "home range area"
      diff <- c("smaller", "larger")
      
      meta <- rv$metaList_groups[[2]][["hr"]]
      req(meta)
      ratio <- round(.get_ratios(meta)$est, 1)
      ratio
      
      if (ratio == 1) out_txt <- paste0(
        "Group A's ", var, " is equal to Group B's.")
      else if (ratio < 1) out_txt <- paste0(
        "Group A's ", var, " is ",
        round(abs(100 - ratio * 100), 1),
        "% ", diff[1], " than Group B's.")
      else if (ratio > 1) out_txt <- paste0(
        "Group A's ", var, " area is ",
        round(abs(100 - ratio * 100), 1),
        "% ", diff[2], " than Group B's.")
      
      return(out_txt)
      
    }) # end of renderText, "txt_hr_ratio"
    
    output$txt_sd_ratio <- renderText({
      req("compare" %in% rv$which_meta, rv$metaList_groups[[2]])
      
      out_txt <- NULL
      var <- "speed"
      diff <- c("slower", "faster")
      
      meta <- rv$metaList_groups[[2]][["ctsd"]]
      req(meta)
      ratio <- round(.get_ratios(meta)$est, 1)
      ratio
      
      if (ratio == 1) out_txt <- paste0(
        "Group A's ", var, " is equal to Group B's.")
      else if (ratio < 1) out_txt <- paste0(
        "Group A's ", var, " is ",
        round(abs(100 - ratio * 100), 1),
        "% ", diff[1], " than Group B's.")
      else if (ratio > 1) out_txt <- paste0(
        "Group A's ", var, " area is ",
        round(abs(100 - ratio * 100), 1),
        "% ", diff[2], " than Group B's.")
      
      return(out_txt)
      
    }) # end of renderText, "txt_sd_ratio"
    
    ## Show/hide summary table: -------------------------------------------
    
    # observe({
    #   req(input$add_meta_table)
    #   if (input$add_meta_table) 
    #        shinyjs::toggle(id = "metaBox_summary")
    #   
    # }) %>% # end of observe,
    #   bindEvent(input$add_meta_table)
    
    ## Add notes explaining table outputs: --------------------------------
    
    output$metaUI_legend_all <- renderUI({
      req(rv$grouped, rv$metaList, rv$set_analysis,
          rv$metaList)
      req(!is.na(rv$grouped))
      
      out <- rv$metaList[[rv$set_analysis]]
      is_subpop_detected <- out$logs$subpop_detected
      
      txt <- txt_evidence <- NULL
      col_subpop <- ifelse(is_subpop_detected, "cl-sea-d", "cl-dgr")
      
      txt_match <- "correctly"
      if (rv$grouped) {
        meta_truth <- rv$metaList_groups[[1]][[rv$set_analysis]]
        is_subpop <- meta_truth$logs$subpop_detected
        
        if (is_subpop == is_subpop_detected) {
          txt_match <- "correctly"
          col_subpop <- "cl-sea"
        } else {
          txt_match <- "incorrectly"
          col_subpop <- "cl-dgr"
        }
        
        if (is_subpop) {
          txt_evidence <- span(
            span(ifelse(
              meta_truth$mods$subpop_detected[[2,2]] > 2,
              "strong", "weak"), "evidence", class = col_subpop),
            "of sub-populations.")
          
        } else {
          txt_evidence <- span(
            span("no evidence", class = col_subpop),
            "of sub-populations",
            ifelse(meta_truth$mods$subpop_detected[[2,2]] < 2, 
                   "(though with \u0394AICc \uFF1C 2).",
                   "(\u0394AICc \uFF1E 2)."))
        }
        
        txt <- span(
          style = paste("font-size: 14px;",
                        "font-family: var(--monosans);"),
          "The initial dataset from the",
          shiny::icon("paw", class = "cl-sea"),
          span("Species", class = "cl-sea"), "tab had",
          txt_evidence)
        
      } else {
        # txt_method <- span(
        #   "Model selection is performed between the \u03C7\u00B2-IG",
        #   "population model (with population mean and variance) and",
        #   "the Dirac-\u03B4 population model (population mean only).")
      }
      
      if (rv$grouped && is_subpop_detected) {
        txt_subpop <- tagList(span(
          "In the simulated dataset, sub-populations were", 
          span(txt_match, class = col_subpop),
          "detected."))
      } else if (!rv$grouped && !is_subpop_detected) {
        txt_subpop <- tagList(span(
          "In the simulated dataset, a single population was", 
          span(txt_match, class = col_subpop),
          "detected. No evidence of sub-populations in the",
          "current dataset."))
      } else if (rv$grouped && !is_subpop_detected) {
        txt_subpop <- tagList(span(
          "In the simulated dataset, sub-populations were", 
          wrap_none(span("not detected", class = col_subpop), ".")))
      } else if (!rv$grouped && is_subpop_detected) {
        txt_subpop <- tagList(span(
          "In the simulated dataset, sub-populations were", 
          wrap_none(span("detected", class = col_subpop), ".")))
          # "even though simulations were based on",
          # "a single set of parameters."))
      }
      
      ui <- tagList(
        p(style = "margin-top: 35px;"),
        span(class = "help-block",
             style = "text-align: justify !important;",
             
             fontawesome::fa("circle-exclamation", fill = pal$dgr),
             span("Note:", class = "help-block-note"),
             # txt_method, br(),
             txt, txt_subpop
        ))
      
      return(ui)
      
    }) # end of renderUI, "metaUI_legend_all"

    output$metaUI_legend <- renderUI({
      req(rv$metaList_groups[[3]])
      
      ui <- tagList(
        p(style = "margin-top: 35px;"),
        span(class = "help-block",
             style = "text-align: justify !important;",
             
             fontawesome::fa("circle-exclamation", fill = pal$dgr),
             span("Note:", class = "help-block-note"),
             "Model selection is performed between the \u03C7\u00B2-IG",
             "population model (with population mean and variance) and",
             "the Dirac-\u03B4 population model (population mean only)."
        ))
      
      return(ui)
      
    }) # end of renderUI, "metaUI_legend"
    
    ## Render footer for outputs box: -------------------------------------
    
    output$metaUI_footer <- renderUI({
      req(rv$is_meta, rv$which_m)
      
      if (rv$which_m == "get_m") {
        shinyjs::show(id = "metaBox_summary")
        
        return(tagList(
          column(
            width = 12,
            style = "max-width: 250px; float: right;",
            
            splitLayout(
              cellWidths = c("20%", "1%", "79%"),
              cellArgs = list(style = "align: center;"),
              
              shiny::actionButton(
                inputId = ns("metaHelp_bias"),
                label = NULL,
                width = "100%",
                icon = icon("circle-question"),
                class = "btn-warning"),
              br(),
              shiny::actionButton(
                inputId = ns("add_meta_table"),
                label = span("Show", span("table", class = "cl-sea")),
                icon = icon("eye"),
                width = "100%")
              
            ))) # end of tagList
        ) # end of return
        
      } else {
        shinyjs::hide(id = "metaBox_summary")
        return(NULL)
      }
      
    }) # end of renderUI, "metaUI_footer"
    
    # OPERATIONS ----------------------------------------------------------
    ## Run global meta-analyses: ------------------------------------------
    
    observe({
      req(rv$active_tab == 'meta')
      req(rv$truth, !rv$is_meta, rv$is_valid)
      req(rv$simList, length(rv$simList) <= 2)
      
      m <- length(rv$simList)
      m <- ifelse(m == 1, "one simulation", "two simulations")
      
      shinyalert::shinyalert(
        type = "warning",
        title = "Warning",
        text = tagList(span(
          "Only", m, "currently available.",
          "Run more", span("simulations", class = "cl-grn"), 
          "in one of the previous analyses tabs."
        )),
        html = TRUE,
        size = "xs")
        
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'meta')
      req(rv$truth, !rv$is_meta, length(rv$simList) > 2)
      
      rv$metaEst <- NULL
      rv$metaErr <- NULL
      rv$metaEst_groups <- NULL
      rv$metaErr_groups <- NULL
      
      start <- Sys.time()
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = tagList(span(
          style = "font-size: 18px;",
          span("Running", style = "color: #797979;"),
          wrap_none(span("meta-analyses", class = "cl-sea"),
                    span("...", style = "color: #797979;")))
        ))
      
      shinyFeedback::showToast(
        type = "info",
        message = paste0("Running meta-analyses..."),
        .options = list(
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right")
      )
      
      msg_log(
        style = "warning",
        message = paste0("Meta-analyses ",
                         msg_warning("in progress"), "..."))
      
      set_target <- c()
      if ("Home range" %in% rv$which_question) {
        req(rv$akdeList)
        set_target <- c(set_target, "hr")
      }
      if ("Speed & distance" %in% rv$which_question) {
        req(rv$ctsdList)
        set_target <- c(set_target, "ctsd")
      }
      
      if (rv$grouped) {
        req(length(rv$groups[[2]]$A) > 0,
            length(rv$groups[[2]]$B) > 0)
      }
      
      lists <- .build_meta_objects(rv,
                                   set_target = set_target,
                                   subpop = rv$grouped,
                                   trace = FALSE)
      list2env(lists, envir = environment())
      
      metaList <- list()
      if ("Home range" %in% rv$which_question) {
        metaList[["hr"]] <- outList[["All"]][["hr"]] }
      if ("Speed & distance" %in% rv$which_question) {
        metaList[["ctsd"]] <- outList[["All"]][["ctsd"]] }
      
      if (rv$grouped) {
        
        datList_groups <- list()
        
        if ("Home range" %in% rv$which_question) {
          datList_groups <- datList[["groups"]][["hr"]]
          rv$metaList_groups[[2]][["hr"]] <- 
            outList[["groups"]][["hr"]]
        }
        
        if ("Speed & distance" %in% rv$which_question) {
          datList_groups <- .get_groups(rv$ctsdList, rv$groups[[2]])
          rv$metaList_groups[[2]][["ctsd"]] <-
            outList[["groups"]][["ctsd"]]
        }
        
      } # end of if (rv$grouped)
      
      for (i in seq_along(metaList)) {
        out <- metaList[[i]]
        
        name <- "mean"
        sum.obj <- out$meta
        nms.obj <- rownames(sum.obj)
        tmp <- sum.obj[grep(name, nms.obj), ]
        tmpunit <- extract_units(nms.obj[grep(name, nms.obj)])
        
        if (out$type == "hr") {
          truth_summarized <- get_true_hr(
            sigma = rv$sigma,
            
            emulated = rv$is_emulate,
            fit = if (rv$is_emulate) rv$meanfitList else NULL,
            
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL,
            summarized = TRUE)
          truth <- truth_summarized[["All"]]$area
        }
        
        if (out$type == "ctsd") {
          truth_summarized <- get_true_speed(
            data = rv$simList,
            seed = rv$seedList,
            
            tau_p = rv$tau_p,
            tau_v = rv$tau_v,
            sigma = rv$sigma,
            
            emulated = rv$is_emulate,
            fit = if (rv$is_emulate) rv$meanfitList else NULL,
            
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL,
            
            summarized = TRUE)
          truth <- truth_summarized[["All"]]
        }
        
        rv$metaEst <<- rbind(rv$metaEst, data.frame(
          type = out$type,
          group = "All",
          "lci" = tmp[[1]],
          "est" = tmp[[2]],
          "uci" = tmp[[3]],
          unit = tmpunit))
        
        rv$metaErr <<- rbind(rv$metaErr, data.frame(
          type = out$type,
          group = "All",
          "lci" = ((tmp[[1]] %#% tmpunit) - truth) / truth,
          "est" = ((tmp[[2]] %#% tmpunit) - truth) / truth,
          "uci" = ((tmp[[3]] %#% tmpunit) - truth) / truth))
      }
      
      if (rv$grouped) {
        for (target in rv$set_target) {
          out_groups <- rv$metaList_groups[[2]][[target]]
          
          sum.objA <- out_groups$meta$A
          sum.objB <- out_groups$meta$B
          nms.objA <- rownames(sum.objA)
          nms.objB <- rownames(sum.objB)
          
          tmpA <- sum.objA[grep(name, nms.objA), ]
          tmpB <- sum.objB[grep(name, nms.objB), ]
          tmpunitA <- extract_units(nms.objA[grep(name, nms.objA)])
          tmpunitB <- extract_units(nms.objB[grep(name, nms.objB)])
          
          if (out_groups$type == "hr") {
            
            truth_summarized <- get_true_hr(
              sigma = rv$sigma,
              emulated = rv$is_emulate,
              fit = if (rv$is_emulate) rv$meanfitList else NULL,
              grouped = rv$grouped,
              groups = if (rv$grouped) rv$groups[[2]] else NULL,
              summarized = TRUE)
            
            truth_A <- truth_summarized[["A"]]$area
            truth_B <- truth_summarized[["B"]]$area
          }
          
          if (out_groups$type == "ctsd") {
            
            truth_summarized <- get_true_speed(
              data = rv$simList,
              seed = rv$seedList,
              
              tau_p = rv$tau_p,
              tau_v = rv$tau_v,
              sigma = rv$sigma,
              
              emulated = rv$is_emulate,
              fit = if (rv$is_emulate) rv$meanfitList else NULL,
              
              grouped = rv$grouped,
              groups = if (rv$grouped) rv$groups[[2]] else NULL,
              
              summarized = TRUE)
            
            truth_A <- truth_summarized[["A"]]
            truth_B <- truth_summarized[["B"]]
          }
          
          rv$metaEst_groups <<- rbind(rv$metaEst_groups, data.frame(
            type = out_groups$type,
            group = "A",
            lci = tmpA[[1]],
            est = tmpA[[2]],
            uci = tmpA[[3]],
            unit = tmpunitA))
          rv$metaEst_groups <<- rbind(rv$metaEst_groups, data.frame(
            type = out_groups$type,
            group = "B",
            lci = tmpB[[1]],
            est = tmpB[[2]],
            uci = tmpB[[3]],
            unit = tmpunitB))
          
          rv$metaErr_groups <<- rbind(rv$metaErr_groups, data.frame(
            type = out_groups$type,
            group = "A",
            lci = ((tmpA[[1]] %#% tmpunitB) - truth_A) / truth_A,
            est = ((tmpA[[2]] %#% tmpunitB) - truth_A) / truth_A,
            uci = ((tmpA[[3]] %#% tmpunitB) - truth_A) / truth_A))
          rv$metaErr_groups <<- rbind(rv$metaErr_groups, data.frame(
            type = out_groups$type,
            group = "B",
            lci = ((tmpB[[1]] %#% tmpunitB) - truth_B) / truth_B,
            est = ((tmpB[[2]] %#% tmpunitB) - truth_B) / truth_B,
            uci = ((tmpB[[3]] %#% tmpunitB) - truth_B) / truth_B))
        }
      }
      
      shinyjs::show(id = "metaBox_outputs")
      if ("Home range" %in% rv$which_question) {
        shinyjs::show(id = "metaBox_err_hr")
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        shinyjs::show(id = "metaBox_err_speed")
      }
      
      if (length(rv$which_question) == 1) {
        if ("Home range" %in% rv$which_question) {
          shinyjs::hide(id = "metaBox_err_speed")
        }
        
        if ("Speed & distance" %in% rv$which_question) {
          shinyjs::hide(id = "metaBox_err_hr")
        }
      }
      
      msg_log(
        style = "success",
        message = paste0("Meta-analyses ",
                         msg_success("completed"), "."),
        run_time = difftime(Sys.time(), start, units = "sec"))
      
      shinyFeedback::showToast(
        type = "success",
        message = "Meta-analyses completed!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"
        )
      )
      
      rv$is_meta <- TRUE
      rv$metaList <- metaList
      if (rv$grouped) rv$metaList_groups[[3]] <- TRUE
      
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      bindEvent(input$run_meta)
    
    ## Run meta-analyses (for each set of simulations): -------------------

    get_meta_outputs <- reactive({
      
      start <- Sys.time()
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = tagList(span(
          style = "font-size: 18px;",
          span("Running", style = "color: #797979;"),
          wrap_none(
            span("meta-analyses (permutations)", class = "cl-sea"),
            span("...", style = "color: #797979;")))))
      
      msg_log(
        style = "warning",
        message = paste0("Meta-analyses (permutations) ",
                         msg_warning("in progress"), "..."))
      
      get_analysis <- c()
      if ("Home range" %in% rv$which_question) {
        req(rv$akdeList)
        req(length(rv$akdeList) > 0)
        req(length(rv$simList) == length(rv$akdeList))
        get_analysis <- c(get_analysis, "hr")
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        req(rv$ctsdList)
        req(length(rv$ctsdList) > 0)
        req(length(rv$simList) == length(rv$ctsdList))
        get_analysis <- c(get_analysis, "ctsd")
      }
      
      tmp <- run_meta_permutations(
        rv, set_target = get_analysis,
        subpop = rv$grouped,
        random = FALSE, 
        trace = FALSE)
      
      msg_log(
        style = "success",
        message = paste0("Meta-analyses (permutations) ",
                         msg_success("completed"), "."),
        run_time = difftime(Sys.time(), start, units = "sec"))
      
      shinybusy::remove_modal_spinner()
      return(tmp)
      
    }) %>% # end of reactive, "get_meta_outputs",
      bindCache(c(
        rv$which_question, 
        rv$simList, 
        rv$simfitList))
    
    
    observe({
      req(rv$active_tab == 'meta')
      req(rv$which_question,
          !is.null(rv$grouped),
          rv$set_analysis)
      req(length(rv$simList) > 2)
      
      shinyjs::hide(id = "metaBox_summary")
      rv$random <- FALSE
      
      tmp <- get_meta_outputs()
      rv$meta_tbl <<- dplyr::bind_rows(rv$meta_tbl, tmp)
      rv$meta_tbl <- dplyr::distinct(rv$meta_tbl)
      rv$meta_tbl_resample <- NULL
      
      shinyjs::show(id = "metaBox_summary")
      
    }) %>% # end of observe,
      bindEvent(input$run_meta)
    
    
    get_meta_permutation_outputs <- reactive({
      
      start <- Sys.time()
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = tagList(span(
          style = "font-size: 18px;",
          span("Resampling individuals \n",
               "for", style = "color: #797979;"),
          wrap_none(span("meta-analyses", class = "cl-sea"),
                    span("...", style = "color: #797979;")))))
      
      msg_log(
        style = "warning",
        message = paste0("Resampling ",
                         msg_warning("in progress"), "..."))
      
      get_analysis <- c()
      if ("Home range" %in% rv$which_question) {
        req(rv$akdeList)
        req(length(rv$akdeList) > 0)
        req(length(rv$simList) == length(rv$akdeList))
        get_analysis <- c(get_analysis, "hr")
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        req(rv$ctsdList)
        req(length(rv$ctsdList) > 0)
        req(length(rv$simList) == length(rv$ctsdList))
        get_analysis <- c(get_analysis, "ctsd")
      }
      
      tmp <- run_meta_permutations(
        rv, set_target = get_analysis,
        subpop = rv$grouped,
        random = TRUE, max_samples = input$n_resamples,
        trace = TRUE)
      
      msg_log(
        style = "success",
        message = paste0("Resampling ", msg_success("completed"), "."),
        run_time = difftime(Sys.time(), start, units = "sec"))
      
      shinyFeedback::showToast(
        type = "success",
        message = paste("Resampling completed!"),
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
      return(tmp)
      
    }) %>% # end of reactive, "get_meta_permutation_outputs",
      bindCache(c(
        rv$which_question, 
        rv$simList, 
        rv$simfitList,
        input$n_resamples))
    
    observe({
      req(rv$active_tab == 'meta')
      req(rv$which_question,
          !is.null(rv$grouped),
          rv$set_analysis,
          rv$meta_tbl)
      
      rv$random <- TRUE
      n_resamples <- input$n_resamples
      # rv$meta$n_resample <- rv$meta$n_resample + input$n_resamples
      
      tmp <- get_meta_permutation_outputs()
      rv$meta_tbl_resample <<- rbind(rv$meta_tbl_resample,
                                     dplyr::distinct(tmp))
      

    }) %>% # end of observe,
      bindEvent(input$run_meta_resample)
    
    # PLOTS ---------------------------------------------------------------
    ## Rendering meta plot at the individual-level: -----------------------
    
    observe({
      if (length(rv$which_question) == 2)
        shinyjs::show(id = "metaOutput_target") else
          shinyjs::hide(id = "metaOutput_target")
      
    }) # end of observe
    
    output$metaPlot_all <- ggiraph::renderGirafe({
      req(rv$which_question, rv$simList, rv$simfitList,
          rv$truth, !is.null(rv$is_emulate))
      req(length(rv$simList) == length(rv$simfitList))
      req(length(rv$simfitList) > 1)
      
      if (length(rv$which_question) == 2) {
        req(rv$set_analysis)
        set_analysis <- rv$set_analysis
      } else {
        set_analysis <- switch(rv$which_question,
                               "Home range" = "hr",
                               "Speed & distance" = "ctsd")
      }
      
      proceed <- TRUE
      if (set_analysis == "hr") {
        N1 <- extract_dof(rv$simfitList, "area")
        if (all(N1 < 0.001)) proceed <- NULL
      }
      
      if (set_analysis == "ctsd") {
        N2 <- extract_dof(rv$simfitList, "speed")
        if (all(N2 < 0.001)) proceed <- NULL
      }
      
      req(proceed)
      
      if (set_analysis == "hr") {
        req(rv$akdeList)
        outList <- rv$akdeList
        x_label <- "Home range area (in "
        
        truth_summarized <- get_true_hr(
          sigma = rv$sigma,
          emulated = rv$is_emulate,
          fit = if (rv$is_emulate) rv$meanfitList else NULL,
          grouped = rv$grouped,
          groups = if (rv$grouped) rv$groups[[2]] else NULL,
          summarized = TRUE)
        
        truth <- truth_summarized[["All"]]$area
        if (rv$grouped) {
          truth_A <- truth_summarized[["A"]]$area
          truth_B <- truth_summarized[["B"]]$area
        }
      }
      
      if (set_analysis == "ctsd") {
        req(rv$ctsdList)
        outList <- rv$ctsdList
        x_label <- "Movement speed (in "
        
        truth_summarized <- get_true_speed(
          data = rv$simList,
          seed = rv$seedList,
          
          tau_p = rv$tau_p,
          tau_v = rv$tau_v,
          sigma = rv$sigma,
          
          emulated = rv$is_emulate,
          fit = if (rv$is_emulate) rv$meanfitList else NULL,
          
          grouped = rv$grouped,
          groups = if (rv$grouped) rv$groups[[2]] else NULL,
          
          summarized = TRUE)
        
        truth <- truth_summarized[["All"]]
        if (rv$grouped) {
          truth_A <- truth_summarized[["A"]]
          truth_B <- truth_summarized[["B"]]
        }
      }
      
      outList[sapply(outList, is.null)] <- NULL # drop NULLs
      out <- extract_outputs(
        outList,
        name = set_analysis,
        groups = if (rv$grouped) rv$groups[[2]] else NULL,
        si_units = FALSE, 
        meta = TRUE)
      req(out)
      
      if (rv$grouped) x_shape <- c(15, 16, 17)
      else x_shape <- c(15, 16)
      
      truth <- out$unit[[1]] %#% truth
      
      if (rv$is_emulate) {
        if (set_analysis == "hr") {
          truthList <- get_true_hr(
            data = rv$simList,
            seed = rv$seedList,
            sigma = rv$sigma,
            
            emulated = rv$is_emulate,
            fit = if (rv$is_emulate) rv$meanfitList else NULL,
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL)
          
          true_value <- lapply(truthList, \(x) out$unit[[1]] %#% x$area)
          names(true_value) <- names(truthList)
        }
        
        if (set_analysis == "ctsd") {
          true_value <- get_true_speed(
            data = rv$simList,
            seed = rv$seedList,
            sigma = rv$sigma,
            
            emulated = rv$is_emulate,
            fit = if (rv$is_emulate) rv$meanfitList else NULL,
            grouped = rv$grouped,
            groups = if (rv$grouped) rv$groups[[2]] else NULL)
          
          tmp_names <- names(true_value)
          tmp_list <- list()
          for (x in seq_along(true_value)) {
            tmp_list[[x]] <- out$unit[[1]] %#% true_value[[x]]
          }
          true_value <- tmp_list
          names(true_value) <- tmp_names
        }
        
        out_truth <- out
        for (id in names(true_value)) {
          out_truth$true_value[out$id == id] <- true_value[[id]]
        }
      }
      
      if (rv$grouped) {
        x_color <- c("black", "#77b131", "#009da0")
        truth_A <- out$unit[[1]] %#% truth_A
        truth_B <- out$unit[[1]] %#% truth_B
        out$group <- factor(out$group,
                            levels = c("All", "A", "B"))
      } else {
        x_color <- c("black", "#009da0")
      }
      
      f <- .3
      x_label <- paste0(x_label, out$unit[[1]], ")")
      p.all <- out %>%
        ggplot2::ggplot(
          ggplot2::aes(x = est,
                       y = id,
                       group = as.factor(group),
                       color = as.factor(group))) +
        
        ggplot2::geom_vline(xintercept = truth,
                            color = "black",
                            linewidth = 0.8,
                            linetype = "solid") +
        
        { if (rv$grouped)
          ggplot2::geom_vline(xintercept = truth_A,
                              color = "#77b131",
                              linewidth = 0.7,
                              linetype = "solid")
        } +
        { if (rv$grouped)
          ggplot2::geom_vline(xintercept = truth_B,
                              color = pal$sea,
                              linewidth = 0.7,
                              linetype = "solid")
        } +
        
        ggplot2::geom_point(
          ggplot2::aes(shape = as.factor(group)),
          size = 3) +
        
        ggplot2::geom_linerange(
          ggplot2::aes(xmin = lci, xmax = uci),
          linewidth = 2,
          alpha = 0.5) +
        
        ggplot2::facet_grid(subject ~ ., 
                            switch = "y",
                            scales = "free_y",
                            space = "free_y") +
        
        { if (rv$is_emulate)
          ggplot2::geom_point(
            data = out_truth,
            mapping = ggplot2::aes(x = as.numeric(true_value),
                                   y = id,
                                   group = as.factor(group),
                                   fill = "True area"),
            size = 3,
            shape = 4) } +
        
        { if (rv$is_emulate)
        ggplot2::scale_fill_manual(
          "Truth:", values = c("True area" = "black")) } +
        
        ggplot2::scale_color_manual("Group:", values = x_color) +
        ggplot2::scale_shape_manual("Group:", values = x_shape) +
        ggplot2::labs(x = x_label, y = "") +
        
        theme_movedesign(font_available = rv$is_font) +
        ggplot2::theme(
          strip.placement = "outside",
          legend.position = "none",
          axis.text.y = ggplot2::element_blank()) %>%
        suppressWarnings() +
        ggplot2::theme(legend.position = "bottom")
      
      ggiraph::girafe(
        ggobj = suppressWarnings(p.all),
        width_svg = 5.5, height_svg = max(2, length(rv$simList) * f),
        options = list(
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_tooltip(
            opacity = 1,
            use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill: #1279BF;",
                        "stroke: #1279BF;",
                        "cursor: pointer;")))) %>%
        suppressWarnings()
      
    }) # end of renderGirafe, "metaPlot_all"
    
    ## Rendering meta plot at the group-level (if available): -------------
    
    output$metaPlot_groups <- ggiraph::renderGirafe({
      req(rv$grouped, rv$truth, !is.null(rv$metaList_groups))
      req(rv$metaEst, rv$metaList_groups[[3]], rv$metaEst_groups)
      
      if (length(rv$which_question) == 2) {
        req(rv$set_analysis)
        set_analysis <- rv$set_analysis
      } else {
        set_analysis <- switch(rv$which_question,
                               "Home range" = "hr",
                               "Speed & distance" = "ctsd")
      }
      
      # out <- rbind(rv$metaErr, rv$metaErr_groups) %>% 
      #   dplyr::filter(type == set_analysis)
      #  out$truth <- rep(0, nrow(out))
      
      out <- rbind(rv$metaEst, rv$metaEst_groups) %>%
        dplyr::filter(type == set_analysis)
      
      # x_label <- "Error (%)"
      if (set_analysis == "hr") {
        req(rv$akdeList)
        outList <- rv$akdeList
        name <- "area"
        x_label <- "Home range area (in "
        
        truth_summarized <- get_true_hr(
          sigma = rv$sigma,
          emulated = rv$is_emulate,
          fit = if (rv$is_emulate) rv$meanfitList else NULL,
          grouped = rv$grouped,
          groups = rv$groups[[2]],
          summarized = TRUE)
        
        truth <- out$unit[[1]] %#% truth_summarized[["All"]]$area
        truth_A <- out$unit[[1]] %#% truth_summarized[["A"]]$area
        truth_B <- out$unit[[1]] %#% truth_summarized[["B"]]$area
      }
      
      if (set_analysis == "ctsd") {
        req(rv$ctsdList)
        outList <- rv$ctsdList
        name <- "speed"
        x_label <- "Movement speed (in "
        
        truth_summarized <- get_true_speed(
          data = rv$simList,
          seed = rv$seedList,
          
          tau_p = rv$tau_p,
          tau_v = rv$tau_v,
          sigma = rv$sigma,
          
          emulated = rv$is_emulate,
          fit = if (rv$is_emulate) rv$meanfitList else NULL,
          
          grouped = rv$grouped,
          groups = rv$groups[[2]],
          
          summarized = TRUE)
        
        truth <- out$unit[[1]] %#% truth_summarized[["All"]]
        truth_A <- out$unit[[1]] %#% truth_summarized[["A"]]
        truth_B <- out$unit[[1]] %#% truth_summarized[["B"]]
      }
      
      out <- out %>%
        dplyr::mutate(
          group = dplyr::recode(
            group, "A" = "Group A", "B" = "Group B")) %>%
        dplyr::mutate(
          group = factor(group, levels = c("All", "Group A", "Group B")))
      
      x_label <- paste0(x_label, as.character(out$unit[1]), ")")
      x_color <- c("black", "#77b131", pal$sea)
      x_shape <- c(15,16,17)
      x_linetype <- c("solid", "dotted", "dotted")
      f <- .65
      
      p.groups <- out %>% 
        ggplot2::ggplot(
          ggplot2::aes(x = est,
                       y = id, 
                       shape = as.factor(group),
                       group = as.factor(group),
                       color = as.factor(group))) +
        
        ggplot2::geom_vline(
          xintercept = truth,
          color = "black",
          linewidth = 0.8,
          linetype = "solid") +
        
        ggplot2::geom_vline(xintercept = truth_A,
                            color = "#77b131",
                            linewidth = 0.7,
                            linetype = "solid") +
        ggplot2::geom_vline(xintercept = truth_B,
                            color = pal$sea,
                            linewidth = 0.7,
                            linetype = "solid") +
        
        ggplot2::geom_point(size = 4.5) +
        ggplot2::geom_linerange(ggplot2::aes(xmin = lci,
                                             xmax = uci),
                                linewidth = 3,
                                alpha = 0.5) +
        
        ggplot2::facet_grid(group ~ .,
                            switch = "y",
                            scales = "fixed",
                            space = "fixed") +
        
        ggplot2::labs(x = x_label, y = "") +
        
        # ggplot2::scale_x_continuous(labels = scales::percent) +
        ggplot2::scale_color_manual("Group:", values = x_color) +
        ggplot2::scale_shape_manual("Group:", values = x_shape) +
        
        theme_movedesign(font_available = rv$is_font) +
        ggplot2::theme(
          panel.spacing = ggplot2::unit(0, "lines"),
          strip.placement = "outside",
          legend.position = "none",
          axis.text.y = ggplot2::element_blank()) %>%
        suppressWarnings() +
        ggplot2::theme(legend.position = "bottom")
      
      ggiraph::girafe(
        ggobj = p.groups,
        width_svg = 5.5, height_svg = 3,
        options = list(
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_tooltip(
            opacity = 1,
            use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill: #1279BF;",
                        "stroke: #1279BF;",
                        "cursor: pointer;")))) %>%
        suppressWarnings()
      
    }) # end of renderGirafe, "metaPlot_groups"
    
    ## Rendering plots for optimal population sample sizes: ---------------
    
    ## Moved to mod_viz_meta.R
    
    ## Rendering error plot of optimal search outputs (ratios): -----------
    
    output$metaPlot_m_ratio_optimal <- ggiraph::renderGirafe({
      req(rv$which_meta == "compare", rv$grouped)
      req(rv$meta_tbl, rv$which_m, rv$which_meta,
          rv$which_question, rv$set_analysis)
      req(length(rv$simList) > 1)
      
      rv$random <- FALSE
      
      plot_title <- NULL
      if (length(rv$which_question) > 1) {
        plot_title <- ifelse(rv$set_analysis == "hr",
                             "For home range:",
                             "For speed & distance:")
      }
      
      pal_values <- c("Yes" = pal$sea,
                      "Near" = pal$grn,
                      "No" = pal$dgr)
      
      get_analysis <- c()
      if ("Home range" %in% rv$which_question) {
        req(rv$akdeList)
        req(length(rv$akdeList) > 0)
        req(length(rv$simList) == length(rv$akdeList))
        get_analysis <- c(get_analysis, "hr")
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        req(rv$ctsdList)
        req(length(rv$ctsdList) > 0)
        req(length(rv$simList) == length(rv$ctsdList))
        get_analysis <- c(get_analysis, "ctsd")
      }
      
      truth <- c()
      truthList <- .get_expected_values(rv, get_analysis)
      
      if (rv$set_analysis == "hr") {
        truth[["hr"]] <- truthList[["hr"]][["A"]]$area/
          truthList[["hr"]][["B"]]$area
      }
      
      if (rv$set_analysis == "ctsd") {
        truth[["ctsd"]] <- truthList[["ctsd"]][["A"]]/
          truthList[["ctsd"]][["B"]]
      }
      
      out_all <- rv$meta_tbl
      
      req(rv$metaList_groups)
      req(rv$metaList[[rv$set_analysis]])
      dodge_width <- .4
      
      is_subpop <- rv$metaList_groups[["intro"]][[
        rv$set_analysis]]$logs$subpop_detected
      is_final_subpop <- out_all %>%
        dplyr::filter(type == rv$set_analysis) %>% 
        dplyr::filter(group == "All") %>%
        dplyr::pull(subpop_detected)
      is_final_subpop <- ifelse(is_final_subpop == "FALSE", "No", "Yes")
      # Note: this refers to finding subpops within the population.
      
      pal1 <- c("Yes" = pal$sea, "No" = pal$dgr)
      pal2 <- c("Yes" = pal$dgr, "No" = pal$sea)
      pal_values <- if(is_subpop) pal1 else pal2
      
      color_title <- "Sub-population detected?"
      
      if (rv$random) {
        req(rv$meta_tbl_resample)
        
        out <- rv$meta_tbl_resample %>% 
          dplyr::select(-c(truth, est, lci, uci,
                           error, error_lci, error_uci, group)) %>%
          dplyr::filter(type == rv$set_analysis) %>% 
          dplyr::mutate(m == as.integer(m)) %>% 
          tidyr::drop_na(ratio_est) %>% 
          dplyr::distinct()
        
        max_samples <- max(unique(out$sample))
        
        out_mean <- out %>% 
          dplyr::group_by(type, m) %>% 
          dplyr::summarize(
            n = dplyr::n(),
            ratio_est = mean(ratio_est, na.rm = TRUE),
            ratio_lci = mean(ratio_lci, na.rm = TRUE),
            ratio_uci = mean(ratio_uci, na.rm = TRUE)) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            color = dplyr::case_when(
              (ratio_lci < truth[[rv$set_analysis]] &
                 ratio_uci > truth[[rv$set_analysis]]) ~ "Yes",
              TRUE ~ "No")) %>% 
          quiet() %>% 
          suppressMessages() %>% 
          suppressWarnings()
        
        color_title <- "Sub-population detected?"
        plot_subtitle <- paste(
          "<b>Maximum number of samples:</b>", max_samples)
        
        out_mean$color <- is_final_subpop
        
        p.ratio.optimal <- out_mean %>% 
          ggplot2::ggplot(
            ggplot2::aes(x = as.factor(m),
                         y = ratio_est,
                         fill = m,
                         color = color)) +
          
          ggplot2::geom_hline(
            yintercept = truth[[rv$set_analysis]],
            linewidth = 0.3,
            linetype = "solid") +
          
          ggplot2::geom_jitter(
            data = out,
            mapping = ggplot2::aes(x = as.factor(m),
                                   y = ratio_est,
                                   color = color),
            position = ggplot2::position_jitterdodge(dodge.width = 0.4),
            size = 2, color = "grey80", alpha = 0.9) +
          
          ggplot2::geom_linerange(
            ggplot2::aes(ymin = ratio_lci,
                         ymax = ratio_uci),
            show.legend = TRUE,
            position = ggplot2::position_dodge(width = 0.4),
            linewidth = 2.2, alpha = 0.3) +
          ggplot2::geom_point(
            position = ggplot2::position_dodge(width = 0.4),
            show.legend = TRUE,
            size = 3.5) +
          
          ggplot2::labs(
            title = plot_title,
            subtitle = plot_subtitle,
            x = "<i>Population</i> sample size, <i>m</i>",
            y = "Ratio (Group A / Group B)",
            color = color_title) +
          
          ggplot2::scale_y_continuous(breaks = scales::breaks_pretty()) +
          ggplot2::scale_color_manual(values = pal_values, drop = FALSE) +
          
          theme_movedesign(font_available = rv$is_font) +
          ggplot2::theme(
            legend.position = "bottom",
            plot.title = ggtext::element_markdown(
              size = 15, face = 2, hjust = 1,
              margin = ggplot2::margin(b = 2)),
            plot.subtitle = ggtext::element_markdown(
              size = 14, hjust = 1, margin = ggplot2::margin(b = 15))) +
          ggplot2::guides(fill = "none")
        
      } else {
        
        out <- out_all %>% 
          dplyr::select(-c(truth, est, lci, uci,
                           error, error_lci, error_uci, group)) %>%
          dplyr::filter(type == rv$set_analysis) %>% 
          dplyr::distinct()
        req(all(!is.na(out$est)))
        req(nrow(out) > 0)
        
        out <- out %>%
          dplyr::group_by(type) %>% 
          dplyr::rowwise() %>%
          dplyr::mutate(
            color = dplyr::case_when(
              (ratio_lci < truth[[rv$set_analysis]] &
                 ratio_uci > truth[[rv$set_analysis]]) ~ "Yes",
              TRUE ~ "No"))
        
        p.ratio.optimal <- out %>%
          ggplot2::ggplot(
            ggplot2::aes(x = as.factor(m),
                         y = ratio_est,
                         color = color)) +
          
          ggplot2::geom_hline(
            yintercept = truth[[rv$set_analysis]],
            linewidth = 0.3,
            linetype = "solid") +
          ggplot2::geom_point(
            size = 4,
            position = ggplot2::position_dodge(width = dodge_width)) +
          ggplot2::geom_linerange(
            ggplot2::aes(ymin = ratio_lci,
                         ymax = ratio_uci),
            position = ggplot2::position_dodge(width = dodge_width)) +
          
          ggplot2::labs(
            title = plot_title,
            x = "Number of individuals",
            y = "Ratio (Group A / Group B)",
            color = color_title) +
          
          ggplot2::scale_y_continuous(breaks = scales::breaks_pretty()) +
          ggplot2::scale_color_manual(values = pal_values) +
          # ggplot2::scale_shape_manual("Group:", values = c(16,17)) +
          theme_movedesign(font_available = rv$is_font) +
          ggplot2::theme(
            legend.position = "bottom",
            plot.title = ggtext::element_markdown(
              size = 14, hjust = 1, margin = ggplot2::margin(b = 15)))
        
      }
      
      ggiraph::girafe(
        ggobj = p.ratio.optimal,
        width_svg = 5.5, height_svg = 4,
        options = list(
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_tooltip(
            opacity = 1,
            use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill: #1279BF;",
                        "stroke: #1279BF;",
                        "cursor: pointer;")))) %>%
        suppressWarnings()
      
    }) %>% # end of renderGirafe, "metaPlot_m_ratio_optimal"
      bindEvent(list(input$run_meta,
                     input$run_meta_resample,
                     rv$set_analysis))
    
    # TABLES --------------------------------------------------------------
    ## Rendering meta-analyses outputs: -----------------------------------
    
    output$metaTable_all <- reactable::renderReactable({
      req(rv$metaList, rv$sigma)
      req(length(rv$metaList) > 0)
      
      if ("Home range" %in% rv$which_question) req(rv$akdeList)
      if ("Speed & distance" %in% rv$which_question) req(rv$ctsdList)
      
      if (length(rv$which_question) == 2) {
        req(rv$set_analysis)
        req(length(rv$metaList) == 2)
        set_analysis <- rv$set_analysis
      } else {
        set_analysis <- switch(rv$which_question,
                               "Home range" = "hr",
                               "Speed & distance" = "ctsd")
      }
      
      dt_meta <- as.data.frame(rv$metaList[[set_analysis]]$meta)
      tmpunit <- extract_units(rownames(dt_meta[1, ]))
      rownames(dt_meta)[[1]] <- paste0("Mean (", tmpunit, ")")
      
      # if (rv$grouped == rv$metaList[[1]]$logs$subpop_detected) {
      #   format_cov <- function(value, col) {
      #     color <- case_when(
      #       abs(value) == 0 ~ "#dd4b39",
      #       TRUE ~ "#000000")
      #     list(color = color)
      #   }
      # } else {
      #   format_cov <- function(value, col) {
      #     color <- case_when(
      #       abs(value) == 0 ~ "#000000",
      #       TRUE ~ "#dd4b39")
      #     list(color = color)
      #   }
      # }
      
      reactable::reactable(
        dt_meta,
        searchable = FALSE,
        highlight = FALSE,
        compact = FALSE,
        striped = TRUE,
      
        defaultColDef = reactable::colDef(
          headerClass = "rtable_header", align = "left",
            minWidth = 50),
        columns = list(
          low = reactable::colDef(
            minWidth = 40, name = "Low",
            # style = format_cov,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 3)),
          est = reactable::colDef(
            minWidth = 40, name = "Est",
            # style = format_cov,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 3)),
          high = reactable::colDef(
            minWidth = 40, name = "High",
            # style = format_cov,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 3))
        ))
      
    }) # end of renderReactable, "metaTable_all"
    
    ## Rendering meta-analyses outputs (for groups): ----------------------
    
    output$metaTable_groups <- reactable::renderReactable({
      req(rv$metaList_groups[[3]],
          rv$set_analysis,
          input$metaInput_mod)
      
      out <- rv$metaList_groups[[2]][[rv$set_analysis]]$mods
      req(length(out) == 4)
      
      dt_out <- out[[input$metaInput_mod]]
      req(dt_out)
      
      reactable::reactable(
        dt_out,
        searchable = FALSE,
        highlight = FALSE,
        compact = FALSE,
        striped = TRUE,
        
        defaultColDef = reactable::colDef(
          headerClass = "rtable_header", align = "left"),
        columns = list(
          model = reactable::colDef(name = "Model"),
          delta_AICc = reactable::colDef(
            name = "\u0394AICc",
            minWidth = 80,
            style = function(value) {
              list(color = case_when(
                abs(value) > 2 ~ "black",
                TRUE ~ "#dd4b39"))},
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 3))
        ))
      
    }) # end of renderReactable, "metaTable_groups"
    
    ## Rendering simulation summary table: --------------------------------
    
    # output$metaTable_m <- reactable::renderReactable({
    #   req(rv$which_question)
    #   
    #   if (length(rv$which_question) == 2) {
    #     req(rv$set_analysis)
    #     set_analysis <- rv$set_analysis
    #   } else {
    #     set_analysis <- switch(rv$which_question,
    #                            "Home range" = "hr",
    #                            "Speed & distance" = "ctsd")
    #   }
    #   
    #   if (set_analysis == "hr") {
    #     req(rv$hr$tbl)
    #     out_dt <- rv$hr$tbl[, -1]
    #   }
    #   
    #   if (set_analysis == "ctsd") {
    #     req(rv$sd$tbl)
    #     out_dt <- rv$sd$tbl[, -1]
    #   }
    #   
    #   out_dt <- dplyr::select(out_dt, -data)
    #   
    #   nms <- list(
    #     group = "Group:",
    #     taup = "\u03C4\u209A",
    #     dur = "Duration",
    #     dti = "Interval",
    #     n = "n",
    #     N1 = "N (area)",
    #     area = "Area",
    #     area_err = "Error",
    #     area_err_min = "Error (95% LCI)",
    #     area_err_max = "Error (95% UCI)")
    #   
    #   reactable::reactable(
    #     data = out_dt,
    #     compact = TRUE,
    #     highlight = TRUE,
    #     striped = TRUE,
    #     
    #     defaultPageSize = 5,
    #     paginationType = "jump",
    #     showPageSizeOptions = TRUE,
    #     pageSizeOptions = c(5, 10, 20),
    #     showPageInfo = FALSE,
    #     
    #     defaultColDef =
    #       reactable::colDef(
    #         headerClass = "rtable_header",
    #         align = "center",
    #         minWidth = 60),
    #     
    #     columns = list(
    #       data = reactable::colDef(
    #         name = nms[["data"]]),
    #       taup = reactable::colDef(
    #         minWidth = 80, name = nms[["taup"]],
    #         style = list(fontWeight = "bold")),
    #       dur = reactable::colDef(
    #         minWidth = 80, name = nms[["dur"]],
    #         style = list(fontWeight = "bold")),
    #       dti = reactable::colDef(
    #         minWidth = 80, name = nms[["dti"]],
    #         style = list(fontWeight = "bold")),
    #       n = reactable::colDef(
    #         name = nms[["n"]],
    #         style = format_num,
    #         format = reactable::colFormat(separators = TRUE,
    #                                       digits = 0)),
    #       N1 = reactable::colDef(
    #         minWidth = 80, name = nms[["N1"]],
    #         style = format_num,
    #         format = reactable::colFormat(separators = TRUE,
    #                                       digits = 1)),
    #       area = reactable::colDef(
    #         minWidth = 80, name = nms[["area"]]),
    #       
    #       area_err = reactable::colDef(
    #         minWidth = 80, name = nms[["area_err"]],
    #         style = format_perc,
    #         format = reactable::colFormat(percent = TRUE,
    #                                       digits = 1)),
    #       area_err_min = reactable::colDef(
    #         minWidth = 80, name = nms[["area_err_min"]],
    #         style = format_perc,
    #         format = reactable::colFormat(percent = TRUE,
    #                                       digits = 1)),
    #       area_err_max = reactable::colDef(
    #         minWidth = 80, name = nms[["area_err_max"]],
    #         style = format_perc,
    #         format = reactable::colFormat(percent = TRUE,
    #                                       digits = 1))
    #     ))
    #   
    # }) # end of renderReactable, "metaTable_m"
    
    ## Rendering meta-analyses outputs (permutations): --------------------
    
    output$metaTable_m_optimal <- reactable::renderReactable({
      req(rv$which_question, rv$meta_tbl)
      
      dt_meta <- rv$meta_tbl %>%
        dplyr::filter(type == rv$set_analysis) %>% 
        dplyr::select(-overlaps, -type) %>%
        dplyr::mutate(subpop = as.logical(subpop_detected)) %>% 
        dplyr::select(m, error_lci, error, error_uci, group, subpop)
      
      dt_meta$subpop <- ifelse(dt_meta$subpop, "Yes", "No")
      
      nms <- list(
        m = "m",
        lci = "95% LCI",
        est = "Est",
        uci = "95% UCI",
        subpop = "Detected?",
        group = "Group"
      )
      
      colgroups <- list(
        reactable::colGroup(
          name = "Error", columns = c("error_lci", 
                                      "error",
                                      "error_uci")))
      
      reactable::reactable(
        data = dt_meta,
        compact = TRUE,
        highlight = TRUE,
        striped = TRUE,
        
        defaultPageSize = 5,
        paginationType = "jump",
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 20),
        showPageInfo = FALSE,
        
        defaultSorted = list(m = "asc", group = "asc"),
        defaultColDef = reactable::colDef(
          headerClass = "rtable_header",
          align = "center",
          minWidth = 60),
        
        columns = list(
          m = reactable::colDef(
            name = nms[["m"]]),
          error_lci = reactable::colDef(
            name = nms[["lci"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 1)),
          error = reactable::colDef(
            name = nms[["est"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 1)),
          error_uci = reactable::colDef(
            name = nms[["uci"]],
            style = format_perc,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 1)),
          subpop = reactable::colDef(
            name = nms[["subpop"]]),
          group = reactable::colDef(
            name = nms[["group"]])),
        
        columnGroups = colgroups
        
      ) # end of reactable
      
    }) %>% # end of renderReactable, "metaTable_m_optimal"
      bindEvent(list(input$add_meta_table, 
                     rv$ctsdList,
                     rv$akdeList,
                     rv$meta_tbl,
                     rv$set_analysis))
    
    # BLOCKS --------------------------------------------------------------
    ## Outputs: -----------------------------------------------------------
    
    observe({
      req(rv$metaErr,
          "Home range" %in% rv$which_question)
      
      mod_blocks_server(
        id = "metaBlock_hr",
        rv = rv, type = "hr", name = "metaErr")
      
    }) # end of observe
    
    output$metaBlock_hr_ratio <- shiny::renderUI({
      req(rv$grouped, rv$metaList_groups[[3]])
      
      meta <- rv$metaList_groups[[2]][["hr"]]
      req(!is.null(meta))
      
      observed_ratio <- .get_ratios(meta)
      observed_ratio
      
      return(tagList(
        p(style = "margin-top: 10px;"),
        parBlock(
          icon = "divide",
          header = "Ratio",
          value = paste0(round(observed_ratio$est, 1), ":1"),
          subtitle = paste0(
            scales::label_comma(.1)(observed_ratio$lci),
            ":1 \u2014 ", scales::label_comma(.1)
            (observed_ratio$uci), ":1"))
      ))
      
    }) # end of renderUI, "metaBlock_hr_ratio"
    
    observe({
      req(rv$metaErr,
          "Speed & distance" %in% rv$which_question)
      
      tmp <- dplyr::filter(rv$metaErr, type == "ctsd")
      req(nrow(tmp) > 0)
      
      mod_blocks_server(
        id = "metaBlock_speed",
        rv = rv, type = "ctsd", name = "metaErr")
      
    }) # end of observe
    
    output$metaBlock_speed_ratio <- shiny::renderUI({
      req(rv$grouped, rv$metaList_groups[[3]])
      
      meta <- rv$metaList_groups[[2]][["ctsd"]]
      req(!is.null(meta))
      
      observed_ratio <- .get_ratios(meta)
      observed_ratio
      
      return(tagList(
        p(style = "margin-top: 10px;"),
        parBlock(
          icon = "divide",
          header = "Ratio",
          value = paste0(round(observed_ratio$est, 1), ":1"),
          subtitle = paste0(
            scales::label_comma(.1)(observed_ratio$lci),
            ":1 \u2014 ", scales::label_comma(.1)
            (observed_ratio$uci), ":1"))
      ))
      
    }) # end of renderUI, "metaBlock_speed_ratio"
    
    # HELP MODALS ---------------------------------------------------------
    ## Help modal (resample): ---------------------------------------------
    
    # observe({
    #   
    #   shiny::showModal(
    #     shiny::modalDialog(
    #       title = h4(span("Resampling", class = "cl-sea"),
    #                  "individuals:"),
    #       
    #       fluidRow(
    #         style = paste("margin-right: 20px;",
    #                       "margin-left: 20px;"),
    #         
    #         # h4(style = "margin-top: 30px;", "For more information:"),
    #         # 
    #         # p(style = "font-family: var(--monosans);",
    #         #   "- Noonan, M. J., Fleming, C. H., Akre, T. S.,",
    #         #   "Drescher-Lehman, J., Gurarie, E., Harrison, A. L.,",
    #         #   "Kays, R. & Calabrese, J. M. (2019). Scale-insensitive",
    #         #   "estimation of speed and distance traveled from animal",
    #         #   "tracking data. Movement Ecology, 7(1), 1-15.")
    #         
    #         p("TBA")
    #         
    #       ), # end of fluidRow
    #       
    #       footer = modalButton("Dismiss"),
    #       size = "m")) # end of modal
    #   
    # }) %>% # end of observe,
    #   bindEvent(input$metaHelp_resample)
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_tab_meta_ui("tab_meta_1")
    
## To be copied in the server
# mod_tab_meta_server("tab_meta_1")
