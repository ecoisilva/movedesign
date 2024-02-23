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
                "If meta-analyses are your goal,", br(),
                "then click the",
                icon("paper-plane", class = "cl-mdn"),
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
              )),
            
            br(),
            shinyWidgets::radioGroupButtons(
              inputId = ns("metaInput_type"),
              label = "Show analyses:",
              choices = c(
                "Home range" = "hr",
                "Speed & distance" = "ctsd"),
              selected = "hr",
              checkIcon = list(yes = icon("circle-check")),
              direction = "vertical",
              justified = TRUE)
            
          ), # end of box // hrBox_sizes
          
          # Error: --------------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Home range:", class = "ttl-box"),
            id = ns("metaBox_err_hr"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            mod_blocks_ui(ns("metaBlock_hr"))
            
          ), # end of box // metaBox_err_hr
          
          shinydashboardPlus::box(
            title = span("Speed:", class = "ttl-box"),
            id = ns("metaBox_err_speed"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            mod_blocks_ui(ns("metaBlock_speed"))
            
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
            title = span("Tables:", class = "ttl-box"),
            id = ns("metaBox_summary"),
            width = NULL,
            solidHeader = FALSE,
            
            # tabsetPanel(
            #   id = ns("metaTabs_tables"),
            #   
            #   tabPanel(
            #     value = ns("metaPanel_estimates"),
            #     title = tagList(
            #       icon("filter", class = "cl-sea"),
            #       span("Individual", class = "ttl-panel")
            #     ),
            #     
            #     p(style = "margin-top: 10px;"),
            #     reactable::reactableOutput(ns("metaTable_m"))
            #     
            #   ), # end of panels (1 out of 2)
            #   
            #   tabPanel(
            #     value = ns("metaPanel_groups"),
            #     title = tagList(
            #       icon("paw", class = "cl-sea"),
            #       span("Dataset", class = "ttl-panel")
            #     ),
            
            p(style = "margin-top: 10px;"),
            div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-7",
                ggiraph::girafeOutput(
                  outputId = ns("metaPlot_m_optimal"),
                  width = "100%", height = "100%")),
            
            div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-5",
                reactable::reactableOutput(
                  ns("metaTable_m_optimal")))
            
            # ) # end of panels (2 out of 2)
            # ) # end of tabs // metaTabs_tables
            
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
    
    observe({
      
      if (length(rv$which_question) == 2) {
        req(input$metaInput_type)
        rv$set_analysis <- input$metaInput_type
      }
      
    }) # end of observe
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide elements at the start: ----------------------------------------
    
    shinyjs::hide(id = "txt_ratio")
    shinyjs::hide(id = "txt_ratio_label")
    
    boxnames <- c("simulations",
                  "outputs",
                  "summary",
                  "err_hr",
                  "err_speed")
    
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("metaBox_", boxnames[i]))
    }
    
    ## Update based on workflow: ------------------------------------------
    
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
      req(rv$metaList_groups, rv$set_analysis)
    
      out <- rv$metaList_groups[[rv$set_analysis]]$mods
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
      if (!is.null(rv$metaList_groups))
        showTab(inputId = "metaTabs_outputs",
                target = ns("metaPanel_groups"))
      
    }) # end of observe
    
    
    output$nsims_total <- renderText({
      if (!is.null(rv$simList)) return(length(rv$simList))
      else return(0)
    }) # end of renderText, "nsims_total"
    
    
    ## Render new text (for effect size): ---------------------------------
    
    output$txt_hr_ratio <- renderText({
      req("compare" %in% rv$which_meta, rv$ratio[["hr"]])
      
      out_txt <- NULL
      var <- "home range area"
      diff <- c("smaller", "larger")
      
      if (rv$ratio[["hr"]] == 1) out_txt <- paste0(
        "Group A's ", var, " is equal to Group B's.")
      else if (rv$ratio[["hr"]] < 1) out_txt <- paste0(
        "Group A's ", var, " is ",
        round(abs(100 - rv$ratio[["hr"]] * 100), 1),
        "% ", diff[1], " than Group B's.")
      else if (rv$ratio[["hr"]] > 1) out_txt <- paste0(
        "Group A's ", var, " area is ",
        round(abs(100 - rv$ratio[["hr"]] * 100), 1),
        "% ", diff[2], " than Group B's.")
      
      return(out_txt)
      
    }) # end of renderText, "txt_hr_ratio"
    
    output$txt_sd_ratio <- renderText({
      req("compare" %in% rv$which_meta, rv$ratio[["ctsd"]])
      
      out_txt <- NULL
      var <- "speed"
      diff <- c("slower", "faster")
      
      if (rv$ratio[["ctsd"]] == 1) out_txt <- paste0(
        "Group A's ", var, " is equal to Group B's.")
      else if (rv$ratio[["ctsd"]] < 1) out_txt <- paste0(
        "Group A's ", var, " is ",
        round(abs(100 - rv$ratio[["ctsd"]] * 100), 1),
        "% ", diff[1], " than Group B's.")
      else if (rv$ratio[["ctsd"]] > 1) out_txt <- paste0(
        "Group A's ", var, " area is ",
        round(abs(100 - rv$ratio[["ctsd"]] * 100), 1),
        "% ", diff[2], " than Group B's.")
      
      return(out_txt)
      
    }) # end of renderText, "txt_sd_ratio"
    
    ## Show/hide summary table: -------------------------------------------
    
    observe({
      if (input$add_meta_table) shinyjs::toggle(id = "metaBox_summary")
      
    }) %>% # end of observe,
      bindEvent(input$add_meta_table)
    
    ## Add notes explaining table outputs: --------------------------------
    
    output$metaUI_legend_all <- renderUI({
      req(rv$grouped, rv$metaList, rv$set_analysis)
      
      out <- rv$metaList[[rv$set_analysis]]
      subpop_detected <- out$logs$subpop_detected
      
      if (rv$grouped && subpop_detected) {
        ui_extra <- tagList(span(
          "Sub-populations were", 
          span("correctly", class = "cl-sea"),
          "detected."))
      } else if (!rv$grouped && !subpop_detected) {
        ui_extra <- tagList(span(
          "A single population was", 
          span("correctly", class = "cl-sea"),
          "detected. No evidence of sub-populations in the",
          "current dataset."))
      } else if (rv$grouped && !subpop_detected) {
        ui_extra <- tagList(span(
          "Sub-populations were", 
          wrap_none(span("not detected", class = "cl-dgr"), ","),
          "even though simulations were based on",
          "two sets of parameters."))
      } else if (!rv$grouped && subpop_detected) {
        ui_extra <- tagList(span(
          "Sub-populations were", 
          wrap_none(span("detected", class = "cl-dgr"), ","),
          "even though simulations were based on",
          "a single set of parameters."))
      }
      
      ui <- tagList(
        p(style = "margin-top: 35px;"),
        span(class = "help-block",
             style = "text-align: justify !important;",
             
             fontawesome::fa("circle-exclamation", fill = pal$dgr),
             span("Note:", class = "help-block-note"),
             "Model selection is performed between the \u03C7\u00B2-IG",
             "population model (with population mean and variance) and",
             "the Dirac-\u03B4 population model (population mean only).",
             br(), ui_extra
        ))
      
      return(ui)
      
    }) # end of renderUI, "metaUI_legend"

    output$metaUI_legend <- renderUI({
      req(rv$metaList_groups)
      
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
      
    }) %>% # end of renderUI, "metaUI_footer",
      bindEvent(input$run_meta)
    
    footer = uiOutput("metaUI_footer")
   
    
    # OPERATIONS ----------------------------------------------------------
    ## Run meta-analyses: -------------------------------------------------
    
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
      req(rv$active_tab == 'meta',
          rv$truth, !rv$is_meta,
          length(rv$simList) > 2)
      
      rv$metaEst <- NULL
      rv$metaErr <- NULL
      rv$metaEst_groups <- NULL
      rv$metaErr_groups <- NULL
      
      # TODO TOCHECK two workflows in the same session
      # lead to repeat values on plots
      
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
      
      metaList <- list()
      if ("Home range" %in% rv$which_question) {
        req(rv$akdeList)
        x <- rv$akdeList
        x[sapply(x, is.null)] <- NULL
        metaList[["hr"]] <- capture_meta(
          x,
          variable = "area",
          units = TRUE, 
          verbose = FALSE,
          plot = FALSE,
          type = "hr")
      }
      
      if ("Speed & distance" %in% rv$which_question) {
        req(rv$ctsdList)
        x <- rv$ctsdList
        x[sapply(x, is.null)] <- NULL
        metaList[["ctsd"]] <- capture_meta(
          x,
          variable = "speed",
          units = TRUE, 
          verbose = FALSE,
          plot = FALSE,
          type = "ctsd")
      }
      
      if (rv$grouped) {
        req(length(rv$groups[[2]]$A) > 0,
            length(rv$groups[[2]]$B) > 0)
        
        outList <- list()
        create_groups <- function(x, groups = rv$groups[[2]]) {
          group_A <- x[groups[["A"]]]
          group_A[sapply(group_A, is.null)] <- NULL
          group_B <- x[groups[["B"]]]
          group_B[sapply(group_B, is.null)] <- NULL
          return(list(A = group_A,
                      B = group_B))
        }
        
        metaList_groups <- list()
        if ("Home range" %in% rv$which_question) {
          req(rv$akdeList)
          outList <- create_groups(rv$akdeList)
          metaList_groups[["hr"]] <- capture_meta(
            outList,
            units = TRUE, 
            verbose = TRUE,
            plot = FALSE,
            type = "hr") %>% 
            suppressMessages() %>% 
            suppressWarnings() %>% 
            quiet()
          
          if (is.null(metaList_groups[["hr"]]))
            msg_log(
              style = "danger",
              message = paste0(
                msg_danger("Home range"), 
                " meta-analyses for groups ",
                msg_danger("failed"), ","),
              detail = "Run more simulations in the appropriate tab.")
        }
        
        if ("Speed & distance" %in% rv$which_question) {
          req(rv$ctsdList)
          outList <- create_groups(rv$ctsdList)
          metaList_groups[["ctsd"]] <- capture_meta(
            outList,
            units = TRUE, 
            verbose = TRUE,
            plot = FALSE,
            type = "ctsd") %>% 
            suppressMessages() %>% 
            suppressWarnings() %>% 
            quiet()
          
          if (is.null(metaList_groups[["ctsd"]]))
            msg_log(
              style = "danger",
              message = paste0(
                msg_danger("Speed"), 
                " meta-analyses for groups ",
                msg_danger("failed"), ","),
              detail = "Run more simulations in the appropriate tab.")
        }
        
      } # end of if (rv$grouped)
      
      for (i in seq_along(metaList)) {
        out <- metaList[[i]]
        
        name <- "mean"
        sum.obj <- out$meta
        nms.obj <- rownames(sum.obj)
        tmp <- sum.obj[grep(name, nms.obj), ]
        tmpunit <- extract_units(nms.obj[grep(name, nms.obj)])
        if (out$type == "hr")
          truth <- rv$truth$hr[["area"]][["All"]]
        if (out$type == "ctsd")
          truth <- rv$truth$ctsd[["All"]]
        
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
        for (i in seq_along(metaList_groups)) {
          out_groups <- metaList_groups[[i]]
          
          sum.objA <- out_groups$meta$A
          sum.objB <- out_groups$meta$B
          nms.objA <- rownames(sum.objA)
          nms.objB <- rownames(sum.objB)
          
          tmpA <- sum.objA[grep(name, nms.objA), ]
          tmpB <- sum.objB[grep(name, nms.objB), ]
          tmpunitA <- extract_units(nms.objA[grep(name, nms.objA)])
          tmpunitB <- extract_units(nms.objB[grep(name, nms.objB)])
          
          if (out_groups$type == "hr") {
            truth_A <- rv$truth$hr[["area"]][["A"]]
            truth_B <- rv$truth$hr[["area"]][["B"]]
          }

          if (out_groups$type == "ctsd") {
            truth_A <- rv$truth$ctsd[["A"]]
            truth_B <- rv$truth$ctsd[["B"]]
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
      
      Sys.sleep(3)
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
      if (rv$grouped) rv$metaList_groups <- metaList_groups
      
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe,
      bindEvent(input$run_meta)
      
    # PLOTS ---------------------------------------------------------------
    ## Rendering meta plot at the individual-level: -----------------------
    
    observe({
      if (length(rv$which_question) == 2)
        shinyjs::show(id = "metaInput_type") else
          shinyjs::hide(id = "metaInput_type")
      
    }) # end of observe
    
    output$metaPlot_all <- ggiraph::renderGirafe({
      req(rv$which_question, rv$simList, rv$simfitList, rv$truth)
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
      datList <- rv$simList
      fitList <- rv$simfitList
      
      if (set_analysis == "hr") {
        req(rv$akdeList)
        outList <- rv$akdeList
        x_label <- "Home range area (in "
        truth <- rv$truth$hr[["area"]][[1]]
        name <- "hr"
      }
      
      if (set_analysis == "ctsd") {
        req(rv$ctsdList)
        outList <- rv$ctsdList
        x_label <- "Movement speed (in "
        truth <- rv$truth$ctsd[[1]]
        name <- "ctsd"
      }
      
      outList[sapply(outList, is.null)] <- NULL # drop NULLs
      out <- extract_outputs(outList,
                             name = name,
                             si_units = FALSE, 
                             meta = TRUE)
      truth <- out$unit[[1]] %#% truth
      
      if (rv$grouped) {
        x_color <- c("black", 
                       sapply(seq_along(rv$simList), function(x) {
                         nm <- names(rv$simList)[[x]]
                         return(ifelse(nm %in% rv$groups[[2]]$A,
                                       pal$grn_d, pal$grn))
                       }))
        x_color <- rev(x_color)
      } else {
        x_color <- c(rep(pal$sea, length(datList)), "black")
      }
      
      f <- .3
      x_label <- paste0(x_label, out$unit[[1]], ")")
      p.all <- out %>% 
        ggplot2::ggplot(
          ggplot2::aes(x = est,
                       y = id,
                       # y = as.factor(reorder(id, est)), 
                       color = id)) +
        
        ggplot2::geom_vline(xintercept = truth,
                            color = "black",
                            linewidth = 0.5,
                            linetype = "solid") +
        
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_linerange(ggplot2::aes(xmin = lci,
                                             xmax = uci),
                                linetype = "dotted") +
        ggplot2::facet_grid(group ~ ., 
                            switch = "y",
                            scales = "free_y",
                            space = "free_y") +
        # ggplot2::scale_x_log10() +
        ggplot2::scale_color_manual(values = x_color) +
        ggplot2::labs(x = x_label, y = "") +
        
        theme_movedesign() +
        ggplot2::theme(
          strip.placement = "outside",
          legend.position = "none",
          axis.text.y = ggplot2::element_blank()) %>%
        suppressWarnings()
      
      ggiraph::girafe(
        ggobj = suppressWarnings(p.all),
        width_svg = 5.5, height_svg = max(2, length(datList) * f),
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
    
    ## Rendering meta plot at the group-level: ----------------------------
    
    output$metaPlot_groups <- ggiraph::renderGirafe({
      req(rv$grouped, rv$truth)
      req(rv$metaEst, rv$metaList_groups, rv$metaEst_groups)
      
      if (length(rv$which_question) == 2) {
        req(rv$set_analysis)
        set_analysis <- rv$set_analysis
      } else {
        set_analysis <- switch(rv$which_question,
                               "Home range" = "hr",
                               "Speed & distance" = "ctsd")
      }
      
      datList <- rv$simList
      fitList <- rv$simfitList
      
      x_label <- "Error (%)"
      
      if (set_analysis == "hr") {
        req(rv$akdeList)
        outList <- rv$akdeList
        name <- "area"
        # x_label <- "Home range area (in "
        # truth <- rv$truth$hr[["area"]]
      }
      
      if (set_analysis == "ctsd") {
        req(rv$ctsdList)
        outList <- rv$ctsdList
        name <- "speed"
        # x_label <- "Movement speed (in "
        # truth <- rv$truth$ctsd
      }
      
      out <- rbind(rv$metaErr, rv$metaErr_groups) %>% 
        dplyr::filter(type == set_analysis)
      
      out$truth <- rep(0, nrow(out))
      # out$truth <- sapply(1:nrow(out), function(x) {
      #   group <- out$group[x]
      #   truth_group <- truth[[group]]
      #   return(out$unit[x] %#% truth_group)
      # })
      
      # truth_group_A <- out$unit[1] %#% truth[["A"]]
      # truth_group_B <- out$unit[1] %#% truth[["B"]]
      # truth <- out$unit[1] %#% truth[["All"]]
      
      out <- out %>% 
        dplyr::mutate(group = dplyr::recode(group,
                                            "A" = "Group A",
                                            "B" = "Group B"))
      
      # x_label <- paste0(x_label, as.character(out$unit[1]), ")")
      # x_color <- c(pal$sea, "black", "grey50")
      # x_linetype <- c("solid", "dotted", "dotted")
      
      x_color <- c("black", pal$grn_d, pal$grn)
      x_linetype <- c("solid", "dotted", "dotted")
      f <- .65
      
      p.groups <- out %>% 
        ggplot2::ggplot(
          ggplot2::aes(x = est, y = group, color = group)) +
        
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = truth),
          color = "black",
          linewidth = 0.5,
          linetype = "solid") +

        # ggplot2::geom_vline(
        #   data = dplyr::filter(out, group == "All"),
        #   ggplot2::aes(xintercept = truth),
        #   color = x_color[1],
        #   linewidth = 0.5,
        #   linetype = "solid") +
        # ggplot2::geom_vline(
        #   data = dplyr::filter(out, group == "Group A"),
        #   ggplot2::aes(xintercept = truth_group_A),
        #   color = x_color[2],
        #   linewidth = 0.5,
        #   linetype = "solid") +
        # ggplot2::geom_vline(
        #   data = dplyr::filter(out, group == "Group B"),
        #   ggplot2::aes(xintercept = truth_group_B),
        #   color = x_color[3],
        #   linewidth = 0.5,
        #   linetype = "solid") +
        ggplot2::facet_wrap(group ~ ., dir = "v", scales = "free_y") +
        
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_linerange(ggplot2::aes(xmin = lci,
                                             xmax = uci,
                                             linetype = group)) +
        
        ggplot2::labs(x = x_label, y = "") +
        # ggplot2::scale_x_log10() +
        ggplot2::scale_x_continuous(labels = scales::percent) +
        ggplot2::scale_color_manual(values = x_color) +
        # ggplot2::scale_linetype_manual(values = x_linetype) +
        
        theme_movedesign() +
        ggplot2::theme(legend.position = "none")
      
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
    
    ## Rendering error plot of optimal search outputs: --------------------
    
    output$metaPlot_m_optimal <- ggiraph::renderGirafe({
      req(rv$meta_tbl)
      
      out <- rv$meta_tbl
      # main_color <- dplyr::case_when(
      #   abs(mean(out$est)) > .5 ~ as.character(pal$dgr),
      #   abs(mean(out$est)) > .1 ~ as.character(pal$gld),
      #   TRUE ~ as.character(pal$sea))
      
      if (rv$grouped) {
      main_color <- ifelse(out$subpop, pal$sea, pal$dgr)
      secondary_color <- ifelse(out[nrow(out), "subpop"],
                                pal$sea, pal$dgr)
      } else {
        main_color <- ifelse(out$subpop, pal$dgr, pal$sea)
        secondary_color <- ifelse(out[nrow(out), "subpop"],
                                  pal$dgr, pal$sea)
      }
      p.optimal <- out %>%
        ggplot2::ggplot(
          ggplot2::aes(x = m, y = est, color = subpop)) +
        ggplot2::geom_hline(yintercept = mean(out$est),
                            color = secondary_color,
                            linetype = "dotted") +
        ggplot2::geom_hline(yintercept = 0,
                            linewidth = 0.3,
                            linetype = "solid") +
        ggplot2::geom_point() +
        ggplot2::geom_linerange(
          ggplot2::aes(ymin = lci,
                       ymax = uci)) +
        
        ggplot2::labs(x = "Number of individuals", y = "Error (%)") +
        ggplot2::scale_y_continuous(
          labels = scales::percent) +
        ggplot2::scale_color_manual(values = main_color) +
        
        theme_movedesign() +
        ggplot2::theme(legend.position = "none")
      
      ggiraph::girafe(
        ggobj = p.optimal,
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
      
    }) # end of renderGirafe, "metaPlot_m_optimal"
    
    # TABLES --------------------------------------------------------------
    ## Rendering meta-analyses outputs: -----------------------------------
    
    output$metaTable_all <- reactable::renderReactable({
      req(rv$metaList, rv$sigma)
      
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
            format = reactable::colFormat(digits = 2)),
          est = reactable::colDef(
            minWidth = 40, name = "Est",
            # style = format_cov,
            format = reactable::colFormat(digits = 3)),
          high = reactable::colDef(
            minWidth = 40, name = "High",
            # style = format_cov,
            format = reactable::colFormat(digits = 3))
        ))
      
    }) # end of renderReactable, "metaTable_all"
    
    ## Rendering meta-analyses outputs (for groups): ----------------------
    
    output$metaTable_groups <- reactable::renderReactable({
      req(rv$metaList_groups, 
          rv$set_analysis,
          input$metaInput_mod)
      
      out <- rv$metaList_groups[[rv$set_analysis]]$mods
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
            format = reactable::colFormat(digits = 3))
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
    
    ## Optimal search outputs: --------------------------------------------
    
    output$metaTable_m_optimal <- reactable::renderReactable({
      req(rv$which_question, rv$meta_tbl)
      
      if (length(rv$which_question) == 2) {
        req(rv$set_analysis)
        set_analysis <- rv$set_analysis
      } else {
        set_analysis <- switch(rv$which_question,
                               "Home range" = "hr",
                               "Speed & distance" = "ctsd")
      }
      
      dt_meta <- rv$meta_tbl %>%
        dplyr::select(-overlaps, -type) %>%
        dplyr::mutate(subpop = as.logical(subpop))

      nms <- list(
        m = "M",
        lci = "Error (95% LCI)",
        est = "Error",
        uci = "Error (95% UCI)",
        subpop = "Subpop detected?"
      )
      
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
        defaultSorted = list(m = "desc"),
        defaultColDef = reactable::colDef(
          headerClass = "rtable_header",
          align = "center",
          minWidth = 60),
        
        columns = list(
          m = reactable::colDef(
            name = nms[["m"]]),
          lci = reactable::colDef(
            name = nms[["lci"]],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE, digits = 1)),
          est = reactable::colDef(
            name = nms[["est"]],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE, digits = 1)),
          uci = reactable::colDef(
             name = nms[["uci"]],
            style = format_perc,
            format = reactable::colFormat(percent = TRUE, digits = 1)),
          subpop = reactable::colDef(
            name = nms[["subpop"]])
        ))
      
    }) %>% # end of renderReactable, "metaTable_m_optimal"
      bindEvent(list(input$add_meta_table, 
                     rv$ctsdList,
                     rv$akdeList))
    
    # BLOCKS --------------------------------------------------------------
    ## Outputs: -----------------------------------------------------------
    
    observe({
      req(rv$metaErr,
          "Home range" %in% rv$which_question)
      
      mod_blocks_server(
        id = "metaBlock_hr",
        rv = rv, type = "hr", name = "metaErr")
      # TODO BUGFIX filtering type == type wasn't working, maybe fixed?
      
    }) # end of observe
    
    observe({
      req(rv$metaErr,
          "Speed & distance" %in% rv$which_question)
      
      mod_blocks_server(
        id = "metaBlock_speed",
        rv = rv, type = "ctsd", name = "metaErr")
      # TODO BUGFIX filtering type == type wasn't working, maybe fixed?
      
    }) # end of observe
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_tab_meta_ui("tab_meta_1")
    
## To be copied in the server
# mod_tab_meta_server("tab_meta_1")
