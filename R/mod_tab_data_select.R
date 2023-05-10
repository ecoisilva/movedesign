#' tab_data_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_tab_data_select_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      # Introduction: -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          shinydashboardPlus::box(
            title = span("Select movement data:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "file-circle-plus",
                                   height = "21px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "var(--sea-dark)"),
            id = ns("select_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = TRUE, closable = FALSE,
            
            column(
              align = "center", width = 12,
              
              p(style = "max-width: 1000px;",
                
                "The main goal in this tab is to extract relevant",
                "parameters from pre-existing data.",
                "Ultimately, you want to choose a species that",
                span("behaves similarly", class = "cl-sea-d"),
                "to your intended study species,",
                "as all subsequent steps will built upon these",
                "parameters.", br()),
              
              p(style = "text-align: center;",
                "First, choose a species and individual",
                "from the list.", br(), "Then click",
                icon("wand-magic-sparkles", class = "cl-mdn"),
                span("Validate", class = "cl-mdn"), "and",
                icon("paper-plane", class = "cl-mdn"),
                wrap_none(span("Extract", class = "cl-mdn"), "."))
              
            ) # end of column (text)
            
          ) # end of box // select_intro
      ), # end of div (top row)
      
      # [left column] -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-4 col-md-4 col-lg-3",
          
          # Select species & individual: ----------------------------------
          
          shinydashboardPlus::box(
            title = span("Dataset:", class = "ttl-box_solid"),
            id = ns("selectBox_species"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,
            
            # shinyWidgets::pickerInput(
            #   inputId = ns("sp_selected"),
            #   label = NULL,
            #   choices = ctmm_species,
            #   choicesOpt = list(
            #     subtext = c(
            #       "Syncerus caffer",
            #       "Pelecanus occidentalis",
            #       "Nasua narica",
            #       "Panthera onca",
            #       "Chrysocyon brachyurus",
            #       "Procapra gutturosa",
            #       "Glyptemys insculpta"),
            #     style = c(rep("font-style: italic;", 7))),
            #   options = list(`live-search` = TRUE,
            #                  title = "Pick a species"),
            #   selected = NULL),
            
            shiny::selectizeInput(
              inputId = ns("sp_selected"),
              label = NULL,
              choices = list(
                "African Buffalo" = "buffalo",
                "Brown Pelican" = "pelican",
                "Coati" = "coati",
                "Jaguar" = "jaguar",
                "Maned Wolf" = "wolf",
                "Mongolian Gazelle" = "gazelle",
                "Wood turtle" = "turtle"),
              selected = NULL,
              options = list(
                placeholder = "Pick a species",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            
            div(
              class = "text_binom",
              verbatimTextOutput(outputId = ns("binomial_name"))
            ),
            
            shiny::selectizeInput(
              inputId = ns("id_selected"),
              label = NULL,
              choices = "",
              selected = NULL,
              multiple = FALSE,
              options = list(
                placeholder = "Pick an individual",
                onInitialize = I('function() { this.setValue(""); }'))
            ),
            
            footer = splitLayout(
              uiOutput(ns("selectUI_validate")),
              actionButton(
                inputId = ns("selectButton_extract"),
                icon =  icon("paper-plane"),
                label = "Extract",
                width = "100%",
                class = "btn-primary")
            ) # end of footer
            
          ), # end of box // selectBox_species
          
          # Tracking regime: ----------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Tracking regime:", class = "ttl-box"),
            id = ns("selectBox_regime"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            fluidRow(
              column(width = 12, uiOutput(ns("selectInfo_dur"))),
              column(width = 12, uiOutput(ns("selectInfo_dti")))
            ) # end of fluidRow
            
          ) # end of box // selectBox_regime
      ), # end of div (left column)
      
      # [right column] ----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-8 col-md-8 col-lg-9",
          
          # Visualization: ------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Data visualization:", class = "ttl-box"),
            id = ns("selectBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            mod_comp_viz_ui("comp_viz_selected")
            
          ) # end of box // selectBox_viz
          
      ), # end of column (right)
      
      # [bottom column] ---------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          # Displaying relevant information: ------------------------------
          
          div(class = "col-lg-6 no-padding-left",
              shinydashboardPlus::box(
                title = span("Displaying parameters:", class = "ttl-box"),
                id = ns("selectBox_parameters"),
                width = NULL,
                solidHeader = FALSE,
                
                ## Extracted parameters:
                uiOutput(ns("selectUI_parameters"))
                
              ) # end of box // selectBox_parameters
          ), # end of div
          
          ## Sample sizes:
          
          div(class = "col-lg-6 no-padding-right",
              shinydashboardPlus::box(
                title = span("Displaying sample sizes:",
                             class = "ttl-box"),
                id = ns("selectBox_sizes"),
                width = NULL,
                solidHeader = FALSE,
                
                fluidRow(
                  column(width = 4, uiOutput(ns("selBlock_n"))),
                  column(width = 4, mod_blocks_ui(ns("selBlock_Narea"))),
                  column(width = 4, mod_blocks_ui(ns("selBlock_Nspeed")))
                ) # end of fluidRow
                
              ) # end of box // selectBox_sizes
          ), # end of div
          
          # Additional information: ---------------------------------------
          
          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("selectBox_misc"),
            width = NULL, solidHeader = FALSE,
            
            verbatimTextOutput(outputId = ns("selectUI_time"))
            
          ) # end of box // selectBox_misc
      ) # end of column (bottom)
      
    ), # end of fluidRow
    
    # MODALS: -------------------------------------------------------------
    
    create_modal(var = "taup", id = "select"),
    create_modal(var = "tauv", id = "select"),
    create_modal(var = "sigma", id = "select"),
    NULL
    
  ) # end of tagList
}

#' tab_data_select Server Functions
#'
#' @noRd
mod_tab_data_select_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    ## Match id for input, plot and table:
    
    observe({
      req(input$id_selected != "")
      vals$id <- input$id_selected
    }) %>% bindEvent(input$id_selected)
    
    observe({
      vals$id <- vals$table_selection
    })
    observe({
      vals$id <- vals$plot_selection
    })
    
    observe({
      req(input$sp_selected != "")
      
      shiny::updateSelectizeInput(
        session,
        inputId = "id_selected",
        label = NULL,
        choices = names(dataset_selected()))
      
    }) %>% # end of observe,
      bindEvent(input$sp_selected)
    
    observe({
      req(vals$active_tab == 'data_select')
      
      shiny::updateSelectizeInput(
        session,
        inputId = "id_selected",
        label = NULL,
        choices = names(dataset_selected()),
        selected = vals$id)
      
    }) %>% bindEvent(vals$id)
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide all boxes at start: -------------------------------------------
    
    boxnames <- c("regime", 
                  "parameters",
                  "sizes",
                  "misc")
    
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("selectBox_", boxnames[i]))
    }
    
    ## Add scientific name below common name: -----------------------------
    
    output$binomial_name <- renderText({
      req(input$sp_selected != "")
      
      nms <- c("buffalo" = "Syncerus caffer",
               "pelican" = "Pelecanus occidentalis",
               "coati" = "Nasua narica",
               "jaguar" = "Panthera onca",
               "wolf" = "Chrysocyon brachyurus",
               "gazelle" = "Procapra gutturosa",
               "turtle" = "Glyptemys insculpta")
      
      return(nms[input$sp_selected][[1]])
    }) # end of renderText, "binomial_name"
    
    ## Render validate button: --------------------------------------------
    
    output$selectUI_validate <- renderUI({
      
      out <- shiny::actionButton(
        inputId = ns("validate_select"),
        icon =  icon("wand-magic-sparkles"),
        label = "Validate",
        width = "100%")
      
      if (!is.null(vals$is_valid)) {
        if (vals$is_valid) 
          out <- shiny::actionButton(
            inputId = ns("validate_select"),
            icon =  icon("circle-check"),
            label = "Validated!",
            width = "100%",
            class = "btn-info")
      }
      
      return(out)
      
    }) # end of renderUI, "selectUI_validate"
    
    ## Render parameters box: ---------------------------------------------
    
    output$selectUI_parameters <- renderUI({
      req(vals$data_type == "selected",
          vals$tmpid, vals$tmpsp1, vals$tmpsp2)
      
      tagList(
        column(
          align = "center", width = 12,
          
          p("These parameters have been extracted from",
            "individual", span(vals$tmpid, class = "cl-sea-d"),
            "and species", span(vals$tmpsp1, class = "cl-sea-d"),
            wrap_none("(", em(vals$tmpsp2), ")."),
            "They will only update if you change the",
            "individual and/or species selected, and then",
            "click the buttons",
            icon("wand-magic-sparkles", class = "cl-mdn"),
            span("Validate", class = "cl-mdn"), "and",
            icon("paper-plane", class = "cl-mdn"),
            wrap_none(span("Extract", class = "cl-mdn"), "."))
          
        ), # end of column (for text)
        
        column(width = 12, uiOutput(ns("selBlock_process"))),
        
        fluidRow(
          column(width = 6, mod_blocks_ui(ns("selBlock_taup"))),
          column(width = 6, mod_blocks_ui(ns("selBlock_tauv")))),
        
        fluidRow(
          column(width = 6, mod_blocks_ui(ns("selBlock_sigma"))),
          column(width = 6, mod_blocks_ui(ns("selBlock_speed"))))
        
      ) # end of out_ui
      
    }) # end of renderUI, "selectUI_parameters"
    
    # ALERTS --------------------------------------------------------------
    
    ## After clicking "Extract" button:
    
    observe({
      req(vals$which_question)
      
      if (is.null(vals$is_valid)) {
        shinyalert::shinyalert(
          title = "Oops!",
          text = tagList(span(
            'Please select a species and an individual',
            'first, then click the',
            icon("wand-magic-sparkles", class = "cl-mdn"),
            span('Validate', class = "cl-mdn"), "and",
            icon("paper-plane", class = "cl-mdn"),
            span('Extract', class = "cl-mdn"),
            'buttons.'),
          html = TRUE,
          size = "xs"))
      }
      
    }) %>% # end of observe, then:
      bindEvent(input$selectButton_extract)
    
    # OPERATIONS ----------------------------------------------------------
    ## 1. Select data: ----------------------------------------------------
    
    ### 1.1. Load species dataset from the 'ctmm' package:
    
    dataset_selected <- reactive({
      req(input$sp_selected != "")
      reset_reactiveValues(vals) # reset vals between data tabs
      
      utils::data(list = input$sp_selected, package = "ctmm")
      
      out_dataset <- NULL
      if (input$sp_selected == "buffalo") out_dataset <- buffalo
      if (input$sp_selected == "pelican") out_dataset <- pelican
      if (input$sp_selected == "coati") out_dataset <- coati
      if (input$sp_selected == "jaguar") out_dataset <- jaguar
      if (input$sp_selected == "wolf") out_dataset <- wolf
      if (input$sp_selected == "gazelle") out_dataset <- gazelle
      if (input$sp_selected == "turtle") out_dataset <- turtle
      
      req(out_dataset)
      
      # Check if data is anonymized:
      
      if (!("timestamp" %in% names(out_dataset[[1]]))) {
        
        out_dataset <- pseudonymize(out_dataset)
        
        shinyFeedback::showToast(
          type = "success",
          title = "Data is anonymized...",
          message = "Origin location and time added.",
          .options = list(
            timeOut = 2500,
            progressBar = FALSE,
            closeButton = TRUE,
            preventDuplicates = TRUE,
            positionClass = "toast-bottom-right"))
        
        msg_log(
          style = "success",
          message = paste0("Data pseudonymization ",
                           msg_success("completed"), "."),
          detail = "Origin location and time added.")
      }
      
      vals$species <- input$sp_selected
      vals$dataList <- out_dataset
      vals$data_type <- "selected"

      index <- rownames(vals$ctmm) %>% match(x = input$sp_selected)
      vals$species_common <- vals$ctmm[index, 1]
      vals$species_binom <- vals$ctmm[index, 2]
      
      vals$id <- NULL
      return(out_dataset)
      
    }) %>% # end of reactive,
      bindEvent(input$sp_selected)
    
    # 1.2. Subset data based on individual selection:
    
    data_selected <- reactive({
      
      out_data <- dataset_selected()[[vals$id]]
      vals$input_x <- ifelse(!is.null(out_data$"x"),
                             "x", "longitude")
      vals$input_y <- ifelse(!is.null(out_data$"y"),
                             "y", "latitude")
      vals$input_t <- ifelse(!is.null(out_data$"timestamp"),
                             "timestamp", "t")
      
      return(out_data)
    })
    
    ## 2. Validate data: --------------------------------------------------
    
    guess <- reactive({
      ctmm::ctmm.guess(dataset_selected()[[vals$id]],
                       interactive = FALSE)
    })
    
    fit_selected <- reactive({
      
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("found"), "..."))
      
      fitList <- readRDS(
        system.file("extdata",
                    paste0(input$sp_selected, "_fitList.rds"),
                    package = "movedesign"))
      
      msg_log(paste0("...Model fit ",
                    msg_success("loaded"), "."))
      
      out_fit <- fitList[[vals$id]][[1]]
      vals$fit0 <- out_fit
      vals$guess0 <- guess()
      return(out_fit)
      
    }) # end of reactive // fit_selected
    
    observe({
      
      if (is.null(vals$which_question)) {
        
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
      
    }) %>% bindEvent(input$validate_select)
    
    is_valid <- reactive({
      req(vals$dataList, input$id_selected != "")
      
      ### Set up for validation:
      
      taup <- extract_pars(fit_selected(), name = "position")
      tauv <- extract_pars(fit_selected(), name = "velocity")
      
      ### Validate based on research question(s):
      
      is_data_valid <- FALSE
      if (is.null(taup) && is.null(tauv)) {
        
        shinyalert::shinyalert(
          type = "error",
          title = "Dataset invalid",
          text = tagList(span(
            "Data is",
            wrap_none(span("independent", class = "cl-dgr"), ","),
            "and no signature of autocorrelation parameters",
            "remains in this dataset.",
            "Please select a different individual or dataset to",
            "proceed with", span("home range", class = "cl-dgr"),
            "estimation.")),
          
          confirmButtonText = "Dismiss",
          html = TRUE)

      } else if ("Home range" %in% vals$which_question) {
        is_data_valid <- TRUE
        
        if (is.null(taup)) {
          
          shinyalert::shinyalert(
            type = "error",
            
            title = "Dataset invalid",
            text = tagList(span(
              "No significant signature of the animal's",
              span("position autocorrelation", class = "cl-dgr"),
              "parameter remains in this dataset.",
              "Please select a different individual or dataset to",
              "proceed with", span("home range", class = "cl-dgr"),
              "estimation.")),
            
            confirmButtonText = "Dismiss",
            html = TRUE)
          
          msg_log(
            style = "danger",
            message = paste("No signature of",
                            msg_danger("position autocorrelation"),
                            "found."),
            detail = "Select a different dataset to proceed.")
          
          is_data_valid <- FALSE
        }
        
      } else if ("Speed & distance" %in% vals$which_question) {
        is_data_valid <- TRUE
        
        if (is.null(tauv)) {
          
          shinyalert::shinyalert(
            type = "error",
            title = "Dataset invalid",
            text = tagList(span(
              "No significant signature of the animal's",
              span("velocity autocorrelation", class = "cl-dgr"),
              "parameter remains in this dataset.",
              "Please select a different individual or dataset to",
              "proceed with", span("distance/speed", class = "cl-dgr"),
              "estimation.")),
            
            confirmButtonText = "Dismiss",
            html = TRUE)
          
          msg_log(
            style = "danger",
            message = paste("No signature of",
                            msg_danger("velocity autocorrelation"),
                            "found."),
            detail = "Select a different dataset to proceed.")
          
          is_data_valid <- NULL
        }
      }
      
      req(vals$id)
      vals$data0 <- data_selected()
      vals$is_valid <- is_data_valid
      return(is_data_valid)
      
    }) # end of reactive

    observe({
      req(is_valid())

      msg_log(
        style = "success",
        message = paste0("Species and individual ",
                         msg_success("validated"), "."),
        detail = paste0("Species selected is the ",
                        msg_success(input$sp_selected),
                        ", and the individual is ",
                        msg_success(input$id_selected), "."))
      
      shinyFeedback::showToast(
        type = "success",
        message = "Data validated!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right")
      )
      
    }) %>% # end of observe,
      bindEvent(input$validate_select)

    # PARAMETERS ----------------------------------------------------------
    ## Extract location variance, timescales, etc.: -----------------------
    
    extract_sigma <- reactive({
      if (is.null(vals$var_fraction)) frac <- .65
      else frac <- vals$var_fraction
      
      svf <- extract_svf(data_selected(), fraction = frac)
      vals$svf <- svf
      
      return(extract_pars(obj = vals$fit0, 
                          name = "sigma", 
                          data = vals$data0,
                          fraction = frac))
      
    }) # end of reactive
    
    observe({
      req(vals$which_question,
          vals$data_type == "selected",
          vals$data0, vals$is_valid)
      
      shinyjs::show(id = "selectBox_parameters")
      shinyjs::show(id = "selectBox_regime")
      shinyjs::show(id = "selectBox_sizes")
      
      vals$sigma0 <- extract_sigma()
      vals$tau_p0 <- extract_pars(fit_selected(), name = "position")
      vals$tau_v0 <- extract_pars(fit_selected(), name = "velocity")
      vals$speed0 <- extract_pars(fit_selected(), name = "speed")
      
      vals$tmpsp1 <- vals$species_common
      vals$tmpsp2 <- vals$species_binom
      vals$tmpid <- vals$id
      
      shinyFeedback::showToast(
        type = "success",
        message = "Parameters extracted!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"))
      
      msg_log(
        style = "success",
        message = paste0("Parameters ",
                         msg_success("extracted"), "."),
        detail = paste("Proceed to",
                       msg_success('Sampling design'), "tab."))
      
      if (!vals$tour_active) {
        shinyalert::shinyalert(
          className = "modal_success",
          type = "success",
          title = "Success!",
          text = tagList(span(
            "Proceed to the", br(),
            icon("stopwatch", class = "cl-mdn"),
            span("Tracking regime", class = "cl-mdn"), "tab."
          )),
          html = TRUE,
          size = "xs")
      }
      
    }) %>% # end of observe, then:
      bindEvent(input$selectButton_extract)
    
    # BLOCKS --------------------------------------------------------------
    ## Movement process: --------------------------------------------------
    
    output$selBlock_process <- shiny::renderUI({
      req(vals$tmpid, vals$fit0)
      
      if (vals$tmpid == "Simulated individual") {
        NULL } else {
          sum.fit <- summary(vals$fit0)
          
          parBlock(
            header = fluidRow(
              style = paste("margin-bottom: -14px;"),
              actionButton(
                inputId = ns("selectHelp_mods"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;")),
              br(), "Movement process"),
            value = sum.fit$name[1])
          
        } # end of if () statement
    }) # end of renderUI, "selBlock_process"
    
    ## Timescale parameters: ----------------------------------------------
    
    observe({
      req(vals$tau_p0)
      
      mod_blocks_server(
        id = "selBlock_taup", 
        vals = vals, type = "tau", name = "tau_p0",
        input_name = list(
          chr = "select_taup0",
          html = wrap_none("Position autocorrelation ",
                           "(\u03C4", tags$sub("p"), ")")),
        input_modal = "modal_taup_select")
    })
    
    observe({
      req(vals$tau_v0)
      
      mod_blocks_server(
        id = "selBlock_tauv",
        vals = vals, type = "tau", name = "tau_v0",
        input_name = list(
          chr = "select_tauv0",
          html = wrap_none("Velocity autocorrelation ",
                           "(\u03C4", tags$sub("v"), ")")),
        input_modal = "modal_tauv_select")
    })
    
    ## Location variance: -------------------------------------------------
    
    observe({
      req(vals$sigma0)
      
      mod_blocks_server(
        id = "selBlock_sigma",
        vals = vals, type = "sigma", name = "sigma0",
        input_name = list(
          chr = "select_sigma0",
          html = wrap_none("Location variance ",
                    "(\u03C3", tags$sub("p"), ")")),
        input_modal = "modal_sigma_select")
    })
    
    ## Speed: -------------------------------------------------------------
    
    observe({
      req(vals$speed0)

      mod_blocks_server(
        id = "selBlock_speed",
        vals = vals, type = "speed", name = "speed0",
        input_name = list(
          chr = "select_speed0",
          html = wrap_none("Velocity variance (\u03C3", 
                           tags$sub("v"), ")")),
        input_modal = "modal_speed_select")
      
    })
    
    ## Tracking regime: ---------------------------------------------------
    
    output$selectInfo_dur <- shiny::renderUI({
      req(vals$data0)
      
      dur <- extract_pars(vals$data0, name = "period")
      out <- fix_unit(dur$value, dur$unit)
      
      parBlock(header = "Sampling duration",
               value = paste(out[1], out[2]))
      
    }) # end of renderUI // selectInfo_dur
    
    output$selectInfo_dti <- shiny::renderUI({
      req(vals$data0)
      
      dti <- extract_pars(vals$data0, name = "interval")
      out <- fix_unit(dti$value, dti$unit)
      
      parBlock(header = "Sampling interval",
               value = paste(out[1], out[2]),
               subtitle = "between fixes")
      
    }) # end of renderUI // selectInfo_dti
    
    ## Sample sizes: ------------------------------------------------------
    
    output$selBlock_n <- shiny::renderUI({
      req(vals$data0)
      
      sampleBlock(
        number = NULL,
        numberIcon = FALSE,
        header = nrow(vals$data0),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)
      
    }) # end of renderUI, "uplBlock_n" (absolute sample size)
    
    observe({
      req(vals$fit0)
      
      mod_blocks_server(
        id = "selBlock_Narea", 
        vals = vals, data = vals$data0, fit = vals$fit0,
        type = "N", name = "area")
    
      mod_blocks_server(
        id = "selBlock_Nspeed", 
        vals = vals, data = vals$data0, fit = vals$fit0,
        type = "N", name = "speed")
      
    }) # end of observe
    
    # MODALS & HELP -------------------------------------------------------
    
    observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Movement models or processes:",
          
          reactable::reactableOutput(ns("selTable_models")),
          
          footer = tagList(modalButton("Dismiss")),
          size = "l"))
      
    }) %>% # end of observe,
      bindEvent(input$selectHelp_mods)
    
    output$selTable_models <- reactable::renderReactable({
      mods <- movedesign::movmods

      nm <- sub('(^\\w+)\\s.+','\\1', 
                summary(fit_selected())$name[1])
      
      preselected_mod <- NULL
      if (!is.null(match(nm, mods$name_short))) {
        preselected_mod <- match(nm, mods$name_short) 
      }
      out <- mods %>% dplyr::select(!.data$name_short)
      
      cell_yn <- function(value) {
        # Render as an X mark or check mark
        if (value == "No") "\u274c No" else "\u2714\ufe0f Yes"
      }
      
      reactable::reactable(
        out,
        searchable = FALSE,
        highlight = TRUE,
        defaultSelected = preselected_mod,
        defaultColDef = reactable::colDef(
          headerClass = "rtable_header",
          align = "left"),
        columns = list(
          name = reactable::colDef(
            name = "Movement process",
            minWidth = 195),
          
          tau_p = reactable::colDef(
            minWidth = 60,
            name = paste0("\u03C4","\u209A"),
            cell = cell_yn),
          
          tau_v = reactable::colDef(
            minWidth = 60,
            name = paste0("\u03C4","\u1D65"),
            cell = cell_yn),
          
          hrange = reactable::colDef(
            minWidth = 80,
            name = "Home range",
            cell = cell_yn),
          
          pars = reactable::colDef(
            name = "Parameterization")
        ),
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(
            backgroundColor = "#eee",
            boxShadow = "inset 2px 0 0 0 #009da0")))
      
    }) # end of renderReactable // selTable_models
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_data_select_ui("tab_data_select_1")

## To be copied in the server
# mod_tab_data_select_server("tab_data_select_1")
