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
    shiny::fluidRow(
      
      # Introduction: -----------------------------------------------------
      
      div(class = div_column_main,
          
          shinydashboardPlus::box(
            
            title = span("Select movement data:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "file",
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
                "parameters.",
                br()),
              
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
      
      div(class = div_column_left,
          
          # Select species & individual: ----------------------------------
          
          shinydashboardPlus::box(
            title = span("Dataset:", class = "ttl-box_solid"),
            id = ns("selectBox_species"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,
            
            shinyWidgets::pickerInput(
              inputId = ns("sp_selected"),
              label = NULL,
              choices = ctmm_species,
              choicesOpt = list(
                subtext = c(
                  "Syncerus caffer",
                  "Pelecanus occidentalis",
                  "Nasua narica",
                  "Panthera onca",
                  "Chrysocyon brachyurus",
                  "Procapra gutturosa",
                  "Glyptemys insculpta"),
                style = c(rep("font-style: italic;", 7))),
              options = list(`live-search` = TRUE,
                             title = "Pick a species"),
              selected = NULL),
            
            shinyWidgets::pickerInput(
              inputId = ns("id_selected"),
              label = NULL,
              choices = "",
              multiple = FALSE,
              options = list(`live-search` = TRUE,
                             title = "Pick an individual"),
              selected = NULL),
            
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
          
          # Select variables: ---------------------------------------------
          
          # shinydashboardPlus::box(
          #   title = span("Variables", class = "ttl-box_solid"),
          #   id = ns("selectBox_variables"),
          #   status = "primary",
          #   width = NULL,
          #   solidHeader = TRUE,
          #   collapsible = FALSE, closable = FALSE,
          #
          #   selectInput(inputId = ns("selectVar_x"),
          #               label = "X coordinate:",
          #               selected = "x",
          #               choices = ""),
          #   selectInput(inputId = ns("selectVar_y"),
          #               label = "Y coordinate:",
          #               selected = "y",
          #               choices = ""),
          #   selectInput(inputId = ns("selectVar_t"),
          #               label = "Datetime:",
          #               selected = "timestamp",
          #               choices = ""),
          #
          #   actionButton(
          #     inputId = ns("selectButton_extract"),
          #     icon =  icon("paper-plane"),
          #     label = "Extract",
          #     width = "100%",
          #     class = "btn-primary")
          #
          # ), # end of box // selectBox_variables
          
          # Tracking regime: ----------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Tracking regime:", class = "ttl-box"),
            id = ns("selectBox_regime"),
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
      
      div(class = div_column_right,
          
          # Visualization: ------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Data visualization:", class = "ttl-box"),
            id = ns("selectBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            mod_comp_viz_ui("comp_viz_selected")
            
          ) # end of box // selectBox_viz
          
      ), # end of column (center)
      
      # [bottom column] ---------------------------------------------------
      
      div(class = div_column_main,
          
          # Displaying relevant metrics: ----------------------------------
          
          ## Extracted parameters:
          uiOutput(ns("selectUI_parameters")),
          
          ## Sample sizes:
          shinydashboardPlus::box(
            title = span("Displaying sample sizes:", class = "ttl-box"),
            id = ns("selectBox_sizes"),
            width = NULL,
            solidHeader = FALSE,
            
            fluidRow(
              column(width = 4, uiOutput(ns("selectBlock_n"))),
              column(width = 4, uiOutput(ns("selectBlock_Narea"))),
              column(width = 4, uiOutput(ns("selectBlock_Nspeed"))),
            ) # end of fluidRow
            
          ), # end of box // selectBox_sizes
          
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
    
    create_modal(var = "taup",  id = "select"),
    create_modal(var = "tauv",  id = "select"),
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
    
    # DYNAMIC UI ELEMENTS -----------------------------------------------
    ## Hide all boxes at start: -----------------------------------------
    
    tmpnames <- c("regime",
                  "sizes",
                  "misc")
    
    for(i in 1:length(tmpnames)) {
      shinyjs::hide(id = paste0("selectBox_", tmpnames[i]))
    }
    
    ## Match id for input, plot and table: --------------------------------
    
    observe({
      req(input$id_selected)
      vals$id <- input$id_selected
    })
    
    observe({
      req(vals$table_selection)
      vals$id <- names(vals$dataList)[vals$table_selection$selected]
    })
    
    observe({
      req(vals$plot_selection)
      vals$id <- names(vals$dataList)[
        match(vals$plot_selection, names(vals$dataList))]
    })
    
    observe({
      req(vals$dataList, vals$id)
      
      shinyWidgets::updatePickerInput(
        session,
        inputId = "id_selected",
        label = NULL,
        choices = names(vals$dataList),
        selected = isolate(vals$id))
      
    }) # end of observe
    
    ## Render validate button: --------------------------------------------
    
    output$selectUI_validate <- renderUI({
      
      if (vals$is_valid) {
        shiny::actionButton(
          inputId = ns("validate_select"),
          icon =  icon("circle-check"),
          label = "Validated!",
          width = "100%",
          class = "btn-info")
      } else {
        shiny::actionButton(
          inputId = ns("validate_select"),
          icon =  icon("wand-magic-sparkles"),
          label = "Validate",
          width = "100%")
        # , class = "btn-danger")
      }
      
    }) %>% # end of renderUI // selectUI_validate
      bindEvent(vals$is_valid)
    
    # SELECT DATA -------------------------------------------------------
    ## Select species: --------------------------------------------------
    
    ### 1.1. Load species data from the 'ctmm' package:
    
    observe({
      req(vals$active_tab == 'data_select', input$sp_selected)
      
      vals$is_valid <- FALSE
      vals$is_success <- NULL
      
      vals$data0 <- NULL
      vals$data_type <- "selected"
      
      vals$species <- input$sp_selected
      utils::data(list = vals$species, package = "ctmm")
      
      df0 <- NULL
      if(vals$species == "buffalo") df0 <- buffalo
      if(vals$species == "pelican") df0 <- pelican
      if(vals$species == "coati") df0 <- coati
      if(vals$species == "jaguar") df0 <- jaguar
      if(vals$species == "wolf") df0 <- wolf
      if(vals$species == "gazelle") df0 <- gazelle
      if(vals$species == "turtle") df0 <- turtle
      
      # Check if data is anonymized:
      
      req(df0)
      if (!("timestamp" %in% names(df0[[1]]))) {
        
        vals$is_anonymized <- TRUE
        
        shiny::showNotification(
          duration = 5,
          ui = shiny::span(
            "Data is anonymized,", br(),
            "simulating location and time."),
          closeButton = FALSE, type = "warning")
        
        msg_log(
          style = "success",
          message = paste0("Data anonymization ",
                           msg_success("completed"), "."),
          detail = "Added randomized origin (location and time).")
        
        df0 <- ctmm:::pseudonymize(df0)
        
      } else { vals$is_anonymized <- FALSE }
      
      vals$dataList <- df0
      
      shinyWidgets::updatePickerInput(
        session,
        inputId = "id_selected",
        label = NULL,
        choices = names(vals$dataList))
      
    }) # end of observe
    
    # 1.2. Subset data based on individual selection:
    
    observe({
      req(vals$dataList,
          vals$id,
          vals$active_tab == 'data_select')
      
      vals$is_success <- NULL
      df_subset <- vals$dataList[[vals$id]]
      
      index <- ctmm_species %>% match(x = vals$species)
      vals$species_common <- names(ctmm_species[index])
      vals$species_binom <- names(ctmm_species_binom[index])
      vals$data0 <- df_subset
      
      # shinyjs::enable("selectButton_extract")
      
    }) %>% # end of observe,
      bindEvent(input$id_selected)
    
    ## Validate data: ---------------------------------------------------
    
    observe({
      req(vals$which_question, vals$data0, vals$species_binom)
      
      if(is.null(vals$needs_fit)) {
        vals$needs_fit <- TRUE
      }
      
      ### Model fitting:
      
      guess0 <- reactive({
        ctmm::ctmm.guess(vals$data0, interactive = FALSE)
      }) %>% bindCache(vals$species_binom,
                       vals$id)
      vals$guess <- guess0()
      
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("available"), "."),
        detail = "...Loading existing model fit now."
      )
      
      fitList <- readRDS(
        system.file("extdata",
                    paste0(vals$species, "_fitList.rds"),
                    package = "movedesign"))
      
      msg_log(
        style = "success",
        message = "...Model fit loaded.",
        detail = ""
      )
      
      vals$fit0 <- fitList[[vals$id]][[1]]
      vals$needs_fit <- FALSE
      rm(fitList)
      
      ### Set up for validation:
      
      taup <- extract_pars(vals$fit0, par = "position")
      taup_lci <- extract_pars(vals$fit0, par = "position", type = "low")
      taup_uci <- extract_pars(vals$fit0, par = "position", type = "high")

      tauv <- extract_pars(vals$fit0, par = "velocity")
      tauv_lci <- extract_pars(vals$fit0, par = "velocity", type = "low")
      tauv_uci <- extract_pars(vals$fit0, par = "velocity", type = "high")
      
      ### Validate based on research questions:
      
      if ("Home range" %in% vals$which_question) {
        if (is.na(taup$value)) {
          
          shinyalert::shinyalert(
            type = "error",
            
            title = "Dataset invalid",
            text = span(
              "No significant signature of the animal's",
              span("position autocorrelation", class = "cl-dgr"),
              "parameter remains in this dataset.",
              "Please select a different individual or dataset to",
              "proceed with", span("home range", class = "cl-dgr"),
              "estimation."),
            
            confirmButtonText = "Dismiss",
            html = TRUE)
          
          msg_log(
            style = "danger",
            message = paste0("Dataset has no remaining ",
                             msg_danger("range residency"),
                             " signature."),
            detail = paste("Select a different individual",
                           "or dataset to proceed."))
          
          vals$is_valid <- FALSE
          
        } else if (taup_uci$value/taup_lci$value > 5) {
          
          shinyalert::shinyalert(
            type = "error",
            
            title = "Dataset invalid",
            text = span(
              "The estimation of the",
              span("position autocorrelation parameter", 
                   class = "cl-dgr"),
              "is too uncertain.",
              "Please select a different individual or dataset to",
              "proceed with", span("home range", class = "cl-dgr"),
              "estimation."),
            
            confirmButtonText = "Dismiss",
            html = TRUE)
          
          msg_log(
            style = "danger",
            message = paste0("Parameter has wide ",
                             msg_danger("confidence intervals"), "."),
            detail = paste0("Select a different individual",
                            "or dataset to proceed."))
          vals$is_valid <- FALSE
        }
        vals$is_valid <- TRUE
      }
      
      if ("Speed & distance" %in% vals$which_question) {
        if (is.na(tauv$value)) {
          
          shinyalert::shinyalert(
            type = "error",
            
            title = "Dataset invalid",
            text = span(
              "No significant signature of the animal's",
              span("velocity autocorrelation", class = "cl-dgr"),
              "parameter remains in this dataset.",
              "Please select a different individual or dataset to",
              "proceed with", span("distance/speed", class = "cl-dgr"),
              "estimation."),
            
            confirmButtonText = "Dismiss",
            html = TRUE)
          
          msg_log(
            style = "danger",
            message = paste0("Dataset has no remaining ",
                             msg_danger("velocity"), 
                             " signature."),
            detail = paste0("Select a different individual",
                            "or dataset to proceed."))
          
          vals$is_valid <- FALSE
          
        } else if (tauv_uci$value/tauv_lci$value > 5) {
          
          shinyalert::shinyalert(
            type = "error",
            
            title = "Dataset invalid",
            text = span(
              "The estimation of the",
              span("position autocorrelation parameter", 
                   class = "cl-dgr"),
              "is too uncertain.",
              "Please select a different individual or dataset to",
              "proceed with", span("home range", class = "cl-dgr"),
              "estimation."),
            
            confirmButtonText = "Dismiss",
            html = TRUE)
          
          msg_log(
            style = "danger",
            message = paste0("Parameter has wide ",
                             msg_danger("confidence intervals"), "."),
            detail = paste0("Select a different individual",
                            "or dataset to proceed."))
          
          vals$is_valid <- FALSE
        }
        vals$is_valid <- TRUE
      }
      
      req(vals$is_valid)
      vals$input_x <- ifelse(!is.null(vals$data0$"x"),
                             "x", "longitude")
      vals$input_y <- ifelse(!is.null(vals$data0$"y"),
                             "y", "latitude")
      vals$input_t <- ifelse(!is.null(vals$data0$"timestamp"),
                             "timestamp", "t")
      
      if(is.null(vals$is_success)) {
        msg_log(
          style = "success",
          message = paste0("Species and individual ",
                           msg_success("validated"), "."),
          detail = paste0("Species selected is the ",
                          msg_success(vals$species),
                          ", and the individual is ",
                          msg_success(vals$id), "."))
      }
      vals$is_success <- TRUE
      
      shinyFeedback::showToast(
        type = "success",
        message = "Data validated!",
        .options = list(
          timeOut = 3000,
          extendedTimeOut = 3500,
          progressBar = FALSE,
          closeButton = TRUE,
          preventDuplicates = TRUE,
          positionClass = "toast-bottom-right"
        )
      )
      
      # if(!input$select_intro$collapsed &&
      #    vals$tour_active) { NULL } else {
      #      shinydashboardPlus::updateBox("select_intro",
      #                                    action = "toggle")
      #    }
      
    }) %>% # end of observe,
      bindEvent(input$validate_select, ignoreInit = TRUE)
    
    
    # PARAMETERS --------------------------------------------------------
    # After clicking "Extract" button:
    
    observe({
      shinyjs::show(id = "selectBox_regime")
      
      if(is.null(vals$is_valid) || vals$is_valid == FALSE) {
        
        shinyalert::shinyalert(
          title = "Oops!",
          text = span(
            'Please select a species and an individual',
            'first, then click the',
            icon("wand-magic-sparkles", class = "cl-mdn"),
            span('Validate', class = "cl-mdn"), "and",
            icon("paper-plane", class = "cl-mdn"),
            span('Extract', class = "cl-mdn"),
            'buttons.'),
          html = TRUE,
          size = "xs")
        
      } else {
        req(vals$data_type == "selected",
            vals$data0, vals$id, vals$species_binom,
            vals$is_valid, vals$is_success)
      }
      
      shinyjs::show(id = "selectBox_sizes")
      
      vals$tmpsp1 <- vals$species_common
      vals$tmpsp2 <- vals$species_binom
      vals$tmpid <- vals$id
      
      ## Extract semi-variance parameter: -------------------------------
      
      if (is.null(vals$var_fraction)) frac <- .65
      else frac <- vals$var_fraction
      
      svf <- prepare_svf(vals$data0, fraction = frac)
      vals$sigma0_min <- mean(svf$var_low95)
      vals$sigma0_max <- mean(svf$var_upp95)
      vals$svf <- svf
      
      ## Extract timescale and spatial parameters: ----------------------
      
      output$selectUI_parameters <- renderUI({
        
        shinydashboardPlus::box(
          title = span("Displaying parameters:", class = "ttl-box"),
          id = ns("selectBox_parameters"),
          width = NULL,
          solidHeader = FALSE,
          
          column(
            align = "center", width = 12,
            
            renderUI({
              req(vals$tmpid)
              if(vals$tmpid == "Simulated individual") {
                NULL
              } else {
                req(vals$tmpid, vals$tmpsp1, vals$tmpsp2)
                
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
              } # end of if() statement
            }) # end of renderUI
            
          ), # end of column (for text)
          
          column(width = 12, uiOutput(ns("selectBlock_process"))),
          
          fluidRow(
            column(width = 4, uiOutput(ns("selectBlock_sigma"))),
            column(width = 4, uiOutput(ns("selectBlock_taup"))),
            column(width = 4, uiOutput(ns("selectBlock_tauv")))
          )
          
        ) # end of box
      }) # end of renderUI
      
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
      
      # shinyjs::disable("selectButton_extract")
      
      if(!vals$tour_active) {
        
        shinyalert::shinyalert(
          className = "modal_success",
          type = "success",
          title = "Success!",
          text = span(
            "Proceed to the", br(),
            icon("stopwatch", class = "cl-mdn"),
            span('Tracking regime', class = "cl-mdn"), "tab."),
          html = TRUE,
          size = "xs")
      }
      
    }) %>% # end of observe, then:
      bindEvent(input$selectButton_extract)
    
    # BLOCKS ------------------------------------------------------------
    ## Movement process: ------------------------------------------------
    
    output$selectBlock_process <- shiny::renderUI({
      req(vals$tmpid, vals$fit0)
      
      if(vals$tmpid == "Simulated individual") {
        NULL } else {
          sum.fit <- summary(vals$fit0)
          
          parBlock(
            header = shiny::fluidRow(
              style = paste("margin-bottom: -14px;"),
              actionButton(
                inputId = ns("selectHelp_mods"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;")),
              br(), "Movement process"
            ),
            value = sum.fit$name[1])
          
        } # end of if() statement
    }) # end of renderUI
    
    ## Timescale parameters: --------------------------------------------
    
    output$selectBlock_taup <- shiny::renderUI({
      req(vals$tmpid, vals$fit0)
      
      if(vals$tmpid == "Simulated individual") {
        NULL } else {
          
          sum.fit <- summary(vals$fit0)
          tempnames <- rownames(sum.fit$CI)
          
          if(length(grep('\u03C4', tempnames)) == 1 ||
             length(grep('\u03C4', tempnames)) == 2) {
            
            tempunits <-
              ( tempnames[grep('position', tempnames)] %>%
                  extract_units() )
            
            fit.tau_p <- sum.fit$CI[
              grep('position', tempnames), 2] # %#% tempunits
            fit.tau_p_low <- sum.fit$CI[
              grep('position', tempnames), 1]
            fit.tau_p_high <- sum.fit$CI[
              grep('position', tempnames), 3]
            
            vals$tau_p0 <- fit.tau_p
            vals$tau_p0_min <- fit.tau_p_low
            vals$tau_p0_max <- fit.tau_p_high
            vals$tau_p0_units <- tempunits
            
            parBlock(
              header = shiny::fluidRow(
                style = paste("margin-bottom: -14px;"),
                actionButton(
                  inputId = ns("selectHelp_taup"),
                  icon = icon("circle-question"),
                  label = NULL,
                  style = paste("background-color: #fff;",
                                "color: black;",
                                "padding: 0;")) %>%
                  bsplus::bs_attach_modal(id_modal = "modal_taup_select"),
                br(),
                wrap_none("Position autocorrelation ",
                          "(\u03C4", tags$sub("p"), ")")
              ),
              value =
                paste(scales::label_comma(
                  accuracy = .1)(vals$tau_p0),
                  vals$tau_p0_units),
              subtitle =
                paste(
                  ifelse(vals$tau_p0_min == 0,
                         "0",
                         scales::label_comma(
                           accuracy = .1)(vals$tau_p0_min)),
                  "—",
                  scales::label_comma(
                    accuracy = .1)(vals$tau_p0_max)))
          }
          
        } # end of if() statement
    }) # end of renderUI
    
    output$selectBlock_tauv <- shiny::renderUI({
      req(vals$tmpid, vals$fit0)
      
      if(vals$tmpid == "Simulated individual") {
        NULL } else {
          
          sum.fit <- summary(vals$fit0)
          tempnames <- rownames(sum.fit$CI)
          
          if(!length(grep('velocity', tempnames))) { NULL
          } else {
            
            tempunits <-
              ( tempnames[grep('velocity', tempnames)] %>%
                  extract_units() )
            
            fit.tau_v <- sum.fit$CI[
              grep('velocity', tempnames), 2]
            fit.tau_v_low <- sum.fit$CI[
              grep('velocity', tempnames), 1]
            fit.tau_v_high <- sum.fit$CI[
              grep('velocity', tempnames), 3]
            
            vals$tau_v0 <- fit.tau_v
            vals$tau_v0_min <- fit.tau_v_low
            vals$tau_v0_max <- fit.tau_v_high
            vals$tau_v0_units <- tempunits
            
            parBlock(
              header = shiny::fluidRow(
                style = paste("margin-bottom: -14px;"),
                actionButton(
                  inputId = ns("selectHelp_tauv"),
                  icon = icon("circle-question"),
                  label = NULL,
                  style = paste("background-color: #fff;",
                                "color: black;",
                                "padding: 0;")) %>%
                  bsplus::bs_attach_modal(id_modal = "modal_tauv_select"),
                br(),
                wrap_none("Velocity autocorrelation ",
                          "(\u03C4", tags$sub("v"), ")")
              ),
              value =
                paste(scales::label_comma(
                  accuracy = .1)(fit.tau_v), tempunits),
              subtitle =
                paste(
                  ifelse(fit.tau_v_low == 0,
                         "0",
                         scales::label_comma(
                           accuracy = .1)(fit.tau_v_low)),
                  "—",
                  scales::label_comma(
                    accuracy = .1)(fit.tau_v_high)))
          }
          
        } # end of if() statement
    }) # end of renderUI
    
    ## Spatial parameters: ----------------------------------------------
    
    output$selectBlock_sigma <- shiny::renderUI({
      req(vals$tmpid, vals$fit0)
      
      if(vals$tmpid == "Simulated individual") {
        NULL } else {
          
          vals$sigma0 <- ctmm:::var.covm(vals$fit0$sigma, ave = T)
          vals$sigma0_units <- "m^2"
          
          sig <- fix_unit(vals$sigma0, unit = "m^2",
                          convert = TRUE, ui = TRUE)
          
          parBlock(
            header = shiny::fluidRow(
              style = paste("margin-bottom: -14px;"),
              actionButton(
                inputId = ns("selectHelp_sigma"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_sigma_select"),
              br(),
              span(HTML("Semi-variance (\u03C3)"))
            ),
            value = span(HTML("&nbsp;", sig$value, sig$unit)),
            subtitle =
              paste(
                ifelse(vals$sigma0_min == 0,
                       "0",
                       scales::label_comma(
                         accuracy = .1)(vals$sigma0_min)),
                "—",
                scales::label_comma(
                  accuracy = .1)(vals$sigma0_max)))
          
        } # end of if() statement
    }) # end of renderUI
    
    ##  Tracking regime: --------------------------------------------------
    
    output$selectInfo_dur <- shiny::renderUI({
      req(vals$data0)
      
      dat <- summary(vals$data0)
      nms <- names(dat)
      
      dur <- as.numeric(dat[grep('sampling period', nms)])
      dur_unit <- nms[grep('sampling period', nms)] %>%
        extract_units()
      
      out <- fix_unit(dur, dur_unit)
      parBlock(header = "Sampling duration",
               value = paste(out[1], out[2]))
      
    }) # end of renderUI // selectInfo_dur
    
    output$selectInfo_dti <- shiny::renderUI({
      req(vals$data0)
      
      dat <- summary(vals$data0)
      nms <- names(dat)
      
      dti <- as.numeric(dat[grep('sampling interval', nms)])
      dti_unit <- nms[grep('sampling interval', nms)] %>%
        extract_units()
      
      out <- fix_unit(dti, dti_unit)
      parBlock(header = "Sampling interval",
               value = paste(out[1], out[2]),
               subtitle = "between fixes")
      
    }) # end of renderUI // selectInfo_dti
    
    ## Sample sizes: ----------------------------------------------------
    
    output$selectBlock_n <- shiny::renderUI({
      req(vals$data0)
      
      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data0),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)
      
    }) # end of renderUI // selectBlock_n (absolute sample size)
    
    output$selectBlock_Narea <- shiny::renderUI({
      req(vals$fit0)
      
      nms <- names(summary(vals$fit0)$DOF)
      N <- summary(vals$fit0)$DOF[grep('area', nms)][[1]]
      n <- nrow(vals$data0)
      vals$N1 <- N
      
      value <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")
      
      sampleBlock(
        number = value,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)
      
    }) # end of renderUI // selectBlock_Narea (effective)
    
    output$selectBlock_Nspeed <- shiny::renderUI({
      req(vals$fit0)
      
      nms <- names(summary(vals$fit0)$DOF)
      N <- summary(vals$fit0)$DOF[grep('speed', nms)][[1]]
      n <- nrow(vals$data0)
      vals$N2 <- N
      
      value <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")
      
      sampleBlock(
        number = value,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = wrap_none("(N", tags$sub("speed"), ")"),
        rightBorder = FALSE,
        marginBottom = FALSE)
      
    }) # end of renderUI // selectBlock_Nspeed (effective)
    
    # MODALS & HELP -----------------------------------------------------
    
    observe({
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Movement models or processes:",
          
          reactable::reactableOutput(ns("dataTable_processes")),
          
          footer = tagList(
            modalButton("Dismiss")
          ),
          size = "l"))
      
    }) %>% bindEvent(input$selectHelp_mods)
    
    output$dataTable_processes <- reactable::renderReactable({
      
      mods <- movedesign::movmods
      nm <- sub('(^\\w+)\\s.+','\\1', summary(vals$fit0)$name[1])
      
      if(is.null(match(nm, mods$name_short))) {
        preselected_mod <- NULL
      } else {
        preselected_mod <- match(nm, mods$name_short)
      }
      df0 <- mods %>% dplyr::select(!name_short)
      
      reactable::reactable(
        df0,
        searchable = TRUE,
        highlight = TRUE,
        # selection = "single",
        defaultSelected = preselected_mod,
        defaultColDef =
          reactable::colDef(
            headerClass = "rtable_header",
            align = "left"),
        columns = list(
          name = reactable::colDef(
            name = "Movement process",
            minWidth = 195),
          
          tau_p = reactable::colDef(
            minWidth = 60,
            name = paste0("\u03C4","\u209A"),
            cell = reactable::JS(
              paste0("function(cellInfo) {
                // Render as an X mark or check mark
                return cellInfo.value === 'No' ? '\u274c No' : ",
                     "'\u2714\ufe0f Yes'}"))),
          
          tau_v = reactable::colDef(
            minWidth = 60,
            name = paste0("\u03C4","\u1D65"),
            cell = reactable::JS(
              paste0("function(cellInfo) {
                // Render as an X mark or check mark
                return cellInfo.value === 'No' ? '\u274c No' : ",
                     "'\u2714\ufe0f Yes'}"))),
          
          hrange = reactable::colDef(
            minWidth = 80,
            name = "Home range",
            cell = reactable::JS(
              paste0("function(cellInfo) {
                // Render as an X mark or check mark
                return cellInfo.value === 'No' ? '\u274c No' : ",
                     "'\u2714\ufe0f Yes'}"))),
          
          pars = reactable::colDef(
            name = "Parameterization")
        ),
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(
            backgroundColor = "#eee",
            boxShadow = "inset 2px 0 0 0 #009da0")))
      
    }) # end of renderReactable // dataTable_processes
    
    # Additional information: -------------------------------------------
    
    output$selectUI_time <- renderText({
      req(vals$selectOut_time)
      
      paste0("Model fitting took approximately ",
             round(vals$selectOut_time, 1), " minutes.")
      
    }) # end of renderText // selectOut_time
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_data_select_ui("tab_data_select_1")

## To be copied in the server
# mod_tab_data_select_server("tab_data_select_1")
