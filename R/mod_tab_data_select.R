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
          div(class = "col-lg-6 no-padding-left",
              uiOutput(ns("selectUI_parameters"))
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
                  column(width = 4, uiOutput(ns("selectBlock_n"))),
                  column(width = 4, uiOutput(ns("selectBlock_Narea"))),
                  column(width = 4, uiOutput(ns("selectBlock_Nspeed"))),
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
      
    }) # end of renderUI // selectUI_validate
    
    # SELECT DATA -------------------------------------------------------
    ## Select species: --------------------------------------------------
    
    ### 1.1. Load species data from the 'ctmm' package:
    
    observe({
      req(vals$active_tab == 'data_select')
      
      vals$data0 <- NULL
      vals$data_type <- "selected"
      
      vals$species <- input$sp_selected
      utils::data(list = vals$species, package = "ctmm")
      
      tmp <- NULL
      if (vals$species == "buffalo") tmp <- buffalo
      if (vals$species == "pelican") tmp <- pelican
      if (vals$species == "coati") tmp <- coati
      if (vals$species == "jaguar") tmp <- jaguar
      if (vals$species == "wolf") tmp <- wolf
      if (vals$species == "gazelle") tmp <- gazelle
      if (vals$species == "turtle") tmp <- turtle
      
      req(tmp)
      
      # Check if data is anonymized:
      
      if (!("timestamp" %in% names(tmp[[1]]))) {
        
        tmp <- pseudonymize(tmp)
        vals$is_pseudonymized <- TRUE
        
        shinyFeedback::showToast(
          type = "success",
          title = "Data is anonymized...",
          message = "Origin location and time added.",
          .options = list(
            timeOut = 2500,
            progressBar = FALSE,
            closeButton = TRUE,
            preventDuplicates = TRUE,
            positionClass = "toast-bottom-right")
        )
        
        msg_log(
          style = "success",
          message = paste0("Data pseudonymization ",
                           msg_success("completed"), "."),
          detail = "Origin location and time added.")
        
      } else { vals$is_pseudonymized <- FALSE }
      
      vals$dataList <- tmp
      
      shinyWidgets::updatePickerInput(
        session,
        inputId = "id_selected",
        label = NULL,
        choices = names(vals$dataList))
      
    }) %>% # end of observe
      bindEvent(input$sp_selected)
    
    # 1.2. Subset data based on individual selection:

    observe({
      req(vals$dataList,
          vals$id,
          vals$active_tab == 'data_select')
      
      vals$is_valid <- FALSE
      df_subset <- vals$dataList[[vals$id]]

      index <- ctmm_species %>% match(x = vals$species)
      vals$species_common <- names(ctmm_species[index])
      vals$species_binom <- names(ctmm_species_binom[index])
      vals$data0 <- df_subset
      
      # shinyjs::enable("selectButton_extract")
      
    }) %>% # end of observe,
      bindEvent(input$id_selected, ignoreInit = TRUE)

    ## Validate data: ---------------------------------------------------

    observe({
      if (is.null(vals$which_question)) {

        shinyalert::shinyalert(
          title = "No research goal selected",
          text = span(
            "Please select a research goal in the",
            icon("house", class = "cl-blk"),
            span("Home", class = "cl-blk"),
            "tab before proceeding."),
          html = TRUE,
          size = "xs")
      }
      
      req(vals$which_question,
          vals$data0, vals$id)
      
      ### Model fitting:

      guess <- reactive({
        ctmm::ctmm.guess(vals$data0, interactive = FALSE)
      }) %>% bindCache(vals$species_binom,
                       vals$id)
      
      vals$guess <- guess()

      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("found"), "..."))

      fitList <- readRDS(
        system.file("extdata",
                    paste0(vals$species, "_fitList.rds"),
                    package = "movedesign"))

      msg_log(
        style = "success",
        message = paste0("Model fit ",
                         msg_success("loaded"), "."))
      
      vals$fit0 <- fitList[[vals$id]][[1]]
      vals$needs_fit <- FALSE
      rm(fitList)

      ### Set up for validation:
      
      taup <- extract_pars(vals$fit0, par = "position")
      tauv <- extract_pars(vals$fit0, par = "velocity")

      ### Validate based on research questions:
      
      vals$is_valid <- TRUE
      if (is.null(taup) && is.null(tauv)) {
        
        shinyalert::shinyalert(
          type = "error",

          title = "Dataset invalid",
          text = span(
            "Data is",
            wrap_none(span("independent", class = "cl-dgr"), ","),
            "and no signature of autocorrelation parameters",
            "remains in this dataset.",
            "Please select a different individual or dataset to",
            "proceed with", span("home range", class = "cl-dgr"),
            "estimation."),

          confirmButtonText = "Dismiss",
          html = TRUE)
        
        vals$is_valid <- NULL
      }
      
      req(vals$is_valid)
      
      if ("Home range" %in% vals$which_question) {
        if (is.null(taup)) {
          
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
            message = paste("No signature of",
                            msg_danger("position autocorrelation"),
                            "found."),
            detail = "Select a different dataset to proceed.")
          
          vals$is_valid <- NULL
        }
      }

      if ("Speed & distance" %in% vals$which_question) {
        if (is.null(tauv)) {

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
            message = paste("No signature of",
                            msg_danger("velocity autocorrelation"),
                            "found."),
            detail = "Select a different dataset to proceed.")

          vals$is_valid <- NULL
        }
      }

      req(vals$is_valid)
      vals$input_x <- ifelse(!is.null(vals$data0$"x"),
                             "x", "longitude")
      vals$input_y <- ifelse(!is.null(vals$data0$"y"),
                             "y", "latitude")
      vals$input_t <- ifelse(!is.null(vals$data0$"timestamp"),
                             "timestamp", "t")

      msg_log(
        style = "success",
        message = paste0("Species and individual ",
                         msg_success("validated"), "."),
        detail = paste0("Species selected is the ",
                        msg_success(vals$species),
                        ", and the individual is ",
                        msg_success(vals$id), "."))
      
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

      # if (!input$select_intro$collapsed &&
      #    vals$tour_active) { NULL } else {
      #      shinydashboardPlus::updateBox("select_intro",
      #                                    action = "toggle")
      #    }

    }) %>% # end of observe,
      bindEvent(input$validate_select)

    # PARAMETERS --------------------------------------------------------
    # After clicking "Extract" button:

    observe({
      req(vals$which_question)
      shinyjs::show(id = "selectBox_regime")

      if (is.null(vals$is_valid)) {

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
        
      }

      req(vals$data_type == "selected",
          vals$data0, vals$is_valid)
      
      shinyjs::show(id = "selectBox_sizes")

      vals$tmpsp1 <- vals$species_common
      vals$tmpsp2 <- vals$species_binom
      vals$tmpid <- vals$id

      ## Extract semi-variance parameter: -------------------------------
      
      if (is.null(vals$var_fraction)) frac <- .65
      else frac <- vals$var_fraction

      svf <- extract_svf(vals$data0, fraction = frac)
      vals$svf <- svf
      vals$sigma0 <- extract_pars(obj = vals$fit0, 
                                  par = "sigma", 
                                  data = vals$data0,
                                  fraction = frac)
      
      ## Extract timescale and spatial parameters: ----------------------

      taup <- extract_pars(vals$fit0, par = "position")
      vals$tau_p0 <- taup

      tauv <- extract_pars(vals$fit0, par = "velocity")
      vals$tau_v0 <- tauv
      
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
              
              if (vals$tmpid == "Simulated individual") {
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

              } # end of if () statement
            }) # end of renderUI

          ), # end of column (for text)

          column(width = 12, uiOutput(ns("selectBlock_process"))),

          fluidRow(
            column(width = 6, uiOutput(ns("selectBlock_taup"))),
            column(width = 6, uiOutput(ns("selectBlock_tauv")))
          ),

          fluidRow(
            column(width = 6, uiOutput(ns("selectBlock_sigma"))),
            column(width = 6, uiOutput(ns("selectBlock_speed")))
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
          html = TRUE,
          text = span(
            "Proceed to the", br(),
            icon("stopwatch", class = "cl-mdn"),
            span('Tracking regime', class = "cl-mdn"), "tab."),
          size = "xs")
      }
      
    }) %>% # end of observe, then:
      bindEvent(input$selectButton_extract)

    # BLOCKS ------------------------------------------------------------
    ## Movement process: ------------------------------------------------

    output$selectBlock_process <- shiny::renderUI({
      req(vals$tmpid, vals$fit0)

      if (vals$tmpid == "Simulated individual") {
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

        } # end of if () statement
    }) # end of renderUI

    ## Timescale parameters: --------------------------------------------

    output$selectBlock_taup <- shiny::renderUI({
      req(vals$tmpid, vals$fit0, vals$tau_p0)

      if (vals$tmpid == "Simulated individual") {
        NULL } else {

          par <- vals$tau_p0
          tau_p0 <- fix_unit(par["est", 1], par$unit[1])
          tau_p0_min <- scales::label_comma(.1)(par["low", 1])
          tau_p0_max <- scales::label_comma(.1)(par["high", 1])

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
            value = paste(tau_p0$value, tau_p0$unit),
            subtitle = paste(ifelse(tau_p0_min == 0,
                                    "0", tau_p0_min),
                             "\u2014", tau_p0_max))

        } # end of if () statement
    }) # end of renderUI

    output$selectBlock_tauv <- shiny::renderUI({
      req(vals$tmpid, vals$fit0, vals$tau_v0)

      if (vals$tmpid == "Simulated individual") {
        NULL } else {

          par <- vals$tau_v0
          tau_v0 <- fix_unit(par["est", 1], par$unit[1])
          tau_v0_min <- scales::label_comma(.1)(par["low", 1])
          tau_v0_max <- scales::label_comma(.1)(par["high", 1])

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
            value = paste(tau_v0$value, tau_v0$unit),
            subtitle = paste(ifelse(tau_v0_min == 0,
                                    "0", tau_v0_min),
                             "\u2014", tau_v0_max))

        } # end of if () statement
    }) # end of renderUI

    ## Other parameters: --------------------------------------------------
    
    output$selectBlock_sigma <- shiny::renderUI({
      req(vals$tmpid, vals$fit0, vals$sigma0)

      if (vals$tmpid == "Simulated individual") {
        NULL } else {

          par <- vals$sigma0
          sig <- fix_unit(par$value[2], unit = par$unit[2],
                           convert = TRUE, ui = TRUE)
          sig_lci <- fix_unit(par$value[1], unit = par$unit[1],
                               convert = TRUE, ui = TRUE)
          sig_uci <- fix_unit(par$value[3], unit = par$unit[3],
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
            subtitle =  paste(ifelse(sig_lci$value == 0,
                                     "0", sig_lci$value),
                              "\u2014", sig_uci$value))

        } # end of if () statement
    }) # end of renderUI

    output$selectBlock_speed <- shiny::renderUI({
      req(vals$tmpid, vals$fit0)

      if (vals$tmpid == "Simulated individual") {
        NULL } else {

          par <- extract_pars(vals$fit0, par = "speed")
          speed <- fix_unit(par["est", 1], par$unit[1])
          speed_lci <- scales::label_comma(.1)(par["low", 1])
          speed_uci <- scales::label_comma(.1)(par["high", 1])
          unit <- speed$unit %>% abbrv_unit()

          parBlock(
            header = shiny::fluidRow(
              style = paste("margin-bottom: -14px;"),
              actionButton(
                inputId = ns("selectHelp_speed"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_speed_select"),
              br(),
              span(HTML("Velocity (\u03BD)"))
            ),
            value = span(HTML("&nbsp;", speed$value, unit)),
            subtitle =  paste(ifelse(speed_lci == 0,
                                     "0", speed_lci),
                              "\u2014", speed_uci))

        } # end of if () statement
    }) # end of renderUI

    ##  Tracking regime: --------------------------------------------------

    output$selectInfo_dur <- shiny::renderUI({
      req(vals$data0)

      dur <- extract_pars(vals$data0, par = "period")
      out <- fix_unit(dur$value, dur$unit)

      parBlock(header = "Sampling duration",
               value = paste(out[1], out[2]))

    }) # end of renderUI // selectInfo_dur

    output$selectInfo_dti <- shiny::renderUI({
      req(vals$data0)

      dti <- extract_pars(vals$data0, par = "interval")
      out <- fix_unit(dti$value, dti$unit)

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

      N <- extract_dof(vals$fit0, par = "area")
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

      N <- extract_dof(vals$fit0, par = "speed")
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
      nm <- sub('(^\\w+)\\s.+','\\1', 
                summary(vals$fit0)$name[1])

      if (is.null(match(nm, mods$name_short))) {
        preselected_mod <- NULL
      } else {
        preselected_mod <- match(nm, mods$name_short)
      }
      
      out <- mods %>% dplyr::select(!.data$name_short)

      reactable::reactable(
        out,
        searchable = TRUE,
        highlight = TRUE,
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
