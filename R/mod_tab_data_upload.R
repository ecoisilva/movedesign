#' tab_data_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom utils read.csv
#'
mod_tab_data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      # Introduction: -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          shinydashboardPlus::box(
            title = span("Upload movement data:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "file-csv",
                                   height = "21px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "var(--sea-dark)"),
            id = ns("upload_intro"),
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
                
                "Upload a dataset as a .csv file with at least",
                "four variables:",
                wrap_none(span("animal ID", class = "cl-dgr"), ","),
                span("x", class = "cl-dgr"), "and",
                span("y", class = "cl-dgr"), "coordinates, and",
                wrap_none(span("timestamp", class = "cl-dgr"), "."),
                br()),
              
              p(style = "text-align: center;",
                "First, browse and choose the appropriate",
                span(".csv", class = "cl-sea"), "file.",
                br(), "Then click",
                icon("circle-check", class = "cl-mdn"),
                span("Validate", class = "cl-mdn"), "and",
                icon("paper-plane", class = "cl-mdn"),
                wrap_none(span("Extract", class = "cl-mdn"), "."))
              
            ) # end of column (text)
            
          ) # end of box // upload_intro
      ), # end of div (top row)
      
      # [left column] ----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-4 col-md-4 col-lg-3",
          
          # Submit file: --------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Upload file:", class = "ttl-box_solid"),
            id = ns("uploadBox_file"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = TRUE,
            
            fileInput(
              inputId = ns("file_csv"),
              label = NULL,
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
            tags$hr(),
            
            shinyWidgets::prettyCheckbox(
              inputId = ns("file_header"),
              label = span("Header", class = "cl-blk"),
              status = "primary",
              animation = "tada",
              outline = TRUE,
              value = TRUE), br(),
            
            shinyWidgets::radioGroupButtons(
              inputId = ns("file_sep"),
              label = "Separator",
              choices = c("Comma (,)" = ",",
                          "Semicolon (;)" = ";",
                          "Tab" = "\t"),
              selected = ",",
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-square-o", 
                            style = "color: steelblue")),
              direction = "vertical",
              width = "100%"),
            
            shinyWidgets::radioGroupButtons(
              inputId = ns("file_quote"),
              label = "Quote",
              choices = c("None" = "",
                          "Double quote (\")" = '"',
                          "Single quote (\')" = "'"),
              selected = '"',
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-square-o", 
                            style = "color: steelblue")),
              direction = "vertical",
              width = "100%"),
            
            footer = shiny::actionButton(
              inputId = ns("confirm_upload"),
              label = "Confirm",
              icon =  icon("upload"),
              width = "100%")
            
          ), # end of box // uploadBox_file
          
          # Select species & individual: ----------------------------------
          
          shinydashboardPlus::box(
            title = span("Dataset:", class = "ttl-box_solid"),
            id = ns("uploadBox_species"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = TRUE,
            
            shiny::textInput(
              inputId = ns("sp_uploaded"),
              label = "Scientific name:",
              placeholder = "Add species name"),
            
            shiny::selectizeInput(
              inputId = ns("id_uploaded"),
              label = "Individual:",
              choices = "",
              selected = NULL,
              multiple = FALSE,
              options = list(
                placeholder = "Pick an individual",
                onInitialize = I('function() { this.setValue(""); }'))
            ),
            
            selectInput(inputId = ns("uploadVar_x"),
                        label = "X coordinate:",
                        selected = "x",
                        choices = ""),
            selectInput(inputId = ns("uploadVar_y"),
                        label = "Y coordinate:",
                        selected = "y",
                        choices = ""),
            selectInput(inputId = ns("uploadVar_t"),
                        label = "Datetime:",
                        selected = "timestamp",
                        choices = ""),
            
            footer = splitLayout(
              uiOutput(ns("uploadUI_validate")),
              actionButton(
                inputId = ns("uploadButton_extract"),
                icon =  icon("paper-plane"),
                label = "Extract",
                width = "100%",
                class = "btn-primary")
            ) # end of footer
            
          ), # end of box // uploadBox_species
          
          # Tracking regime: ----------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Tracking regime:", class = "ttl-box"),
            id = ns("uploadBox_regime"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            fluidRow(
              column(width = 12, uiOutput(ns("uplBlock_dur"))),
              column(width = 12, uiOutput(ns("uplBlock_dti")))
            ) # end of fluidRow
            
          ) # end of box // uploadBox_regime
      ), # end of div (left column)
      
      # [right column] ----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-8 col-md-8 col-lg-9",
          
          # Visualization: ------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Data vizualization:", class = "ttl-box"),
            id = ns("uploadBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            mod_comp_viz_ui("comp_viz_uploaded")
            
          ) # end of box // uploadBox_viz
          
      ), # end of column (right)
      
      # [bottom column] ---------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          # Displaying relevant information: ------------------------------
          
          div(class = "col-lg-6 no-padding-left",
              shinydashboardPlus::box(
                title = span("Displaying parameters:", class = "ttl-box"),
                id = ns("uploadBox_parameters"),
                width = NULL,
                solidHeader = FALSE,
                
                ## Extracted parameters:
                uiOutput(ns("uploadUI_parameters"))
                
              ) # end of box // uploadBox_parameters
          ), # end of div
          
          ## Sample sizes:
          
          div(class = "col-lg-6 no-padding-right",
              shinydashboardPlus::box(
                title = span("Displaying sample sizes:",
                             class = "ttl-box"),
                id = ns("uploadBox_sizes"),
                width = NULL,
                solidHeader = FALSE,
                
                fluidRow(
                  column(width = 4, uiOutput(ns("uplBlock_n"))),
                  column(width = 4, mod_blocks_ui(ns("uplBlock_Narea"))),
                  column(width = 4, mod_blocks_ui(ns("uplBlock_Nspeed")))
                ) # end of fluidRow
                
              ) # end of box // uploadBox_sizes
          ), # end of div
          
          # Additional information: ---------------------------------------
          
          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("uploadBox_misc"),
            width = NULL, solidHeader = FALSE,
            
            verbatimTextOutput(outputId = ns("uploadUI_time"))
            
          ) # end of box // uploadBox_misc
      ) # end of column (bottom)
      
    ), # end of fluidRow
    
    # MODALS: -------------------------------------------------------------
    
    create_modal(var = "taup", id = "upload"),
    create_modal(var = "tauv", id = "upload"),
    create_modal(var = "sigma", id = "upload"),
    NULL
    
  ) # end of tagList
}

#' tab_data_upload Server Functions
#'
#' @noRd
mod_tab_data_upload_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    ## Match id for input, plot and table:
    
    observe({
      req(input$id_uploaded != "")
      vals$id <- input$id_uploaded
    }) %>% bindEvent(input$id_uploaded)
    
    observe({
      vals$id <- vals$table_selection
    })
    observe({
      vals$id <- vals$plot_selection
    })
    
    observe({
      dataList <- dataset_uploaded()
      
      shiny::updateSelectizeInput(
        session,
        inputId = "id_uploaded",
        label = NULL,
        choices = names(dataList))

    }) %>% # end of observe,
      bindEvent(input$confirm_upload)

    observe({
      req(vals$dataList, vals$active_tab == 'data_upload')
      
      shiny::updateSelectizeInput(
        session,
        inputId = "id_uploaded",
        label = NULL,
        choices = names(vals$dataList),
        selected = vals$id)
      
    }) %>% # end of observe,
      bindEvent(vals$id)
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide all boxes at start: -------------------------------------------
    
    boxnames <- c("species",
                  "regime",
                  "parameters",
                  "sizes",
                  "misc")
    
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("uploadBox_", boxnames[i]))
    }
    
    ## Render validate button: --------------------------------------------
    
    output$uploadUI_validate <- renderUI({
      
      out <- shiny::actionButton(
        inputId = ns("validate_upload"),
        icon =  icon("wand-magic-sparkles"),
        label = "Validate",
        width = "100%")
      
      if (!is.null(vals$is_valid)) {
        if (vals$is_valid) 
          out <- shiny::actionButton(
            inputId = ns("validate_upload"),
            icon =  icon("circle-check"),
            label = "Validated!",
            width = "100%",
            class = "btn-info")
      }
      
      return(out)
      
    }) # end of renderUI, "uploadUI_validate"
    
    ## Render parameters box: ---------------------------------------------
    
    output$uploadUI_parameters <- renderUI({
      req(vals$data_type == "uploaded")
      
      tagList(
        column(
          align = "center", width = 12,
          
          p("These parameters have been extracted from",
            "individual", span(vals$tmpid, class = "cl-sea-d"),
            "and species",
            wrap_none(span(vals$tmpsp, class = "cl-sea-d"), "."),
            "They will only update if you change the",
            "individual and/or species selected, and then",
            "click the buttons",
            icon("circle-check", class = "cl-mdn"),
            span("Validate", class = "cl-mdn"), "and",
            icon("paper-plane", class = "cl-mdn"),
            wrap_none("Extract", css = "cl-mdn", end = "."))
          
        ), # end of column (for text)
        
        column(width = 12, uiOutput(ns("uplBlock_model"))),
        
        fluidRow(
          column(width = 6, mod_blocks_ui(ns("uplBlock_taup"))),
          column(width = 6, mod_blocks_ui(ns("uplBlock_tauv")))),
        
        fluidRow(
          column(width = 6, mod_blocks_ui(ns("uplBlock_sigma"))),
          column(width = 6, mod_blocks_ui(ns("uplBlock_speed"))))
        
      ) # end of out_ui
      
    }) # end of renderUI, "uploadUI_parameters"
    
    ## If data available, update variable inputs: -------------------------
    
    observe({
      req(vals$data_type == "uploaded")
      
      updateSelectInput(
        session, inputId = "uploadVar_x",
        label = "X coordinate:",
        choices = names(vals$data0),
        selected = ifelse(!is.null(vals$data0$"x"),
                          "x", "longitude"))
      updateSelectInput(
        session, inputId = "uploadVar_y",
        label = "Y coordinate:",
        choices = names(vals$data0),
        selected = ifelse(!is.null(vals$data0$"y"),
                          "y", "latitude"))
      updateSelectInput(
        session, inputId = "uploadVar_t",
        label = "Datetime:",
        choices = names(vals$data0),
        selected = ifelse(!is.null(vals$data0$"timestamp"),
                          "timestamp", "t"))
      
      vals$input_x <- input$uploadVar_x
      vals$input_y <- input$uploadVar_y
      vals$input_t <- input$uploadVar_t
      
    }) %>% # end of observe,
      bindEvent(vals$data0)
    
    # ALERTS --------------------------------------------------------------
    
    ## During validation:
    
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
      
      req(vals$which_question,
          vals$data0)
      
      if (input$sp_uploaded == "") {
        
        # If no species name is written down:
        
        msg_log(
          style = "danger",
          message = paste0("Species name ",
                           msg_danger("not found"), "."),
          detail = "Please input the name of your study species.")
        
        shinyalert::shinyalert(
          title = "Missing species name",
          text = tagList(span(
            'Please input the name of',
            'your study species, then click the',
            icon("circle-check", class = "cl-mdn"),
            span("Validate", class = "cl-mdn"),
            'button again.')),
          html = TRUE,
          size = "xs")
        
      } else {
        vals$species_binom <- input$sp_uploaded
        vals$needs_fit <- TRUE
        
      } # end of if ()
      
    }) %>% # end of observe,
      bindEvent(input$validate_upload)
    
    ## After clicking "Extract" button:
    
    observe({
      req(vals$which_question)
      shinyjs::show(id = "uploadBox_regime")
      
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
      bindEvent(input$uploadButton_extract)
    
    # OPERATIONS ----------------------------------------------------------
    ## 1. Upload .cs.csv file with data: ----------------------------------
    
    ### 1.1. Read in file submission:
    
    reading_file <- reactive({
      if (is.null(input$file_csv)) return("")
      
      read.csv(file = input$file_csv$datapath,
               header = input$file_header,
               sep = input$file_sep,
               quote = input$file_quote)
    })
    
    dataset_uploaded <- reactive({
      reset_reactiveValues(vals) # reset vals between data tabs
      
      out_dataset <- NULL
      out_dataset <- reading_file()
      out_dataset <- tryCatch(ctmm::as.telemetry(out_dataset),
                              error = function(e) e)
      
      if (inherits(out_dataset, "error")) {
        msg_log(
          style = "danger",
          message = paste0("File is ",
                           msg_danger("not correctly formatted"), "."),
          detail = "May be missing one or more columns.")
        return(NULL)
      }
      
      req(out_dataset)
      
      msg_log(
        style = "success",
        message = paste0("File ",
                         msg_success("submitted"), "."),
        detail = "Please select one individual from this dataset.")
      
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
      
      shinyjs::show(id = "uploadBox_species")
      
      if (!input$uploadBox_file$collapsed) {
        shinydashboardPlus::updateBox("uploadBox_file",
                                      action = "toggle")
      }
      
      # Check number of individuals within dataset:
      
      dataList <- out_dataset
      if (names(out_dataset)[[1]] == "timestamp") {
        dataList <- list()
        dataList[[1]] <- out_dataset
        names(dataList) <- summary(as_tele_list(out_dataset))$identity
      }
      
      vals$dataList <- out_dataset
      vals$data_type <- "uploaded"
      vals$id <- NULL
      
      return(dataList)
      
    }) # end of reactive
    
    # 1.2. Subset data based on individual selection:
    
    observe({
      req(vals$active_tab == 'data_upload', 
          vals$dataList, vals$id)
      
      if (names(vals$dataList)[[1]] == "timestamp") {
        df_subset <- vals$dataList
      } else { df_subset <- vals$dataList[[vals$id]] }
      
      vals$data0 <- df_subset
      vals$is_valid <- FALSE
      
      shinyjs::show(id = "uploadVar_x")
      shinyjs::show(id = "uploadVar_y")
      shinyjs::show(id = "uploadVar_t")
      
    }) # end of observe
    
    ## 2. Validate data: --------------------------------------------------
    
    timing_fit <- shiny::reactive({
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = span(
          style = "font-size: 18px;",
          span("Calculating", style = "color: #797979;"),
          HTML(paste0(span("run time", class = "cl-sea"),
                      span("...", style = "color: #797979;")))
        )
      )
      
      out <- guesstimate_time(vals$data0, parallel = vals$parallel)
      
      shinybusy::remove_modal_spinner()
      return(out)
      
    }) %>% # end of reactive, timing_fit()
      bindCache(c(vals$id, vals$species_binom))
    
    observe({
      req(vals$which_question,
          vals$data0, vals$species_binom)
      
      ### Model fitting:
      
      expt <- timing_fit()
      confirm_time <- NULL
      
      if ((expt$max %#% expt$unit) > (15 %#% "minutes")) {
        
        out <- fix_unit(expt$max, expt$unit, convert = TRUE)
        
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            vals$confirm_time <- x
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
      
      req(confirm_time)
      
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("in progress"), "."),
        detail = "Please wait for model selection to finish:")
      
      loading_modal("Selecting movement model",
                    runtime = expt$range, for_time = TRUE)
      
      start <- Sys.time()
      guess0 <- ctmm::ctmm.guess(vals$data0, interactive = FALSE)
      inputList <- list(list(vals$data0, guess0))
      
      vals$fit0 <- NULL
      fit0 <- tryCatch(
        par.ctmm.select(inputList, parallel = vals$parallel),
        error = function(e) e)
      
      time_fit0 <- difftime(Sys.time(), start, units = "sec")
      vals$time[1] <- vals$time[1] + time_fit0[[1]]
      
      if (!inherits(fit0, "error")) { 
        vals$fit0 <- fit0
      } else {
        msg_log(
          style = "danger",
          message = paste0("Model fit ", msg_danger("failed"), "."),
          detail = "May be due to low absolute sample size.")
      }
      req(vals$fit0)
      
      msg_log(
        style = 'success',
        message = paste0("Model fit ",
                         msg_success("completed"), "."),
        with_time = time_fit0)
      
      vals$needs_fit <- FALSE
      shinybusy::remove_modal_spinner()
      
      ### Set up for validation:
      
      taup <- extract_pars(vals$fit0, name = "position")
      tauv <- extract_pars(vals$fit0, name = "velocity")
      
      ### Validate based on research questions:
      
      vals$is_valid <- TRUE
      if (is.null(taup) & is.null(tauv)) {
        
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
            "estimation."
          )),
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
            text = tagList(span(
              "No significant signature of the animal's",
              span("position autocorrelation", class = "cl-dgr"),
              "parameter remains in this dataset.",
              "Please select a different individual or dataset to",
              "proceed with", span("home range", class = "cl-dgr"),
              "estimation."
            )),
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
            text = tagList(span(
              "No significant signature of the animal's",
              span("velocity autocorrelation", class = "cl-dgr"),
              "parameter remains in this dataset.",
              "Please select a different individual or dataset to",
              "proceed with", span("distance/speed", class = "cl-dgr"),
              "estimation."
            )),
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
                        msg_success(vals$species_binom),
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
          positionClass = "toast-bottom-right"))
      
      # if (!input$select_intro$collapsed &&
      #    vals$tour_active) { NULL } else {
      #      shinydashboardPlus::updateBox("select_intro",
      #                                    action = "toggle")
      #    }
      
    }) %>% # end of observe,
      bindEvent(input$validate_upload)
    
    # PARAMETERS ----------------------------------------------------------
    ## Extract spatial variance, timescales, etc.: ------------------------
    
    extract_sigma <- reactive({
      if (is.null(vals$var_fraction)) frac <- .65
      else frac <- vals$var_fraction
      
      svf <- extract_svf(vals$data0, fraction = frac)
      vals$svf <- svf
      
      return(extract_pars(obj = vals$fit0, 
                          name = "sigma", 
                          data = vals$data0,
                          fraction = frac))
      
    }) # end of reactive
    
    observe({
      req(vals$which_question,
          vals$data_type == "uploaded",
          vals$data0, vals$fit0, vals$is_valid)
      
      shinyjs::show(id = "uploadBox_parameters")
      shinyjs::show(id = "uploadBox_regime")
      shinyjs::show(id = "uploadBox_sizes")
      
      vals$sigma0 <- extract_sigma()
      vals$tau_p0 <- extract_pars(vals$fit0, name = "position")
      vals$tau_v0 <- extract_pars(vals$fit0, name = "velocity")
      vals$speed0 <- extract_pars(vals$fit0, name = "speed")
      
      vals$tmpsp <- vals$species_binom
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
      
      # if (!input$uploadBox_species$collapsed) {
      #   shinydashboardPlus::updateBox("uploadBox_species",
      #                                 action = "toggle")
      # }
      
      shinyjs::hide(id = "uploadVar_x")
      shinyjs::hide(id = "uploadVar_y")
      shinyjs::hide(id = "uploadVar_t")
      
    }) %>% # end of observe, then:
      bindEvent(input$uploadButton_extract)
    
    # BLOCKS --------------------------------------------------------------
    ## Movement process: --------------------------------------------------
    
    output$uplBlock_model <- shiny::renderUI({
      req(vals$tmpid)

      if (vals$tmpid == "Simulated individual") {
        NULL } else {
          sum.fit <- summary(vals$fit0)

          parBlock(
            header = shiny::fluidRow(
              style = paste("margin-bottom: -14px;"),
              actionButton(
                inputId = ns("uploadHelp_mods"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;")),
              br(), "Movement process"),
            value = sum.fit$name[1])

        } # end of if () statement
    }) # end of renderUI, "uplBlock_model"
    
    ## Timescale parameters: ----------------------------------------------
    
    observe({
      req(vals$tau_p0)
      
      mod_blocks_server(
        id = "uplBlock_taup", 
        vals = vals, type = "tau", name = "tau_p0",
        input_name = list(
          chr = "select_taup0",
          html = wrap_none("Position autocorrelation ",
                           "(\u03C4", tags$sub("p"), ")")),
        input_modal = "modal_taup_upload")
    })
    
    observe({
      req(vals$tau_v0)
      
      mod_blocks_server(
        id = "uplBlock_tauv",
        vals = vals, type = "tau", name = "tau_v0",
        input_name = list(
          chr = "select_tauv0",
          html = wrap_none("Velocity autocorrelation ",
                           "(\u03C4", tags$sub("v"), ")")),
        input_modal = "modal_tauv_upload")
    })
    
    ## Spatial variance: --------------------------------------------------
    
    observe({
      req(vals$sigma0)
      
      mod_blocks_server(
        id = "uplBlock_sigma",
        vals = vals, type = "sigma", name = "sigma0",
        input_name = list(
          chr = "select_sigma0",
          html = span(HTML("Semi-variance (\u03C3)"))),
        input_modal = "modal_sigma_upload")
    })
    
    ## Speed: -------------------------------------------------------------
    
    observe({
      req(vals$speed0)
      
      mod_blocks_server(
        id = "uplBlock_speed",
        vals = vals, type = "speed", name = "speed0",
        input_name = list(
          chr = "select_speed0",
          html = span(HTML("Velocity (\u03BD)"))),
        input_modal = "modal_speed_upload")
    })
    
    
    ## Tracking regime: ---------------------------------------------------
    
    output$uplBlock_dur <- shiny::renderUI({
      req(vals$data0)
      
      dur <- extract_pars(vals$data0, name = "period")
      out <- fix_unit(dur$value, dur$unit)
      
      parBlock(header = "Sampling duration",
               value = paste(out[1], out[2]))
      
    }) # end of renderUI, "uplBlock_dur"
    
    output$uplBlock_dti <- shiny::renderUI({
      req(vals$data0)
      
      dti <- extract_pars(vals$data0, name = "interval")
      out <- fix_unit(dti$value, dti$unit)
      
      parBlock(header = "Sampling interval",
               value = paste(out[1], out[2]),
               subtitle = "between fixes")
      
    }) # end of renderUI, "uplBlock_dti"
    
    ## Sample sizes: ------------------------------------------------------
    
    output$uplBlock_n <- shiny::renderUI({
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
        id = "uplBlock_Narea", 
        vals = vals, data = vals$data0, fit = vals$fit0,
        type = "N", name = "area")
      
      mod_blocks_server(
        id = "uplBlock_Nspeed", 
        vals = vals, data = vals$data0, fit = vals$fit0,
        type = "N", name = "speed")
      
    }) # end of observe
    
    # MODALS & HELP -----------------------------------------------------
    
    observe({
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Movement models or processes:",
          
          reactable::reactableOutput(ns("upTable_models")),
          
          footer = tagList(modalButton("Dismiss")),
          size = "l"))
      
    }) %>% # end of observe,
      bindEvent(input$uploadHelp_mods)
    
    output$upTable_models <- reactable::renderReactable({
      req(vals$fit0)
      
      mods <- movedesign::movmods
      
      nm <- sub('(^\\w+)\\s.+','\\1', 
                summary(vals$fit0)$name[1])
      
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
      
    }) # end of renderReactable // upTable_models
    
    # MISC ----------------------------------------------------------------
    
    output$uploadUI_time <- renderText({
      req(vals$uploadOut_time) # no longer listed
      
      paste0("Model fitting took approximately ",
             round(vals$uploadOut_time, 1), " minutes.")
      
    }) # end of renderText // uploadOut_time
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_data_upload_ui("tab_data_upload_1")

## To be copied in the server
# mod_tab_data_upload_server("tab_data_upload_1")
