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

      div(class = div_column_main,

          shinydashboardPlus::box(

            title = span("Submit movement data:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "file-arrow-up",
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

              p("The main goal in this tab is to extract relevant",
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

      # [right column] ----------------------------------------------------

      div(class = div_column_left,

          # Submit file: --------------------------------------------------

          shinydashboardPlus::box(
            title = span("Upload file", class = "ttl-box_solid"),
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

            radioButtons(inputId = ns("file_sep"),
                         label = "Separator",
                         choices = c("Comma (,)" = ",",
                                     "Semicolon (;)" = ";",
                                     "Tab" = "\t"),
                         selected = ","),

            radioButtons(inputId = ns("file_quote"),
                         label = "Quote",
                         choices = c(None = "",
                                     "Double quote (\")" = '"',
                                     "Single quote (\')" = "'"),
                         selected = '"'),

            footer = shiny::actionButton(
              inputId = ns("confirm_upload"),
              label = "Confirm",
              icon =  icon("upload"),
              width = "100%")

          ), # end of box // uploadBox_file

          # Select species & individual: ----------------------------------

          shinydashboardPlus::box(
            title = span("Species", class = "ttl-box_solid"),
            id = ns("uploadBox_species"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            shiny::textInput(
              inputId = ns("sp_uploaded"),
              label = NULL,
              placeholder = "(scientific name)"),

            shiny::selectizeInput(
              inputId = ns("id_uploaded"),
              label = "Select an individual:",
              choices = "",
              multiple = FALSE,
              options = list(
                placeholder = '(select id)',
                onInitialize = I('function() { this.setValue(""); }'))
            ),

            uiOutput(ns("uploadUI_validate"))

          ), # end of box // uploadBox_species

          # Select variables: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Variables", class = "ttl-box_solid"),
            id = ns("uploadBox_variables"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE, closable = FALSE,

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

            actionButton(
              inputId = ns("uploadButton_extract"),
              icon =  icon("paper-plane"),
              label = "Extract",
              width = "100%",
              class = "btn-primary")

          ), # end of box // uploadBox_variables

          # Tracking regime: ----------------------------------------------

          shinydashboardPlus::box(
            title = span("Tracking regime", class = "ttl-box_solid"),
            id = ns("uploadBox_regime"),
            status = "info",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            fluidRow(
              column(width = 12, uiOutput(ns("uploadInfo_dur"))),
              column(width = 12, uiOutput(ns("uploadInfo_dti")))
            ) # end of fluidRow

          ) # end of box // uploadBox_regime
      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(class = div_column_right,

          # Visualization: ------------------------------------------------

          shinydashboardPlus::box(
            title = span("Data vizualization:", class = "ttl-box"),
            id = ns("uploadBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,

            mod_comp_viz_ui("comp_viz_uploaded")

          ) # end of box // uploadBox_viz

      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          # Displaying relevant metrics: ----------------------------------

          ## Extracted parameters:
          uiOutput(ns("uploadUI_parameters")),

          ## Sample sizes:
          shinydashboardPlus::box(
            title = span("Displaying sample sizes:", class = "ttl-box"),
            id = ns("uploadBox_sizes"),
            width = NULL,
            solidHeader = FALSE,

            fluidRow(
              column(width = 4, uiOutput(ns("uploadBlock_n"))),
              column(width = 4, uiOutput(ns("uploadBlock_Narea"))),
              column(width = 4, uiOutput(ns("uploadBlock_Nspeed"))),
            ) # end of fluidRow

          ), # end of box // uploadBox_sizes

          # Additional information: ---------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("uploadBox_misc"),
            width = NULL, solidHeader = FALSE,

            verbatimTextOutput(outputId = ns("uploadUI_time"))

          ) # end of box // uploadBox_misc
      ) # end of column (bottom)

    ) # end of fluidRow
  ) # end of tagList
}

#' tab_data_upload Server Functions
#'
#' @noRd
mod_tab_data_upload_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()

    # DYNAMIC UI ELEMENTS -----------------------------------------------
    ## Hide all boxes at start: -----------------------------------------

    tmpnames <- c("species",
                  "regime",
                  "variables",
                  "sizes",
                  "misc")

    for(i in 1:length(tmpnames)) {
      shinyjs::hide(id = paste0("uploadBox_", tmpnames[i]))
    }

    ## Render validate button: -------------------------------------------

    output$uploadUI_validate <- renderUI({

      tmpbutton <- shiny::actionButton(
        inputId = ns("validate_upload"),
        icon =  icon("wand-magic-sparkles"),
        label = "Validate",
        width = "100%",
        class = "btn-danger")

      if (is.null(vals$is_valid)) {
        tmpbutton

      } else {
        if (vals$is_valid) {
          shiny::actionButton(
            inputId = ns("validate_upload"),
            icon =  icon("circle-check"),
            label = "Validated!",
            width = "100%",
            class = "btn-info")
        } else {
          tmpbutton
        }
      }

    }) # end of renderUI // uploadUI_validate

    ## If subset data available, update inputs: -------------------------

    observe({
      req(vals$data0)

      updateSelectInput(session, inputId = "uploadVar_x",
                        label = "X coordinate:",
                        choices = names(vals$data0),
                        selected =
                          ifelse(!is.null(vals$data0$"x"),
                                 "x", "longitude"))
      updateSelectInput(session, inputId = "uploadVar_y",
                        label = "Y coordinate:",
                        choices = names(vals$data0),
                        selected =
                          ifelse(!is.null(vals$data0$"y"),
                                 "y", "latitude"))
      updateSelectInput(session, inputId = "uploadVar_t",
                        label = "Datetime:",
                        choices = names(vals$data0),
                        selected =
                          ifelse(!is.null(vals$data0$"timestamp"),
                                 "timestamp", "t"))

      vals$input_x <- input$uploadVar_x
      vals$input_y <- input$uploadVar_y
      vals$input_t <- input$uploadVar_t

    }) # end of observe

    ## Match id for input, plot and table: --------------------------------

    observe({
      req(input$id_uploaded)
      vals$id <- input$id_uploaded
    })

    observe({
      req(vals$table_selection)
      vals$id <- names(vals$dataList)[
        vals$table_selection$selected]
    })

    observe({
      req(vals$plot_selection)
      vals$id <- names(vals$dataList)[
        match(vals$plot_selection, names(vals$dataList))]
    })

    observe({
      req(vals$dataList, vals$id)

      shiny::updateSelectizeInput(
        session,
        inputId = "id_uploaded",
        label = "Select an individual:",
        choices = names(vals$dataList),
        selected = isolate(vals$id))

    }) # end of observe

    # UPLOAD DATA -------------------------------------------------------
    ## Upload .csv file with movement dataset: --------------------------

    ## 1.1. Read in file submission:

    observe({

      # Reset values between tabs:
      reset_data_values(vals)

      vals$data_type <- "uploaded"
      inFile <- input$file_csv
      
      df0 <- read.csv(inFile$datapath,
                      header = input$file_header,
                      sep = input$file_sep,
                      quote = input$file_quote)

      df0 <- ctmm::as.telemetry(df0)
      
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

      msg_log(
        style = "success",
        message = paste0("File ",
                         msg_success("submitted"), "."),
        detail = "Please select one individual from this dataset.")

      shinyjs::show(id = "uploadBox_species")
      if(!input$uploadBox_file$collapsed) {
        shinydashboardPlus::updateBox("uploadBox_file",
                                      action = "toggle")
      } else { NULL }

      vals$dataList <- df0

      # Check number of individuals within dataset:
      if(names(vals$dataList)[[1]] == "timestamp") {
        tmplist <- list()
        tmplist[[1]] <- vals$dataList
        names(tmplist) <- as_tele_list(vals$dataList) %>%
          summary() %>% rownames()
        vals$dataList <- tmplist
      }

      shiny::updateSelectizeInput(
        session,
        inputId = "id_uploaded",
        label = "Select an individual:",
        choices = names(vals$dataList))

    }) %>% # end of observe, then:
      bindEvent(input$confirm_upload)

    # 1.2. Subset data based on individual selection:

    observe({
      req(vals$dataList, vals$id,
          vals$active_tab == 'data_upload')

      if(names(vals$dataList)[[1]] == "timestamp") {
        df_subset <- vals$dataList
      } else {
        df_subset <- vals$dataList[[vals$id]]
      }

      vals$data0 <- df_subset
      shinyjs::show(id = "uploadBox_variables")
      vals$is_valid <- FALSE

    }) # end of observe

    ## Validate data: ---------------------------------------------------

    observe({
      req(vals$data0)

      if(input$sp_uploaded == "") {
        # If no species name is written down:

        msg_log(
          style = "danger",
          message = paste0("Species name ",
                           msg_danger("not found"), "."),
          detail = paste("Please input scientific name",
                         "of your study species."))

        shinyalert::shinyalert(
          title = "Missing species name",
          text = span(
            'Please input the scientific name of',
            'your study species, then click the',
            icon("circle-check", class = "cl-mdn"),
            span("Validate", class = "cl-mdn"),
            'button again.'),
          html = TRUE,
          size = "xs")

      } else {
        vals$species_binom <- input$sp_uploaded
        vals$needs_fit <- TRUE

      } # end of if()

      req(vals$species_binom)
      if(is.null(vals$needs_fit)) {
        vals$needs_fit <- TRUE
      }

      eval_n <- nrow(vals$data0)
      nms <- names(summary(vals$data0))
      dur_unit <- nms[grep('sampling period',nms)] %>%
        extract_units()
      dur <- as.numeric(
        summary(vals$data0)[grep('sampling period', nms)])
      eval_dur <- dur %#% dur_unit

      if(eval_dur < (1 %#% "days") || eval_n < 5) {

        shinyalert::shinyalert(
          title = "Warning",
          text = span(
            "Chosen individual has a sampling duration",
            "of less than", span("one day", class = "cl-dgr"),
            "and/or less than",
            wrap_none(span("5 locations", class = "cl-dgr"), "."),
            "Please select a different individual for validation."),

          html = TRUE,
          size = "xs")

        msg_log(
          style = "danger",
          message = paste0("Individual with low ",
                           msg_danger("tracking duration"),
                           " or ", msg_danger("total fixes"), "..."),
          detail = "Select a different individual to proceed.")

        vals$is_valid <- FALSE
        shinyjs::hide(id = "uploadBox_parameters")

      } else {

        msg_log(
          style = "success",
          message = paste0("Species and individual ",
                           msg_success("validated"), "."),
          detail = paste0("Species selected is ",
                          msg_success(vals$species_binom),
                          ", and the individual is ",
                          msg_success(vals$id), "."))

        vals$is_valid <- TRUE

        # print(vals$tour_active)
        # if(!input$upload_intro$collapsed &&
        #    vals$tour_active) { NULL } else {
        #      shinydashboardPlus::updateBox("upload_intro",
        #                                    action = "toggle")
        #    }

      } # end of evaluation
    }) %>% # end of observe,
      bindEvent(input$validate_upload, ignoreInit = TRUE)

    # PARAMETERS --------------------------------------------------------
    
    runtime <- shiny::reactive({
      
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
      
      out <- estimate_time(vals$data0, parallel = vals$parallel)
      
      shinybusy::remove_modal_spinner()
      return(out)
      
    }) %>% # end of reactive, data_sim
      bindCache(c(vals$id, vals$species_binom))
    
    # After clicking "Extract" button:
    
    observe({
      shinyjs::show(id = "uploadBox_regime")

      if(is.null(vals$species_binom)) {

        shinyalert::shinyalert(
          title = "Oops!",
          text = span(
            "Please select a species/dataset and an individual",
            "first, then click the",
            icon("circle-check", class = "cl-mdn"),
            span("Validate", class = "cl-mdn"), "and",
            icon("paper-plane", class = "cl-mdn"),
            span("Extract", class = "cl-mdn"),
            "buttons."),
          html = TRUE,
          size = "xs")

      } else {
        req(vals$data0, vals$id, vals$species_binom)
      }

      ## Model fitting: -------------------------------------------------
      
      req(vals$is_valid)
      start <- Sys.time()
      
      # Fit models to current data:
      
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("in progress"), "."),
        detail = "Please wait for model selection to finish:")
      
      expt <- runtime()
      
      expt_max <- expt$max
      expt_min <- expt$min
      expt_units <- expt$units
      
      if ((expt_max %#% expt_units) > 900) {
        
        vals$confirm_time <- FALSE
        shinyalert::shinyalert(
          className = "modal_warning",
          title = "Do you wish to proceed?",
          callbackR = function(x) {
            vals$confirm_time <- x
          },
          text = span(
            "Expected run time for the next phase", br(),
            "is approximately",
            span(expt_min, "\u2013", expt_max,
                 class = "cl-dgr"),
            wrap_none(span(expt_units,
                           class = "cl-dgr"), ".")
          ),
          type = "warning",
          showCancelButton = TRUE,
          cancelButtonText = "Stop",
          confirmButtonCol = pal$mdn,
          confirmButtonText = "Proceed",
          html = TRUE
        )
      } else {
        confirm <- TRUE
      }
      
      req(confirm)
      if (expt_max == expt_min) {
        tmptxt <- paste("\u2264", expt_max, expt_units)
      } else {
        tmptxt <- paste(expt_min, "\u2013",
                        expt_max, expt_units)
      }
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        
        text = span(
          style = "font-size: 18px;",
          span("Selecting", style = "color: #797979;"),
          HTML(paste0(span("movement model", class = "cl-sea"),
                      span(".", style = "color: #797979;"))),
          p(),
          p("Expected run time:",
            style = paste("background-color: #eaeaea;",
                          "color: #797979;",
                          "font-size: 16px;",
                          "text-align: center;")), br(),
          p(tmptxt,
            style = paste("background-color: #eaeaea;",
                          "color: #009da0;",
                          "font-size: 16px;",
                          "text-align: center;",
                          "margin-top: -40px;")),
          p()
          
        ) # end of text
      ) # end of modal
      
      guess0 <- ctmm::ctmm.guess(vals$data0, interactive = FALSE)
      inputList <- list(list(vals$data0, guess0))
      fit0 <- reactive({
        par_ctmm.select(inputList, parallel = vals$parallel)
      }) %>% bindCache(vals$species_binom,
                       vals$id)
      
      vals$fit0 <- fit0()
      time_fit0 <- difftime(Sys.time(), start, units = "mins")
      vals$uploadOut_time <- time_fit0
      
      if (round(time_fit0, 1) < 1) {
        tmpdetail <- paste("This step took less than one minute.")
      } else {
        tmpdetail <- paste("This step took approximately",
                           round(difftime(Sys.time(), start,
                                          units = 'min'), 0),
                           "minutes.")
      }
      
      if (!is.null(vals$fit0)) {
        msg_log(
          style = 'success',
          message = paste0("Model fit ",
                           msg_success("completed"), "."),
          detail = tmpdetail)
        
        shinyjs::show(id = "uploadBox_sizes")
        
        vals$tmpsp <- vals$species_binom
        vals$tmpid <- vals$id
        
        sum.fit <- summary(vals$fit0)
        vals$needs_fit <- FALSE
        
        ## Extract semi-variance parameter: -------------------------------
        
        svf <- prepare_svf(vals$data0, fraction = .65)
        vals$sigma0_min <- mean(svf$var_low95)
        vals$sigma0_max <- mean(svf$var_upp95)
        
        ## Extract timescale and spatial parameters: ----------------------
        
        output$uploadUI_parameters <- renderUI({
          
          shinydashboardPlus::box(
            title = span("Displaying parameters:", class = "ttl-box"),
            id = ns("uploadBox_parameters"),
            width = NULL,
            solidHeader = FALSE,
            
            column(
              align = "center", width = 12,
              
              renderUI({
                if(vals$tmpid == "Simulated individual") { NULL
                } else {
                  
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
                  
                } # end of if() statement
              }) # end of renderUI
              
            ), # end of column (for text)
            
            column(width = 12, uiOutput(ns("uploadBlock_movprocess"))),
            
            fluidRow(
              column(width = 4, uiOutput(ns("uploadBlock_sigma"))),
              column(width = 4, uiOutput(ns("uploadBlock_taup"))),
              column(width = 4, uiOutput(ns("uploadBlock_tauv")))
            )
            
          ) # end of box
        }) # end of renderUI
        
        if (!vals$tour_active) {
          
          shinyalert::shinyalert(
            className = "modal_success",
            type = "success",
            title = "Success!",
            text = span(
              "Proceed to the", br(),
              icon("compass-drafting", class = "cl-mdn"),
              span('Analyses', class = "cl-mdn"), "tabs."),
            html = TRUE,
            size = "xs")
        }
        
      } # end of if(), !is.null(fit1)
      
      shinybusy::remove_modal_spinner()
      
    }) %>% # end of observe, then:
      bindEvent(input$uploadButton_extract)

    ## Validate parameters: ---------------------------------------------

    ### Checking if tau v available for initial dataset:

    observe({

      req(vals$valid_tauv)
      if(vals$valid_tauv == "No" &&
         "Speed & distance" %in% vals$which_question) {

        shinyalert::shinyalert(
          type = "error",

          title = "Speed & distance invalid",
          text = span(
            "No statistically significant signature of the animal's",
            span("velocity", class = "cl-dgr"),
            "remains in this dataset.",
            "Please select a different individual or dataset to",
            "proceed with", span("distance/speed", class = "cl-dgr"),
            "estimation"),

          confirmButtonText = "Dismiss",
          html = TRUE)

        msg_log(
          style = "danger",
          message = paste0("Data has no remaining ",
                           msg_danger("velocity"), " signature."),
          detail = paste("Select a different individual",
                         "or dataset to proceed."))

      } # end of if(), valid tau_v
    }) # end of observe


    # BLOCKS ------------------------------------------------------------
    ## Movement process: ------------------------------------------------

    output$uploadBlock_movprocess <- shiny::renderUI({
      if(vals$tmpid == "Simulated individual") {
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
              br(), "Movement process"
            ),
            value = sum.fit$name[1])

        } # end of if() statement
    }) # end of renderUI

    ## Timescale parameters: --------------------------------------------

    output$uploadBlock_taup <- shiny::renderUI({
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
                  inputId = ns("dataHelp_taup"),
                  icon = icon("circle-question"),
                  label = NULL,
                  style = paste("background-color: #fff;",
                                "color: black;",
                                "padding: 0;")), br(),
                span(
                  HTML(paste0("Position autocorrelation ",
                              "(\u03C4", tags$sub("p"), ")")))
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

    output$uploadBlock_tauv <- shiny::renderUI({
      if(vals$tmpid == "Simulated individual") {
        NULL } else {

          sum.fit <- summary(vals$fit0)
          tempnames <- rownames(sum.fit$CI)

          if(!length(grep('velocity', tempnames))) {

            vals$valid_tauv <- "No"
            NULL

          } else {

            vals$valid_tauv <- "Yes"
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
                  inputId = ns("dataHelp_tauv"),
                  icon = icon("circle-question"),
                  label = NULL,
                  style = paste("background-color: #fff;",
                                "color: black;",
                                "padding: 0;")), br(),
                span(
                  HTML(paste0("Velocity autocorrelation ",
                              "(\u03C4", tags$sub("v"), ")")))
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

    output$uploadBlock_sigma <- shiny::renderUI({
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
                inputId = ns("dataHelp_sigma"),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;")), br(),
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

    output$uploadInfo_dur <- shiny::renderUI({

      dat <- summary(vals$data0)
      nms <- names(dat)
      
      dur <- as.numeric(dat[grep('sampling period', nms)])
      dur_unit <- nms[grep('sampling period', nms)] %>%
        extract_units()
      
      out <- fix_unit(dur, dur_unit)
      parBlock(header = "Sampling duration",
               value = paste(out$value, out$unit))

    }) # ender of renderUI // uploadInfo_dur

    output$uploadInfo_dti <- shiny::renderUI({

      dat <- summary(vals$data0)
      nms <- names(dat)
      
      dti <- as.numeric(dat[grep('sampling interval', nms)])
      dti_unit <- nms[grep('sampling interval', nms)] %>%
        extract_units()
      
      out <- fix_unit(dti, dti_unit)
      parBlock(header = "Sampling interval",
               value = paste(out$value, out$unit),
               subtitle = "between fixes")

    }) # end of renderUI // uploadInfo_dti

    ## Sample sizes: ----------------------------------------------------

    output$uploadBlock_n <- shiny::renderUI({
      req(vals$data0)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data0),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)

    }) # end of renderUI // uploadBlock_n (absolute sample size)

    output$uploadBlock_Narea <- shiny::renderUI({
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

    }) # end of renderUI // uploadBlock_Narea (effective)

    output$uploadBlock_Nspeed <- shiny::renderUI({
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
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // uploadBlock_Nspeed (effective)

    # MODALS & HELP -----------------------------------------------------

    # observe({
    #
    #   shiny::showModal(
    #     shiny::modalDialog(
    #       title = "Movement models or processes:",
    #
    #       reactable::reactableOutput(ns("dataTable_processes")),
    #
    #       footer = tagList(
    #         modalButton("Dismiss")
    #       ),
    #       size = "l"))
    #
    # }) %>% bindEvent(input$uploadHelp_mods)
    #
    # output$dataTable_processes <- reactable::renderReactable({
    #
    #   tmpname <-
    #     sub('(^\\w+)\\s.+','\\1', summary(vals$fit0)$name[1])
    #
    #   if(is.null(match(tmpname, movmods$name_short))) {
    #     preselected_mod <- NULL
    #   } else {
    #     preselected_mod <- match(tmpname, movmods$name_short)
    #   }
    #   df0 <- movmods %>%
    #     dplyr::select(!name_short)
    #
    #   reactable::reactable(
    #     df0,
    #     searchable = TRUE,
    #     highlight = TRUE,
    #     selection = "single",
    #     defaultSelected = preselected_mod,
    #     defaultColDef =
    #       reactable::colDef(
    #         headerClass = "rtable_header",
    #         align = "left"),
    #     columns = list(
    #       name = reactable::colDef(
    #         name = "Movement process",
    #         minWidth = 195),
    #
    #       tau_p = reactable::colDef(
    #         name = paste0("\u03C4","\u209A"),
    #         minWidth = 60,
    #         cell = reactable::JS(
    #           paste0("function(cellInfo) {
    #             // Render as an X mark or check mark
    #             return cellInfo.value === 'No' ? '\u274c No' : ",
    #                  "'\u2714\ufe0f Yes'}"))),
    #
    #       tau_v = reactable::colDef(
    #         name = paste0("\u03C4","\u1D65"),
    #         minWidth = 60,
    #         cell = reactable::JS(
    #           paste0("function(cellInfo) {
    #             // Render as an X mark or check mark
    #             return cellInfo.value === 'No' ? '\u274c No' : ",
    #                  "'\u2714\ufe0f Yes'}"))),
    #
    #       hrange = reactable::colDef(
    #         minWidth = 80,
    #         name = "Home range",
    #         cell = reactable::JS(
    #           paste0("function(cellInfo) {
    #             // Render as an X mark or check mark
    #             return cellInfo.value === 'No' ? '\u274c No' : ",
    #                  "'\u2714\ufe0f Yes'}"))),
    #
    #       pars = reactable::colDef(
    #         name = "Parameterization")
    #     ),
    #     theme = reactable::reactableTheme(
    #       rowSelectedStyle = list(
    #         backgroundColor = "#eee",
    #         boxShadow = "inset 2px 0 0 0 #009da0")))
    #
    # }) # end of renderReactable // dataTable_processes

    # Additional information: -------------------------------------------

    output$uploadUI_time <- renderText({
      req(vals$uploadOut_time)
      paste0("Model fitting took approximately ",
             round(vals$uploadOut_time, 1), " minutes.")
    }) # end of renderText // uploadOut_time

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_data_upload_ui("tab_data_upload_1")

## To be copied in the server
# mod_tab_data_upload_server("tab_data_upload_1")
