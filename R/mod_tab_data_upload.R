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
      div(
        class = div_column_main,
        shinydashboardPlus::box(
          title = span("Submit movement data:", class = "ttl-tab"),
          icon = fontawesome::fa(
            name = "file-csv",
            height = "21px",
            margin_left = "14px",
            margin_right = "8px",
            fill = "var(--sea-dark)"
          ),
          id = ns("upload_intro"),
          width = NULL,
          solidHeader = FALSE, headerBorder = FALSE,
          collapsible = TRUE, closable = FALSE,
          column(
            align = "center", width = 12,
            p(
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
              br()
            ),
            p(
              style = "text-align: center;",
              "First, browse and choose the appropriate",
              span(".csv", class = "cl-sea"), "file.",
              br(), "Then click",
              icon("circle-check", class = "cl-mdn"),
              span("Validate", class = "cl-mdn"), "and",
              icon("paper-plane", class = "cl-mdn"),
              wrap_none(span("Extract", class = "cl-mdn"), ".")
            )
          ) # end of column (text)
        ) # end of box // upload_intro
      ), # end of div (top row)

      # [right column] ----------------------------------------------------

      div(
        class = div_column_left,

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
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          tags$hr(),
          shinyWidgets::prettyCheckbox(
            inputId = ns("file_header"),
            label = span("Header", class = "cl-blk"),
            status = "primary",
            animation = "tada",
            outline = TRUE,
            value = TRUE
          ), br(),
          radioButtons(
            inputId = ns("file_sep"),
            label = "Separator",
            choices = c(
              "Comma (,)" = ",",
              "Semicolon (;)" = ";",
              "Tab" = "\t"
            ),
            selected = ","
          ),
          radioButtons(
            inputId = ns("file_quote"),
            label = "Quote",
            choices = c(
              None = "",
              "Double quote (\")" = '"',
              "Single quote (\')" = "'"
            ),
            selected = '"'
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_upload"),
            label = "Confirm",
            icon = icon("upload"),
            width = "100%"
          )
        ), # end of box // uploadBox_file

        # Select species & individual: ----------------------------------

        shinydashboardPlus::box(
          title = span("Dataset", class = "ttl-box_solid"),
          id = ns("uploadBox_species"),
          status = "primary",
          width = NULL,
          solidHeader = TRUE,
          collapsible = FALSE,
          shiny::textInput(
            inputId = ns("sp_uploaded"),
            label = NULL,
            placeholder = "What species?"
          ),
          shinyWidgets::pickerInput(
            inputId = ns("id_uploaded"),
            label = NULL,
            choices = "",
            multiple = FALSE,
            options = list(
              `live-search` = TRUE,
              title = "Pick an individual"
            ),
            selected = NULL
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
          selectInput(
            inputId = ns("uploadVar_x"),
            label = "X coordinate:",
            selected = "x",
            choices = ""
          ),
          selectInput(
            inputId = ns("uploadVar_y"),
            label = "Y coordinate:",
            selected = "y",
            choices = ""
          ),
          selectInput(
            inputId = ns("uploadVar_t"),
            label = "Datetime:",
            selected = "timestamp",
            choices = ""
          ),
          actionButton(
            inputId = ns("uploadButton_extract"),
            icon = icon("paper-plane"),
            label = "Extract",
            width = "100%",
            class = "btn-primary"
          )
        ), # end of box // uploadBox_variables

        # Tracking regime: ----------------------------------------------

        shinydashboardPlus::box(
          title = span("Tracking regime:", class = "ttl-box"),
          id = ns("uploadBox_regime"),
          status = "info",
          width = NULL,
          solidHeader = FALSE,
          collapsible = TRUE,
          fluidRow(
            column(width = 12, uiOutput(ns("uploadInfo_dur"))),
            column(width = 12, uiOutput(ns("uploadInfo_dti")))
          ) # end of fluidRow
        ) # end of box // uploadBox_regime
      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(
        class = div_column_right,

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

      div(
        class = div_column_main,

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

    tmpnames <- c(
      "species",
      "regime",
      "variables",
      "sizes",
      "misc"
    )

    for (i in 1:length(tmpnames)) {
      shinyjs::hide(id = paste0("uploadBox_", tmpnames[i]))
    }

    ## Render validate button: -------------------------------------------

    output$uploadUI_validate <- renderUI({
      tmpbutton <- shiny::actionButton(
        inputId = ns("validate_upload"),
        icon = icon("wand-magic-sparkles"),
        label = "Validate",
        width = "100%",
        class = "btn-danger"
      )

      if (is.null(vals$is_valid)) {
        tmpbutton
      } else {
        if (vals$is_valid) {
          shiny::actionButton(
            inputId = ns("validate_upload"),
            icon = icon("circle-check"),
            label = "Validated!",
            width = "100%",
            class = "btn-info"
          )
        } else {
          tmpbutton
        }
      }
    }) # end of renderUI // uploadUI_validate

    ## If subset data available, update inputs: -------------------------

    observe({
      req(vals$data0)

      updateSelectInput(session,
        inputId = "uploadVar_x",
        label = "X coordinate:",
        choices = names(vals$data0),
        selected =
          ifelse(!is.null(vals$data0$"x"),
            "x", "longitude"
          )
      )
      updateSelectInput(session,
        inputId = "uploadVar_y",
        label = "Y coordinate:",
        choices = names(vals$data0),
        selected =
          ifelse(!is.null(vals$data0$"y"),
            "y", "latitude"
          )
      )
      updateSelectInput(session,
        inputId = "uploadVar_t",
        label = "Datetime:",
        choices = names(vals$data0),
        selected =
          ifelse(!is.null(vals$data0$"timestamp"),
            "timestamp", "t"
          )
      )

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
      vals$id <- names(vals$dataList)[vals$table_selection$selected]
    })

    observe({
      req(vals$plot_selection)
      vals$id <- names(vals$dataList)[
        match(vals$plot_selection, names(vals$dataList))
      ]
    })

    observe({
      req(vals$dataList, vals$id)

      shinyWidgets::updatePickerInput(
        session,
        inputId = "id_uploaded",
        label = NULL,
        choices = names(vals$dataList),
        selected = isolate(vals$id)
      )
    }) # end of observe

    # UPLOAD DATA -------------------------------------------------------
    ## Upload .csv file with movement dataset: --------------------------

    ## 1.1. Read in file submission:

    observe({
      # Reset values between tabs:
      reset_data_values(vals)

      vals$data_type <- "uploaded"
      inFile <- input$file_csv

      tmp <- read.csv(inFile$datapath,
        header = input$file_header,
        sep = input$file_sep,
        quote = input$file_quote
      )

      tmp <- ctmm::as.telemetry(tmp)

      msg_log(
        style = "success",
        message = paste0(
          "File ",
          msg_success("submitted"), "."
        ),
        detail = "Please select one individual from this dataset."
      )

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
            positionClass = "toast-bottom-right"
          )
        )

        msg_log(
          style = "success",
          message = paste0(
            "Data pseudonymization ",
            msg_success("completed"), "."
          ),
          detail = "Origin location and time added."
        )
      } else {
        vals$is_pseudonymized <- FALSE
      }

      shinyjs::show(id = "uploadBox_species")
      if (!input$uploadBox_file$collapsed) {
        shinydashboardPlus::updateBox("uploadBox_file",
          action = "toggle"
        )
      } else {
        NULL
      }

      vals$dataList <- tmp

      # Check number of individuals within dataset:

      if (names(vals$dataList)[[1]] == "timestamp") {
        tmplist <- list()
        tmplist[[1]] <- vals$dataList
        names(tmplist) <- as_tele_list(vals$dataList) %>%
          summary() %>%
          rownames()
        vals$dataList <- tmplist
      }

      shinyWidgets::updatePickerInput(
        session,
        inputId = "id_uploaded",
        label = NULL,
        choices = names(vals$dataList)
      )
    }) %>% # end of observe, then:
      bindEvent(input$confirm_upload)

    # 1.2. Subset data based on individual selection:

    observe({
      req(
        vals$dataList,
        vals$id,
        vals$active_tab == "data_upload"
      )

      if (names(vals$dataList)[[1]] == "timestamp") {
        df_subset <- vals$dataList
      } else {
        df_subset <- vals$dataList[[vals$id]]
      }

      vals$data0 <- df_subset
      shinyjs::show(id = "uploadBox_variables")
      vals$is_valid <- FALSE
    }) # end of observe

    ## Validate data: ---------------------------------------------------

    runtime <- shiny::reactive({
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = span(
          style = "font-size: 18px;",
          span("Calculating", style = "color: #797979;"),
          HTML(paste0(
            span("run time", class = "cl-sea"),
            span("...", style = "color: #797979;")
          ))
        )
      )

      out <- guesstimate_time(vals$data0, parallel = vals$parallel)

      shinybusy::remove_modal_spinner()
      return(out)
    }) %>% # end of reactive, data_sim
      bindCache(c(vals$id, vals$species_binom))

    observe({
      if (is.null(vals$which_question)) {
        shinyalert::shinyalert(
          title = "No research goal selected",
          text = span(
            "Please select a research goal in the",
            icon("house", class = "cl-blk"),
            span("Home", class = "cl-blk"),
            "tab before proceeding."
          ),
          html = TRUE,
          size = "xs"
        )
      }

      req(
        vals$which_question,
        vals$data0
      )

      if (input$sp_uploaded == "") {
        # If no species name is written down:

        msg_log(
          style = "danger",
          message = paste0(
            "Species name ",
            msg_danger("not found"), "."
          ),
          detail = paste(
            "Please input scientific name",
            "of your study species."
          )
        )

        shinyalert::shinyalert(
          title = "Missing species name",
          text = span(
            "Please input the name of",
            "your study species, then click the",
            icon("circle-check", class = "cl-mdn"),
            span("Validate", class = "cl-mdn"),
            "button again."
          ),
          html = TRUE,
          size = "xs"
        )
      } else {
        vals$species_binom <- input$sp_uploaded
        vals$needs_fit <- TRUE
      } # end of if ()

      ### Model fitting: --------------------------------------------------

      req(vals$species_binom)
      start <- Sys.time()

      msg_log(
        style = "warning",
        message = paste0(
          "Model fit ",
          msg_warning("in progress"), "."
        ),
        detail = "Please wait for model selection to finish:"
      )

      expt <- runtime()
      expt_max <- expt$max
      expt_min <- expt$min
      expt_units <- expt$units

      if ((expt_max %#% expt_units) > (15 %#% "minutes")) {
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
              class = "cl-dgr"
            ),
            wrap_none(span(expt_units,
              class = "cl-dgr"
            ), ".")
          ),
          type = "warning",
          showCancelButton = TRUE,
          cancelButtonText = "Stop",
          confirmButtonCol = pal$mdn,
          confirmButtonText = "Proceed",
          html = TRUE
        )
      } else {
        vals$confirm_time <- TRUE
      }

      req(vals$confirm_time)
      if (expt_max == expt_min) {
        tmptxt <- paste("\u2264", expt_max, expt_units)
      } else {
        tmptxt <- paste(
          expt_min, "\u2013",
          expt_max, expt_units
        )
      }

      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = span(
          style = "font-size: 18px;",
          span("Selecting", style = "color: #797979;"),
          HTML(paste0(
            span("movement model", class = "cl-sea"),
            span(".", style = "color: #797979;")
          )),
          p(),
          p("Expected run time:",
            style = paste(
              "background-color: #eaeaea;",
              "color: #797979;",
              "font-size: 16px;",
              "text-align: center;"
            )
          ), br(),
          p(tmptxt,
            style = paste(
              "background-color: #eaeaea;",
              "color: #009da0;",
              "font-size: 16px;",
              "text-align: center;",
              "margin-top: -40px;"
            )
          ),
          p()
        ) # end of text
      ) # end of modal

      guess <- ctmm::ctmm.guess(vals$data0, interactive = FALSE)
      vals$guess <- guess

      inputList <- list(list(vals$data0, guess))
      fit0 <- reactive({
        par.ctmm.select(inputList, parallel = vals$parallel)
      }) %>% bindCache(
        vals$species_binom,
        vals$id
      )

      vals$fit0 <- fit0()
      time_fit0 <- difftime(Sys.time(), start, units = "mins")
      vals$uploadOut_time <- time_fit0

      if (round(time_fit0, 1) < 1) {
        tmpdetail <- paste("This step took less than one minute.")
      } else {
        tmpdetail <- paste(
          "This step took approximately",
          round(difftime(Sys.time(), start,
            units = "min"
          ), 0),
          "minutes."
        )
      }

      if (!is.null(vals$fit0)) {
        msg_log(
          style = "success",
          message = paste0(
            "Model fit ",
            msg_success("completed"), "."
          ),
          detail = tmpdetail
        )

        shinyjs::show(id = "uploadBox_sizes")

        vals$tmpsp <- vals$species_binom
        vals$tmpid <- vals$id

        vals$needs_fit <- FALSE
      }

      shinybusy::remove_modal_spinner()

      ### Set up for validation: ------------------------------------------

      taup <- extract_pars(vals$fit0, par = "position")
      tauv <- extract_pars(vals$fit0, par = "velocity")

      ### Validate based on research questions: ---------------------------

      vals$is_valid <- TRUE
      if (is.null(taup) & is.null(tauv)) {
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
            "estimation."
          ),
          confirmButtonText = "Dismiss",
          html = TRUE
        )

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
              "estimation."
            ),
            confirmButtonText = "Dismiss",
            html = TRUE
          )

          msg_log(
            style = "danger",
            message = paste(
              "No signature of",
              msg_danger("position autocorrelation"),
              "found."
            ),
            detail = "Select a different dataset to proceed."
          )

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
              "estimation."
            ),
            confirmButtonText = "Dismiss",
            html = TRUE
          )

          msg_log(
            style = "danger",
            message = paste(
              "No signature of",
              msg_danger("velocity autocorrelation"),
              "found."
            ),
            detail = "Select a different dataset to proceed."
          )

          vals$is_valid <- NULL
        }
      }

      req(vals$is_valid)
      vals$input_x <- ifelse(!is.null(vals$data0$"x"),
        "x", "longitude"
      )
      vals$input_y <- ifelse(!is.null(vals$data0$"y"),
        "y", "latitude"
      )
      vals$input_t <- ifelse(!is.null(vals$data0$"timestamp"),
        "timestamp", "t"
      )

      msg_log(
        style = "success",
        message = paste0(
          "Species and individual ",
          msg_success("validated"), "."
        ),
        detail = paste0(
          "Species selected is the ",
          msg_success(vals$species),
          ", and the individual is ",
          msg_success(vals$id), "."
        )
      )

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
      bindEvent(input$validate_upload)

    # PARAMETERS --------------------------------------------------------
    # After clicking "Extract" button:

    observe({
      req(vals$which_question)
      shinyjs::show(id = "uploadBox_regime")

      if (is.null(vals$is_valid)) {
        shinyalert::shinyalert(
          title = "Oops!",
          text = span(
            "Please select a species and an individual",
            "first, then click the",
            icon("wand-magic-sparkles", class = "cl-mdn"),
            span("Validate", class = "cl-mdn"), "and",
            icon("paper-plane", class = "cl-mdn"),
            span("Extract", class = "cl-mdn"),
            "buttons."
          ),
          html = TRUE,
          size = "xs"
        )
      }

      req(
        vals$data_type == "uploaded", vals$fit0,
        vals$data0, vals$id, vals$species_binom,
        vals$is_valid
      )

      ## Extract semi-variance parameter: -------------------------------

      if (is.null(vals$var_fraction)) {
        frac <- .65
      } else {
        frac <- vals$var_fraction
      }

      svf <- extract_svf(vals$data0, fraction = frac)
      vals$svf <- svf
      vals$sigma0 <- extract_pars(
        obj = vals$fit0,
        par = "sigma",
        data = vals$data0,
        fraction = frac
      )

      ## Extract timescale and spatial parameters: ----------------------

      taup <- extract_pars(vals$fit0, par = "position")
      vals$tau_p0 <- taup

      tauv <- extract_pars(vals$fit0, par = "velocity")
      vals$tau_v0 <- tauv

      output$uploadUI_parameters <- renderUI({
        shinydashboardPlus::box(
          title = span("Displaying parameters:", class = "ttl-box"),
          id = ns("uploadBox_parameters"),
          width = NULL,
          solidHeader = FALSE,
          column(
            align = "center", width = 12,
            renderUI({
              if (vals$tmpid == "Simulated individual") {
                NULL
              } else {
                p(
                  "These parameters have been extracted from",
                  "individual", span(vals$tmpid, class = "cl-sea-d"),
                  "and species",
                  wrap_none(span(vals$tmpsp, class = "cl-sea-d"), "."),
                  "They will only update if you change the",
                  "individual and/or species selected, and then",
                  "click the buttons",
                  icon("circle-check", class = "cl-mdn"),
                  span("Validate", class = "cl-mdn"), "and",
                  icon("paper-plane", class = "cl-mdn"),
                  wrap_none("Extract", css = "cl-mdn", end = ".")
                )
              } # end of if () statement
            }) # end of renderUI
          ), # end of column (for text)

          column(width = 12, uiOutput(ns("uploadBlock_movprocess"))),
          fluidRow(
            column(width = 6, uiOutput(ns("uploadBlock_taup"))),
            column(width = 6, uiOutput(ns("uploadBlock_tauv")))
          ),
          fluidRow(
            column(width = 6, uiOutput(ns("uploadBlock_sigma"))),
            column(width = 6, uiOutput(ns("uploadBlock_speed")))
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
          positionClass = "toast-bottom-right"
        )
      )

      if (!vals$tour_active) {
        shinyalert::shinyalert(
          className = "modal_success",
          type = "success",
          title = "Success!",
          text = span(
            "Proceed to the", br(),
            icon("stopwatch", class = "cl-mdn"),
            span("Tracking regime", class = "cl-mdn"), "tab."
          ),
          html = TRUE,
          size = "xs"
        )
      }
    }) %>% # end of observe, then:
      bindEvent(input$uploadButton_extract)

    # BLOCKS ------------------------------------------------------------
    ## Movement process: ------------------------------------------------

    output$uploadBlock_movprocess <- shiny::renderUI({
      if (vals$tmpid == "Simulated individual") {
        NULL
      } else {
        sum.fit <- summary(vals$fit0)

        parBlock(
          header = shiny::fluidRow(
            style = paste("margin-bottom: -14px;"),
            actionButton(
              inputId = ns("uploadHelp_mods"),
              icon = icon("circle-question"),
              label = NULL,
              style = paste(
                "background-color: #fff;",
                "color: black;",
                "padding: 0;"
              )
            ),
            br(), "Movement process"
          ),
          value = sum.fit$name[1]
        )
      } # end of if () statement
    }) # end of renderUI

    ## Timescale parameters: --------------------------------------------

    output$uploadBlock_taup <- shiny::renderUI({
      req(vals$tmpid, vals$fit0, vals$tau_p0)

      if (vals$tmpid == "Simulated individual") {
        NULL
      } else {
        par <- vals$tau_p0
        tau_p0 <- fix_unit(par["est", 1], par$unit[1])
        tau_p0_min <- scales::label_comma(.1)(par["low", 1])
        tau_p0_max <- scales::label_comma(.1)(par["high", 1])

        parBlock(
          header = shiny::fluidRow(
            style = paste("margin-bottom: -14px;"),
            actionButton(
              inputId = ns("dataHelp_taup"),
              icon = icon("circle-question"),
              label = NULL,
              style = paste(
                "background-color: #fff;",
                "color: black;",
                "padding: 0;"
              )
            ),
            br(),
            wrap_none(
              "Position autocorrelation ",
              "(\u03C4", tags$sub("p"), ")"
            )
          ),
          value = paste(tau_p0$value, tau_p0$unit),
          subtitle = paste(
            ifelse(tau_p0_min == 0,
              "0", tau_p0_min
            ),
            "\u2014", tau_p0_max
          )
        )
      } # end of if () statement
    }) # end of renderUI

    output$uploadBlock_tauv <- shiny::renderUI({
      req(vals$tmpid, vals$fit0, vals$tau_v0)

      if (vals$tmpid == "Simulated individual") {
        NULL
      } else {
        par <- vals$tau_v0
        tau_v0 <- fix_unit(par["est", 1], par$unit[1])
        tau_v0_min <- scales::label_comma(.1)(par["low", 1])
        tau_v0_max <- scales::label_comma(.1)(par["high", 1])

        parBlock(
          header = shiny::fluidRow(
            style = paste("margin-bottom: -14px;"),
            actionButton(
              inputId = ns("dataHelp_tauv"),
              icon = icon("circle-question"),
              label = NULL,
              style = paste(
                "background-color: #fff;",
                "color: black;",
                "padding: 0;"
              )
            ),
            br(),
            wrap_none(
              "Velocity autocorrelation ",
              "(\u03C4", tags$sub("v"), ")"
            )
          ),
          value = paste(tau_v0$value, tau_v0$unit),
          subtitle = paste(
            ifelse(tau_v0_min == 0,
              "0", tau_v0_min
            ),
            "\u2014", tau_v0_max
          )
        )
      } # end of if () statement
    }) # end of renderUI

    ## Other parameters: --------------------------------------------------

    output$uploadBlock_sigma <- shiny::renderUI({
      req(vals$tmpid, vals$fit0, vals$sigma0)

      if (vals$tmpid == "Simulated individual") {
        NULL
      } else {
        par <- vals$sigma0
        sig <- fix_unit(par$value[2],
          unit = par$unit[2],
          convert = TRUE, ui = TRUE
        )
        sig_lci <- fix_unit(par$value[1],
          unit = par$unit[1],
          convert = TRUE, ui = TRUE
        )
        sig_uci <- fix_unit(par$value[3],
          unit = par$unit[3],
          convert = TRUE, ui = TRUE
        )

        parBlock(
          header = shiny::fluidRow(
            style = paste("margin-bottom: -14px;"),
            actionButton(
              inputId = ns("dataHelp_sigma"),
              icon = icon("circle-question"),
              label = NULL,
              style = paste(
                "background-color: #fff;",
                "color: black;",
                "padding: 0;"
              )
            ), br(),
            span(HTML("Semi-variance (\u03C3)"))
          ),
          value = span(HTML("&nbsp;", sig$value, sig$unit)),
          subtitle = paste(
            ifelse(sig_lci$value == 0,
              "0", sig_lci$value
            ),
            "\u2014", sig_uci$value
          )
        )
      } # end of if () statement
    }) # end of renderUI

    output$uploadBlock_speed <- shiny::renderUI({
      req(vals$tmpid, vals$fit0)

      if (vals$tmpid == "Simulated individual") {
        NULL
      } else {
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
              style = paste(
                "background-color: #fff;",
                "color: black;",
                "padding: 0;"
              )
            ),
            br(),
            span(HTML("Velocity (\u03BD)"))
          ),
          value = span(HTML("&nbsp;", speed$value, unit)),
          subtitle = paste(
            ifelse(speed_lci == 0,
              "0", speed_lci
            ),
            "\u2014", speed_uci
          )
        )
      } # end of if () statement
    }) # end of renderUI

    ##  Tracking regime: --------------------------------------------------

    output$uploadInfo_dur <- shiny::renderUI({
      req(vals$data0)

      dur <- extract_pars(vals$data0, par = "period")
      out <- fix_unit(dur$value, dur$unit)

      parBlock(
        header = "Sampling duration",
        value = paste(out[1], out[2])
      )
    }) # ender of renderUI // uploadInfo_dur

    output$uploadInfo_dti <- shiny::renderUI({
      req(vals$data0)

      dti <- extract_pars(vals$data0, par = "interval")
      out <- fix_unit(dti$value, dti$unit)

      parBlock(
        header = "Sampling interval",
        value = paste(out[1], out[2]),
        subtitle = "between fixes"
      )
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
        marginBottom = TRUE
      )
    }) # end of renderUI // uploadBlock_n (absolute sample size)

    output$uploadBlock_Narea <- shiny::renderUI({
      req(vals$fit0)

      N <- extract_dof(vals$fit0, par = "area")
      n <- nrow(vals$data0)
      vals$N1 <- N

      value <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%"
      )

      sampleBlock(
        number = value,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE
      )
    }) # end of renderUI // uploadBlock_Narea (effective)

    output$uploadBlock_Nspeed <- shiny::renderUI({
      req(vals$fit0)

      N <- extract_dof(vals$fit0, par = "speed")
      n <- nrow(vals$data0)
      vals$N2 <- N

      value <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%"
      )

      sampleBlock(
        number = value,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE
      )
    }) # end of renderUI // uploadBlock_Nspeed (effective)

    # MODALS & HELP -----------------------------------------------------

    observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Movement models or processes:",
          reactable::reactableOutput(ns("dataTable_processes")),
          footer = tagList(
            modalButton("Dismiss")
          ),
          size = "l"
        )
      )
    }) %>% bindEvent(input$uploadHelp_mods)

    output$dataTable_processes <- reactable::renderReactable({
      mods <- movedesign::movmods
      nm <- sub(
        "(^\\w+)\\s.+", "\\1",
        summary(vals$fit0)$name[1]
      )

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
            align = "left"
          ),
        columns = list(
          name = reactable::colDef(
            name = "Movement process",
            minWidth = 195
          ),
          tau_p = reactable::colDef(
            minWidth = 60,
            name = paste0("\u03C4", "\u209A"),
            cell = reactable::JS(
              paste0(
                "function(cellInfo) {
                // Render as an X mark or check mark
                return cellInfo.value === 'No' ? '\u274c No' : ",
                "'\u2714\ufe0f Yes'}"
              )
            )
          ),
          tau_v = reactable::colDef(
            minWidth = 60,
            name = paste0("\u03C4", "\u1D65"),
            cell = reactable::JS(
              paste0(
                "function(cellInfo) {
                // Render as an X mark or check mark
                return cellInfo.value === 'No' ? '\u274c No' : ",
                "'\u2714\ufe0f Yes'}"
              )
            )
          ),
          hrange = reactable::colDef(
            minWidth = 80,
            name = "Home range",
            cell = reactable::JS(
              paste0(
                "function(cellInfo) {
                // Render as an X mark or check mark
                return cellInfo.value === 'No' ? '\u274c No' : ",
                "'\u2714\ufe0f Yes'}"
              )
            )
          ),
          pars = reactable::colDef(
            name = "Parameterization"
          )
        ),
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(
            backgroundColor = "#eee",
            boxShadow = "inset 2px 0 0 0 #009da0"
          )
        )
      )
    }) # end of renderReactable // dataTable_processes

    # Additional information: -------------------------------------------

    output$uploadUI_time <- renderText({
      req(vals$uploadOut_time)

      paste0(
        "Model fitting took approximately ",
        round(vals$uploadOut_time, 1), " minutes."
      )
    }) # end of renderText // uploadOut_time
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_data_upload_ui("tab_data_upload_1")

## To be copied in the server
# mod_tab_data_upload_server("tab_data_upload_1")
