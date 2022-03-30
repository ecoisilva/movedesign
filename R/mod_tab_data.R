#' tab_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(

      # Introduction: -----------------------------------------------------

      div(class = div_column_main,

          shinydashboardPlus::box(

            title = span("Submit movement data:", style =
                           paste("padding-top: 14px;", ttl_main)),
            icon = fontawesome::fa(name = "file-upload",
                                   height = "22px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "#e3e3e3"),
            id = ns("data_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = TRUE, closable = FALSE,

            column(
              align = "center", width = 12,

              p("The main goal in this tab is to extract relevant",
                "parameters from pre-existing data.",
                "Ultimately, you want to choose a species that",
                HTML(paste0(
                  span("behaves similarly", style = txt_key), ".")),
                "to your intended study species,",
                "as all subsequent steps will built upon these",
                "parameters.",
                "You can choose one of two paths:",
                span("Option 1.", style = col_main),
                "Select one of the seven species available within the",
                a(href = mainlink_ctmm, 'ctmm'), 'package',
                "and extract their parameters.",
                span("Option 2.", style = col_main),
                "Upload a dataset as a .csv file with at least",
                "four variables:",
                HTML(paste0(span("animal ID", style = txt_caution), ",")),
                span("x", style = txt_caution), "and",
                span("y", style = txt_caution), "coordinates, and",
                HTML(paste0(span("timestamp", style = txt_caution), ".")),
                br()),

              p("Which one do you choose?",
                style = txt_label_bold),

              uiOutput(ns("dataInput_type"))

            ), # end of column (text)

            uiOutput(ns("dataText_intro"))

          ) # end of box // data_intro
      ), # end of div (top row)

      # [right column] ----------------------------------------------------

      div(class = div1_column_right,

          # Submit or select data: ----------------------------------------

          uiOutput(ns("dataBox_option1")), # select data
          uiOutput(ns("dataBox_option2")), # submit file

          # Add species name and select individual for option 2:
          uiOutput(ns("dataBox_option2_species")),

          # Select variables: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Variables", style = ttl_box.solid),
            id = ns("dataBox_variables"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE, closable = FALSE,

            selectInput(inputId = ns("dataVar_x"),
                        label = "X coordinate:",
                        selected = "x",
                        choices = ""),
            selectInput(inputId = ns("dataVar_y"),
                        label = "Y coordinate:",
                        selected = "y",
                        choices = ""),
            selectInput(inputId = ns("dataVar_t"),
                        label = "Datetime:",
                        selected = "timestamp",
                        choices = ""),

            actionButton(
              inputId = ns("pars.extract"),
              icon =  icon("paper-plane"),
              label = "Extract",
              width = "100%",
              class = "btn-primary")

          ), # end of box // dataBox_variables

          # Tracking regime: ----------------------------------------------

          shinydashboardPlus::box(
            title = span("Tracking regime", style = ttl_box.solid),
            id = ns("dataBox_regime"),
            status = "info",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            fluidRow(
              column(width = 12, uiOutput(ns("dataInfo_dur"))),
              column(width = 12, uiOutput(ns("dataInfo_dti")))
            ) # end of fluidRow

          ) # end of box // dataBox_regime
      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(class = div1_column_left,

          # Visualization: ------------------------------------------------

          shinydashboardPlus::box(
            title = span("Displaying dataset:", style = ttl_box),
            id = ns("dataBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,

            tabsetPanel(
              id = ns("dataTabs_viz"),

              tabPanel(
                value = ns("dataPanel_all"),
                title = tagList(
                  fontawesome::fa(name = "paw", fill = hex_border),
                  span("Data", style = ttl_panel)
                ),

                reactable::reactableOutput(ns("dataTable_all")),
                br(),

                ggiraph::girafeOutput(
                  outputId = ns("dataPlot_all"),
                  width = "100%", height = "100%")

              ), # end of panels (1 out of 3)

              tabPanel(
                value = ns("dataPanel_individual"),
                title = tagList(
                  fontawesome::fa(name = "filter", fill = hex_border),
                  span("Selected individual", style = ttl_panel)
                ),

                ggiraph::girafeOutput(
                  outputId = ns("dataPlot_id"),
                  width = "95%", height = "100%") %>%
                  shinycssloaders::withSpinner(
                    type = getOption("spinner.type", default = 7),
                    color = getOption("spinner.color",
                                      default = "#f4f4f4")),

                DT::dataTableOutput(ns("dataTable_id")) %>%
                  shinycssloaders::withSpinner(
                    type = getOption("spinner.type", default = 7),
                    color = getOption("spinner.color",
                                      default = "#f4f4f4")),

                uiOutput(ns("dataTable_showVars"))

              ), # end of panels (2 out of 3)

              tabPanel(
                value = ns("dataPanel_svf"),
                title = tagList(
                  fontawesome::fa(name = "chart-line",
                                  fill = hex_border),
                  span("Variogram", style = ttl_panel)
                ),

                br(),
                ggiraph::girafeOutput(
                  outputId = ns("dataPlot_svf"),
                  width = "100%", height = "100%") %>%
                  shinycssloaders::withSpinner(
                    proxy.height = "200px",
                    type = getOption("spinner.type", default = 7),
                    color = getOption("spinner.color",
                                      default = "#f4f4f4")
                  ),

                sliderInput(
                  ns("dataVar_timeframe"),
                  label = span(paste("Proportion of the",
                                     "variogram plotted (in %):"),
                               style = txt_label_bold),
                  min = 0, max = 100, value = 65, step = 5),
                br()

              ) # end of panels (1 out of 3)
            ) # end of tabs
          ) # end of box // dataBox_viz

      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          # Displaying relevant metrics: ----------------------------------

          ## Extracted parameters:
          uiOutput(ns("dataBox_parameters")),

          ## Sample sizes:
          shinydashboardPlus::box(
            title = span("Displaying sample sizes:", style = ttl_box),
            id = ns("dataBox_sizes"),
            width = NULL,
            solidHeader = FALSE,

            fluidRow(
              column(width = 4, uiOutput(ns("dataBlock_n"))),
              column(width = 4, uiOutput(ns("dataBlock_Narea"))),
              column(width = 4, uiOutput(ns("dataBlock_Nspeed"))),
            ) # end of fluidRow

          ), # end of box

          # Additional information: ---------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", style = ttl_box),
            id = ns("dataBox_misc"),
            width = NULL, solidHeader = FALSE,

            verbatimTextOutput(outputId = ns("time_data"))

          ) # end of box // dataBox_misc
      ) # end of column (bottom)

    ) # end of fluidRow
  ) # end of tagList
}

#' tab_data Server Functions
#'
#' @noRd
mod_tab_data_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # DYNAMIC UI ELEMENTS -----------------------------------------------
    ## Hide all boxes at start: -----------------------------------------

    tmpnames <- c("viz", "regime", "upload",
                  "variables", "sizes", "misc")

    for(i in 1:length(tmpnames)) {
      shinyjs::hide(id = paste0("dataBox_", tmpnames[i]))
    }

    ## Render data input type: ------------------------------------------

    output$dataInput_type <- renderUI({

      shiny::selectizeInput(
        inputId = ns("data_type"),
        width = "40%",
        label = NULL,
        choices = c("Option 1. Select species" = 1,
                    "Option 2. Upload data" = 2),
        options = list(
          placeholder = 'Select an option here',
          onInitialize = I('function() { this.setValue(""); }')))

    }) # end of renderUI // dataInput_type

    shiny::observe({
      req(vals$active_tab == 'real')

      tmptxt <- span(
        style = ft_center,

        "Then click",
        fontawesome::fa(name = "check-circle", fill = hex_main),
        span("Validate", style = btn_primary), "and",
        fontawesome::fa(name = "paper-plane", fill = hex_main),
        HTML(paste0(span("Extract", style = btn_primary), ".")))

      if(input$data_type == 1) {
        shinyjs::show(id = "dataBox_viz")

        reset_data_values(vals)

        output$dataText_intro <- renderUI({

          column(
            align = "center", width = 12,

            p(style = ft_center,
              "First, choose a species and individual",
              "from the list.",
              br(), tmptxt)

          ) # end of column (text)
        }) # end of renderUI // dataText_intro

        output$dataBox_option1 <- renderUI({

          shinydashboardPlus::box(
            title = span("Species", style = ttl_box.solid),
            id = ns("dataBox_select"),
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
              selected = NULL),

            shiny::selectizeInput(
              inputId = ns("id_selected"),
              label = "Select an individual:",
              choices = "",
              multiple = FALSE,
              options = list(
                placeholder = '(select id)',
                onInitialize = I('function() { this.setValue(""); }'))
            ),

            shiny::actionButton(
              inputId = ns("validate_select"),
              icon =  icon("check-circle"),
              label = "Validate",
              width = "100%")

          ) # end of box // dataBox_select
        }) # end of renderUI // dataBox_option1

        shinyjs::show(id = "dataBox_option1")
        shinyjs::hide(id = "dataBox_option2")
        shinyjs::hide(id = "dataBox_option2_species")

      } else if(input$data_type == 2) {
        shinyjs::show(id = "dataBox_viz")

        reset_data_values(vals)

        output$dataText_intro <- renderUI({

          column(
            align = "center", width = 12,

            p(style = ft_center,
              "First, browse and choose the appropriate",
              span(".csv", style = col_border), "file.",
              br(), tmptxt)

          ) # end of column (text)
        }) # end of renderUI // dataText_intro

        output$dataBox_option2 <- renderUI({

          shinydashboardPlus::box(
            title = span("Upload file", style = ttl_box.solid),
            id = ns("dataBox_file"),
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
              label = span("Header", style = col_black),
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

            shiny::actionButton(
              inputId = ns("confirm_upload"),
              label = "Confirm",
              icon =  icon("upload"),
              width = "100%")

          ) # end of box // dataBox_file
        }) # end of renderUI // dataBox_option2

        shinyjs::show(id = "dataBox_option2")
        shinyjs::hide(id = "dataBox_option1")

      } # end of if() statements

    }) %>% # end of observe, then:
      shiny::bindEvent(input$data_type)

    output$dataBox_option2_species <- renderUI({
      req(vals$dataList)

      dfList <- as_tele_list(vals$dataList)
      sumdfList <- summary(dfList)

      shinydashboardPlus::box(
        title = span("Species", style = ttl_box.solid),
        id = ns("dataBox_upload"),
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
          label = 'Select an individual:',
          choices = row.names(sumdfList),
          selected = row.names(sumdfList)[1]),

        shiny::actionButton(
          inputId = ns("validate_upload"),
          label = "Validate",
          icon =  icon("check-circle"),
          width = "100%")

      ) # end of box
    }) # end of renderUI

    ## If subset data available, update inputs: -------------------------

    observe({
      req(vals$data0)

      updateSelectInput(session, inputId = "dataVar_x",
                        label = "X coordinate:",
                        choices = names(vals$data0),
                        selected =
                          ifelse(!is.null(vals$data0$"x"),
                                 "x", "longitude"))
      updateSelectInput(session, inputId = "dataVar_y",
                        label = "Y coordinate:",
                        choices = names(vals$data0),
                        selected =
                          ifelse(!is.null(vals$data0$"y"),
                                 "y", "latitude"))
      updateSelectInput(session, inputId = "dataVar_t",
                        label = "Datetime:",
                        choices = names(vals$data0),
                        selected =
                          ifelse(!is.null(vals$data0$"timestamp"),
                                 "timestamp", "t"))
    }) # end of observe

    output$dataTable_showVars <- renderUI({
      req(input$dataVar_x,
          input$dataVar_y,
          input$dataVar_t)

      shinyWidgets::pickerInput(
        inputId = ns('show_vars'),
        width = "100%",
        label = span("Columns to show above:",
                     style = txt_label_bold),
        choices = names(vals$data0),
        selected = c(input$dataVar_x,
                     input$dataVar_y,
                     input$dataVar_t),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ), multiple = TRUE)

    }) # end of renderUI // dataTable_showVars

    # SELECT DATA TYPE --------------------------------------------------
    ## 1. Select species from the 'ctmm' package: -----------------------

    ### 1.1. Load species data:

    shiny::observe({
      req(vals$active_tab == 'real', input$sp_selected)

      vals$data0 <- NULL
      vals$data_type <- "selected"

      vals$species <- input$sp_selected
      data(list = vals$species, package = "ctmm")

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

      # Update to match table/plot selection:
      shiny::updateSelectizeInput(
        session,
        inputId = "id_selected",
        label = "Select an individual:",
        choices = names(vals$dataList))

    }) # end of observe

    observe({
      req(vals$dataList, vals$active_tab == 'real')


    }) # end of observe

    # 1.2. Subset data based on individual selection:

    observe({
      req(vals$dataList, vals$active_tab == 'real')

      df_subset <- vals$dataList[[input$id_selected]]

      index <- ctmm_species %>% match(x = vals$species)
      vals$species_common <- names(ctmm_species[index])
      vals$species_binom <- names(ctmm_species_binom[index])
      vals$id <- input$id_selected
      vals$data0 <- df_subset

      # vals$n_ids <- length(input$id_selected)

      shinyjs::show(id = "dataBox_variables")

    }) %>% # end of observe,
      bindEvent(input$id_selected)

    ## 2. Upload .csv file with movement dataset: -----------------------

    ## 2.1. Read in file submission:

    observe({
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

      vals$dataList <- df0

      msg_log(
        style = "success",
        message = paste0("File ",
                         msg_success("submitted"), "."),
        detail = "Please select one individual from this dataset.")

      shinyjs::show(id = "dataBox_upload")
      if(!input$dataBox_file$collapsed) {
        shinydashboardPlus::updateBox("dataBox_file",
                                      action = "toggle")
      } else { NULL }

      shinyjs::show(id = "dataBox_option2_species")

      # Update to match table/plot selection:
      shiny::updateSelectizeInput(
        session,
        inputId = "id_uploaded",
        label = "Select an individual:",
        choices = names(vals$dataList))

    }) %>% # end of observe, then:
      bindEvent(input$confirm_upload)

    # 2.2. Subset data based on individual selection:

    observe({
      req(vals$dataList, vals$active_tab == 'real')

      df_subset <- vals$dataList[[input$id_uploaded]]
      vals$id <- input$id_uploaded
      vals$data0 <- df_subset

      shinyjs::show(id = "dataBox_variables")

    }) %>% # end of observe,
      bindEvent(input$id_uploaded)

    # 2.3 Validate data uploaded:

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
            fontawesome::fa(name = "check-circle",
                            fill = hex_main),
            span("Validate", style = btn_primary),
            'button again.'),
          html = TRUE,
          size = "xs")

      } else {

        vals$species_binom <- input$sp_uploaded
        vals$needs_fit <- TRUE

      } # end of if()
    }) %>% # end of observe,
      bindEvent(input$validate_upload)

    ## Validate data: ---------------------------------------------------

    to_validate <- reactive({
      list(input$validate_select, input$validate_upload)
    })

    observe({
      req(vals$data0, vals$species_binom)

      print("Validating...")

      if(is.null(vals$needs_fit)) {
        vals$needs_fit <- TRUE
      }

      eval_n <- nrow(vals$data0)
      tempnames <- names(summary(vals$data0))
      tempunits <- tempnames[grep('sampling period',
                                  tempnames)] %>%
        extract_units()
      dur <- as.numeric(
        summary(vals$data0)[grep('sampling period',
                                 tempnames)])
      eval_dur <- dur %#% tempunits

      if(eval_dur < (10 %#% "days") || eval_n < 200) {

        shinyalert::shinyalert(
          title = "Warning",
          text = span(
            "Selected individual has a sampling duration",
            "of less than", span("10 days", style = txt_caution),
            "and/or less than",
            HTML(paste0(span("200 locations",
                             style = txt_caution), ".")),
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
        shinyjs::hide(id = "box_mod_extract")

      } else {

        updateTabsetPanel(
          session,
          inputId = "dataTabs_viz",
          selected = "tab_data_1-dataPanel_individual")

        msg_log(
          style = "success",
          message = paste0("Species and individual ",
                           msg_success("validated"), "."),
          detail = paste0("Species selected is the ",
                          msg_success(vals$species),
                          ", and the individual is ",
                          msg_success(vals$id), "."))

        vals$is_valid <- TRUE

        # print(vals$tour_active)
        # if(!input$data_intro$collapsed &&
        #    vals$tour_active) { NULL } else {
        #      shinydashboardPlus::updateBox("data_intro",
        #                                    action = "toggle")
        #    }

      } # end of evaluation
    }) %>% # end of observe,
      bindEvent(to_validate(), ignoreInit = TRUE)

    # SUMMARIZE DATA ----------------------------------------------------

    observe({ # First, create data summary:
      req(vals$dataList)

      if(!("timestamp" %in% names(vals$dataList[[1]]))) {

        vals$is_anonymized <- TRUE

        shiny::showNotification(
          duration = 5,
          ui = shiny::span(
            "Data is anonymized,", br(),
            "simulating location and time."),
          closeButton = FALSE, type = "warning")

        msg_log(
          style = "success",
          message = "Anonymized data completed",
          detail = "Simulated location and time added."
        )

        vals$dataList <- ctmm:::pseudonymize(vals$dataList)

      } else { vals$is_anonymized <- FALSE }

      dfList <- as_tele_list(vals$dataList)
      sumdfList <- summary(dfList)
      for(i in 1:length(dfList)) {
        sumdfList$n[i] <- nrow(dfList[[i]])
      }

      sum_col1 <- grep('period', names(sumdfList))
      sum_col2 <- grep('interval', names(sumdfList))
      sumdfList[,sum_col1] <- round(sumdfList[sum_col1], 1)
      sumdfList[,sum_col2] <- round(sumdfList[sum_col2], 1)
      sumdfList <- sumdfList %>% dplyr::select(-longitude,
                                               -latitude)
      vals$sum <- sumdfList

    }) # end of observe

    # TABLES ------------------------------------------------------------
    ## Table for summary of all individuals: ----------------------------

    output$dataTable_all <- reactable::renderReactable({
      req(vals$sum)

      preselection_table <- NULL
      if(vals$data_type == "selected") {
        tmp <- input$id_selected
      } else if(vals$data_type == "uploaded") {
        tmp <- input$id_uploaded
      }

      if(!is.null(tmp)) {
        if(tmp == "") {
          preselection_table <- NULL
        } else {
          preselection_table <-
            match(tmp,
                  names(vals$dataList)) }
      }

      reactable::reactable(
        vals$sum,
        searchable = TRUE,
        selection = "single",
        onClick = "select",
        highlight = TRUE,

        defaultSelected = preselection_table,
        defaultColDef =
          reactable::colDef(
            headerClass = "rtable_header",
            align = "left"),
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(
            backgroundColor = "#eee",
            boxShadow = "inset 2px 0 0 0 #009da0")),

        columns = list(
          longitude = reactable::colDef(
            format = reactable::colFormat(digits = 3)),
          latitude = reactable::colDef(
            format = reactable::colFormat(digits = 3))))
    })

    output$dataTable_all_state <- renderPrint({
      state <- req(reactable::getReactableState("dataTable_all"))
      vals$table1_state <- state # print(state$selected)
    })

    ## Table for selected individual data: ------------------------------

    output$dataTable_id <- DT::renderDataTable({
      req(vals$data0, input$show_vars)

      temp <- NULL
      temp <- vals$data0[, input$show_vars, drop = FALSE]
      if(!is.null(temp$timestamp)) {
        temp$timestamp <- as.character(temp$timestamp)
      } else { NULL }

      DT::datatable(
        data = temp,
        class = "display nowrap",
        options = list(searching = FALSE,
                       lengthMenu = c(5, 10, 15))
      ) %>% DT::formatRound(
        columns = c("x", "y"),
        digits = 4,
        interval = 3,
        mark = ","
      )

    }) # end of renderDataTable // dataTable_id


    # PLOTS -------------------------------------------------------------
    ## Rendering all data (xy): -----------------------------------------

    output$dataPlot_all <- ggiraph::renderGirafe({
      req(vals$dataList, vals$data_type != "simulated")

      if(vals$data_type == "selected") {

        # to address compatibility of the pelican dataset:
        if(vals$species != "pelican") {
          newdat.all <- as_tele_df(vals$dataList)
        } else {
          data_df <- list()
          for(i in 1:length(vals$dataList)) {
            temp0 <- vals$dataList[i]
            if(names(vals$dataList)[i] == "gps") {
              temp1 <- temp0@.Data[1] %>% as.data.frame
              temp1 <- temp1 %>%
                dplyr::select('gps.timestamp',
                              'gps.x',
                              'gps.y')
            }
            if(names(vals$dataList)[i] == "argos") {
              temp1 <- temp0@.Data[1] %>% as.data.frame
              temp1 <- temp1 %>%
                dplyr::select('argos.timestamp',
                              'argos.x',
                              'argos.y')
            }
            colnames(temp1) <- c("timestamp", "x", "y")
            temp1$name <- rep(names(vals$dataList)[i], nrow(temp1))
            data_df[[i]] <- temp1 } # end of forloop

          newdat.all <- do.call(rbind.data.frame, data_df) }
      } else { newdat.all <- as_tele_df(vals$dataList) }

      p.all <- ggplot2::ggplot() +
        ggiraph::geom_point_interactive(
          data = newdat.all,
          ggplot2::aes(x, y,
                       color = id,
                       tooltip = id,
                       data_id = id),
          size = 1.5, alpha = 0.6) +
        ggplot2::labs(x = "x coordinate",
                      y = "y coordinate") +

        ggplot2::scale_color_grey() +
        theme_movedesign() +
        ggplot2::theme(legend.position = "none")

      if(!is.null(input$id_selected)) {
        if(input$id_selected == "") {
          preselection <- NULL
        } else {
          preselection <- input$id_selected }
      } else { preselection <- NULL }

      if(!is.null(input$id_uploaded)) {
        if(input$id_uploaded == "") {
          preselection <- NULL
        } else {
          preselection <- input$id_uploaded }
      } else { preselection <- NULL }

      ggiraph::girafe(
        ggobj = p.all,
        width_svg = 6, height_svg = 5,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .1),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_selection(
            selected = preselection,
            type = "single",
            css = paste("fill:#dd4b39;",
                        "stroke:#eb5644;")))
      )

    }) # end of renderGirafe

    output$dataPlot_all_state <- renderPrint({
      print(input$dataPlot_all_selected)
      print(input$dataPlot_all_key_selected)
      print(input$dataPlot_all_theme_selected)
    })

    ## Rendering individual data (xy): ----------------------------------

    output$dataPlot_id <- ggiraph::renderGirafe({
      req(vals$data0,
          input$dataVar_x,
          input$dataVar_y,
          input$dataVar_t)

      if( class(vals$data0) == "data.frame" ) {
        vals$data0 <- ctmm::as.telemetry(vals$data0) }

      newdat <- as.data.frame(vals$data0[[input$dataVar_x]])
      names(newdat) <- "x"
      newdat$y <- vals$data0[[input$dataVar_y]]
      newdat$time <- vals$data0[[input$dataVar_t]]

      newdat$time <- as.POSIXct(
        newdat$time, format = "%Y-%m-%d %H:%M:%S")

      tmp <- min(newdat$y) - diff(range(newdat$y)) * .15

      p <- ggplot2::ggplot() +

        ggiraph::geom_path_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = time,
            tooltip = time,
            data_id = time),
          alpha = .9) +
        ggiraph::geom_point_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = time,
            tooltip = time,
            data_id = time),
          size = 2) +

        ggplot2::labs(x = "x coordinate",
                      y = "y coordinate") +

        ggplot2::scale_x_continuous(
          labels = scales::comma) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(tmp, max(newdat$y))) +
        viridis::scale_color_viridis(
          name = "Tracking time:",
          option = "D", trans = "time",
          breaks = c(min(newdat$time),
                     max(newdat$time)),
          labels = c("Start", "End")) +

        theme_movedesign() +
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

      ggiraph::girafe(
        ggobj = p,
        width_svg = 7.5, height_svg = 5,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .1),
          ggiraph::opts_zoom(max = 5),
          # ggiraph::opts_hover_inv(css = "opacity:0.4;"),
          # ggiraph::opts_selection(
          #   type = "single",
          #   css = paste("fill:#dd4b39;",
          #               "stroke:#eb5644;",
          #               "r:5pt;")),
          ggiraph::opts_tooltip(
            # opacity = .8,
            # css = paste("background-color:gray;",
            #             "color:white;padding:2px;",
            #             "border-radius:2px;"),
            use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill:#1279BF;",
                        "stroke:#1279BF;",
                        "cursor:pointer;"))))

    }) # end of renderGirafe // dataPlot_id

    ## Rendering variogram (svf): ---------------------------------------

    output$dataPlot_svf <- ggiraph::renderGirafe({
      req(vals$data_type != "simulated")

      frac <- input$dataVar_timeframe / 100
      svf <- prepare_svf(vals$data0, fraction = frac)
      p <- plotting_svf(svf)

      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_hover(
            css = paste("fill:#ffbf00;",
                        "stroke:#ffbf00;"))
        ))
    })

    # PARAMETERS --------------------------------------------------------

    # After clicking "Extract" button:

    shiny::observe({
      shinyjs::show(id = "dataBox_regime")

      if(is.null(vals$species_binom)) {

        shinyalert::shinyalert(
          title = "Oops!",
          text = span(
            'Please select a species/dataset and an individual',
            'first, then click the',
            fontawesome::fa(name = "check-circle", fill = hex_main),
            span('Validate', style = col_main), "and",
            fontawesome::fa(name = "paper-plane", fill = hex_main),
            span('Extract', style = col_main),
            'buttons.'),
          html = TRUE,
          size = "xs")

      } else {
        req(vals$data0, vals$id, vals$species_binom)
      }

      ## Model fitting: -------------------------------------------------
      req(vals$is_valid)

      start <- Sys.time()

      guess0 <- reactive({
        ctmm::ctmm.guess(vals$data0, interactive = FALSE)
      }) %>% bindCache(vals$species_binom,
                       vals$id)

      vals$guess <- guess0()

      # Fit models to current data:

      if(vals$data_type == "selected") {

        tmpspecies <- vals$species

        msg_log(
          style = "warning",
          message = paste0("Model fit ",
                           msg_warning("available"), "."),
          detail = "...Loading existing model fit now."
        )

        shiny::withProgress({
          fitList <- readRDS(
            system.file("extdata",
                       paste0(tmpspecies, "_fitList.rds"),
                       package = "movedesign"));
          Sys.sleep(1.5) },
          message = "Loading model fit.",
          detail = "Please wait...")

        msg_log(
          style = "success",
          message = "...Model fit loaded.",
          detail = ""
        )

        vals$fit <- fitList[[vals$id]][[1]]
        rm(fitList)

      } else {

        fit0 <- reactive({
          ctmm::ctmm.select(vals$data0, guess0(), trace = TRUE)
        }) %>% bindCache(vals$species_binom,
                         vals$id)

        msg_log(
          style = "warning",
          message = "...Fitting movement models.",
          detail = "Please wait for model selection to finish."
        )

        shiny::withProgress({
          vals$fit <- fit0()
        },
        message = "Fitting different models.",
        detail = "This may take a while...")

        vals$time_data <- difftime(Sys.time(), start,
                                   units = "mins")
        msg_log(
          style = "success",
          message = "Model fitting complete.",
          detail = "")
      }

      shinyjs::show(id = "dataBox_sizes")

      vals$tmpsp1 <- vals$species_common
      vals$tmpsp2 <- vals$species_binom
      vals$tmpid <- vals$id

      sum.fit <- summary(vals$fit)
      vals$needs_fit <- FALSE

      ## Extract semi-variance parameter: -------------------------------

      svf <- prepare_svf(vals$data0, fraction = .65)
      vals$sigma0_min <- mean(svf$var_low95)
      vals$sigma0_max <- mean(svf$var_upp95)

      ## Extract timescale and spatial parameters: ----------------------

      output$dataBox_parameters <- renderUI({

        shinydashboardPlus::box(
          title = span("Displaying parameters:", style = ttl_box),
          id = ns("box_mod_extract"),
          width = NULL,
          solidHeader = FALSE,

          column(
            align = "center", width = 12,

            renderUI({
              if(vals$tmpid == "Simulated individual") {
                NULL
              } else {

                p("These parameters have been extracted from",
                  "individual", span(vals$tmpid,
                                     style = txt_key),
                  "and species", span(vals$tmpsp1,
                                      style = txt_key),
                  span(HTML(
                    paste0("(", em(vals$tmpsp2),
                           ")."))),
                  "They will only update if you change the",
                  "individual and/or species selected, and then",
                  "click the buttons",
                  fontawesome::fa(name = "check-circle",
                                  fill = hex_main),
                  span("Validate", style = btn_primary), "and",
                  fontawesome::fa(name = "paper-plane",
                                  fill = hex_main),
                  span(HTML(paste0(
                    span("Extract", style = btn_primary), "."))))
              } # end of if() statement
            }) # end of renderUI

          ), # end of column (for text)

          column(width = 12, uiOutput(ns("infobox_movprocess"))),

          fluidRow(
            column(width = 4, uiOutput(ns("infobox_sigma"))),
            column(width = 4, uiOutput(ns("infobox_taup"))),
            column(width = 4, uiOutput(ns("infobox_tauv")))
          )

        ) # end of box
      }) # end of renderUI

    }) %>% # end of observe, then:
      bindEvent(input$pars.extract)

    # BLOCKS ------------------------------------------------------------
    ## Movement process: ------------------------------------------------

    output$infobox_movprocess <- shiny::renderUI({
      if(vals$tmpid == "Simulated individual") {
        NULL } else {
          sum.fit <- summary(vals$fit)

          parBlock(
            text = shiny::fluidRow(
              style = paste("margin-bottom: -14px;"),
              actionButton(
                inputId = ns("dataHelp_mods"),
                icon = icon("question-circle"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;")),
              br(), "Movement process"
            ),
            header = sum.fit$name[1])

        } # end of if() statement
    }) # end of renderUI

    ## Timescale parameters: --------------------------------------------

    output$infobox_taup <- shiny::renderUI({
      if(vals$tmpid == "Simulated individual") {
        NULL } else {

          sum.fit <- summary(vals$fit)
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
              text = shiny::fluidRow(
                style = paste("margin-bottom: -14px;"),
                actionButton(
                  inputId = ns("dataHelp_taup"),
                  icon = icon("question-circle"),
                  label = NULL,
                  style = paste("background-color: #fff;",
                                "color: black;",
                                "padding: 0;")), br(),
                span(
                  HTML(paste0("Position autocorrelation ",
                              "(\u03C4", tags$sub("p"), ")")))
              ),
              header =
                paste(scales::label_comma(
                  accuracy = .1)(vals$tau_p0),
                  vals$tau_p0_units),
              number =
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

    output$infobox_tauv <- shiny::renderUI({
      if(vals$tmpid == "Simulated individual") {
        NULL } else {

          sum.fit <- summary(vals$fit)
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
              text = shiny::fluidRow(
                style = paste("margin-bottom: -14px;"),
                actionButton(
                  inputId = ns("dataHelp_tauv"),
                  icon = icon("question-circle"),
                  label = NULL,
                  style = paste("background-color: #fff;",
                                "color: black;",
                                "padding: 0;")), br(),
                span(
                  HTML(paste0("Velocity autocorrelation ",
                              "(\u03C4", tags$sub("v"), ")")))
              ),
              header =
                paste(scales::label_comma(
                  accuracy = .1)(fit.tau_v), tempunits),
              number =
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

    output$infobox_sigma <- shiny::renderUI({
      if(vals$tmpid == "Simulated individual") {
        NULL } else {

          vals$sigma0 <- ctmm:::var.covm(vals$fit$sigma, ave = TRUE)
          vals$sigma0_units <- "m^2"
          sig <- fix_spUnits(vals$sigma0, units = "m^2")

          parBlock(
            text = shiny::fluidRow(
              style = paste("margin-bottom: -14px;"),
              actionButton(
                inputId = ns("dataHelp_sigma"),
                icon = icon("question-circle"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;")), br(),
              span(HTML("Semi-variance (\u03C3)"))
            ),
            header = span(HTML(sig[1], sig[3])),
            number =
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

    output$dataInfo_dur <- shiny::renderUI({

      sum.dat <- summary(vals$data0)
      tempnames <- names(sum.dat)

      tempunits <- tempnames[grep('sampling period',
                                  tempnames)] %>%
        extract_units()

      dur <- as.numeric(
        sum.dat[grep('sampling period',
                     tempnames)])

      vals$dur0 <- dur
      vals$dur0_units <- tempunits

      out <- fix_timeunits(dur, vals$dur0_units)
      parBlock(text = "Sampling duration",
               header = paste(out[1], out[2]))

    }) # ender of renderUI // dataInfo_dur

    output$dataInfo_dti <- shiny::renderUI({

      sum.dat <- summary(vals$data0)
      tempnames <- names(sum.dat)

      tempunits <- tempnames[grep('sampling interval',
                                  tempnames)] %>%
        extract_units()

      dti <- as.numeric(sum.dat[grep('sampling interval',
                                     tempnames)])

      vals$dti0 <- dti
      vals$dti0_units <- tempunits

      out <- fix_timeunits(dti, vals$dti0_units)
      parBlock(text = "Sampling interval",
               header = paste(out[1], out[2]),
               number = "between fixes")

    }) # end of renderUI // dataInfo_dti

    ## Sample sizes: ----------------------------------------------------

    output$dataBlock_n <- shiny::renderUI({
      req(vals$data0)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data0),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)

    }) # end of renderUI // dataBlock_n (absolute sample size)

    output$dataBlock_Narea <- shiny::renderUI({
      req(vals$fit)

      tempnames <- names(summary(vals$fit)$DOF)
      N <- summary(vals$fit)$DOF[grep('area', tempnames)][[1]]
      n <- nrow(vals$data0)

      diff_perc <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      vals$Narea <- N

      sampleBlock(
        number = diff_perc,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // dataBlock_Narea (effective)

    output$dataBlock_Nspeed <- shiny::renderUI({
      req(vals$fit)

      tmpnames <- names(summary(vals$fit)$DOF)
      N <- summary(vals$fit)$DOF[grep('speed', tmpnames)][[1]]
      n <- nrow(vals$data0)

      diff_perc <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      vals$Nspeed <- N

      sampleBlock(
        number = diff_perc,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("speed"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // dataBlock_Nspeed (effective)

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

    }) %>% bindEvent(input$dataHelp_mods)

    output$dataTable_processes <- reactable::renderReactable({

      tmpname <-
        sub('(^\\w+)\\s.+','\\1', summary(vals$fit)$name[1])

      if(is.null(match(tmpname, movmods$name_short))) {
        preselected_mod <- NULL
      } else {
        preselected_mod <- match(tmpname, movmods$name_short)
      }
      df0 <- movmods %>%
        dplyr::select(!name_short)

      reactable::reactable(
        df0,
        searchable = TRUE,
        highlight = TRUE,
        selection = "single",
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
            name = paste0("\u03C4","\u209A"),
            minWidth = 60,
            cell = reactable::JS(
              paste0("function(cellInfo) {
                // Render as an X mark or check mark
                return cellInfo.value === 'No' ? '\u274c No' : ",
                     "'\u2714\ufe0f Yes'}"))),

          tau_v = reactable::colDef(
            name = paste0("\u03C4","\u1D65"),
            minWidth = 60,
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

    output$time_data <- renderText({
      req(vals$time_data)
      paste0("Model fitting took approximately ",
             round(vals$time_data, 1), " minutes.")
    }) # end of renderText // time_data

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_data_ui("tab_data_1")

## To be copied in the server
# mod_tab_data_server("tab_data_1")
