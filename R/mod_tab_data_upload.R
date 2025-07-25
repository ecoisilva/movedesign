#' tab_data_upload UI Function
#'
#' @description Upload data tab module.
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
                
                "Upload a dataset as a", span(".csv", class = "cl-sea"),
                "file with at least four columns:",
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
      
      # [left column] -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-4 col-md-4 col-lg-3",
          
          # Submit data file: ---------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Upload data file:", class = "ttl-box_solid"),
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
            
            shinyWidgets::radioGroupButtons(
              inputId = ns("file_dec"),
              label = "Decimals",
              choices = c("Period (.)" = ".",
                          "Comma (,)" = ","),
              selected = ".",
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-square-o", 
                            style = "color: steelblue")),
              direction = "vertical",
              width = "100%"),
            
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
          
          # Submit calibration file: --------------------------------------
          
          # shinydashboardPlus::box(
          #   title = span("Upload calibration file:",
          #                class = "ttl-box_solid"),
          #   id = ns("uploadBox_calibration"),
          #   status = "primary",
          #   width = NULL,
          #   solidHeader = TRUE,
          #   collapsible = TRUE,
          #   
          #   fileInput(
          #     inputId = ns("calibration_csv"),
          #     label = NULL,
          #     multiple = FALSE,
          #     accept = c("text/csv",
          #                "text/comma-separated-values,text/plain",
          #                ".csv")),
          #   tags$hr(),
          #   
          #   shinyWidgets::radioGroupButtons(
          #     inputId = ns("file_dec"),
          #     label = "Decimals",
          #     choices = c("Period (.)" = ".",
          #                 "Comma (,)" = ","),
          #     selected = ".",
          #     checkIcon = list(
          #       yes = tags$i(class = "fa fa-check-square", 
          #                    style = "color: steelblue"),
          #       no = tags$i(class = "fa fa-square-o", 
          #                   style = "color: steelblue")),
          #     direction = "vertical",
          #     width = "100%"),
          #   
          #   shinyWidgets::radioGroupButtons(
          #     inputId = ns("file_sep"),
          #     label = "Separator",
          #     choices = c("Comma (,)" = ",",
          #                 "Semicolon (;)" = ";",
          #                 "Tab" = "\t"),
          #     selected = ",",
          #     checkIcon = list(
          #       yes = tags$i(class = "fa fa-check-square", 
          #                    style = "color: steelblue"),
          #       no = tags$i(class = "fa fa-square-o", 
          #                   style = "color: steelblue")),
          #     direction = "vertical",
          #     width = "100%"),
          #   
          #   shinyWidgets::radioGroupButtons(
          #     inputId = ns("file_quote"),
          #     label = "Quote",
          #     choices = c("None" = "",
          #                 "Double quote (\")" = '"',
          #                 "Single quote (\')" = "'"),
          #     selected = '"',
          #     checkIcon = list(
          #       yes = tags$i(class = "fa fa-check-square", 
          #                    style = "color: steelblue"),
          #       no = tags$i(class = "fa fa-square-o", 
          #                   style = "color: steelblue")),
          #     direction = "vertical",
          #     width = "100%"),
          #   
          #   footer = shiny::actionButton(
          #     inputId = ns("confirm_upload"),
          #     label = "Confirm",
          #     icon =  icon("upload"),
          #     width = "100%")
          #   
          # ), # end of box // uploadBox_calibration
          
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
              multiple = TRUE,
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
          
          # Sampling schedule: --------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Sampling schedule:", class = "ttl-box"),
            id = ns("uploadBox_schedule"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            fluidRow(
              column(width = 12, mod_blocks_ui(ns("uplBlock_dur"))),
              column(width = 12, mod_blocks_ui(ns("uplBlock_dti")))
            ) # end of fluidRow
            
          ) # end of box // uploadBox_schedule
      ), # end of div (left column)
      
      # [right column] ----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-8 col-md-8 col-lg-9",
          
          # Visualization: ------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Data visualization:", class = "ttl-box"),
            id = ns("uploadBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            mod_viz_ui("comp_viz_uploaded")
            
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
                mod_comp_pars_ui("comp_pars_uploaded")
                
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
                  column(width = 4, mod_blocks_ui(ns("uplBlock_n"))),
                  column(width = 4, mod_blocks_ui(ns("uplBlock_Narea"))),
                  column(width = 4, mod_blocks_ui(ns("uplBlock_Nspeed")))
                ), # end of fluidRow
                
                uiOutput(ns("uploadUI_size_notes"))
                
              ) # end of box // uploadBox_sizes
          ), # end of div
          
          # Additional information: ---------------------------------------
          
          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("uploadBox_misc"),
            width = NULL, solidHeader = FALSE,
            
            verbatimTextOutput(outputId = ns("upload_time"))
            
          ) # end of box // uploadBox_misc
      ) # end of column (bottom)
      
    ), # end of fluidRow
    
    # MODALS: -------------------------------------------------------------
    
    create_modal(var = "taup",  id = "upload"),
    create_modal(var = "tauv",  id = "upload"),
    create_modal(var = "sigma", id = "upload"),
    NULL
    
  ) # end of tagList
}

#' tab_data_upload Server Functions
#'
#' @noRd
mod_tab_data_upload_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    # rv$upload <- reactiveValues() # currently empty
    
    ## Matching id for input, plot and table: -----------------------------
    
    id_debounced <- reactive({
      if (is.null(input$id_uploaded)) return(NULL)
      else return(input$id_uploaded)
    }) %>% debounce(1000)
    
    observe({
      req(rv$active_tab == 'data_upload')
      rv$id <- id_debounced()
    })
    
    observe({
      req(rv$active_tab == 'data_upload', rv$datList)
      
      shiny::updateSelectizeInput(
        session,
        inputId = "id_uploaded",
        choices = names(rv$datList),
        selected = rv$id)
      
      shinyjs::hide(id = "selectBox_pars")
      shinyjs::hide(id = "selectBox_sizes")
      
    }) # end of observe
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide all boxes at start: -------------------------------------------
    
    boxnames <- c("species",
                  "schedule",
                  "parameters",
                  "sizes",
                  "misc")
    
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("uploadBox_", boxnames[i]))
    }
    
    observe({
      req(rv$active_tab, rv$data_type)
      if (rv$active_tab == 'data_upload' && rv$data_type != "uploaded")
        shinyjs::hide(id = "uploadBox_viz")
      
    }) # end of observe
    
    ## Render validate button: --------------------------------------------
    
    output$uploadUI_validate <- renderUI({
      
      out <- shiny::actionButton(
        inputId = ns("validate_upload"),
        icon =  icon("wand-magic-sparkles"),
        label = "Validate",
        width = "100%")
      
      if (!is.null(rv$is_valid)) {
        if (rv$is_valid) 
          out <- shiny::actionButton(
            inputId = ns("validate_upload"),
            icon =  icon("circle-check"),
            label = "Validated!",
            width = "100%",
            class = "btn-info")
      }
      
      return(out)
      
    }) # end of renderUI, "uploadUI_validate"
    
    ## Render notes for low effective sample sizes: -----------------------
    
    output$uploadUI_size_notes <- renderUI({
      req(rv$which_question)
      req(rv$datList, rv$fitList, rv$id)
      req(rv$id %in% names(rv$datList))
      
      add_ui <- FALSE
      ui_N_area <- NULL
      ui_N_speed <- NULL
      
      txt_if_hr <- NULL
      if (length(rv$which_question) > 1) {
        req(rv$tau_p[[1]], rv$tau_v[[1]])
        
        N1 <- do.call(c, extract_dof(rv$fitList[rv$id], name = "area"))
        N2 <- do.call(c, extract_dof(rv$fitList[rv$id], name = "speed"))
        
        add_word <- NULL
        if (any(N1 <= 5) || mean(N1) < 5) {
          ui_N_area <- span(
            span("N[area]", class = "cl-dgr"),
            "is < 5 for", sum(N1 < 5),
            ifelse(sum(N1 < 5) == 1, "individual.", "individuals."))
          add_word <- "also"
          add_ui <- TRUE
          
          txt_if_hr <- wrap_none(
            "Please select only those individuals with ",
            "larger effective sample sizes (ideally > 30), ",
            "and those who meet the range residency ",
            "assumption, before proceeding", css = "cl-dgr",
            end = ".")
        }
        
        if (any(N2 <= 5) || mean(N2) < 5) {
          ui_N_speed <- span(
           span("N[speed]", class = "cl-dgr"), "is", add_word,
           "< 5 for", sum(N2 < 5),
           ifelse(sum(N2 < 5) == 1, "individual.", "individuals."))
          add_ui <- TRUE
        }
        
      } else {
        
        switch(
          rv$which_question,
          "Home range" = {
            req(rv$tau_p[[1]])
            N1 <- do.call(
              c, extract_dof(rv$fitList[rv$id], name = "area"))
            
            if (any(N1 <= 5) || mean(N1) < 5) {
              ui_N_area <- span(
                span("N[area]", class = "cl-dgr"),
                "is < 5 for", sum(N1 < 5),
                ifelse(sum(N1 < 5) == 1, "individual.", "individuals."))
              add_ui <- TRUE
            }
            
          },
          "Speed & distance" = {
            req(rv$tau_v[[1]])
            N2 <- do.call(
              c, extract_dof(rv$fitList[rv$id], name = "speed"))
            
            if (any(N2 <= 5) || mean(N2) < 5) {
              ui_N_speed <- span(
                span("N[speed]", class = "cl-sea"),
                "is < 5 for", sum(N2 < 5),
                ifelse(sum(N2 < 5) == 1, "individual.", "individuals."))
            }
            
          },
          stop(paste0("No handler for ",
                      rv$which_question, "."))
        )
      }
      
      ui <- NULL
      if (add_ui) {
        ui <- span(
          class = "help-block", 
          tagList(
            fontawesome::fa("triangle-exclamation", fill = pal$dgr),
            span("Warning:", class = "help-block-note"), 
            ui_N_area,
            ui_N_speed,
            "Very small effective sample sizes may lead to",
            "negatively biased estimates.",
            txt_if_hr))
        
        rv$add_note <- TRUE
      }
      
      return(ui)
      
    }) # end of renderUI, "uploadUI_size_notes"
    
    ## If data available, update variable inputs: -------------------------
    
    observe({
      req(rv$active_tab == 'data_upload', 
          rv$datList, rv$id)
      
      rv$is_valid <- NULL # was FALSE
      
      shinyjs::show(id = "uploadVar_x")
      shinyjs::show(id = "uploadVar_y")
      shinyjs::show(id = "uploadVar_t")
      
    }) %>% # end of observe,
      bindEvent(rv$id)
    
    observe({
      req(rv$active_tab == 'data_upload')
      req(rv$data_type == "uploaded")
      req(rv$datList)

      out_data <- rv$datList[[1]]
      
      updateSelectInput(
        session, inputId = "uploadVar_x",
        label = "X coordinate:",
        choices = names(out_data),
        selected = ifelse(!is.null(out_data$"x"), "x", "longitude"))
      updateSelectInput(
        session, inputId = "uploadVar_y",
        label = "Y coordinate:",
        choices = names(out_data),
        selected = ifelse(!is.null(out_data$"y"), "y", "latitude"))
      updateSelectInput(
        session, inputId = "uploadVar_t",
        label = "Datetime:",
        choices = names(out_data),
        selected = ifelse(!is.null(out_data$"timestamp"),
                          "timestamp", NULL))
      
    }) %>% # end of observe,
      bindEvent(rv$datList)
    
    observe({
      rv$input_x <- input$uploadVar_x
      rv$input_y <- input$uploadVar_y
      rv$input_t <- input$uploadVar_t
    })
    
    # ALERTS --------------------------------------------------------------
    
    ## During validation:
    
    observe({
      if (is.null(rv$which_question)) {
        
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
      
      req(rv$which_question,
          rv$datList)
      
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
        rv$species <- rv$species_binom <- input$sp_uploaded
        
      } # end of if ()
      
    }) %>% # end of observe,
      bindEvent(input$validate_upload)
    
    ## After clicking "Extract" button:
    
    observe({
      req(rv$which_question)
      
      if (is.null(rv$is_valid)) {
        shinyalert::shinyalert(
          title = "Oops!",
          text = tagList(span(
            'Please select a species and an individual',
            'first, then click the',
            icon("wand-magic-sparkles", class = "cl-mdn"),
            span('Validate', class = "cl-mdn"), "and",
            icon("paper-plane", class = "cl-mdn"),
            span('Extract', class = "cl-mdn"),
            'buttons.')),
          html = TRUE,
          size = "xs")
      }
      
    }) %>% # end of observe, then:
      bindEvent(input$uploadButton_extract)
    
    # OPERATIONS ----------------------------------------------------------
    ## 1. Upload .csv file with data: -------------------------------------
    
    reading_file <- reactive({
      if (is.null(input$file_csv)) return("")
      
      if (input$file_dec == ".") {
        out_file <- tryCatch(read.csv(
          file = input$file_csv$datapath,
          header = TRUE,
          sep = input$file_sep,
          quote = input$file_quote),
          error = function(e) e)
      } else {
        out_file <- tryCatch(read.table(
          file = input$file_csv$datapath,
          header = TRUE,
          sep = input$file_sep,
          quote = input$file_quote,
          dec = input$file_dec),
          error = function(e) e)
      }
      
      if (any(grepl("\u00EF..", colnames(out_file)))) {
        out_file <- tryCatch(read.table(
          file = input$file_csv$datapath,
          header = TRUE,
          sep = input$file_sep,
          fileEncoding = "UTF-8-BOM",
          quote = input$file_quote,
          dec = input$file_dec),
          error = function(e) e)
      }
      
      if (inherits(out_file, "error")) {
        msg_log(
          style = "warning",
          message = paste0("File may be ",
                           msg_warning("incorrectly formatted"), "."),
          detail = "Try a different separator and/or quote.")
        return(NULL)
      }
      
      return(out_file)
      
    }) # end of reactive, reading_file()
    
    observe({
      req(rv$active_tab == 'data_upload')
      
      species <- NULL
      out_dataset <- NULL
      reset_reactiveValues(rv)
      
      out_dataset <- reading_file()
      req(out_dataset)
      
      if ("individual.taxon.canonical.name" %in% names(out_dataset)) {
        species <- out_dataset$individual.taxon.canonical.name[1]
        rv$species <- species
      }
      
      if (any(grepl("UTMzone", names(out_dataset)))) {
        out_dataset <- out_dataset %>%
          dplyr::mutate(
            UTMzone = as.numeric(gsub("\\D", "", .data$UTMzone)))
      }
      
      parsedate::parse_date("1111-11-11")
      
      out_dataset <- tryCatch(
        ctmm::as.telemetry(out_dataset, timeformat = "auto"),
        error = function(e) e) %>%
        suppressMessages() %>% 
        suppressWarnings() %>% 
        quiet()
      
      if (inherits(out_dataset, "error")) {
          if (grepl("Could not identify location columns",
                    out_dataset)) {
            
            if (any(grepl("UTM", names(out_dataset)))) {
            } else {
              msg_log(
                style = "warning",
                message = paste0(
                  "Assuming ", msg_warning("latitude/longitude"), ","),
                detail = paste(
                  "If incorrect, please add missing easting,",
                  "northing, and/or UTM zone columns."))
              
              if (any(grepl("x.", names(out_dataset)))) {
                out_dataset$longitude <- out_dataset[
                  , grepl("x.", names(out_dataset))]
              }
              if (any(grepl("y.", names(out_dataset)))) {
                out_dataset$latitude <- out_dataset[
                  , grepl("y.", names(out_dataset))]
              }
            }
          }
        
        out_dataset <- tryCatch(
          ctmm::as.telemetry(out_dataset, timeformat = "auto"),
          error = function(e) e)
      }
      
      if (inherits(out_dataset, "error")) {
        
        msg_log(
          style = "danger",
          message = paste0("File is ",
                           msg_danger("not correctly formatted"), "."),
          detail = "May be missing one or more columns.")
        
        shinyalert::shinyalert(
          type = "error",
          title = "File invalid",
          text = tagList(span(
            "File is",
            wrap_none(
              span("not correctly formatted", class = "cl-dgr"), ","), 
            "and cannot be converted. Please check for missing columns",
            "(animal ID, x & y coordinates, timestamp).")),
          confirmButtonText = "Dismiss",
          html = TRUE,
          size = "xs")
        
        shinybusy::remove_modal_spinner()
        return(NULL)
      }
      
      msg_log(
        style = "success",
        message = paste0("File ", msg_success("uploaded"), "."))
      
      req(out_dataset)
      
      # Check number of individuals within dataset:
      
      datList <- out_dataset
      if ("timestamp" %in% names(out_dataset)) {
        datList <- list()
        datList[[1]] <- out_dataset
        names(datList) <- summary(as_tele_list(out_dataset))$identity
      }
      
      # Check if data is anonymized:
      
      if (!("timestamp" %in% names(datList[[1]]))) {
        datList <- pseudonymize(datList)
        
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
      
      newdat <- telemetry_as_df(datList)
      
      if(all(is.na(newdat$x), is.na(newdat$y))) {
        
        msg_log(
          style = "danger",
          message = paste0("Coercion to telemetry object ",
                           msg_danger("failed"), "."),
          detail = "Check column names of coordinates.")
        
        shinybusy::remove_modal_spinner()
        return(NULL)
      }
      
      shinyjs::show(id = "uploadBox_species")
      # shinyjs::hide(id = "uploadVar_x")
      # shinyjs::hide(id = "uploadVar_y")
      # shinyjs::hide(id = "uploadVar_t")
      
      if (!input$uploadBox_file$collapsed) {
        shinydashboardPlus::updateBox("uploadBox_file",
                                      action = "toggle")
      }
      
      if (class(datList)[1] != "list" && 
          class(datList[[1]])[1] != "ctmm")
        datList <- list(datList)
      
      tmp <- list()
      new_nms <- c()
      old_nms <- names(datList)
      for (x in seq_along(datList)) {
        if (nrow(datList[[x]]) > 1) {
          new_nms <- c(new_nms, old_nms[[x]])
          tmp[[x]] <- datList[[x]] 
        } else {
          message("Individual ", x, " removed (only one location).")
          tmp[[x]] <- NULL
        }
      }
      datList <- tmp[!sapply(tmp, is.null)]
      names(datList) <- new_nms
      
      rv$datList <- datList
      rv$fitList <- NULL
      rv$svfList <- NULL
      rv$id <- NULL
      
      rv$time <- list(
        "upload" = c(0, 0),
        "sims" = c(0, 0),
        "hr" = c(0, 0),
        "ctsd" = c(0, 0),
        "total" = c(0, 0))
      
      rv$data_type <- "uploaded"
      
      shinyjs::show(id = "uploadBox_viz")
      
      if (req(species) != "")
        shiny::updateTextInput(
          session = session,
          inputId = "sp_uploaded",
          label = "Scientific name:",
          value = species)
      
      out_data <- rv$datList[[1]]
      req(!is.null(out_data$"timestamp"))
      rv$input_x <- ifelse(!is.null(out_data$"x"), "x", "longitude")
      rv$input_y <- ifelse(!is.null(out_data$"y"), "y", "latitude")
      rv$input_t <- "timestamp"
      
    }) %>% # end of observe,
      bindEvent(input$confirm_upload)
    
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
      
      out <- guess_time(data = rv$datList[rv$id], parallel = rv$parallel)
      
      shinybusy::remove_modal_spinner()
      return(out)
      
    }) %>% # end of reactive, timing_fit()
      bindCache(rv$id, 
                rv$species_binom)
    
    
    fit_model <- reactive({
      out <- NULL
      
      datList <- rv$datList[rv$id]
      guessList <- tryCatch(
        lapply(seq_along(datList), function (x)
          ctmm::ctmm.guess(datList[[x]],
                           interactive = FALSE)),
        error = function(e) e)
      
      if (inherits(guessList, "error")) {
        msg_log(
          style = "danger",
          message = paste0(
            "Parameter guesstimation ", msg_danger("failed"), "."),
          detail = "Submitted file may be incorrectly formatted.")
        return(NULL)
      }
      
      out <- fitting_model(datList)
      
      if (is.null(out)) {
        msg_log(
          style = "danger",
          message = paste0(
            "Model fit ", msg_danger("failed"), "."),
          detail = "Check uploaded data for issues.")
        return(NULL)
      }
      
      return(out)
      
    }) %>% # end of reactive, fit_model()
      bindCache(rv$datList,
                rv$id, 
                rv$species_binom)
    
    observe({
      req(rv$which_question,
          rv$id, rv$datList, rv$species_binom)
      req(input$sp_uploaded != "")
      
      ### Model fitting:
      
      expt <- timing_fit()
      rv$confirm_time <- NULL
      
      # if ((expt$max %#% expt$unit) > (15 %#% "minutes")) {
      #   
      #   out_expt <- fix_unit(expt$max, expt$unit, convert = TRUE)
      #   
      #   shinyalert::shinyalert(
      #     className = "modal_warning",
      #     title = "Do you wish to proceed?",
      #     callbackR = function(x) { rv$confirm_time <- x },
      #     text = tagList(span(
      #       "Expected run time for the next phase", br(),
      #       "is approximately",
      #       wrap_none(span(out_expt$value, out_expt$unit,
      #                      class = "cl-dgr"), ".")
      #     )),
      #     type = "warning",
      #     showCancelButton = TRUE,
      #     cancelButtonText = "Stop",
      #     confirmButtonCol = pal$mdn,
      #     confirmButtonText = "Proceed",
      #     html = TRUE)
      #   
      # } else { rv$confirm_time <- TRUE }
      rv$confirm_time <- TRUE
      
      req(rv$confirm_time)
      start_fit <- Sys.time()
      msg_log(
        style = "warning",
        message = paste0("Model fit ",
                         msg_warning("in progress"), ","),
        detail = "Please wait for model selection to finish:")
      
      m <- length(rv$datList[rv$id])
      if (rv$parallel) m <- round_any(
        m/parallel::detectCores(logical = FALSE), 1, f = ceiling) 
      
      loading_modal("Selecting movement model",
                    exp_time = expt, parallel = rv$parallel, n = m)
      fitList <- fit_model()
      
      if (inherits(fitList, "error")) {
        proceed <- NULL
        msg_log(
          style = "danger",
          message = paste0("Model fit ", msg_danger("failed"), "."),
          detail = "May be due to low absolute sample size.")
        req(proceed)
        
      } else {
        
        if (class(fitList)[1] != "list" && 
            class(fitList[[1]])[1] != "ctmm")
          fitList <- list(fitList)
        
        names(fitList) <- names(rv$datList[rv$id])
        rv$fitList <- fitList
        rv$is_isotropic <- fitList[[1]]$sigma@isotropic[[1]]
      }
      
      req(rv$fitList)
      time_fit <- difftime(Sys.time(), start_fit, units = "sec")
      rv$time[["upload"]][[1]] <- 
        rv$time[["upload"]][[1]] + time_fit[[1]]
      
      msg_log(
        style = 'success',
        message = paste0("Model fit ",
                         msg_success("completed"), "."),
        run_time = time_fit)
      
      shinybusy::remove_modal_spinner()
      
      ### Set up for validation:
      
      taup <- extract_pars(rv$fitList, name = "position", meta = TRUE)
      tauv <- extract_pars(rv$fitList, name = "velocity", meta = TRUE)
      
      ### Validate based on research question(s):
      
      rv$is_valid <- TRUE
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
            "estimation.")),
          confirmButtonText = "Dismiss",
          html = TRUE)
        
        rv$is_valid <- NULL
      }
      
      req(rv$is_valid)
      if ("Home range" %in% rv$which_question && is.null(taup)) {
        
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
          message = paste(
            "No signature of",
            msg_danger("position autocorrelation"), "found."),
          detail = "Select different dataset(s) to proceed.")
        
        rv$is_valid <- NULL
      }
      
      if ("Speed & distance" %in% rv$which_question && is.null(tauv)) {
        
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
          message = paste(
            "No signature of",
            msg_danger("velocity autocorrelation"), "found."),
          detail = "Select a different dataset to proceed.")
        
        rv$is_valid <- NULL
      }
      
      req(rv$is_valid)
      
      if (length(rv$datList) == 1)
        txt_extra <- ", and the individual is " else
          txt_extra <- ", and the individuals are:\n   "
      
      msg_log(
        style = "success",
        message = paste0("Species and individual ",
                         msg_success("validated"), "."),
        detail = paste0("Species selected is the ",
                        msg_success(rv$species_binom),
                        txt_extra, msg_success(toString(rv$id)),"."))
      
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
      
      shinyjs::show(id = "uploadBox_parameters")
      rv$confirm_time <- FALSE
      
    }) %>% # end of observe,
      bindEvent(input$validate_upload)
    
    # PARAMETERS ----------------------------------------------------------
    ## Extract timescales, location variance, etc.: -----------------------
    
    observe({
      req(rv$which_question,
          rv$data_type == "uploaded", rv$id, rv$is_valid)
      req(rv$datList, rv$fitList)
      
      shinyjs::show(id = "uploadBox_schedule")
      shinyjs::show(id = "uploadBox_sizes")
      shinyjs::show(id = "uploadBox_parameters")
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = tagList(
          span("Extracting", style = "color: #797979;"),
          wrap_none(span("parameters", class = "cl-sea"),
                    span("...", style = "color: #797979;"))))
      
      rv$meanfitList <- NULL
      dat0 <- rv$datList[rv$id]
      fit0 <- rv$fitList[rv$id]
      
      nm_mods <- lapply(rv$fitList, function(x) summary(x)$name)
      n_OUf <- sum(grepl("^OUf", nm_mods))
      
      to_filter_out <- paste0("^OU\u03A9")
      if (any(grep(to_filter_out, unlist(nm_mods), perl = TRUE))) {
        to_remove <- grep(to_filter_out, unlist(nm_mods), perl = TRUE)
        
        msg_log(
          style = "danger",
          message = paste0(
            "Individual(s) ", msg_danger("removed"), ": ",
            msg_danger(toString(names(fit0)[to_remove]))),
          detail = "Movement model OU\u03A9 is invalid.")
        
        fit0 <- fit0[-to_remove]
        rv$id <- rv$id[-to_remove]
        nm_mods <- lapply(fit0, function(x) summary(x)$name)
      }
      
      # to_filter <- "^IOU|^OUF|^OU(?!f)"
      if (length(rv$which_question) == 1) {
        if ("Home range" == rv$which_question) {
          msg_log(
            style = "danger",
            message = paste0(
              "Assuming ", msg_danger("range residency"), ","),
            detail = paste("Assuming all selected individuals",
                           "are range resident."))
          # to_filter <- "^OU(?!f)|^OUF"
        }

        if ("Speed & distance" == rv$which_question) {
          # to_filter <- "^IOU|^OUF"
        }
      }
      
      # fit0 <- fit0[grep(to_filter, unlist(nm_mods), perl = TRUE)]
      
      if (length(fit0) == 0) {
        msg_log(
          style = "error",
          message = paste0(
            "Extraction ", msg_danger("failed"), ","),
          detail = paste("No individuals left after",
                         "filtering for movement processes."))
        shinybusy::remove_modal_spinner()
        
        shinyalert::shinyalert(
          type = "error",
          title = "Individuals invalid",
          text = tagList(span(
            "No individuals left after filtering for",
            "movement models with a signature of the relevant",
            wrap_none(
              span(" autocorrelation timescale",
                   class = "cl-dgr"), "."),
            "Please select different individuals to proceed.")),
          confirmButtonText = "Dismiss",
          html = TRUE,
          size = "xs")
        
        return(NULL)
      }
      
      rv$is_isotropic <- c("All" = TRUE)
      if (rv$add_ind_var) {
        
        fit0[sapply(fit0, is.null)] <- NULL
        meanfit0 <- tryCatch(
          mean(x = fit0, sample = TRUE) %>%
            suppressMessages() %>%
            suppressWarnings() %>%
            quiet(),
          error = function(e) e)
        
        if (inherits(meanfit0, "error")) {
          msg_log(
            style = "danger",
            message = paste0(
              "Cannot incorporate ",
              msg_danger("population variation"), ","),
            detail = "Reverting to population mean estimates only.")
          
          fit0 <- rv$fitList[rv$id]
          get_meta <- ifelse(length(rv$id) == 1, FALSE, TRUE)
          rv$sigma <- extract_pars(fit0, "sigma", meta = get_meta)
          rv$tau_p <- extract_pars(fit0, "position", meta = get_meta)
          rv$tau_v <- extract_pars(fit0, "velocity", meta = get_meta)
          rv$speed <- extract_pars(fit0, "speed", meta = get_meta)
          rv$add_ind_var <- FALSE
          
        } else {
          
          rv$sigma <- extract_pars(meanfit0, name = "sigma")
          rv$tau_p <- extract_pars(meanfit0, name = "position")
          rv$tau_v <- extract_pars(meanfit0, name = "velocity")
          rv$speed <- extract_pars(meanfit0, name = "speed")
          rv$meanfitList <- list(meanfit0)
          names(rv$meanfitList) <- c("All")
          rv$is_isotropic <- c("All" = meanfit0$sigma@isotropic[[1]])
        }
        
      } else {
        
        fit0 <- rv$fitList[rv$id]
        get_meta <- ifelse(length(rv$id) == 1, FALSE, TRUE)
        rv$sigma <- extract_pars(fit0, "sigma", meta = get_meta)
        rv$tau_p <- extract_pars(fit0, "position", meta = get_meta)
        rv$tau_v <- extract_pars(fit0, "velocity", meta = get_meta)
        rv$speed <- extract_pars(fit0, "speed", meta = get_meta)
        
        if (n_OUf >= 1)
          msg_log(
            style = "danger",
            message = paste0(
              "OUf process(es) ", msg_danger("selected"), ","),
            detail = paste("Cannot distinguish between",
                           "autocorrelation timescales."))
      }
      
      rv$mu <- list(array(0, dim = 2, 
                          dimnames = list(c("x", "y"))))
      
      names(rv$sigma) <- c("All")
      if (!is.null(rv$tau_p)) names(rv$tau_p) <- c("All")
      if (!is.null(rv$tau_v)) names(rv$tau_v) <- c("All")
      if (!is.null(rv$speed)) names(rv$speed) <- c("All")
      names(rv$mu) <- c("All")
      
      rv$proceed <- TRUE
      
      if (rv$grouped) {
        
        rv$is_isotropic <- c(rv$is_isotropic, "A" = TRUE, "B" = TRUE)
        fitA <- rv$fitList[rv$groups[[1]][["A"]]]
        fitB <- rv$fitList[rv$groups[[1]][["B"]]]
        
        meanfitA <- tryCatch(
          mean(fitA) %>% 
            suppressMessages() %>% 
            suppressWarnings() %>% 
            quiet(),
          error = function(e) e)
        
        meanfitB <- tryCatch(
          mean(fitB) %>% 
            suppressMessages() %>% 
            suppressWarnings() %>% 
            quiet(),
          error = function(e) e)
        
        if (inherits(fitA, "error") ||
            inherits(fitB, "error")) {
          
          msg_log(
            style = "danger",
            message = paste0(
              "Extraction ", msg_danger("failed"), 
              "for one of the groups."))
          rv$add_ind_var <- FALSE
          
        } else {
          rv$meanfitList <- list(rv$meanfitList[[1]],
                                 meanfitA, meanfitB)
          names(rv$meanfitList) <- c("All", "A", "B")
          rv$is_isotropic <- c(
            rv$is_isotropic[[1]],
            "A" = meanfitA$sigma@isotropic[[1]],
            "B" = meanfitB$sigma@isotropic[[1]])
        }
        
        rv$mu <- list(rv$mu[[1]], rv$mu[[1]], rv$mu[[1]])
        
        ### Validate groups: ----------------------------------------------
        
        fitA <- tryCatch({
          simulate_seeded(rv$meanfitList[["A"]], rv$seed0)
        }, error = function(e) {
          message("A warning occurred:", conditionMessage(e), "\n")
        })
        
        fitB <- tryCatch({
          simulate_seeded(rv$meanfitList[["B"]], rv$seed0)
        }, error = function(e) {
          message("A warning occurred:", conditionMessage(e), "\n")
        })
        
        validate_A <- tryCatch({
          ctmm::simulate(fitA, t = seq(0, 100, by = 1), seed = rv$seed0)
        }, error = function(e) {
          return(NULL)
        })
        
        validate_B <- tryCatch({
          ctmm::simulate(fitB, t = seq(0, 100, by = 1), seed = rv$seed0)
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.null(validate_A) || is.null(validate_B)) {
          bug_group <- c()
          if (is.null(validate_A)) bug_group <- c(bug_group, "A")
          if (is.null(validate_B)) bug_group <- c(bug_group, "B")
          
          msg_log(
            style = "danger",
            message = paste0(
              "Validation ", msg_danger("failed"),
              " of group(s): ", msg_danger(toString(bug_group))),
            detail = "Try again with different groupings.")
          
          rv$is_valid <- FALSE
          are_groups_valid <- FALSE
          shinybusy::remove_modal_spinner()
          
          shinyalert::shinyalert(
            type = "error",
            title = paste(span("Invalid", class = "cl-dgr"), "groups"),
            text = tagList(span(
                "Please try selecting differents individuals",
                "in each", span("group", class = "cl-dgr"),
                "(start by removing those with",
                wrap_none(span("N < 5", class = "cl-dgr"), "),"),
              "or chose a different", span("dataset", class = "cl-dgr"), 
              "altogether, before proceeding.")),
            html = TRUE,
            size = "xs")
          rv$proceed <- FALSE
          
        } else {
          
          msg_log(
            style = "success",
            message = paste0(
              "Groups ", msg_success("validated"), "."),
            detail = paste0(
              "Group A is ",
              msg_success(toString(rv$groups[["intro"]][["A"]])), ";",
              "\n", "   Group B is ",
              msg_success(toString(rv$groups[["intro"]][["B"]])), "."))
          rv$proceed <- TRUE
          are_groups_valid <- TRUE
          
          shinybusy::remove_modal_spinner()
        }
        
        # end of if (rv$grouped)
        
      } else are_groups_valid <- TRUE
      
      if (!rv$proceed || !are_groups_valid) {
        
      }
      req(rv$proceed, are_groups_valid)
      
      ### Extract variogram:
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "var(--sea)",
        text = tagList(span(
          style = "font-size: 18px;",
          span("Extracting", style = "color: #797979;"),
          wrap_none(span("variogram", class = "cl-sea"),
                    span("...", style = "color: #797979;")))))
      
      msg_log(
        style = "warning",
        message = paste0("Extracting ", msg_warning("variograms"), ","),
        detail = "This may take a while...")
      
      start_svf <- Sys.time()
      rv$svfList <- extract_svf(rv$datList[rv$id],
                                rv$fitList[rv$id], fraction = 1)
      time_svf <- difftime(Sys.time(), start_svf, units = "sec")
      
      msg_log(
        style = 'success',
        message = paste0("Variograms ", msg_success("extracted"), ","),
        run_time = time_svf)
      
      shinybusy::remove_modal_spinner()
      
      rv$tmp$sp <- rv$species_binom
      rv$tmp$id <- rv$id
      
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
        message = paste0("All parameters ",
                         msg_success("extracted"), "."),
        detail = paste("Proceed to",
                       msg_success('Sampling design'), "tab."))
      
      if (!rv$tour_active) {
        shinyalert::shinyalert(
          className = "modal_success",
          type = "success",
          title = "Success!",
          text = tagList(span(
            "Proceed to the", br(),
            icon("stopwatch", class = "cl-mdn"),
            span("Sampling design", class = "cl-mdn"), "tab."
          )),
          html = TRUE,
          size = "xs")
      }
      
      shinyjs::hide(id = "uploadVar_x")
      shinyjs::hide(id = "uploadVar_y")
      shinyjs::hide(id = "uploadVar_t")
      shinyjs::show(id = "uploadBox_schedule")
      
    }) %>% # end of observe, then:
      bindEvent(input$uploadButton_extract)
    
    ## Extract parameters for groups: -------------------------------------
    
    observe({
      req(rv$proceed)
      req(rv$is_valid,
          rv$which_question,
          rv$data_type == "uploaded",
          rv$which_meta == "compare", 
          rv$active_tab == 'data_upload')
      req(length(rv$sigma) == 1)
      req(rv$datList, rv$fitList, rv$groups)
      
      fit <- list(A = rv$fitList[rv$groups[[1]]$A],
                  B = rv$fitList[rv$groups[[1]]$B])
      
      rv$sigma <- c(rv$sigma, lapply(1:2, function(x) {
        extract_pars(
          obj = fit[[x]],
          name = "sigma", meta = TRUE)[[1]]
      }))
      names(rv$sigma) <- c("All", "A", "B") 
      
      rv$tau_p <- c(rv$tau_p, lapply(1:2, function(x) {
        extract_pars(
          obj = fit[[x]], 
          name = "position", meta = TRUE)[[1]]
      }))
      names(rv$tau_p) <- c("All", "A", "B") 
      
      rv$tau_v <- c(rv$tau_v, lapply(1:2, function(x) {
        extract_pars(
          obj = fit[[x]], 
          name = "velocity", meta = TRUE)[[1]]
      }))
      names(rv$tau_v) <- c("All", "A", "B") 
      
      rv$speed <- c(rv$speed, lapply(1:2, function(x) {
        extract_pars(
          obj = fit[[x]],
          name = "speed", meta = TRUE)[[1]]
      }))
      names(rv$speed) <- c("All", "A", "B") 
      
      rv$mu <- list(array(0, dim = 2, 
                          dimnames = list(c("x", "y"))),
                    array(0, dim = 2, 
                          dimnames = list(c("x", "y"))),
                    array(0, dim = 2, 
                          dimnames = list(c("x", "y"))))
      names(rv$mu) <- c("All", "A", "B") 
      
      rv$proceed <- NULL
      
    }) %>% # end of observe,
      bindEvent(rv$proceed)
    
    # BLOCKS --------------------------------------------------------------
    ## Sampling schedule: -------------------------------------------------
    
    observe({
      req(rv$active_tab == 'data_upload')
      req(rv$datList, rv$id)
      req(rv$id %in% names(rv$datList))
      
      mod_blocks_server(
        id = "uplBlock_dur",
        rv = rv, data = rv$datList[rv$id],
        type = "dur")
      
      mod_blocks_server(
        id = "uplBlock_dti", 
        rv = rv, data = rv$datList[rv$id],
        type = "dti")
      
    }) # end of observe
    
    ## Sample sizes: ------------------------------------------------------
    
    observe({
      req(rv$active_tab == 'data_upload', rv$datList, rv$id)
      req(rv$id %in% names(rv$datList))
      
      datList <- rv$datList[rv$id]
      
      mod_blocks_server(
        id = "uplBlock_n", 
        rv = rv, data = rv$datList[rv$id],
        type = "n",
        options = list(rightBorder = FALSE,
                       marginBottom = TRUE))
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'data_upload')
      req(rv$datList, rv$fitList, rv$id, rv$is_valid)
      req(rv$id %in% names(rv$datList))
      
      mod_blocks_server(
        id = "uplBlock_Narea", 
        rv = rv, data = rv$datList[rv$id], obj = rv$fitList[rv$id],
        type = "N", name = "area")
      
      mod_blocks_server(
        id = "uplBlock_Nspeed", 
        rv = rv, data = rv$datList[rv$id], obj = rv$fitList[rv$id],
        type = "N", name = "speed")
      
    }) # end of observe
    
    # MISC ----------------------------------------------------------------
    
    output$upload_time <- renderText({
      req(rv$time[["upload"]][[1]] > 0)
      
      out <- fix_unit(rv$time[["upload"]][[1]],
                      "seconds", convert = TRUE)
      
      return(paste0("Model fitting took approximately ",
                    out$value, " ", out$unit, "."))
      
    }) # end of renderText, "upload_time"
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_data_upload_ui("tab_data_upload_1")

## To be copied in the server
# mod_tab_data_upload_server("tab_data_upload_1")
