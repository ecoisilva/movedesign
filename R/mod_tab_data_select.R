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
              
            ), # end of column (text)
            
            footer = uiOutput(ns("selectUI_reset"))
            
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
              multiple = TRUE,
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
              column(width = 12, mod_blocks_ui(ns("selBlock_dur"))),
              column(width = 12, mod_blocks_ui(ns("selBlock_dti")))
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
                id = ns("selectBox_pars"),
                width = NULL,
                solidHeader = FALSE,
                
                ## Extracted parameters:
                mod_comp_pars_ui("comp_pars_selected")
                
              ) # end of box // selectBox_pars
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
                  column(width = 4, mod_blocks_ui(ns("selBlock_n"))),
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
    
    create_modal(var = "taup",  id = "select"),
    create_modal(var = "tauv",  id = "select"),
    create_modal(var = "sigma", id = "select"),
    NULL
    
  ) # end of tagList
}

#' tab_data_select Server Functions
#'
#' @noRd
mod_tab_data_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    ## Matching id for input, plot and table: -----------------------------
    
    id_debounced <- reactive({
      if (is.null(input$id_selected)) return(NULL)
      else return(input$id_selected)
    }) %>% debounce(1000)
    
    observe({
      req(rv$active_tab == 'data_select')
      rv$id <- id_debounced()
    })
    
    observe({
      req(rv$active_tab == 'data_select', rv$datList)
      
      shiny::updateSelectizeInput(
        session,
        inputId = "id_selected",
        choices = names(rv$datList),
        selected = rv$id)
      
    }) # end of observe
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    ## Hide all boxes at start: -------------------------------------------
    
    boxnames <- c("regime", 
                  "pars",
                  "sizes",
                  "misc")
    
    for (i in 1:length(boxnames)) {
      shinyjs::hide(id = paste0("selectBox_", boxnames[i]))
    }
    
    observe({
      req(rv$active_tab, rv$data_type)
      if (rv$active_tab == 'data_select' && rv$data_type != "selected")
        shinyjs::hide(id = "selectBox_viz")
      
    }) # end of observe
    
    ## Add scientific name below common name: -----------------------------
    
    output$binomial_name <- renderText({
      req(rv$species != "")
      
      nms <- c("buffalo" = "Syncerus caffer",
               "pelican" = "Pelecanus occidentalis",
               "coati" = "Nasua narica",
               "jaguar" = "Panthera onca",
               "wolf" = "Chrysocyon brachyurus",
               "gazelle" = "Procapra gutturosa",
               "turtle" = "Glyptemys insculpta")
      
      return(nms[rv$species][[1]])
      
    }) %>% # end of renderText, "binomial_name",
      bindEvent(rv$species)
    
    ## Render validate button: --------------------------------------------
    
    output$selectUI_validate <- renderUI({
      
      out <- shiny::actionButton(
        inputId = ns("validate_select"),
        icon =  icon("wand-magic-sparkles"),
        label = "Validate",
        width = "100%")
      
      if (!is.null(rv$is_valid)) {
        if (rv$is_valid) 
          out <- shiny::actionButton(
            inputId = ns("validate_select"),
            icon =  icon("circle-check"),
            label = "Validated!",
            width = "100%",
            class = "btn-info")
      }
      
      return(out)
      
    }) # end of renderUI, "selectUI_validate"
    
    ## Render introduction box footer (reset button): ---------------------
    
    output$selectUI_reset <- renderUI({
      req(rv$datList, rv$species)
      
      if (rv$data_type != "selected") {
        ui <- tagList(column(
          width = 12, align = "right",
          style = "padding-right: 0px;",
          
          shiny::actionButton(
            inputId = ns("selectButton_reset"),
            label = span("Reset", span("all values", class = "cl-sea")),
            icon = icon("trash-can"),
            class = "btn-primary",
            width = "120px")
        ))
        
      } else ui <- NULL
      return(ui)
      
    }) # end of renderUI, "selectUI_reset"
    
    # ALERTS --------------------------------------------------------------
    
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
            'buttons.'),
          html = TRUE,
          size = "xs"))
      }
      
    }) %>% # end of observe, then:
      bindEvent(input$selectButton_extract)
    
    # OPERATIONS ----------------------------------------------------------
    ## 1. Select data: ----------------------------------------------------
    
    ### 1.1. Load species dataset from the 'ctmm' package:
    
    observe({
      req(input$sp_selected != "",
          length(input$sp_selected) != 0,
          rv$active_tab == 'data_select')
      
      # shinyjs::hide(id = "selectBox_pars")
      # shinyjs::hide(id = "selectBox_sizes")
      
      out_dataset <- NULL
      reset_reactiveValues(rv) # reset rv between data tabs

      utils::data(list = input$sp_selected, package = "ctmm")
      out_dataset <- get(input$sp_selected)
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
      
      if (class(out_dataset)[1] != "list" && 
          class(out_dataset[[1]])[1] != "ctmm")
        out_dataset <- list(out_dataset)
      
      rv$species <- input$sp_selected
      req(!anyNA(names(out_dataset)))
      
      shiny::updateSelectizeInput(
        session,
        inputId = "id_selected",
        choices = names(rv$datList),
        selected = "")
      
      rv$datList <- out_dataset
      rv$svfList <- extract_svf(out_dataset, fraction = 1)
      rv$fitList <- NULL
      rv$id <- NULL
      
      rv$data_type <- "selected"
      index <- rownames(rv$ctmm) %>% match(x = input$sp_selected)
      rv$species_common <- rv$ctmm[index, 1]
      rv$species_binom <- rv$ctmm[index, 2]
      
      shinyjs::show(id = "selectBox_pars")
      shinyjs::show(id = "selectBox_viz")
      shinyjs::hide(id = "selectBox_sizes")
      
      return(out_dataset)

    }) %>% # end of observe,
      bindEvent(input$sp_selected)
    
    # 1.2. Subset and set data columns (x, y, t):
    
    observe({
      req(rv$active_tab == 'data_select',
          rv$datList, rv$id)
      
      out_data <- rv$datList[rv$id]
      req(!is.null(out_data[[1]]$"timestamp"))
      rv$input_x <- ifelse(!is.null(out_data[[1]]$"x"), "x", "longitude")
      rv$input_y <- ifelse(!is.null(out_data[[1]]$"y"), "y", "latitude")
      rv$input_t <- "timestamp"
      
    }) %>% # end of observe,
      bindEvent(rv$id)
    
    ## 2. Validate data: --------------------------------------------------
    
    observe({
      req(rv$active_tab == 'data_select',
          rv$data_type == "selected",
          rv$species,
          rv$datList)
      
      # msg_log(
      #   style = "warning",
      #   message = paste0("Model fit ",
      #                    msg_warning("found"), "..."))
      
      fitList <- readRDS(
        system.file("extdata",
                    paste0(isolate(rv$species), "_fitList.rds"),
                    package = "movedesign"))
      
      # msg_log(paste0("...Model fit ",
      #               msg_success("loaded"), "."))
      
      rv$fitList <- lapply(seq_along(fitList), function(x)
        fitList[[x]][[1]])
      names(rv$fitList) <- names(isolate(rv$datList))
      
    }) %>% # end of observe,
      bindEvent(rv$species)
    
    
    observe({
      
      if (is.null(rv$which_question))
        shinyalert::shinyalert(
          title = "No research goal selected",
          text = tagList(span(
            "Please select a research question in the",
            icon("house", class = "cl-blk"),
            span("Home", class = "cl-blk"),
            "tab before proceeding.")),
          html = TRUE,
          size = "xs")
      
    }) %>% bindEvent(input$validate_select)
    
    is_valid <- reactive({
      req(rv$datList, rv$fitList, rv$id)
      
      ### Set up for validation:
      
      fitList <- rv$fitList[rv$id]
      taup <- extract_pars(fitList, name = "position")
      tauv <- extract_pars(fitList, name = "velocity")
      
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

      } else if ("Home range" %in% rv$which_question) {
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
        
      } else if ("Speed & distance" %in% rv$which_question) {
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
      
      req(rv$id)
      rv$is_valid <- is_data_valid
      return(is_data_valid)
      
    }) # end of reactive

    observe({
      req(input$id_selected != "")
      req(is_valid())

      msg_log(
        style = "success",
        message = paste0("Species and individual(s) ",
                         msg_success("validated"), "."),
        detail = paste0("Species selected is the ",
                        msg_success(input$sp_selected),
                        ", with the selected individual(s): ",
                        msg_success(toString(input$id_selected)), "."))
      
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
    
    observe({
      req(rv$which_question,
          rv$data_type == "selected",
          rv$datList, rv$fitList, rv$id, rv$is_valid)
      
      if (("compare" %in% rv$which_meta) && 
          (length(rv$groups[[1]]$A) == 0 ||
           length(rv$groups[[1]]$B) == 0)) {
        
        shinyalert::shinyalert(
          type = "error",
          title = "No groups found",
          text = tagList(span(
            "No groups were set, or one of the groups is blank.",
            "Go to the",
            icon("object-ungroup", class = "cl-jgl"),
            span("Groups", class = "cl-jgl"), "tab to fix."
          )),
          html = TRUE,
          size = "xs")
        
      } else {
        
        shinyjs::show(id = "selectBox_regime")
        shinyjs::show(id = "selectBox_sizes")
        
        dat0 <- rv$datList[rv$id]
        fit0 <- rv$fitList[rv$id]
        get_meta <- ifelse(length(rv$id) == 1, FALSE, TRUE)
        
        shinybusy::show_modal_spinner(
          spin = "fading-circle",
          color = "var(--sea)",
          text = tagList(
            span("Extracting", style = "color: #797979;"),
            wrap_none(span("parameters", class = "cl-sea"),
                      span("...", style = "color: #797979;"))))
        
        rv$sigma <- extract_pars(
          obj = fit0, data = dat0, name = "sigma", meta = get_meta)
        rv$tau_p <- extract_pars(
          obj = fit0, name = "position", meta = get_meta)
        rv$tau_v <- extract_pars(
          obj = fit0, name = "velocity", meta = get_meta)
        rv$speed <- extract_pars(
          obj = fit0, name = "speed", meta = get_meta)
        
        if (rv$grouped) rv$proceed <- TRUE
  
        if (length(rv$id) == 1) rv$mu <- list(fit0[[1]]$mu)
        else rv$mu <- list(array(0, dim = 2, 
                                 dimnames = list(c("x", "y"))))
        
        rv$svfList <- extract_svf(dat0, fit0)
        
        rv$tmp$sp_common <- rv$species_common
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
          message = paste0("Parameters ",
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
        
        shinybusy::remove_modal_spinner()
      }
      
    }) %>% # end of observe,
      bindEvent(input$selectButton_extract)
    
    ## Extract parameters for groups: -------------------------------------
    
    observe({
      req(rv$proceed,
          rv$which_question,
          rv$data_type == "selected",
          rv$active_tab == 'data_select')
      req(rv$datList, rv$fitList, rv$groups, rv$is_valid)
      req(length(rv$sigma) == 1)
      
      dat <- list(A = rv$datList[rv$groups[[1]]$A],
                  B = rv$datList[rv$groups[[1]]$B])
      fit <- list(A = rv$fitList[rv$groups[[1]]$A],
                  B = rv$fitList[rv$groups[[1]]$B])
      
      lapply(1:2, function(x) {
        extract_pars(
          obj = fit[[x]], data = dat[[x]], 
          name = "sigma", meta = TRUE)[[1]]
      }) %>% c(rv$sigma, .) -> rv$sigma
      names(rv$sigma) <- c("All", "A", "B") 
      
      lapply(1:2, function(x) {
        extract_pars(
          obj = fit[[x]], 
          name = "position", meta = TRUE)[[1]]
      }) %>% c(rv$tau_p, .) -> rv$tau_p
      names(rv$tau_p) <- c("All", "A", "B") 

      lapply(1:2, function(x) {
        extract_pars(
          obj = fit[[x]], 
          name = "velocity", meta = TRUE)[[1]]
      }) %>% c(rv$tau_v, .) -> rv$tau_v
      names(rv$tau_v) <- c("All", "A", "B") 
      
      lapply(1:2, function(x) {
        extract_pars(
          obj = fit[[x]],
          name = "speed", meta = TRUE)[[1]]
      }) %>% c(rv$speed, .) -> rv$speed
      names(rv$speed) <- c("All", "A", "B") 
      
      rv$mu <- list(array(0, dim = 2, 
                          dimnames = list(c("x", "y"))),
                    array(0, dim = 2, 
                          dimnames = list(c("x", "y"))),
                    array(0, dim = 2, 
                          dimnames = list(c("x", "y"))))
      names(rv$mu) <- c("All", "A", "B") 
      
      rv$proceed <- NULL
      
      # modA <- prepare_mod(
      #   tau_p = rv$tau_p[[2]][2, ],
      #   tau_v = rv$tau_v[[2]][2, ],
      #   sigma = rv$sigma[[2]][2, ],
      #   mu = rv$mu[[2]])
      # modB <- prepare_mod(
      #   tau_p = rv$tau_p[[3]][2, ],
      #   tau_v = rv$tau_v[[3]][2, ],
      #   sigma = rv$sigma[[3]][2, ],
      #   mu = rv$mu[[3]])
      # rv$modList <- X
      
    }) %>% # end of observe,
      bindEvent(rv$proceed)
    
    # BLOCKS --------------------------------------------------------------
    ## Tracking regime: ---------------------------------------------------
    
    observe({
      req(rv$active_tab == 'data_select')
      req(rv$datList, rv$id)
      req(rv$id %in% names(rv$datList))
      
      mod_blocks_server(
        id = "selBlock_dur",
        rv = rv, data = rv$datList[rv$id],
        type = "dur")
      
      mod_blocks_server(
        id = "selBlock_dti", 
        rv = rv, data = rv$datList[rv$id],
        type = "dti")
      
    }) # end of observe
    
    ## Sample sizes: ------------------------------------------------------
    
    observe({
      req(rv$active_tab == 'data_select', rv$datList, rv$id)
      req(rv$id %in% names(rv$datList))
      
      datList <- rv$datList[rv$id]
      
      mod_blocks_server(
        id = "selBlock_n", 
        rv = rv, data = rv$datList[rv$id],
        type = "n",
        options = list(rightBorder = FALSE,
                       marginBottom = TRUE))
      
    }) # end of observe
    
    observe({
      req(rv$active_tab == 'data_select')
      req(rv$datList, rv$fitList, rv$id)
      req(rv$id %in% names(rv$datList))
      
      mod_blocks_server(
        id = "selBlock_Narea", 
        rv = rv, data = rv$datList[rv$id], fit = rv$fitList[rv$id],
        type = "N", name = "area")
      
      mod_blocks_server(
        id = "selBlock_Nspeed", 
        rv = rv, data = rv$datList[rv$id], fit = rv$fitList[rv$id],
        type = "N", name = "speed")
      
    }) # end of observe
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_data_select_ui("tab_data_select_1")

## To be copied in the server
# mod_tab_data_select_server("tab_data_select_1")
