#' comp_settings UI Function
#'
#' @description Settings component.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comp_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(

    column(
      width = 12, align = "center",
      
      p(),
      shiny::downloadButton(
        outputId = ns("download_settings"),
        label = "Save settings",
        icon = shiny::icon("download"),
        style = "width: 100%"),
      p(),
      uiOutput(ns("text_save")),
      
      tags$hr(style = "border-color: #ffffff;"),
      
      h5("Restore settings:", style = "color: #fff;"),
      p(style = "font-size: 14px; text-align: justify; color: #ffffff;",
        
        "After using this app for the first time, you can save",
        "your progress (and any previously defined values) to",
        "your local environment, by accessing the",
        icon("gears", class = "cl-sea"),
        "symbol on the",
        HTML(paste0(span("upper right corner", class = "cl-sea"), "."))
      ),
      
      uiOutput(ns("saving_vals")),
      
      fileInput(ns("restore_state"),
                label = span("Upload saved settings file:",
                     style = paste0(
                       "color: #ffffff;",
                       "font-size: 15px;",
                       "letter-spacing: 0.5px;")),
                accept = ".rds",
                placeholder = ".rds file"),
      
      p(style = "font-size: 14px; text-align: justify; color: #ffffff;",
        
        "Click", span("Browse...", class = "cl-sea"),
        "and select a previously saved",
        span(".rds", class = "cl-sea"),
        "file to load stored values."),
      
      # tags$hr(style = "border-color: #2c3b41;"),
      # h4("Language:"),
      # uiOutput(ns("menu_language")),

      # selectInput(inputId = ns("lang_select"),
      #             label = NULL,
      #             choices = languages,
      #             selected = "EN",
      #             width = "80px",
      #             selectize = TRUE),
      
      tags$hr(style = "border-color: #ffffff;"),
      
      shinyWidgets::prettyCheckbox(
        inputId = ns("parallel"),
        label = span("Parallel mode",
                     style = paste0(
                       "color: #ffffff;",
                       "font-size: 15px;",
                       "letter-spacing: 0.5px;")),
        value = TRUE),
      shinyWidgets::autonumericInput(
        inputId = ns("ncores"),
        label = NULL,
        currencySymbol = " core(s)",
        currencySymbolPlacement = "s",
        decimalPlaces = 0,
        minimumValue = 1,
        maximumValue = 32,
        value = 1, wheelStep = 1),
      
      p(),
      tags$hr(style = "border-color: #ffffff;"),
      
      actionButton(ns("browser"),
                   icon = shiny::icon("screwdriver-wrench"),
                   label = "Browser console",
                   style = "width: 100%"),
      tags$script("$('#browser').hide();"),
      
      p(style = "margin-bottom: 15px;")
      
    ) # end of column
  ) # end of tagList
}

#' comp_settings Server Functions
#'
#' @noRd
mod_comp_settings_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      browser()
    }) %>% bindEvent(input$browser)
    
    observe({
      rv$parallel <- input$parallel
    }) %>% bindEvent(input$parallel)

    # observe({
    # }) %>% bindEvent(input$lang_select)
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    output$text_save <- renderUI({
      
      if (is.null(rv$datList)) {
        shinyjs::disable("download_settings")
        out_text <- paste(
          "Return here after running through the workflow once to",
          "save all stored values to your local environment.")
      } else {
        out_text <- "Save all stored values to your local environment."
        shinyjs::enable("download_settings")
      }
      
      p(style = "font-size: 14px; text-align: justify; color: #ffffff;",
        out_text)
      
    }) # end of output, "text_save"
    
    ## Update number of cores: --------------------------------------------
    
    observe({
      
      num_cores <- parallel::detectCores(logical = FALSE)
      
      shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "ncores",
        label = span("Number of cores:", class = "cl-wht"),
        value = num_cores)
      
    }) %>% # end of observe,
      bindEvent(rv$parallel)
    
    # SETTINGS ------------------------------------------------------------
    ## Save settings/values (for next session): ---------------------------
    
    output$download_settings <- downloadHandler(
      filename = function() {
        paste0("movedesign-settings_", Sys.Date(), ".rds")
      },
      content = function(file) {
        if (is.null(rv$simList)) {
          shiny::showNotification(
            "No data available to save!", type = "error", duration = 8)
        } else { base::saveRDS(reactiveValuesToList(rv), file = file) }
        
      } # end of content
    ) # end of output, "download_settings"
    
    ## Restore settings/values (from previous session): -------------------
    
    observe({
      # validate(need(input$restore_state, message = FALSE))
      # loading_modal("Restoring values")
      
      rv$restored_rv <- readRDS(
        file = paste(input$restore_state$datapath))
      rv$restored <- TRUE
      
      # shiny::showModal(
      #   shiny::modalDialog(
      #     title = "Previous parameters restored!",
      #     
      #     p("Type:", rv$data_type),
      #     p("Number of simulations:", length(rv$simList)),
      #     
      #     footer = tagList(
      #       modalButton("Dismiss")
      #     ),
      #     size = "s"))
      
      # shinybusy::remove_modal_spinner()
      
    }, label = "o-about_restore") %>% # end of observe,
      bindEvent(input$restore_state)

    
  }) # end of moduleServer
}


## To be copied in the UI
# mod_comp_settings_ui("comp_settings_1")

## To be copied in the server
# mod_comp_settings_server("comp_settings_1")
