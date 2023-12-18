#' comp_settings UI Function
#'
#' @description Settings component.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comp_settings_ui <- function(id){
  ns <- NS(id)
  tagList(

    column(
      width = 12, align = "center",
      p(),
      actionButton(ns("browser"),
                   icon = shiny::icon("screwdriver-wrench"),
                   label = "Browser console",
                   style = "width: 100%"),
      tags$script("$('#browser').hide();"),
      
      p(),
      shiny::downloadButton(
        outputId = ns("download_settings"),
        label = "Save settings",
        icon = shiny::icon("download"),
        style = "width: 100%"),
      p(),
      uiOutput(ns("text_save")),
      
      # tags$hr(style = "border-color: #2c3b41;"),
      # h4("Language:"),
      # uiOutput(ns("menu_language")),

      # selectInput(inputId = ns("lang_select"),
      #             label = NULL,
      #             choices = languages,
      #             selected = "EN",
      #             width = "80px",
      #             selectize = TRUE),

      tags$hr(style = "border-color: #2c3b41;"),
      shinyWidgets::prettyCheckbox(
        inputId = ns("parallel"),
        label = span("Parallel mode",
                     style = paste0(
                       "color: #ffffff;",
                       "font-size: 15px;",
                       "letter-spacing: 0.5px;")),
        value = TRUE),
      br(),
      shinyWidgets::autonumericInput(
        inputId = ns("ncores"),
        label = "Number of cores:",
        currencySymbol = " core(s)",
        currencySymbolPlacement = "s",
        decimalPlaces = 0,
        minimumValue = 1,
        maximumValue = 32,
        value = 1, wheelStep = 1),
      br()
      
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
      
      out_text <- "Save all stored values to your local environment."
      if (is.null(rv$dataList)) {
        out_text <- paste(
          "Return here after running any analyses to save",
          "all stored values to your local environment.")
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
            "No data to save", type = "error", duration = 8)
        } else { saveRDS(reactiveValuesToList(rv), file = file) }
        
      } # end of content
    ) # end of output, "download_settings"

  }) # end of moduleServer
}


## To be copied in the UI
# mod_comp_settings_ui("comp_settings_1")

## To be copied in the server
# mod_comp_settings_server("comp_settings_1")
