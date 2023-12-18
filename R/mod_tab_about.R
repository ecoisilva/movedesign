#' tab_about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_tab_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(

      # Introduction: -----------------------------------------------------

      shinydashboardPlus::box(
        title = NULL,
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = FALSE, closable = FALSE,

        column(
          align = "center", width = 12,
          style = "padding: 0 20px 0 20px;",
          
          img(src = "www/logo.png", height = "140px"), p(),
          
          p(style = "max-width: 685px;",

            "This", a(href = 'http://shiny.rstudio.com', 'Shiny'),
            "application will assist researchers in designing",
            span("movement ecology", class = "cl-grn"),
            "studies related to two",
            "main research questions: the estimation of",
            span("home ranges", class = "cl-sea-d"), "and of",
            wrap_none(span("speed and distance traveled",
                             class = "cl-sea-d"), ".")),
          br(),
          p(style = "max-width: 685px;",
            span(class = "help-block",
                 style = "text-align: center !important;",
                 
                 fontawesome::fa("circle-exclamation", fill = "#dd4b39"),
                 span("Note:", class = "help-block-note"),
                 "This is the", span( "development", class = "cl-dgr"),
                 "version of the application, currently undergoing",
                 "testing. Use with caution, as it may crash",
                 "or behave unexpectedly.")),
          p(style = "margin-bottom: 35px;")

          ) # end of column (text)
      ), # end of box // intro

      # Tour/tutorial section: --------------------------------------------

      # Section currently in progress:
      shinydashboardPlus::box(
        id = "about_tour",
        title = NULL,
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = FALSE, closable = FALSE,

        column(
          align = "center", width = 12,

          br(),
          h2("How does this",
             span("application", class = "cl-sea"), "work?"),
          p(),
          
          mod_comp_tour_ui("tour_1"),
          
          p(), shinyWidgets::awesomeCheckbox(
            inputId = ns("overwrite_active"),
            label = span(
              "Use fixed", span("seed", class = "cl-sea"),
              "for tutorials only"),
            value = FALSE),
          p()
          
        ) # end of column (text)
      ), # end of box // tour

      # Workflows section: ------------------------------------------------

      shinydashboardPlus::box(
        id = "about_workflow",
        title = NULL,
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = FALSE, closable = FALSE,

        column(
          align = "center", width = 12,

          fluidRow(
            align = "center",
            div(id = "content-workflow",

                br(),
                h2("What is your",
                   wrap_none(span("workflow",
                                    class = "cl-sea"), "?")),
                p(style = "padding: none;"),

                ### Data source -------------------------------------------

                shinyWidgets::radioGroupButtons(
                  inputId = ns("which_data"),
                  label = span("Data source:", style = "font-size: 16px;"),
                  choices = c("Upload" = "Upload",
                              "Select" = "Select",
                              "Simulate" = "Simulate"),
                  selected = character(0),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square",
                                 style = "color: var(--sea);"),
                    no = tags$i(class = "fa fa-square-o",
                                style = "color: var(--danger);")),
                  individual = TRUE),

                ### Research question(s) ----------------------------------

                shinyWidgets::checkboxGroupButtons(
                  inputId = ns("which_question"),
                  label = span("Research questions:",
                               style = "font-size: 16px;"),
                  choices = c("Home range",
                              "Speed & distance"),
                  selected = character(0),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square",
                                 style = "color: var(--sea);"),
                    no = tags$i(class = "fa fa-square-o",
                                style = "color: var(--danger);")),
                  individual = TRUE),
                
                # div(class = "btn-nobg",
                # shinyWidgets::radioGroupButtons(
                #   inputId = ns("which_m"),
                #   label = span("Devices deployed:",
                #                style = "font-size: 16px;"),
                #   choiceNames = c(
                #     tagList(span(em(
                #       '"I have a specific number in mind."'))), 
                #     tagList(span(em(
                #       '"I want to determine the',
                #       span("optimal", class = "cl-sea"),
                #       'number of devices."')))),
                #   choiceValues = list("set_m", "get_m"),
                #   selected = character(0),
                #   checkIcon = list(
                #     yes = tags$i(class = "fa fa-check-square",
                #                  style = "color: var(--sea);"),
                #     no = tags$i(class = "fa fa-square-o",
                #                 style = "color: var(--danger);")),
                #   direction = "vertical")),
                # 
                # div(class = "btn-nobg",
                # shinyWidgets::radioGroupButtons(
                #   inputId = ns("which_meta"),
                #   label = span("Target:",
                #                style = "font-size: 16px;"),
                #   choiceNames = c(
                #     tagList(span(
                #       "Mean of",
                #       span("sampled population", 
                #            class = "cl-sea"))), 
                #     tagList(span(
                #       "Compare", span("two", class = "cl-sea"),
                #       "sampled populations"))),
                #   choiceValues = list("mean", "compare"),
                #   selected = character(0),
                #   checkIcon = list(
                #     yes = tags$i(class = "fa fa-check-square",
                #                  style = "color: var(--sea);"),
                #     no = tags$i(class = "fa fa-square-o",
                #                 style = "color: var(--danger);")),
                #   direction = "vertical"))
                
            ) # end of div
          ) # end of fluidRow

        ) # end of column
      ), # end of box // tour

      # Miscellaneous: ----------------------------------------------------

      shinydashboardPlus::box(
        id = "about_misc",
        title = NULL,
        width = 12,
        solidHeader = FALSE, headerBorder = FALSE,
        collapsible = FALSE, closable = FALSE,

        column(
          align = "center", width = 6,
          style = "border-right: 1px solid #ececec;",
          
          ## Restore application settings: --------------------------------

          h5("Restore settings:"),

          p("After using this app for the first time, you can save",
            "your progress (and any previously defined values) to",
            "your local environment, by accessing the",
            icon("gears", class = "cl-mdn"),
            "symbol on the",
            HTML(paste0(span("upper right corner", class = "cl-blk"), "."))
          ),

          uiOutput(ns("saving_vals")),

          fileInput(ns("restore_state"),
                    label = "Upload file:",
                    accept = ".rds",
                    placeholder = ".rds file"),

          p("Click", span("Browse...", class = "cl-blk"),
            "and select a previously saved",
            span(".rds", class = "cl-sea"),
            "file to load stored values.")

        ), # end of column (right)

        ## Contact details: ---------------------------------------------

        column(
          align = "center", width = 6,

          h5("Project contact:"),
          p(style = "text-align: center;",
            "In\u00EAs Silva,",
            fontawesome::fa("envelope", fill = "var(--sea)"),
            span("i.simoes-silva\u0040hzdr.de", class = "cl-sea")),
          tags$hr(),

          h5("Institutional contact:"),
          p(style = "font-size: 14px; text-align: center;",
              "Center for Advanced Systems Understanding (CASUS),",
              "Helmholtz-Zentrum Dresden-Rossendorf e.V. (HZDR),",
              "Untermarkt 20, 02826, G\u00F6rlitz \u2014 Germany"),
          p(),
          img(src = "www/logo_casus.png", height = "70px"),
          p(),
          a(href = "https://www.casus.science/",
            "https://www.casus.science/"), 
          p()

        ) # end of column (right)
      ) # end of box

    ) # end of fluidRow
  ) # end to tagList
}

#' tab_about Server Functions
#'
#' @noRd
mod_tab_about_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    observe({
      rv$which_data <- input$which_data
      rv$which_question <- input$which_question
    }, label = "o-about_questions")
    
    observe({
      rv$which_meta <- input$which_meta
    }, label = "o-about_meta")
    
    
    observe({
      rv$overwrite_active <- input$overwrite_active
    }, label = "o-about_overwrite")
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    # observe({
    #   shinyWidgets::updateCheckboxGroupButtons(
    #     session = session,
    #     inputId = "which_question",
    #     selected = rv$which_question)
    # }, label = "o-about_update")
    
    observe({
      req(input$which_meta)
      
      if (!is.null(input$which_meta) &&
          is.null(input$which_question)) {
        
        shinyalert::shinyalert(
          type = "error",
          title = "Missing estimate",
          text = tagList(span(
            "Meta-analyses requires you to pick a",
            "target estimate (e.g.,",
            wrap_none(span("home range", class = "cl-dgr"), ", ",
                      span("speed and distance", class = "cl-dgr"),
            ").")
          )),
          html = TRUE,
          size = "xs")
      }
    }) # end of observe
    
    
    # SETTINGS ------------------------------------------------------------
    ## Generating seed: ---------------------------------------------------
    
    observe({
      req(rv$active_tab == 'about')
      
      if (input$overwrite_active) {
        req(input$overwrite_active)
        
        msg_log(
          style = "warning",
          message = paste0("Seed is now ", msg_warning("fixed"), "."),
          detail = "Not recommended outside of tutorials.")
        rv$seed0 <- 100
        
      } else {
        seed <- round(stats::runif(1, min = 1, max = 999999), 0)
        rv$seed0 <- seed
      }
      
    }, label = "o-about_generate_seed") # end of observe
    
    ## Restore settings/values (from previous session): -------------------
    
    observe({

      validate(need(input$restore_state, message = FALSE))
      restored_vals <- readRDS(
        file = paste(input$restore_state$datapath))
      rv$restored_vals <- restored_vals

      shiny::showModal(
        shiny::modalDialog(
          title = "Previous parameters restored!",

          p("Data status:"),
          p("Type:", rv$restored_vals$"data_type"),

          p("Parameters:"),
          p("Seed:", rv$restored_vals$"seed0"),

          footer = tagList(
            modalButton("Dismiss")
          ),
          size = "s"))

    }, label = "o-about_restore") %>% # end of observe,
      bindEvent(input$restore_state)

  })
}

## To be copied in the UI
# mod_tab_about_ui("tab_about_1")

## To be copied in the server
# mod_tab_about_server("tab_about_1")
