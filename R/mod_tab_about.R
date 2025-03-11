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
          
          p(style = paste("max-width: 685px;",
                          "text-align: center;",
                          "margin-top: 10px;",
                          "margin-bottom: 0px;"),
            "Click below for a", 
            span("guided tutorial", class = "cl-sea"), "with:"),
          
          mod_comp_tour_ui("tour_1"), # TODO WIP
          shinyWidgets::awesomeCheckbox(
            inputId = ns("overwrite_active"),
            label = span(
              "Use fixed", span("seed", class = "cl-sea"),
              "for tutorials only"),
            value = FALSE),
          br(),
          
          p(style = paste("max-width: 685px;",
                          "text-align: center;",
                          "margin-top: 0px;",
                          "margin-bottom: 10px;"),
            "For more details, check the first manuscript",
            wrap_none(
              a(href = paste0("https://besjournals.onlinelibrary.wiley.com/",
                              "doi/10.1111/2041-210X.14153"), "here"), ".")),
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
                  label = span("Research target:",
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

                div(class = "btn-nobg",
                    shinyWidgets::radioGroupButtons(
                      inputId = ns("which_meta"),
                      label = span("Analytical target:",
                                   style = "font-size: 16px;"),
                      choiceNames = c(
                        tagList(span(
                          span("Individual",
                               class = "cl-sea"), "estimate")),
                        tagList(span(
                          "Mean estimate of",
                          span("sampled population",
                               class = "cl-sea"))),
                        tagList(span(
                          "Compare estimates of",
                          span("two", class = "cl-sea"),
                          "sampled populations"))),
                      choiceValues = list("none", "mean", "compare"),
                      selected = character(0),
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-check-square",
                                     style = "color: var(--sea);"),
                        no = tags$i(class = "fa fa-square-o",
                                    style = "color: var(--danger);")),
                      direction = "vertical")),

                div(class = "btn-nobg",
                    shinyWidgets::radioGroupButtons(
                      inputId = ns("which_m"),
                      label = span("Deployment:",
                                   style = "font-size: 16px;"),
                      choiceNames = c(
                        tagList(span(em(
                          '"I plan to deploy a',
                          span("set", class = "cl-jgl"),
                          'number of VHF/GPS tags."'))),
                        tagList(span(em(
                          '"I want to determine the',
                          span("minimum", class = "cl-jgl"),
                          'number of VHF/GPS tags."')))),
                      choiceValues = list("set_m", "get_m"),
                      selected = character(0),
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-check-square",
                                     style = "color: var(--jungle);"),
                        no = tags$i(class = "fa fa-square-o",
                                    style = "color: var(--danger);")),
                      direction = "vertical")),

                p(style = "margin-top: 10px;"),
                fluidRow(
                  column(width = 12, align = "center",
                         shinyWidgets::awesomeCheckbox(
                           inputId = ns("is_emulate"),
                           label = span(
                             "Add",
                             span("individual", class = "cl-sea"),
                             "variation",
                             style = "font-size: 15px;"),
                           value = FALSE))),
                
                uiOutput(ns("aboutUI_pop_var"))

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
        
        div(style = "text-align: center;", 
            img(src = "www/logo_casus.png", height = "70px"),
            p(),
            a(href = "https://www.casus.science/",
              "https://www.casus.science/"), 
            p())

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
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    # shinyjs::hide(id = "is_emulate")
    
    observe({
      font_available <- tryCatch({
        gdtools::register_gfont(family = "Roboto Condensed")
      })
      
      if (inherits(font_available, "error")) {
        rv$is_font <- FALSE
      } else {
        rv$is_font <- TRUE
      }

    }) # end of observe
   
    observe({
      rv$which_data <- input$which_data
      rv$which_question <- input$which_question
      rv$which_meta <- input$which_meta
      
      target_map <- c("Home range" = "hr",
                      "Speed & distance" = "ctsd")
      set_target <- target_map[rv$which_question]
      names(set_target) <- set_target
      rv$set_target <- set_target
      
    }, label = "o-about_workflow")
    
    observe({
      rv$which_m <- input$which_m
      if (req(rv$which_meta) == "none") rv$which_m <- NULL
      if (req(rv$which_meta) == "none") shinyjs::disable("is_emulate")
      else shinyjs::enable("is_emulate")
      
    }, label = "o-about_m")
    
    observe({
      rv$overwrite_active <- input$overwrite_active
    }, label = "o-about_overwrite")
    
    observe({
      rv$is_emulate <- input$is_emulate
    }, label = "o-about_emulate")
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    shinyjs::hide(id = "which_m")
    shinyjs::hide(id = "num_tags")
    shinyjs::hide(id = "num_tags_max")
    
    observe({
      if (rv$which_meta == "none") {
        shinyjs::hide(id = "which_m")
        shinyWidgets::updateAwesomeCheckbox(
          session = session,
          inputId = "is_emulate",
          value = FALSE)
      } else shinyjs::show(id = "which_m")
      
    }) %>% # end of observe,
      bindEvent(rv$which_meta)
    
    observe({
      req(rv$which_m == "get_m",
          length(rv$which_question) == 2)
      
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "which_question",
        selected = character(0))
      
      shinyalert::shinyalert(
        type = "error",
        title = "Warning",
        text = tagList(span(
          "Searching for the", span("minimum", class = "cl-jgl"),
          "number of VHF/GPS tags is an iterative process.",
          "Currently, this option only allows for one",
          span("research question", class = "cl-dgr"),
          "at a time. Please select either 'Home range' or",
          "'Speed & distance' (but not both) to proceed.")),
        confirmButtonText = "Dismiss",
        html = TRUE,
        size = "xs")
      
    }) # end of observe
    
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
            ")."))),
          html = TRUE,
          size = "xs")
      }
      
    }) # end of observe
    
    observe({
      req(rv$which_data)
      
      if (rv$which_data == "Simulate") {
        shinyjs::hide(id = "is_emulate")
        shinyWidgets::updateAwesomeCheckbox(
          session = session,
          inputId = "is_emulate",
          value = FALSE)
        
      } else {
        shinyjs::show(id = "is_emulate")
      }
      

    }) # end of observe
    
    output$aboutUI_pop_var <- renderUI({
      req(rv$which_question, rv$which_meta, rv$is_emulate == TRUE)
      
      ui <- ui_txt <- NULL
      
      if (length(rv$which_question) > 1) {
        ui_txt <- "home range and speed & distance estimation."
      } else {
        ui_txt <- paste(
          switch(
            rv$which_question,
            "Home range" = { "home range" },
            "Speed & distance" = { "speed & distance" }),
          "estimation.")
      }
      
      if (rv$which_meta == "mean" || rv$which_meta == "compare") {
        ui <- tagList(
          p(style = "max-width: 685px;",
            span(
              class = "help-block",
              style = "text-align: center !important;",
              
              fontawesome::fa("circle-exclamation", fill = pal$dgr),
              span("Note:", class = "help-block-note"),
              "Requires careful selection of which individuals",
              "to inform subsequent simulations. All selected",
              "individuals must fit the assumptions for ", ui_txt)))
      }
      return(ui)
      
    }) # end of renderUI, "aboutUI_pop_var"
    
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
    
    ## If settings are restored: ------------------------------------------
    
    observe({
      req(rv$restored)
      loading_modal("Restoring values")
      
      rv$which_data <- rv$restored_rv$which_data
      rv$which_question <- rv$restored_rv$which_question
      rv$which_meta <- rv$restored_rv$which_meta
      
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "which_data",
        selected = rv$which_data)
      
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "which_question",
        selected = rv$which_question)
      
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "which_meta",
        selected = rv$which_meta)
      
      shinybusy::remove_modal_spinner()
      req(rv$which_meta)
      
      if (rv$which_meta != "none") {
        rv$which_m <- rv$restored_rv$which_m
        rv$is_emulate <- rv$restored_rv$is_emulate
        
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "which_m",
          selected = rv$which_m)
        
        shinyWidgets::updateAwesomeCheckbox(
          session = session,
          inputId = "is_emulate",
          value = rv$is_emulate)
      }
      
    }) %>% # end of observe,
      bindEvent(rv$restored)
    
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_about_ui("tab_about_1")

## To be copied in the server
# mod_tab_about_server("tab_about_1")
