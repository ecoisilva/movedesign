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
          br() # ,
          # p(style = "max-width: 685px;",
          #   span(class = "notes-block",
          #        style = "text-align: center !important;",
          # 
          #        fontawesome::fa("circle-exclamation", fill = "#dd4b39"),
          #        span("Note:", class = "cl-dgr"),
          #        "This is the", span( "development", class = "cl-dgr"),
          #        "version of the application, currently undergoing",
          #        "testing. Use with caution, as it may crash",
          #        "or behave unexpectedly.")),
          # p(style = "margin-bottom: 35px;")

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
            span("guided tutorial", class = "cl-sea"), "for:"),
          
          mod_comp_tour_ui("tour_1"),
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
            "For more details, check the first published manuscript",
            wrap_none(
              a(href = paste0(
                "https://doi.org/10.1111/2041-210X.14153"), "here",
                target = "_blank", rel = "noopener noreferrer"), ","),
            br(),
            "and the preprint of the second manuscript",
            wrap_none(
              a(href = paste0(
                "https://www.biorxiv.org/content/",
                "10.1101/2025.07.30.667390v2"), "here",
                target = "_blank", rel = "noopener noreferrer"), ".")),
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
                
                div(id = "content_workflow-first",
                shinyWidgets::radioGroupButtons(
                  inputId = ns("which_data"),
                  label = span("Data source:",
                               style = "font-size: 16px;"),
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
                          "sampled groups"))),
                      choiceValues = list("none", "mean", "compare"),
                      selected = character(0),
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-check-square",
                                     style = "color: var(--sea);"),
                        no = tags$i(class = "fa fa-square-o",
                                    style = "color: var(--danger);")),
                      direction = "vertical"))
                ), # end of div

                div(id = "content_workflow-second",
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
                          'number of VHF/GPS tags."'))),
                        tagList(span(em(
                          '"I want to get the',
                          span("recommended", class = "cl-jgl"),
                          'sampling parameters."')))
                        ),
                      choiceValues = list("set_m",
                                          "get_m",
                                          "get_all"),
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
                           inputId = ns("add_ind_var"),
                           label = span(
                             "Add",
                             span("individual", class = "cl-sea"),
                             "variation",
                             style = "font-size: 15px;"),
                           value = FALSE))),
                
                uiOutput(ns("aboutUI_pop_var")), p()
                ) # end of div

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
    
    observe({
      req(!rv$is_font)
      
      # Retrieve all fonts currently available
      all_gfonts <- NA
      all_gfonts <- gfonts::get_all_fonts()

      font_available <- tryCatch({
        gdtools::register_gfont(family = "Roboto Condensed")
      })

      if (inherits(font_available, "error")) {
        rv$is_font <- FALSE
      } else {
        rv$is_font <- TRUE
      }

    }) # end of observe
   
    ## Workflow selections: -----------------------------------------------
    
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
    
    ## Deployment type: ---------------------------------------------------
    
    observe({
      req(input$which_m)
      req(rv$which_meta != "none")
      rv$which_m <- input$which_m
      
    }, label = "o-about_m")
    
    observe({
      req(rv$which_meta)
      
      if (rv$which_meta == "none") {
        
        rv$which_m <- "none"
        
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "which_m",
          selected = character(0))
        
        shinyjs::hide(id = "which_m")
        shinyjs::disable("add_ind_var")
        shinyWidgets::updateAwesomeCheckbox(
          session = session,
          inputId = "add_ind_var",
          value = FALSE)
        
      } else {
        
        rv$which_m <- NULL
        
        shinyjs::show(id = "which_m")
        if (!isTRUE(rv$which_data == "Simulate"))
          shinyjs::enable("add_ind_var")
      }
      
    }, label = "o-about_meta") %>%
      bindEvent(rv$which_meta)
    
    observe({
      rv$overwrite_active <- input$overwrite_active
    }, label = "o-about_overwrite")
    
    observe({
      rv$add_ind_var <- isTRUE(input$add_ind_var)
    }, label = "o-about_ind_var")
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    shinyjs::hide(id = "which_m")
    
    ## Individual variation is unavailable for simulated data: ------------
    
    observe({
      req(rv$which_data)
      
      if (rv$which_data == "Simulate") {
        shinyjs::hide(id = "add_ind_var")
        shinyWidgets::updateAwesomeCheckbox(
          session = session,
          inputId = "add_ind_var",
          value = FALSE)
        
      } else {
        shinyjs::show(id = "add_ind_var")
        
        if (isTRUE(rv$which_meta != "none"))
          shinyjs::enable("add_ind_var")
      }
      
    }) %>% # end of observe,
      bindEvent(rv$which_data)
    
    ## Iterative deployments allow one research target only: --------------
    
    observe({
      req(rv$which_m %in% c("get_m", "get_all"),
          length(rv$which_question) == 2)
      
      txt_goal <- if (rv$which_m == "get_m")
        span("minimum", class = "cl-jgl") else
          span("optimal", class = "cl-jgl")
      
      txt_what <- if (rv$which_m == "get_m")
        "number of VHF/GPS tags" else "sampling parameters"
      
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "which_question",
        selected = character(0))
      
      shinyalert::shinyalert(
        type = "error",
        title = "Warning",
        text = tagList(span(
          "Searching for the", txt_goal, txt_what,
          "is an iterative process.",
          "This option only allows for one",
          span("research question", class = "cl-dgr"),
          "at a time. Please select either 'Home range' or",
          "'Speed & distance' (but not both) to proceed.")),
        confirmButtonText = "Dismiss",
        html = TRUE,
        size = "xs")
      
    }) %>% # end of observe,
      bindEvent(rv$which_m, rv$which_question)
    
    ## A meta-analysis needs a research target: ---------------------------
    
    observe({
      req(input$which_meta)
      req(is.null(input$which_question))
      
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
      
    }) %>% # end of observe,
      bindEvent(input$which_meta)
    
    ## Note on individual variation: --------------------------------------
    
    output$aboutUI_pop_var <- renderUI({
      req(rv$which_question, rv$which_meta, isTRUE(rv$add_ind_var))
      req(rv$which_meta %in% c("mean", "compare"))
      
      ui_txt <- if (length(rv$which_question) > 1) {
        "home range and speed & distance estimation."
      } else {
        paste(switch(rv$which_question,
                     "Home range" = "home range",
                     "Speed & distance" = "speed & distance"),
              "estimation.")
      }
      
      p(style = "max-width: 685px;",
        span(
          class = "notes-block",
          style = "text-align: center !important;",
          
          fontawesome::fa("circle-exclamation", fill = pal$dgr),
          span("Note:", class = "cl-dgr"),
          "Requires careful selection of individuals",
          "to inform subsequent simulations. Ensure all selected",
          "individuals meet the assumptions for ", ui_txt))
      
    }) # end of renderUI, "aboutUI_pop_var"
    
    # SETTINGS ------------------------------------------------------------
    ## Generating seed: ---------------------------------------------------
    
    isolate({
      if (is.null(rv$seed0)) {
        rv$seed0 <- round(stats::runif(1, min = 1, max = 999999), 0)
      }
    })
    
    observe({
      if (isTRUE(input$overwrite_active)) {
        
        msg_log(
          style = "warning",
          message = paste0("Seed is now ", msg_warning("fixed"), "."),
          detail = "Not recommended outside of tutorials.")
        rv$seed0 <- 100
        
      } else {
        rv$seed0 <- round(stats::runif(1, min = 1, max = 999999), 0)
      }
      
    }, label = "o-about_generate_seed") %>%
      bindEvent(input$overwrite_active, ignoreInit = TRUE)
    
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
      
      if (rv$which_meta == "none") {
        rv$which_m <- "none"
        rv$add_ind_var <- FALSE
        
      } else {
        rv$which_m <- rv$restored_rv$which_m
        rv$add_ind_var <- rv$restored_rv$add_ind_var
        
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "which_m",
          selected = rv$which_m)
        
        shinyWidgets::updateAwesomeCheckbox(
          session = session,
          inputId = "add_ind_var",
          value = rv$add_ind_var)
      }
      
    }) %>% # end of observe,
      bindEvent(rv$restored)
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_about_ui("tab_about_1")

## To be copied in the server
# mod_tab_about_server("tab_about_1")
