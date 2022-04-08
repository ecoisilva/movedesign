#' comp_tour UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comp_tour_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("default_tour"),
      label = "Click here for a guided tutorial",
      icon = icon("compass"),
      width = "240px",
      class = "btn-primary")
  )
}

#' comp_tour Server Functions
#'
#' @noRd
mod_comp_tour_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reactive_modal_text <- reactive({
      if(!is.null(vals$akde_sim)) {
        HTML(paste("test"))
      } else {
        HTML(paste(
          "If the",
          span("estimate CIs",
               style =  paste(txt_tour, col_border)),
          "are wide, and the",
          span("effective sample size",
               style = paste(txt_tour, col_border)),
          "is low, then we can adjust the tracking regime to see how",
          "to obtain a more precise estimate.", shiny::p(),

          add_action,
          "Click ",
          fontawesome::fa(name = "wrench", fill = hex_border),
          span("'Adjust regime'",
               style = paste(txt_tour, col_border)),
          "and press 'Next'."
        ))
      }

    })

    # isValue_id <- reactive({
    #   return(is.null(vals$id))
    # })

    # Build tours: ------------------------------------------------------

    add_action <- span("Action:", style = paste(txt_action))

    build_mainTour <- function(ns, vals) {
      element <- intro <- character(0)

      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(

          p(span("Welcome to", style = paste(ttl_tour,
                                             "color: white;")),
            HTML(
              paste0("move",
                     span("design",
                          style = paste(ttl_tour, col_border)))),
            style = ttl_tour),

          "Here, you will be guided step-by-step through the",
          "app. At times, you will be prompted to perform a",
          "specific action, which will be",
          HTML(paste0(span("highlighted", style = txt_action), ".")),
          "Please take care to follow these instructions,",
          "since later parts of the tutorial require that all",
          "previous steps are performed.",
          p(), "If no action is requested, simply proceed",
          "to the next step by",
          span("clicking on the 'Next' button or using the right",
               "arrow of your keyboard.", style = txt_action)
        )))

      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          span("Warning:", style = paste(txt_tour, col_caution)),
          "do not interact with anything outside of higlighted zones",
          "create alongside the tutorial boxes.",
          p(),
          "This tutorial will",
          span("not", style = paste(txt_tour, col_caution)),
          "cover definitions in detail.",
          "If you want to check the more comprehensive",
          fontawesome::fa(name = "question-circle"),
          "help tips, please do so outside of the tutorial."
        )))

      # element <- c(element, "#workflow-content")
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     span(
      #       "Please choose",
      #       fontawesome::fa(name = "check",
      #                       fill = hex_item),
      #       span("a data source",
      #            style = txt_tour_item),
      #       "and your research question(s).",
      #       style = txt_action)
      #   )))

      element <- c(element, "#workflow-content")
      intro <- c(
        intro,
        HTML(paste(
          "In this tutorial, you will be guided through a real",
          "tracking project already available through the",
          span("ctmm", style = txt_tour_grey),
          "R package.",
          "For assistance with a simulation from scratch, exit",
          "this tour (press the ", span("x", style = txt_tour_grey),
          " button above) and click on the",
          fontawesome::fa(name = "file-signature", fill = hex_border),
          span("Simulate data", style = txt_tour_border),
          "tab. A secondary guided tour will be available there.",
          p(),
          span(
            "Please choose",
            fontawesome::fa(name = "check",
                            fill = hex_item),
            span("Select from available species",
                 style = txt_tour_item),
            "and the two research questions:",
            fontawesome::fa(name = "check-square",
                            fill = hex_border),
            "Home range, and",
            fontawesome::fa(name = "check-square",
                            fill = hex_border),
            "Speed & distance.", style = txt_action)
        )))

      element <- c(element, ".sidebar")
      intro <- c(
        intro,
        HTML(paste(
          "The corresponding steps are now highlighted.",
          "First, we go to the",
          fontawesome::fa(name = "file", fill = hex_border),
          span("Select data", style = txt_tour_border), "tab.",
          p(),
          span("Click on the 'Next' button or use the right arrow",
               "of your keyboard to proceed.", style = txt_action)
        )))

      ## Data tabs: -------------------------------------------------------

      tab3 <- paste0("#tab_data_select_1", "-")
      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          "Parameters extracted from the species provided",
          "may help inform animal tracking studies of other",
          "species with similar",
          paste0(span("movement behaviors",
                      style = txt_tour_border), "."),

          span(
            "First, make sure the species currently selected",
            "from the list is the",
            HTML(paste0(
              span("African Buffalo", style = txt_tour_item), ".")),
            "Do not click any other button as of yet.",
            style = txt_action)
        )))

      element <- c(element, "#comp_viz_selected-dataTabs_viz")
      intro <- c(
        intro,
        HTML(paste(
          "These are the data vizualization tabs. The first",
          "tab", fontawesome::fa(name = "paw", fill = hex_border),
          "covers all individuals present in the selected dataset.",
          "The second tab", fontawesome::fa(name = "filter",
                                             fill = hex_border),
          "filters to the chosen individual for",
          "parameter extraction.",
          "The third tab ", fontawesome::fa(name = "chart-line",
                                             fill = hex_border),
          "will show you the", span("variogram",
                                      style = txt_tour_item),
          "for that particular individual."
        )))

      element <- c(element, "#comp_viz_selected-dataTable_all")
      intro <- c(
        intro,
        HTML(paste(
          "You want to pick the individual",
          "most likely to provided accurate parameter",
          "estimates that represent your intended",
          paste0(span("movement behavior",
                      style = txt_tour_border), "."),
          p(),
          "Ideally, this means an individual",
          "with a considerably high number of fixes",
          paste0(span("(n)", style = txt_tour_item),
                 ", long sampling duration ",
                 span("(period)", style = txt_tour_item),
                 ", and/or short time between fixes ",
                 span("(interval)", style = txt_tour_item), " "),
          HTML("&#x2014"), "depending on your research question,",
          "respectively."
        )))

      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            "Now, select an individual from the dropdown menu",
            "and click the", fontawesome::fa(name = "magic"),
            "'Validate' button before proceeding.",
            style = txt_action),
          p(),
          "Outside of this tutorial, you can also select an",
          "individual from the table or",
          "the plot in the data vizualization",
          fontawesome::fa(name = "paw", fill = hex_border),
          " tab."
        )))

      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          "The validation step ensures that certain datasets (with",
          "short sampling durations or low number of fixes) are not",
          "used, as the uncertainty associated with any",
          "parameters will be too high.",
          p(),
          "If the button changes to",
          fontawesome::fa(name = "check-circle"),
          "'Validated!', and",
          "If a", span("'Success!'", style = txt_output),
          "message appears below,",

          span(
            "then you can proceed by clicking the",
            fontawesome::fa(name = "paper-plane"),
            "'Extract' button.",
            style = txt_action)
        )))

      element <- c(element, paste0(tab3, "selectBox_regime"))
      intro <- c(
        intro,
        HTML(paste(
          "This is the current tracking regime, extracted from",
          "the original dataset. You will be able to test out",
          "a different", span("tracking regime",
                              style = txt_tour_item),
          "later on."
        )))

      element <- c(element, paste0(tab3, "selectUI_parameters"))
      intro <- c(
        intro,
        HTML(paste(
          "All relevant temporal",
          paste0("(", span("e.g.", style = txt_tour_italic),
                 ", ", span("position", style = txt_tour_border),
                 " and ", span("velocity autocorrelation ",
                               style = txt_tour_border),
                 "timescale)"), "and spatial parameters",
          paste0("(", span("semi-variance", style = txt_tour_border),
                 ")"), "will show up here.",
          "All subsequent steps will built upon these",
          "parameters."
        )))

      element <- c(element, paste0(tab3, "selectBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, you can see the",
          span("sample sizes", style = txt_tour_item),
          "of the original dataset.",
          span("Absolute sample size (n)", style = txt_tour_border),
          "corresponds to the total number of observations in a",
          "dataset. The more important information comes from the",
          span("effective sample sizes (N)", style = txt_tour_border),
          "for", span("home range", style = txt_tour_border), "and",
          span("speed & distance", style = txt_tour_border),
          "estimation.",
          "The", span("effective sample size",
                      style = txt_tour_border),
          "is equal to number of home range crossings that",
          "occurred during the study period."
        )))

      ## Device tab: ------------------------------------------------------

      element <- c(element, "#group_device")
      intro <- c(
        intro,
        HTML(paste(
          span("Now we are ready to evaluate the intended",
               span("tracking regime", style = txt_tour_item),
               "for a future tracking project.",
               style = paste(txt_tour,
                             "text-align: center !important;"))
        )))

      tab4 <- paste0("#tab_device_1", "-")
      element <- c(element, paste0(tab4, "reg_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "In this section, we will simulate a new dataset",
          "conditioned upon the previous parameters,",
          "while selecting a custom tracking regime.",
          p(),
          span(
            "First, choose GPS as your tracking device.",
            style = txt_action)
        )))

      element <- c(element, paste0(tab4, "regBox_device"))
      intro <- c(
        intro,
        HTML(paste(
          "With", HTML(paste0(
            span("GPS & satellite loggers", style = txt_tour_item), ",")),
          "sampling is", span("automated", style = txt_tour_border),
          "and conducted by satellite systems. Therefore,",
          span("sampling frequency", style = txt_tour_caution),
          "and", span("sampling duration", style = txt_tour_caution),
          "are both inherently linked with",
          HTML(paste0(span("GPS battery life",
                           style = txt_tour_border), ".")),
          "This tradeoff restricts the volume of data that",
          "can be collected by GPS & satellite loggers."
        )))

      element <- c(element, paste0(tab4, "regBox_device"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, we can set the maximum",
          span("GPS battery life", style = txt_tour_item),
          "(how long the GPS is expected to last),",
          "the maximum", span("GPS fix rate", style = txt_tour_item),
          "(the longest time interval between fixes available",
          "at the duration above), and the",
          span("decay rate", style = txt_tour_item),
          "(how fast the GPS battery is expected to decay by increasing",
          "the sampling interval).",

          ""

        )))


      ## Analyses tab: ----------------------------------------------------
      ### Home range ------------------------------------------------------

      element <- c(element, "#group_design")
        intro <- c(
          intro,
          HTML(paste(
            "Now we are ready to move on to either the",
            "estimation of",
            span("home range", style = txt_tour_item),
            "and", HTML(paste0(span("distance/speed traveled",
                                    style = txt_tour_item), ".")),
            "This tutorial will run through one and then the other."
          )))

        # element <- c(element, paste0(tab1, "dataBox_regime"))
        # intro <- c(
        #   intro,
        #   HTML(paste(
        #     "regime box, click adjust", br(),
        #
        #     "To simulate new locations",
        #     "conditioned upon the species' movement model",
        #     "and data, but with a different sampling rate."
        #   )))
        #
        # # Help for HR tab:
        #
        # tab5 <- paste0("#tab_hrange_1", "-")
        #
        # element <- c(element, paste0(tab5, "hr_intro"))
        # intro <- c(
        #   intro,
        #   HTML(paste(
        #     p(span("Lets estimate", style = paste(ttl_tour,
        #                                           "color: white;")),
        #       span("home range",
        #            style = paste(ttl_tour, col_border)),
        #       style = ttl_tour),
        #
        #     add_action,
        #     "'Run estimation' button to run the AKDE function",
        #     "in the background."
        #   )))
        #
        # element <- c(element, paste0(tab5, "hrBox_areas"))
        # intro <- c(
        #   intro,
        #   HTML(paste(
        #     "First, you can see two sets of values:", br(),
        #     fontawesome::fa(name = "bullseye", fill = "#bdbdbd"),
        #     paste0(span("Estimate", style = paste(txt_tour,
        #                                           "color: #bdbdbd;")),
        #            ", which is the ",
        #            span("home range area point estimate",
        #                 style =  paste(txt_tour, col_border)),
        #            ", followed by the 95% confidence intervals (",
        #            span("min — max",
        #                 style =  paste(txt_tour, col_border)),
        #            ") as the point estimate is subject ",
        #            "to uncertainty."), br(),
        #     fontawesome::fa(name = "exclamation-circle",
        #                     fill = "#bdbdbd"),
        #     paste0(span("Expected error",
        #                 style = paste(txt_tour, "color: #bdbdbd;")),
        #            ", which is the ",
        #            span("relative error (in %) ",
        #                 style =  paste(txt_tour, col_caution)),
        #            "of the point estimate (and of the 95% CIs; ",
        #            "once again, ",
        #            span("min — max",
        #                 style =  paste(txt_tour, col_caution)),
        #            ") in relation to ",
        #            "the expected value (truth) derived ",
        #            "from the distribution.")
        #   )))
        #
        # element <- c(element, paste0(tab5, "hrBox_areas"))
        # intro <- c(
        #   intro,
        #   HTML(paste(
        #     "You want the",
        #     span("expected error",
        #          style = paste(txt_tour, "color: #bdbdbd;")),
        #     "to be low, and the CIs to 'hug' the point estimate",
        #     "as much as possible. If not, the point estimate is highly",
        #     "uncertain, and the dataset may require a longer",
        #     paste0(span("sampling duration",
        #                 style = paste(txt_tour, col_caution)), ".")
        #   )))
        #
        #
        # element <- c(element, paste0(tab5, "hrBox_sizes"))
        # intro <- c(
        #   intro,
        #   HTML(paste(
        #     "It is also important to consider the sample sizes.",
        #     "You want the",
        #     span("effective sample size",
        #          style = paste(txt_tour, col_border)),
        #     "to be as high as possible, reducing uncertainty."
        #   )))
        #
        # element <- c(element, paste0(tab5, "hrBox_regime"))
        # intro <- c(
        #   intro,
        #   reactive_modal_text()
        # )
        #
        # element <- c(element, "#Tour_tmp")
        # intro <- c(
        #   intro,
        #   HTML(paste(
        #     "[work in progress]"
        #   )))

        # ...in progress...

      element <- c(element, "#Tour_end")
      intro <- c(
        intro,
        HTML(paste(
          p(span("Thank you for using",
                 style = paste(ttl_tour, "color: white;")),
            HTML(
              paste0("move",
                     span("design",
                          style = paste(ttl_tour, col_border)),
                     "!")),
            style = ttl_tour)
        )))

      data.frame(element = element,
                 intro = intro,
                 stringsAsFactors = FALSE)

    } # end of main tour

    # -------------------------------------------------------------------

    # Allow dynamic UI actions based on tour being active:

    observe({
      req(vals$active_tab)
      if(vals$active_tab == 'about') vals$tour_active <- FALSE
    })

    # If 'guided tour' button is clicked:

    observe({
      vals$tour_active <- TRUE

      tour <- build_mainTour(ns, vals)
      rintrojs::introjs(
        session = session,
        options = list(
          steps = tour,
          showStepNumbers = F,
          showButtons = T,
          showBullets = T,
          scrollToElement = T
        ),
        events = list(onbeforechange =
                        rintrojs::readCallback('switchTabs')))

    }) %>% # observe event, bound to:
      bindEvent(input$default_tour)

  }) # end of moduleServer
}

## To be copied in the UI
# mod_comp_tour_ui("comp_tour_1")

## To be copied in the server
# mod_comp_tour_server("comp_tour_1")
