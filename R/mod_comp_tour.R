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
      width = "238px",
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
          span("confidence intervals", class = "cl-grn"),
          "are wide, and the",
          span("effective sample size", class = "cl-sea"),
          "is low, then we can adjust the tracking regime to see how",
          "to obtain a more precise estimate.", shiny::p(),

          add_action,
          "Click ",
          icon("wrench", class = "cl-sea"),
          span("'Adjust regime'", class = "cl-sea"),
          "and click 'Next'."
        ))
      }

    })

    # isValue_id <- reactive({
    #   return(is.null(vals$id))
    # })

    # Build tours: ------------------------------------------------------

    add_action <- span("Action:", class = "tour-action")

    build_mainTour <- function(ns, vals) {
      element <- intro <- character(0)

      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(

          p(class = "ttl-tour cl-wht",

            "Welcome to",
            wrap_none("move", span("design", class = "cl-sea"))),

          "Here, you will be guided step-by-step through the",
          "app. At times, you will be prompted to perform a",
          "specific action, which will be",
          HTML(paste0(span("highlighted", class = "txt_action"), ".")),
          "Please take care to follow these instructions,",
          "since later parts of the tutorial require that all",
          "previous steps are performed.",
          p(), "If no action is requested, simply proceed",
          "to the next step by",
          span("clicking on the 'Next' button or using the right",
               "arrow of your keyboard.", class = "tour_action")
        )))

      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          span("Warning:", class = "cl-dgr"),
          "do not interact with anything outside of highlighted zones",
          "create alongside the tutorial boxes.",
          p(),
          "This tutorial will",
          span("not", class = "cl-dgr"),
          "cover definitions in detail.",
          "If you want to check the more comprehensive",
          icon("circle-question"),
          "help tips, please do so outside of the tutorial."
        )))

      # element <- c(element, "#workflow-content")
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     span(
      #       "Please choose",
      #       icon(name = "check",
      #            class = "cl-sea-d"),
      #       span("a data source",
      #            class = "cl-sea-d"),
      #       "and your research question(s).",
      #       class = "tour_action")
      #   )))

      element <- c(element, "#workflow-content")
      intro <- c(
        intro,
        HTML(paste(
          "In this tutorial, you will be guided through a real",
          "tracking project already available through the",
          span("ctmm", class = "cl-grn"),
          "R package.",
          "For assistance with a simulation from scratch, exit",
          "this tour (click the ", span("x", class = "cl-grey"),
          " button above) and click on the",
          icon("file-signature", class = "cl-sea"),
          span("Simulate data", class = "cl-sea"),
          "tab. A secondary guided tour will be available there.",
          p(),
          span(
            class = "tour_action",

            "Please choose",
            icon(name = "check", class = "cl-sea-d"),
            span("Select from available species", class = "cl-sea-d"),
            "and the two research questions:",
            icon(name = "check-square", class = "cl-sea-d"),
            HTML(paste0(span("Home range", class = "cl-sea-d"), ",")),
            "and",
            icon(name = "check-square", class = "cl-sea-d"),
            HTML(paste0(span("Speed & distance", class = "cl-sea-d"),
                        ".")))
        )))

      element <- c(element, ".sidebar")
      intro <- c(
        intro,
        HTML(paste(
          "The corresponding steps are now marked in bold.",
          "First, we will go to the",
          icon(name = "file", class = "cl-sea"),
          span("Select data", class = "cl-sea"), "tab",
          HTML("&mdash;"), "to load a dataset from the",
          span("ctmm", class = "cl-grn"), "R package",
          HTML("&mdash;"), "then the",
          icon(name = "stopwatch", class = "cl-sea"),
          span("Tracking regime", class = "cl-sea"), "tab",
          HTML("&mdash;"),
          "to set the sampling duration and interval for evaluation",
          HTML("&mdash;"), "and finally the",
          icon(name = "compass-drafting", class = "cl-sea"),
          span("Analyses", class = "cl-sea"), "tabs."
        )))

      ## Data tabs: -------------------------------------------------------

      tab3 <- paste0("#tab_data_select_1", "-")
      element <- c(element, paste0(tab3, "select_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, you can select one of the seven species",
          "available within the", a(href = mainlink_ctmm, 'ctmm'),
          "package and extract their parameters."
        )))



      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          "Parameters extracted from one of the species provided",
          "may help inform animal tracking studies of other",
          "species with similar",
          paste0(span("movement behaviors",
                      class = "cl-sea"), "."),

          span(
            "First, make sure the species currently selected",
            "from the list is the",
            HTML(paste0(
              span("African Buffalo", class = "cl-sea-d"), ".")),
            "Do not click any other button as of yet.",
            class = "tour_action")
        )))

      element <- c(element, "#comp_viz_selected-dataTabs_viz")
      intro <- c(
        intro,
        HTML(paste(
          "These are the data vizualization tabs. The first",
          "tab", icon("paw", class = "cl-sea"),
          "covers all individuals present in the selected dataset.",
          "The second tab", icon("filter", class = "cl-sea"),
          "filters to the chosen individual for",
          "parameter extraction.",
          "The third tab ", icon("chart-line", class = "cl-sea"),
          "will show you the", span("variogram",
                                      class = "cl-sea-d"),
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
                      class = "cl-sea"), "."),
          p(),
          "Ideally, this means an individual",
          "with a considerably high number of fixes",
          paste0(span("(n)", class = "cl-sea-d"),
                 ", long sampling duration ",
                 span("(period)", class = "cl-sea-d"),
                 ", and/or short time between fixes ",
                 span("(interval)", class = "cl-sea-d"), " "),
          HTML("&#x2014"), "depending on your research question,",
          "respectively."
        )))

      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            "Now, select an individual from the dropdown menu",
            "and click the", icon("wand-magic-sparkles"),
            "'Validate' button before proceeding.",
            class = "tour_action"),
          p(),
          "Outside of this tutorial, you can also select an",
          "individual from the table or",
          "the plot in the data vizualization",
          icon("paw", class = "cl-sea"),
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
          icon(name = "circle-check"),
          "'Validated!',",
          span(
            "then you can proceed by clicking the",
            icon("paper-plane"), "'Extract' button.",
            class = "tour_action")
        )))

      element <- c(element, paste0(tab3, "selectBox_regime"))
      intro <- c(
        intro,
        HTML(paste(
          "This is the current tracking regime, extracted from",
          "the original dataset. You will also be able to test out",
          "different", span("tracking regimes",
                              class = "cl-sea-d"),
          "later on."
        )))

      element <- c(element, paste0(tab3, "selectUI_parameters"))
      intro <- c(
        intro,
        HTML(paste(
          "All relevant temporal",
          paste0("(", span("e.g.", style = "font-style: italic;"),
                 ", ", span("position", class = "cl-sea"),
                 " and ", span("velocity autocorrelation ",
                               class = "cl-sea"),
                 "timescale)"), "and spatial parameters",
          paste0("(", span("semi-variance", class = "cl-sea"),
                 ")"), "will show up here.",
          "All subsequent steps will built upon these",
          "parameters."
        )))

      element <- c(element, paste0(tab3, "selectBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, you can see the",
          span("sample sizes", class = "cl-sea-d"),
          "of the original dataset.",
          span("Absolute sample size (n)", class = "cl-sea"),
          "corresponds to the total number of observations in a",
          "dataset. The more important information comes from the",
          span("effective sample sizes (N)", class = "cl-sea"),
          "for", span("home range", class = "cl-sea"), "and",
          span("speed & distance", class = "cl-sea"),
          "estimation.",
          "The", span("effective sample size",
                      class = "cl-sea"),
          "is equal to number of home range crossings that",
          "occurred during the study period."
        )))

      ## Device tab: ------------------------------------------------------

      element <- c(element, "#group_device")
      intro <- c(
        intro,
        HTML(paste(
          span("Now we are ready to evaluate the intended",
               span("tracking regime", class = "cl-sea-d"),
               "for a future tracking project.",
               style = "text-align: center !important;")
        )))

      tab4 <- paste0("#tab_device_1", "-")
      element <- c(element, paste0(tab4, "reg_intro"))
      intro <- c(
        intro,
        HTML(paste(
          # "In this section, we can select",
          # span("sampling parameters", style = txt_key),
          # "to simulate a new dataset from the",
          # "same", span("movement model", style = txt_key),
          # "of the species provided.",

          "In this section, we will simulate a new dataset",
          "conditioned upon the previous parameters,",
          "while selecting a custom tracking regime.",
          p(),
          span(
            "First, choose GPS as your tracking device.",
            class = "tour_action")
        )))

      element <- c(element, paste0(tab4, "regBox_device"))
      intro <- c(
        intro,
        HTML(paste(
          "With", HTML(paste0(
            span("GPS & satellite loggers", class = "cl-sea-d"), ",")),
          "sampling is", span("automated", class = "cl-sea"),
          "and conducted by satellite systems. Therefore,",
          span("sampling frequency", class = "cl-dgr"),
          "and", span("sampling duration", class = "cl-dgr"),
          "are both inherently linked with",
          HTML(paste0(span("GPS battery life",
                           class = "cl-sea"), ".")),
          "This tradeoff restricts the volume of data that",
          "can be collected by GPS & satellite loggers."
        )))

      element <- c(element, paste0(tab4, "regBox_device"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, we can set the maximum",
          span("GPS battery life", class = "cl-sea-d"),
          "(how long the GPS is expected to last),",
          "the maximum", span("GPS fix rate", class = "cl-sea-d"),
          "(the longest time interval between fixes available",
          "at the duration above), and the",
          span("decay rate", class = "cl-sea-d"),
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
            span("home range", class = "cl-sea-d"),
            "and", HTML(paste0(span("distance/speed traveled",
                                    class = "cl-sea-d"), ".")),
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
        #     p(span("Lets estimate", style = paste(ttl-tour,
        #                                           "color: white;")),
        #       span("home range",
        #            style = paste(ttl-tour, col_border)),
        #       style = ttl-tour),
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
        #     fontawesome::fa(name = "circle-exclamation",
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
        #                 class = "cl-dgr"), ".")
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

          p(class = "ttl-tour cl-wht",

            "Thank you for using",
            wrap_none("move", span("design", class = "cl-sea"), "!"))
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
