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
      label = "Click here for a guided tour",
      icon = icon("compass"),
      width = "220px",
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

          insert_action,
          "Click ",
          fontawesome::fa(name = "wrench", fill = hex_border),
          span("'Adjust regime'",
               style = paste(txt_tour, col_border)),
          "and press 'Next'."
        ))
      }

    })
    # Build tours: ------------------------------------------------------

    insert_action <- span("Action:", style = paste(txt_action))

    build_mainTour <- function(ns, vals) {

      element <- intro <- character(0)

      # Help for Home tab:

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
          "specific action, which will be marked with",
          insert_action,
          "text. Please take care",
          "to follow these instructions, since later parts of",
          "the tour may assume that all previous steps were",
          "performed.", shiny::p(),

          insert_action,
          "click on the 'Next' button or use the right arrow",
          "of your keyboard to proceed."
        ))
      )

      # Select if simulated or selected data path is chosen:

      element <- c(element, "#group_data")
      intro <- c(
        intro,
        HTML(paste(
          "In this tour, you will be guided through a real",
          "tracking project already available through the",
          span("ctmm", style = paste(txt_tour, col_border)),
          "R package.",

          "For assistance with a simulated dataset, exit",
          "this tour (press the ", span("x", style = col_grey),
          " button above) and click on the",
          span("Simulate data", style = paste(txt_tour, col_grey)),
          "tab. A secondary guided tour will be available there."
        ))
      )

      # Help for Data tab:

      tab1 <- paste0("#tab_data_1", "-")

      element <- c(element, paste0(tab1, "data_intro"))
      intro <- c(
        intro,
        HTML(paste(
          insert_action,
          "In this box, please choose",
          paste0(span("Option 1. Select species",
                      style = paste(txt_tour, col_grey)), ","),
          "then go to the next step."
        )))

      element <- c(element, paste0(tab1, "dataBox_option1"))
      intro <- c(
        intro,
        HTML(paste(
          insert_action,
          "First, select one of the available species from the list.",
          "Parameters extracted",
          "from the species provided may help inform animal tracking",
          "studies of other species with similar",
          paste0(span("movement behaviors",
                      style = paste(txt_tour, col_border)), "."),
          p(),

          "Then, select one individual from its dataset, and click",
          "the", fontawesome::fa(name = "check-circle"),
          "Validate button before proceeding."
        )))

      element <- c(element, paste0(tab1, "dataPlot_id"))
      intro <- c(
        intro,
        HTML(paste(
          "This plot shows the individual selected",
          "(in x/y coordinate system)..."
        )))

      element <- c(element, paste0(tab1, "dataTable_id"))
      intro <- c(
        intro,
        HTML(paste(
          ".. While the table shows the dataset itself.",
          "This is useful to make sure the data is being read in",
          "correctly, and that the relevant variables",
          "are available."
        )))

      element <- c(element, paste0(tab1, "dataBox_variables"))
      intro <- c(
        intro,
        HTML(paste(

          insert_action,

          "Confirm that the correct variables are being picked up for",
          "x and y coordinates, and for timestamp.", br(),
          "Then click", fontawesome::fa(name = "paper-plane"),
          "Extract button to extract all relevant parameters",
          "from the dataset. This action may take a while, since",
          "it requires model fitting."
        )))

      element <- c(element, paste0(tab1, "dataBox_parameters"))
      intro <- c(
        intro,
        HTML(paste(
          "All relevant parameters",
          paste0(
            "(", span("e.g.", style = paste(txt_tour,
                                            "font-style: italic;")),
            ", ", span("position", style = paste(txt_tour, col_border)),
            " and ", span("velocity autocorrelation ",
                          style = paste(txt_tour, col_border)),
            "timescale)"), "will show up here."
        )))

      # element <- c(element, "#group_device")
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     "Now we are ready to set the tracking regime."
      #   )))
      #
      # element <- c(element, "#group_design")
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     "Now we are ready to move on to either the",
      #     "estimation of",
      #     span("home range", style = paste(txt_tour, col_key)),
      #     "and", HTML(paste0(span("distance/speed traveled",
      #                             style = paste(txt_tour, col_key)),
      #                        ".")),
      #     "This tour will run through one and then the other."
      #   )))
      #
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
      #     insert_action,
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

      element <- c(element, "#Tour_tmp")
      intro <- c(
        intro,
        HTML(paste(
          "[work in progress]"
        )))

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

        ))
      )

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
          nextLabel = "Next",
          prevLabel = "Previous",
          showStepNumbers = F,
          showButtons = T,
          showBullets = T
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
