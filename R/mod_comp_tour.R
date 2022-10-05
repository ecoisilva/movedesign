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
    pal <- load_pal()
    
    # Build tours: ------------------------------------------------------

    # add_action <- span("Action:", class = "tour-action")

    build_mainTour <- function(ns, vals) {
      element <- intro <- character(0)
  
      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          p(class = "ttl_tour cl-wht",

            "Welcome to",
            wrap_none("move", span("design", class = "cl-sea-l"))),

          "Here, you will be guided step-by-step through the",
          "app. At times, you will be prompted to perform a",
          "specific action, which will be",
          span(
            class = "tour_action",
            wrap_none(fontawesome::fa("forward-step"),
                      " highlighted", ".")),
          "Please take care to follow these instructions,",
          "since later parts of the tutorial require that all",
          "previous steps are performed.",
          p(), "If no action is requested, simply proceed",
          "to the next step by",
          span(
            class = "tour_action",
            "clicking on the 'Next' button or using the right",
            "arrow of your keyboard.")
        )))

      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          span("Warning:", class = "cl-dgr-l"),
          "do not interact with anything outside of highlighted zones",
          "create alongside the tutorial boxes.",
          p(),
          "This tutorial will",
          span("not", class = "cl-dgr-l"),
          "cover definitions in detail.",
          "If you want to check the more comprehensive",
          fontawesome::fa("circle-question", fill = "white"),
          "help tips."
        )))

      element <- c(element, "#content-workflow")
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
          fontawesome::fa("file-signature", fill = pal$sea),
          span("Simulate data", class = "cl-sea-l"),
          "tab. A secondary guided tour will be available there.",
          p(),
          span(
            class = "tour_action",

            fontawesome::fa("forward-step"),
            "Please choose",
            fontawesome::fa("square-check", fill = "white"),
            "'Select' as your Data source, to choose",
            "from a list of available species, ",
            "and", fontawesome::fa("square-check", fill = "white"),
            "'Home range' as your research question.")
        )))

      element <- c(element, ".sidebar")
      intro <- c(
        intro,
        HTML(paste(
          "The tabs necessary for this workflow are shown in order.",
          "First, we will go to (1) the",
          fontawesome::fa("file", fill = pal$sea),
          span("Select data", class = "cl-sea-l"), "tab",
          HTML("&mdash;"), "to load a dataset from the",
          span("ctmm", class = "cl-grn"), "R package",
          HTML("&mdash;"), "then (2) the",
          fontawesome::fa("stopwatch", fill = pal$sea),
          span("Tracking regime", class = "cl-sea-l"), "tab",
          HTML("&mdash;"),
          "to set the sampling duration and interval for evaluation",
          HTML("&mdash;"), "and finally (3) the",
          fontawesome::fa("compass-drafting", fill = pal$sea),
          span("Analyses", class = "cl-sea-l"), "tab.",
          p(),
          "Afterwards, we can see a detailed summary of",
          "all the outputs in the",
          fontawesome::fa("box-archive", fill = pal$sea),
          span("Report", class = "cl-sea-l"), "tab."
        )))

      ## Data tabs: -------------------------------------------------------

      tab3 <- paste0("#tab_data_select_1", "-")
      element <- c(element, paste0(tab3, "select_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "In this tab, you will be able to select one of the seven",
          "species available within the", span("ctmm", class = "cl-grn"),
          "R package and extract their parameters.",

          "Parameters extracted from one of the species provided",
          "may help inform animal tracking studies of other",
          "species with similar",
          paste0(span("movement behaviors",
                      class = "cl-sea-l"), ".")
        )))

      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action",

            fontawesome::fa("forward-step"),
            "First, make sure the species currently selected",
            "from the list is the African Buffalo",
            wrap_none("(", em("Syncerus caffer"), ")."),
            "Do not select or click any other options yet.")
        )))

      element <- c(element, "#comp_viz_selected-dataTabs_viz")
      intro <- c(
        intro,
        HTML(paste(
          "These are the data visualization tabs. The first",
          "tab", fontawesome::fa("paw", fill = pal$sea),
          "covers all individuals present in the selected dataset.",
          "The second tab", fontawesome::fa("filter", fill = pal$sea),
          "filters to the chosen individual for",
          "parameter extraction. The third tab ",
          fontawesome::fa("chart-line", fill = pal$sea),
          "will show you the", span("variogram", class = "cl-grn"),
          "for that particular individual."
        )))

      element <- c(element, "#comp_viz_selected-dataTable_all")
      intro <- c(
        intro,
        HTML(paste(
          "You want to pick the individual",
          "that accurately represents the",
          span("movement behavior", class = "cl-sea-l"),
          "you intend to emulate.",
          p(),
          "Ideally, this is an individual with similar",
          wrap_none(span("directional persistence",
                         class = "cl-grn"), ","), "and/or",
          span("home range crossing time", class = "cl-grn"),
          HTML("&#x2014"), "depending on your research question.",
          p(),
          "You will be able to see these parameters after",
          "the 'Extract' step, but the",
          span("sampling regime", class = "cl-sea-l"),
          "will need to be sufficient to detect these",
          "parameters, so a 'Validate' step is also necessary."
        )))

      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action",

            fontawesome::fa("forward-step"),
            "Now, pick individual 'Cilla' from the dropdown menu",
            "and click the", fontawesome::fa("wand-magic-sparkles"),
            "'Validate' button before proceeding."),
          p(),
          "Outside of this tutorial, you can also select an",
          "individual from the table or",
          "the plot in the data visualization",
          fontawesome::fa("paw", fill = pal$sea), " tab."
        )))

      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          "The validation step ensures that certain datasetst",
          "are no used (e.g., with no",
          span("velocity/position autocorrelation",
               class = "cl-sea-l"), "parameter estimate",
          "for the respective research question) are not",
          "used, or if the uncertainty associated with those",
          "parameters is too high.",
          p(),
          "If the button now reads",
          fontawesome::fa("circle-check"),
          "'Validated!',",
          span(
            class = "tour_action",

            fontawesome::fa("forward-step"),
            "proceed by clicking the",
            fontawesome::fa("paper-plane"), "'Extract' button.")
        )))

      element <- c(element, paste0(tab3, "selectBox_regime"))
      intro <- c(
        intro,
        HTML(paste(
          "This is the current tracking regime, extracted from",
          "the original dataset. You will be able to test out",
          "different", span("tracking regimes", class = "cl-sea-l"),
          "later on."
        )))

      element <- c(element, paste0(tab3, "selectUI_parameters"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, you can see the relevant temporal",
          paste0("(", span("e.g.", style = "font-style: italic;"),
                 ", ", span("position", class = "cl-sea-l"),
                 " and ", span("velocity autocorrelation ",
                               class = "cl-sea-l"),
                 "timescale)"), "and spatial parameters",
          paste0("(", span("semi-variance", class = "cl-sea-l"),
                 ")"), "will show up here.",
          "All subsequent steps will built upon these",
          "parameters."
        )))

      element <- c(element, paste0(tab3, "selectBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, you can see the",
          span("sample sizes", class = "cl-sea-l"),
          "of the original dataset.",
          span("Absolute sample size (n)", class = "cl-grn"),
          "corresponds to the total number of observations in a",
          "dataset. The more important information comes from the",
          span("effective sample sizes (N)", class = "cl-grn"),
          "for", span("home range", class = "cl-sea-l"), "and",
          span("speed & distance", class = "cl-sea-l"),
          "estimation."
        )))
      
      element <- c(element, paste0(tab3, "selectBlock_Narea"))
      intro <- c(
        intro,
        HTML(paste(
          "For our research question, we should focus on the",
          span("effective sample size", class = "cl-grn"),
          "for home range estimation, which",
          "is equal to number of home range crossings that",
          "occurred during the study period."
        )))

      ## Device tab: ------------------------------------------------------

      element <- c(element, "#group_device")
      intro <- c(
        intro,
        HTML(paste(
          span("Now we are ready to evaluate the intended",
               span("tracking regime", class = "cl-sea-l-d"),
               "for a future tracking project.",
               style = "text-align: center !important;")
        )))

      tab4 <- paste0("#tab_device_1", "-")
      element <- c(element, paste0(tab4, "reg_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "We will simulate a new dataset",
          "conditioned upon the previous parameters,",
          "while selecting a custom tracking regime.",
          p(),
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"),
            "First, choose GPS/Satellite logger as",
            "your tracking device.")
        )))

      element <- c(element, "#content_device-type")
      intro <- c(
        intro,
        HTML(paste(
          "With", HTML(paste0(
            span("GPS & satellite loggers", class = "cl-grn"), ",")),
          "sampling is", span("automated", class = "cl-sea-l"),
          "and conducted by satellite systems. Therefore,",
          span("sampling frequency", class = "cl-grn"),
          "and", span("sampling duration", class = "cl-grn"),
          "are both inherently linked with",
          HTML(paste0(span("GPS battery life",
                           class = "cl-sea-l"), ".")),
          "This tradeoff restricts the volume of data that",
          "can be collected by GPS & satellite loggers."
        )))

      element <- c(element, paste0(tab4, "regBox_gps_device"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, we can set the maximum",
          span("GPS battery life", class = "cl-sea-l"),
          "(how long the GPS is expected to last),",
          "the", span("maximum GPS fix rate", class = "cl-sea-l"),
          "(the longest time interval between fixes available",
          "at the duration above), the",
          span("minimum GPS fix rate", class = "cl-sea-l"),
          "(the shortest time interval between fixes,",
          "when duration approaches", 
          wrap_none(span("zero", class = "cl-dgr-l"), ","),
          "and the", span("decay rate", class = "cl-sea-l"),
          "(how fast the GPS battery is expected to decay by",
          "increasing the sampling interval).",
          p(),
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"),
            "Keep the current inputs as shown,",
            "and proceed to the next tutorial box.")
        )))
      
      element <- c(element, paste0(tab4, "regBox_sampling"))
      intro <- c(
        intro,
        HTML(paste(
          "Here we plot the sampling duration", em("versus"),
          "the", span("sampling frequency", class = "cl-grn"),
          "(number of fixes per hour).",
          "As frequency increases, the", 
          span("duration", class = "cl-grn"),
          "of the device will decrease until it approaches",
          wrap_none(span("zero", class = "cl-dgr-l"), "."),
          p(),
          "For the tutorial, the sampling interval of",
          span("one hour", class = "cl-sea-l"),
          "(corresponding to the frequency of",
          wrap_none(span("1 fix every hour", class = "cl-sea-l"), ")"),
          "is already been selected."
        )))
      
      element <- c(element, paste0(tab4, "regBox_sampling"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"), 
            "Click the", fontawesome::fa("wand-magic-sparkles"),
            "'Validate' button until it reads 'Validated!',",
            "then click the", fontawesome::fa("bolt"), 
            "'Run' button."),
          p(),
          "You will see two notifications: the first during",
          "the calculation of the expected run time,",
          "and the second during the actual simulation and model",
          "fitting (conditioned by the previously set parameters).",
          "Wait until both are finished before proceeding."
        )))
      
      element <- c(element, paste0(tab4, "regBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          "As mentioned earlier, we focus on the",
          span("effective sample size", class = "cl-grn"),
          "for home range estimation",
          wrap_none("(", HTML("N<sub>area</sub>"), ")."),
          "For the set species parameters, and tracking regime of",
          span("1 fix every hour", class = "cl-sea-l"), "for",
          wrap_none(span("4 months", class = "cl-sea-l"), ","),
          "we will likely have an effective sample size of",
          "\u2264 20."
        )))
      
      element <- c(element, paste0(tab4, "regBox_sims"))
      intro <- c(
        intro,
        HTML(paste(
          "In the", fontawesome::fa("location-dot", fill = pal$sea),
          span("Data", class = "cl-sea-l"), "we plotted the new",
          "simulated data (in color), and the initial dataset",
          "(in grey): in this case, the individual Cilla from the",
          "African Buffalo dataset.",
          p(),
          "In the", fontawesome::fa("chart-line", fill = pal$sea),
          span("Variogram", class = "cl-sea-l"), "we can check if",
          "the new simulated individual is",
          wrap_none(span("range resident", class = "cl-grn"), ","),
          "i.e. the semi-variance reaches an asymptote."
        )))
      
      element <- c(element, paste0(tab4, "regBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"), 
            "Click on the", fontawesome::fa("bookmark"), "'Add to table'", 
            "button to save all species and sampling parameters to",
            "a table for ease of comparison."
          )
        )))
      
      element <- c(element, paste0(tab4, "regBox_summary"))
      intro <- c(
        intro,
        HTML(paste(
          span(style = "text-align: center !important;",
          "Here are the outputs from the",
          fontawesome::fa("stopwatch", fill = pal$sea),
          span("Tracking regime", class = "cl-sea-l"), "tab.")
        )))
      
      ## Analyses tab: ----------------------------------------------------
      ### Home range ------------------------------------------------------

      element <- c(element, "#group_design")
      intro <- c(
        intro,
        HTML(paste(
          "Now we are ready to move on to",
          span("home range", class = "cl-sea-l"), "estimation."
        )))

      tab5 <- paste0("#tab_hrange_1", "-")
      element <- c(element, paste0(tab5, "hr_intro"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"), 
            "Click on the", fontawesome::fa("paper-plane"), 
            "'Run estimation' button to estimate home range area.",
            p()),
          "For this step, we are using the",
          span("Autocorrelated Kernel Density Estimator (AKDE)",
               class = "cl-sea-l"), "available in the",
          span("ctmm", class = "cl-grn"),
          "R package."
        )))
      
      element <- c(element, paste0(tab5, "hrBox_viz"))
      intro <- c(
        intro,
        HTML(paste(
          "This is the visual output from the",
          span("home range", class = "cl-sea"), "estimation.",
          "You want the 95% CIs (in", wrap_none(
            span("red", class = "cl-dgr-l"), ")"),
          "to 'hug' the estimate (in grey)",
          "as much as possible. To show the 95% CIs, click on",
          "the corresponding grey buttons).",
          p(),
          "If the CIs are wide, the estimate is highly",
          "uncertain, and your study may require a longer",
          wrap_none(span("sampling duration", class = "cl-grn"), ".")
        )))
      
      element <- c(element, "#content_hr-areas")
      intro <- c(
        intro,
        HTML(paste(
          "To further help during evaluation,",
          "you can see two sets of values:",
          p(),
          fontawesome::fa(name = "bullseye", fill = "#bdbdbd"),
          wrap_none(
            span("Estimate", style = "color: #bdbdbd;"),
            ", which is the home range area ",
            span("point estimate", class = "cl-sea-l"), ", ",
          "followed by the 95% confidence intervals (low — high) ",
          "as the point estimate is subject to uncertainty."), 
          p(),
          fontawesome::fa(name = "radiation", fill = "#bdbdbd"),
          wrap_none(
            span("Expected error", style = "color: #bdbdbd;"),
            ", which is the ", span("relative error (in %) ",
                                    class = "cl-dgr-l"),
            "of the point estimate (and of the 95% CIs; ",
            "once again, low — high) ",
            "in relation to ",
            "the expected value (truth) derived ",
            "from the distribution.")
        )))

      
      element <- c(element, "#content_hr-areas")
      intro <- c(
        intro,
        HTML(paste(
          "Similarly to the plot, you want the",
          span("expected error", class = "cl-dgr-l"),
          "to be low, and the CIs to 'hug' the point estimate",
          "as much as possible. If not, the point estimate is highly",
          "uncertain."
        )))
     

      element <- c(element, paste0(tab5, "hrBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          HTML("N<sub>area</sub>"), "is \u2264 20,",
          "which means we have around or less than 20 home range",
          "crossing events in this dataset. You want this",
          span("effective sample size", class = "cl-grn"),
          "to be as high as possible, reducing uncertainty."
        )))

      element <- c(element, paste0(tab5, "hrBox_viz"))
      intro <- c(
        intro,
        HTML(paste(
          "You can once again save all results to a table",
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"), 
            "by clicking on the", fontawesome::fa("bookmark")
            , "'Add to table'", "button."
          )
        )))
      
      element <- c(element, paste0(tab5, "hrBox_summary"))
      intro <- c(
        intro,
        HTML(paste(
          span(style = "text-align: center !important;",
               "Here are the outputs from the",
               fontawesome::fa("map-location-dot", fill = pal$sea),
               span("Home range", class = "cl-sea-l"), "tab.")
        )))
      
      
      ## Report tab: ------------------------------------------------------
      
      tab7 <- paste0("#tab_report_1", "-")
      element <- c(element, paste0(tab7, "repBox_details"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"), 
            "Click the", fontawesome::fa("bookmark"),
            "'Built report' button to see a detailed",
            "interpretation of how likely your current tracking regime",
            "is to answer your research question."),
          p(),
          "In this case,",
          "is the", span("sampling duration", class = "cl-sea-l"),
          "sufficient to obtain a reliable",
          span("home range area", class = "cl-sea-l"), "estimate?"
          
          # "Change the credible intervals here."
        )))
      
      # element <- c(element, paste0(tab7, "repBox_pars"))
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     "This box recaps all species parameters",
      #     "and tracking regime details."
      #   )))
      
      element <- c(element, paste0(tab7, "repBox_analyses"))
      intro <- c(
        intro,
        HTML(paste(
          "You can compare your results to our simulations of an",
          "approximate position autocorrelation parameter."
        )))
      
      element <- c(element, paste0(tab7, "repBox_tables"))
      intro <- c(
        intro,
        HTML(paste(
          "All settings and outputs will be collected here.",
          "If you compare multiple", 
          wrap_none(span("tracking regimes", class = "cl-grn"), ","),
          "they will show up here as well."
        )))
      
      element <- c(element, "#Tour_end")
      intro <- c(
        intro,
        HTML(paste(
          p(class = "ttl_tour cl-wht",

            "Thank you for using",
            wrap_none("move", span("design", class = "cl-sea-l"), "!"))
        )))

      data.frame(element = element,
                 intro = intro,
                 stringsAsFactors = FALSE)

    } # end of main tour

    # -------------------------------------------------------------------

    # Allow dynamic UI actions based on tour being active:

    observe({
      req(vals$active_tab)
      if (vals$active_tab == 'about') vals$tour_active <- FALSE
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
                        rintrojs::readCallback("switchTabs")))
      
    }) %>% # observe event, bound to:
      bindEvent(input$default_tour)
    
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_comp_tour_ui("comp_tour_1")

## To be copied in the server
# mod_comp_tour_server("comp_tour_1")


      