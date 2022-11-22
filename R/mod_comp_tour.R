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
          "since later parts of the tour require that all",
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
          "cover definitions in detail,",
          "but you can check out the more comprehensive",
          fontawesome::fa("circle-question", fill = "white"),
          "help tips at any time."
        )))

      element <- c(element, "#content-workflow")
      intro <- c(
        intro,
        HTML(paste(
          "You will be guided through a real",
          "tracking project already available through the",
          span("ctmm", class = "cl-grn"),
          "R package.",
          "For assistance with a simulation from scratch, exit",
          "this tour (click the ", span("x", class = "cl-grey"),
          " button above) and go to the",
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
            "both 'Home range' and 'Speed & distance'",
            "as your research questions.")
        )))

      element <- c(element, ".sidebar")
      intro <- c(
        intro,
        HTML(paste(
          "The tabs necessary for this workflow are shown in order.",
          "First, we will go to the (1)",
          fontawesome::fa("file-circle-plus", fill = pal$sea),
          span("Select data", class = "cl-sea-l"), "tab",
          HTML("&mdash;"), "to load a dataset from the",
          span("ctmm", class = "cl-grn"), "R package",
          HTML("&mdash;"), "then the (2)",
          fontawesome::fa("stopwatch", fill = pal$sea),
          span("Sampling design", class = "cl-sea-l"), "tab",
          HTML("&mdash;"),
          "to set the sampling duration and interval for evaluation",
          HTML("&mdash;"), "and finally the (3)",
          fontawesome::fa("compass-drafting", fill = pal$sea),
          span("Analyses", class = "cl-sea-l"), "tabs.",
          p(),
          "Afterwards, we can see a detailed summary of",
          "all the outputs in the",
          fontawesome::fa("box-archive", fill = pal$sea),
          span("Report", class = "cl-sea-l"), "tab.",
          p(),
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"),
            "Proceed to the next tutorial box",
            "as it will automatically move you to the",
            fontawesome::fa("file-circle-plus", fill = "white"),
            "'Select data' tab.")
        )))

      ## Data tabs: -------------------------------------------------------

      tab3 <- paste0("#tab_data_select_1", "-")
      element <- c(element, paste0(tab3, "select_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "In this tab, you will be able to select one of the seven",
          "species available within the", span("ctmm", class = "cl-grn"),
          "package.",
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
          HTML("&#x2014"), "you will have access to these values",
          "after parameter extraction."
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
          "The validation step ensures that the initial sampling",
          "design will need to be sufficient to detect the",
          span("position autocorrelation", class = "cl-sea-l"),
          "parameter", 
          wrap_none("for (", span("home range ",
                         class = "cl-grn"), "estimation),"),
          "and/or the", 
          span("velocity autocorrelation", class = "cl-sea-l"),
          "parameter", 
          wrap_none("for (", span("speed & distance ",
                                  class = "cl-grn"), "estimation)."),
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
          "This is the current sampling design, extracted from",
          "the original dataset. You will be able to test out",
          "different", span("sampling designs", class = "cl-sea-l"),
          "later on."
        )))

      element <- c(element, paste0(tab3, "selectUI_parameters"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, you can see the movement process",
          HTML("&#x2014"), "in this case, the",
          span("Ornstein-Uhlenbeck with foraging (OUF)",
               class = "cl-grn"),
          wrap_none(HTML("&#x2014"), ","),
          "the extracted characteristic timescales",
          paste0("(", span("position", class = "cl-sea-l"),
                 " & ", span("velocity autocorrelation",
                               class = "cl-sea-l"),
                 ")"), "and associated parameters",
          paste0("(", span("semi-variance", class = "cl-sea-l"),
                 " and ", span("velocity", class = "cl-sea-l"),
                 ")."),
          "All subsequent steps will built upon this",
          "information."
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
          "estimation. We want both effective sample sizes to",
          "be as high as possible; but for GPS devices, there is",
          "a tradeoff, as we will see in the next tab."
        )))
      
      ## Device tab: ------------------------------------------------------
      
      tab4 <- paste0("#tab_device_1", "-")
      element <- c(element, paste0(tab4, "reg_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "Now we are ready to evaluate the intended",
          span("sampling design", class = "cl-sea-l"),
          "for a future tracking project by simulating a new dataset",
          "conditioned upon the previously extracted parameters.",
          p(),
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"),
            "Choose GPS/Satellite logger as",
            "your tracking device.")
        )))

      # c(element, "#content_device-type")
      element <- c(element, paste0(tab4, "reg_intro")) 
      intro <- c(
        intro,
        HTML(paste(
          "With these loggers",
          "sampling is", span("automated", class = "cl-sea-l"),
          "and conducted by satellite systems. Therefore,",
          span("sampling frequency", class = "cl-grn"),
          "and", span("sampling duration", class = "cl-grn"),
          "are both inherently linked with",
          HTML(paste0(span("GPS battery life",
                           class = "cl-sea-l"), ".")),
          "This tradeoff restricts the volume of data that",
          "can be collected."
        )))

      element <- c(element, paste0(tab4, "regBox_gps_device"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, we can set the maximum",
          span("GPS battery life", class = "cl-sea-l"),
          "(how long the GPS is expected to last),", "at the",
          span("maximum GPS fix rate", class = "cl-sea-l"),
          "(the longest time interval between fixes available",
          "for the duration above).",
          p(),
          "In this case, we have a GPS model that will last 12",
          "months if a new location/fix is recorded every day.",
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
          "Here, we plot the",
          span("sampling duration", class = "cl-grn"),
          em("versus"), span("sampling frequency", class = "cl-grn"),
          "(number of fixes per hour).",
          "As frequency increases (more fixes per hour), the battery",
          "life of the device will decrease until it approaches",
          wrap_none(span("zero", class = "cl-dgr-l"), "."),
          p(),
          "For the tutorial, the sampling interval of",
          span("2 hours", class = "cl-sea-l"),
          "(corresponding to the frequency of",
          wrap_none(
            span("0.5 fixes per hour", class = "cl-sea-l"), ")"),
          "has already been selected. Change to logarithmic scale",
          "for a clearer view of the curve."
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
          "fitting (conditioned by current parameters).",
          "Wait until both are finished before proceeding."
        )))
      
      element <- c(element, paste0(tab4, "regBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          "As mentioned earlier, we focus on the",
          span("effective sample size", class = "cl-grn"),
          "for home range estimation",
          wrap_none("(", HTML("N<sub>area</sub>"), ")"),
          "and speed/distance estimation",
          wrap_none("(", HTML("N<sub>speed</sub>"), ")."),
          "For the current parameters, and sampling design of",
          span("one fix every 2 hours", class = "cl-sea-l"), "for",
          "a realized duration of",
          wrap_none(span("1.7 months", class = "cl-sea-l"), ","),
          "we will have", HTML("N<sub>area</sub>"),
          "< 5 and", HTML("N<sub>speed</sub>"),"\u2248 164."
        )))
      
      element <- c(element, paste0(tab4, "regBox_sims"))
      intro <- c(
        intro,
        HTML(paste(
          "In the", fontawesome::fa("location-dot", fill = pal$sea),
          span("Data", class = "cl-sea-l"), "we plot the new",
          "simulated data (in color), and the initial dataset",
          "(in grey): in this case, the individual Cilla from the",
          "African Buffalo dataset.",
          p(),
          "In the", fontawesome::fa("chart-line", fill = pal$sea),
          span("Variogram", class = "cl-sea-l"), "we can check if",
          "the new simulated individual is",
          wrap_none(span("range resident", class = "cl-grn"), ","),
          em("i.e."), "if the semi-variance reaches an asymptote."
        )))
      
      element <- c(element, paste0(tab4, "regBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"), 
            "Click on the", fontawesome::fa("bookmark"), "'Add to table'", 
            "button to save all current parameters to",
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
          span("Sampling design", class = "cl-sea-l"), "tab.")
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
            ", which is the ",
            span("point estimate", class = "cl-sea-l"), ", ",
          "followed by the 95% confidence intervals (low", HTML("&#x2014"),
          "high) as the point estimate is subject to uncertainty."), 
          p(),
          fontawesome::fa(name = "radiation", fill = "#bdbdbd"),
          wrap_none(
            span("Expected error", style = "color: #bdbdbd;"),
            ", which is the ", span("relative error (in %) ",
                                    class = "cl-dgr-l"),
            "of the point estimate (and of the 95% CIs; ",
            "once again, low", HTML("&#x2014"),
            "high) in relation to the expected value (truth) ",
            "derived from the distribution.")
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
          HTML("N<sub>area</sub>"), "is equal to 4.8,",
          "which means we have less than five home range",
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
      
      ### Speed & distance ------------------------------------------------
      
      element <- c(element, "#group_design")
      intro <- c(
        intro,
        HTML(paste(
          "Now we can move on to",
          span("speed & distance", class = "cl-sea-l"), "estimation."
        )))
      
      tab6 <- paste0("#tab_ctsd_1", "-")
      element <- c(element, paste0(tab6, "sd_intro"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action",
            
            fontawesome::fa("forward-step"), 
            "Click on the", fontawesome::fa("paper-plane"), 
            "'Run estimation' button to estimate speed & distance.",
            p()),
          "For this step, we are using the",
          span("Continuous Time Speed & Distance (CTSD)",
               class = "cl-sea-l"), "estimator available in the",
          span("ctmm", class = "cl-grn"),
          "R package."
        )))
      
      element <- c(element, paste0(tab6, "sdPlot_path"))
      intro <- c(
        intro,
        HTML(paste(
          "This is the visual output from the",
          span("distance", class = "cl-sea"), "estimation.",
          "You want the trajectory (in color) to follow",
          "the true fine-scale trajectory (in grey) as closely",
          "as possible.",
          p(),
          "Similarly to the home range tab,",
          "you can also check the",
          fontawesome::fa(name = "gauge-high", fill = "#bdbdbd"),
          span("Total distance traveled", style = "color: #bdbdbd;"),
          "estimate and CIs, followed by the",
          fontawesome::fa(name = "radiation", fill = "#bdbdbd"),
          wrap_none(
            span("Expected error", style = "color: #bdbdbd;"), ","),
          "for the current sampling design."
        )))
      
      element <- c(element, paste0(tab6, "sdPlot_main"))
      intro <- c(
        intro,
        HTML(paste(
          "This is the visual output from the",
          span("speed", class = "cl-sea"), "estimation.",
          "You want the", span("speed estimate", class = "cl-sea"),
          "to match with the true speed (in black)",
          "as closely as possible.",
          p(),
          
          "If the CIs are wide, the estimate is highly",
          "uncertain, and your study may require a shorter",
          wrap_none("sampling interval", 
                    css = "cl-grn", end = ".")
        )))
      
      element <- c(element, paste0(tab6, "sdBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          HTML("N<sub>speed</sub>"), "is equal to 165,",
          "which means we have the equivalent of 165 independent",
          "locations for speed estimation. You want this",
          span("effective sample size", class = "cl-grn"),
          "to be as high as possible, reducing uncertainty."
        )))
      
      element <- c(element, paste0(tab6, "sdBox_outputs"))
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
      
      element <- c(element, paste0(tab6, "sdBox_summary"))
      intro <- c(
        intro,
        HTML(paste(
          span(style = "text-align: center !important;",
               "Here are the outputs from the",
               fontawesome::fa("gauge-high", fill = pal$sea),
               span("Speed & distance", class = "cl-sea-l"), "tab.")
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
            "answer to how likely your current sampling design",
            "is to answer your research questions."),
          p(),
          "In this case,",
          "is the", span("sampling duration", class = "cl-sea-l"),
          "sufficient to obtain a reliable",
          span("home range area", class = "cl-sea-l"), "estimate?",
          p(),
          "And is the", span("sampling interval", class = "cl-sea-l"),
          "sufficient to obtain a reliable",
          span("speed & distance", class = "cl-sea-l"), "estimate?"
        )))
      
      element <- c(element, paste0(tab7, "repBox_pars"))
      intro <- c(
        intro,
        HTML(paste(
          "This box recaps all extracted parameters",
          "and the current sampling design."
        )))
      
      element <- c(element, paste0(tab7, "repBox_analyses"))
      intro <- c(
        intro,
        HTML(paste(
          "You can compare your results to our simulations of",
          "a similar",
          span("position autocorrelation", class = "cl-sea-l"),
          "and/or",
          span("velocity autocorrelation", class = "cl-sea-l"),
          "parameter."
        )))
      
      element <- c(element, paste0(tab7, "repBox_tables"))
      intro <- c(
        intro,
        HTML(paste(
          "All settings and outputs will be collected here.",
          "If you compare multiple", 
          wrap_none(span("sampling designs", class = "cl-grn"), ","),
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


      