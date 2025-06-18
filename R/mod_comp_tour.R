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
    
    fluidRow(
      align = "center",
      div(id = "content-tutorial",
          
          p(),
          # actionButton(
          #   inputId = ns("default_tour"),
          #   label = "Individual estimate",
          #   icon = icon("compass"),
          #   width = "238px",
          #   class = "btn-primary"),

          splitLayout(
            cellWidths = c("200px", "1%", "200px"),
            cellArgs = list(style = 'align: center;'),
            actionButton(
              inputId = ns("default_tour"),
              label = "Individual estimate",
              icon = icon("compass"),
              width = "100%",
              class = "btn-primary"),
            p(),
            actionButton(
              inputId = ns("default_tour_pop"),
              icon =  icon("compass"),
              label = "Population estimate",
              width = "100%",
              class = "btn-info")

          ), # end of splitLayout
          p()
          
      )) # end of fluidRow
  
  ) # end of tagList
}

#' comp_tour Server Functions
#'
#' @noRd
mod_comp_tour_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    rv$tour <- reactiveValues()
    
    text_device <- reactive({
      
      span(
        "Set the 'GPS battery life' to 2 years and the",
        "'maximum GPS fix rate' to 1 fix every 6 hours.")
      
    }) # end of reactive, text_device()
    
    text_sizes <- reactive({
      
      dur <- "10.1 months"
      N1 <- "32"
      N2 <- "1,296"
      
      span(
        "For the current parameters, the sampling interval of",
        span("one fix every 2 hours", class = "cl-sea-l"), 
        "and the realized duration of",
        wrap_none(span(dur, class = "cl-sea-l"), ","),
        "results in an", HTML("N<sub>area</sub>"),
        "\u2248", N1, "and", HTML("N<sub>speed</sub>"), 
        "\u2248", wrap_none(N2, "."))
      
    }) # end of reactive, text_sizes()
    
    text_hr <- reactive({
      
      N1 <- rep("32", 2)
      
      span(
        HTML("N<sub>area</sub>"), "is equal to", wrap_none(N1[1], ","),
        "which means we have", N1[2], "home range",
        "crossing events in this dataset. You want this",
        span("effective sample size", class = "cl-grn"),
        "to be as high as possible, reducing uncertainty.")
      
    }) # end of reactive, text_hr()
    
    text_ctsd <- reactive({
      
      N2 <- "1,296"
      
      span(
        HTML("N<sub>speed</sub>"), "is equal to", wrap_none(N2, ","),
        "which means we have the equivalent of", N2, "independently",
        "sampled velocities for speed estimation. You want this",
        span("effective sample size", class = "cl-grn"),
        "to be as large as possible, reducing uncertainty.")
      
    }) # end of reactive, text_ctsd()
    
    # TOURS: --------------------------------------------------------------
    
    ## Build 'Individual estimate' tour: ----------------------------------
    
    .build_tour <- function(ns, rv) {
      action_bell <- wrap_none("(", fontawesome::fa("bell"), ")")
      element <- intro <- character(0)
      
      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          p(class = "ttl_tour cl-wht",
            
            "Welcome to",
            wrap_none("move", span("design", class = "cl-sea-l"))),
          
          "Here, you will be guided step-by-step through the",
          "application. At certain points, you will be prompted to",
          "perform specific actions, which will be clearly",
          
          span(class = "tour_action", action_bell, "highlighted."),
          "Follow these instructions carefully,",
          "as later parts of the tutorial require that all",
          "previous steps were performed.",
          p(), "If no action is requested, simply proceed",
          "to the next step by",
          
          span(class = "tour_action", action_bell,
               "clicking on the 'Next' button or using the right",
               "arrow of your keyboard.")
        )))
      
      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          span("Warning:", class = "cl-dgr"),
          "do not interact with anything outside the highlighted zones",
          "created alongside the tutorial boxes.",
          p(),
          "This tutorial will not cover definitions in detail,",
          "but you can check out the more comprehensive",
          fontawesome::fa("circle-question", fill = "white"),
          "help tips at a later time."
        )))
      
      element <- c(element, "#content-workflow")
      intro <- c(
        intro,
        HTML(paste(
          "You will be guided through a real",
          "animal tracking project available through the",
          span("ctmm R", class = "cl-grn"), "package.",
          "For assistance with a simulation from scratch, exit",
          "this tour (click the ", span("x", class = "cl-grey"),
          " button above) and go to the",
          fontawesome::fa("file-pen", fill = pal$sea),
          span("Simulate data", class = "cl-sea-l"),
          "tab. Another guided tour will be available there.",
          p(),
          
          span(class = "tour_action", action_bell,
               "Please choose",
               fontawesome::fa("square-check", fill = "white"),
               "'Select' as your Data source, to choose",
               "from a list of available species, ",
               "and both ",
               fontawesome::fa("square-check", fill = "white"),
               "'Home range' and ",
               fontawesome::fa("square-check", fill = "white"),
               "'Speed & distance' as your research targets.",
               br(), 
               "For analytical targets,",
               "select 'Individual estimate'.")
        )))
      
      element <- c(element, ".sidebar")
      intro <- c(
        intro,
        HTML(paste(
          "The tabs necessary for this workflow are shown in order.",
          "First, we will go to the (1)",
          fontawesome::fa("file-circle-plus", fill = pal$sea),
          span("Select data", class = "cl-sea-l"),
          "tab (to load a dataset from the",
          span("ctmm", class = "cl-grn"), "package),",
          "then the (2)",
          fontawesome::fa("stopwatch", fill = pal$sea),
          span("Sampling design", class = "cl-sea-l"), "tab",
          "(to set the sampling duration and interval for evaluation)",
          "and finally the (3)",
          fontawesome::fa("compass-drafting", fill = pal$sea),
          span("Analyses", class = "cl-sea-l"), "tabs.",
          p(),
          "Afterwards, we can see a detailed summary of",
          "all the outputs in the",
          fontawesome::fa("box-archive", fill = pal$sea),
          span("Report", class = "cl-sea-l"), "tab.",
          p(),
          
          span(class = "tour_action", action_bell,
               "Proceed to the next step",
               "as it will move you to the",
               fontawesome::fa("file-circle-plus", fill = "white"),
               "'Select data' tab automatically.")
        )))
      
      ## Data tabs: -------------------------------------------------------
      
      tab3 <- paste0("#tab_data_select_1", "-")
      element <- c(element, paste0(tab3, "select_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "In this tab, you will be able to select one of the seven",
          "species available in the", span("ctmm", class = "cl-grn"),
          "package.",
          "Parameters extracted from one of the species provided",
          "can help inform tracking studies of",
          "species with similar", 
          paste0(span("movement behaviors", class = "cl-sea-l"), ".")
        )))
      
      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action", action_bell,
            "First, select the African Buffalo",
            wrap_none("(", em("Syncerus caffer"), ")"),
            "as your study species.",
            "Do not select or click any other options yet.")
        )))
      
      element <- c(element, "#comp_viz_selected-vizTabs_data")
      intro <- c(
        intro,
        HTML(paste(
          "These are the data visualization tabs. The first",
          "tab", fontawesome::fa("paw", fill = pal$sea),
          "covers all individuals present in the selected dataset.",
          "The second tab", fontawesome::fa("filter", fill = pal$sea),
          "filters to the chosen individual for",
          "parameter extraction. The third tab ",
          fontawesome::fa("bug", fill = pal$sea), 
          "can help to identify outliers. The fourth and final tab",
          fontawesome::fa("chart-line", fill = pal$sea),
          "will show you the", span("variogram", class = "cl-grn"),
          "for that particular individual."
        )))
      
      element <- c(element, "#comp_viz_selected-vizTable_all")
      intro <- c(
        intro,
        HTML(paste(
          "You want to pick an individual",
          "that accurately represents the",
          span("movement behavior", class = "cl-sea-l"),
          "you intend to emulate.",
          p(),
          "Ideally, this will be one where you can detect",
          wrap_none(span("directional persistence",
                         class = "cl-grn"), ","), "and/or",
          span("home range crossing time", class = "cl-grn"),
          HTML("&#x2014"), "you will see if these values",
          "are available during the validation step."
        )))
      
      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action", action_bell,
            
            "Now, pick individual 'Cilla' from the dropdown menu",
            "and click the", fontawesome::fa("wand-magic-sparkles"),
            "'Validate' button before proceeding."),
          p(),
          "Outside of this tour, you can also select an",
          "individual from the table or",
          "the plot in the data visualization",
          fontawesome::fa("paw", fill = pal$sea), " tab."
        )))
      
      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          "The validation step ensures that we can detect the",
          span("position autocorrelation", class = "cl-sea-l"),
          "timescale parameter for", span("home range", 
                                          class = "cl-grn"), 
          "estimation, and the", 
          span("velocity autocorrelation", class = "cl-sea-l"),
          "timescale parameter for", span("speed & distance",
                                          class = "cl-grn"), 
          "estimation.",
          p(),
          
          span(
            class = "tour_action", action_bell,
            "The button should now read",
            fontawesome::fa("circle-check"),
            "'Validated!'. Now click the",
            fontawesome::fa("paper-plane"), "'Extract' button.")
        )))
      
      element <- c(element, paste0(tab3, "selectBox_schedule"))
      intro <- c(
        intro,
        HTML(paste(
          "This is the current sampling design, extracted from",
          "the original dataset. You will be able to test out",
          "different", span("sampling designs", class = "cl-sea-l"),
          "later on."
        )))
      
      element <- c(element, paste0(tab3, "selectBox_pars"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, you can see the movement process",
          HTML("&#x2014"), "in this case, the",
          span("Ornstein-Uhlenbeck with foraging (OUF) model",
               class = "cl-grn"),
          wrap_none(HTML("&#x2014"), ","),
          "the extracted characteristic timescales",
          paste0("(", span("position", class = "cl-sea-l"),
                 ", and ", span("velocity autocorrelation",
                                class = "cl-sea-l"),
                 ")"), "and other measures",
          paste0("(", span("location", class = "cl-sea-l"),
                 " and ", span("velocity variance", class = "cl-sea-l"),
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
          "be as large as possible; but for GPS devices, there is",
          "a tradeoff, as we will see in the next tab."
        )))
      
      ## Device tab: ------------------------------------------------------
      
      # element <- c(element, "#group_design")
      
      tab4 <- paste0("#tab_design_1", "-")
      element <- c(element, paste0(tab4, "dev_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "Now we are ready to evaluate the intended",
          span("sampling design", class = "cl-sea-l"),
          "for a future tracking project by simulating data",
          "conditioned upon the previously extracted parameters.",
          p(),
          
          span(class = "tour_action", action_bell,
               
               "Choose GPS/Satellite logger as",
               "your tracking device."),
          
          p(),
          "With these devices",
          "sampling is", span("automated", class = "cl-sea-l"),
          "and conducted by satellite systems. Therefore,",
          span("sampling duration", class = "cl-grn"),
          "and", span("sampling interval", class = "cl-grn"),
          "are both inherently linked with",
          HTML(paste0(span("GPS battery life",
                           class = "cl-sea-l"), ".")),
          "This tradeoff restricts the volume of data that",
          "can be collected."
        )))
      
      element <- c(element, paste0(tab4, "devBox_gps_device"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, we can set the maximum",
          span("GPS battery life", class = "cl-sea-l"),
          "(how long the GPS is expected to last),", "and the",
          span("maximum GPS fix rate", class = "cl-sea-l"),
          "(the longest time interval between fixes available",
          "for the duration above).",
          p(),
          "Currently (with the default inputs), we have a GPS",
          "model with a battery that will last 12 months",
          "if a new location (fix) is recorded every day.",
          p(),
          span(class = "tour_action", action_bell,
               text_device())
        )))
      
      element <- c(element, paste0(tab4, "devBox_sampling"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, we plot the",
          span("sampling duration", class = "cl-grn"),
          em("versus"), span("sampling interval", class = "cl-grn"),
          "(time between when new fixes or locations are collected).",
          "As sampling interval decreases (shorter time between",
          "new fixes), the battery",
          "life also decreases until it approaches",
          wrap_none(span("zero", class = "cl-dgr"), "."),
          "The plot has been set to logarithmic scale",
          "for a clearer view of this tradeoff.",
          p(),
          
          span(
            class = "tour_action", action_bell,
            "Select the point in the figure corresponding to",
            "the sampling interval to",
            span("2 hours", class = "cl-sea-l"),
            "(equal to a frequency of",
            wrap_none(
              span("12 fixes per day", class = "cl-sea-l"), ")."))
        )))
      
      element <- c(element, paste0(tab4, "devBox_sampling"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action", action_bell,
            
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
      
      element <- c(element, paste0(tab4, "devBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          "As mentioned earlier, we focus on both",
          span("effective sample sizes", class = "cl-grn"),
          HTML("&#x2014"), "for home range estimation",
          wrap_none("(", HTML("N<sub>area</sub>"), ")"),
          "and speed & distance estimation",
          wrap_none("(", HTML("N<sub>speed</sub>"), ")."),
          text_sizes()
        )))
      
      element <- c(element, paste0(tab4, "devBox_sims"))
      intro <- c(
        intro,
        HTML(paste(
          "In the", fontawesome::fa("location-dot", fill = pal$sea),
          span("Species", class = "cl-sea-l"), "we plot the new",
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
      
      # element <- c(element, paste0(tab4, "devBox_sizes"))
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     span(
      #       class = "tour_action", action_bell,
      #       "Throughout this walkthrough, you can click on the", 
      #       fontawesome::fa("bookmark"), "'Show table'", 
      #       "button to show relevant inputs and outputs",
      #       "in a table for a quick overview."),
      #     p(),
      #     "If you set different sampling parameters,",
      #     "you can also check them here.",
      #     "Keep in mind that for further analyses, the application",
      #     "will use the last set of parameters/data simulated."
      #   )))
      
      element <- c(element, paste0(tab4, "devBox_summary"))
      intro <- c(
        intro,
        HTML(paste(
          "Here are the outputs from the",
          fontawesome::fa("stopwatch", fill = pal$sea),
          span("Sampling design", class = "cl-sea-l"), "tab.",
          "Now we are ready to move on to",
          span("home range", class = "cl-sea-l"), "estimation."
        )))
      
      ## Analyses tab: ----------------------------------------------------
      ### Home range ------------------------------------------------------
      
      # element <- c(element, "#analyses")
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #  
      #   )))
      
      tab5 <- paste0("#tab_hrange_1", "-")
      element <- c(element, paste0(tab5, "hr_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "For this step, we are using the",
          span("Autocorrelated Kernel Density Estimator (AKDE)",
               class = "cl-sea-l"), "method available in the",
          span("ctmm", class = "cl-grn"),
          "R package.",
          
          span(
            class = "tour_action", action_bell,
            
            "Click on the", fontawesome::fa("paper-plane"), 
            "'Run estimation' button to estimate home range area.")
        )))
      
      # element <- c(element, paste0(tab5, "hrBox_viz"))
      element <- c(element, "#hr_outputs")
      intro <- c(
        intro,
        HTML(paste(
          "This is the visual output from the",
          span("home range", class = "cl-sea"), "estimation.",
          "You want the point estimate (in ", wrap_none(
            span("blue", class = "cl-sea"), end = "),"), 
          "to be similar to the true home range (in grey),",
          "and the 95% CIs to 'hug' the point estimate",
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
            "of the area, followed by the 95% confidence intervals",
            " (low", HTML("&#x2014"), "high), ",
            "as the point estimate is subject to uncertainty."), 
          p(),
          fontawesome::fa(name = "radiation", fill = "#bdbdbd"),
          wrap_none(
            span("Relative error", class = "cl-dgr-l"),
            ", which, for a single simulation", 
            " is the ", span("error (in %)", class = "cl-dgr-l"),
            " of the point estimate (and of the 95% CIs;",
            " once again, low", HTML("&#x2014"),
            "high) in relation to the expected value (truth)",
            " derived from the distribution.")
        )))
      
      element <- c(element, "#content_hr-areas")
      intro <- c(
        intro,
        HTML(paste(
          "Similarly to the plot, you want the error to be",
          wrap_none(span("low", class = "cl-sea-l"), end = ","),
          "and the CIs to 'hug' the point estimate",
          "as much as possible. If not, the point estimate is highly",
          "uncertain."
        )))
      
      element <- c(element, paste0(tab5, "hrBox_sizes"))
      intro <- c(
        intro,
        HTML(paste(
          text_hr()
        )))
      
      # element <- c(element, paste0(tab5, "hrBox_viz"))
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     span(
      #       class = "tour_action", action_bell,
      #       "You can once again see all outputs to a table",
      #       "by clicking on the", fontawesome::fa("bookmark")
      #       , "'Show table'", "button located at the bottom",
      #       "of the box."
      #     )
      #   )))
      
      # element <- c(element, paste0(tab5, "hrBox_summary"))
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     "Here are the outputs from the",
      #     fontawesome::fa("map-location-dot", fill = pal$sea),
      #     span("Home range", class = "cl-sea-l"), "tab.",
      #     "Now we are ready to move on to",
      #     span("speed & distance", class = "cl-sea-l"), "estimation."
      #   )))
      
      element <- c(element, "#Tour_middle")
      intro <- c(
        intro,
        HTML(paste(
          "Now we are ready to move on to",
          span("speed & distance", class = "cl-sea-l"), "estimation."
        )))
      
      ### Speed & distance ------------------------------------------------
      
      tab6 <- paste0("#tab_ctsd_1", "-")
      element <- c(element, paste0(tab6, "sd_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "For this step, we are using the",
          span("Continuous Time Speed & Distance (CTSD)",
               class = "cl-sea-l"), "estimator available in the",
          span("ctmm", class = "cl-grn"),
          "R package.", p(),
          span(class = "tour_action", action_bell,
               
               "Click on the", fontawesome::fa("paper-plane"),
               "'Run estimation' button to estimate speed & distance.")
        )))
      
      # element <- c(element, paste0(tab6, "sdPlot_path"))
      element <- c(element, "#sd_outputs")
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
            span("Relative error", style = "color: #bdbdbd;"), ","),
          "for the current sampling design."
        )))
      
      element <- c(element, paste0(tab6, "sdPlot_speed"))
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
          text_ctsd()
        )))
      
      # element <- c(element, paste0(tab6, "sdBox_outputs"))
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     span(class = "tour_action", action_bell,
      #          "You can once again see all outputs to a table",
      #          "by clicking on the", fontawesome::fa("bookmark")
      #          , "'Show table'", "button located at the bottom",
      #          "of the box."
      #     )
      #   )))
      # 
      # element <- c(element, paste0(tab6, "sdBox_summary"))
      # intro <- c(
      #   intro,
      #   HTML(paste(
      #     "Here are the outputs from the",
      #     fontawesome::fa("gauge-high", fill = pal$sea),
      #     span("Speed & distance", class = "cl-sea-l"), "tab.",
      #     "Now we are ready to move on to the",
      #     span("Report", class = "cl-sea-l"), "tab."
      #   )))
      
      ## Report tab: ------------------------------------------------------
      
      tab7 <- paste0("#tab_report_1", "-")
      element <- c(element, paste0(tab7, "repBox_details"))
      intro <- c(
        intro,
        HTML(paste(
            "Here, you can generates a comprehensive summary",
            "of all study design outputs. This report consolidates",
            "key findings, highlighting how our current sampling",
            "effort affects estimation accuracy."
        )))
      
      element <- c(element, paste0(tab7, "repBox_details"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action", action_bell,
            
            "Click the", fontawesome::fa("bookmark"),
            "'Build report' button to see a detailed",
            "assessment of how well your current sampling design",
            "addresses your research targets.")
        )))
      
      element <- c(element, paste0(tab7, "repBox_analyses"))
      intro <- c(
        intro,
        HTML(paste(
          p(),
          "In this case,",
          "is the", span("sampling duration", class = "cl-sea-l"),
          "sufficient to obtain a reliable",
          span("home range area", class = "cl-sea-l"), "estimate?",
          "And is the", span("sampling interval", class = "cl-sea-l"),
          "sufficient to obtain a reliable",
          span("speed & distance", class = "cl-sea-l"), "estimate?",
          p(),
          "You can also compare your results to our",
          "simulations with similar",
          span("timescale", class = "cl-sea-l"), "and",
          span("sampling", class = "cl-sea-l"), "parameters."
        )))
      
      # element <- c(element, paste0(tab7, "repPlot_precision"))
      element <- c(element, "#section-two_questions")
      intro <- c(
        intro,
        HTML(paste(
          "To interpret these plots: given 400 simulations",
          " of a movement process with \u03C4\u209A = 8 days",
          "(for a duration of 256 days), the expected errors",
          "of AKDE estimates have a 95% probability of",
          "falling within the range shown (shaded area in",
          wrap_none(span("blue", class = "cl-sea"), ")."), p(),
          "Even though our current error is only 2.9%, uncertainty",
          "is still high, i.e., similar simulation parameters",
          "resulted in an under- or overestimation of the home",
          "range area up to 20-40%)."
        )))
      
      element <- c(element, "#section-two_questions")
      intro <- c(
        intro,
        HTML(paste(
          "For speed & distance estimation, given 400 simulations",
          "of a movement processes with \u03C4\u1D65 = 1 hour",
          "(and a sampling interval of one fix every 1.5 hours),",
          "the expected errors",
          "of CTSD estimates have a 95% probability of",
          "falling close to zero (shaded area in",
          wrap_none(span("green", class = "cl-grn"), ").")
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
    
    ## Build 'Population estimate' tour: ----------------------------------
    
    .build_tour_pop <- function(ns, rv) {
      action_bell <- wrap_none("(", fontawesome::fa("bell"), ")")
      element <- intro <- character(0)
      
      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          p(class = "ttl_tour cl-wht",
            
            "Welcome to",
            wrap_none("move", span("design", class = "cl-sea-l"))),
          
          "Here, you will be guided step-by-step through the",
          span(
            wrap_none("move", span("design",
                                   class = "txt_mono cl-sea-l")),
               class = "txt_code inline"),
          "application. At certain points, you will be prompted to",
          "perform specific actions, which will be clearly",
          span(class = "tour_action", 
                 action_bell, "highlighted."),
          "Follow these instructions carefully,",
          "as later parts of the tutorial require that all",
          "previous steps were performed.",
          "If no action is requested, simply proceed",
          "to the next step.", p(),
          
          span(class = "tour_action", action_bell,
               "Click",
               span("Next", style = paste0(
                 "font: 13px/normal 'Roboto Condensed', sans-serif;",
                 "background-color: #00000061;",
                 "letter-spacing: 0.5px;",
                 "text-transform: uppercase;")),
               "or use the right arrow of your keyboard to proceed.")
        )))
      
      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          span("Warning:", class = "cl-dgr"),
          "do not interact with anything outside the highlighted",
          "zones created alongside the tutorial boxes.",
          p(),
          "This tutorial will",
          span("not", class = "cl-dgr"),
          "cover definitions in detail,",
          "but you can check out the more comprehensive",
          fontawesome::fa("circle-question", fill = "white"),
          "help tips at a later time."
        )))
      
      element <- c(element, "#Tour_start")
      intro <- c(
        intro,
        HTML(paste(
          "This workflow follows Silva <em>et al.</em> (in prep), and",
          "presents a simplified case study, designed for quick", 
          "execution.",
          "We will use a GPS tracking dataset of African buffalos",
          "(<em>Syncerus caffer</em>) tracked in Kruger National Park",
          "between 2005 and 2006 to inform our simulations.",
          p(),
          "Our primary goal here is to reliably estimate",
          span("mean home range area", class = "txt_bold cl-gld"),
          "of a population of African Buffalos.",
          "Check the", 
          span("vignette", class = "txt_bold cl-sea-l"), "or the",
          span("manuscript", class = "txt_bold cl-sea-l"),
          "for more detailed workflows,",
          "including both research targets."
        )))
      
      element <- c(element, "#content_workflow-first")
      intro <- c(
        intro,
        HTML(paste(
          span(class = "tour_action", action_bell,
               "Please choose",
               fontawesome::fa("square-check", fill = "white"),
               span("'Select'", class = "txt_code"), "as your",
               wrap_none(
                 span("data source", class = "txt_bold"), ", and"),
               fontawesome::fa("square-check", fill = "white"),
               span("'Home range'", class = "txt_code"), "as the",
               wrap_none(
                 span("research target", class = "txt_bold"), "."),
               "For the",
               wrap_none(
                 span("analytical target", class = "txt_bold"), ","),
               "choose",
               span("'Mean estimate of",
                    wrap_none(
                      span("sampled population",
                           class = "txt_mono cl-sea-l"),
                    "'"), class = "txt_code"),
               "to display deployment options.")
        )))
      
      element <- c(element, "#content_workflow-second")
      intro <- c(
        intro,
        HTML(paste(
          span(class = "tour_action", action_bell,
               "For",
               wrap_none(
                 span("deployment", class = "txt_bold"), ","),
               "select",
               span("'I plan to deploy a",
                    wrap_none(
                      span("set",
                           class = "txt_mono cl-grn"),
                      " number of VHF/GPS tags'."),
                    class = "txt_code"),
               "Then check the",
               span("'Add",
                    wrap_none(
                      span("individual",
                           class = "txt_mono cl-grn"),
                      " variation'"),
                    class = "txt_code"),
               "checkbox, as we wish to account for",
               "individual differences.")
        )))
      
      element <- c(element, ".sidebar")
      intro <- c(
        intro,
        HTML(paste(
          "The tabs necessary for this workflow are shown in order.",
          "First, we will go to the",
          fontawesome::fa("file-circle-plus", fill = pal$sea),
          span("Select data", class = "cl-sea-l"), "tab",
          "(to load a dataset from the",
          span(
            wrap_none(span("ctmm", class = "txt_mono cl-sea-l")),
            class = "txt_code inline"),
          "package), then the",
          fontawesome::fa("stopwatch", fill = pal$sea),
          span("Sampling design", class = "cl-sea-l"),
          "tab (to set the sampling duration and",
          "interval for evaluation), then the",
          fontawesome::fa("compass-drafting", fill = pal$sea),
          span("Analyses", class = "cl-sea-l"), "tab",
          "(to estimate home range areas), and finally the",
          fontawesome::fa("layer-group", fill = pal$sea),
          span("Meta-analyses", class = "cl-sea-l"), "tab",
          "(to estimate <em>mean</em> home range area).",
          p(),
          "Afterwards, we can see a detailed summary of",
          "all the outputs in the",
          fontawesome::fa("box-archive", fill = pal$sea),
          span("Report", class = "cl-sea-l"), "tab.",
          p(),
          
          span(class = "tour_action", action_bell,
               "Proceed to the next step",
               "as it will move you to the",
               fontawesome::fa("file-circle-plus", fill = pal$sea),
               span("Select data", class = "cl-sea-l"), "tab",
               "automatically.")
        )))
      
      ## Data tabs: -------------------------------------------------------
      
      tab3 <- paste0("#tab_data_select_1", "-")
      element <- c(element, paste0(tab3, "select_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "In this tab, you will be able to select one of the seven",
          "species available in the",
          span(
            wrap_none(span("ctmm", class = "txt_mono cl-sea-l")),
            class = "txt_code inline"), 
          span("R", class = "txt_code inline"), "package.",
          "Parameters extracted from one of the species provided",
          "can help inform tracking studies of",
          "species with similar", 
          paste0(span("movement behaviors", class = "cl-sea-l"), ".")
        )))
      
      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action", action_bell,
            "First, select the",
            span("African Buffalo", class = "txt_code inline"),
            wrap_none("(", em("Syncerus caffer"), ")"),
            "as your study species.",
            "Do not select any other options yet.")
        )))
      
      element <- c(element, "#comp_viz_selected-vizTabs_data")
      intro <- c(
        intro,
        HTML(paste(
          "These are the data visualization tabs. The",
          span("first tab", class = "cl-sea-l"),
          fontawesome::fa("paw", fill = pal$sea),
          "displays all individuals in the selected dataset.",
          "The", span("second tab", class = "cl-sea-l"),
          fontawesome::fa("filter", fill = pal$sea),
          "focuses on one individual at a time.",
          "The", span("third tab", class = "cl-sea-l"),
          fontawesome::fa("bug", fill = pal$sea), 
          "assists in identifying outliers (which should",
          "be removed and data re-uploaded).",
          "The", span("fourth tab", class = "cl-sea-l"),
          fontawesome::fa("chart-line", fill = pal$sea),
          "presents", span("variograms", class = "cl-grn"),
          "for the selected individuals."
        )))
      
      element <- c(element, paste0(tab3, "selectBox_species"))
      intro <- c(
        intro,
        HTML(paste(
          "You should ideally choose datasets and",
          "individuals that are representative of the",
          span("population", class = "cl-sea-l"),
          "you wish to emulate.", p(),
          span(
            class = "tour_action", action_bell,
            
            "Select all individuals from the dropdown menu",
            "and click the", fontawesome::fa("wand-magic-sparkles"),
            span("Validate", class = "txt_code inline"),
            "button."),
          p(),
          "Keep in mind that, for home range estimation,",
          "all selected individuals must meet",
          "the", span("range residency assumption", class = "cl-sea-l"),
          "(see the vignette for details).",
          p(),
          span(
            class = "tour_action", action_bell,
            "The button should now read",
            fontawesome::fa("circle-check"),
            span("Validated!", class = "txt_code inline"),
            wrap_none(span("Validated!", class = "txt_code inline"),
                      end = "."), "Now click the",
            fontawesome::fa("paper-plane"), 
            span("Extract", class = "txt_code inline"), "button.")
        )))
      
      element <- c(element, paste0(tab3, "selectBox_pars"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, you can see the movement process",
          HTML("&#x2014"), "in this case, the",
          span("Ornstein-Uhlenbeck with foraging (OUF) model",
               class = "cl-grn"),
          wrap_none(HTML("&#x2014"), ","),
          "the extracted characteristic timescales",
          paste0("(", span("position", class = "cl-sea-l"),
                 ", and ", span("velocity autocorrelation",
                                class = "cl-sea-l"),
                 ")"), "and other relevant measures",
          paste0("(", span("location", class = "cl-sea-l"),
                 " and ", span("velocity variance", class = "cl-sea-l"),
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
          "of the original dataset. The",
          span("absolute sample size (n)", class = "cl-grn"),
          "represents the total number of locations.",
          "The more critical information comes from the",
          span("effective sample sizes (N)", class = "cl-grn"),
          "for", span("home range", class = "cl-sea-l"), "and",
          span("speed & distance", class = "cl-sea-l"),
          "estimation. We want the effective sample sizes to",
          "be as large as possible."
        )))
      
      ## Device tab: ------------------------------------------------------
      
      tab4 <- paste0("#tab_design_1", "-")
      element <- c(element, paste0(tab4, "dev_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "Now we are ready to evaluate the intended",
          span("sampling design", class = "cl-sea-l"),
          "for a future tracking project by simulating data",
          "conditioned upon the previously extracted parameters.",
          p(),
          
          span(class = "tour_action", action_bell,
               
               "Choose GPS/Satellite logger as",
               "your tracking device.")
        )))
      
      element <- c(element, "#content_device-limitations")
      intro <- c(
        intro,
        HTML(paste(
          "We will also incorporate three additional components:",
          span("fix success rate", class = "txt_bold cl-grn"),
          "(the proportion of successful fixes relative to all",
          "attempted fixes, reflecting the reliability of the GPS",
          "signal),", span("tag failure", class = "txt_bold cl-grn"),
          "(the probability that a GPS tag will fail",
          "at some point during data collection),",
          "and", span("location error", class = "txt_bold cl-grn"),
          "(the positional uncertainty associated with each",
          "recorded location).", p(),
          
          span(class = "tour_action", action_bell,
               "Enable all three settings by checking",
               "the corresponding boxes.")
        )))
      
      element <- c(element, paste0(tab4, "devBox_gps_device"))
      intro <- c(
        intro,
        HTML(paste(
          span(class = "tour_action", action_bell,
               "Uncheck the",
               span("Select from plot", class = "txt_code inline"),
               "checkbox, which will allow us to manually input",
               "the sampling interval."),
          p(),
          "For this tutorial, we will use the following",
          "sampling schedule: a <em>sampling duration</em> of",
          wrap_none(span("3 months", class = "txt_code"),
                    end = ","),
          "with 12 new locations collected per day",
          "<em>sampling interval</em> of",
          wrap_none(span("2 hours", class = "txt_code"), ")."),
          p(),
          span(class = "tour_action", action_bell,
               "First, set",
               span("GPS battery life", class = "txt_code inline"),
               "(equivalent to the",
               "maximum sampling duration) to",
               wrap_none(span("3 months", class = "txt_code"), "."))
        )))
      
      element <- c(element, paste0(tab4, "devBox_sampling"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, we set the <em>sampling interval</em>",
          "(time between when new fixes or locations are collected).",
          p(),
          span(class = "tour_action", action_bell,
               "Set",
               span("What sampling interval will you evaluate?",
               class = "txt_code inline"), "to",
               wrap_none(span("2 hours", class = "txt_code"), "."))
        )))
      
      element <- c(element, "#content_limitations")
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action", action_bell,
            "Set fix success rate to",
            wrap_none(span("85%", class = "txt_code"), ","),
            "tag failure to",
            wrap_none(span("5%", class = "txt_code"), ","),
            "and location error",
            wrap_none(span("15 meters", class = "txt_code"), "."))
        )))
      
      element <- c(element, paste0(tab4, "devBox_sampling"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action", action_bell,
            
            "Click the",
            fontawesome::fa("wand-magic-sparkles"),
            span("Validate", class = "txt_code inline"),
            "button, wait until it reads",
            wrap_none(span("Validated!", class = "txt_code inline"),
                      end = ","), "then click the",
            fontawesome::fa("paper-plane"), 
            span("Run", class = "txt_code inline"), "button."),
          p(),
          "You will see two notifications: the first during",
          "the calculation of the expected run time,",
          "and the second during the simulation and model",
          "fitting process (conditioned on the current parameters).",
          "Please wait for both to finish before proceeding."
        )))
      
      element <- c(element, paste0(tab4, "devBox_summary"))
      intro <- c(
        intro,
        HTML(paste(
          "Here are the outputs from the",
          fontawesome::fa("stopwatch", fill = pal$sea),
          span("Sampling design", class = "cl-sea-l"), "tab.",
          "Now we are ready to move on to",
          span("home range", class = "cl-sea-l"), "estimation."
        )))
      
      ## Analyses tab: ----------------------------------------------------
      ### Home range ------------------------------------------------------
      
      tab5 <- paste0("#tab_hrange_1", "-")
      element <- c(element, paste0(tab5, "hr_intro"))
      intro <- c(
        intro,
        HTML(paste(
          "For this step, we are using the",
          span("Autocorrelated Kernel Density Estimator (AKDE)",
               class = "cl-sea-l"), "method available in the",
          span(
            wrap_none(span("ctmm", class = "txt_mono cl-sea-l")),
            class = "txt_code inline"), 
          span("R", class = "txt_code inline"), "package.", p(),
          
          span(
            class = "tour_action", action_bell,
            
            "Click on the", fontawesome::fa("paper-plane"), 
            span("Run estimation", class = "txt_code inline"),
            "button to estimate home range area.")
        )))
      
      element <- c(element, "#hr_outputs")
      intro <- c(
        intro,
        HTML(paste(
          "We can now see the outputs for a single simulation,",
          "providing a starting point.",
          "You want the point estimate (in ", wrap_none(
            span("blue", class = "txt_bold cl-sea"), "),"), 
          "to be similar to the true home range (in",
          wrap_none(
            span("grey", class = "txt_bold cl-grey"), "),"),
          "and the 95% CIs to 'hug' the point estimate",
          "as much as possible. To show the 95% CIs, click",
          "the corresponding grey buttons.",
          p(),
          "If the CIs are wide, the estimate is highly",
          "uncertain, and your study may require a longer",
          wrap_none(
            span("sampling duration", class = "cl-grn"), "."),
          "However, we can refine this assessment further by",
          "running more simulations in the next steps."
        )))
      
      element <- c(element, "#content_hr-areas")
      intro <- c(
        intro,
        HTML(paste(
          "To support evaluation, two sets of values",
          "are provided:", p(),
          fontawesome::fa(name = "bullseye", fill = "#bdbdbd"),
          wrap_none(
            span("Estimate", class = "txt_code inline"),
            ", which is the ",
            span("point estimate", class = "cl-sea-l"),
            " of the area, followed by its 95% confidence",
            " intervals (low", HTML("&#x2014"), "high)."), 
          p(),
          fontawesome::fa(name = "radiation", fill = "#bdbdbd"),
          wrap_none(
            span("Relative error", class = "txt_code inline"),
            ", which, for a single simulation", 
            " is the ", span("error (in %)", class = "cl-dgr-l"),
            " of the point estimate (and of the 95% CIs)",
            " compared to the expected (true) value derived",
            " from the distribution.")
        )))
      
      tab6 <- paste0("#comp_m_in_hr", "-")
      element <- c(element, paste0(tab6, "mBox_nsims"))
      intro <- c(
        intro,
        HTML(paste(
          "Here, we are able to set the",
          span("total number of tags", class = "cl-grn txt_bold"),
          "(refers to the number of tracking units deployed",
          "during a study), and the",
          span("error threshold", class = "cl-grn txt_bold"),
          "(used to assess whether the estimates fall within",
          "acceptable range from the expected values).",
          p(),
          
          span(
            class = "tour_action", action_bell,
            
            "Set the total number of tags to",
            span("14", class = "txt_code inline"),
            "and the error threshold at",
            wrap_none(
              span("5%", class = "txt_code inline"), end = ","),
            "before clicking the", fontawesome::fa("bolt"),
            span("Simulate", class = "txt_code inline"),
            "button."),
          p(),
          
          "A message will appear showing the",
          "expected runtime. Wait for the process to finish",
          "before proceeding."
        )))
      
      element <- c(element, "#hr_outputs")
      intro <- c(
        intro,
        HTML(paste(
          "You can now explore all the outputs,",
          "using the",
          span("Show simulation no.", class = "txt_code inline"),
          "slider. This allows you to see how",
          span("home range estimates", class = "cl-sea-l"),
          "vary across simulated individuals."
        )))
      
      element <- c(element, "#Tour_middle")
      intro <- c(
        intro,
        HTML(paste(
          "Now we are ready to move on to the",
          span("Meta-analyses", class = "cl-sea-l"), "tab."
        )))
      
      ## Meta-analyses tab: -----------------------------------------------
      
      tab7 <- paste0("#tab_meta_1", "-")
      element <- c(element, paste0(tab7, "meta_intro"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action", action_bell,
            
            "Click on the", fontawesome::fa("paper-plane"), 
            span("Run meta-analyses", class = "txt_code inline"),
            "button to obtain information on",
            "population-level inferences using all",
            span("14", class = "txt_code inline"),
            "simulated individuals.")
        )))
      
      element <- c(element, paste0(tab7, "metaBox_err_hr"))
      intro <- c(
        intro,
        HTML(paste(
          "This box displays the relative error in the",
          span("mean home range area estimate",
               class = "cl-gld txt_bold"),
          "along with the 95% confidence intervals.",
          "In addition, two plots are generated: one",
          "showing the <em>individual</em> estimates of home range",
          " areas, and the other the <em>population-level</em>",
          " estimates of home range areas."
        )))
      
      element <- c(element, paste0(tab7, "metaBox_outputs"))
      intro <- c(
        intro,
        HTML(paste(
          "The first plot displays the estimated",
          span("home range areas", class = "cl-sea-l"),
          "(x-axis, in km\u00B2) for each individual (y-axis),",
          "along with their 95% confidence intervals.",
          p(),
          "The black square represents the",
          span("mean", class = "cl-sea-l txt_italic"),
          span("population estimate", class = "cl-sea-l"),
          "(across all individuals) with its 95%",
          "confidence interval. The vertical solid line indicates the",
          "expected", span("true", class = "txt_bold"),
          "value for the current species parameters."
        )))
      
      element <- c(element, paste0(tab7, "metaBox_summary"))
      intro <- c(
        intro,
        HTML(paste(
         "The second plot illustrates how varying the number",
         "of individuals (from a <em>population</em> sample size of",
         span("2", class = "txt_code inline"),
         "to a maximum of",
         wrap_none(span("14", class = "txt_code inline"), "),"),
         "influences population-level mean estimates.",
         p(),
         "Each point represents the mean relative",
         "error (%) of this metric, along with its 95%",
         "confidence intervals, plotted against the number of",
         "tracked individuals. Dashed horizontal lines",
         "indicate the predefined error threshold of",
         wrap_none(
           span("5%", class = "txt_code inline"), "."),
         p(),
         "An accompanying table provides detailed information,",
         "as well as whether a sub-population was detected at",
         "each population sample size."
        )))
      
      element <- c(element, paste0(tab7, "metaBox_summary"))
      intro <- c(
        intro,
        HTML(paste(
          "To assess the spread of estimates, we can resample",
          "from the total population through",
          wrap_none(span("combination testing",
                         class = "cl-grn txt_bold"), "."),
          "This involves randomly reassigning individuals into new",
          "sets and rerunning the estimation of",
          span("mean", class = "cl-sea-l txt_italic"),
          span("home range areas", class = "cl-sea-l"),
          "for each new set.",
          p(),
          span(
            class = "tour_action", action_bell,
            "Set the resamples to",
            span("15", class = "txt_code inline"),
            "then click the",
            fontawesome::fa("wand-magic-sparkles"),
            span("Resample", class = "txt_code inline"), "button.",
            "Note that higher values are recommended when",
            "simulating larger population sample sizes.")
        )))
      
      element <- c(element, paste0(tab7, "metaBox_summary"))
      intro <- c(
        intro,
        HTML(paste(
          "The new plot illustrates how different combinations",
          "of individuals shape the observed",
          span("mean", class = "cl-sea-l txt_italic"),
          span("estimates", class = "cl-sea-l"),
          "across increasing population sample sizes.", p(),
          "Variation remains high,",
          "suggesting that additional simulations could help stabilize",
          "the mean estimate of home range areas."
        )))
      
      element <- c(element, "#Tour_middle")
      intro <- c(
        intro,
        HTML(paste(
          "Now we are ready to move on to the",
          span("Report", class = "cl-sea-l"), "tab."
        )))
      
      ## Report tab: ------------------------------------------------------
      
      tab7 <- paste0("#tab_report_1", "-")
      element <- c(element, paste0(tab7, "repBox_details"))
      intro <- c(
        intro,
        HTML(paste(
          span(
            class = "tour_action", action_bell,
            
            "Click the", fontawesome::fa("bookmark"),
            span("Build report", class = "txt_code inline"),
            "button to generate a detailed assessment of how",
            "well the current sampling strategy",
            "aligns with the set research targets.")
        )))
      
      element <- c(element, paste0(tab7, "repBox_meta"))
      intro <- c(
        intro,
        HTML(paste(
          p(),
          "Here, a",
          "<em>population</em> sample size of",
          span("14", class = "txt_code inline"),
          "individuals does not",
          "meet the", span("5%", class = "txt_code inline"),
          "error threshold for the",
          wrap_none(span("mean home range area",
                         class = "cl-gld txt_bold"), ","),
          "indicating that this sampling stragegy at",
          "this population sample size is insufficient",
          "to produce reliable estimates."
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
      
    } # end of main tour (populations)
    
    # OPERATIONS --------------------------------------------------------
    
    # Allow dynamic UI actions if tour is active:
    
    observe({
      req(rv$active_tab)
      if (rv$active_tab == 'about') rv$tour_active <- FALSE
    })
    
    # If 'guided tour' (single) button is clicked:
    
    observe({
      rv$tour_active <- TRUE
      
      tour <- .build_tour(ns, rv)
      rintrojs::introjs(
        session = session,
        options = list(steps = tour,
                       showStepNumbers = FALSE,
                       showButtons = TRUE,
                       showBullets = TRUE,
                       scrollToElement = TRUE),
        events = list(onbeforechange = 
                        rintrojs::readCallback("switchTabs")))
      
    }) %>% # end of observe,
      bindEvent(input$default_tour)
    
    # If 'guided tour' (population) button is clicked:
    
    observe({
      rv$tour_active <- TRUE

      tour <- .build_tour_pop(ns, rv)
      rintrojs::introjs(
        session = session,
        options = list(steps = tour,
                       showStepNumbers = FALSE,
                       showButtons = TRUE,
                       showBullets = TRUE,
                       scrollToElement = TRUE),
        events = list(onbeforechange =
                        rintrojs::readCallback("switchTabs")))

    }) %>% # end of observe,
      bindEvent(input$default_tour_pop)
    
  }) # end of moduleServer
}

## To be copied in the UI
# mod_comp_tour_ui("comp_tour_1")

## To be copied in the server
# mod_comp_tour_server("comp_tour_1")
