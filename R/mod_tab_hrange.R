#' tab_hrange UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_hrange_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

      ## Introduction: ----------------------------------------------------

      div(class = div_column_main,

          shinydashboardPlus::box(

            title = span("Home range estimation:", style =
                           paste("padding-top: 14px;", ttl_main)),
            icon = fontawesome::fa(name = "map-marked-alt",
                                   height = "22px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "#e3e3e3"),
            id = ns("hr_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = TRUE, closable = FALSE,

            column(
              align = "center", width = 12,

              p(span("Home range", style = txt_key),
                "is the area repeatedly used throughout an animal's",
                "lifetime for all its normal behaviors and activities,",
                "excluding occasional exploratory excursions.",
                "As animal movement",
                "is inherently", span("autocorrelated", style = txt_key),
                "(locations are similar as a function of space and",
                " distance), the",
                span("Autocorrelated Kernel Density Estimators (AKDEs)",
                     style = txt_border),
                "are the most appropriate method for",
                span("home range", style = txt_key), "estimation."),

              p(style = ft_center,
                "If home range estimation is is your goal,",
                "then click the",
                fontawesome::fa(name = "paper-plane", fill = hex_main),
                HTML(paste0(span("Run estimation", style = btn_primary))),
                "button."),

              splitLayout(
                cellWidths = c("38px", "1%", "200px"),
                cellArgs = list(style = 'align: center;'),

                shiny::actionButton(
                  inputId = ns("hrHelp_akde"),
                  label = NULL,
                  width = "100%",
                  icon = icon("question-circle"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("run_hr"),
                  label = "Run estimation",
                  icon =  icon("paper-plane"),
                  width = "100%",
                  class = "btn-primary")
              ),
              br()

            ) # end of column (for text)
          ), # end of box // hr_intro

          uiOutput(ns("hrInput_show_all"))

      ), # end of div (top row)

      # [right column] ----------------------------------------------------

      div(class = div1_column_right,

          ## Tracking regime: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Tracking regime", style = ttl_box.solid),
            id = ns("hrBox_regime"),
            status = "info",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("hrTabs_regime"),

              tabPanel(
                value = ns("hrPanel_regime"),
                title = fontawesome::fa(name = "map-marker-alt",
                                        fill = hex_border),
                p(),
                fluidRow(
                  column(width = 12, uiOutput(ns("hrInfo_dur"))),
                  column(width = 12, uiOutput(ns("hrInfo_dti")))
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("hrPanel_regime_new"),
                title = fontawesome::fa(name = "bolt",
                                        fill = hex_caution),
                p(),
                fluidRow(
                  column(width = 12, uiOutput(ns("hrInfo_dur_new"))),
                  column(width = 12, uiOutput(ns("hrInfo_dti_new")))
                ), # end of fluidRow

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12, align = "center",

              splitLayout(
                cellWidths = c("29%", "1%", "70%"),
                cellArgs = list(style = "align: center;"),

                shiny::actionButton(
                  inputId = ns("hrHelp_regime"),
                  label = NULL,
                  width = "100%",
                  icon = icon("question-circle"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("hr_adjRegime"),
                  label = "Modify",
                  icon = icon("redo"), # wrench
                  class = "btn-info",
                  width = "100%")

              ) # end of splitLayout

            ) # end of column (footer)
          ), # end of box // hrBox_regime

          ## Home range area: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Home range area:", style = ttl_box),
            id = ns("hrBox_areas"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("hrTabs_area"),

              tabPanel(
                value = ns("hrPanel_area"),
                title = fontawesome::fa(name = "map-marker-alt",
                                        fill = hex_border),

                uiOutput(ns("hrInfo_est")),
                uiOutput(ns("hrInfo_err")),
                p()

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("hrPanel_area_new"),
                title = fontawesome::fa(name = "bolt",
                                        fill = hex_caution),

                uiOutput(ns("hrInfo_est_new")),
                uiOutput(ns("hrInfo_err_new")),
                p()

              ) # end of panels (2 out of 2)
            ), # end of tabs

            footer = column(
              width = 12, align = "center",

              splitLayout(
                cellWidths = c("29%", "1%", "70%"),
                cellArgs = list(style = "align: center;"),

                shiny::actionButton(
                  inputId = ns("hrHelp_bias"),
                  label = NULL,
                  width = "100%",
                  icon = icon("question-circle"),
                  class = "btn-warning"),
                br(),
                shiny::actionButton(
                  inputId = ns("show_ctsdTable"),
                  label = "Table",
                  icon = icon("list"),
                  width = "100%",
                  class = "btn-primary")
              ) # end of splitLayout

            ) # end of column (footer)
          ) # end of box // hrBox_areas

      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(class = div1_column_left,

          ## Sample sizes: ------------------------------------------------

          shinydashboardPlus::box(
            title = span("Sample sizes:", style = ttl_box),
            id = ns("hrBox_sizes"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,

            tabsetPanel(
              id = ns("hrTabs_size"),

              tabPanel(
                value = ns("hrPanel_size"),
                title = tagList(
                  fontawesome::fa(name = "map-marker-alt",
                                  fill = hex_border),
                  span("Data", style = ttl_panel)
                ),

                fluidRow(
                  column(width = 6, uiOutput(ns("hrBlock_n"))),
                  column(width = 6, uiOutput(ns("hrBlock_Narea"))),
                ) # end of fluidRow

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("hrPanel_size_new"),
                title = tagList(
                  fontawesome::fa(name = "bolt", fill = hex_caution),
                  span("Conditional simulation", style =
                         paste0(ttl_panel, col_caution))
                ),

                fluidRow(
                  column(width = 6, uiOutput(ns("hrBlock_n_new"))),
                  column(width = 6, uiOutput(ns("hrBlock_Narea_new"))),
                ) # end of fluidRow

              ) # end of panels (2 out of 2)
            ) # end of tabs
          ), # end of box // hrBox_sizes

          ## Home range plot: ---------------------------------------------

          shinydashboardPlus::box(
            title = span("Plotting home range:", style = ttl_box),
            id = ns("hrBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,

            tabsetPanel(
              id = ns("hrTabs_viz"),

              tabPanel(
                value = ns("hrPanel_viz"),
                title = tagList(
                  fontawesome::fa(name = "map-marker-alt",
                                  fill = hex_border),
                  span("Data", style = ttl_panel)
                ),

                ggiraph::girafeOutput(
                  outputId = ns("hrPlot_main"),
                  width = "100%", height = "100%"),

                shinyWidgets::checkboxGroupButtons(
                  inputId = ns("hrShow_levels"),
                  label = "Show estimate levels:",
                  choices = c("95% low CI",
                              "Estimate",
                              "95% high CI"),
                  selected = "Estimate",
                  checkIcon = list(yes = icon("check-circle")),
                  justified = TRUE)

              ), # end of panels (1 out of 2)

              tabPanel(
                value = ns("hrPanel_viz_new"),
                title = tagList(
                  fontawesome::fa(name = "bolt", fill = hex_caution),
                  span("Conditional simulation", style =
                         paste0(ttl_panel, col_caution))
                ),

                ggiraph::girafeOutput(
                  outputId = ns("hrPlot_simulation"),
                  width = "100%", height = "100%"),
                # %>%
                #   shinycssloaders::withSpinner(
                #     proxy.height = "200px",
                #     type = getOption("spinner.type", default = 7),
                #     color = getOption("spinner.color",
                #                       default = "#f4f4f4")
                #   ),

                uiOutput(ns("hrText_sims")),

                column(
                  width = 12, align = "center",
                  span("Add the",
                       span("original locations",
                            style = paste0(txt_label_bold, col_border)),
                       "to the plot above:",
                       style = paste0(ft_center,
                                      txt_label_bold,
                                      col_main)),

                  shinyWidgets::switchInput(
                    inputId = ns("hrShow_datasets"),
                    onLabel = "Yes",
                    offLabel = "No",
                    labelWidth = "25px")
                ), # end of column

                shinyWidgets::checkboxGroupButtons(
                  inputId = ns("hrhrShow_levels_sim"),
                  label = span("Show estimate levels:",
                               style = txt_label_bold),
                  choices = c("95% low CI",
                              "Estimate",
                              "95% high CI"),
                  selected = "Estimate",
                  checkIcon = list(yes = icon("check-circle")),
                  justified = TRUE)

              ) # end of panels (2 out of 2)
            ) # end of tabs
          ) # end of box // hrBox_viz

      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          ## Table: -------------------------------------------------------

          shinydashboardPlus::box(
            title = span("Table:", style = ttl_box),
            id = ns("hrBox_summary"),
            width = NULL,
            solidHeader = FALSE,

            DT::dataTableOutput(ns("hrTable")),
            p(),
            uiOutput(ns("show_errScale"))

          ), # end of box // hrBox_summary

          ## Additional information: --------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", style = ttl_box),
            id = ns("hrBox_misc"),
            width = NULL,
            solidHeader = FALSE,

            # verbatimTextOutput(ns("hrInfo_console")),
            verbatimTextOutput(outputId = ns("time_simulate")),
            verbatimTextOutput(outputId = ns("time_hr"))

          ) # end of box
      ) # end of column (bottom)

    ) # end of fluidRow
  ) # end of tagList
}

#' tab_hrange Server Functions
#'
#' @noRd
mod_tab_hrange_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # DYNAMIC UI ELEMENTS -----------------------------------------------
    ## Hide secondary tabs at start: ------------------------------------

    vec <- c("regime", "area", "size", "viz")
    for(i in 1:length(vec)) {
      tmp_id <- paste0("hrTabs_", vec[i])
      tmp_target <- paste0("hrPanel_", vec[i], "_new")
      hideTab(inputId = tmp_id, target = ns(tmp_target))
    }

    tmpnames <- c("regime",
                  "sizes",
                  "viz",
                  "areas",
                  "summary",
                  "misc")

    for(i in 1:length(tmpnames)) {
      shinyjs::hide(id = paste0("hrBox_", tmpnames[i]))
    }

    ## Quick select all boxes after adjusting tracking regime: ----------

    observe({

      if(input$hrInput_show == 1) { tmp <- ""
      } else { tmp <- "_new" }

      tabs <- paste0("hrTabs_", vec)
      panels <- paste0("hrPanel_", vec)

      for(v in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[v]),
          selected = paste0("tab_hrange_1-", panels[v], tmp)) }

    }) %>% # end of observe.
      bindEvent(input$hrInput_show)

    # COMPUTATIONS ------------------------------------------------------
    # Fitting movement model (if needed): -------------------------------

    observe({
      req(vals$active_tab == 'hr')

      if(is.null(vals$data1)) {

        shinyalert::shinyalert(
          type = "error",
          title = "No data found",
          text = span(
            "Please go to the",
            fontawesome::fa(name = "map-marked-alt", fill = hex_main),
            span("Tracking regime", style = col_main), "tab",
            "and make sure to both (1) set a tracking regime, and",
            "(2) run a new simulation by pressing the",
            fontawesome::fa(name = "bolt", fill = hex_caution),
            span("'Run'", style = col_main), "button."
          ),
          html = TRUE,
          size = "xs")

      } else {

        if(vals$needs_fit) {

          msg_log(
            style = "danger",
            message = paste0("Model fit ",
                             msg_danger("not found"), "."),
            detail = "Please wait for 'ctmm.select()' to finish.")

          start <- Sys.time()
          newmod <- prepare_pars(
            tau_p0 = vals$tau_p0, tau_p0_units = vals$tau_p0_units,
            tau_v0 = vals$tau_v0, tau_v0_units = vals$tau_v0_units,
            sigma0 = vals$sigma0, sigma0_units = vals$sigma0_units)

          shiny::withProgress({
            fit0 <- ctmm::ctmm.fit(vals$data1, newmod)
          },
          message = "Fitting movement model.",
          detail = "This may take a while...")

          msg_log(
            style = 'success',
            message = paste0("Model fit ",
                             msg_success("completed"), "."),
            detail = paste(
              "This step took approximately",
              round(difftime(Sys.time(), start, units = 'mins'), 1),
              "minutes."))

          vals$guess <- NULL
          vals$newfit <- fit0
          vals$needs_fit <- FALSE

        } # end of if(vals$needs_fit)
      } # end of if(is.null(vals$data1))

    }) # end of observe

    observe({
      shinyjs::show(id = 'hrBox_regime')
      shinyjs::show(id = 'hrBox_sizes')
    }) %>% bindEvent(vals$newfit)

    # ADJUSTING TRACKING REGIME -----------------------------------------
    # Adjust sampling parameters necessary for simulation:

    observe({
      req(vals$dur0_dev, vals$dti0_dev)

      if(!is.null(vals$akde)) {

        # Sampling duration:

        dur <- round("days" %#% vals$dur0_dev, 0)
        tau_p0 <- round("days" %#% vals$tau_p0 %#% vals$tau_p0_units, 0)
        dur_choices <- c(
          10, dur, tau_p0, tau_p0 * 10,
          tau_p0 * 50, tau_p0 * 100, tau_p0 * 200,
          tau_p0 * 400, tau_p0 * 600, tau_p0 * 800
        ) %>% plyr::round_any(5, f = round) %>%
          unique() %>% sort()
        dur_choices <- dur_choices[dur_choices != 0]

        # Sampling interval:

        df_fixrate <- dplyr::arrange(gps_fixrate, desc(freq))
        value <- vals$dti0_dev
        index <- which.min(abs(df_fixrate$nu - value))
        dti_choices <- df_fixrate$nu_notes

        if(vals$data_type == "simulated") {
          tmprange <- NULL
        } else { tmprange <-
          paste(ifelse(vals$tau_p0_min == 0, "0",
                       scales::label_comma(
                         accuracy = .1)(vals$tau_p0_min)),
                "—", scales::label_comma(
                  accuracy = .1)(vals$tau_p0_max))
        }

        shiny::showModal(
          shiny::modalDialog(
            title = h4("Adjusting tracking regime:"),

            fluidRow(
              style = paste("margin-right: 40px;",
                            "margin-left: 40px;"),

              p("Here you can adjust",
                span("sampling parameters", style = txt_key),
                "to predict or simulate additional data from the",
                "same", span("movement model", style = txt_key),
                "of the species provided."),

              p("Based on your research question,",
                "we recommend that",
                span("sampling duration", style = txt_border),
                "is that least 10 times the",
                span(HTML(paste0("position autocorrelation ",
                                 "(\u03C4", tags$sub("p"), ")")),
                     style = txt_border), "value, to reduce bias",
                "of the conditional simulation.",
                span(style = col_black,
                     "These recommended values are already,",
                     "set below, so you can just click",
                     fontawesome::fa(name = "bolt",
                                     fill = hex_caution),
                     span("Run simulation",
                          style = paste(txt_tour, col_caution)),
                     "to proceed.")
              ),

              parBlock(
                text = span(
                  HTML(paste0("Position autocorrelation ",
                              "(\u03C4", tags$sub("p"), ")"))),
                header =
                  paste(scales::label_comma(
                    accuracy = .1)(vals$tau_p0),
                    vals$tau_p0_units),
                number = tmprange),

              shinyWidgets::sliderTextInput(
                inputId = ns('hr_dur0'),
                label = "Sampling duration (in days):",
                width = "100%",
                choices = dur_choices,
                selected = tau_p0 * 10 %>%
                  plyr::round_any(5, f = round),
                from_min = ifelse(dur > tau_p0 * 10,
                                  dur, tau_p0 * 10) %>%
                  plyr::round_any(5, f = round),
                from_max = ifelse(dur > tau_p0 * 800,
                                  dur, tau_p0 * 800) %>%
                  plyr::round_any(5, f = round)
              ),

              shinyWidgets::sliderTextInput(
                inputId = ns('hr_dti0'),
                label = "Sampling interval:",
                width = "100%",

                choices = dti_choices,
                selected = dti_choices[index],
                from_min = dti_choices[index],
                from_max = dti_choices[nrow(dti_choices)]),

              uiOutput(ns("hrText_sampling")),

              p(span("Proceed with caution!", style = txt_caution),
                "Longer sampling durations and lower sampling",
                "intervals will add run time to simulation, model",
                "fitting, and estimation functions."),

            ), # end of fluidRow

            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                inputId = ns("run_hr_new"),
                label = "Run simulation",
                icon =  icon("bolt"),
                class = "btn-danger")
            ),

            size = "m")) # end of modal

      } else {

        shinyalert::shinyalert(
          title = "Error",
          text = span(

            "First, estimate the home range",
            "based on the original dataset",
            "by clicking the",
            fontawesome::fa(name = "paper-plane", fill = hex_main),
            HTML(paste0(span("Run estimation", style = btn_primary))),
            "button."),

          html = TRUE,
          size = "xs")

      } # end of ifelse statement
    }) %>% bindEvent(input$hr_adjRegime)

    output$hrText_sampling <- renderUI({
      req(vals$data1, input$hr_dur0, input$hr_dti0)

      tmp <- gps_fixrate$nu[match(input$hr_dti0,
                                  gps_fixrate$nu_notes)]

      n_new <- length(
        seq(from = 1,
            to = input$hr_dur0 %#% "days",
            by = tmp))

      splitLayout(

        parBlock(text = "Number of original locations:",
                 header = scales::label_comma(
                   accuracy = 1)(nrow(vals$data1))),

        parBlock(text = "Number of new locations:",
                 header = span(scales::label_comma(
                   accuracy = 1)(n_new),
                   style = col_caution))

      ) # end of splitLayout
    }) # end of renderUI // hrText_sampling

    # HOME RANGE ESTIMATION ---------------------------------------------

    ## Estimating for initial tracking regime: --------------------------
    # Estimate home range after pressing the 'run_hr' button:

    observe({
      req(vals$is_valid)

      # Check if data is available:
      if(!is.null(vals$data1)) {

        tmplist <- list("hrBox_areas",
                        "hrBox_viz",
                        "hrBox_misc")

        for(i in 1:length(tmplist)) {
          shinyjs::show(id = tmplist[i])
        }

        req(vals$data1, vals$newfit, vals$tmpid, vals$id)
        if(vals$tmpid != vals$id) {

          shinyalert::shinyalert(
            title = "Oops!",
            text = span(
              "Data selected is from individual",
              HTML(paste0(span(vals$id, style = col_caution),
                          ",")), "but parameters are from",
              HTML(paste0(span(vals$tmpid, style = col_caution), ".")),
              br(),

              "Please extract parameters in the",
              fontawesome::fa(name = "paw", fill = hex_main),
              span("Data", style = col_main), "tab",
              "for the appropriate individual before",
              "estimating home range."),
            html = TRUE,
            size = "xs")

        } else {

          start <- Sys.time()
          shiny::withProgress({
            vals$akde <- ctmm::akde(data = vals$data1,
                                    CTMM = vals$newfit)
          },
          message = "Estimating home range.",
          detail = "This may take a while...")

          vals$hrEst <- summary(vals$akde)$CI[2]
          vals$hrEst_min <- summary(vals$akde)$CI[1]
          vals$hrEst_max <- summary(vals$akde)$CI[3]

          vals$time_hr <- difftime(Sys.time(), start,
                                   units = "mins")

        } # end of if() statement

        truth <- -2 * log(0.05) * pi * (vals$sigma0 %#%
                                          vals$sigma0_units)

        tempnames <- rownames(summary(vals$akde)$CI)
        units <- extract_units(
          tempnames[grep('^area', tempnames)])
        vals$hr_units <- units

        err <- ((vals$hrEst %#% units) - truth) / truth

        vals$hrErr <- err
        vals$hrErr_min <- ((vals$hrEst_min %#% units) -
                             truth) / truth
        vals$hrErr_max <- ((vals$hrEst_max %#% units) -
                             truth) / truth


      } else {

        shinyalert::shinyalert(
          type = "error",
          title = "No data found",
          text = span(
            'Please upload, select or simulate an', br(),
            span('movement dataset', style = col_caution),
            'first in the',
            fontawesome::fa(name = "paw", fill = hex_main),
            span('Data', style = col_main), "tabs."),
          html = TRUE,
          size = "xs")

      } # end of ifelse statement

    }) %>% # observeEvent
      bindEvent(input$run_hr)

    ## Estimating for new tracking regime: ------------------------------

    observe({

      # Show "conditional simulation" panels:
      for(i in 1:length(vec)) {
        tmp_id <- paste0("hrTabs_", vec[i])
        tmp_target <- paste0("hrPanel_", vec[i], "_new")
        showTab(inputId = tmp_id, target = ns(tmp_target))
      }

      tabs <- paste0("hrTabs_", vec)
      panels <- paste0("hrPanel_", vec)
      for(v in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0(tabs[v]),
          selected = paste0("tab_hrange_1-",
                            panels[v], "_new")) }

      # Capture new sampling duration and interval:
      vals$dti1_units <- sub('^.* ([[:alnum:]]+)$',
                             '\\1', input$hr_dti0)

      tmp <- gps_fixrate$nu[match(input$hr_dti0,
                                  gps_fixrate$nu_notes)]

      vals$dti1 <- vals$dti1_units %#% round(tmp, 0)
      vals$dur1 <- input$hr_dur0
      vals$dur1_units <- "days"

      ### 1. Simulate new dataset:
      # Fill in the gaps of original dataset + new duration:

      vals$conditional <- TRUE

      msg_log(
        style = "warning",
        message = "Simulating with new tracking regime.",
        detail = "This may take a while...")

      start <- begin <- Sys.time()
      df2 <- ctmm::simulate(
        vals$data0,
        vals$fit,
        t = seq(0,
                vals$dur1 %#% vals$dur1_units,
                by = vals$dti1 %#% vals$dti1_units))

      df2 <- ctmm:::pseudonymize(df2)
      df2$index <- 1:nrow(df2)
      vals$data2_hr <- df2

      msg_log(
        style = "success",
        message = "Simulation completed.",
        detail = paste(
          "This step took approximately",
          round(difftime(Sys.time(), start, units = 'secs'), 1),
          "seconds."))

      ### 2. Fit models to simulation:

      guess1 <- reactive({
        ctmm::ctmm.guess(vals$data2_hr, interactive = FALSE)
      }) %>% bindCache(vals$dti1,
                       vals$dur1)

      vals$guess_new <- guess1()

      msg_log(
        style = "warning",
        message = "Fitting models to simulation.",
        detail = "This may take a while...")

      start <- Sys.time()

      if(vals$data_type != "simulated") {
        mod1 <- prepare_pars(
          tau_p0 = vals$tau_p0,
          tau_p0_units = vals$tau_p0_units,
          tau_v0 = vals$tau_v0,
          tau_v0_units = vals$tau_v0_units,
          sigma0 = vals$sigma0,
          sigma0_units = vals$sigma0_units)
      } else {
        mod1 <- vals$ctmm_mod
      }

      fit1 <- reactive({
        ctmm::ctmm.fit(
          vals$data2_hr,
          mod1,
          method = "pHREML",
          control = list(method = "pNewton",
                         cores = -1))

      }) %>% bindCache(vals$dti1,
                       vals$dur1)

      start <- Sys.time()
      shiny::withProgress({
        vals$fit1 <- fit1()
      },
      message = "Fitting models to simulation.",
      detail = "This may take a while...")

      msg_log(
        style = "success",
        message = "Model fit completed.",
        detail = paste(
          "This step took approximately",
          round(difftime(Sys.time(), start, units = 'mins'), 1),
          "minutes."))

      vec <- c("regime", "area", "size", "viz")
      for(i in 1:length(vec)) {
        updateTabsetPanel(
          session,
          inputId = paste0("hrTabs_", vec[i]),
          selected = paste0("tab_hrange_1-hrPanel_",
                            vec[i], "_new"))
      }

      ### 3. Run the home range estimator (AKDE):

      start <- Sys.time()
      shiny::withProgress({
        akde_new <- ctmm::akde(data = vals$data2_hr,
                               CTMM = vals$fit1)
        end <- Sys.time()
      },
      message = "Estimating simulated home range.",
      detail = "This may take a while...")

      vals$akde_new <- akde_new
      vals$hrEst_new <- summary(vals$akde_new)$CI[2]
      vals$hrEst_min_new <- summary(vals$akde_new)$CI[1]
      vals$hrEst_max_new <- summary(vals$akde_new)$CI[3]

      truth <- -2 * log(0.05) * pi * (vals$sigma0 %#%
                                        vals$sigma0_units)

      tempnames <- rownames(summary(vals$akde_new)$CI)
      units <- extract_units(
        tempnames[grep('^area', tempnames)])
      vals$hr_units_new <- units

      vals$hrErr_new <- ((vals$hrEst_new %#% units) -
                           truth) / truth
      vals$hrErr_min_new <- ((vals$hrEst_min_new %#% units) -
                               truth) / truth
      vals$hrErr_max_new <- ((vals$hrEst_max_new %#% units) -
                               truth) / truth

      vals$time_simulate <- difftime(Sys.time(), begin,
                                     units = 'mins')

      removeModal()

      # Show buttons to change panels:
      output$hrInput_show_all <- renderUI({
        shinyWidgets::radioGroupButtons(
          inputId = ns("hrInput_show"),
          label = NULL,
          choices = c("Show initial tracking regime" = 1,
                      "Show modified tracking regime" = 2),
          checkIcon = list(yes = icon("check-circle")),
          selected = 2,
          justified = TRUE)
      })

    }) %>% bindEvent(input$run_hr_new)

    # BLOCKS ------------------------------------------------------------
    ## Tracking regime: -------------------------------------------------

    observe({
      req(vals$data1)

      sumdat <- summary(vals$data1)
      tempnames <- names(sumdat)

      output$hrInfo_dur <- shiny::renderUI({

        tempunits <- tempnames[
          grep('sampling period', tempnames)] %>%
          extract_units()

        dur <- as.numeric(
          sumdat[grep('sampling period', tempnames)])

        out <- fix_timeunits(dur, tempunits)
        parBlock(text = "Sampling duration",
                 header = paste(out[1], out[2]))

      }) # end of renderUI // hrInfo_dur

      output$hrInfo_dti <- shiny::renderUI({

        dti <- as.numeric(
          sumdat[grep('sampling interval', tempnames)])

        out <- fix_timeunits(dti, vals$dti0_units)
        parBlock(text = "Sampling interval",
                 header = paste(out[1], out[2]),
                 number = "between fixes")

      }) # end of renderUI // hrInfo_dti
    }) # end of observe

    observe({
      req(vals$dti1, vals$dur1)

      output$hrText_sims <- renderUI({

        dti1 <- vals$dti1
        dti1_txt <- vals$dti1_units
        dur1 <- vals$dur1
        dur1_mth <- "months" %#% vals$dur1 %#% vals$dur1_units

        if(dur1 == 1) {
          dur1_txt <- HTML(paste0(
            "for ",  span("1 day", style = txt_key), "."))
        } else { if(dur1 == 365) {
          dur1_txt <- HTML(paste0(
            "for ",  span("1 year", style = txt_key), "."))
        } else {
          dur1_txt <- HTML(paste(
            "for a duration of",
            span(round(dur1, 1),
                 "days", style = txt_caution), "(or",
            HTML(paste0(
              span(paste0(round(dur1_mth, 1),
                          " months"), style = txt_caution), ")."))
          )) }
        }

        p(br(),
          "This new tracking regime is equal to",
          "a new location every",
          span(round(dti1, 1), dti1_txt,
               style = txt_key),
          dur1_txt)

      }) # end of renderUI // hrText_sims

      output$hrInfo_dur_new <- shiny::renderUI({

        dur <- vals$dur0_units %#% vals$dur1 %#% vals$dur1_units
        tempunits <- vals$dur0_units

        dur <- ifelse(
          dur %% 1 == 0,
          scales::label_comma(accuracy = 1)(dur),
          scales::label_comma(accuracy = .1)(dur))

        parBlock(
          text = "Sampling duration",
          header = span(paste(dur, tempunits),
                        style = col_caution))

      }) # ender of renderUI // hrInfo_dur_new

      output$hrInfo_dti_new <- shiny::renderUI({

        dti <- vals$dti1
        tempunits <- vals$dti1_units

        check_if <- sub('.*(?=.$)', '', tempunits, perl = T)
        if(check_if == "s") { tempunits
        } else { tempunits <- paste0(tempunits, "s") }

        dti <- ifelse(
          dti %% 1 == 0,
          scales::label_comma(accuracy = 1)(dti),
          scales::label_comma(accuracy = .1)(dti))

        parBlock(
          text = "Sampling interval",
          header = span(paste(dti, tempunits),
                        style = col_caution),
          number = span("between fixes",
                        style = col_caution))

      }) # ender of renderUI // hrInfo_dti_new
    }) # end of observe

    ## Sample sizes: ----------------------------------------------------

    output$hrBlock_n <- shiny::renderUI({
      req(vals$data1)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data1),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = TRUE)

    }) # end of renderUI // hrBlock_n (absolute sample size)

    output$hrBlock_Narea <- shiny::renderUI({
      req(vals$newfit)

      tempnames <- names(summary(vals$newfit)$DOF)
      N <- summary(vals$newfit)$DOF[grep('area', tempnames)][[1]]
      n <- nrow(vals$data1)

      diff_perc <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      vals$Narea <- N

      sampleBlock(
        number = diff_perc,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // hrBlock_Narea (effective)

    output$hrBlock_n_new <- shiny::renderUI({
      req(vals$data2_hr)

      sampleBlock(
        numberIcon = FALSE,
        header = nrow(vals$data2_hr),
        line1 = "Absolute sample size",
        line2 = "(n)",
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // hrBlock_n_new (absolute sample size)

    output$hrBlock_Narea_new <- shiny::renderUI({
      req(vals$fit1)

      tempnames <- names(summary(vals$fit1)$DOF)
      N <- summary(vals$fit1)$DOF[grep('area', tempnames)][[1]]
      n <- nrow(vals$data2_hr)

      diff_perc <- paste0(
        "-", round((100 - ((N * 100) / n)), 1), "%")

      sampleBlock(
        number = diff_perc,
        numberIcon = TRUE,
        header = round(N, 1),
        line1 = "Effective sample size",
        line2 = HTML(paste0("(N", tags$sub("area"), ")")),
        rightBorder = FALSE,
        marginBottom = FALSE)

    }) # end of renderUI // hrBlock_Narea_new (effective)

    ## Outputs: ---------------------------------------------------------
    ### Home range area: ------------------------------------------------

    output$hrInfo_est <- shiny::renderUI({

      est <- fix_spUnits(vals$hrEst, vals$hr_units)
      est_min <- fix_spUnits(vals$hrEst_min, vals$hr_units)
      est_max <- fix_spUnits(vals$hrEst_max, vals$hr_units)

      parBlock(
        icon = "map-marked-alt",
        text = "Estimate",
        header = span(HTML("&nbsp;", est[1], est[3]),
                      style = col_main),
        number = span(paste(est_min[1], "—", est_max[1]),
                      style = col_main))


    }) # end of renderUI // hrInfo_est

    output$hrInfo_est_new <- shiny::renderUI({
      req(vals$akde_new)

      est <- fix_spUnits(vals$hrEst_new, vals$hr_units_new)
      est_min <- fix_spUnits(vals$hrEst_min_new, vals$hr_units_new)
      est_max <- fix_spUnits(vals$hrEst_max_new, vals$hr_units_new)

      parBlock(
        icon = "map-marked-alt",
        text = "Estimate",
        header = span(HTML("&nbsp;", est[1], est[3]),
                      style = col_main),
        number = span(paste(est_min[1], "—", est_max[1]),
                      style = col_main))

    }) # end of renderUI // hrInfo_est_new

    ### Relative error: -------------------------------------------------
    # Calculate bias in estimate versus expected value:

    output$hrInfo_err <- shiny::renderUI({
      req(vals$hrErr)

      errorBlock(
        icon = "radiation",
        text = "Expected error",
        value = vals$hrErr,
        min = vals$hrErr_min,
        max = vals$hrErr_max,
        rightBorder = FALSE)

    }) # end of renderUI // hrInfo_err

    output$hrInfo_err_new <- shiny::renderUI({
      req(vals$hrErr_new)

      errorBlock(
        icon = "radiation", # "exclamation-circle",
        text = "Expected error",
        value = vals$hrErr_new,
        min = vals$hrErr_min_new,
        max = vals$hrErr_max_new,
        rightBorder = FALSE)

    }) # end of renderUI // hrInfo_err_new

    # PLOTS -------------------------------------------------------------
    ## Rendering home range estimate plot (xy): -------------------------

    output$hrPlot_main <- ggiraph::renderGirafe({
      req(vals$akde)

      ud <- plotting_hr(dat = vals$data1,
                        ud = vals$akde,
                        levels = input$hrShow_levels)

      ggiraph::girafe(
        ggobj = ud,
        options = list(
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("fill:#dd4b39;",
                        "stroke:#eb5644;")))
      )

    }) # end of renderGirafe

    ## Rendering simulation plot (xy): --------------------------------

    output$hrPlot_simulation <- ggiraph::renderGirafe({
      req(vals$data2_hr, vals$akde_new)

      # Rendering home range estimate plot:

      ud_sim <- plotting_hrsim(dat = vals$data1,
                               datsim = vals$data2_hr,
                               ud = vals$akde_new,
                               levels = input$hrhrShow_levels_sim,
                               show = input$hrShow_datasets)

      ggiraph::girafe(
        ggobj = ud_sim,
        options = list(
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("fill:#dd4b39;",
                        "stroke:#eb5644;")))
      )

    }) # end of renderGirafe

    # TABLES ------------------------------------------------------------
    # Save estimation outputs to data.frame:

    observe({
      req(vals$Narea, vals$hrErr)

      shinyjs::show(id = "hrBox_summary")

      ### Original tracking regime: -------------------------------------

      originalrow <- data.frame(
        data = "Original",
        taup = NA,
        dur = NA,
        n = nrow(vals$data1),
        N = vals$Narea,
        area = NA,
        err = vals$hrErr,
        err_min = vals$hrErr_min,
        err_max = vals$hrErr_max)

      originalrow$taup <-
        paste(scales::label_comma(
          accuracy = .1)(vals$tau_p0),
          abbreviate_time(vals$tau_p0_units))

      originalrow$dur <-
        paste(scales::label_comma(
          accuracy = .1)(vals$dur0),
          abbreviate_time(vals$dur0_units))

      tempunits <- vals$hr_units
      if(tempunits == "square kilometers") {
        tempunits <- paste0("km", tags$sup(2)) }
      if(tempunits == "hectares") { tempunits <- "ha" }

      originalrow$area <-
        paste(scales::label_comma(
          accuracy = .1)(vals$hrEst),
          tempunits)

      vals$df_areas <<- rbind(vals$df_areas, originalrow)

    }) %>% # end of observe, then:
      bindEvent(input$show_ctsdTable)

    observe({

      ### New tracking regime: ------------------------------------------

      fitnames <- names(summary(vals$fit1)$DOF)
      N_new <- summary(vals$fit1)$DOF[
        grep('area', fitnames)][[1]]

      newrow <- data.frame(
        data = "Simulated",
        taup = NA,
        dur = NA,
        n = nrow(vals$data2_hr),
        N = N_new,
        area = NA,
        err = vals$hrErr_new,
        err_min = vals$hrErr_min_new,
        err_max = vals$hrErr_max_new)

      newrow$taup <-
        paste(scales::label_comma(
          accuracy = .1)(vals$tau_p0),
          abbreviate_time(vals$tau_p0_units))

      newrow$dur <-
        paste(scales::label_comma(
          accuracy = .1)(vals$dur0_units %#%
                           vals$dur1 %#% vals$dur1_units),
          abbreviate_time(vals$dur0_units))

      tempunits <- vals$hr_units_new
      if(tempunits == "square kilometers") {
        tempunits <- paste0("km", tags$sup(2)) }
      if(tempunits == "hectares") { tempunits <- "ha" }

      newrow$area <-
        paste(scales::label_comma(
          accuracy = .1)(vals$hrEst_new),
          tempunits)

      df_areas <- dplyr::bind_rows(vals$df_areas, newrow)
      vals$df_areas <- df_areas

    }) %>%  # end of observe, then:
      bindEvent(vals$akde_new)

    ## Rendering output table: ------------------------------------------

    output$hrTable <- DT::renderDataTable({
      req(vals$df_areas)

      columnNames <- list(
        data = "Data:",
        taup = paste0("\u03C4","\u209A"),
        dur = "Duration",
        n = "n",
        N = paste0("N", tags$sub("area")),
        area = "HR area",
        err = "Error",
        err_min = "Error (min)",
        err_max = "Error (max)")

      DT::datatable(
        data = vals$df_areas,
        colnames = as.vector(unlist(columnNames)),
        rownames = FALSE,
        escape = FALSE,
        options = list(
          paging = F, dom = "t",
          order = list(list(0, 'asc')),
          columnDefs = list(list(className = 'dt-center',
                                 targets = 0:8)))
      ) %>%
        sparkline::spk_add_deps() %>%
        DT::formatStyle(
          columns = c("n", "N"),
          fontWeight = "bold",
          color = DT::styleInterval(
            c(5, 30),
            c(hex_caution, '#f5b700', hex_main))
        ) %>%
        DT::formatCurrency(
          columns = c("n", "N"),
          currency = "",
          digits = 0
        ) %>%
        DT::formatStyle(
          columns = "err",
          fontWeight = "bold",
          color = DT::styleInterval(
            c(-0.8, -0.2, 0.2, 0.8),
            c(hex_caution, '#f5b700',
              hex_border,
              '#f5b700', hex_caution))
        ) %>%
        DT::formatStyle(
          columns = c("err_min", "err_max"),
          color = DT::styleInterval(
            c(-0.8, -0.2, 0.2, 0.8),
            c(hex_caution, '#f5b700',
              hex_border,
              '#f5b700', hex_caution))
        ) %>%
        DT::formatPercentage(
          columns = c("err", "err_min", "err_max"),
          digits = 1
        )

    }) # end of renderDataTable // hrTable

    ### Show error scale (slider): --------------------------------------

    output$show_errScale <- renderUI({
      req(vals$hrErr_new)

      min <- round(vals$hrErr_min * 100, 0)
      max <- round(vals$hrErr_max * 100, 0)

      min_new <- round(vals$hrErr_min_new * 100, 0)
      max_new <- round(vals$hrErr_max_new * 100, 0)
      tmp <- which.max(c(abs(min),
                         abs(max),
                         abs(min_new),
                         abs(max_new)))
      tmp <- ifelse(tmp < 100, 100, tmp)

      column(
        width = 12, align = "center",

        br(),
        p("Visualizing error (%) range:",
          style = paste(ft, ft_center,
                        "color: #006466;",
                        "font-size: 21px;")),

        shinyWidgets::sliderTextInput(
          inputId = "viz_err1",
          label = span("Original tracking dataset:",
                       style = txt_label_bold),
          choices = c(0, seq(-tmp, tmp, by = 1)) %>% sort(),
          selected = c(min, max),
          from_min = min,
          from_max = min,
          to_min = max,
          to_max = max
        ),

        shinyWidgets::sliderTextInput(
          inputId = "viz_err2",
          label = span("New tracking dataset:",
                       style = txt_label_bold),
          choices = c(0, seq(-tmp, tmp, by = 1)) %>% sort(),
          selected = c(min_new, max_new),
          from_min = min_new,
          from_max = min_new,
          to_min = max_new,
          to_max = max_new
        ),

      ) # end of column
    }) # end of renderUI // show_errScale

    # HELP TOUR & MODALS: -----------------------------------------------
    ## Help tour (tracking regime): -------------------------------------

    observe({

      element <- intro <- character(0)
      element <- c(element, "#Tour_main")

      intro <- c(
        intro,
        HTML(paste(
          "Click the ",
          fontawesome::fa(name = "wrench", fill = hex_border),
          span("Modify", style = col_border), "button",
          "to adjust the tracking regime."))
      )

      tour <- data.frame(element = element,
                         intro = intro,
                         stringsAsFactors = FALSE)

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
      bindEvent(input$hrHelp_regime)

    ## Help modal (biases): ---------------------------------------------

    output$helpPlot_error <- ggiraph::renderGirafe({

      df0 <- df_sims %>%
        dplyr::filter(method == "pHREML_AKDEc") %>%
        dplyr::mutate(error = (bias - 1) / 1) %>%
        dplyr::group_by(duration)

      dfsum0 <- df0 %>%
        dplyr::summarise(
          mean.error = mean(error, na.rm = TRUE),
          sd.error = sd(error, na.rm = TRUE),
          n.error = dplyr::n()) %>%
        dplyr::mutate(
          se.error = sd.error / sqrt(n.error),
          lci.error = mean.error -
            qt(1-(0.05/2), n.error - 1) * se.error,
          uci.error = mean.error +
            qt(1-(0.05/2), n.error - 1) * se.error)

      brks <- as.numeric(levels(as.factor(dfsum0$duration)))
      p <- ggplot2::ggplot() +
        ggplot2::geom_hline(yintercept = 0) +

        ggiraph::geom_point_interactive(
          data = df0,
          mapping = ggplot2::aes(
            x = duration,
            y = error,
            data_id = error),
          color = "grey90", size = 3) +

        ggiraph::geom_point_interactive(
          data = dfsum0,
          mapping = ggplot2::aes(
            x = duration,
            y = mean.error,
            tooltip = paste0(round(mean.error * 100, 1), "%"),
            data_id = mean.error,
            group = duration,
            color = duration),
          size = 2) +

        viridis::scale_color_viridis(option = "D") +
        ggplot2::scale_x_continuous(
          trans = "log", labels = brks, breaks = brks) +
        ggplot2::scale_y_continuous(labels = scales::percent) +

        ggplot2::labs(x = "Duration (in days)",
                      y = "Relative error (%)") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),

          legend.position = "none",
          text = ggplot2::element_text(
            family = "Roboto Condensed"),
          axis.title.x = ggplot2::element_text(
            family = "Roboto Condensed",
            face = "bold", hjust = .97),
          axis.title.y = ggplot2::element_text(
            family = "Roboto Condensed",
            face = "bold", hjust = .5))

      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("r:5pt;",
                        "fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_selection(
            type = "single",
            css = paste("r:3pt;",
                        "fill:#dd4b39;",
                        "stroke:#eb5644;"))))

    }) # end of renderGirafe // helpPlot_error

    observe({

      shiny::showModal(
        shiny::modalDialog(
          title = h4("Explaining biases:"),

          withMathJax(
            paste0("$$\\small{\\text{Relative error (%)}",
                   " = \\frac{\\text{estimate}-\\text{truth}}",
                   "{\\text{truth}}\\times 100}$$")
          ),

          p(
            style = paste(ft_center, col_main),
            "How much uncertainty is associated",
            "with an estimate?"),

          p(
            "The",
            span("relative error (%)", style = txt_caution),
            "of an", span("home range estimate", style = txt_key),
            "decreases as",
            span("sampling duration", style = txt_border),
            "increases, and is ultimately dependent on the",
            span("position autocorrelation",
                 HTML(paste0("(\u03C4", tags$sub("p"), ")")),
                 "timescale.", style = txt_border),
            "In the case of",
            span(
              HTML(paste0(
                span(
                  HTML(paste0("\u03C4", tags$sub("p"),
                              " = 1 day")),
                  style = col_black), ","))),
            "we can expect bias to decrease as shown",
            "below:"),

          ggiraph::girafeOutput(
            outputId = ns("helpPlot_error"),
            width = "100%", height = "100%"),

          footer = modalButton("Dismiss"),
          size = "m")) # end of modal

    }) %>% # observe event, bound to:
      bindEvent(input$hrHelp_bias)

    # Additional information: -------------------------------------------

    output$time_simulate <- renderText({
      req(vals$time_simulate)
      paste0("New tracking regime simulation took approximately ",
             round(vals$time_simulate, 1), " minutes.")
    })

    output$time_hr <- renderText({
      req(vals$time_hr)
      paste0("Home range estimation took approximately ",
             round(vals$time_hr, 1), " minutes.")
    })

    output$hrInfo_console <- renderPrint({
      return(list(summary(vals$akde),
                  summary(vals$fit1)))
    })

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_hrange_ui("tab_hrange_1")

## To be copied in the server
# mod_tab_hrange_server("tab_hrange_1")
