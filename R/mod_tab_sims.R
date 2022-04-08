#' tab_sims UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats runif
#'
mod_tab_sims_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

      # Introduction: -----------------------------------------------------

      div(class = div_column_main,

          shinydashboardPlus::box(

            title = span("Simulate movement data:", style =
                           paste("padding-top: 14px;", ttl_main)),
            icon = fontawesome::fa(name = "file-signature",
                                   height = "20px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "#e3e3e3"),
            id = ns("sims_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = FALSE, closable = FALSE,

            column(
              align = "center", width = 12,

              p("Choose values that reflect your intended",
                "study species, then click the",
                fontawesome::fa(name = "seedling", fill = hex_main),
                span("Generate seed", style = col_main), "and",
                fontawesome::fa(name = "bolt", fill = hex_main),
                HTML(paste0(span("Run simulation", style = col_main))),
                "buttons (in that order). If needed, re-adjust values",
                "until you achieve a simulation that behaves similarly",
                "to your study species.")

            ) # end of column (for text)
          ) # end of box // sims_intro
      ), # end of div (top row)

      # [right column] ----------------------------------------------------

      div(class = div1_column_right,

          # PARAMETERS: ---------------------------------------------------
          ## Timescale parameters -----------------------------------------

          shinydashboardPlus::box(
            title = span("Temporal parameters", style = ttl_box.solid),
            id = ns("simBox_timescales"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,

            ### Position autocorrelation:

            splitLayout(
              cellWidths = c("92%", "15px"),

              p(HTML("&nbsp;"),
                HTML(paste0("Position autocorrelation ",
                            "(\u03C4", tags$sub("p"), "):")),
                style = paste0(ft, col_main,
                               "font-size: 14px;",
                               "letter-spacing: 0.5px;",
                               "margin: 0 0 5px -7px;",
                               "overflow: hidden;")),

              actionButton(
                inputId = ns("simsHelp_taup"),
                icon = icon("question-circle"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_tau_p0")
            ),

            splitLayout(
              cellWidths = c("40%", "60%"),

              numericInput(
                inputId = ns('tau_p0'),
                label = NULL,
                min = 1, value = 1),

              selectInput(
                inputId = ns("tau_p0_units"),
                label = NULL,
                choices = c("Month(s)" = "months",
                            "Weeks(s)" = "weeks",
                            "Day(s)" = "days",
                            "Hour(s)" = "hours"),
                selected = "days")

            ), # end of splitLayout // tau_p

            ### Velocity autocorrelation:

            splitLayout(
              cellWidths = c("92%", "15px"),

              p(HTML("&nbsp;"),
                HTML(paste0("Velocity autocorrelation ",
                            "(\u03C4", tags$sub("v"), "):")),
                style = paste0(ft, col_main,
                               "font-size: 14px;",
                               "letter-spacing: 0.5px;",
                               "margin: 0 0 5px -7px;",
                               "overflow: hidden;")),

              actionButton(
                inputId = ns("simsHelp_tauv"),
                icon = icon("question-circle"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_tau_v0")
            ),

            splitLayout(
              cellWidths = c("40%", "60%"),

              numericInput(
                inputId = ns('tau_v0'),
                label = NULL,
                min = 1, max = 500, value = 1),

              selectInput(
                inputId = ns("tau_v0_units"),
                label = NULL,
                choices = c("Day(s)" = "days",
                            "Hour(s)" = "hours",
                            "Minute(s)" = "minutes"),
                selected = "hours")

            ) # end of splitLayout // tau_v

          ), # end of box // simBox_timescales

          ## Spatial parameters -------------------------------------------

          shinydashboardPlus::box(
            title = span("Spatial parameters", style = ttl_box.solid),
            id = ns("simBox_spatialscales"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,

            splitLayout(
              cellWidths = c("92%", "15px"),

              p(HTML("&nbsp;"),
                HTML("Semi-variance (\u03C3):"),
                style = paste0(ft, col_main,
                               "font-size: 14px;",
                               "letter-spacing: 0.5px;",
                               "margin: 0 0 5px -7px;")),

              actionButton(
                inputId = ns("simsHelp_var"),
                icon = icon("question-circle"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;",
                              "float: right;")) %>%
                bsplus::bs_attach_modal(id_modal = "modal_sigma0")
            ),

            splitLayout(
              cellWidths = c("40%", "60%"),

              numericInput(
                inputId = ns('sigma0'),
                label = NULL,
                min = 1, max = 500, value = 1),

              selectInput(
                inputId = ns("sigma0_units"),
                label = NULL,
                choices = c(
                  "km\u00B2" = "square kilometers",
                  "m\u00B2" = "square meters",
                  "ha" = "ha"),
                selected = "km\u00B2")

            ) # end of splitLayout

          ), # end of box // simBox_spatialscales

          ## Submit parameters --------------------------------------------

          shinydashboardPlus::box(
            id = "simBox_submit",
            width = NULL,
            headerBorder = FALSE,

            actionButton(inputId = ns("generateSeed"),
                         icon = icon("seedling"),
                         label = "Generate seed",
                         width = "100%"),

            fluidRow(
              column(width = 12,
                     verbatimTextOutput(outputId = ns("seedvalue"))
              )), p(style = "padding: 0px;"),

            actionButton(
              inputId = ns("run_sim"),
              icon =  icon("bolt"),
              label = "Run simulation",
              width = "100%",
              class = "btn-primary")

          ) # end of box // simBox_submit
      ), # end of div (right column)

      # [center column] ---------------------------------------------------

      div(class = div1_column_left,

          # Visualization: ------------------------------------------------

          shinydashboardPlus::box(
            title = span("Visualizing simulated data:", style = ttl_box),
            id = ns("simBox_viz"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,

            tabsetPanel(
              id = ns("simTabs_viz"),

              tabPanel(
                value = ns("simPanel_id"),
                title = tagList(
                  fontawesome::fa(name = "paw", fill = hex_border),
                  span("Data", style = ttl_panel)),

                br(),

                ggiraph::girafeOutput(
                  outputId = ns("simPlot_id"),
                  width = "95%", height = "100%") %>%
                  shinycssloaders::withSpinner(
                    type = getOption("spinner.type", default = 7),
                    color = getOption("spinner.color",
                                      default = "#f4f4f4"))


              ), # end of panels (1 out of 3)

              tabPanel(
                value = ns("simPanel_animated"),
                title = tagList(
                  fontawesome::fa(name = "route", fill = hex_border),
                  span("Animation", style = ttl_panel)
                ), br(),

                ggiraph::girafeOutput(
                  outputId = ns("simPlot_animated"),
                  width = "95%", height = "100%"),

                column(width = 12, align = "center",
                       uiOutput(ns("simInput_timeline"))
                ), br()

              ) # end of panels (2 out of 2)
            ) # end of tabs

          ), # end of box // simBox_viz

          shinydashboardPlus::box(
            id = ns("simBox_repeat"),
            width = NULL,
            headerBorder = FALSE,

            column(
              width = 12, align = "center",

              p("Do you wish to compare multiple simulations?",
                style = paste(ft, ft_center,
                              "color: #006466;",
                              "font-size: 20px;")),

              p(style = "text-align: justify",
                "You can add all chosen parameters",
                "to a table for easy comparisons,",
                "by clicking the",
                fontawesome::fa(name = "bookmark", fill = hex_main),
                span("Add to table", style = btn_primary),
                "button below after each simulation run.")

            ), # end of column (box body)

            footer = column(
              width = 12, align = "center",

              splitLayout(
                cellWidths = c("40%", "30%", "30%"),
                cellArgs = list(style = "align: right;"),

                br(),
                actionButton(
                  inputId = ns("repeat_sim"),
                  label = "Repeat",
                  icon = icon("redo"),
                  class = "btn-info",
                  width = "90%"),

                actionButton(
                  inputId = ns("simButton_save"),
                  label = span("Add to",
                               span("table", style = col_border)),
                  icon = icon("bookmark"),
                  width = "100%")

              ) # end of splitLayout
            ) # end of column (footer)

          ) # end of box // simBox_repeat
      ), # end of column (center)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          # Table: --------------------------------------------------------

          shinydashboardPlus::box(
            title = span("Summary table:", style = ttl_box),
            id = ns("simBox_summary"),
            width = NULL,
            solidHeader = FALSE,

            DT::dataTableOutput(ns("simsTable")),
            column(
              width = 12, align = "center",

              p(style = ft_center,
                br(), span("Note:", style = col_caution),
                "the", span("movement speed",
                            style = "color: #000000;"),
                "value returned here",
                "assumes a Gaussian stochastic process for",
                "a faster computation, but the true value may",
                "not necessarily be normally distributed.")

            ), # end of column (text)

            br(),
            div(style = "display:inline-block; float:right",
                actionButton(
                  inputId = ns("simsTable_clear"),
                  label = "Clear table",
                  icon =  icon("trash"),
                  width = "110px")), br()

          ), # end of box

          # Information and R console: ------------------------------------

          shinydashboardPlus::box(
            title = span("Additional information:", style = ttl_box),
            id = ns("simBox_misc"),
            width = NULL,
            solidHeader = FALSE,

            verbatimTextOutput(outputId = ns("console_sims"))

          ) # end of box

      ), # end of column (bottom)

      # FIXED PANELS: -----------------------------------------------------
      ## Help button: -----------------------------------------------------

      fixedPanel(
        actionButton(
          inputId = ns("help_sims"),
          label = "Help",
          icon = icon("question-circle"),
          style = paste("color: #fff;",
                        "background-color: #222d32;",
                        "border-color: #222d32")),

        right = 25, top = 75, width = "40px")

    ), # end of fluidRow

    # MODALS: -------------------------------------------------------------

    modal_tau_p0,
    modal_tau_v0,
    modal_sigma0,
    NULL

  ) # end of tagList
}

#' tab_sims Server Functions
#'
#' @noRd
mod_tab_sims_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # DYNAMIC UI ELEMENTS -----------------------------------------------

    ## Hide boxes initially:
    shinyjs::hide(id = "simBox_summary")
    shinyjs::hide(id = "simBox_repeat")

    # SIMULATIONS -------------------------------------------------------
    ## Initial sampling parameters: -------------------------------------

    observe({

      tmp_taup <- "days" %#% input$tau_p0 %#% input$tau_p0_units
      vals$dur0 <- ifelse(tmp_taup > 1, tmp_taup * 10, 10)
      vals$dur0_units <- "days"

    }) %>% bindEvent(input$tau_p0, input$tau_p0_units)

    observe({
      tmp_tauv <- input$tau_v0 %#% input$tau_v0_units
      vals$dti0 <- ifelse(tmp_tauv <= 120, 1, round(
        "minutes" %#% (input$tau_v0 %#% input$tau_v0_units)/4, 0))
      vals$dti0_units <- "minutes"

    }) %>% bindEvent(input$tau_v0, input$tau_v0_units)

    to_run <- reactive({
      list(input$run_sim, input$repeat_sim)
    })

    to_rerun <- reactive({
      list(input$generateSeed, input$repeat_sim)
    })
    seed0 <- reactive({
      round(stats::runif(1, min = 1, max = 10000), 0)
    }) %>% bindEvent(to_rerun(), ignoreInit = TRUE)

    ## Prepare model and run simulation: --------------------------------

    observe({
      if(!is.null(vals$seed0)) {

        vals$ctmm_mod <- prepare_pars(
          tau_p0 = input$tau_p0,
          tau_p0_units = input$tau_p0_units,
          tau_v0 = input$tau_v0,
          tau_v0_units = input$tau_v0_units,
          sigma0 = input$sigma0,
          sigma0_units = input$sigma0_units)

        sim0 <- reactive({
          simulate_data(
            mod0 = vals$ctmm_mod,
            dur0 = vals$dur0, dur0_units = vals$dur0_units,
            dti0 = 20, dti0_units = "seconds",
            seed0 = vals$seed0)
        }) %>%
          bindCache(vals$tau_p0,
                    vals$tau_p0_units,
                    vals$tau_v0,
                    vals$tau_v0_units,
                    vals$sigma0,
                    vals$sigma0_units,
                    vals$seed0)

        # Simulate full data:

        msg_log(
          style = "warning",
          message = paste0("Simulation ",
                           msg_warning("in progress"), "..."),
          detail = "Please wait for the simulation to finish."
        )

        start_sim <- Sys.time()
        withProgress({
          vals$data_full <- sim0()
        },
        message = "Simulation in progress.",
        detail = "This may take a while...")

        # Subset data:

        tmpsim <- vals$data_full[which(
          vals$data_full$t <= vals$dur0 %#% vals$dur0_units), ]
        rows.thin <- seq(1, nrow(tmpsim), by = vals$dti0)
        vals$data0 <- tmpsim[rows.thin, ]

        # Store relevant values:

        vals$data_type <- "simulated"
        vals$species_binom <- vals$species <- "Simulated"
        vals$id <- vals$tmpid <- "Simulated individual"

        vals$tau_p0 <- input$tau_p0
        vals$tau_p0_units <- input$tau_p0_units
        vals$tau_v0 <- input$tau_v0
        vals$tau_v0_units <- input$tau_v0_units
        vals$sigma0 <- input$sigma0
        vals$sigma0_units <- input$sigma0_units

        msg_log(
          style = "success",
          message = paste0("Simulation ",
                           msg_success("completed"), "."),
          detail = paste(
            "This step took approximately",
            round(difftime(Sys.time(), start_sim,
                           units = 'min'), 1),
            "minutes."))
        vals$is_valid <- TRUE

        shinyjs::enable("simButton_save")

        ## Run model fit: -----------------------------------------------

        vals$guess <- NULL
        vals$needs_fit <- FALSE

        start <- Sys.time()
        msg_log(
          style = "warning",
          message = paste0("...", msg_warning("Fitting"),
                           " movement model."),
          detail = "Please wait for model fit to finish.")

        withProgress({
          vals$fit <- ctmm::ctmm.fit(
            vals$data0, vals$ctmm_mod,
            method = 'pHREML',
            control = list(method = 'pNewton', cores = -1))
        },
        message = "Fitting movement model.",
        detail = "This may take a while...")

        vals$time_sims <- difftime(Sys.time(), start,
                                   units = "mins")
        msg_log(
          style = "success",
          message = paste0("Model fitting ",
                           msg_success("completed"), "."),
          detail = paste(
            "This step took approximately",
            round(vals$time_sims, 1), "minutes."))

        shinyjs::show(id = "simBox_misc")

      } else {

        shinyalert::shinyalert(
          title = "No seed found",
          text = span(
            'Please generate a seed value first, by', br(),
            'clicking the',
            fontawesome::fa(name = "seedling", fill = hex_main),
            span('Generate seed', style = col_main),
            'button.'),
          html = TRUE,
          size = "xs")

      } # end of if() statement

      shinyjs::show(id = "simBox_repeat")

    }) %>% # end of observe,
      bindEvent(to_run(), ignoreInit = TRUE)

    # PREPARE -----------------------------------------------------------

    calculate_dist <- reactive({
      # Distance traveled:

      tmpdat <- data.frame(
        x = vals$data0$x,
        y = vals$data0$y)

      tmpdist <- list()
      for(i in 2:nrow(vals$data0)) {
        tmpdist[[i]] <-
          sqrt((tmpdat$x[i] - tmpdat$x[i-1])^2 +
                 (tmpdat$y[i] - tmpdat$y[i-1])^2)
      }
      dist <- c(0, do.call("rbind", tmpdist))
      return(dist)
    })

    ## Preparing data for animation plot: -------------------------------

    data_animated <- reactive({
      req(vals$data0, vals$data_type == "simulated")

      dat <- vals$data0
      t_origin <- "1111-10-31 23:06:32"
      dat$timestamp <- as.POSIXct(dat$t, origin = t_origin)

      data_animated <- dat[which(dat$t <= input$timeline), ]
      return(data_animated)

    })

    # PLOTS -------------------------------------------------------------
    ## Rendering simulated data plot (xy): ------------------------------

    output$simPlot_id <- ggiraph::renderGirafe({
      req(vals$data0, vals$data_type == "simulated")

      newdat <- vals$data0
      newdat <- newdat[which(newdat$t <= (1 %#% "day")), ]

      out_tp <- fix_time(vals$tau_p0, vals$tau_p0_units)
      out_dur <- fix_time(vals$dur0, vals$dur0_units)
      subtitle <- paste(
        "Highlighting one \u03C4\u209A cycle",
        paste0("(\u2248 ", out_tp[1], " ", out_tp[2], ")"),
        "out of ", out_dur[1], out_dur[2])

      ymin <- min(vals$data0$y) - diff(range(vals$data0$y)) * .2
      ymax <- max(vals$data0$y) + diff(range(vals$data0$y)) * .2
      p <- ggplot2::ggplot() +
        ggplot2::geom_path(
          vals$data0, mapping = ggplot2::aes(
            x = x, y = y),
          col = "grey90", size = 1) +

        ggiraph::geom_path_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y, color = timestamp),
          size = 0.8) +
        ggiraph::geom_point_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = timestamp,
            tooltip = timestamp,
            data_id = timestamp),
          size = 2.5) +

        ggplot2::labs(
          title = "Simulated individual:",
          subtitle = subtitle,
          x = "x coordinate",
          y = "y coordinate") +

        ggplot2::scale_x_continuous(
          labels = scales::comma) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(ymin, ymax)) +
        viridis::scale_color_viridis(
          name = "Time:",
          option = "D", trans = "time",
          breaks = c(min(newdat$time),
                     max(newdat$time)),
          labels = c("Start", "End")) +

        theme_movedesign() +
        ggplot2::guides(
          color = ggplot2::guide_colorbar(
            title.vjust = 1.02)) +
        ggplot2::theme(
          legend.position = c(0.76, 0.08),
          legend.direction = "horizontal",
          legend.title = ggplot2::element_text(
            size = 11, face = "bold.italic"),
          legend.key.height = ggplot2::unit(0.3, "cm"),
          legend.key.width = ggplot2::unit(0.6, "cm")
        )

      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 6,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .1),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_tooltip(use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill:#1279BF;",
                        "stroke:#1279BF;",
                        "cursor:pointer;")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))

    }) # end of renderGirafe // simPlot_id

    ## Rendering animation (xy), for 1-day of data: ---------------------

    output$simInput_timeline <- renderUI({
      req(vals$data0, vals$data_type == "simulated")

      tags$div(
        class = "timelineinput",
        sliderInput(
          inputId = ns("timeline"),
          label = p("Animating one full day,",
                    paste0(vals$dti0, "-min steps:"),
                    style = txt_label_bold),

          min = vals$dti0 %#% vals$dti0_units,
          max = 1 %#% "day",
          value = 1 %#% "day",
          step = vals$dti0 %#% vals$dti0_units,
          animate = animationOptions(interval = 400),
          ticks = FALSE,
          width = "85%"))

    }) # end of renderUI // simInput_timeline

    # ANIMATION

    output$simPlot_animated <- ggiraph::renderGirafe({
      req(input$timeline)

      # Time elapsed:

      dat <- data_animated()
      datfull <- vals$data0[which(vals$data0$t <= (1 %#% "day")), ]
      nday <- format(max(dat$timestamp), "%d")

      subtitle <- paste("Day", nday,
                        format(max(dat$timestamp), "%H:%M:%S"))

      thrs_elapsed <- paste("hours" %#% max(dat$t), "hours")
      tmin_elapsed <- paste(vals$dti0_units %#% max(dat$t),
                            vals$dti0_units)

      # Distance traveled:

      tmpdat <- data.frame(
        x = dat$x,
        y = dat$y)

      tmpdist <- list()
      for(i in 2:nrow(tmpdat)) {
        tmpdist[[i]] <-
          sqrt((tmpdat$x[i] - tmpdat$x[i-1])^2 +
                 (tmpdat$y[i] - tmpdat$y[i-1])^2)
      }
      tmpdat$dist <- c(0, do.call("rbind", tmpdist))
      dist <- paste(
        scales::label_comma(
          accuracy = 1)(sum(tmpdat$dist, na.rm = TRUE)),
        "meters")

      ymin <- min(datfull$y) - diff(range(datfull$y)) * .2
      ymax <- max(datfull$y) + diff(range(datfull$y)) * .2
      p <- ggplot2::ggplot() +

        ggplot2::geom_path(
          data = datfull, mapping = ggplot2::aes(x = x, y = y),
          col = "grey90") +
        ggplot2::geom_point(
          data = datfull, mapping = ggplot2::aes(x = x, y = y),
          col = "grey90", size = 2) +
        ggplot2::geom_path(
          data = dat,
          mapping = ggplot2::aes(x = x, y = y, color = timestamp),
          size = 1.2) +
        ggplot2::geom_point(
          data = dat,
          mapping = ggplot2::aes(x = x, y = y, color = timestamp),
          size = 2.5) +

        # Time elapsed:

        ggplot2::annotate(
          "text", family = "Roboto Condensed",
          col = hex_main,
          x = min(datfull$x) + diff(range(datfull$x)) * .2,
          y = ymax - diff(range(datfull$y)) * .1,
          fontface = 2, size = 5, lineheight = 1.5,
          label = paste("Time elapsed:\n")) +
        ggplot2::annotate(
          "text", family = "Roboto Condensed",
          col = hex_main,
          x = min(datfull$x) + diff(range(datfull$x)) * .2,
          y = ymax - diff(range(datfull$y)) * .1,
          fontface = 1, size = 4, lineheight = 1.5,
          label = tmin_elapsed) +

        # Distance traveled:

        ggplot2::annotate(
          "text", family = "Roboto Condensed",
          col = hex_main,
          x = max(datfull$x) - diff(range(datfull$x)) * .2,
          y = ymax - diff(range(datfull$y)) * .1,
          fontface = 2, size = 5, lineheight = 1.5,
          label = paste("Distance traveled:\n")) +
        ggplot2::annotate(
          "text", family = "Roboto Condensed",
          col = hex_main,
          x = max(datfull$x) - diff(range(datfull$x)) * .2,
          y = ymax - diff(range(datfull$y)) * .1,
          fontface = 1, size = 4, lineheight = 1.5,
          label = paste(dist)) +

        ggplot2::labs(
          title = "Timestamp:",
          subtitle = subtitle,
          x = "x coordinate",
          y = "y coordinate") +

        ggplot2::scale_x_continuous(
          labels = scales::comma) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(ymin, ymax)) +
        viridis::scale_color_viridis(
          name = "Tracking time:",
          option = "mako", direction = -1, trans = "time") +

        theme_movedesign() +
        ggplot2::theme(legend.position = "none")

      ggiraph::girafe(
        ggobj = p,
        width_svg = 6, height_svg = 6,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .1),
          ggiraph::opts_toolbar(saveaspng = FALSE)))

    }) # end of renderUI

    # TABLES ------------------------------------------------------------
    ## Listing multiple simulation parameters: --------------------------

    observe({
      shinyjs::show(id = "simBox_summary")

      simrow <- data.frame(
        taup = NA,
        tauv = NA,
        sigma = NA,
        dist = NA,
        meandist = NA,
        speed = NA)

      simrow$taup <-
        paste(scales::label_comma(
          accuracy = .1)(vals$tau_p0), vals$tau_p0_units)

      simrow$tauv <-
        paste(scales::label_comma(
          accuracy = .1)(vals$tau_v0), vals$tau_v0_units)

      simrow$sigma <-
        paste(scales::label_comma(
          accuracy = .1)(vals$sigma0), vals$sigma0_units)

      tmpdist <- calculate_dist()
      simrow$dist <-
        paste(scales::label_comma(
          accuracy = 1)(sum(tmpdist, na.rm = TRUE)), "meters")

      simrow$meandist <-
        paste(scales::label_comma(
          accuracy = .1)(mean(tmpdist)), "meters")

      tmpnames <- rownames(summary(vals$fit)$CI)
      speed <- summary(vals$fit)$CI[
        grep('speed', tmpnames), 2]
      speedunits <- tmpnames[grep('speed', tmpnames)] %>%
        extract_units()

      if(speedunits == "kilometers/day") {
        speedunits <- "km/day" }

      simrow$speed <-
        paste(scales::label_comma(
          accuracy = .1)(speed), speedunits)

      vals$df_sims <<- rbind(vals$df_sims, simrow)
      shinyjs::disable("simButton_save")

    }) %>% # end of observe
      bindEvent(input$simButton_save)

    output$simsTable <- DT::renderDataTable({

      columnNames <- list(
        taup = "\u03C4\u209A",
        tauv = "\u03C4\u1D65",
        sigma = "\u03C3",
        dist = "Tot. Distance",
        meandist = "Avg. Distance",
        speed = "Avg. Speed")

      DT::datatable(
        data = vals$df_sims,
        colnames = as.vector(unlist(columnNames)),
        escape = FALSE,
        options = list(
          paging = F, dom = "t",
          rowGroup = list(dataSrc = 1),
          columnDefs = list(list(className = 'dt-center',
                                 targets = 6))))

    }) # end of renderDataTable // simsTable

    observe({
      vals$df_sims <- NULL
    }) %>% # end of observe,
      bindEvent(input$simsTable_clear)

    # HELP TOUR & MODALS: -----------------------------------------------

    # build_simsTour <- function(ns, vals) {
    #
    #   element <- intro <- character(0)
    #
    #   element <- c(element, "#Tour_start")
    #   intro <- c(
    #     intro,
    #     HTML(paste("Sims tab"))
    #   )
    #
    #   element <- c(element, paste0("#tab_sims_1", "-",
    #                                "sims_intro"))
    #   intro <- c(
    #     intro,
    #     HTML(paste(
    #
    #       span("Action:", style = paste(txt_action)),
    #       "In this box, please choose",
    #       paste0(span("Option 1. Select species",
    #                   style = paste(txt_tour, col_grey)), ","),
    #       "then go to the next step."
    #
    #     ))
    #   )
    #
    #   element <- c(element, paste0("#tab_sims_1", "-",
    #                                "simBox_timescales"))
    #   intro <- c(
    #     intro,
    #     HTML(paste("Sims tab"))
    #   )
    #
    #   element <- c(element, paste0("#tau_v0_units"))
    #   intro <- c(
    #     intro,
    #     HTML(paste("Sims tab"))
    #   )
    #
    #   element <- c(element, paste0("#tab_sims_1", "-",
    #                                "simBox_spatialscales"))
    #   intro <- c(
    #     intro,
    #     HTML(paste("Sims tab"))
    #   )
    #
    #   element <- c(element, paste0("#tab_sims_1", "-",
    #                                "simBox_simulate"))
    #   intro <- c(
    #     intro,
    #     HTML(paste("Sims tab"))
    #   )
    #
    #   element <- c(element, paste0("#tab_sims_1", "-",
    #                                "simBox_viz"))
    #   intro <- c(
    #     intro,
    #     HTML(paste("Sims tab"))
    #   )
    #
    #   element <- c(element, paste0("#tab_sims_1", "-",
    #                                "simBox_samplesizes"))
    #   intro <- c(
    #     intro,
    #     HTML(paste("Sims tab"))
    #   )
    #
    #   element <- c(element, paste0("#tab_sims_1", "-",
    #                                "simBox_mvmetrics"))
    #   intro <- c(
    #     intro,
    #     HTML(paste("Sims tab"))
    #   )
    #
    #   data.frame(element = element,
    #              intro = intro,
    #              stringsAsFactors = FALSE)
    #
    # } # end of sims tour
    #
    # observe({
    #   tour_sims <- build_simsTour(ns, vals)
    #
    #   rintrojs::introjs(
    #     session = session,
    #     options = list(
    #       steps = tour_sims,
    #       nextLabel = "Next",
    #       prevLabel = "Previous",
    #       showStepNumbers = F,
    #       showButtons = T,
    #       showBullets = T
    #     ))
    #
    # }) %>% bindEvent(input$help_sims)

    # Additional information: -------------------------------------------

    output$seedvalue <- renderPrint({
      vals$seed0 <- seed0()
      return(vals$seed0)
    })

    output$console_sims <- renderText({
      req(vals$time_sims)
      paste0("The simulation took approximately ",
             round(vals$time_sims, 1), " minutes.")
    })

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_sims_ui("tab_sims_1")

## To be copied in the server
# mod_tab_sims_server("tab_sims_1")
