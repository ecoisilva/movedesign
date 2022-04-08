#' tab_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

      # [left column] -----------------------------------------------------

      div(class = div_column_half,

          uiOutput(ns("report_species"))

      ), # end of UI column (left)

      # [right column] ----------------------------------------------------

      div(class = div_column_half,

          uiOutput(ns("report_device")),

          shinydashboard::tabBox(
            id = ns("reportBox_sizes"),
            width = NULL,

            tabPanel(title = "Sample sizes:",
                     uiOutput(ns("report_sizes"))
            ) # end of tabPanel

          ) # end of tabBox // reportBox_sizes
      ), # end of UI column (right)

      # [bottom column] ---------------------------------------------------

      div(class = div_column_main,

          ## Tables: ------------------------------------------------------

          shinydashboardPlus::box(
            title = span("Tables:", style = ttl_box),
            width = NULL,
            solidHeader = FALSE,

            DT::dataTableOutput(ns("endTable_sims")),
            br(),
            DT::dataTableOutput(ns("endTable_regs")),
            br(),
            DT::dataTableOutput(ns("endTable_outs"))

          ) # end of box // tables
      ) # end of UI column (bottom)

    ) # end of fluidRow
  ) # end of tagList
}

#' tab_report Server Functions
#'
#' @noRd
mod_tab_report_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      shinyjs::toggle("report_plot", condition = input$map_locs)
    }) %>%
      bindEvent(input$map_locs)

    observe({
      req(vals$data_type)

      if(vals$data_type == "simulated") {
        shinyjs::hide("map_locs")
      }
    })

    # INFORMATION -------------------------------------------------------
    ## Species & individual: --------------------------------------------

    output$report_plot <- mapboxer::renderMapboxer({
      req(vals$data_type != "simulated")

      locs <- as.data.frame(vals$data0$longitude)
      names(locs) <- "lng"
      locs$lat <- vals$data0$latitude
      locs$time <- vals$data0$timestamp

      p <- mapboxer::as_mapbox_source(locs) %>%
        mapboxer::mapboxer(
          center = c(mean(locs$lng), mean(locs$lat)),
          zoom = 4
        ) %>%
        mapboxer::add_circle_layer(
          circle_color = "#ffbf00",
          circle_radius = 7,
          id = "locs"
        ) %>%
        mapboxer::add_navigation_control(pos = "top-left")

      return(p)
    }) # end of renderMapboxer

    output$report_species <- renderUI({
      req(vals$tau_p0, vals$tau_v0, vals$sigma0)

      if(!is.null(vals$species_common)) {

        subtitle <- tagList(
          span(vals$species_common, style = txt_gold),
          span(em(paste0("(", vals$species_binom, ")")))
        )

      } else { subtitle <- em(vals$species_binom) }

      out_sig <- fix_spUnits(vals$sigma0, units = vals$sigma0_units)

      shinydashboard::tabBox(
        width = NULL,

        tabPanel(
          title = "Species",
          icon = icon("paw"),

          shinydashboardPlus::navPills(
            id = "reportinfo_species",

            shinydashboardPlus::navPillsItem(
              left = HTML(paste0("Position autocorrelation ",
                                 "(\u03C4", tags$sub("p"), ")")),
              color = hex_main,
              right = paste(scales::label_comma(
                accuracy = .1)(vals$tau_p0), vals$tau_p0_units)),

            shinydashboardPlus::navPillsItem(
              left = HTML(paste0("Velocity autocorrelation ",
                                 "(\u03C4", tags$sub("v"), ")")),
              color = hex_main,
              right = paste(scales::label_comma(
                accuracy = .1)(vals$tau_v0), vals$tau_v0_units)),

            shinydashboardPlus::navPillsItem(
              left = HTML("Spatial variance (\u03C3)"),
              color = hex_main,
              right = span(HTML(out_sig[1], out_sig[3]))),

            mapboxer::mapboxerOutput(outputId = ns("report_plot"))

          ), # end of navPills
        ), # end of tabPanel

        footer = fluidRow(
          splitLayout(
            cellArgs = list(style = 'margin-bottom: -10px;'),

            shiny::p(),
            column(width = 12, align = "center",
                   shinyWidgets::switchInput(ns("map_locs"),
                                             label = "Minimap",
                                             value = FALSE,
                                             inline = TRUE))),
          br())

      ) # end of tabBox
    }) # end of renderUI // report_species

    ## Device: ---------------------------------------------------------

    output$report_device <- renderUI({
      req(vals$dur0_dev, vals$dti0_dev)

      dur <- fix_time(vals$dur0_units_dev %#%
                             vals$dur0_dev, vals$dur0_units_dev)
      dti <- fix_time(vals$dti0_units_dev %#%
                             vals$dti0_dev, vals$dti0_units_dev)

      shinydashboard::tabBox(
        id = ns("reportBox_regime"),
        width = NULL,
        tabPanel(
          title = "Tracking regime",

          shinydashboardPlus::navPills(
            id = "report_regime",

            shinydashboardPlus::navPillsItem(
              left = "Sampling duration",
              color = hex_border,
              right = paste(dur[1], dur[2])),

            shinydashboardPlus::navPillsItem(
              left = "Sampling interval",
              color = hex_border,
              right = paste(dti[1], dti[2]))

          )) # end of tabPanel
      ) # end of tabBox

    }) # ender of renderUI // report_device

    ## Sample sizes: ---------------------------------------------------

    output$report_sizes <- renderUI({
      req(vals$newfit, vals$device_n)
      shinyjs::show(id = "reportBox_sizes")

      n <- vals$device_n
      N1 <- round(vals$device_N1, 1)
      N2 <- round(vals$device_N2, 1)

      if(n <= 5) {
        n_color <- "red"
      } else if(n >= 30) {
        n_color <- "light-blue"
      } else {
        n_color <- "yellow"
      }

      if(N1 <= 5) {
        N1_color <- "red"
        N1_icon <- icon("angle-double-down")
      } else { if(N1 >= 30) {
        N1_color <- "light-blue"
        N1_icon <- icon("angle-down")
      } else {
        N1_color <- "yellow"
        N1_icon <- icon("angle-down")
      } }

      if(N2 <= 5) {
        N2_color <- "red"
        N2_icon <- icon("angle-double-down")
      } else { if(N2 >= 30) {
        N2_color <- "light-blue"
        N2_icon <- icon("angle-down")
      } else {
        N2_color <- "yellow"
        N2_icon <- icon("angle-down")
      } }

      shinydashboardPlus::navPills(
        id = "reportinfo_sizes",

        shinydashboardPlus::navPillsItem(
          left = "Absolute sample size",
          icon = NULL,
          color = n_color,
          right = n),

        shinydashboardPlus::navPillsItem(
          left = "Effective sample size (area)",
          icon = N1_icon,
          color = N1_color,
          right = N1),

        shinydashboardPlus::navPillsItem(
          left = "Effective sample size (speed)",
          icon = N2_icon,
          color = N2_color,
          right = N2)

      ) # end of navPills
    }) # end of renderUI // report_sizes

    # TABLES ------------------------------------------------------------

    # Simulations table: ------------------------------------------------

    output$endTable_sims <- DT::renderDataTable({
      req(vals$data_type == "simulated", vals$df_sims)

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

    }) # end of renderDataTable // endTable_sims

    ## Tracking regime table: -------------------------------------------

    output$endTable_regs <- DT::renderDataTable({
      req(vals$df_regs)

      columnNames <- list(
        device = "Type",
        taup = paste0("\u03C4","\u209A"),
        tauv = paste0("\u03C4","\u1D65"),
        dur = "Duration",
        dti = "Interval",
        n = "n",
        N1 = paste0("N", tags$sub("area")),
        N2 = paste0("N", tags$sub("speed")),
        fit = "Fitted?")

      DT::datatable(
        data = vals$df_regs,
        colnames = as.vector(unlist(columnNames)),
        rownames = FALSE,
        escape = FALSE,
        options = list(
          paging = F, dom = "t",
          rowGroup = list(dataSrc = 1),
          columnDefs = list(list(className = 'dt-center',
                                 targets = 7)))) %>%
        DT::formatStyle(
          columns = c("n", "N1", "N2"),
          fontWeight = "bold",
          color = DT::styleInterval(
            c(5, 30),
            c(hex_caution, "#ffbf00", hex_main))
        ) %>%
        DT::formatCurrency(
          columns = c("n", "N1", "N2"),
          currency = "",
          digits = 0
        )

    }) # end of renderDataTable // endTable_regs

    ## Output tables: ----------------------------------------------------

    output$endTable_outs <- DT::renderDataTable({
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

    }) # end of renderDataTable // endTable_outs

  }) # end of moduleServer
}

## To be copied in the UI
# mod_tab_report_ui("tab_report_1")

## To be copied in the server
# mod_tab_report_server("tab_report_1")
