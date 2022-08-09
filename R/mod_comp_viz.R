#' comp_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comp_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(

    tabsetPanel(
      id = ns("dataTabs_viz"),

      tabPanel(
        value = ns("dataPanel_all"),
        title = tagList(
          icon("paw", class = "cl-sea"),
          span("Data", class = "ttl-panel")
        ),

        p(),
        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-7",
            reactable::reactableOutput(ns("dataTable_all"))
        ),

        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-5",
            ggiraph::girafeOutput(
              outputId = ns("dataPlot_all"),
              width = "100%", height = "100%")
        )

      ), # end of panels (1 out of 3)

      tabPanel(
        value = ns("dataPanel_individual"),
        title = tagList(
          icon("filter", class = "cl-sea"),
          span("Selected individual", class = "ttl-panel")
        ),

        p(),
        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-6",
            ggiraph::girafeOutput(
              outputId = ns("dataPlot_id"),
              width = "100%", height = "100%") %>%
              shinycssloaders::withSpinner(
                type = getOption("spinner.type", default = 7),
                color = getOption("spinner.color",
                                  default = "#f4f4f4"))
        ),

        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-6",
            reactable::reactableOutput(ns("dataTable_id")),
            # DT::dataTableOutput(ns("dataTable_id")) %>%
            #   shinycssloaders::withSpinner(
            #     type = getOption("spinner.type", default = 7),
            #     color = getOption("spinner.color",
            #                       default = "#f4f4f4")),

            uiOutput(ns("dataTable_showVars"))
        )

      ), # end of panels (2 out of 3)

      tabPanel(
        value = ns("dataPanel_svf"),
        title = tagList(
          icon("chart-line", class = "cl-sea"),
          span("Variogram", class = "ttl-panel")
        ),

        br(),
        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-8",
            ggiraph::girafeOutput(
              outputId = ns("dataPlot_svf"),
              width = "100%", height = "100%")),

        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-4",
            column(
              width = 12, align = "center",
              p(),
              shiny::sliderInput(
                ns("dataVar_timeframe"),
                label = span(paste("Proportion of the",
                                   "variogram plotted (in %):")),
                min = 0, max = 100, value = 65, step = 5,
                width = "90%")
            ))

      ) # end of panels (1 out of 3)
    ) # end of tabs

  )
}

#' comp_viz Server Functions
#'
#' @noRd
mod_comp_viz_server <- function(id, vals) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    pal <- load_pal()

    observe({
      req(vals$is_valid)

      tabselected <- NULL
      if(vals$is_valid && vals$active_tab == 'data_upload') {
        tabselected <- "comp_viz_uploaded-dataPanel_individual"
      }
      if(vals$is_valid && vals$active_tab == 'data_select') {
        tabselected <- "comp_viz_selected-dataPanel_individual"
      }

      updateTabsetPanel(
        session,
        inputId = "dataTabs_viz",
        selected = tabselected)

    }) %>% bindEvent(vals$is_valid)

    # SUMMARIZE DATA ----------------------------------------------------

    observe({ # First, create data summary:
      req(vals$dataList)

      if(!("timestamp" %in% names(vals$dataList[[1]]))) {

        vals$is_anonymized <- TRUE

        shiny::showNotification(
          duration = 5,
          ui = shiny::span(
            "Data is anonymized,", br(),
            "simulating location and time."),
          closeButton = FALSE, type = "warning")

        msg_log(
          style = "success",
          message = "Anonymized data completed",
          detail = "Simulated location and time added."
        )

        vals$dataList <- ctmm:::pseudonymize(vals$dataList)

      } else { vals$is_anonymized <- FALSE }

      dfList <- as_tele_list(vals$dataList)
      sumdfList <- summary(dfList)

      for(i in 1:length(dfList)) {
        sumdfList$n[i] <- nrow(dfList[[i]])
      }

      sum_col1 <- grep('period', names(sumdfList))
      sum_col2 <- grep('interval', names(sumdfList))
      sumdfList[,sum_col1] <- round(sumdfList[sum_col1], 1)
      sumdfList[,sum_col2] <- round(sumdfList[sum_col2], 1)
      sumdfList <- sumdfList %>% dplyr::select(-longitude,
                                               -latitude)

      vals$sum <- sumdfList

    }) # end of observe

    output$dataTable_showVars <- renderUI({
      req(vals$input_x,
          vals$input_y,
          vals$input_t)

      shinyWidgets::pickerInput(
        inputId = ns("show_vars"),
        width = "100%",
        label = span("Columns to show above:",
                     class = "txt-label"),
        choices = names(vals$data0),
        selected = c(vals$input_x,
                     vals$input_y,
                     vals$input_t),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ), multiple = TRUE)

    }) # end of renderUI // dataTable_showVars

    # PLOTS -------------------------------------------------------------
    ## Rendering all data (xy): -----------------------------------------

    output$dataPlot_all <- ggiraph::renderGirafe({
      req(vals$dataList, vals$data_type != "simulated")

      if(vals$data_type == "selected") {

        # to address compatibility of the pelican dataset:
        if(vals$species != "pelican") {
          newdat.all <- as_tele_df(vals$dataList)
        } else {
          data_df <- list()
          for(i in 1:length(vals$dataList)) {
            temp0 <- vals$dataList[i]
            if(names(vals$dataList)[i] == "gps") {
              temp1 <- temp0@.Data[1] %>% as.data.frame
              temp1 <- temp1 %>%
                dplyr::select('gps.timestamp',
                              'gps.x',
                              'gps.y')
            }
            if(names(vals$dataList)[i] == "argos") {
              temp1 <- temp0@.Data[1] %>% as.data.frame
              temp1 <- temp1 %>%
                dplyr::select('argos.timestamp',
                              'argos.x',
                              'argos.y')
            }
            colnames(temp1) <- c("timestamp", "x", "y")
            temp1$name <- rep(names(vals$dataList)[i], nrow(temp1))
            data_df[[i]] <- temp1 } # end of forloop

          newdat.all <- do.call(rbind.data.frame, data_df) }
      } else { newdat.all <- as_tele_df(vals$dataList) }

      yrange <- diff(range(newdat.all$y))
      xrange <- diff(range(newdat.all$x))

      if(yrange < 1.5 * xrange) {
        ymin <- min(newdat.all$y) - yrange * .3
        ymax <- max(newdat.all$y) + yrange * .3
      } else if(yrange < 2 * xrange) {
        ymin <- min(newdat.all$y) - yrange * .5
        ymax <- max(newdat.all$y) + yrange * .5
      } else {
        ymin <- min(newdat.all$y)
        ymax <- max(newdat.all$y)
      }

      if(xrange < 2 * yrange) {
        xmin <- min(newdat.all$x) - xrange * .5
        xmax <- max(newdat.all$x) + xrange * .5
      } else {
        xmin <- min(newdat.all$x)
        xmax <- max(newdat.all$x)
      }

      p.all <- ggplot2::ggplot() +
        ggiraph::geom_point_interactive(
          data = newdat.all,
          ggplot2::aes(x, y,
                       color = id,
                       tooltip = id,
                       data_id = id),
          size = 1.2) +
        ggplot2::labs(x = "x coordinate",
                      y = "y coordinate") +
        # ggplot2::coord_fixed() +

        ggplot2::scale_x_continuous(
          labels = scales::comma,
          limits = c(xmin, xmax)) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(ymin, ymax)) +
        ggplot2::scale_color_grey() +

        theme_movedesign() +
        ggplot2::theme(legend.position = "none")

      ggiraph::girafe(
        ggobj = p.all,
        # width_svg = 6, height_svg = 5,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .5),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill:#ffbf00;",
                        "stroke:#ffbf00;")),
          ggiraph::opts_selection(
            selected = vals$id,
            type = "single",
            css = paste("fill:#dd4b39;",
                        "stroke:#eb5644;")))
      )

    }) # end of renderGirafe

    observe({
      req(input$dataPlot_all_selected)
      vals$plot_selection <- input$dataPlot_all_selected
    })

    ## Rendering individual data (xy): ----------------------------------

    output$dataPlot_id <- ggiraph::renderGirafe({
      req(vals$data0,
          vals$input_x,
          vals$input_y,
          vals$input_t)

      if( class(vals$data0) == "data.frame" ) {
        vals$data0 <- ctmm::as.telemetry(vals$data0) }

      newdat <- as.data.frame(vals$data0[[vals$input_x]])
      names(newdat) <- "x"
      newdat$y <- vals$data0[[vals$input_y]]
      newdat$time <- vals$data0[[vals$input_t]]

      newdat$time <- as.POSIXct(
        newdat$time, format = "%Y-%m-%d %H:%M:%S")

      yrange <- diff(range(newdat$y))
      xrange <- diff(range(newdat$x))

      if(yrange < 1.5 * xrange) {
        ymin <- min(newdat$y) - yrange * .3
        ymax <- max(newdat$y) + yrange * .3
      } else if(yrange < 2 * xrange) {
        ymin <- min(newdat$y) - yrange * .5
        ymax <- max(newdat$y) + yrange * .5
      } else {
        ymin <- min(newdat$y)
        ymax <- max(newdat$y)
      }

      if(xrange < 2 * yrange) {
        xmin <- min(newdat$x) - xrange * .5
        xmax <- max(newdat$x) + xrange * .5
      } else {
        xmin <- min(newdat$x)
        xmax <- max(newdat$x)
      }

      p <- ggplot2::ggplot() +

        ggiraph::geom_path_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = time,
            tooltip = time,
            data_id = time),
          alpha = .9) +
        ggiraph::geom_point_interactive(
          newdat, mapping = ggplot2::aes(
            x = x, y = y,
            color = time,
            tooltip = time,
            data_id = time),
          size = 1.2) +

        ggplot2::labs(x = "x coordinate",
                      y = "y coordinate") +

        ggplot2::scale_x_continuous(
          labels = scales::comma,
          limits = c(xmin, xmax)) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(ymin, ymax)) +

        viridis::scale_color_viridis(
          name = "Tracking time:",
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
          legend.title = ggplot2::element_text(size = 11),
          legend.key.height = ggplot2::unit(0.3, "cm"),
          legend.key.width = ggplot2::unit(0.6, "cm")
        )

      ggiraph::girafe(
        ggobj = p,
        # width_svg = 7.5, height_svg = 5,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .5),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_selection(type = "none"),
          # ggiraph::opts_selection(
          #   type = "single",
          #   css = paste("fill:#dd4b39;",
          #               "stroke:#eb5644;",
          #               "r:5pt;")),
          # ggiraph::opts_hover_inv(css = "opacity:0.4;"),
          ggiraph::opts_tooltip(
            # css = paste("background-color:gray;",
            #             "color:white;padding:2px;",
            #             "border-radius:2px;"),
            opacity = 1,
            use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill:#1279BF;",
                        "stroke:#1279BF;",
                        "cursor:pointer;"))))

    }) # end of renderGirafe // dataPlot_id

    ## Rendering variogram (svf): ---------------------------------------

    output$dataPlot_svf <- ggiraph::renderGirafe({
      req(vals$svf, vals$data_type != "simulated")

      svf <- vals$svf %>%
        dplyr::slice_min(lag, prop = input$dataVar_timeframe / 100)
      vals$var_fraction <- input$dataVar_timeframe

      p <- plotting_svf(svf, fill = pal$dgr)
      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .5),
          ggiraph::opts_hover(css = paste("fill:#ffbf00;",
                                          "stroke:#ffbf00;"))
        ))

    }) # end of renderGirafe // dataPlot_svf

    # TABLES ------------------------------------------------------------
    ## Table for summary of all individuals: ----------------------------

    output$dataTable_all <- reactable::renderReactable({
      req(vals$dataList, vals$sum)

      reactable::reactable(
        vals$sum,
        selection = "single",
        onClick = "select",
        searchable = TRUE,
        highlight = TRUE,
        compact = FALSE,
        striped = TRUE,

        defaultSelected = match(vals$id, names(vals$dataList)),
        defaultColDef =
          reactable::colDef(
            headerClass = "rtable_header", align = "left"),
        columns = list(
          n = reactable::colDef(name = "\u2014 n"),
          longitude = reactable::colDef(
            format = reactable::colFormat(digits = 3)),
          latitude = reactable::colDef(
            format = reactable::colFormat(digits = 3))))
    })

    observe({
      state <- req(reactable::getReactableState("dataTable_all"))
      vals$table_selection <- state
    })

    ## Table for selected individual data: ------------------------------

    output$dataTable_id <- reactable::renderReactable({
      req(vals$data0, input$show_vars)

      tmpdat <- NULL
      tmpdat <- as_tele_dt(list(vals$data0))
      tmpdat <- tmpdat %>% dplyr::select(input$show_vars)
      if(!is.null(tmpdat$timestamp)) {
        tmpdat$timestamp <- as.character(tmpdat$timestamp)
      } else { NULL }

      reactable::reactable(
        tmpdat,
        compact = FALSE,
        highlight = TRUE,
        striped = TRUE,
        defaultPageSize = 5,
        paginationType = "jump",
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 20),
        showPageInfo = FALSE,
        minRows = 5,

        defaultColDef =
          reactable::colDef(
            headerClass = "rtable_header", align = "right"),

        columns = list(
          timestamp = reactable::colDef(
            minWidth = 150,
            format = reactable::colFormat(datetime = TRUE)),
          x = reactable::colDef(
            minWidth = 90,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 3)),
          y = reactable::colDef(
            minWidth = 90,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 3)),
          longitude = reactable::colDef(
            minWidth = 90,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 3)),
          latitude = reactable::colDef(
            minWidth = 90,
            format = reactable::colFormat(separators = TRUE,
                                          digits = 3)))
      )

    }) # end of renderDataTable // dataTable_id

  }) # moduleServer
}

## To be copied in the UI
# mod_comp_viz_ui("comp_viz_1")

## To be copied in the server
# mod_comp_viz_server("comp_viz_1")
