#' comp_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(

    tabsetPanel(
      id = ns("vizTabs_data"),

      tabPanel(
        value = ns("vizPanel_all"),
        title = tagList(
          icon("paw", class = "cl-sea"),
          span("Dataset", class = "ttl-panel")
        ),

        p(style = "margin-top: 10px;"),
        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-7",
            reactable::reactableOutput(ns("vizTable_all"))
        ),

        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-5",
            ggiraph::girafeOutput(
              outputId = ns("vizPlot_all"),
              width = "100%", height = "100%")
        )

      ), # end of panels (1 out of 5)
      
      tabPanel(
        value = ns("vizPanel_groups"),
        title = tagList(
          icon("object-ungroup", class = "cl-jgl"),
          span("Groups", class = "ttl-panel cl-jgl")
        ),
        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
            p(style = "margin-top: 10px;"),
            uiOutput(ns("select_groups")),
            uiOutput(ns("selectUI_legend_groups")))
        
      ), # end of panels (2 out of 5)

      tabPanel(
        value = ns("vizPanel_individual"),
        title = tagList(
          icon("filter", class = "cl-sea"),
          span("Individual", class = "ttl-panel")
        ),
        
        p(style = "margin-top: 10px;"),
        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-6",
            
            shiny::selectizeInput(
              inputId = ns("vizInput_id"),
              label = "Individual ID:",
              choices = "",
              selected = NULL,
              multiple = FALSE,
              options = list(
                placeholder = "Pick an individual",
                onInitialize = I('function() { this.setValue(""); }'))
            ),
            
            ggiraph::girafeOutput(
              outputId = ns("vizPlot_id"),
              width = "100%", height = "100%")
        ),

        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-6",
            reactable::reactableOutput(ns("vizTable_id")),
            uiOutput(ns("vizTable_showVars"))
        )

      ), # end of panels (3 out of 5)
      
      tabPanel(
        value = ns("vizPanel_outlier"),
        title = tagList(
          icon("bug", class = "cl-sea"),
          span("Outliers", class = "ttl-panel")
        ),
        
        p(),
        ggiraph::girafeOutput(
          outputId = ns("vizPlot_outlier"),
          width = "100%", height = "100%")
        
      ), # end of panels (4 out of 5)
      
      tabPanel(
        value = ns("vizPanel_svf"),
        title = tagList(
          icon("chart-line", class = "cl-sea"),
          span("Variogram", class = "ttl-panel")
        ),
        
        p(),
        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-8",
            ggiraph::girafeOutput(
              outputId = ns("vizPlot_svf"),
              width = "100%", height = "100%")),

        div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-4",
            column(
              width = 12, align = "center",
              p(),
              shiny::sliderInput(
                ns("vizInput_fraction"),
                label = "Proportion of variogram plotted:",
                min = 0, max = 100, value = 50, step = 5,
                post = "%",
                width = "100%"),
              p(),
              shinyWidgets::awesomeCheckbox(
                inputId = ns("vizInput_add_fit"),
                label = span(
                  "Add", span("model fit", class = "cl-sea"),
                  "to variogram", icon("wrench")),
                value = FALSE),
              p()
            ))

      ) # end of panels (5 out of 5)
    ) # end of tabs

  )
}

#' comp_viz Server Functions
#'
#' @noRd
mod_viz_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    id_debounced <- reactive({
      state <- reactable::getReactableState("vizTable_all")
      if (is.null(state$selected)) return(NULL)
      
      req(state$selected)
      id <- names(rv$datList)[state$selected]
      if (identical(id, character(0)) || any(is.na(id))) return(NULL)
      else return(id)
    }) %>% debounce(1000)
    
    observe({
      req(rv$datList, rv$data_type != "simulated")
      if (rv$active_tab == 'data_select') req(rv$data_type == "selected")
      if (rv$active_tab == 'data_upload') req(rv$data_type == "uploaded")
      
      rv$id <- id_debounced()
      rv$status <- FALSE
      
    }) # end of observe
    
    observe({
      req(rv$which_meta)
      
      if (rv$which_meta != "compare") {
        rv$grouped <- FALSE
      } else {
        if (length(req(input$set_groups)$A) != 0 &&
            length(req(input$set_groups)$B) != 0) {
          rv$groups[[1]] <- input$set_groups
          rv$grouped <- TRUE
        }
      }
      
    }) # end of observe
    
    ## Summarize data: ----------------------------------------------------
    
    output$vizTable_showVars <- renderUI({
      req(rv$input_x,
          rv$input_y,
          rv$input_t,
          rv$datList,
          input$vizInput_id)
      
      shinyWidgets::pickerInput(
        inputId = ns("show_vars"),
        width = "100%",
        label = span("Columns to show above:",
                     class = "txt-label"),
        choices = names(rv$datList[[input$vizInput_id]]),
        selected = c(rv$input_x,
                     rv$input_y,
                     rv$input_t),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ), multiple = TRUE)
      
    }) # end of renderUI // vizTable_showVars
    
    ## Select individuals for groups: -------------------------------------
    
    observe({
      hideTab(inputId = "vizTabs_data",
              target = ns("vizPanel_groups"))
      if ("compare" %in% req(rv$which_meta))
        showTab(inputId = "vizTabs_data",
                target = ns("vizPanel_groups"))
    }) # end of observe
    
    observe({
      if (length(req(rv$datList)) >= 2)
        shinyjs::show(id = "select_groups")
      else shinyjs::hide(id = "select_groups")
      
    }) # end of observe
    
    output$select_groups <- renderUI({
      req(rv$datList)
      
      if (is.null(rv$id)) {
        ui <- tagList(
          p(style = "margin-top: 35px;"),
          span(class = "help-block",
               style = "text-align: center !important;",
               
               fontawesome::fa("circle-exclamation", fill = pal$dgr),
               span("Note:", class = "help-block-note"),
               "Please select all the individuals from the",
               fontawesome::fa("stopwatch", fill = pal$sea),
               span("Dataset", class = "cl-sea"), "box",
               "which you intend to assign into groups."),
          p(style = "margin-bottom: 35px;"))
        
      } else {
        req(rv$id)
        m <- length(rv$datList)
        set_id <- rv$id
        
        ui <- chooserInput(ns("set_groups"), 
                           leftLabel = "Group A", 
                           rightLabel = "Group B",
                           leftChoices = set_id, 
                           rightChoices = c(),
                           size = 4, 
                           multiple = TRUE)
      }
      
      return(ui)
      
    }) %>% # end of renderUI, "select_groups",
      bindEvent(c(rv$datList, rv$id))
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    observe({
      req(rv$active_tab, rv$is_valid)
      req(rv$active_tab == 'data_select' ||
         rv$active_tab == 'data_upload')
        
      shinyjs::hide(id = "vizInput_add_fit")
      if (!is.null(rv$fitList) && !is.null(rv$svfList[[1]]$fit)) 
        shinyjs::show(id = "vizInput_add_fit")
      
    }) # end of observe
    
    observe({
      req(rv$is_valid,
          rv$datList, rv$id)
      
      tabselected <- NULL
      
      if ("compare" %in% req(rv$which_meta)) {
        if (rv$active_tab == 'data_upload')
          tabselected <- "comp_viz_uploaded-vizPanel_groups"
        
        if (rv$active_tab == 'data_select')
          tabselected <- "comp_viz_selected-vizPanel_groups"
      } else {
        if (rv$is_valid && length(rv$datList[rv$id]) > 1) {
          if (rv$active_tab == 'data_upload')
            tabselected <- "comp_viz_uploaded-vizPanel_individual"
          
          if (rv$active_tab == 'data_select')
            tabselected <- "comp_viz_selected-vizPanel_individual"
        }
      }
        
      req(tabselected)
      updateTabsetPanel(
        session,
        inputId = "vizTabs_data",
        selected = tabselected)
      
    }) %>% # end of observe,
      bindEvent(rv$is_valid)
    
    observe({
      req(rv$datList)
      
      shiny::updateSelectizeInput(
          session,
          inputId = "vizInput_id",
          label = "Individual ID:",
          choices = names(rv$datList),
          selected = names(rv$datList)[[1]])
      
    }) %>% # end of observe,
      bindEvent(rv$datList)
    
    observe({
      req(rv$active_tab == 'data_upload' ||
          rv$active_tab == 'data_select')
      
      if (length(rv$id) == 1) shinyjs::hide(id = "vizInput_id")
      else shinyjs::show(id = "vizInput_id")
    }) %>% # end of observe,
      bindEvent(rv$id)
    
    ## Rendering legend for groups: -------------------------------------
    
    estimating_initial_hr <- reactive({
      
      req(rv$datList, rv$fitList)
      req(rv$which_question, rv$grouped, rv$is_valid)
      req(length(rv$groups[[1]][["A"]]) > 0,
          length(rv$groups[[1]][["B"]]) > 0)
      
      loading_modal("Detecting subpopulations")
      
      start_hr <- Sys.time()
      if ((length(rv$datList) == 1) ||
          (rv$grouped && length(rv$datList) == 2)) {
        
        hr <- tryCatch(
          ctmm::akde(rv$datList[[1]], rv$fitList[[1]]),
          warning = function(w) NULL,
          error = function(e) NULL)
        hr <- list(hr)
        
        if (rv$grouped) {
          hr2 <- tryCatch(
            ctmm::akde(rv$datList[[2]], rv$fitList[[2]]),
            warning = function(w) NULL,
            error = function(e) NULL)
          hr <- list(hr[[1]], hr2)
        }
        
      } else {
        
        hr <- par.akde(
          rv$datList,
          rv$fitList,
          parallel = rv$parallel)
        
      }
      
      shinybusy::remove_modal_spinner()
      
      return(hr)
      
    }) %>% # end of reactive, estimating_initial_hr()
      bindCache(rv$species,
                rv$id,
                rv$datList,
                length(rv$fitList))
    
    
    get_meta_inputs <- reactive({
      datList <- rv$datList
      fitList <- rv$fitList
      
      input <- .get_groups(fitList, groups = rv$groups[[1]])
      txt_target <- list(
        "hr" = c("area", "home range area"),
        "ctsd" = c("speed", "movement speed"))
      
      out <- lapply(rv$set_target, function(x) {
        .capture_meta(input,
                      variable = txt_target[[x]][[1]],
                      units = FALSE,
                      verbose = TRUE,
                      plot = FALSE) %>% 
          suppressMessages() %>%
          quiet()
      })
      
      if (length(rv$set_target) == 1) {
        target_map <- list(
          "hr" = list("hr" = out[["hr"]], "ctsd" = NULL),
          "ctsd" = list("hr" = NULL, "ctsd" = out[["ctsd"]]))
        
        outList <- list(
          "intro" = target_map[[rv$set_target]],
          "final" = list("hr" = NULL, "ctsd" = NULL),
          "is_final" = FALSE)
        
      } else {
        
        outList <- list(
          "intro" = list("hr" = out[["hr"]], "ctsd" = out[["ctsd"]]),
          "final" = list("hr" = NULL, "ctsd" = NULL),
          "is_final" = FALSE)
      }
      
      rv$metaList_groups <- outList

      return(out)
      
    }) %>% # end of reactive, "get_meta_inputs",
      bindCache(c(
        rv$which_question, 
        rv$datList,
        rv$groups[[1]]))
    
    output$selectUI_legend_groups <- renderUI({
      req(rv$datList, rv$fitList)
      req(rv$which_question, rv$set_target, rv$grouped, rv$is_valid)
      req(length(rv$groups[[1]][["A"]]) > 0,
          length(rv$groups[[1]][["B"]]) > 0)
      req(length(rv$fitList) > 0)
      
      txt_target <- list(
        "hr" = c("area", "home range area"),
        "ctsd" = c("speed", "movement speed"))
      
      out <- get_meta_inputs()
      detected <- sapply(out, function(x) x$logs$subpop_detected)
      
      if (length(detected) == 1) {
        
        if (detected[1]) {
          txt_detected <- tagList(span(
            "Sub-populations were", span("correctly", class = "cl-sea"),
            "detected for", txt_target[[rv$set_target]][[2]],
            "with the current groups."))
        } else if (!detected[1]) {
          txt_detected <- tagList(span(
            "No sub-populations detected with the current groups",
            "for", wrap_none(txt_target[[rv$set_target]][[2]], "."),
            "Proceed with", wrap_none(span("caution", 
                                           class = "cl-dgr"), ".")))
        }
        
      } else {
        if (detected[["hr"]] && detected[["ctsd"]]) {
          txt_detected <- tagList(span(
            "Sub-populations were correctly detected for",
            span("home range area", class = "cl-sea"),
            "and for", span("movement speed", class = "cl-sea"),
            "with the current groups."))
        } else if (detected[["hr"]] && !detected[["ctsd"]]) {
          txt_detected <- tagList(span(
            "Sub-populations were correctly detected for",
            span("home range area", class = "cl-sea"),
            "but not for", span("movement speed", class = "cl-dgr"),
            "with the current groups."))
        } else if (!detected[["hr"]] && detected[["ctsd"]]) {
          txt_detected <- tagList(span(
            "Sub-populations were correctly detected for",
            span("movement speed", class = "cl-sea"),
            "but not for", span("home range area", class = "cl-dgr"),
            "with the current groups."))
        } else if (!detected[["hr"]] && !detected[["ctsd"]]) {
          txt_detected <- tagList(span(
            "No sub-populations detected with the current groups",
            "for", span("home range area", class = "cl-dgr"),
            "or for", 
            wrap_none(span("movement speed", class = "cl-dgr"), "."),
            "Proceed with", wrap_none(span("caution", 
                                           class = "cl-dgr"), ".")))
        }
      }
      
      ui <- tagList(
        p(style = "margin-top: 35px;"),
        span(class = "help-block",
             style = "text-align: justify !important;",
             
             fontawesome::fa("circle-exclamation", fill = pal$dgr),
             span("Note:", class = "help-block-note"),
             txt_detected
        ))
      
      return(ui)
      
    }) # end of renderUI, "selectUI_legend_groups"
    
    # PLOTS -------------------------------------------------------------
    ## Rendering all data (xy): -----------------------------------------

    observe({
      rv$status <- TRUE
    }) %>% bindEvent(input$vizPlot_all_selected)
    
    output$vizPlot_all <- ggiraph::renderGirafe({
      if (rv$active_tab == 'data_select') req(rv$data_type == "selected")
      if (rv$active_tab == 'data_upload') req(rv$data_type == "uploaded")

      req(rv$datList)
        
      datList <- rv$datList
      newdat.all <- telemetry_as_df(datList)
      
      req(all(!is.na(newdat.all$x),
              !is.na(newdat.all$y)))

      yrange <- diff(range(newdat.all$y))
      xrange <- diff(range(newdat.all$x))

      yrange_factor <- ifelse(yrange < 1.5 * xrange, 0.3,
                              ifelse(yrange < 2 * xrange, 0.5, 0))
      xrange_factor <- ifelse(xrange < 2 * yrange, 0.5, 0)

      ymin <- min(newdat.all$y) - yrange * yrange_factor
      ymax <- max(newdat.all$y) + yrange * yrange_factor
      xmin <- min(newdat.all$x) - xrange * xrange_factor
      xmax <- max(newdat.all$x) + xrange * xrange_factor

      id <- NULL
      if (!is.null(rv$id)) {
        if (length(rv$id) > 1) id <- rv$id
        else { if (rv$id != "") id <- rv$id }
      }
      
      p.all <- ggplot2::ggplot() +
        ggiraph::geom_point_interactive(
          data = newdat.all,
          ggplot2::aes(x = .data$x,
                       y = .data$y,
                       color = .data$id,
                       tooltip = .data$id,
                       data_id = .data$id),
          size = 1.2) +
        ggplot2::labs(x = "x coordinate",
                      y = "y coordinate") +

        ggplot2::scale_x_continuous(
          labels = scales::comma,
          limits = c(xmin, xmax)) +
        ggplot2::scale_y_continuous(
          labels = scales::comma,
          limits = c(ymin, ymax)) +
        ggplot2::scale_color_grey() +

        theme_movedesign(font_available = rv$is_font) +
        ggplot2::theme(legend.position = "none")
      
      ggiraph::girafe(
        ggobj = p.all,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .5),
          ggiraph::opts_zoom(max = 5),
          ggiraph::opts_hover(
            css = paste("fill: #06c6ca;",
                        "stroke: #06c6ca;")),
          ggiraph::opts_selection(
            selected = id,
            type = "multiple",
            css = paste("alpha: .5;",
                        "fill: #009da0;",
                        "stroke: #009da0;"))))
      
    }) # end of renderGirafe
    
    ## Rendering individual data (xy): ----------------------------------

    output$vizPlot_id <- ggiraph::renderGirafe({
      if (rv$active_tab == 'data_select') req(rv$data_type == "selected")
      if (rv$active_tab == 'data_upload') req(rv$data_type == "uploaded")
      
      req(rv$datList,
          rv$input_x,
          rv$input_y,
          rv$input_t)
      
      set_id <- 1
      if (!is.null(rv$id)) {
        if (length(rv$id) == 1) set_id <- rv$id
      }
      
      if (!is.null(input$vizInput_id))
        set_id <- input$vizInput_id
      
      dat <- rv$datList[[set_id]]
      req(all(!is.na(dat$x), !is.na(dat$y), !is.null(dat$timestamp)))
      
      newdat <- data.frame(x = dat[[rv$input_x]],
                           y = dat[[rv$input_y]],
                           t = dat[[rv$input_t]]) %>% 
        dplyr::mutate(t = as.POSIXct(t, format = "%Y-%m-%d %H:%M:%S"))
      
      yrange <- diff(range(newdat$y))
      xrange <- diff(range(newdat$x))
      
      yrange_factor <- ifelse(yrange < 1.5 * xrange, 0.3,
                              ifelse(yrange < 2 * xrange, 0.5, 0))
      xrange_factor <- ifelse(xrange < 2 * yrange, 0.5, 0)
      
      ymin <- min(newdat$y) - yrange * yrange_factor
      ymax <- max(newdat$y) + yrange * yrange_factor
      xmin <- min(newdat$x) - xrange * xrange_factor
      xmax <- max(newdat$x) + xrange * xrange_factor
      
      rv$status <- TRUE
      
      p <- ggplot2::ggplot(
        data = newdat, 
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          color = .data$t,
          tooltip = .data$t,
          data_id = .data$t)) +

        ggplot2::geom_path(alpha = .9) +
        ggiraph::geom_point_interactive(size = 1.2) +

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
          breaks = c(min(newdat$t),
                     max(newdat$t)),
          labels = c("Start", "End")) +

        theme_movedesign(font_available = rv$is_font) +
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
        width_svg = 5.5, height_svg = 5,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE, width = .5),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_tooltip(
            opacity = 1,
            use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill: #1279BF;",
                        "stroke: #1279BF;",
                        "cursor: pointer;"))))
      
    }) # end of renderGirafe // vizPlot_id
    
    ## Rendering outlier plot: --------------------------------------------
    
    output$vizPlot_outlier <- ggiraph::renderGirafe({
      if (rv$active_tab == 'data_select') req(rv$data_type == "selected")
      if (rv$active_tab == 'data_upload') req(rv$data_type == "uploaded")
      req(rv$datList, rv$svfList, rv$id)
      
      m <- length(rv$datList)
      dat <- rv$datList

      if (!is.null(rv$id)) {
        if (length(rv$id) == 0) dat <- dat
        else if (length(rv$id) == 1) dat <- dat[1]
        else dat <- dat[rv$id]
      }
      
      out <- plotting_outliers(dat, rv$is_font)
      ft_size <- ifelse(m == 1, 13, ifelse(m >= 10, 6, 11))
      
      # shinyFeedback::showToast(
      #   type = "success",
      #   message = "Outlier plots completed!",
      #   .options = list(
      #     timeOut = 3000,
      #     extendedTimeOut = 3500,
      #     progressBar = FALSE,
      #     closeButton = TRUE,
      #     preventDuplicates = TRUE,
      #     positionClass = "toast-bottom-right"))
      
      ggiraph::girafe(
        ggobj = suppressWarnings(
          ggpubr::ggarrange(plotlist = out$plot)),
        options = list(
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_sizing(rescale = TRUE, width = .5),
          ggiraph::opts_hover(css = paste("fill: #ffbf00;",
                                          "stroke: #ffbf00;"))
        ))

    }) # end of renderGirafe // vizPlot_outlier
    
    ## Rendering variogram (svf): ---------------------------------------
    
    output$vizPlot_svf <- ggiraph::renderGirafe({
      if (rv$active_tab == 'data_select') req(rv$data_type == "selected")
      if (rv$active_tab == 'data_upload') req(rv$data_type == "uploaded")
      req(rv$datList, rv$svfList, rv$id)
      req(length(rv$svfList) == length(rv$datList[rv$id]))
      
      svf <- rv$svfList

      if (!is.null(rv$id)) {
        if (length(rv$id) == 0) svf <- svf
        else if (length(rv$id) == 1) svf <- svf[1]
        else svf <- svf[rv$id]
      }
      
      p <- plotting_svf(
        svf, fill = rep(pal$dgr, length(rv$datList[rv$id])),
        add_fit = ifelse(is.null(input$vizInput_add_fit),
                         FALSE, input$vizInput_add_fit),
        fraction = input$vizInput_fraction / 100,
        font_available = rv$is_font)
      
      ggiraph::girafe(
        ggobj = suppressWarnings(ggpubr::ggarrange(plotlist = p)),
        options = list(
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_sizing(rescale = TRUE, width = .5),
          ggiraph::opts_hover(css = paste("fill: #ffbf00;",
                                          "stroke: #ffbf00;"))
        ))
      
    }) # end of renderGirafe // vizPlot_svf
    
    # TABLES ------------------------------------------------------------
    ## Table for summary of all individuals: ----------------------------

    output$vizTable_all <- reactable::renderReactable({
      req(rv$datList, rv$which_meta)
      
      if (rv$active_tab == 'data_select') req(rv$data_type == "selected")
      if (rv$active_tab == 'data_upload') req(rv$data_type == "uploaded")
      
      id <- NULL
      if (!is.null(rv$id)) {
        req(all(!is.na(rv$id)))
        if (length(rv$id) > 1) id <- match(rv$id, names(rv$datList))
        else { if (rv$id != "") id <- match(rv$id, names(rv$datList)) }
      }
      
      if (rv$status && !is.null(input$vizPlot_all_selected)) {
        id <- match(input$vizPlot_all_selected, names(rv$datList))
      } else if (is.null(input$vizPlot_all_selected))
        id <- NULL
      
      out_sum <- summary(rv$datList)
      out_sum$n <- sapply(rv$datList, function(x) nrow(x))
      
      sum_col1 <- grep("period", names(out_sum))
      sum_col2 <- grep("interval", names(out_sum))
      out_sum[, sum_col1] <- round(out_sum[sum_col1], 1)
      out_sum[, sum_col2] <- round(out_sum[sum_col2], 1)
      out_sum <- dplyr::select(out_sum,
                               -c(.data$longitude, .data$latitude))
      
      if (!is.null(rv$fitList)) {
        
        out_sum$mod <- lapply(rownames(out_sum), function(name) {
          if (name %!in% names(rv$fitList)) { return("N/A") } else {
            stringr::word(summary(rv$fitList[[name]])$name, 1)
          }
        })
        
        out_sum$N_area <- lapply(rownames(out_sum), function(name) {
          if (name %!in% names(rv$fitList)) { return("N/A") } else {
            round(
              do.call(c, 
                      extract_dof(rv$fitList[[name]],
                                  name = "area")), 1)
          }
        })
        
        out_sum$N_speed <- lapply(rownames(out_sum), function(name) {
          if (name %!in% names(rv$fitList)) { return("N/A") } else {
            round(
              do.call(c, 
                      extract_dof(rv$fitList[[name]],
                                  name = "speed")), 1)
          }
        })
        
      } # end of if (!is.null(rv$fitList))
      
      if (rv$grouped) {
        if (!is.null(rv$groups[[2]]))
          if (!is.null(rv$groups[[2]][["A"]]) &&
              !is.null(rv$groups[[2]][["B"]])) {
            
            out_sum$group <- sapply(names(rv$datList), function(x)
              ifelse(x %in% rv$groups[[2]][["A"]], "A", "B"))
          }
        
      } # end of if (rv$grouped)
      
      is_selection <- "multiple"
      if (rv$which_meta == "none") {
        is_selection <- "single"
        if(length(id) > 1) id <- NULL
      }
      
      if (rv$which_meta == "compare") {
        if (!is.null(rv$groups[[2]][["A"]]))
        out_sum <- dplyr::relocate(out_sum, .data$group)
      }
      
      if (anyNA(id)) id <- NULL
      
      reactable::reactable(
        out_sum,
        onClick = "select",
        selection = is_selection,
        searchable = TRUE,
        highlight = TRUE,
        compact = FALSE,
        striped = TRUE,
        
        defaultSelected = id,
        defaultColDef =
          reactable::colDef(
            headerClass = "rtable_header", 
            align = "left",
            minWidth = 120),
        columns = list(
          n = reactable::colDef(
            name = "n",
            minWidth = 60,
            style = format_num,
            format = reactable::colFormat(
              separators = TRUE, locale = "en-US", digits = 0)),
          mod = if ("mod" %in% names(out_sum)) {
            reactable::colDef(
              minWidth = 80, name = "Model:") },
          group = if (rv$grouped) {
            reactable::colDef(
              name = "Group:",
              minWidth = 80) },
          longitude = reactable::colDef(
            format = reactable::colFormat(digits = 3)),
          latitude = reactable::colDef(
            format = reactable::colFormat(digits = 3)),
          N_area = if ("N_area" %in% names(out_sum)) {
            reactable::colDef(
              minWidth = 90, name = "N (area)",
              style = format_num,
              format = reactable::colFormat(separators = TRUE,
                                            digits = 1)) },
          N_speed = if ("N_speed" %in% names(out_sum)) {
            reactable::colDef(
              name = "N (speed)",
              minWidth = 90,
              style = format_num,
              format = reactable::colFormat(separators = TRUE,
                                            digits = 1)) }
        ))
      
    }) # end of renderReactable
    
    ## Table for selected individual data: ------------------------------
    
    output$vizTable_id <- reactable::renderReactable({
      if (rv$active_tab == 'data_select') req(rv$data_type == "selected")
      if (rv$active_tab == 'data_upload') req(rv$data_type == "uploaded")
      req(rv$datList, input$show_vars)
      req(!anyNA(names(rv$datList)))
      
      set_id <- 1
      if (!is.null(rv$id)) {
        if (length(rv$id) == 1) set_id <- rv$id
      } else set_id <- input$vizInput_id
      
      req(rv$datList[set_id][[1]])
      tmpdat <- NULL
      tmpdat <- tele_to_dt(rv$datList[set_id])
      
      tmpdat <- tmpdat %>% dplyr::select(input$show_vars)
      if (!is.null(tmpdat$timestamp)) {
        tmpdat$timestamp <- as.POSIXct(
          strptime(tmpdat$timestamp, format = "%Y-%m-%d %H:%M"))
        tmpdat$timestamp <- as.character(tmpdat$timestamp)
      } else { NULL }
      
      format_column <- reactable::colFormat(separators = TRUE,
                                            digits = 3)

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
          timestamp = reactable::colDef(minWidth = 150),
          # format = reactable::colFormat(datetime = TRUE)),
          x = reactable::colDef(
            minWidth = 90, format = format_column),
          y = reactable::colDef(
            minWidth = 90, format = format_column),
          longitude = reactable::colDef(
            minWidth = 90, format = format_column),
          latitude = reactable::colDef(
            minWidth = 90, format = format_column))
      )

    }) # end of rendervizTable, "vizTable_id"

  }) # end of moduleServer
}

## To be copied in the UI
# mod_viz_ui("comp_viz_1")

## To be copied in the server
# mod_viz_server("comp_viz_1")
