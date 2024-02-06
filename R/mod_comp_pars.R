#' comp_pars UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_comp_pars_ui <- function(id) {
  ns <- NS(id)
  choose_pars <- c("tau_p", "tau_v")
  names(choose_pars) <- c("Position autocorrelation (\u03C4\u209A)",
                          "Velocity autocorrelation (\u03C4\u1D65)")
  
  tagList(
    tabsetPanel(
      id = ns("parTabs"),
      
      tabPanel(
        value = ns("parPanel_all"),
        title = tagList(
          icon("hourglass", class = "cl-sea"),
          span("Timescales", class = "ttl-panel")
        ),
        
        p(style = "margin-top: 10px;"),
        fluidRow(column(
            width = 12,
            
            shinyWidgets::radioGroupButtons(
              inputId = ns("parInput_type"),
              label = "Show parameter:",
              choices = choose_pars,
              selected = "tau_p",
              checkIcon = list(yes = icon("circle-check")),
              justified = TRUE),
            p(),
            
            ggiraph::girafeOutput(
              outputId = ns("parPlot_all"),
              width = "100%", height = "50%"),
            uiOutput(ns("parUI_legend"))
            
          )) # end of fluidRow
        
      ), # end of panels (1 out of 2)
      
      tabPanel(
        value = ns("parPanel_individual"),
        title = tagList(
          icon("paw", class = "cl-sea"),
          span("Dataset", class = "ttl-panel")
        ),
        
        p(style = "margin-top: 10px;"),
        fluidRow(column(
          width = 12,
          
          # column(
          #   width = 12,
          #   shiny::selectizeInput(
          #     inputId = ns("parInput_id"),
          #     label = "Individual ID:",
          #     choices = "",
          #     selected = NULL,
          #     multiple = FALSE,
          #     options = list(
          #       placeholder = "Pick an individual",
          #       onInitialize = I('function() { this.setValue(""); }'))
          #   )),
          
          uiOutput(ns("parUI_parameters"))
          
        )) # end of fluidRow
      ) # end of panels (2 out of 2)
    ) # end of tabs
    
  ) # end of tagList
}
    
#' comp_pars Server Functions
#'
#' @noRd
mod_comp_pars_server <- function(id, rv, set_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    observe({
      req(length(rv$which_question) == 1)
      
      if (rv$which_question == "Home range")
        shinyWidgets::updateRadioGroupButtons(
          inputId = "parInput_type",
          selected = "tau_p")
      else if (rv$which_question == "Speed & distance")
        shinyWidgets::updateRadioGroupButtons(
          inputId = "parInput_type",
          selected = "tau_v")

    }) # end of observe
    
    observe({
      req(rv$species, rv$datList)
      
      shiny::updateSelectizeInput(
        session,
        inputId = "parInput_id",
        label = "Individual ID:",
        choices = names(rv$datList),
        selected = names(rv$datList)[[1]])
      
    }) %>% # end of observe,
      bindEvent(rv$species)
    
    observe({
      
      if (length(rv$id) == 1) {
        hideTab(inputId = "parTabs", target = ns("parPanel_all"))
        shinyjs::hide(id = "parInput_id")
      } else if (length(rv$id) > 1) {
        showTab(inputId = "parTabs", target = ns("parPanel_all"))
        shinyjs::show(id = "parInput_id")
      }
      
    }) %>% # end of observe,
      bindEvent(rv$id)
    
    ## Render parameters box: ---------------------------------------------
    
    output$parUI_parameters <- renderUI({
      req(rv$data_type, rv$id)
      
      out_note <- span(
        "They will only update if you change the",
        "individual(s) and/or species selected, and then",
        "click the buttons",
        icon("wand-magic-sparkles", class = "cl-mdn"),
        span("Validate", class = "cl-mdn"), "and",
        icon("paper-plane", class = "cl-mdn"),
        wrap_none(span("Extract", class = "cl-mdn"), "."))
      
      if (length(rv$id) == 1) {
        
        if (rv$data_type == "selected") {
          req(rv$tmp$id, rv$tmp$sp, rv$tmp$sp_common)
          out_p <- span(
            "These parameters have been extracted from",
            "individual", span(rv$tmp$id, class = "cl-sea-d"),
            "and species", span(rv$tmp$sp_common, class = "cl-sea-d"),
            wrap_none("(", em(rv$tmp$sp), ")."))
        }
        
        if (rv$data_type == "uploaded") {
          out_p <- span(
            "These parameters have been extracted from",
            "individual", span(rv$tmp$id, class = "cl-sea-d"),
            "and species",
            wrap_none(span(rv$tmp$sp, class = "cl-sea-d"), "."))
        }
        
        out_ui <- column(
          align = "center", width = 12, 
          p(out_p, out_note))
        
      } else {
        out_ui <- column(
          align = "center", width = 12, 
          NULL) #TODO
      }
      
      out <- tagList(
        out_ui,
        column(width = 12, uiOutput(ns("parBlock_process"))),
        
        fluidRow(
          column(width = 6, mod_blocks_ui(ns("parBlock_taup"))),
          column(width = 6, mod_blocks_ui(ns("parBlock_tauv")))),
        
        fluidRow(
          column(width = 6, mod_blocks_ui(ns("parBlock_sigma"))),
          column(width = 6, mod_blocks_ui(ns("parBlock_speed"))))
      )
      
      return(out)

    }) # end of renderUI, "parUI_parameters"
    
    ## Show/hide parameter subtabs: ---------------------------------------
    
    observe({
      req(rv$datList, rv$fitList)
      
      hideTab(inputId = "vizTabs_viz",
              target = ns("parPanel_individual"))
      updateTabsetPanel(session,
                        inputId = "parTabs",
                        selected = ns("parPanel_all"))
      
      if (!is.null(rv$is_valid) && !is.null(rv$sigma)) {
        showTab(inputId = "vizTabs_viz",
                target = ns("parPanel_individual"))
        updateTabsetPanel(session,
                          inputId = "parTabs",
                          selected = ns("parPanel_individual"))
      }
      
    }) # end of observe
    
    # PLOTS ---------------------------------------------------------------
    ## Rendering parameters for all individuals: --------------------------
    
    output$parUI_legend <- renderUI({
      req(rv$which_question, rv$is_valid, input$parInput_type)
      req(rv$datList, rv$fitList)
      req(length(rv$fitList) > 1)
      
      ui <- ui_extra <- NULL
      taup <- extract_pars(rv$fitList, name = "position", meta = TRUE)
      tauv <- extract_pars(rv$fitList, name = "velocity", meta = TRUE)
      
      if (length(rv$which_question) == 2)
        ui_extra <- span(
          "Please select a different individual or dataset if proceeding",
          "with both", span("home range", class = "cl-dgr"), 
          "and",  span("speed/distance", class = "cl-dgr"), "estimation.")
      
      if (input$parInput_type == "tau_p" && is.null(taup)) {
        ui <- tagList(
          p(style = "margin-top: 15px;"),
          span(class = "help-block",
               style = "text-align: justify !important;",
               
               fontawesome::fa("circle-exclamation", fill = pal$dgr),
               span("Note:", class = "help-block-note"), 
               "No significant signature of the animal's",
               span("position autocorrelation", class = "cl-dgr"),
               "parameter remains in this dataset.", ui_extra))
        shinyjs::hide(id = "parPlot_all")
      } else {
        shinyjs::show(id = "parPlot_all")
      }
      
      if (input$parInput_type == "tau_v" && is.null(tauv)) {
        ui <- tagList(
          p(style = "margin-top: 15px;"),
          span(class = "help-block",
               style = "text-align: justify !important;",
               
               fontawesome::fa("circle-exclamation", fill = pal$dgr),
               span("Note:", class = "help-block-note"), 
               "No significant signature of the animal's",
               span("velocity autocorrelation", class = "cl-dgr"),
               "parameter remains in this dataset.", ui_extra))
        shinyjs::hide(id = "parPlot_all")
      } else {
        shinyjs::show(id = "parPlot_all")
      }
      
      return(ui)
      
    }) # end of renderUI, "parUI_legend"
    
    output$parPlot_all <- ggiraph::renderGirafe({
      req(rv$which_question, input$parInput_type)
      req(rv$datList, length(rv$fitList) > 1)
      
      datList <- rv$datList
      fitList <- rv$fitList
      if (!is.null(rv$id))
        if (length(rv$id) != 0) {
          datList <- datList[rv$id]
          fitList <- fitList[rv$id]
        }
      
      if (input$parInput_type == "tau_p") {
        name <- "position"
        x_label <- "Position autocorrelation (in " 
      }
      if (input$parInput_type == "tau_v") {
        name <- "velocity"
        x_label <- "Velocity autocorrelation (in "
      }
      
      capture_meta(fitList, 
                   variable = paste("tau", name),
                   units = FALSE, 
                   verbose = FALSE, 
                   plot = FALSE) -> out
      req(out)
      
      nms <- out$names
      out <- out$meta
      
      pars <- extract_pars(data = datList, obj = fitList, name = name)
      pars <- do.call(rbind, pars)
      pars$m <- rep(nms, each = 3)
      
      pars$variable <- rep(c("low", "est", "high"), length(nms))
      pars <- pars %>% tidyr::pivot_wider(
        names_from = variable,
        values_from = value) %>% 
        dplyr::mutate(m = as.factor(m)) %>%
        dplyr::add_row(
          unit = pars$unit[1],
          m = "All",
          low  = pars$unit[1] %#% out[1, 1],
          est  = pars$unit[1] %#% out[1, 2],
          high = pars$unit[1] %#% out[1, 3])
      
      if (rv$grouped) {
        pars <- pars %>%
          dplyr::mutate(group = dplyr::case_when(
            m %in% unlist(rv$groups[[1]]["A"]) ~ "Group A",
            m %in% unlist(rv$groups[[1]]["B"]) ~ "Group B",
            TRUE ~ ""))
        x_color <- c(pal$sea, "black", "grey50")
        f <- .65
      } else {
        pars <- pars %>% dplyr::mutate(group = ifelse(
          m == "All", "All", "Individuals"))
        x_color <- c(pal$sea, "black")
        x_axis_color <- c(pal$sea, rep("black", length(datList)))
        f <- .3
      }
      x_label <- paste0(x_label, as.character(pars$unit[1]), ")")
      pars$m <- factor(pars$m, levels = c("All", nms))
      
      p.all <- pars %>% 
        ggplot2::ggplot(ggplot2::aes(x = est, y = m, color = group)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_linerange(ggplot2::aes(xmin = low,
                                             xmax = high)) +
        { if (rv$grouped)
          ggplot2::facet_wrap(group ~ ., dir = "v",
                              scales = "free_y") } +
        
        ggplot2::labs(x = x_label, y = "") +
        ggplot2::scale_x_log10() +
        ggplot2::scale_color_manual(values = x_color) +
        
        theme_movedesign() +
        
        { if (!rv$grouped)
          ggplot2::theme(axis.text.y =
                           ggplot2::element_text(
                             color = x_axis_color)) %>%
            suppressWarnings() } +
        ggplot2::theme(legend.position = "none")
      
      ggiraph::girafe(
        ggobj = p.all,
        width_svg = 5.5, height_svg = max(2, length(datList) * f),
        options = list(
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_tooltip(
            opacity = 1,
            use_fill = TRUE),
          ggiraph::opts_hover(
            css = paste("fill: #1279BF;",
                        "stroke: #1279BF;",
                        "cursor: pointer;")))) %>%
        suppressWarnings()

    }) # end of renderGirafe, "parPlot_all"
    
    # BLOCKS --------------------------------------------------------------
    ## Movement process: --------------------------------------------------
    
    output$parBlock_process <- shiny::renderUI({
      req(rv$fitList)
      
      # set_id <- input$parInput_id
      
      set_id <- 1
      if (!is.null(rv$id)) {
        if (length(rv$id) == 1) set_id <- rv$id
      } else set_id <- names(rv$datList) # input$parInput_id
      
      fitList <- rv$fitList
      nms_all <- sapply(seq_along(fitList), function (x)
        sub('(^\\w+)\\s.+','\\1', 
            summary(fitList[[x]])$name[1]))
      nms_all <- unique(nms_all)
      
      subtitle <- NULL
      nms <- nms_all
      if (length(nms_all) > 1) {
        nms <- as.data.frame(nms) %>% 
          dplyr::count(nms) %>%
          dplyr::slice(which.max(n)) %>%
          dplyr::pull(nms)
        subtitle <- toString(nms_all[!nms_all %in% nms])
        if (length(subtitle) == 0) {
          subtitle <- NULL
        } else subtitle <- paste("Others:", subtitle)
      }
      
      parBlock(
        header = fluidRow(
          style = paste("margin-bottom: -14px;"),
          actionButton(
            inputId = ns("selectHelp_mods"),
            icon = icon("circle-question"),
            label = NULL,
            style = paste("background-color: #fff;",
                          "color: black;",
                          "padding: 0;")),
          br(), "Movement process"),
        value = nms,
        subtitle = subtitle)
      
    }) # end of renderUI, "parBlock_process"
    
    ## Timescale parameters: ----------------------------------------------
    
    observe({
      req(rv$tau_p)
      
      mod_blocks_server(
        id = "parBlock_taup", 
        rv = rv, type = "tau", name = "tau_p",
        input_name = list(
          chr = "data_taup",
          html = wrap_none("Position autocorrelation ",
                           "(\u03C4", tags$sub("p"), ")")),
        input_modal = paste0("modal_taup_", set_type))
      
    }) # end of observe
    
    observe({
      req(rv$tau_v)
      
      mod_blocks_server(
        id = "parBlock_tauv",
        rv = rv, type = "tau", name = "tau_v",
        input_name = list(
          chr = "data_tauv",
          html = wrap_none("Velocity autocorrelation ",
                           "(\u03C4", tags$sub("v"), ")")),
        input_modal = paste0("modal_tauv_", set_type))
      
    }) # end of observe
    
    ## Location variance: -------------------------------------------------
    
    observe({
      req(rv$sigma)
      
      mod_blocks_server(
        id = "parBlock_sigma",
        rv = rv, type = "sigma", name = "sigma",
        input_name = list(
          chr = "data_sigma",
          html = wrap_none("Location variance ",
                           "(\u03C3", tags$sub("p"), ")")),
        input_modal = paste0("modal_sigma_", set_type))
      
    }) # end of observe
    
    ## Speed: -------------------------------------------------------------
    
    observe({
      req(rv$speed)
      
      mod_blocks_server(
        id = "parBlock_speed",
        rv = rv, type = "speed", name = "speed",
        input_name = list(
          chr = "data_speed",
          html = wrap_none("Velocity variance (\u03C3", 
                           tags$sub("v"), ")")), 
        input_modal = paste0("modal_speed_", set_type))
      
    }) # end of observe
    
    # MODALS & HELP -------------------------------------------------------
    
    observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Movement models or processes:",
          
          div(class = "no_selection",
              reactable::reactableOutput(ns("parTable_mods"))),
          
          footer = tagList(modalButton("Dismiss")),
          size = "l"))
      
    }) %>% # end of observe,
      bindEvent(input$selectHelp_mods)
    
    output$parTable_mods <- reactable::renderReactable({
      mods <- movedesign::movmods
      
      set_id <- 1
      if (!is.null(rv$id)) {
        if (length(rv$id) != 0) set_id <- rv$id
      } else set_id <- names(rv$datList) # input$parInput_id
      
      fitList <- rv$fitList[set_id]
      
      nms <- sapply(seq_along(fitList), function (x)
        sub('(^\\w+)\\s.+','\\1', 
            summary(fitList[[x]])$name[1]))
      nms <- unique(nms)
      if ("OUf" %in% nms) nms <- nms[nms != "OUf"]
      
      preselected_mod <- NULL
      if (!is.null(match(nms, mods$name_short))) {
        preselected_mod <- match(nms, mods$name_short) 
      }
      
      out <- mods %>% dplyr::select(!.data$name_short)
      
      cell_yn <- function(value) {
        # Render as an X mark or check mark
        if (value == "No") "\u274c No" else "\u2714\ufe0f Yes"
      }
      
      reactable::reactable(
        out,
        searchable = FALSE,
        highlight = TRUE,
        selection = "multiple",
        
        defaultSelected = preselected_mod,
        defaultColDef = reactable::colDef(
          headerClass = "rtable_header",
          align = "left"),
        
        columns = list(
          name = reactable::colDef(
            name = "Movement process",
            minWidth = 195),
          
          tau_p = reactable::colDef(
            minWidth = 60,
            name = paste0("\u03C4","\u209A"),
            cell = cell_yn),
          
          tau_v = reactable::colDef(
            minWidth = 60,
            name = paste0("\u03C4","\u1D65"),
            cell = cell_yn),
          
          hrange = reactable::colDef(
            minWidth = 80,
            name = "Home range",
            cell = cell_yn),
          
          pars = reactable::colDef(
            name = "Parameterization")
        ))
      
    }) # end of renderReactable, "parTable_mods"
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_comp_pars_ui("comp_pars_1")
    
## To be copied in the server
# mod_comp_pars_server("comp_pars_1")
