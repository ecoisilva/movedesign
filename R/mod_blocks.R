#' blocks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_blocks_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("block"))
  ) # end of tagList
}
    
#' block_pars Server Functions
#'
#' @noRd 
mod_blocks_server <- function(id,
                              vals, 
                              type,
                              name = NULL, 
                              data = NULL,
                              fit = NULL,
                              input_name = NULL, 
                              input_modal = NULL,
                              class = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # REACTIVES -----------------------------------------------------------
    
    check_type <- reactive({
      
      dplyr::case_when(
        type %in% c("tau", "sigma", "speed") ~ "species",
        type %in% c("dur", "dti") ~ "design",
        type %in% c("n", "N") ~ "metrics",
        type %in% c("hr", "ctsd", "dist") ~ "outputs"
        
      ) # end of case_when
      
    }) # end of reactive, check_type()
    
    prepare_outputs <- reactive({
      
      block_type <- check_type()
      switch(block_type,
             
             # Timescale, spatial parameters and velocity:
             "species" = {
               
               out <- vals[[name]]
               
               if (type == "tau" || type == "speed") {
                 est <- fix_unit(out["est", 1], out$unit[1])
                 lci <- scales::label_comma(.1)(out["low", 1])
                 uci <- scales::label_comma(.1)(out["high", 1])
               }
               
               if (type == "sigma") {
                 
                 out <- fix_unit(out, convert = TRUE, ui = TRUE)
                 lci <- out[1,1]
                 est <- out[2, ]
                 uci <- out[3,1]
               }
               
               if (type == "tau") {
                 value <- paste(
                   scales::label_comma(.1)(est$value), est$unit)
               } else {
                 value <- span(HTML(
                   "&nbsp;", 
                   scales::label_comma(.1)(est$value), est$unit))
               }
               
               subtitle <- NULL
               if (!is.na(lci) && !is.na(uci)) {
                 subtitle <- paste(ifelse(lci == 0, "0", lci), 
                                   "\u2014", uci)
               }
               perc <- NULL
               
             }, # end of type == "species"
             
             # Tracking regime:
             "design" = {
               
               if (type == "dur") {
                 tmpname <- "period" 
                 subtitle <- NULL
               } else if (type == "dti") {
                 tmpname <- "interval"
                 subtitle <- "between fixes"
               }
               
               tmpvalue <- extract_pars(data, name = tmpname)
               tmpvalue <- fix_unit(tmpvalue$value, tmpvalue$unit)
               value <- paste(tmpvalue$value, tmpvalue$unit)
               perc <- NULL
               
             }, # end of type == "design"
             
             # Sample sizes:
             "metrics" = {
               
               N <- extract_dof(fit, par = name)
               n <- nrow(data)
               
               value <- round(N, 1)
               perc <- paste0(
                 "-", round((100 - ((N * 100) / n)), 1), "%")
               subtitle <- HTML(paste0("(N", tags$sub(name), ")"))
               
             }, # end of type == "metrics"
             
             # Outputs from analyses:
             "outputs" = {
               
               out <- vals[[name]]
               
               if (grepl("Est", name)) {
                 out <- fix_unit(out, ui = TRUE, convert = TRUE)
                 
                 if (type == "ctsd")
                   out$unit[2] <- abbrv_unit(out$unit[2])
                 
                 if ((out$value[2] %% 1) * 10 == 0) {
                   value <- scales::label_comma(1)(out$value[2])
                 } else {
                   value <- scales::label_comma(.1)(out$value[2])
                 }
                 
                 value <- paste(value, out$unit[2])
                 subtitle <- paste(round(out$value[1], 1),
                                   "\u2014", round(out$value[3], 1))
               }
               
               if (grepl("Err", name)) {
                 value <- out$value
                 subtitle <- NULL
               }
               
               perc <- NULL
               
             }, # end of type == "metrics"
             
             stop(paste0("No handler for ", type, "."))
             
      ) # end of switch
      
      return(list(
        value = value,
        subtitle = subtitle,
        perc = perc 
      ))
      
    }) # end of reactive, prepare_outputs()
    
    # DYNAMIC UI ELEMENTS -------------------------------------------------
    
    output$block <- shiny::renderUI({
      block_type <- check_type()
      
      ## Species parameters: ----------------------------------------------
      
      if (block_type == "species") {
        
        if (is.null(input_modal)) {
        out_block <- parBlock(
          header = input_name[["html"]],
          value = prepare_outputs()[["value"]],
          subtitle = prepare_outputs()[["subtitle"]])
        
        } else {
          out_block <- parBlock(
            header = shiny::fluidRow(
              style = paste("margin-bottom: -14px;"),
              actionButton(
                inputId = ns(paste0("help_", input_name[["chr"]])),
                icon = icon("circle-question"),
                label = NULL,
                style = paste("background-color: #fff;",
                              "color: black;",
                              "padding: 0;")) %>%
                bsplus::bs_attach_modal(id_modal = input_modal),
              br(),
              input_name[["html"]]
            ),
            value = prepare_outputs()[["value"]],
            subtitle = prepare_outputs()[["subtitle"]])
        }
        
      } # end of block_type == "species"
      
      ## Sampling design: -------------------------------------------------
      
      if (block_type == "design") {
        req(data)
        
        value <- prepare_outputs()[["value"]]
        subtitle <- prepare_outputs()[["subtitle"]]
        
        if (!is.null(class)) {
          value <- span(value, class = class)
          subtitle <- span(subtitle, class = class)
        }
        
        out_block <- parBlock(
          header = input_name,
          value = value,
          subtitle = subtitle)
        
      } # end of block_type == "design"

      ## Sample sizes: ----------------------------------------------------
      
      if (block_type == "metrics") {
        req(data, fit)
        
        out_block <- sampleBlock(
          number = prepare_outputs()[["perc"]],
          numberIcon = TRUE,
          header = prepare_outputs()[["value"]],
          line1 = "Effective sample size",
          line2 = prepare_outputs()[["subtitle"]],
          rightBorder = FALSE,
          marginBottom = FALSE)
        
      } # end of block_type == "metrics"
      
      ## Outputs: ---------------------------------------------------------
      
      if (block_type == "outputs") {
        
        value <- prepare_outputs()[["value"]]
        subtitle <- prepare_outputs()[["subtitle"]]
        
        if (!is.null(class)) {
          value <- span(value, class = class)
          subtitle <- span(subtitle, class = class)
        }
        
        ### Estimates: ----------------------------------------------------
        
        if (grepl("Est", name)) {
          switch(type,
                 "hr" = {
                   header <- "Estimate" 
                   icon <- "bullseye"
                 },
                 "ctsd" = {
                   txt <- ifelse(grepl("new", name), "new", "initial")
                   header <- span("Movement speed from", br(),
                                  txt, "design:")
                   icon <- "gauge-high"
                 },
                 "dist" = {
                   txt <- ifelse(grepl("new", name), "new", "initial")
                   header <- span("Total distance traveled from", br(),
                                  txt, "design:")
                   icon <- "map-location-dot"
                 }
          )
          
          out_block <- parBlock(
            icon = icon,
            header = header,
            value = value,
            subtitle = subtitle)
          
        } # end of estimate
        
        ### Error: --------------------------------------------------------
        
        if (grepl("Err", name)) {
          
          out_block <- errorBlock(
            icon = "radiation",
            text = "Expected error",
            value = prepare_outputs()[["value"]][[2]],
            min = prepare_outputs()[["value"]][[1]],
            max = prepare_outputs()[["value"]][[3]],
            rightBorder = FALSE) 
          
        } # end of error
        
      } # end of block_type == "outputs"
      
      return(out_block)
      
    }) # end of renderUI
    
    
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_blocks_ui("block_pars_1")
    
## To be copied in the server
# mod_blocks_server("block_pars_1")
