#' blocks UI Function
#'
#' @description Create UI blocks.
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
                              rv, 
                              type,
                              name = NULL,
                              get_id = 1,
                              get_group = 1,
                              n = NULL,
                              N = NULL,
                              data = NULL,
                              obj = NULL,
                              input_name = NULL, 
                              input_modal = NULL,
                              class = NULL,
                              options = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # REACTIVES -----------------------------------------------------------
    
    get_block_type <- reactive({
      
      out <- dplyr::case_when(
        type %in% c("tau", "sigma", "speed") ~ "species",
        type %in% c("dur", "dti") ~ "design",
        type %in% c("n", "N", "m") ~ "metrics",
        type %in% c("hr", "ctsd", "dist") ~ "outputs",
        TRUE ~ NA
      ) # end of case_when
      
      if (is.na(out)) return(NULL)
      else return(out)
      
    }) # end of reactive, get_block_type()
    
    prepare_outputs <- reactive({
      
      perc <- subtitle <- NULL
      block_type <- get_block_type()
      
      switch(block_type,
             
             # Timescale, spatial parameters and velocity:
             "species" = {
               
               out <- rv[[name]][[get_group]]
               if (!is.numeric(out["est", 1])) return(NULL)
               
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
               
               if (!is.na(lci) && !is.na(uci)) {
                 subtitle <- paste(ifelse(lci == 0, "0", lci), 
                                   "\u2014", uci)
               }
               
             }, # end of type == "species"
             
             # Tracking schedule:
             "design" = {
               
               subtitle <- NULL
               if (type == "dur") name <- "period" 
               if (type == "dti") name <- "interval"
               
               out <- extract_sampling(data, name = name, units = TRUE)
               
               if (length(out) > 1) out <- do.call(rbind, out)
               else out <- out[[1]]
               
               unit <- fix_unit(mean(out$value, na.rm = TRUE),
                                out$unit[1], convert = TRUE)$unit
               
               tmpmeanvalue <- mean(out$value, na.rm = TRUE)
               tmpmeanvalue <- unit %#% tmpmeanvalue

               tmpmaxvalue <- max(out$value, na.rm = TRUE)
               tmpmaxvalue <- unit %#% tmpmaxvalue
               
               tmpminvalue <- min(out$value, na.rm = TRUE)
               tmpminvalue <- unit %#% tmpminvalue
               
               out <- c(
                 "mean" = fix_unit(tmpmeanvalue, unit)$value,
                 "min" = fix_unit(tmpminvalue, unit)$value,
                 "max" = fix_unit(tmpmaxvalue, unit)$value)
               
               if (length(data) != 1) 
                 subtitle <- span(ifelse(out[2] == 0, "0", out[2]), 
                                  "\u2014", out[3], br())
               if (out[2] == out[3]) subtitle <- NULL
               
               value <- paste(out["mean"], unit)
               
             }, # end of type == "design"
             
             # Sample sizes:
             "metrics" = {
               
               if (!is.null(n) && !is.null(N)) {
                 m <- length(n)
                 n <- unlist(n)
                 value <- unlist(N)
               } else {
                 req(data)
                 m <- length(data)
                 n <- sapply(data, nrow)
                 if (type == "n") value <- n
                 if (type == "N") {
                   req(obj)
                   value <- extract_dof(obj, name = name)
                   value <- unlist(value)
                 }
               }
               
               if (m > 1) {
                 min_value <- ifelse(
                   min(value) == 0, "0", 
                   scales::label_comma(accuracy = 1)(min(value)))
                 subtitle <- span(
                   min_value, "\u2014", 
                   scales::label_comma(accuracy = 1)(max(value)))
               }
               
               req(value)
               value <- round(mean(value), 1)
               if (type == "N") {
                 if (mean(value) > mean(n)) {
                   perc <- paste0(
                     round((100 - ((mean(value) * 100) /
                                     mean(n))), 1), "%")
                 } else {
                   perc <- paste0(
                     "-",
                     round((100 - ((mean(value) * 100) /
                                     mean(n))), 1), "%")
                 }
               }
               
             }, # end of type == "metrics"
             
             # Outputs from analyses:
             "outputs" = {
               
               out <- rv[[name]]
               # out <- out[stats::complete.cases(out), ]
               
               if (grepl("Est", name)) {
                 
                 req(get_id)
                 req(!is.na(out[get_id, 5]))
                 
                 out <- fix_unit(
                   data.frame(value = unlist(out[get_id, 2:4]), 
                              unit = out[get_id, 5]), 
                   ui = TRUE, convert = TRUE)
                 
                 if (type == "ctsd") 
                   out$unit[2] <- abbrv_unit(out$unit[2])
                 
                 value <- paste(scales::label_comma(
                   ifelse((out$value[2] %% 1) * 10 == 0, .1, 1))
                   (out$value[2]), out$unit[2])
                 subtitle <- paste(
                   scales::label_comma(0.1)(out$value[1]), 
                   "\u2014", scales::label_comma(0.1)(out$value[3]))
               
               } # end of outputs (estimate)
               
               if (grepl("Err", name)) {
                 
                 is_multiple <- ifelse(nrow(out) > 1, TRUE, FALSE)
                 
                 if (grepl("meta", name)) {
                   out <- out[grep(type, out$type), ]
                   out <- out[nrow(out), ]
                   
                   value <- c(out[get_id, "lci"],
                              out[get_id, "est"],
                              out[get_id, "uci"])
                 } else {
                   
                   if (is.null(get_id) && is_multiple) {
                     ci <- bayestestR::ci(
                       out$est, ci = .95, method = "HDI") %>% 
                       suppressWarnings() %>% 
                       suppressMessages() %>% 
                       quiet()
                     
                     value <- c(ci$CI_low,
                                mean(out$est, na.rm = TRUE),
                                ci$CI_high)
                   } else {
                     req(get_id)
                     req(!is.na(out[get_id, 5]))
                     
                     value <- c(out[get_id, "lci"],
                                out[get_id, "est"],
                                out[get_id, "uci"])
                   }
                 }
                 
               } # end of outputs (error)
               
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
      
      out_block <- NULL
      block_type <- get_block_type()
      req(block_type)
      
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
        
        out <- prepare_outputs()
        value <- out[["value"]]
        subtitle <- out[["subtitle"]]
        
        if (!is.null(class)) {
          value <- span(value, class = class)
          if (!is.null(subtitle))
            subtitle <- span(subtitle, class = class)
        }
        
        if (type == "dur") {
          header <- "Sampling duration"
        }
        
        if (type == "dti") {
          header <- "Sampling interval"
          if (!is.null(subtitle))
            subtitle <- span(subtitle, "between fixes")
          else subtitle <- span("between fixes")
        }
        
        out_block <- parBlock(
          header = header,
          value = value,
          subtitle = subtitle)
        
      } # end of block_type == "design"

      ## Sample sizes: ----------------------------------------------------
      
      if (block_type == "metrics") {
        
        if (is.null(options))
          options <- list(rightBorder = FALSE,
                          marginBottom = FALSE)
        
        out_block <- sizeBlock(
          type = ifelse(type == "n", type, paste0(type, "_", name)),
          percentage = prepare_outputs()[["perc"]],
          icon = ifelse(type == "n", FALSE, TRUE),
          value = prepare_outputs()[["value"]],
          intervals = prepare_outputs()[["subtitle"]],
          rightBorder = options[["rightBorder"]],
          marginBottom = options[["rightBorder"]])
        
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
          is_multiple <- ifelse(length(rv$simList) > 1, TRUE, FALSE)
          
          if (!is_multiple) {
            out_text <- "Relative error"
          } else out_text <- "Mean relative error"
          
          if (grepl("List", name)) out_text <- "Expected error"
          
          out_block <- errorBlock(
            icon = "radiation",
            text = out_text,
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
