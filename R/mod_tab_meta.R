#' tab_meta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_tab_meta_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      # Introduction: -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          shinydashboardPlus::box(
            title = span("Meta-analyses:", class = "ttl-tab"),
            icon = fontawesome::fa(name = "layer-group",
                                   height = "21px",
                                   margin_left = "14px",
                                   margin_right = "8px",
                                   fill = "var(--sea-dark)"),
            id = ns("meta_intro"),
            width = NULL,
            solidHeader = FALSE, headerBorder = FALSE,
            collapsible = TRUE, closable = FALSE,
            
            column(
              align = "center", width = 12,
              
              p(style = "max-width: 1000px;",
                
                "The main goal in this tab is to conduct",
                "meta-analyses (such as comparing the home ranges",
                "of different groups).", br())
              
            ) # end of column (text)
            
          ) # end of box // meta_intro
      ), # end of div (top row)
      
      # [left column] -----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-4 col-md-4 col-lg-3",
          
          # meta species & individual: ----------------------------------
          
          shinydashboardPlus::box(
            title = span("Dataset:", class = "ttl-box_solid"),
            id = ns("metaBox_box1"),
            status = "primary",
            width = NULL,
            solidHeader = TRUE,
            collapsible = FALSE,
            
            NULL
            
          ), # end of box // metaBox_box1
          
          # Tracking regime: ----------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Tracking regime:", class = "ttl-box"),
            id = ns("metaBox_box2"),
            status = "info",
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            NULL,

            footer = splitLayout(
              cellWidths = c("29%", "1%", "70%"),
              cellArgs = list(style = "align: center;"),

              shiny::actionButton(
                inputId = ns("metaHelp_regime"),
                label = NULL,
                width = "100%",
                icon = icon("circle-question"),
                class = "btn-warning"),
              br(),
              shiny::actionButton(
                inputId = ns("metaButton_compare"),
                label = "Modify",
                icon = icon("code-compare"),
                class = "btn-info",
                width = "100%")

            ) # end of splitLayout
            
          ) # end of box // metaBox_box2
      ), # end of div (left column)
      
      # [right column] ----------------------------------------------------
      
      div(class = "col-xs-12 col-sm-8 col-md-8 col-lg-9",
          
          # Visualization: ------------------------------------------------
          
          shinydashboardPlus::box(
            title = span("Data visualization:", class = "ttl-box"),
            id = ns("metaBox_box3"),
            width = NULL,
            solidHeader = FALSE,
            collapsible = TRUE,
            
            NULL
            
          ) # end of box // metaBox_viz
          
      ), # end of column (right)
      
      # [bottom column] ---------------------------------------------------
      
      div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
          
          # Displaying relevant information: ------------------------------
          
          div(class = "col-lg-6 no-padding-left",
              shinydashboardPlus::box(
                title = span("Displaying parameters:", class = "ttl-box"),
                id = ns("metaBox_parameters"),
                width = NULL,
                solidHeader = FALSE,
                
                NULL
                
              ) # end of box // metaBox_parameters
          ), # end of div
          
          # Additional information: ---------------------------------------
          
          shinydashboardPlus::box(
            title = span("Additional information:", class = "ttl-box"),
            id = ns("metaBox_misc"),
            width = NULL, solidHeader = FALSE,
            
            verbatimTextOutput(outputId = ns("metaUI_time"))
            
          ) # end of box // metaBox_misc
      ) # end of column (bottom)
      
    ), # end of fluidRow
    
    # MODALS: -------------------------------------------------------------
    
    NULL
    
  ) # end of tagList
}
    
#' tab_meta Server Functions
#'
#' @noRd 
mod_tab_meta_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pal <- load_pal()
    
    # MAIN REACTIVE VALUES ------------------------------------------------
    
    
    
  }) # end of moduleServer
}
    
## To be copied in the UI
# mod_tab_meta_ui("tab_meta_1")
    
## To be copied in the server
# mod_tab_meta_server("tab_meta_1")
