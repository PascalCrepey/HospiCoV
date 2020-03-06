# Module UI
  
#' @title   mod_model_ui and mod_model_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_model
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_model_ui <- function(id){
  ns <- NS(id)
  tagList(
      sidebarLayout(
          sidebarPanel(
              uiOutput(ns("paramsUI"))
          ),
          mainPanel(
              h2("Output"),
              textOutput(ns("R0"))
          )
      )  
  )
}
    
# Module Server
    
#' @rdname mod_model
#' @export
#' @keywords internal
    
mod_model_server <- function(input, output, session){
    ns <- session$ns
    library(R6)
    source("R/Parameters.R")

    params = Parameters$new()


    ## --- RENDER UI PARAMETERS -----------------------------------------------------
    output$paramsUI = renderUI({
        tagList(
            h3("Parameters"),
            sliderInput(ns("R0"),
                        label = "R0",
                        min   = 0,
                        max   = 5,
                        step  = 0.01, 
                        value = params$R0),
            sliderInput(ns("beta"),
                        label = "Beta",
                        min   = 0,
                        max   = 5,
                        step  = 0.001, 
                        value = params$beta),
            sliderInput(ns("progression"),
                        label = "Transfer rate from E to I",
                        min = 0, max = 10, step = 0.001,
                        value = params$progression),       
            sliderInput(ns("removal"),
                        label = "Transfer rate from I to R",
                        min = 0, max = 10, step = 0.001,
                        value = params$removal),
            sliderInput(ns("symptomatic"),
                        label = "Proportion of symptomatic infections",
                        min = 0, max = 100, step = 0.01,
                        value = params$symptomatic),
            h3("Initial state"),
            sliderInput(ns("preImmune"),
                        label = "Proportion of pre-immune",
                        min = 0, max = 100, step = 0.01,
                        value = params$preImmune),
            sliderInput(ns("preExposed"),
                        label = "Proportion of pre-exposed",
                        min = 0, max = 100, step = 0.01,
                        value = params$preExposed),
            sliderInput(ns("preInfected"),
                        label = "Proportion of pre-infected",
                        min = 0, max = 100, step = 0.01,
                        value = params$preInfected)
        )
    })
    
    ## END RENDER UI PARAMETERS -----------------------------------------------------
    
    output$R0 = renderText({ input$R0 })
}
    
## To be copied in the UI
# mod_model_ui("model_ui_1")
    
## To be copied in the server
# callModule(mod_model_server, "model_ui_1")
 
