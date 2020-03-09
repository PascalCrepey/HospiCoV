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
            tabsetPanel(
              tabPanel("Epidemic parameters", 
                       uiOutput(ns("paramsEpiUI"))),
              tabPanel("Population parameters",
                       uiOutput(ns("paramsPopUI")))
            ), 
            width = 3
          ),
          mainPanel(
            plotOutput(ns("mainPlot"))
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
  params = Parameters$new()

    ## --- RENDER UI PARAMETERS -----------------------------------------------------
  output$paramsEpiUI = renderUI({
    tagList(
      sliderInput(ns("R0"),
                  label = "R0",
                  min   = 0,
                  max   = 5,
                  step  = 0.01, 
                  value = params$R0),
      # sliderInput(ns("beta"),
      #             label = "Beta",
      #             min   = 0,
      #             max   = 5,
      #             step  = 0.001, 
      #             value = params$beta),
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
                  value = params$symptomatic)
    )
  })
  output$paramsPopUI = renderUI({
    tagList(
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
    

  output$mainPlot = renderPlot({
    data = simulation()
    ggplot(data, aes(x = Time, y = Cases, color = AgeGroup)) +
      theme_classic() +
      geom_line()
  })
  
  simulation = reactive({
    #create Parameter
    params = Parameters$new(input$R0)
    #create Population
    pHosp = PolyHosp$new()
    #run the simulation
    pop = pHosp$getPopRegion("Ile-de-France")
    finalRes = runMod(params = params$getList(), sname = "test", population = pop)
  })
}
    
## To be copied in the UI
# mod_model_ui("model_ui_1")
    
## To be copied in the server
# callModule(mod_model_server, "model_ui_1")
 
