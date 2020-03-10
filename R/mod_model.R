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
  agenames = colnames(contact_matrix)
  tagList(
      sidebarLayout(
          sidebarPanel(
            tabsetPanel(
              tabPanel("Epidemic params.", 
                       uiOutput(ns("paramsEpiUI"))),
              tabPanel("Population params.",
                       uiOutput(ns("paramsPopUI")))
            ), 
            width = 3
          ),
          mainPanel(
            fluidRow(column(4, selectInput(inputId = ns("selectedAG"), label = NULL,
                                           choices = c("All", "Aggregated", 
                                                       agenames), selected = "All")),
                     column(4, selectInput(inputId = ns("selectedOutcome"), 
                                           label = NULL,
                                           choices = c("Infected", 
                                                         "Symptomatic cases", 
                                                         "Mild cases", 
                                                         "Hospitalized cases", 
                                                         "ICU cases", 
                                                         "ICU with O2"), 
                                          selected = "Infected")),
                     column(4, selectInput(inputId = ns("selectedDuration"),
                                           label = NULL,
                                           choices = c("Week", "Month", "Trimester", 
                                                       "Semester", "Year"),
                                           selected = "Trimester"
                                           ))),
            fluidRow(column(12,
              plotlyOutput(ns("mainPlot"))
            ))
            
          )
      )  
  )
}
    
# Module Server
    
#' @rdname mod_model
#' @importFrom plotly renderPlotly
#' @export
#' @keywords internal
mod_model_server <- function(input, output, session){
  ns <- session$ns
  params = Parameters$new()
  SimulationParameters = reactiveValues(
    R0 = 3, 
    Duration = "Trimester", 
    Outcome = "Infected", 
    Region = "Bretagne",
    sname = "test",
    #create Population
    pHosp = PolyHosp$new()
  )

    ## --- RENDER UI PARAMETERS -----------------------------------------------------
  output$paramsEpiUI = renderUI({
    tagList(
      sliderInput(ns("R0"),
                  label = "R0",
                  min   = 0,
                  max   = 3,
                  step  = 0.1, 
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
                  value = params$removal)
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
    

  output$mainPlot = renderPlotly({
    data = simulation()
    selectedAG = input$selectedAG
    if (selectedAG == "All") {
      p = ggplot(data, aes(x = Time, y = Infected, color = AgeGroup)) +
        theme_classic() +
        geom_line()
    }else if (selectedAG == "Aggregated") {
      dataAgg = data[, sum(Infected), by = "Time"]
      setnames(dataAgg, "V1", "Infected")
      p = ggplot(dataAgg, aes(x = Time, y = Infected)) +
        theme_classic() +
        geom_line()
    } else{
      p = ggplot(data[AgeGroup == selectedAG,], aes(x = Time, y = Infected, color = AgeGroup)) +
        theme_classic() +
        geom_line()
    }
    p
  })
  
  simulation = reactive({
    #create Parameter
    params = Parameters$new(SimulationParameters$R0)
    params$preInfected = 40
    
    #set duration
    params$duration = SimulationParameters$Duration

    #run the simulation
    pop = SimulationParameters$pHosp$getPopRegion(SimulationParameters$Region)

    finalRes = runMod(params = params$getList(), 
                      sname = SimulationParameters$sname, 
                      population = pop)
  })
  
  observeEvent(input$R0, {
    SimulationParameters$R0 = input$R0
  })
  observeEvent(input$selectedDuration, {
    SimulationParameters$Duration = input$selectedDuration
    #print(SimulationParameters$Duration)
  })
}
    
## To be copied in the UI
# mod_model_ui("model_ui_1")
    
## To be copied in the server
# callModule(mod_model_server, "model_ui_1")
 
