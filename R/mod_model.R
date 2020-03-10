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

    input_byage = function() {
        tagList(
            numericInput(ns("0-4"), label = "0-4", value = 0),
            numericInput(ns("5-9"), label = "5-9", value = 0),
            numericInput(ns("10-14"), label = "10-14", value = 0),
            numericInput(ns("15-19"), label = "15-19", value = 0),
            numericInput(ns("20-24"), label = "20-24", value = 0),
            numericInput(ns("25-29"), label = "25-29", value = 0),
            numericInput(ns("30-34"), label = "30-34", value = 0),
            numericInput(ns("35-39"), label = "35-39", value = 0),
            numericInput(ns("40-44"), label = "40-44", value = 0),
            numericInput(ns("45-49"), label = "45-49", value = 0),
            numericInput(ns("50-54"), label = "50-54", value = 0),
            numericInput(ns("55-59"), label = "55-59", value = 0),
            numericInput(ns("60-64"), label = "60-64", value = 0),
            numericInput(ns("65-69"), label = "65-69", value = 0),
            numericInput(ns("70-74"), label = "70-74", value = 0),
            numericInput(ns("75-79"), label = "75-79", value = 0),
            numericInput(ns("80P"), label = "80P", value = 0)
        )
    }
    
    
  agenames = colnames(contact_matrix)
  tagList(
      sidebarLayout(
          sidebarPanel(
              h4("Model parameters"),
              tabsetPanel(
                  tabPanel("Epidemic params.", 
                           uiOutput(ns("paramsEpiUI"))),
                  tabPanel("Population params.",
                           uiOutput(ns("paramsPopUI")))
              ),
              h4("Outcomes"),
              tabsetPanel(
                  tabPanel("Severity",
                           "Input risk of being severe (from 0 to 1), for each age group",
                           input_byage())
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
              plotly::plotlyOutput(ns("mainPlot"))
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
    

  output$mainPlot = renderPlotly({
    data = simulation()
    selectedAG = input$selectedAG
    if (selectedAG == "All") {
      p = ggplot(data, aes(x = Time, y = Cases, color = AgeGroup)) +
        theme_classic() +
        geom_line()
    }else if (selectedAG == "Aggregated") {
      dataAgg = data[, sum(Cases), by = "Time"]
      setnames(dataAgg, "V1", "Cases")
      p = ggplot(dataAgg, aes(x = Time, y = Cases)) +
        theme_classic() +
        geom_line()
    } else{
      p = ggplot(data[AgeGroup == selectedAG,], aes(x = Time, y = Cases, color = AgeGroup)) +
        theme_classic() +
        geom_line()
    }
    p
  })
  
  simulation = reactive({
    #create Parameter
    params = Parameters$new(SimulationParameters$R0)
    params$preInfected = 40

    #run the simulation
    pop = SimulationParameters$pHosp$getPopRegion(SimulationParameters$Region)

    finalRes = runMod(params = params$getList(), 
                      sname = SimulationParameters$sname, 
                      population = pop)
  })
  
  observeEvent(input$R0, {
    SimulationParameters$R0 = input$R0
  })
}
    
## To be copied in the UI
# mod_model_ui("model_ui_1")
    
## To be copied in the server
# callModule(mod_model_server, "model_ui_1")
 
