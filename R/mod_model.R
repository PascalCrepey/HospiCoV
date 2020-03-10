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

    input_byage = function(id) {
        tagList(
            numericInput(ns(paste0(id,"0-4")), label = "0-4", value = 0),
            numericInput(ns(paste0(id,"5-9")), label = "5-9", value = 0),
            numericInput(ns(paste0(id,"10-14")), label = "10-14", value = 0),
            numericInput(ns(paste0(id,"15-19")), label = "15-19", value = 0),
            numericInput(ns(paste0(id,"20-24")), label = "20-24", value = 0),
            numericInput(ns(paste0(id,"25-29")), label = "25-29", value = 0),
            numericInput(ns(paste0(id,"30-34")), label = "30-34", value = 0),
            numericInput(ns(paste0(id,"35-39")), label = "35-39", value = 0),
            numericInput(ns(paste0(id,"40-44")), label = "40-44", value = 0),
            numericInput(ns(paste0(id,"45-49")), label = "45-49", value = 0),
            numericInput(ns(paste0(id,"50-54")), label = "50-54", value = 0),
            numericInput(ns(paste0(id,"55-59")), label = "55-59", value = 0),
            numericInput(ns(paste0(id,"60-64")), label = "60-64", value = 0),
            numericInput(ns(paste0(id,"65-69")), label = "65-69", value = 0),
            numericInput(ns(paste0(id,"70-74")), label = "70-74", value = 0),
            numericInput(ns(paste0(id,"75-79")), label = "75-79", value = 0),
            numericInput(ns(paste0(id,"80P")), label = "80P", value = 0)
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
                           input_byage("severity")),
                  tabPanel("ICU hospit",
                           "Input risk of being admitted to ICU (from 0 to 1), for each age group",
                           input_byage("ICU"))
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
                            )
                     ),
            fluidRow(column(12,
                            tabsetPanel(
                                tabPanel("Severity",
                                         uiOutput(ns("dateRangeInput")),
                                         plotly::plotlyOutput(ns("severity"))
                                         ),
                                tabPanel("ICU Hospit"))
                            )
                     )
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


  ## ----- OUTCOMES -----------------------------------------------------
  output$dateRangeInput = renderUI({
      min = simulation()[, min(Time)]
      max = simulation()[, max(Time)]

      return(
      sliderInput(ns("dateRange"),
                  label = "Date range",
                  min = min,
                  max = max,
                  value = c(min, max))
      )
  })
  
  severity_risk_byage = reactive({
      c(input$`severity0-4`,
        input$`severity5-9`,
        input$`severity10-14`,
        input$`severity15-19`,
        input$`severity20-24`,
        input$`severity25-29`,
        input$`severity30-34`,
        input$`severity35-39`,
        input$`severity40-44`,
        input$`severity45-49`,
        input$`severity50-54`,
        input$`severity55-59`,
        input$`severity60-64`,
        input$`severity65-69`,
        input$`severity70-74`,
        input$`severity75-79`,
        input$`severity80P`)
  })

  severity_table = reactive({
      severity_byage(simulation(), risk_byage = severity_risk_byage())
  })

  output$severity = plotly::renderPlotly({
      severity_barchart(severity_table(),
                        start_time = input$dateRange[[1]],
                        end_time = input$dateRange[[2]])
  })

  ## ----- END OF OUTCOMES ----------------------------------------------
  
}
    
## To be copied in the UI
# mod_model_ui("model_ui_1")
    
## To be copied in the server
# callModule(mod_model_server, "model_ui_1")
 
