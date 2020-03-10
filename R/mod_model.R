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
        tabsetPanel(
            tabPanel(
                title = "Epidemic curves",
                sidebarLayout(
                    sidebarPanel(
                        h4("Model parameters"),
                        tabsetPanel(
                            tabPanel("Epidemic params.", 
                                     uiOutput(ns("paramsEpiUI"))),
                            tabPanel("Population params.",
                                     uiOutput(ns("paramsPopUI")))
                        )
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
                                 )
                    )
                )
            ),
            tabPanel(
                title = "Outcomes",
                sidebarLayout(
                    sidebarPanel(
                        h4("Outcomes"),
                        tabsetPanel(
                            tabPanel(
                                "Severity",
                                HTML(
                                    "Input risk of being severe (from 0 to 1), for each age group",
                                    "</br>",
                                    "Source:"),
                                a("Guan et al.", href = "https://www.nejm.org/doi/full/10.1056/NEJMoa2002032", target = "_blank"),
                                DT::DTOutput(ns("severity_risk"))
                            ),
                            tabPanel(
                                "ICU hospit",
                                "Input risk of being admitted to ICU (from 0 to 1), for each age group",
                                        #                           input_byage("ICU"))
                                )
                        ),
                        width = 3
                    ),
                    mainPanel(
                        fluidRow(column(10,
                                        uiOutput(ns("dateRangeInput")),
                                        plotly::plotlyOutput(ns("outcome"))
                                        ),
                                 column(2,
                                        radioButtons(ns("outcomeType"),
                                                     label = "Outcome",
                                                     choices = c("Severity"       = "severity",
                                                                 "ICU admissions" = "ICU")
                                                     )
                                        )
                                 )
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

  severity_risk_table = DT::datatable(severity_risk,
                                      selection = list(target = "column", mode = "single"),
                                      options   = list(pageLength = 17),
                                      editable  = list(target = "column",
                                                      disable = list(columns = 1))
                                      )

  output$severity_risk = DT::renderDT({ severity_risk_table })
  
  severity_table = reactive({
      severity_byage(simulation(), severity_risk)
  })

  output$outcome = plotly::renderPlotly({
      req(input$dateRange)
      outcome_barchart(severity_table(),
                       start_time = input$dateRange[[1]],
                       end_time = input$dateRange[[2]])
  })

  ## ----- END OF OUTCOMES ----------------------------------------------
  
}
    
## To be copied in the UI
# mod_model_ui("model_ui_1")
    
## To be copied in the server
# callModule(mod_model_server, "model_ui_1")
 
## TODO OUTCOME ICU
