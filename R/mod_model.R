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
                h4("Model parameters"),
                tabsetPanel(
                    tabPanel("Epidemic params.", 
                             uiOutput(ns("paramsEpiUI"))),
                    tabPanel("Population params.",
                             uiOutput(ns("paramsPopUI"))),
                    tabPanel("Hospital params.",
                             uiOutput(ns("paramsHospUI")))
                )
            ),
            mainPanel(
                fluidRow(column(4, selectInput(inputId = ns("selectedAG"), label = NULL,
                                               choices = c("All", "Aggregated", 
                                                           agenames), selected = "All")),
                         column(4, selectInput(inputId = ns("selectedOutcome"), 
                                               label = NULL,
                                               choices = c("Infected" = "Infected", 
                                                           #"Symptomatic cases" = "symptomatic",  
                                                           "Severity" = "severity", 
                                                           "ICU admissions" = "ICU", 
                                                           "Ventilation in ICU" = "ventilation"), 
                                               selected = "Infected")),
                         column(4, selectInput(inputId = ns("selectedDuration"),
                                               label = NULL,
                                               choices = c("Week", "Month", "Trimester", 
                                                           "Semester", "Year"),
                                               selected = "Trimester"))
                         ),
                tabsetPanel(
                    tabPanel(
                        title = "Time series",
                        fluidRow(column(12,
                                        plotly::plotlyOutput(ns("mainPlot")),
                                        plotly::plotlyOutput(ns("secondPlot"))
                                        )
                                 )
                    ),
                    tabPanel(
                        title = "Age distribution",
                        uiOutput(ns("dateRangeInput")),
                        fluidRow(column(12,
                            plotly::plotlyOutput(ns("outcomePlot"))
                        )),
                        fluidRow(column(12,
                            DT::DTOutput(ns("outcomeTable"))
                        ))
                    ),
                    tabPanel(
                      title = "Hosp. requirements",
                      fluidRow(column(4, selectInput(inputId = ns("selectedHospOutcome"), 
                                                     label = NULL,
                                                     choices = c("Number hospital beds" = "bedhosp",
                                                                 "Number ICU beds" = "bedICU",
                                                                 "Number invasive ventilations" = "bedventil"), 
                                                     selected = "bedhosp")),
                               column(8, uiOutput(ns("dateHospInput"))),
                               column(12,
                                      plotly::plotlyOutput(ns("outcomePlotHosp"))
                                      ),
                               column(12,                                                 
                                      DT::DTOutput(ns("outcomeTableHosp"))
                                      ))
                      ),
                    tabPanel(
                        title = "Outcomes probabilities",
                        fluidRow(
                            column(
                                4,
                                h4("Severity"),
                                HTML(
                                    "Risk of being severe (from 0 to 1), for each age group",
                                    "</br>",
                                    "Source:"),
                                a("Guan et al.", href = "https://www.nejm.org/doi/full/10.1056/NEJMoa2002032", target = "_blank"),
                                DT::DTOutput(ns("severity_risk"))
                            ),
                            column(
                                4,
                                h4("ICU admissions"),
                                HTML(
                                    "Risk of being admitted to ICU (from 0 to 1), for each age group",
                                    "</br>",
                                    "Source:"),
                                a("Guan et al.", href = "https://www.nejm.org/doi/full/10.1056/NEJMoa2002032", target = "_blank"),
                                DT::DTOutput(ns("ICU_risk"))
                            ),
                            column(
                                4,
                                h4("Risk of ventilation when admitted in ICU"),
                                HTML(
                                    "Risk of ventilation when admitted to ICU, overall, and for invasive ventilation",
                                    "</br>",
                                    "Source:"),
                                a("Yang et al.", href = "https://www.thelancet.com/journals/lanres/article/PIIS2213-2600(20)30079-5/fulltext", target = "_blank"),
                                DT::DTOutput(ns("ventil_risks"))
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
#' @importFrom DT formatRound datatable
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
    DaysHosp = 15,
    DaysICU = 15,
    DaysVentil = 15,
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
                  label = "Incubation period",
                  min = 1, max = 10, step = 0.1, post = " days",
                  value = 1/params$progression),       
      sliderInput(ns("removal"),
                  label = "Contagious period",
                  min = 1, max = 10, step = 0.1, post = "days",
                  value = 1/params$removal)
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
  output$paramsHospUI = renderUI({
    tagList(
      sliderInput(ns("DaysHosp"),
                  label = "Number of hospital days",
                  min = 0, max = 21, step = 1,
                  value = SimulationParameters$DaysHosp),
      sliderInput(ns("DaysICU"),
                  label = "Number of days in ICU",
                  min = 0, max = 21, step = 1,
                  value = SimulationParameters$DaysICU),
      sliderInput(ns("DaysVentil"),
                  label = "Number of days of invasive ventilation",
                  min = 0, max = 21, step = 1,
                  value = SimulationParameters$DaysVentil)
    )
  })
    
    ## END RENDER UI PARAMETERS -----------------------------------------------------
    
  observe({
      req(input$selectedOutcome)
      req(input$selectedAG)
      req(SimulationParameters$Duration)
      req(SimulationParameters$R0)
      # if (input$selectedOutcome == "Infected") {
      #     curves = renderCurves(simulation(), input$selectedOutcome, input$selectedAG)
      # } else if (input$selectedOutcome != "Infected") {
          curves = renderCurves(outcome_table(), input$selectedOutcome, input$selectedAG)
      # }

      output$mainPlot   = curves$mainPlot
      output$secondPlot = curves$secondPlot

  })

  
  
  ## ------ RUN MODEL ----------------------------------------------------------
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
  ## ----- COMPUTE OUTCOMES ---------------------------------------------------
  outcome_table = reactive({
    compute_outcomes(simulation(),
                     severity_risk,
                     ICU_risk,
                     ventil_risks,
                     DaysHosp = SimulationParameters$DaysHosp,
                     DaysICU = SimulationParameters$DaysICU,
                     DaysVentil = SimulationParameters$DaysVentil)
  })
  ## -------------------------------------------------------------------------
  
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
                  value = c(min, max),
                  width = "90%")
      )
  })
  
  output$dateHospInput = renderUI({
    min = simulation()[, min(Time)]
    max = simulation()[, max(Time)]
    
    return(
      sliderInput(ns("dateHosp"),
                  label = NULL,
                  min = min,
                  max = max,
                  value = min,
                  width = "90%")
    )
  })

  ## ---- TABLES OF OUTCOME RISKS------------------------------------------
  severity_risk_table = DT::datatable(severity_risk,
                                      selection = list(target = "column", mode = "single"),
                                      rownames = NULL,
                                      extensions = 'Buttons',
                                      editable  = list(target = "column",
                                                      disable = list(columns = 1)),
                                      options = list(dom = 'Bfrtip', paging = FALSE, searching = FALSE, ordering=FALSE,
                                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
                                      ) %>% DT::formatRound(columns = 2, digits = 3)

  ICU_risk_table = DT::datatable(ICU_risk,
                                 selection = list(target = "column", mode = "single"),
                                 rownames = NULL,
                                 extensions = 'Buttons',
                                 editable  = list(target = "column",
                                                  disable = list(columns = 1)),
                                 options = list(dom = 'Bfrtip', paging = FALSE, searching = FALSE, ordering=FALSE,
                                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
                                 ) %>% DT::formatRound(columns = 2, digits = 3)

  ventil_risks_table = DT::datatable(ventil_risks,
                                     selection = list(target = "column", mode = "single"),
                                     rownames = NULL,
                                     extensions = 'Buttons',
                                     editable  = list(target = "column"),
                                     options = list(dom = 'Bfrtip', paging = FALSE, searching = FALSE, ordering=FALSE,
                                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
                                     ) %>% DT::formatRound(columns = 2, digits = 3)
  
  output$severity_risk = DT::renderDT({ severity_risk_table })
  output$ICU_risk = DT::renderDT({ ICU_risk_table })
  output$ventil_risks = DT::renderDT({ ventil_risks_table })
  ##-----------------------------------------------------

  ## ---- OBSERVER TO RENDER BAR CHARTS -----------------
  observe({
      req(input$selectedOutcome)
      if (input$selectedOutcome == "Infected") {
          out = outcome_render(simulation(),
                               start_time = input$dateRange[[1]],
                               end_time = input$dateRange[[2]],
                               outcome = input$selectedOutcome)
      }
      else if (input$selectedOutcome != "Infected"){
          out = outcome_render(outcome_table(),
                               start_time = input$dateRange[[1]],
                               end_time = input$dateRange[[2]],
                               outcome = input$selectedOutcome)
      }
      table = DT::datatable(out$table,
                            fillContainer = F,
                            rownames = NULL,
                            extensions = 'Buttons',
                            options = list(fillContainer = F,
                                           dom = 'Bfrtip', paging = FALSE, searching = FALSE, ordering=FALSE,
                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>% 
        DT::formatRound(columns = 2:4, digits = 0)
      
      output$outcomePlot  = plotly::renderPlotly({ out$plot })
      output$outcomeTable = DT::renderDT({ table })
       
  })

  ## ---- HOSPITAL REQUIREMENTS -------------------------
  observeEvent(input$DaysHosp,{
    SimulationParameters$DaysHosp <- input$DaysHosp
  })
  
  observeEvent(input$DaysICU,{
    SimulationParameters$DaysICU <- input$DaysICU
  })
  
  observeEvent(input$DaysVentil,{
    SimulationParameters$DaysVentil <- input$DaysVentil
  })
  
  observe({
    req(input$selectedHospOutcome)
    out = outcome_render(outcome_table(),
                         start_time = input$dateHosp,
                         end_time = input$dateHosp,
                         outcome = input$selectedHospOutcome)
    table = DT::datatable(out$table,
                          fillContainer = F,
                          rownames = NULL,
                          extensions = 'Buttons',
                          options = list(fillContainer = F,
                                         dom = 'Bfrtip', paging = FALSE, searching = FALSE, ordering=FALSE,
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>% 
      DT::formatRound(columns = 2:4, digits = 0)
    
    output$outcomePlotHosp  = plotly::renderPlotly({ out$plot })
    output$outcomeTableHosp = DT::renderDT({ table })
    
  })

  

  ## ----- END OF OUTCOMES ----------------------------------------------
  
}
    
## To be copied in the UI
# mod_model_ui("model_ui_1")
    
## To be copied in the server
# callModule(mod_model_server, "model_ui_1")
 

