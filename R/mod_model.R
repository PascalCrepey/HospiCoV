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
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
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
                    tabPanel("Hospital params.",
                             uiOutput(ns("paramsHospUI")))
                )
            ),
            mainPanel(
                fluidRow(
                    column(3, uiOutput(ns("regionsSimulated"))),
                    column(3, selectInput(inputId = ns("selectedAG"), label = "Age groups",
                                          choices = c("All", "Aggregated", 
                                                      agenames), selected = "All")),
                    column(3, selectInput(inputId = ns("selectedOutcome"), 
                                          label = "Outcome",
                                          choices = c("Infected" = "Infected", 
                                        #"Symptomatic cases" = "symptomatic",  
                                                      "Severity" = "severity", 
                                                      "ICU admissions" = "ICU", 
                                                      "Ventilation in ICU" = "ventilation",
                                                      "Deaths" = "Deaths"), 
                                          selected = "Infected")),
                    column(3, selectInput(inputId = ns("selectedDuration"),
                                          label = "Period",
                                          choices = c("Week", "Month", "Trimester", 
                                                      "Semester", "Year"),
                                          selected = "Trimester"))
                ),
                tabsetPanel(
                  tabPanel(
                      title = "Hosp. requirements",
                      strong("The outputs correspond to the number of beds that will be required at the selected date."),
                    fluidRow(column(4, selectInput(inputId = ns("selectedHospOutcome"), 
                                                   label = NULL,
                                                   choices = c("Number hospital beds" = "bedhosp",
                                                               "Number ICU beds" = "bedICU",
                                                               "Number invasive ventilations" = "bedventil"), 
                                                   selected = "bedhosp")),
                             column(8, uiOutput(ns("dateHospInput")))),
                    fluidRow(
                      column(12,
                             plotOutput(ns("outcomePlotHosp"), height = "200px")
                      )
                    ),
                    fluidRow(
                      column(12,
                             plotly::plotlyOutput(ns("outcomePlotHospAge"))
                      )
                    ),
                    fluidRow(
                      column(12,                                                 
                             DT::DTOutput(ns("outcomeTableHosp"))
                      )
                    )
                  ),
                    tabPanel(
                        title = "Time series",
                        fluidRow(column(12,
                                        plotly::plotlyOutput(ns("mainPlot")),
                                        uiOutput(ns("CaseTimeSeriesUI")),
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
mod_model_server <- function(input, output, session, modelInputs) {
    ns <- session$ns

    selectedRegions = reactive({
        req(modelInputs$preInf())
        modelInputs$preInf()$Region
    })
        
    
  params = Parameters$new()
  SimulationParameters = reactiveValues(
      R0 = 3,
      matrix = modelInputs$matrix(),
      Duration = "Trimester", 
      Outcome = "Infected", 
    #Region = selectedRegions(),
      sname = "test",
      DaysHosp = 15,
      DaysICU = 15,
      DaysVentil = 15,
      removal = 1/params$removal,
      progression = 1/params$progression,
    #create Population
      pHosp = PolyHosp$new(),
      currDateHosp = as.Date("01/02/2020"),
      ShowCaseTimeSeries = FALSE
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
                  min = 4, max = 10, step = 0.1, post = " days",
                  value = round(1/params$progression)),       
      sliderInput(ns("removal"),
                  label = "Contagious period",
                  min = 6, max = 15, step = 0.1, post = " days",
                  value = round(1/params$removal, 1))
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

  output$regionsSimulated = renderUI({
    selectInput(ns("selectedRegionsUI"),
                label = "Region",
                choices = c("All", selectedRegions()))
  })
  
  output$CaseTimeSeriesUI = renderUI({
      if(input$selectedAG == "Aggregated" & input$selectedOutcome == "Infected") {
          
          checkboxInput(ns("ShowCaseTimeSeries"),
                        label = "Show observed values ",
                        value = FALSE)
      }
  })
                  
    
    ## END RENDER UI PARAMETERS -----------------------------------------------------
    
  observe({
      req(input$selectedOutcome)
      req(input$selectedAG)
      req(SimulationParameters$Duration)
      req(SimulationParameters$R0)
      req(input$selectedRegionsUI)
      # if (input$selectedOutcome == "Infected") {
      #     curves = renderCurves(simulation(), input$selectedOutcome, input$selectedAG)
      # } else if (input$selectedOutcome != "Infected") {
          # curves = renderCurves(outcome_table()[Region %in% input$selectedRegionsUI | All == input$selectedRegionsUI,], input$selectedOutcome, input$selectedAG)
      # }
      # browser()
      if(!(SimulationParameters$ShowCaseTimeSeries)){
        curves = renderCurves(outcome_table()[Region %in% input$selectedRegionsUI | All == input$selectedRegionsUI,], 
                              input$selectedOutcome, 
                              input$selectedAG)
      }
      else {
        if(input$selectedRegionsUI == "All"){
          
          SelectedTimeSeries <- CaseTimeSeries[, .SD,
                                               .SDcols = c("Date", selectedRegions())]
          SelectedTimeSeries[, Cases := rowSums(.SD),
                             .SDcols = 2:length(names(SelectedTimeSeries))[1]]
        }
        else{
          SelectedTimeSeries <- CaseTimeSeries[, .SD,
                                               .SDcols = c("Date", input$selectedRegionsUI)]
          setnames(SelectedTimeSeries, input$selectedRegionsUI, "Cases")
        }
        curves = renderCurves(outcome_table()[Region %in% input$selectedRegionsUI | All == input$selectedRegionsUI,], 
                              input$selectedOutcome, 
                              input$selectedAG,
                              ShowCaseTimeSeries = input$ShowCaseTimeSeries,
                              TimeSeries = SelectedTimeSeries[, .(Date, Cases)])
      }

      output$mainPlot   = curves$mainPlot
      output$secondPlot = curves$secondPlot

  })

  
  
  ## ------ RUN MODEL ----------------------------------------------------------
    simulation = reactive({
        req(selectedRegions())
        req(modelInputs$preInf())
        req(modelInputs$pop())
 
      all_res = lapply(selectedRegions(), function(region) {
          #create Parameter
          params = Parameters$new(SimulationParameters$R0)
          params$preInfected = modelInputs$preInf()[Region == region, preInfected]
          pop = modelInputs$pop()[Region == region]
          startDate = modelInputs$preInf()[Region == region, Date]
          #set matrix
          params$matrix = SimulationParameters$matrix
          #set duration
          params$duration = SimulationParameters$Duration
          #set removal
          params$removal = 1/SimulationParameters$removal
          #set progression 
          params$progression = 1/SimulationParameters$progression
          #run the simulation
          finalRes = runMod(params = params$getList(), 
                            sname = SimulationParameters$sname, 
                            population = pop,
                            startDate = startDate)
          finalRes[, Region := region]
          finalRes[, All := "All"]
      })
      out = rbindlist(all_res)
      return(out)
  })
  
  ## ----- COMPUTE OUTCOMES ---------------------------------------------------
  outcome_table = reactive({
    compute_outcomes(simulation(),
                     severity_risk,
                     ICU_risk,
                     ventil_risks,
                     death_risk,
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
  })
  observeEvent(input$ShowCaseTimeSeries, {
    SimulationParameters$ShowCaseTimeSeries = input$ShowCaseTimeSeries
  })
  observeEvent(input$removal, {
    SimulationParameters$removal = input$removal
  })
  observeEvent(input$progression, {
    SimulationParameters$progression = input$progression
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
    
    if (SimulationParameters$currDateHosp < min) val = min
    else val = SimulationParameters$currDateHosp
    return(
      sliderInput(ns("dateHosp"),
                  label = NULL,
                  min = min,
                  max = max,
                  value = val,
                  width = "90%")
    )
  })
  
  observeEvent(input$dateHosp,{
    SimulationParameters$currDateHosp = input$dateHosp
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
          out = outcome_render(simulation()[Region %in% input$selectedRegionsUI | All == input$selectedRegionsUI,],
                               start_time = input$dateRange[[1]],
                               end_time = input$dateRange[[2]],
                               outcome = input$selectedOutcome)
      }
      else if (input$selectedOutcome != "Infected"){
          out = outcome_render(outcome_table()[Region %in% input$selectedRegionsUI | All == input$selectedRegionsUI,],
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
    req(input$dateHosp)
    req(input$selectedRegionsUI)
    outCurve = outcome_render_instant_curve(outcome_table()[Region %in% input$selectedRegionsUI | All == input$selectedRegionsUI,],
                                       instant_time = input$dateHosp, outcome = input$selectedHospOutcome)
    outAge = outcome_render(outcome_table()[Region %in% input$selectedRegionsUI | All == input$selectedRegionsUI,],
                         start_time = input$dateHosp,
                         end_time = input$dateHosp,
                         outcome = input$selectedHospOutcome)
    table = DT::datatable(outAge$table,
                          fillContainer = F,
                          rownames = NULL,
                          extensions = 'Buttons',
                          options = list(fillContainer = F,
                                         dom = 'Bfrtip', paging = FALSE, searching = FALSE, ordering=FALSE,
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>% 
      DT::formatRound(columns = 2:4, digits = 0)
    
    output$outcomePlotHosp  = renderPlot({ outCurve }, height = 200)
    output$outcomePlotHospAge  = plotly::renderPlotly({ outAge$plot })
    output$outcomeTableHosp = DT::renderDT({ table })
    
  })

  

  ## ----- END OF OUTCOMES ----------------------------------------------
  
}
    
## To be copied in the UI
# mod_model_ui("model_ui_1")
    
## To be copied in the server
# callModule(mod_model_server, "model_ui_1")
 

