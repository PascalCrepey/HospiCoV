# Module UI
  
#' @title   mod_inputs_ui and mod_inputs_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_inputs
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
      sidebarLayout(
          sidebarPanel(
              uiOutput(ns("selectCountry")),
              h4("Upload pre-infected table"),
              fileInput(ns("fileInput"),
                        label = "File",
                        accept = ACCEPTED_FILES),
              uiOutput(ns("fileFormat")),
              textOutput(ns("formatWarning")),
              uiOutput(ns("importOptions")),
              uiOutput(ns("fileLoaded")),
              h4("Upload population table"),
              fileInput(ns("fileInput2"),
                        label = "File",
                        accept = ACCEPTED_FILES),
              uiOutput(ns("fileFormat2")),
              textOutput(ns("formatWarning2")),
              uiOutput(ns("importOptions2")),
              uiOutput(ns("fileLoaded2"))
          ),
          mainPanel(
              h2("Model inputs"),
              includeMarkdown("intro.md"),
              fluidRow(
                  column(
                      12,
                      tabsetPanel(
                          tabPanel("Contact matrix",
                                   DT::DTOutput(ns("matrixDT"))
                                   ),
                          tabPanel("Pre-infected table",
                                   DT::DTOutput(ns("preInfDT"))
                                   ),
                          tabPanel("Population table",
                                   DT::DTOutput(ns("popDT"))
                                   )
                      )
                  )
              )
          )
      )
  )
  
}


FILE_EXTENSIONS = c(".csv", ".xls", ".xlsx")

ACCEPTED_FILES  = c(FILE_EXTENSIONS,
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    "application/vnd.ms-excel",
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")


# Module Server
    
#' @rdname mod_inputs
#' @export
#' @keywords internal
    
mod_inputs_server <- function(input, output, session){
    ns <- session$ns

    output$selectCountry = renderUI({
        selectInput(ns("country"),
                    label = "Please select your country to use the corresponding matrix",
                    choices = names(matrices))
        })

    matrix = reactive({
        req(input$country)
        matrices[[input$country]]
    })

    output$matrixDT = DT::renderDT( DT::datatable(matrix(),
                                                  options = list(scrollX = T,
                                                                 pageLength = 17)))
    
    ## --- UPLOAD PRE INF TABLE ---------------------------------------------------------
        
    fileExt = reactive({
        req(input$fileInput)
        x = sapply(paste0("\\", FILE_EXTENSIONS, "$"),
                   grepl,
                   input$fileInput$name)
        FILE_EXTENSIONS[x]
    })
    
    observeEvent(input$fileInput, {
        output$fileFormat = renderUI({
            radioButtons(ns("fileFormat"),
                         label = "File format",
                         choices = FILE_EXTENSIONS,
                         selected = fileExt(),
                         inline = T)
        })
        output$fileLoaded = NULL
    })
    
    observeEvent(input$fileFormat, {
        if (input$fileFormat != fileExt()) {
            output$formatWarning = renderText({ "Warning: the format selected does not seem to match the format of the uploaded file." })
        } else {
            output$formatWarning = NULL
        }
        output$importOptions = renderUI({
              actionButton(ns("readFile"),
                           label = "Read file",
                           icon  = icon("upload"))
        })
    })
    
    preInf = reactiveVal()
    observeEvent(input$readFile, {
        if (input$fileFormat == ".csv") {
            preInf(fread(input$fileInput$datapath))
        } else if (input$fileFormat == ".xlsx") {
            preInf(setDT(openxlsx::read.xlsx(input$fileInput$datapath)))
        }
        
        output$fileFormat = NULL
        output$importOptions = NULL
        output$fileLoaded = renderText({ "File loaded" })
    })

    ## ---- UPLOAD POP TABLE -------------------------------------------------------
    fileExt2 = reactive({
        req(input$fileInput2)
        x = sapply(paste0("\\", FILE_EXTENSIONS, "$"),
                   grepl,
                   input$fileInput2$name)
        FILE_EXTENSIONS[x]
    })
    
    observeEvent(input$fileInput2, {
        output$fileFormat2 = renderUI({
            radioButtons(ns("fileFormat2"),
                         label = "File format",
                         choices = FILE_EXTENSIONS,
                         selected = fileExt2(),
                         inline = T)
        })
        output$fileLoaded2 = NULL
    })
    
    observeEvent(input$fileFormat2, {
        if (input$fileFormat2 != fileExt2()) {
            output$formatWarning2 = renderText({ "Warning: the format selected does not seem to match the format of the uploaded file." })
        } else {
            output$formatWarning2 = NULL
        }
        output$importOptions2 = renderUI({
            actionButton(ns("readFile2"),
                         label = "Read file",
                         icon  = icon("upload"))
        })
    })
    
    pop = reactiveVal()
    observeEvent(input$readFile2, {
        if (input$fileFormat2 == ".csv") {
            pop(fread(input$fileInput2$datapath))
        } else if (input$fileFormat2 == ".xlsx") {
            pop(setDT(openxlsx::read.xlsx(input$fileInput2$datapath)))
        }
        
        output$fileFormat2 = NULL
        output$importOptions2 = NULL
        output$fileLoaded2 = renderText({ "File loaded" })
    })
    ## ---- END UPLOAD FILES ---------------------------------------------------
    
    output$preInfDT = DT::renderDT({ DT::datatable(preInf(),
                                                 options = list(scrollX = T,
                                                                pageLength = 17))
    })
    output$popDT = DT::renderDT({ DT::datatable(pop(),
                                                  options = list(scrollX = T,
                                                                 pageLength = 17))
    })
    
    return(list(matrix, preInf, pop))

}
    
## To be copied in the UI
# mod_inputs_ui("inputs_ui_1")
    
## To be copied in the server
# callModule(mod_inputs_server, "inputs_ui_1")
 