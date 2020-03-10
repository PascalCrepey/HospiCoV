# Module UI
  
#' @title   mod_pop_ui and mod_pop_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_map
#'
#' @import leaflet
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_pop_ui <- function(id){
  ns <- NS(id)
  fluidRow(column(12, radioButtons(inputId = ns("Selectregion"), 
                                   label = "Selection by ",
                                   choices = c("FINESS" = FALSE, 
                                               "Region" = TRUE),
                                   inline = TRUE)),
           column(12, uiOutput(ns("PopSelectionUI")))
  )
}
    
# Module Server
    
#' @rdname mod_pop
#' @import leaflet
#' @importFrom shinyWidgets multiInput updateMultiInput
#' @export
#' @keywords internal
    
mod_pop_server <- function(input, output, session){
    ns <- session$ns
    pHosp = PolyHosp$new()
    npoly = length(pHosp$vorPolyHosp@data[,"FINESS_GEO"])
    PopParameters = reactiveValues(
      Region = "Bretagne",
      Polygon = "350000741",
      Selectregion = FALSE,
      #create Population
      pHosp = pHosp,
      SelPolygon = rep(FALSE, npoly),
      colPolygon = rep("blue", npoly),
      alphaPolygon = rep(0.05, npoly)
    )
    
    ## --- RENDER UI PARAMETERS -----------------------------------------------------
    output$PopSelectionUI = renderUI({
      tagList(
        if (input$Selectregion){
          fluidRow(
            column(6, multiInput(inputId = ns("Region"), 
                                 label = "Select",
                                 choices = unique(hospCovid[, Region])[order(unique(hospCovid[, Region]))],
                                 selected = PopParameters$Region,
                                 width = "100%",
                                 options = list(enable_search = FALSE,
                                                non_selected_header = "Non-selected",
                                                selected_header = "Selected"))),
            column(6, leaflet::leafletOutput(ns("mapRegion"), height = 800))
          )
        }
        else{
          fluidRow(
            column(6, multiInput(inputId = ns("Polygon"), 
                                 label = "Select",
                                 choiceNames = hospCovid[, paste(FINESS_GEO, 
                                                                 Libelle, 
                                                                 sep = "-")],
                                 choiceValues = hospCovid[, FINESS_GEO],
                                 selected = NULL,
                                 width = "100%",
                                 options = list(enable_search = TRUE,
                                                non_selected_header = "Non-selected",
                                                selected_header = "Selected"))),
            column(6, leaflet::leafletOutput(ns("mapPolygon"), height = 700))
          )
        }
      )
    })
    
    ## END RENDER UI PARAMETERS -----------------------------------------------------
    
    output$mapPolygon = leaflet::renderLeaflet({
      PopParameters$pHosp$vorPolyHosp %>%
        leaflet() %>%
        clearShapes() %>% 
        addTiles() %>%
        addPolylines() %>%
        addCircleMarkers(lng = ~lng, 
                         lat = ~lat, 
                         opacity = 0.8, 
                         radius = 2, 
                         color = "red") %>% 
        addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = PopParameters$alphaPolygon,
                    fillColor = PopParameters$colPolygon,
                    label = ~paste(FINESS_GEO,RS,sep="-"), layerId = ~FINESS_GEO)
      })
    
    observeEvent(input$Polygon,{
      ns <- session$ns
      
      id <- which(PopParameters$pHosp$vorPolyHosp@data[,"FINESS_GEO"] %in% input$Polygon)
      PopParameters$SelPolygon = rep(FALSE, length(pHosp$vorPolyHosp@data[,"FINESS_GEO"]))
      PopParameters$colPolygon = rep("blue", length(pHosp$vorPolyHosp@data[,"FINESS_GEO"]))
      PopParameters$alphaPolygon = rep(0.05, length(pHosp$vorPolyHosp@data[,"FINESS_GEO"]))
      if(length(id) > 0){
        PopParameters$SelPolygon[id] <- TRUE
        PopParameters$colPolygon[id] <- "orange"
        PopParameters$alphaPolygon[id] <- 0.4
      }
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    
    observeEvent(input$mapPolygon_shape_click,{
      ns <- session$ns
      # Polygon
        id <- grep(input$mapPolygon_shape_click$id,
                   PopParameters$pHosp$vorPolyHosp@data[,"FINESS_GEO"])

        if(length(id) > 0){
          if(PopParameters$SelPolygon[id]){
            PopParameters$SelPolygon[id] <- FALSE
            PopParameters$colPolygon[id] <- "blue"
            PopParameters$alphaPolygon[id] <- 0.05
          }
          else{
            PopParameters$SelPolygon[id] <- TRUE
            PopParameters$colPolygon[id] <- "orange"
            PopParameters$alphaPolygon[id] <- 0.4
          }
        }
        
        updateMultiInput(
          session,
          "Polygon",
          selected = PopParameters$pHosp$vorPolyHosp@data[PopParameters$SelPolygon, "FINESS_GEO"])
    }, ignoreInit = TRUE)
}
    
## To be copied in the UI
# mod_pop_ui("pop_ui_1")

## To be copied in the server
# callModule(mod_pop_server, "pop_ui_1")

 
