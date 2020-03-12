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
  fluidRow(column(12, radioButtons(inputId = ns("SelectRegion"), 
                                   label = "Select the area where you want to run the model: ",
                                   choices = c("FINESS", 
                                               "Region"),
                                   selected = "Region",
                                   inline = TRUE)),
           column(12, uiOutput(ns("PopSelectionUI")))
  )
}
    
# Module Server
    
#' @rdname mod_pop
#' @import leaflet
#' @importFrom shinyWidgets multiInput updateMultiInput
#' @importFrom shinyjs hidden show hide
#' @export
#' @keywords internal
    
mod_pop_server <- function(input, output, session){
    ns <- session$ns
    ListRegions = unique(hospCovid[, Region])[order(unique(hospCovid[, Region]))]

    pHosp = PolyHosp$new()
    npoly = length(pHosp$vorPolyHosp@data[,"FINESS_GEO"])
    nregion = length(ListRegions)
    PopParameters = reactiveValues(
      Region = "Ile-de-France",
      Polygon = "350000741",
      SelectRegion = TRUE,
      #create Population
      pHosp = pHosp,
      SelPolygon = hospCovid[, FINESS_GEO == "350000741"] ,
      SelRegion = ListRegions == "Ile-de-France",
      colPolygon = ifelse(hospCovid[, FINESS_GEO == "350000741"], "orange", "blue"),
      colRegion = ifelse(ListRegions == "Ile-de-France", "orange", "blue"),
      alphaPolygon = ifelse(hospCovid[, FINESS_GEO == "350000741"], 0.4, 0.05),
      alphaRegion = ifelse(ListRegions == "Ile-de-France", 0.4, 0.05)
    )
    
    ## --- RENDER UI PARAMETERS -----------------------------------------------------
    output$PopSelectionUI = renderUI({
      tagList(
        #if (input$Selectregion){
          fluidRow(
            column(6, 
             (multiInput(inputId = ns("Region"), 
                             label = "Select",
                             choices = ListRegions,
                             selected = PopParameters$Region,
                             width = "100%",
                             options = list(enable_search = FALSE,
                                            non_selected_header = "Non-selected",
                                            selected_header = "Selected"))),
             shinyjs::hidden(multiInput(inputId = ns("Polygon"), 
                         label = "Select",
                         choiceNames = hospCovid[, paste(FINESS_GEO, 
                                                         Libelle, 
                                                         sep = "-")],
                         choiceValues = hospCovid[, FINESS_GEO],
                         selected = PopParameters$Polygon,
                         width = "100%",
                         options = list(enable_search = TRUE,
                                        non_selected_header = "Non-selected",
                                        selected_header = "Selected")))
            ),
            column(6, 
                   shinyjs::hidden(leaflet::leafletOutput(ns("mapPolygon"), height = 800)),
                   (leaflet::leafletOutput(ns("mapRegion"), height = 800))
                   )
          )
        #}
        # else{
        #   fluidRow(
        #     column(6, multiInput(inputId = ns("Polygon"), 
        #                          label = "Select",
        #                          choiceNames = hospCovid[, paste(FINESS_GEO, 
        #                                                          Libelle, 
        #                                                          sep = "-")],
        #                          choiceValues = hospCovid[, FINESS_GEO],
        #                          selected = NULL,
        #                          width = "100%",
        #                          options = list(enable_search = TRUE,
        #                                         non_selected_header = "Non-selected",
        #                                         selected_header = "Selected"))),
        #     column(6, leaflet::leafletOutput(ns("mapPolygon"), height = 700))
        #   )
        # }
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
        addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = isolate(PopParameters$alphaPolygon),
                    fillColor = isolate(PopParameters$colPolygon),
                    label = ~paste(FINESS_GEO,RS,sep="-"), layerId = ~FINESS_GEO)
      })
    output$mapRegion = leaflet::renderLeaflet({
      #browser()
      PopParameters$pHosp$vorPolyRegion %>%
        leaflet() %>%
        clearShapes() %>% 
        addTiles() %>%
        addPolylines() %>%
        # addCircleMarkers(lng = ~lng, 
        #                  lat = ~lat, 
        #                  opacity = 0.8, 
        #                  radius = 2, 
        #                  color = "red") %>% 
        addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = isolate(PopParameters$alphaRegion),
                    fillColor = isolate(PopParameters$colRegion),
                    label = ~Region, layerId = ~Region)
    })
    
    observeEvent(PopParameters$colPolygon,{
        leafletProxy(mapId = ns("mapPolygon"), data = isolate(PopParameters$pHosp$vorPolyHosp) ) %>%
        addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = PopParameters$alphaPolygon,
                    fillColor = isolate(PopParameters$colPolygon),
                    label = ~paste(FINESS_GEO,RS,sep="-"), layerId = ~FINESS_GEO)

    })
    observeEvent(PopParameters$colRegion,{
      leafletProxy(mapId = ns("mapRegion"), data = isolate(PopParameters$pHosp$vorPolyRegion) ) %>%
        addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = PopParameters$alphaRegion,
                    fillColor = isolate(PopParameters$colRegion),
                    label = ~Region, layerId = ~Region)
      
    })
    
    observeEvent(input$Polygon,{
      id <- which(PopParameters$pHosp$vorPolyHosp@data[,"FINESS_GEO"] %in% input$Polygon)
      
      if (length(id) > 0){
        PopParameters$SelPolygon = rep(FALSE, length(pHosp$vorPolyHosp@data[,"FINESS_GEO"]))
        PopParameters$colPolygon = rep("blue", length(pHosp$vorPolyHosp@data[,"FINESS_GEO"]))
        PopParameters$alphaPolygon = rep(0.05, length(pHosp$vorPolyHosp@data[,"FINESS_GEO"]))
        PopParameters$SelPolygon[id] <- TRUE
        PopParameters$colPolygon[id] <- "orange"
        PopParameters$alphaPolygon[id] <- 0.4
      }
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    observeEvent(input$Region,{
      id <- which(PopParameters$pHosp$vorPolyRegion@data[,Region] %in% input$Region)
      
      if (length(id) > 0){
        PopParameters$SelRegion = rep(FALSE, length(pHosp$vorPolyRegion@data$Region))
        PopParameters$colRegion = rep("blue", length(pHosp$vorPolyRegion@data$Region))
        PopParameters$alphaRegion = rep(0.05, length(pHosp$vorPolyRegion@data$Region))
        PopParameters$SelRegion[id] <- TRUE
        PopParameters$colRegion[id] <- "orange"
        PopParameters$alphaRegion[id] <- 0.4
      }
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    
    observeEvent(input$mapPolygon_shape_click,{
      ns <- session$ns
      # Polygon
        id <- grep(input$mapPolygon_shape_click$id,
                   PopParameters$pHosp$vorPolyHosp@data[,"FINESS_GEO"])

        if(length(id) > 0){
          if(PopParameters$SelPolygon[id] && sum(PopParameters$SelPolygon[-id]) > 0){
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
    
    observeEvent(input$mapRegion_shape_click,{
      #browser()
      ns <- session$ns
      # Polygon
      id <- grep(input$mapRegion_shape_click$id,
                 PopParameters$pHosp$vorPolyRegion@data$Region)
      
      if (length(id) > 0){
        # we perform the change unless it's the last one 
        if (PopParameters$SelRegion[id] && sum(PopParameters$SelRegion[-id]) > 0){
          PopParameters$SelRegion[id] <- FALSE
          PopParameters$colRegion[id] <- "blue"
          PopParameters$alphaRegion[id] <- 0.05
        }
        else{
          PopParameters$SelRegion[id] <- TRUE
          PopParameters$colRegion[id] <- "orange"
          PopParameters$alphaRegion[id] <- 0.4
        }
      }
      
      updateMultiInput(
        session,
        "Region",
        selected = PopParameters$pHosp$vorPolyRegion@data[PopParameters$SelRegion, Region])
    }, ignoreInit = TRUE)
    
    observeEvent(input$SelectRegion, {
      if (input$SelectRegion == "FINESS"){
        shinyjs::show("Polygon")
        shinyjs::show("mapPolygon")
        shinyjs::hide("Region")
        shinyjs::hide("mapRegion")
        PopParameters$SelectRegion = FALSE
      } else if (input$SelectRegion == "Region"){
        shinyjs::show("Region")
        shinyjs::show("mapRegion")
        shinyjs::hide("Polygon")
        shinyjs::hide("mapPolygon")
        PopParameters$SelectRegion = TRUE
      }
    })

    ## Get selected regions
    selectedRegions = reactive({
      if (PopParameters$SelectRegion){
        zones = ListRegions[PopParameters$SelRegion]
      } else {
        zones = hospCovid[PopParameters$SelPolygon, FINESS_GEO]
      }
        # x = input$Region
        # y = input$mapPolygon_shape_click$id
        # return(union(x,y))
      return(list(isRegion = PopParameters$SelectRegion, zones = zones))
    })

    return(selectedRegions)
}
    
## To be copied in the UI
# mod_pop_ui("pop_ui_1")

## To be copied in the server
# callModule(mod_pop_server, "pop_ui_1")

 
