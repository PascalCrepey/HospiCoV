# Module UI
  
#' @title   mod_map_ui and mod_map_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_map
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
          leafletOutput(ns("map"), height = 900)
  )
}
    
# Module Server
    
#' @rdname mod_map
#' @export
#' @keywords internal
    
mod_map_server <- function(input, output, session){
    ns <- session$ns
    source("R/TestVoronoi.R")
    output$map = renderLeaflet({ voronoi_map() })
}
    
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# callModule(mod_map_server, "map_ui_1")
 
