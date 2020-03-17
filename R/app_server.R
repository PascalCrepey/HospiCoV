#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
    callModule(mod_map_server, "map_ui_1")
    selectedRegions = callModule(mod_pop_server, "pop_ui_1")
    callModule(mod_model_server, "model_ui_1", selectedRegions, data)
    data = callModule(mod_inputs_server, "inputs_ui_1")

}

