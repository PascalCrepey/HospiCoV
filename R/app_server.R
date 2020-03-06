#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
    callModule(mod_map_server, "map_ui_1")
    callModule(mod_model_server, "model_ui_1")

}

