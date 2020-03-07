## #' Run the Shiny Application
## #'
## #' @export
## #' @importFrom shiny shinyApp
## #' @importFrom golem with_golem_options
## #run_app <- function(...) {
##   with_golem_options(


## Source ui and server
source("R/app_ui.R")
source("R/app_server.R")

## Source modules
source("R/mod_map.R")
source("R/mod_model.R")

app = shinyApp(ui = app_ui, server = app_server)
    ## golem_opts = list(...) 
##   )
## }
