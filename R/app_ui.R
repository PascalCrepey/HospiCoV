
app_ui <- function() {
  tagList(
    # adding external resources
   golem_add_external_resources(),
    # List the first level UI elements here 
   navbarPage("HospiCov", inverse = T,
              tabPanel("Inputs",
                       mod_inputs_ui("inputs_ui_1")),
              tabPanel("Model",
                       mod_model_ui("model_ui_1")
                       ),
              tabPanel("About",
                       mod_about_ui("about_ui_1")
                       )
              )
  )
}

#' @import shiny
#' @importFrom shinyjs useShinyjs
golem_add_external_resources <- function(){

  ## addResourcePath(
  ##   'www', system.file('app/www', package = 'hospicov')
  ## )
  addResourcePath(
    'MD', system.file('MD', package = 'hospicov')
  )

  tags$head(
    shinyjs::useShinyjs(),
    # golem::activate_js(),
    # golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
  )
}
