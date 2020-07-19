#' gauge_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gauge_box_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' gauge_box Server Function
#'
#' @noRd 
mod_gauge_box_server <- function(input, output, session, value, min, max, color){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_gauge_box_ui("gauge_box_ui_1")
    
## To be copied in the server
# callModule(mod_gauge_box_server, "gauge_box_ui_1")
 
