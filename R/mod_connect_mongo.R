#' connect_mongo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_connect_mongo_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' connect_mongo Server Function
#'
#' @noRd 
mod_connect_mongo_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_connect_mongo_ui("connect_mongo_ui_1")
    
## To be copied in the server
# callModule(mod_connect_mongo_server, "connect_mongo_ui_1")
 
