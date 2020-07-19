#' date_range_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_date_range_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    dateRangeInput("daterange", "Date range:",
                   start  = Sys.Date() - 7,
                   end    = Sys.Date(),
                   min  = Sys.Date() - 7,
                   max    = Sys.Date(),
                   format = "yy/mm/dd",
                   separator = " - ")
  )
}
    
#' date_range_input Server Function
#'
#' @noRd 
mod_date_range_input_server <- function(input, output, session){
  ns <- session$ns
 
  # updateDateRangeInput(
  #   
  # )
}
    
## To be copied in the UI
# mod_date_range_input_ui("date_range_input_ui_1")
    
## To be copied in the server
# callModule(mod_date_range_input_server, "date_range_input_ui_1")
 
