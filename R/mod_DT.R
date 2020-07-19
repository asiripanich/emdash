#' DT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DT_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(DT::dataTableOutput(ns("DTtable")))
  )
}
    
#' DT Server Function
#' 
#' @param data a data.frame
#' @param tab_name table name
#'
#' @noRd 
mod_DT_server <- function(input, output, session, data){
  ns <- session$ns
  
  req(data)

  checkmate::assert_data_frame(data)
  
  output$DTtable <- DT::renderDataTable({
      DT::datatable(
        data,
        options = list(
          scrollX = TRUE,
          pageLength = 50,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        filter = list(position = 'top', clear = FALSE),
        extensions = 'Buttons'
      )
  })
  
}
    
## To be copied in the UI
# mod_DT_ui("DT_ui_1")
    
## To be copied in the server
# callModule(mod_DT_server, "DT_ui_1")
 
