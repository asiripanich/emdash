#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("reload_data"), label = "Reload data"),
    textOutput(ns("last_load_datetime"))
  )
}
    
#' load_data Server Function
#'
#' @noRd 
mod_load_data_server <- function(input, output, session, cons){
  ns <- session$ns
  
  data_r <- reactiveValues(data = data.frame(), name = "data")
  
  output$last_load_datetime <- 
    renderText(paste0("Last loaded: ", as.character(Sys.time())))
  
  observeEvent(input$reload_data, {
    
    data_r$trips <-
      complete_trips(
        query_cleaned_trips(cons)  %>% tidy_cleaned_trips(.),
        query_trip_ends(cons) %>% tidy_trip_ends(.),
        project_crs = get_golem_config("project_crs")
      )
    
    data_r$participants <- 
      tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons)) %>%
      summarise_trips(., data_r$trips)
    
    data_r$click <- runif(1)
  }, ignoreNULL = FALSE)
 
  message("Running: mod_load_data_server")
  
  return(data_r)
  
}
    
## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")
    
## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
 
