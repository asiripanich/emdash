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
    message("About to load trips")
    data_r$trips <- tidy_cleaned_trips(query_cleaned_trips(cons), 
                                       project_crs = get_golem_config("project_crs"))
    message("Finished loading trips")

    anon_locations <- getOption("emdash.anon_locations")
    message(anon_locations)
    if (is.null(anon_locations)) anon_locations = FALSE

    if (anon_locations)  {
        message("About to anonymize trajectories")
        data_r$anon_trips <- data_r$trips %>% anonymize_uuid
        message("Finished anonymizing trajectories")
    } else {
        data_r$anon_trips <- data_r$trips
    }

    message("About to load participants")
    data_r$participants <- 
      tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons)) %>%
      summarise_trips(., data_r$trips)
    message("Finished loading participants")
    
    data_r$click <- runif(1)
  }, ignoreNULL = FALSE)
 
  message("Running: mod_load_data_server")
  
  return(data_r)
  
}
    
## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")
    
## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
 
