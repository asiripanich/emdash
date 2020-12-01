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
    message("Loading participants")
    data_r$participants <- 
      tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons))
    
    if (is.null(data_r$participants)) {
      stop("There are no participants yet, wait until there is at least one ",
           "participant and try again. :(")
    }
    
    message("Loading trips")
    data_r$trips <- tidy_cleaned_trips(query_cleaned_trips(cons), 
                                       project_crs = get_golem_config("project_crs"))
    data_r$trips = data_r$trips[0, ]
    
    message("Loading server calls")
    data_r$server_calls <- tidy_server_calls(query_server_calls(cons))
    data_r$server_calls = data_r$server_calls[0, ]
    
    message("Loading locations")
    data_r$locations <- tidy_cleaned_locations(query_cleaned_locations(cons))
    data_r$locations = data_r$locations[0, ]
    
    message("Creating trajectories within trips")
    data_r$trips_with_trajectories <-
      generate_trajectories(data_r$trips,
                            data_r$locations,
                            project_crs = get_golem_config("project_crs"))
  
    message("Adding summarised trips to participants.") 
    data_r$participants = summarise_trips(data_r$participants, data_r$trips)
  
    message("Adding summarised server calls to participants.") 
    data_r$participants = summarise_server_calls(data_r$participants, data_r$server_calls)
  
    data_r$click <- runif(1)
  }, ignoreNULL = FALSE)
 
  message("Running: mod_load_data_server")
  
  return(data_r)
  
}
    
## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")
    
## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
 
