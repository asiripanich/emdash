#' load_locations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_locations_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(align = "center",
           actionButton(inputId = ns("get_trajectories"), label = "Get trajectories"),
           textOutput(ns('locations_allowed_message'))
           )
}

#' load_locations Server Function
#' @param cons is the mongolite connection to the database
#' @param data_geogr is a reactive dataframe with trips data and locations info
#' @noRd
mod_load_locations_server <- function(input, output, session, cons, data_geogr) {
  ns <- session$ns

  # Load Locations and generate trajectories if get_trajectories is clicked 
  # and locations_info()$allow_load is true.
  
  # Use this to determine the message to show for locations allowed before trips is loaded.
  startup <- reactiveVal(TRUE)
  max_n_docs <- getOption("emdash.max_documents_for_mod_load_trips")
  min_locations_per_trip <- getOption("emdash.min_locations_per_trip")
  
    locations_info <- reactive({
      
      # if trips are ready, check whether it is ok to load locations.
      if (data_geogr$trips_ready() == TRUE) {
        n_trips <- data_geogr$n_trips()
        n_locations <- data_geogr$n_locations()
        
        # trips + trajectories < max_documents: get all locations
        # else trips + 100 * trips < max_documents: get sample
        # else trips + 100 * trips > max_documents: warn user that we can't do this
        # downsample will be either false or the number of locations to include
        downsample <- FALSE
        if (n_trips + n_locations < max_n_docs) {
          allow <- TRUE
          loc_message <- '' # All location points can be used
        } else if (n_trips + min_locations_per_trip * n_trips  <= max_n_docs) {
          allow <- TRUE
          downsample <- max_n_docs - n_trips   # this will serve as our sample size.
          loc_message <- sprintf("Will only use %s locations per trip. For more detailed trajectories, use a smaller date range.", 
                                 ceiling(downsample/n_trips))
          
        } else if (n_trips + min_locations_per_trip * n_trips > max_n_docs){
          allow <- FALSE
          loc_message <- "Cannot get trajectories. The selected date range has too many trips."
        }
        
        return(list(allow_load = allow,sample_size = downsample, message = loc_message))
      
      } else {   # Otherwise, tell user to load trips.
          loc_message <- ifelse(startup(),'Need to load trips first.', 'To get trajectories again, load trips first.')
          return(list(allow_load = FALSE, sample_size = FALSE, message = loc_message))
        }
    })

  
  # Initialize locations_ready as FALSE
  # data_geogr <- reactiveValues(data = data.frame(), name = "data")
  data_geogr$locations_ready <- reactiveVal(FALSE)
  
  # Notify the user if either the locations are downsampled or if you cannot load locations.
  output$locations_allowed_message <-
    renderPrint(cat(locations_info()$message))
  
  # Observe for when get_trajectories gets clicked.
  observeEvent(input$get_trajectories, {

    if (locations_info()$allow_load == TRUE) {
      get_size_kb <- function(x) {object.size(x)/1000}
      
      message("About to load locations")
      
      # if sample_size is not false, use it to limit the number of locations queried.
      if (locations_info()$sample_size != FALSE){
        data_geogr$locations <- downsample_cleaned_locations_by_timestamp(cons,data_geogr$dates(), 
                                                                          locations_info()$sample_size) %>% 
                                  tidy_cleaned_locations()
      } else {
        data_geogr$locations <- tidy_cleaned_locations(query_cleaned_locations_by_timestamp(cons, data_geogr$dates()))
      }
      message("Finished loading locations")
      message(sprintf('Locations size is: %s kb',get_size_kb(data_geogr$locations)))
      
      message("About to create trajectories within trips")
      data_geogr$trips_with_trajectories <- generate_trajectories(data_geogr$trips,
                                                                  data_geogr$locations,
                                                                  project_crs = get_golem_config("project_crs")
      )
      message("Finished creating trajectories within trips")
      message(sprintf('Trips with trajectories size is: %s kb',get_size_kb(data_geogr$trips_with_trajectories)))
      
      # Now that get_trajectories has been clicked and locations_info()$allow_load is TRUE, 
      # update data_geogr$locations_ready() to TRUE.
      # map_data will notice the change in data_geogr$locations_ready() and the map will be updated accordingly
      data_geogr$locations_ready(TRUE)
      
      # Prevent user from loading locations again until after trips has been reloaded.
      data_geogr$trips_ready(FALSE)
      
      # After preparing locations, change locations allowed message.
      startup(FALSE)  
    }
  })
  
  # observeEvent(data_geogr$click, {
  #   data_geogr$locations_ready(FALSE)
  #   shinyjs::show('get_trajectories', asis = TRUE)
  # })
  
  return(data_geogr)
}
