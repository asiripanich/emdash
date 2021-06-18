#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_trips_ui <- function(id) {
  ns <- NS(id)
  cons <- connect_stage_collections(url = getOption("emdash.mongo_url"))
  last_trip <- query_max_trip_timestamp(cons) %>% 
    lubridate::as_datetime(.) %>% as.Date(.)
  
  message(last_trip)
  first_trip <- query_min_trip_timestamp(cons) %>% 
    lubridate::as_datetime(.) %>% as.Date(.)
  message(first_trip)
  
  tagList(
    dateRangeInput(ns("dates"),
                   "Select the range of dates for trip data", 
                   # start = "2016-01-05",
                   # end = "2016-01-05"),
                   start = last_trip - 30,
                   end = last_trip,
                   min = first_trip,
                   max = last_trip),
    textOutput(ns('load_display')),
    actionButton(inputId = ns('reload_trips'), label = 'Reload trips data'),
    textOutput(ns("last_load_datetime"))
  )
}

#' load_data Server Function
#'
#' @noRd
mod_load_trips_server <- function(input, output, session, cons) {
  ns <- session$ns
  
  max_window <- 31  # 1 month

  load_allowed <- reactive({
    window_width <- abs(as.numeric(difftime(input$dates[1],input$dates[2]))) + 1  # include first date
    message(paste("Window_width is",window_width))
    
    good_window <- (window_width <= max_window)
    
    if (good_window) {
      n_trips <- get_query_size(cons,input$dates)
      too_small <- is.null(n_trips)
      if (too_small){
        # When the window is too small, don't load data
        allow <- -1
      } else {
        # The date range is good. Load the data.
        allow <- 0
      }
    } else {
      # When the window is too large, don't load data
      allow <- 1
    }
    
    return(allow)
  })
  
  disp_load_status <- reactive({
    
    if (load_allowed() == 0) {
      out <- ' '
    } else if (load_allowed() == 1) {
      out <- "The selected date range is too wide."
    } else {
      out <- "There are no trips in that range."
    }
    return(cat(out))  # use cat to avoid the extra printed [1]
  })
  
  # When referring to reactives, remember to use parentheses
  output$load_display <- renderPrint(disp_load_status())
  
  data_geogr <- reactiveValues(data = data.frame(), name = "data")
  
  output$last_load_datetime <-
    renderText(paste0("Last loaded: ", as.character(Sys.time())))
  
  observeEvent(input$reload_trips,{

                 if (load_allowed() == 0) {
                   message("About to load trips")
                   data_geogr$trips <- tidy_cleaned_trips(query_cleaned_trips_by_timestamp(cons,input$dates),
                                                          project_crs = get_golem_config("project_crs")
                   )
                   message("Finished loading trips")
                   
                   message("About to load locations")
                   data_geogr$locations <- tidy_cleaned_locations(query_cleaned_locations_by_timestamp(cons,input$dates))
                   message("Finished loading locations")

                   message("About to create trajectories within trips")
                   data_geogr$trips_with_trajectories <- generate_trajectories(data_geogr$trips,
                     data_geogr$locations,
                     project_crs = get_golem_config("project_crs")
                   )
                   message("Finished creating trajectories within trips")
                   
                   # output column names into R
                   # data_geogr$trips %>% colnames() %>% dput()
                   # data_geogr$participants %>% colnames() %>% dput()
                   # data_geogr$trips_with_trajectories %>% colnames() %>% dput()
                   
                   data_geogr$click <- runif(1)
                 }
               },
               ignoreNULL = FALSE
  )
  
  message("Running: mod_load_data_server")
  return(data_geogr)
}

## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")

## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
