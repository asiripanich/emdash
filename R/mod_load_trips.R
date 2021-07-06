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

  first_trip <- query_min_trip_timestamp(cons) %>% 
    lubridate::as_datetime(.) %>% as.Date(.)
  message(paste('First trip:',first_trip))
  
  last_trip <- query_max_trip_timestamp(cons) %>% 
    lubridate::as_datetime(.) %>% as.Date(.)
  message(paste('Last trip: ',last_trip))
  
  thirty_before_last_date <- last_trip - 30

  tagList(
    dateRangeInput(ns("dates"),
                   "Select the range of dates for trip data", 
                   # start = "2016-01-05",
                   # end = "2016-01-05"),
                   start = thirty_before_last_date,
                   end = last_trip,
                   min = min(thirty_before_last_date,first_trip),
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
  
  # Add one day to the final date because we want the date range to include the final date in the date range.
  # The timestamp for a given date is for the beginning of the day.
  # Looking one day later than the final date and converting to timestamp,
  # we get the time stamp at the end of the final day, which is the same as the beginning of the day after.
  dates <- reactive(c(input$dates[1],input$dates[2] + 1))

  load_allowed <- reactive({
    message('The dates reactive values are:')
    message(dates()[1])
    message(dates()[2])
    window_width <- abs(as.numeric(difftime(dates()[1],dates()[2])))
    message(paste("Window_width is",window_width))
    
    good_window <- (window_width <= max_window)
    
    if (good_window) {
      n_trips <- get_query_size(cons,dates())
      message(paste('Number of trips is:',n_trips))
      no_trips <- is.null(n_trips)
      if (no_trips){
        # When the window contains no trips, don't load data
        allow <- -1
      } else {
        # The date range is good. Load the data.
        allow <- 0
      }  # inner 'if-else' end
    } else {
      # When the window is too large, don't load data
      allow <- 1
    }
    
    return(allow)
  })
  
  # When there is a change in load_allowed(), change the displayed message
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
                 message('Reload_trips observed')
                 if (load_allowed() == 0) {
                   message("About to load trips")
                   data_geogr$trips <- tidy_cleaned_trips(query_cleaned_trips_by_timestamp(cons,dates()),
                                                          project_crs = get_golem_config("project_crs")
                   )
                   
                   get_size_kb <- function(x) {object.size(x)/1000}
                   message("Finished loading trips")
                   message(paste('Trips size is: ',get_size_kb(data_geogr$trips)))
                   
                   message("About to load locations")
                   data_geogr$locations <- tidy_cleaned_locations(query_cleaned_locations_by_timestamp(cons,dates()))
                   message("Finished loading locations")
                   
                   message(paste('Locations size is: ',get_size_kb(data_geogr$locations)))

                   message("About to create trajectories within trips")
                   data_geogr$trips_with_trajectories <- generate_trajectories(data_geogr$trips,
                     data_geogr$locations,
                     project_crs = get_golem_config("project_crs")
                   )
                   message("Finished creating trajectories within trips")
                   
                   message(paste('Trips with traj. size is: ',get_size_kb(data_geogr$trips_with_trajectories)))
                   
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
