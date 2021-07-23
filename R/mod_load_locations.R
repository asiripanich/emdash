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

  fluidRow(
    align = "center",
    actionButton(inputId = ns("get_trajectories"), label = "Get Locations and Trajectories"),
    htmlOutput(ns("locations_allowed_message"))
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
  use_startup_message <- reactiveVal(TRUE)
  max_n_docs <- getOption("emdash.max_documents_for_mod_load_trips")
  min_locations_per_trip <- getOption("emdash.min_locations_per_trip")

  # Check the number of trips, the number of locations, and whether trips are ready.
  # update messages and whether locations are allowed accordingly.
  locations_info <- reactive({

    # Handle the case of no trips, eg the user makes the first date a later date than the second date.
    if (is.null(data_geogr$n_trips())) {
      # Skip all other checks.
      out <- list(allow_load = FALSE, sample_size = FALSE, message = "No trips in the selected date range.")
    } else {
      n_trips <- data_geogr$n_trips()
      n_locations <- data_geogr$n_locations()

      # trips + trajectories <= max_documents: get all locations
      # else trips + min_locations_per_trip * trips <= max_documents: get sample
      # else trips + min_locations_per_trip * trips > max_documents: warn user that we can't do this
      # downsample will be either false or the number of locations to include

      # Only downsample in case 2.
      downsample <- FALSE

      if (n_trips + n_locations <= max_n_docs) {
        allow <- TRUE
        loc_message <- "For the current date range, all location points can be used."
      } else if (n_trips * (1 + min_locations_per_trip) <= max_n_docs) {
        allow <- TRUE
        downsample <- max_n_docs - n_trips # this will serve as our sample size.
        loc_message <- sprintf(
          "Will only use %s locations per trip. For more detailed trajectories, use a smaller date range.",
          ceiling(downsample / n_trips)
        )
      } else if (n_trips * (1 + min_locations_per_trip) > max_n_docs) {
        allow <- FALSE
        loc_message <- "Cannot get trajectories. The selected date range has too many trips."
      }

      # if trips are ready, update allow and downsample
      # Otherwise, add a second portion to the locations message and do not allow locations.
      if (data_geogr$trips_ready() == TRUE) {
        out <- list(allow_load = allow, sample_size = downsample, message = loc_message)
      } else {
        extra_message <- ifelse(use_startup_message(),
          "Need to load trips first.",
          "To get trajectories again, load trips first."
        )
        loc_message <- paste(extra_message, loc_message, sep = "<br/>")
        out <- list(allow_load = FALSE, sample_size = FALSE, message = loc_message)
      }
    }
    return(out)
  })

  # Initialize locations_ready as FALSE
  data_geogr$locations_ready <- reactiveVal(FALSE)

  # Tell the user the status of what Get Locations and Trajectories will do.
  output$locations_allowed_message <- renderUI({
    HTML(locations_info()$message)
  })

  # Observe for when get_trajectories gets clicked.
  observeEvent(input$get_trajectories, {
    if (locations_info()$allow_load == TRUE) {
      message("About to load locations")

      # if sample_size is not false, use it to limit the number of locations queried.
      if (locations_info()$sample_size != FALSE) {
        data_geogr$locations <- downsample_cleaned_locations_by_timestamp(
          cons, data_geogr$dates(),
          locations_info()$sample_size
        ) %>%
          tidy_cleaned_locations()
      } else {
        data_geogr$locations <- tidy_cleaned_locations(query_cleaned_locations_by_timestamp(cons, data_geogr$dates()))
      }
      message("Finished loading locations")
      message(sprintf("Locations size is: %s", format(object.size(data_geogr$locations), units = "kB", standard = "SI")))

      message("About to create trajectories within trips")
      data_geogr$trips_with_trajectories <- generate_trajectories(data_geogr$trips,
        data_geogr$locations,
        project_crs = get_golem_config("project_crs")
      )
      message("Finished creating trajectories within trips")
      message(sprintf("Trips with trajectories size is: %s", format(object.size(data_geogr$trips_with_trajectories), units = "kB", standard = "SI")))

      # Now that get_trajectories has been clicked and locations_info()$allow_load is TRUE,
      # update data_geogr$locations_ready() to TRUE.
      # map_data will notice the change in data_geogr$locations_ready() and the map will be updated accordingly
      data_geogr$locations_ready(TRUE)

      # Prevent user from loading locations again until after trips has been reloaded.
      data_geogr$trips_ready(FALSE)

      # Now that locations and trajectories have been loaded once, change how you tell user to load trips.
      use_startup_message(FALSE)
    }
  })

  # observeEvent(data_geogr$click, {
  #   data_geogr$locations_ready(FALSE)
  #   shinyjs::show('get_trajectories', asis = TRUE)
  # })

  return(data_geogr)
}
