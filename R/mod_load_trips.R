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
    lubridate::as_datetime(.) %>%
    as.Date(.)
  message(paste("First trip:", first_trip))

  last_trip <- query_max_trip_timestamp(cons) %>%
    lubridate::as_datetime(.) %>%
    as.Date(.)
  message(paste("Last trip: ", last_trip))

  thirty_before_last_date <- last_trip - 30

  tagList(
    dateRangeInput(ns("dates"),
      "Select the range of dates for trip data",
      # start = "2016-01-05",
      # end = "2016-01-05"),
      start = thirty_before_last_date,
      end = last_trip,
      min = min(thirty_before_last_date, first_trip),
      max = last_trip
    ),
    textOutput(ns("load_display")),
    actionButton(inputId = ns("reload_trips"), label = "Reload trips data"),
    textOutput(ns("last_load_datetime"))
  )
}

#' load_data Server Function
#'
#' @noRd
mod_load_trips_server <- function(input, output, session, cons) {
  ns <- session$ns

  # Add one day to the final date because we want the date range to include the final date.
  # Converting these dates to timestamps gives us the timestamp at the beginning of the first user selected date,
  # and the timestamp at the end of the second user selected date = timestamp for the day after.
  # The timestamp for a given date is for the beginning of the day.
  dates <- reactive(c(input$dates[1], input$dates[2] + 1))

  load_trips_allowed <- reactive({
    message(
      sprintf(
        "The dates reactive values are: %s to %s",
        dates()[1],
        dates()[2]
      )
    )
    window_width <-
      difftime(dates()[2], dates()[1], units = "days") %>%
      as.numeric()
    message(sprintf("Window_width is %s days", window_width))

    n_trips <- get_n_trips_in_query(cons, dates())
    n_locations <- get_n_locations_in_query(cons,dates())
    
    message(sprintf('There are %s trips and %s locations in the date range',
                    n_trips,n_locations))

    if (n_trips > getOption("emdash.max_documents_for_mod_load_trips")) {
      return("The date range is too wide.")
    }

    if (is.null(n_trips)) {
      return("No trips in the selected date range.")
    }

    data_geogr$locations_allowed <- TRUE
    TRUE
  })

  # When referring to reactives, remember to use parentheses
  output$load_display <-
    renderPrint(cat(ifelse(isTRUE(load_trips_allowed()), "", load_trips_allowed())))

  data_geogr <- reactiveValues(data = data.frame(), name = "data")

  output$last_load_datetime <-
    renderText(paste0("Last loaded: ", as.character(Sys.time())))

  observeEvent(input$reload_trips,
    {
      message("Reload_trips observed")
      if (isTRUE(load_trips_allowed())) {
        
        get_size_kb <- function(x) {object.size(x)/1000}
        
        message("About to load trips")
        data_geogr$trips <- query_cleaned_trips_by_timestamp(cons, dates()) %>%
          tidy_cleaned_trips_by_timestamp() %>%
          normalise_uuid() %>%
          data.table::setorder(end_fmt_time) %>%
          tidy_cleaned_trips(project_crs = get_golem_config("project_crs"))
        message("Finished loading trips")
        message(paste('Trips size is: ',get_size_kb(data_geogr$trips)))

        message("About to load locations")
        data_geogr$locations <- tidy_cleaned_locations(query_cleaned_locations_by_timestamp(cons, dates()))
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
