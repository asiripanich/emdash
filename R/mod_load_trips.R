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

  start_date <- last_trip - getOption("emdash.load_trips_start_ndays") + 1

  tagList(
    dateRangeInput(ns("dates"),
      "Select the range of dates for trip data",
      # start = "2016-01-05",
      # end = "2016-01-05"),
      start = start_date,
      end = last_trip,
      min = min(start_date, first_trip),
      max = last_trip
    ),
    textOutput(ns("load_display")),
    actionButton(inputId = ns("load_trips"), label = "Load trips data"),
    textOutput(ns("last_load_datetime"))
  )
}

#' load_data Server Function
#'
#' @noRd
mod_load_trips_server <- function(input, output, session, cons) {
  ns <- session$ns

  data_geogr <- reactiveValues(data = data.frame(), name = "data")
  max_n_docs <- getOption("emdash.max_documents_for_mod_load_trips")

  # Initialize trips ready as FALSE
  data_geogr$trips_ready <- reactiveVal(FALSE)

  # Add one day to the final date because we want the date range to include the final date.
  # Converting these dates to timestamps gives us the timestamp at the beginning of the first user selected date,
  # and the timestamp at the end of the second user selected date = timestamp for the day after.
  # The timestamp for a given date is for the beginning of the day.
  data_geogr$dates <- reactive({

    # Set trips_ready to false so that when a user changes the dates they first have to load trips for those dates
    # before loading locations and generating trajectories
    data_geogr$trips_ready(FALSE)

    dates <- c(input$dates[1], input$dates[2] + 1)
    message(
      sprintf(
        "The dates reactive values are: %s to %s",
        dates[1],
        dates[2]
      )
    )

    window_width <-
      difftime(dates[2], dates[1], units = "days") %>%
      as.numeric()
    message(sprintf("Window_width is %s days", window_width))

    return(dates)
  })

  # Find the how many trip and location documents are in the specified date range.
  data_geogr$n_trips <- reactive({
    get_n_trips_in_query(cons, data_geogr$dates())
  })
  data_geogr$n_locations <- reactive({
    get_n_locations_in_query(cons, data_geogr$dates())
  })

  load_trips_allowed <- reactive({
    n_trips_message <- TRUE

    # Check if n_trips is null before comparing to max docs.
    if (is.null(data_geogr$n_trips())) {
      out <- "No trips in the selected date range."
      n_trips_message <- FALSE
    } else if (data_geogr$n_trips() > max_n_docs) {
      out <- "The date range is too wide."
    } else {
      out <- TRUE
    }

    if (n_trips_message) {
      message(sprintf(
        "There are %s trips and %s locations in the date range",
        data_geogr$n_trips(), data_geogr$n_locations()
      ))
    }
    return(out)
  })

  # When referring to reactives, remember to use parentheses
  output$load_display <-
    renderPrint(cat(ifelse(isTRUE(load_trips_allowed()), "Ready to load trips", load_trips_allowed())))

  output$last_load_datetime <-
    renderText(paste0("Last loaded: ", as.character(Sys.time())))

  observeEvent(input$load_trips,
    {
      message("Load_trips observed")
      if (isTRUE(load_trips_allowed())) {
        message("About to load trips")
        data_geogr$trips <- query_cleaned_trips_by_timestamp(cons, data_geogr$dates()) %>%
          tidy_cleaned_trips_by_timestamp() %>%
          normalise_uuid() %>%
          data.table::setorder(end_fmt_time) %>%
          tidy_cleaned_trips(project_crs = get_golem_config("project_crs"))
        message("Finished loading trips")
        message(sprintf("Trips size is: %s", format(object.size(data_geogr$trips), units = "kB", standard = "SI")))

        # output column names into R
        # data_geogr$trips %>% colnames() %>% dput()
        # data_geogr$participants %>% colnames() %>% dput()
        # data_geogr$trips_with_trajectories %>% colnames() %>% dput()

        data_geogr$click <- runif(1)

        # Set trips ready to TRUE. Now locations can be loaded, as long as the criteria within mod_load_locations --> location_info are met
        data_geogr$trips_ready(TRUE)

        data_geogr$locations_ready(FALSE) # now that trips has changed, we do not want to use them in the map.
      }
    },
    ignoreNULL = FALSE,
    # ignoreInit = TRUE if you don't want any trips to load on startup
    ignoreInit = !getOption("emdash.load_trips_on_startup")
  )

  message("Running: mod_load_trips_server")
  return(data_geogr)
}

## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")

## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
