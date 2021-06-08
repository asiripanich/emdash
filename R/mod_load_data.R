#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("reload_data"), label = "Reload data"),
    textOutput(ns("last_load_datetime"))
  )
}

#' load_data Server Function
#'
#' @noRd
mod_load_data_server <- function(input, output, session, cons) {
  ns <- session$ns

  data_r <- reactiveValues(data = data.frame(), name = "data")

  output$last_load_datetime <-
    renderText(paste0("Last loaded: ", as.character(Sys.time())))

  observeEvent(input$reload_data,
    {
      message("About to load trips")
      data_r$trips <- tidy_cleaned_trips(query_cleaned_trips(cons),
        project_crs = get_golem_config("project_crs")
      )
      message("Finished loading trips")

      message("About to load server calls")
      data_r$server_calls <- tidy_server_calls(query_server_calls(cons))
      message("Finished loading server calls")

      #       message("About to load locations")
      #       data_r$locations <- tidy_cleaned_locations(query_cleaned_locations(cons))
      #       message("Finished loading locations")
      #
      #       message("About to create trajectories within trips")
      #       data_r$trips_with_trajectories <- generate_trajectories(data_r$trips,
      #         data_r$locations,
      #         project_crs = get_golem_config("project_crs")
      #       )
      #       message("Finished creating trajectories within trips")

      message("About to load participants")
      data_r$participants <-
        tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons)) %>%
        summarise_trips(., data_r$trips) %>%
        summarise_server_calls(., data_r$server_calls)
      message("Finished loading participants")

      #       message("About to create trajectories within trips")
      #       data_r$trips_with_trajectories <- generate_trajectories(data_r$trips,
      #         data_r$locations,
      #         project_crs = get_golem_config("project_crs")
      #       )
      #       message("Finished creating trajectories within trips")
      #
      message("About to load participants")
      data_r$participants <-
        tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons)) %>%
        summarise_trips(., data_r$trips) %>%
        summarise_server_calls(., data_r$server_calls)
      message("Finished loading participants")

      # output column names into R
      # data_r$trips %>% colnames() %>% dput()
      # data_r$participants %>% colnames() %>% dput()
      # data_r$trips_with_trajectories %>% colnames() %>% dput()

      data_r$click <- runif(1)
    },
    ignoreNULL = FALSE
  )

  message("Running: mod_load_data_server")

  return(data_r)
}

## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")

## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
