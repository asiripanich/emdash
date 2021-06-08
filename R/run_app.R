#' Run the Shiny Application
#'
#' @param mongo_url address of the emisison mongodb server in mongo
#'   connection string [URI format](https://docs.mongodb.com/manual/reference/connection-string/).
#'   Default as `mongodb://localhost:27017`. If you are using the emdash docker container
#'   then this is set to `mongodb://host.docker.internal:27017`, however this may not
#'   work on Linux. If you are using Linux or Docker version prior to 18.03 you
#'   need to find the gateway ip of your Docker to connect to the database.
#' @param config_file path and file name of the YAML configuration file
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options

run_app <- function(mongo_url, config_file, ...) {
  if (!missing(mongo_url)) {
    checkmate::assert_string(mongo_url)
    options("emdash.mongo_url" = mongo_url)
  }

  # Get the global options found in the config file
  if (missing(config_file)) {
    message("No config file given, the default config file will be used.")
    config_file <- app_sys("config-default.yml")
  }
  config <- config::get(file = config_file)

  options(
    "emdash.disp_signup_trend" = config$display_signup_trend,
    "emdash.cols_to_remove_from_participts_table" = config$cols_to_remove_from_participts_table,
    "emdash.cols_to_remove_from_trips_table" = config$cols_to_remove_from_trips_table,
    "emdash.cols_to_remove_from_map_popup" = config$cols_to_remove_from_map_popup,
    "emdash.col_labels_for_participts" = config$col_labels_for_participts,
    "emdash.anon_locations" = config$anon_locations,
    'emdash.supplementary_tables' = config$supplementary_tables
  )

  app <- with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list(...)
  )

  print(app)
}
