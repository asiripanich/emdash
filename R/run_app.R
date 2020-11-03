#' Run the Shiny Application
#'
#' @param mongo_url address of the emisison mongodb server in mongo 
#'   connection string [URI format](https://docs.mongodb.com/manual/reference/connection-string/). 
#'   Defaut as `mongodb://localhost:27017`. If you are using the emdash docker container
#'   then this is set to `mongodb://host.docker.internal:27017`, however this may not 
#'   work on Linux. If you are using Linux or Docker version prior to 18.03 you
#'   need to find the gateway ip of your Docker to connect to the database.
#' @param ... A series of options to be used inside the app.
#' @param anon_locations (optional) a Boolean value. If TRUE, the UUIDs that are 
#' displayed on 'Maps' will be anonymised to 'user_1', 'user_2', etc.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(mongo_url, anon_locations, ...
) {
  if (!missing(mongo_url)) {
    options('emdash.mongo_url' = mongo_url)
  }
  if (missing(anon_locations)) {
    options('emdash.anon_locations' = FALSE)
  } else {
    options('emdash.anon_locations' = anon_locations)
  }
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(...)
  )
}
