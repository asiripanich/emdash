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
    actionButton(ns("reload_data"), label = "Reload participants data"),
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
      if (checkmate::test_file_exists(emdash_config_get("user_email_csv"), extension = "csv")) {
        user_email_csv <- read.csv(emdash_config_get("user_email_csv"))
        if (checkmate::test_character(user_email_csv$user_email, min.len = 1, all.missing = FALSE)) {
          stage_uuids <- query_stage_uuids(cons, user_emails = na.omit(user_email_csv$user_email))
        } else {
          stage_uuids <- query_stage_uuids(cons)
        }
      }
      message("About to load participants")
      data_r$participants <-
        tidy_participants(query_stage_profiles(cons), stage_uuids) %>%
        summarise_trips_without_trips(., cons) %>%
        summarise_timeuse(., cons) %>%
        summarise_server_calls(., cons)
      message("Finished loading participants")
      message(sprintf("Participants size is: %s kb", format(object.size(data_r$participants), units = "kB", standard = "SI")))

      message("Loading timeuse data")
      timeuse_records <- cons$Stage_timeseries$find(
        query = sprintf(
          '{
          "metadata.key" : "manual/survey_response",
          "data.name" : "TimeUseSurvey",
          %s
        }',
          mongo_create_find_in_uuid_query("user_id", stage_uuids$uuid_decoded)
        ),
          fields = '{
            "user_id": true,
            "metadata.write_fmt_time": true,
            "data.label": true,
            "data.response.sub_label": true,
            "data.response.activity_startdate": true,
            "data.response.activity_starttime": true,
            "data.response.activity_endtime": true
          }'
        ) 
        if (nrow(timeuse_records) != 0) {
          data_r$timeuse <- timeuse_records %>%
            as.data.table() %>%
            normalise_uuid() %>%
            .[, .SD, .SDcols = -patterns("^X_id$")] %>%
            merge(data_r$participants[, .(user_id, user_email)], by = "user_id") %>%
            data.table::setcolorder(c("user_id", "user_email"))
        } else {
          data_r$timeuse <- data.frame()
        }
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
