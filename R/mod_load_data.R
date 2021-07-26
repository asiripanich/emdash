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
      message("About to load participants")
      data_r$participants <-
        tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons)) %>%
        summarise_trips_without_trips(., cons) %>%
        summarise_server_calls(., cons)
      message("Finished loading participants")
      message(sprintf("Participants size is: %s kb", format(object.size(data_r$participants), units = "kB", standard = "SI")))

      table_list <- getOption("emdash.supplementary_tables")

      # For each supplementary table, query the corresponding data
      for (t in table_list) {
        table_type <- names(t)
        table_title <- t[[table_type]]$tab_name

        message(paste("About to load", table_title))
        if (table_type == 'Checkinout'){
          # Get bike check in and include the object ID so we can use it instead of user_id for CUD
          data_r[[table_type]] <- 
            cons$Checkinout$find(query = '{}',
                                 fields = '{"_id": 1, "user_id" : 1, "status": 1, "bikeLabel": 1}'
            ) %>% 
            as.data.table()
          
        } else {
          data_r[[table_type]] <- cons[[table_type]]$find("{}") %>%
            as.data.table()
        }

        if ("user_id" %in% colnames(data_r[[table_type]])) {
          data_r[[table_type]] %>%
            normalise_uuid() %>%
            data.table::setcolorder(c("user_id"))
        }
        message(paste("Finished loading", table_title))
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
