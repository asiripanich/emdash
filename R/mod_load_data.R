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

      table_list <- getOption('emdash.supplementary_tables')
      
      # For each supplementary table, query the corresponding data
      for (t in table_list){
        table_type <- names(t)
        table_title <- t[[table_type]]$tab_name
        
        message(paste("About to load",table_title))
        data_r[[table_type]] <- query_supplementary(cons,table_type)
        
        if ("user_id" %in% colnames(data_r[[table_type]])){
          data_r[[table_type]] %>% normalise_uuid() %>% data.table::setcolorder(c("user_id"))
        }
        message(paste("Finished loading",table_title))
      }
      
      # output column names into R
      # data_r$trips %>% colnames() %>% dput()
      # data_r$participants %>% colnames() %>% dput()
      # data_r$trips_with_trajectories %>% colnames() %>% dput()
      
      message(sprintf("Participants size is: %s kb", format(object.size(data_r$participants), units = 'kB', standard = 'SI')))

      data_r$click <- runif(1)
    }, ignoreNULL = FALSE)
  
  message("Running: mod_load_data_server")
  
  return(data_r)
}

## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")

## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
