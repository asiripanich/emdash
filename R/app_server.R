#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  

  # prepare data ------------------------------------------------------------
  cons <- connect_stage_collections(url = getOption('emdash.mongo_url'))  
  data_r <- callModule(mod_load_data_server, "load_data_ui", cons)
  
  # Side bar ----------------------------------------------------------------
  
  # Dashboard ---------------------------------------------------------------
  # Dashboard - boxes - start ----------
  callModule(
    mod_value_box_server,
    "value_box_ui_unique_users",
    value = paste0(nrow(data_r$participants),
                   " (+", sum(data_r$participants$update_ts == Sys.Date(), na.rm = T), ")"),
    subtitle = "unique users (new today)",
    icon = icon("users")
  )
  callModule(
    mod_value_box_server,
    "value_box_ui_active_users_today",
    value = sum(data_r$participants$n_trips_today != 0, na.rm = T),
    subtitle = "active users today",
    icon = icon("walking")
  )
  callModule(
    mod_value_box_server,
    "value_box_ui_total_trips",
    value = paste0(sum(data_r$participants$n_trips, na.rm = T), 
                   " (+", sum(data_r$participants$n_trips_today, na.rm = T), ")"),
    subtitle = "trips total (new today)",
    icon = icon("route")
  )
  callModule(
    mod_value_box_server,
    "value_box_ui_total_days",
    value = difftime(Sys.Date() ,as.Date(min(data_r$participants$update_ts)), units = "days"),
    subtitle = sprintf("days since the first user\n(%s to %s)", 
                       as.Date(min(data_r$participants$update_ts)), Sys.Date()),
    icon = icon("calendar-plus")
  )
  
  # Dashboard - boxes - end ----------
  
  # Dashboard - plots - start ----------
  callModule(
    mod_ggplotly_server,
    "ggplotly_ui_signup_trend",
    utils_plot_signup_trend(data_r$participants)
  )
  callModule(
    mod_ggplotly_server,
    "ggplotly_ui_trip_trend",
    utils_plot_trip_trend(data_r$trips)
  )
  callModule(
    mod_ggplotly_server,
    "ggplotly_ui_participation_period",
    utils_plot_participation_period(data_r$participants)
  )
  callModule(
    mod_ggplotly_server,
    "ggplotly_ui_branch",
    utils_plot_branch(data_r$participants)
  )
  callModule(
    mod_ggplotly_server,
    "ggplotly_ui_platform",
    utils_plot_platform(data_r$participants)
  )

  # Dashboard - plots - end ----------
  # Tables ------------------------------------------------------------------
  data_esquisse <- reactiveValues(data = data.frame(), name = "data_esquisse")
  
  # Convert the config's label map to a named vector. 
  # The names are the original labels, and the vector values are the new labels.
  named_label_vector <- unlist(getOption('emdash.col_labels_for_participts'))
  originalColumnNames <- names(named_label_vector)
  newColumnNames <- unname(named_label_vector)
  
  observeEvent(input$tabs, { 
    if (input$tabs  == "participants") {
      data_esquisse$data <- 
        data_r$participants %>%
        drop_list_columns() %>%
        data.table::setnames(originalColumnNames,newColumnNames,skip_absent = TRUE)
    }
    if (input$tabs == "trips") {
      data_esquisse$data <- 
        data_r$trips %>%
        drop_list_columns() %>%
        sf::st_drop_geometry()
    }
  })
  
  # INTERACTIVE PLOT PANEL
  callModule(
    esquisse::esquisserServer,
    "esquisse", 
    data = data_esquisse
  )
  
  # DATA TABS
  # 
  # use these to generate lists of columns to inform which columns to remove
  # data_r$participants %>% colnames() %>% dput()
  # data_r$trips %>% colnames() %>% dput()
  # POSSIBLE LINE: allNames[!(allNames %in% config$column_names)]
  # cols_to_remove_from_participts_table <- c("first_trip_datetime", 
  #                                            "last_trip_datetime")
  
  cols_to_remove_from_participts_table <- getOption('emdash.cols_to_remove_from_participts_table')
  cols_to_remove_from_trips_table <- getOption('emdash.cols_to_remove_trips_table')

  observeEvent(data_r$click, {
    callModule(mod_DT_server, "DT_ui_participants", 
               data = data_r$participants %>%
                 dplyr::select(-dplyr::any_of(cols_to_remove_from_participts_table)) %>%
                 data.table::setnames(originalColumnNames,newColumnNames,skip_absent = TRUE)
               )
    callModule(mod_DT_server, "DT_ui_trips",
               data = data_r$trips %>%
                 dplyr::select(-dplyr::any_of(cols_to_remove_from_trips_table)) %>%
                 sf::st_drop_geometry())
    callModule(mod_DT_server,"DT_ui_checkinout",
               data = data_r$checkinout)
  })
  
 
  
  # Maps --------------------------------------------------------------------
  
  # these lists of columns in trips_with_trajectories can inform  
  # 1) which columns to remove in the map filter
  # 2) which columns to remove to pass to the map and show up in the map popups

  # data_r$trips_with_trajectories %>% colnames() %>% dput()
  
  cols_to_include_in_map_filter <- reactive({
    data_r$trips_with_trajectories %>%
    colnames() %>%
    # specify columns to remove here
    setdiff(c("start_fmt_time0", "start_local_dt_timezone", "start_local_time", 
              "end_fmt_time0", "end_local_dt_timezone", "end_local_time", 
              "end_loc_coordinates", "start_loc_coordinates", "duration", "distance", 
              "location_points", "source"))
    })
    
  filtered_trips <- 
    callModule(
      module = esquisse::filterDF,
      id = "filtering",
      data_table = reactive(anonymize_uuid_if_required(data_r$trips_with_trajectories)),
      data_name = reactive("data"),
      data_vars = cols_to_include_in_map_filter, # the map filter uses start_fmt_time and end_fmt_time (UTC time)
      drop_ids = FALSE
    )
  
  cols_to_remove_from_map_popup <- getOption('emdash.cols_to_remove_from_map_popup')
  
  observeEvent(filtered_trips$data_filtered(), {
    callModule(
      mod_mapview_server,
      "mapview_trips",
      data_sf = filtered_trips$data_filtered() %>% 
        dplyr::select(-dplyr::any_of(cols_to_remove_from_map_popup))
    )
  })
  
  # On exit -----------------------------------------------------------------
  session$onSessionEnded(function() {
    message("disconnecting from the emission collections..")
    lapply(cons, function(.x) {
      .x$disconnect()
    })
  })
  
}


