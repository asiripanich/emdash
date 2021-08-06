#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
app_server <- function(input, output, session) {
  # List the first level callModules here

  # prepare data ------------------------------------------------------------
  cons <- connect_stage_collections(url = getOption("emdash.mongo_url"))
  data_r <- callModule(mod_load_data_server, "load_data_ui", cons)
  data_geogr <-
    callModule(mod_load_trips_server, "load_trips_ui", cons, data_r) %>% {
      callModule(mod_load_locations_server, "load_locations_ui", cons, .)
    }

  # Side bar ----------------------------------------------------------------

  # Dashboard ---------------------------------------------------------------
  # Dashboard - boxes - start ----------
  callModule(
    mod_value_box_server,
    "value_box_ui_unique_users",
    value = paste0(
      nrow(data_r$participants),
      " (+", sum(data_r$participants$update_ts == Sys.Date(), na.rm = T), ")"
    ),
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
    value = paste0(
      sum(data_r$participants$n_trips, na.rm = T),
      " (+", sum(data_r$participants$n_trips_today, na.rm = T), ")"
    ),
    subtitle = "trips total (new today)",
    icon = icon("route")
  )
  callModule(
    mod_value_box_server,
    "value_box_ui_total_days",
    value = difftime(Sys.Date(), as.Date(min(data_r$participants$update_ts)), units = "days"),
    subtitle = sprintf(
      "days since the first user\n(%s to %s)",
      as.Date(min(data_r$participants$update_ts)), Sys.Date()
    ),
    icon = icon("calendar-plus")
  )

  # # Dashboard - boxes - end ----------
  #
  # # Dashboard - plots - start ----------
  callModule(
    mod_ggplotly_server,
    "ggplotly_ui_signup_trend",
    utils_plot_signup_trend(data_r$participants)
  )

  observeEvent(data_geogr$click, {
    callModule(
      mod_ggplotly_server,
      "ggplotly_ui_trip_trend",
      utils_plot_trip_trend(data_geogr$trips)
    )
  })

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
  named_label_vector <- unlist(getOption("emdash.col_labels_for_participants"))
  original_col_labels_for_participants <- names(named_label_vector)
  new_col_labels_for_participants <- unname(named_label_vector)

  observeEvent(input$tabs, {
    # Make sure trips exists before attempting to manipulate it
    if (!is.null(data_geogr$trips) && input$tabs == "trips") {
      data_esquisse$data <-
        data_geogr$trips %>%
        dplyr::select(dplyr::any_of(getOption('emdash.cols_to_include_in_trips_table'))) %>%  # select the columns we want
        drop_list_columns() %>%
        sf::st_drop_geometry()
    }

    if (input$tabs %in% names(data_r)) {
      data_esquisse$data <-
        data.table::copy(data_r[[input$tabs]]) %>%
        drop_list_columns()

      if (input$tabs == "participants") {
        data.table::setnames(
          data_esquisse$data,
          original_col_labels_for_participants,
          new_col_labels_for_participants,
          skip_absent = TRUE
        )
      }
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
  # data_geogr$trips %>% colnames() %>% dput()
  # POSSIBLE LINE: allNames[!(allNames %in% config$column_names)]
  # cols_to_remove_from_participts_table <- c("first_trip_datetime",
  #                                            "last_trip_datetime")

  # Only append supplementary table tabs on startup.
  startup_load_supplementary <- reactiveVal(TRUE)
  tableList <- getOption("emdash.supplementary_tables")

  observeEvent(data_r$click, {
    callModule(mod_DT_server, "DT_ui_participants",
      data = data_r$participants %>%
        dplyr::select(-dplyr::any_of(getOption("emdash.cols_to_remove_from_participts_table"))) %>%
        data.table::setnames(
          original_col_labels_for_participants,
          new_col_labels_for_participants,
          skip_absent = TRUE
        )
    )

    # For each supplementary table, append a new tabPanel and run the server function
    # that specifies table behavior
    for (t in tableList) {
      table_type <- names(t)
      table_title <- t[[table_type]]$tab_name

      suppl_table <- data_r[[table_type]]

      # Set the options used by DT::renderDataTable within dtedit and mod_DT
      datatable_options <- list(
        scrollX = TRUE,
        pageLength = 50,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print", "colvis")
      )

      # If the table has a timestamp, make a copy of the timestamp column called fmt_time
      if ("ts" %in% colnames(suppl_table)) {
        suppl_table[["fmt_time"]] <- suppl_table[["ts"]]
        fmt_time_index <- which(names(suppl_table) == "fmt_time")

        # Add columnDefs to datatable options to convert fmt_time to a Date
        datatable_options[["columnDefs"]] <- list(list(
          # target column indices start from 0.
          # However, DT adds a row number column to the display as the 0th column
          targets = fmt_time_index,
          render = htmlwidgets::JS(
            "function(data, type, row) {",
            "return new Date(data*1000);",
            "}"
          )
        ))
      }

      suppl_table <- data_r[[table_type]]

      # Set the options used by DT::renderDataTable within dtedit and mod_DT
      datatable_options <- list(
        scrollX = TRUE,
        pageLength = 50,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print", "colvis")
      )

      # If the table has a timestamp, make a copy of the timestamp column called fmt_time
      if ("ts" %in% colnames(suppl_table)) {
        suppl_table[["fmt_time"]] <- suppl_table[["ts"]]
        fmt_time_index <- which(names(suppl_table) == "fmt_time")

        # Add columnDefs to datatable options to convert fmt_time to a Date
        datatable_options[["columnDefs"]] <- list(list(
          # target column indices start from 0.
          # However, DT adds a row number column to the display as the 0th column
          targets = fmt_time_index,
          render = htmlwidgets::JS(
            "function(data, type, row) {",
            "return new Date(data*1000);",
            "}"
          )
        ))
      }

      # If the table is Bike Check In and there is an 'editable' field, use dtedit
      if (table_type == "Checkinout" & "editable" %in% names(t$Checkinout)) {
        if (isTRUE(startup_load_supplementary())) {
          editable_table_tab <- tabPanel(
            status = "primary",
            title = table_title,
            value = table_type,
            mod_DTedit_ui(id = paste0("DTedit_", table_type))
          )

          appendTab(
            inputId = "tabs",
            tab = editable_table_tab,
            select = FALSE,
            menuName = NULL,
            session = getDefaultReactiveDomain()
          )
        }

        callModule(
          module = mod_DTedit_server,
          id = paste0("DTedit_", table_type),
          table_data = suppl_table,
          table_type = table_type,
          suppl_table_sublist = t,
          DT_options = datatable_options,
          cons
        )
      }
      # For other supplementary tables, use mod_DT
      else {
        if (isTRUE(startup_load_supplementary())) {
          regular_tab <- tabPanel(
            status = "primary",
            title = table_title,
            value = table_type,
            mod_DT_ui(id = paste0("DT_ui_", table_type))
          )

          appendTab(
            inputId = "tabs",
            tab = regular_tab,
            select = FALSE,
            menuName = NULL,
            session = getDefaultReactiveDomain()
          )
        }

        # Run mod_DT_server using data for the current table
        callModule(
          module = mod_DT_server,
          id = paste0("DT_ui_", table_type),
          data = suppl_table,
          DT_options = datatable_options
        )
      }
    }
    startup_load_supplementary(FALSE)
  })

  observeEvent(data_geogr$click, {
    callModule(mod_DT_server, "DT_ui_trips",
      data = data_geogr$trips %>%
        dplyr::select(dplyr::any_of(getOption('emdash.cols_to_include_in_trips_table'))) %>% 
        #dplyr::select(-dplyr::any_of(getOption("emdash.cols_to_remove_from_trips_table"))) %>%
        sf::st_drop_geometry()
    )
  })


  # Maps --------------------------------------------------------------------

  # these lists of columns in trips_with_trajectories can inform
  # 1) which columns to remove in the map filter
  # 2) which columns to remove to pass to the map and show up in the map popups

  # data_geogr$trips_with_trajectories %>% colnames() %>% dput()

  # Set the data used for the map.
  # Use trips_with_trajectories when locations and trajectories are ready.
  # This will update each time locations_ready, trips, or trips with trajectories updates
  map_data <- reactive({
    if (data_geogr$locations_ready() == TRUE) {
      message("Adding trajectories to map.")
      return(data_geogr$trips_with_trajectories)
    } else {
      message("Mapping without trajectories")
      return(data_geogr$trips)
    }
  })

  # This returns a list of column names
  cols_to_include_in_map_filter <- reactive({
    map_data() %>%
      colnames() %>%
      # specify columns to remove here
      setdiff(c(
        "start_fmt_time0", "start_local_dt_timezone", "start_local_time",
        "end_fmt_time0", "end_local_dt_timezone", "end_local_time",
        "end_loc_coordinates", "start_loc_coordinates", "duration", "distance",
        "location_points", "source"
      ))
  })

  # Filter the trips data before passing to the map.
  filtered_trips <- reactive({
    # Do nothing if data_geogr$trips does not exist, which happens on startup
    req(data_geogr$trips)

    callModule(
      module = esquisse::filterDF,
      id = "filtering",
      data_table = reactive(anonymize_uuid_if_required(map_data())),
      data_name = reactive("data"),
      data_vars = cols_to_include_in_map_filter, # the map filter uses start_fmt_time and end_fmt_time (UTC time)
      drop_ids = FALSE
    )
  })

  observeEvent(filtered_trips()$data_filtered(), {
    # since filtered trips and data filtered are both reactive, need to include parentheses after each. otherwise you get:
    # Error in UseMethod: no applicable method for 'select' applied to an object of class "c('reactiveExpr', 'reactive', 'function')"

    callModule(
      mod_mapview_server,
      "mapview_trips",
      data_sf = filtered_trips()$data_filtered() %>%
        dplyr::select(-dplyr::any_of(getOption("emdash.cols_to_remove_from_map_popup")))
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
