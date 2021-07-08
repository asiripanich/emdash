#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
app_server <- function(input, output, session) {
  # List the first level callModules here


  # prepare data ------------------------------------------------------------
  cons <- connect_stage_collections(url = getOption("emdash.mongo_url"))
  data_r <- callModule(mod_load_data_server, "load_data_ui", cons)

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
  named_label_vector <- unlist(getOption("emdash.col_labels_for_participts"))
  originalColumnNames <- names(named_label_vector)
  new_column_names <- unname(named_label_vector)

  observeEvent(input$tabs, {
    if (input$tabs == "participants") {
      data_esquisse$data <-
        data_r$participants %>%
        drop_list_columns() %>%
        data.table::setnames(originalColumnNames, new_column_names, skip_absent = TRUE)
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

  observeEvent(data_r$click, {
    callModule(mod_DT_server, "DT_ui_participants",
               data = data_r$participants %>%
                 dplyr::select(-dplyr::any_of(getOption("emdash.cols_to_remove_from_participts_table"))) %>%
                 data.table::setnames(originalColumnNames, new_column_names, skip_absent = TRUE)
    )
    callModule(mod_DT_server, "DT_ui_trips",
               data = data_r$trips %>%
                 dplyr::select(-dplyr::any_of(getOption("emdash.cols_to_remove_trips_table"))) %>%
                 sf::st_drop_geometry()
    )
    
    tableList <- getOption('emdash.supplementary_tables')
    
    # For each supplementary table, append a new tabPanel and run the server function
    # that specifies table behavior
    for (t in tableList){
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
      if ('ts' %in% colnames(suppl_table)){
        suppl_table[['fmt_time']] <- suppl_table[['ts']]
        fmt_time_index <- which(names(suppl_table) == 'fmt_time')
        
        # Add columnDefs to datatable options to convert fmt_time to a Date
        datatable_options[['columnDefs']] <- list(list(
          # target column indices start from 0. 
          # However, DT adds a row number column to the display as the 0th column
          targets = fmt_time_index,    
          render = JS(
            "function(data, type, row) {",
            "return new Date(data*1000);",
            "}")
        ))
      }
      
      # If the table is Bike Check In and there is an 'editable' field, use dtedit
      if (table_type == 'Checkinout' & 'editable' %in% names(t$Checkinout)) {

        # If we are rendering fmt_time, adjust the target indices because DTedit 
        # does not display a row number column
        # Target column indices start from 0
        if ('columnDefs' %in% names(datatable_options)){
          datatable_options$columnDefs[[1]]$targets <- datatable_options$columnDefs[[1]]$targets - 1
        }
        
        # Store the correct datatable options to use.
        # Without this, dtedit looks at the most recent datatable options meant for the final supplementary table
        DTedit_datatable_options <- datatable_options
        
        db_operations <- t$Checkinout$editable$operations
        allow_delete <- 'D' %in% db_operations
        allow_update <- 'U' %in% db_operations
        allow_insert <- FALSE  #'C' %in% db_operations
        
        # Make status a factor so you can use selectInput
        suppl_table$status <- as.factor(suppl_table$status)
        
        if ('edit_columns' %in% names(t$Checkinout$editable)){
          edit_columns <- t$Checkinout$editable$edit_columns
        } else {
          edit_columns <- 'status'
        }
        
        # Define the callback functions used by dtedit
        #' Insert a row. "Create"
        #' @param data the data including your inserted row
        #' @param row the row where you made the change
        insert_callback <- function(data, row) {
          db_insert(cons,"Checkinout",data[row,])
          return(data)
        }
        
        #' Update a row
        #' @param data the data including your updated row
        update_callback <- function(data, olddata, row) {
          db_update(cons,"Checkinout",data[row,])
          data
        }
        
        #' Delete a row
        delete_callback <- function(data, row) {
          db_delete(cons,"Checkinout", data[row,])   # table_type updates to PolarBear before any CUD functions are called
          return(data[-row, ])
        }

        editable_table_tab <-  tabPanel(
          status = "primary",
          title = table_title,
          value = table_type,
          uiOutput(outputId = paste0("DTedit_ui_",table_type))  # Maybe Later: mod_DTedit_ui(id = paste0("DTedit_ui_",table_type))
        )
        
        appendTab(
          inputId = 'tabs',
          tab = editable_table_tab,
          select = FALSE,
          menuName = NULL,
          session = getDefaultReactiveDomain()
        )

        DTedit::dtedit(input, output,
                       name = paste0("DTedit_ui_",table_type),
                       thedata = suppl_table,
                       edit.cols = edit_columns,
                       # edit.label.cols = c('status'),
                       # input.types = c(status = "selectInput"),
                       # input.choices = list(status = c('TRUE','FALSE')),
                       view.cols = names(suppl_table),
                       callback.update = update_callback,   # db operations defined in utils_update_insert_delete.R
                       callback.insert = insert_callback,
                       callback.delete = delete_callback,
                       show.insert = allow_insert,
                       show.update = allow_update,
                       show.delete = allow_delete,
                       show.copy = FALSE,
                       label.delete = 'Delete Row',
                       datatable.options = DTedit_datatable_options
                     ) 
      }  
      # For other supplementary tables, use mod_DT  
       else {
        regular_tab <-  tabPanel(
          status = "primary",
          title = table_title,
          value = table_type,
          mod_DT_ui(id = paste0("DT_ui_",table_type)) 
        )
        appendTab(
          inputId = 'tabs',
          tab = regular_tab,
          select = FALSE,
          menuName = NULL,
          session = getDefaultReactiveDomain()
        ) 

        # Run mod_DT_server using data for the current table
        callModule(module = mod_DT_server,
                   id = paste0("DT_ui_",table_type),
                   data = suppl_table,
                   DT_options = datatable_options)
       }
      }
    
  })

  # Maps --------------------------------------------------------------------

  # these lists of columns in trips_with_trajectories can inform
  # 1) which columns to remove in the map filter
  # 2) which columns to remove to pass to the map and show up in the map popups

  # data_r$trips_with_trajectories %>% colnames() %>% dput()

  cols_to_include_in_map_filter <- reactive({
    data_r$trips %>%
      colnames() %>%
      # specify columns to remove here
      setdiff(c(
        "start_fmt_time0", "start_local_dt_timezone", "start_local_time",
        "end_fmt_time0", "end_local_dt_timezone", "end_local_time",
        "end_loc_coordinates", "start_loc_coordinates", "duration", "distance",
        "location_points", "source"
      ))
  })

  filtered_trips <-
    callModule(
      module = esquisse::filterDF,
      id = "filtering",
      data_table = reactive(anonymize_uuid_if_required(data_r$trips)),
      data_name = reactive("data"),
      data_vars = cols_to_include_in_map_filter, # the map filter uses start_fmt_time and end_fmt_time (UTC time)
      drop_ids = FALSE
    )

  observeEvent(filtered_trips$data_filtered(), {
    callModule(
      mod_mapview_server,
      "mapview_trips",
      data_sf = filtered_trips$data_filtered() %>%
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
