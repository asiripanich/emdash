#' DTedit UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import DTedit
mod_DTedit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(dteditmodUI(ns("DTedit_table")))
  )
}

#' DTedit Server Function
#'
#' @param input,output,session Internal parameters for {shiny}
#' @param table_data data to place in the editable data table
#' @param table_type Name of the mongo collection the data table is associated with
#' @param suppl_table_sublist list containing information related to the table
#' @param DT_options options used by DT::renderDataTable within dtedit
#' @param cons the connection to mongodb
mod_DTedit_server <- function(input, output, session, table_data, table_type,
                              suppl_table_sublist, DT_options, cons) {
  ns <- session$ns
  req(table_data)

  # If we are rendering fmt_time, adjust the target indices because DTedit
  # does not display a row number column
  # Target column indices start from 0
  if ("columnDefs" %in% names(DT_options)) {
    DT_options$columnDefs[[1]]$targets <- DT_options$columnDefs[[1]]$targets - 1
  }
  
  # If TABLE TYPE is 'participants'
  if (table_type == 'participants') {
  
    allow_update <- TRUE
    allow_insert <- FALSE
    allow_delete <- FALSE
    
    if (!('recommendation' %in% names(table_data))) {
      table_data[['recommendation']] <- rep('unknown',nrow(table_data))
    }
    
    # if you get 'Not all edit.cols are in the data.', check column spellings
    edit_columns <- c('recommendation')
    input_types = c(recommendation = "selectizeInput")
    input_choices = list(
      recommendation = 
        c('check always permission or app force kill',
          'check always permission',
          'check token',
          'check aggressive power savings',
          'unknown')
    )
    
    
    selectize_options <- list(create = TRUE, maxItems = 1)
    
    # Update a row with the function 'db_update_participants'
    # @param data the data including your updated row
    update_callback <- function(data, olddata, row) {
      browser()
      db_update_participants(cons, collection_name = "Stage_Profiles",data[row,])
      data
    }
    
    # Ignore the delete and insert callbacks
    insert_callback <- NULL
    delete_callback <- NULL
  } 
  else {
    # if there is a supplementary table sublist, do this
    #### NOTE: db insert, update, and delete are defined specifically for Checkinout
    
    db_operations <- suppl_table_sublist[[table_type]]$editable$operations
    allow_delete <- "D" %in% db_operations
    allow_update <- "U" %in% db_operations
    allow_insert <- FALSE #' C' %in% db_operations
    
    # Make status a factor so you can use selectInput
    table_data$status <- as.factor(table_data$status)
    
    input_types = c(status = 'selectInput')
    input_choices = list(
      status = 
        c(TRUE,FALSE)
    )
    
    selectize_options <- NULL
    
    if ("edit_columns" %in% names(suppl_table_sublist[[table_type]]$editable)) {
      edit_columns <- suppl_table_sublist[[table_type]]$editable$edit_columns
    } else {
      edit_columns <- "status"
    }
    
    # Define the callback functions used by dtedit
    # Insert a row. "Create"
    # @param data the data including your inserted row
    # @param row the row where you made the change
    insert_callback <- function(data, row) {
      db_insert(cons, table_type, data[row, ])
      return(data)
    }
    
    # Update a row
    # @param data the data including your updated row
    update_callback <- function(data, olddata, row) {
      db_update(cons, table_type, data[row, ])
      data
    }
    
    # Delete a row
    delete_callback <- function(data, row) {
      db_delete(cons, table_type, data[row, ])
      return(data[-row, ])
    }
  }
  
  column_names <-  names(table_data)

  return_values <- callModule(
    DTedit::dteditmod,
    id = "DTedit_table",
    thedata = table_data,
    
    edit.cols = edit_columns, 
    input.types = input_types, 
    input.choices = input_choices,
    selectize = TRUE,
    selectize.options = selectize_options,
    view.cols = column_names[!column_names %in% '_id'], # view the table without the '_id' column
    
    callback.update = update_callback, # db operations defined in utils_update_insert_delete.R
    callback.insert = insert_callback,
    callback.delete = delete_callback,
    show.insert = allow_insert,
    show.update = allow_update,
    show.delete = allow_delete,
    show.copy = FALSE,
    label.delete = "Delete Row",
    
    datatable.options = DT_options,
    filter = list(position = "top", clear = FALSE),
    extensions = "Buttons"
  )
}




## To be copied in the UI
# mod_DT_ui("DT_ui_1")

## To be copied in the server
# callModule(mod_DT_server, "DT_ui_1")
