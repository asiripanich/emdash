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
#' @param data a data.frame
#' @param table_data data to place in the editable data table
#' @param Table_Type Name of the mongo collection the data table is associated with
#' @param suppl_table_sublist list containing information related to the table
#' @param DT_options options used by DT::renderDataTable within dtedit
#' @param cons the connection to mongodb
mod_DTedit_server <- function(input, output, session, table_data, Table_Type,
                              suppl_table_sublist, DT_options, cons) {
  ns <- session$ns
  req(table_data)

  # If we are rendering fmt_time, adjust the target indices because DTedit
  # does not display a row number column
  # Target column indices start from 0
  if ("columnDefs" %in% names(DT_options)) {
    DT_options$columnDefs[[1]]$targets <- DT_options$columnDefs[[1]]$targets - 1
  }

  db_operations <- suppl_table_sublist[[Table_Type]]$editable$operations
  allow_delete <- "D" %in% db_operations
  allow_update <- "U" %in% db_operations
  allow_insert <- FALSE #' C' %in% db_operations

  # Make status a factor so you can use selectInput
  table_data$status <- as.factor(table_data$status)

  if ("edit_columns" %in% names(suppl_table_sublist[[Table_Type]]$editable)) {
    edit_columns <- suppl_table_sublist[[Table_Type]]$editable$edit_columns
  } else {
    edit_columns <- "status"
  }

  #### NOTE: db insert, update, and delete are defined specifically for Checkinout

  # Define the callback functions used by dtedit
  # Insert a row. "Create"
  # @param data the data including your inserted row
  # @param row the row where you made the change
  insert_callback <- function(data, row) {
    db_insert(cons, Table_Type, data[row, ])
    return(data)
  }

  # Update a row
  # @param data the data including your updated row
  update_callback <- function(data, olddata, row) {
    db_update(cons, Table_Type, data[row, ])
    data
  }

  # Delete a row
  delete_callback <- function(data, row) {
    db_delete(cons, Table_Type, data[row, ])
    return(data[-row, ])
  }

  return_values <- callModule(
    DTedit::dteditmod,
    id = "DTedit_table",
    thedata = table_data,
    edit.cols = edit_columns,
    view.cols = names(table_data),
    callback.update = update_callback, # db operations defined in utils_update_insert_delete.R
    callback.insert = insert_callback,
    callback.delete = delete_callback,
    show.insert = allow_insert,
    show.update = allow_update,
    show.delete = allow_delete,
    show.copy = FALSE,
    label.delete = "Delete Row",
    datatable.options = DT_options
  )
}




## To be copied in the UI
# mod_DT_ui("DT_ui_1")

## To be copied in the server
# callModule(mod_DT_server, "DT_ui_1")
