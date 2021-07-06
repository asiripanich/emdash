#' DTedit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_DTedit_ui <- function(id) {
  ns <- NS(id)
  # tagList(
  #   shinycssloaders::withSpinner(uiOutput(ns("DTedit_table")))
  # )
  dteditmodUI(ns('DTedit_table'))
}

#' @title DTedit Server Function
#' @description 
#' 
#' DTedit server module.
#'  
#' @param data a data.frame
#' @param tab_name table name
mod_DTedit_server <- function(input, output, session, table_data, suppl_table_sublist, cons) {
  ns <- session$ns
  req(table_data)

  db_operations <- suppl_table_sublist$Checkinout$editable$operations
  allow_delete <- 'D' %in% db_operations
  allow_update <- 'U' %in% db_operations
  allow_insert <- FALSE  #'C' %in% db_operations
  
  if ('edit_columns' %in% names(suppl_table_sublist$Checkinout$editable)){
    edit_columns <- suppl_table_sublist$Checkinout$editable$edit_columns
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
    browser()
    db_delete(cons,"Checkinout", data[row,])
    return(data[-row, ])
  }
  
  return_values <- callModule(
                 DTedit::dteditmod,
                 id = 'DTedit_table',
                 thedata = table_data,
                 edit.cols = edit_columns,
                 # edit.label.cols = c('status'),
                 # input.types = c(status = "selectInput"),
                 # input.choices = list(status = c('TRUE','FALSE')),
                 view.cols = colnames(table_data),
                 callback.update = update_callback,   # db operations defined in utils_update_insert_delete.R
                 callback.insert = insert_callback,
                 callback.delete = delete_callback,
                 show.insert = allow_insert,
                 show.update = allow_update,
                 show.delete = allow_delete,
                 show.copy = FALSE,
                 label.delete = 'Delete Row',
                 datatable.options = list(
                   scrollX = TRUE,
                   pageLength = 50,
                   dom = "Bfrtip",
                   buttons = c("copy", "csv", "excel", "pdf", "print", "colvis")
                 )
            )
}




## To be copied in the UI
# mod_DT_ui("DT_ui_1")

## To be copied in the server
# callModule(mod_DT_server, "DT_ui_1")









