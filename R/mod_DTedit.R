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
  
  uiOutput(outputId = ns("DTedit_table"))
}

#' DTedit Server Function
#'
#' @param data a data.frame
#' @param tab_name table name
#'
#' @noRd
mod_DTedit_server <- function(input, output, session, data) {
  ns <- session$ns #("DTedit_table")
  req(data)

  #' Insert a row. "Create"
  insert_callback <- function(data, row) {
    mydata <- rbind(data, mydata)
    return(mydata)
  }

  #' Update a row
  update_callback <- function(data, olddata, row) {
    mydata[row,] <- data[1,]
    return(mydata)
  }

  #' Delete a row
  delete_callback <- function(data, row) {
    mydata[row,] <- NULL

    return(mydata)
  }
  
  # user_id <- 1:3
  # other_var <- 2:4
  # new_data <- data.table::data.table(user_id,other_var)
  
  message('DTedit is running')
  message(glue::glue('Class of the data is: {class(data)}\n'))
  message(colnames(data))
  #message(print(data))
  DTedit::dtedit(input, output,
                name = 'DTedit_table',
                thedata = data,
                edit.cols = c('user_id'),#, 'status', 'bikeLabel'),
                edit.label.cols = c('user id'),#, 'status', 'bike label'),
                input.types = c(user_id = 'textInput'),# = 'textAreaInput',
                                # status = 'textAreaInput',
                                # bikeLabel = 'textAreaInput'),
                view.cols = c('user_id','status', 'bikeLabel'),
                callback.update = update_callback,
                callback.insert = insert_callback,
                callback.delete = delete_callback)
    
}




## To be copied in the UI
# mod_DT_ui("DT_ui_1")

## To be copied in the server
# callModule(mod_DT_server, "DT_ui_1")









