library(shiny)
library(DTedit)
library(devtools)


query_supplementary <- function(cons, name) {
  cons[[name]]$find('{}') %>%
    data.table::as.data.table()
}

#'   https://docs.mongodb.com/manual/crud/
#'   
#'   https://docs.mongodb.com/manual/tutorial/query-arrays/
#'   https://jeroen.github.io/mongolite/query-data.html


# Normalise UUID
normalise_uuid <- function(.data, keep_uuid = FALSE) {
  if (nrow(.data) == 0) {
    return(.data)
  }
  # return(.data)
  if (!is.data.table(.data)) {
    setDT(.data)
  }
  if ("uuid" %in% names(.data)) {
    .data[, user_id := sapply(uuid, function(.x) paste0(unlist(.x), collapse = ""))]
    if (!keep_uuid) {
      .data[, uuid := NULL]
    }
  } else {
    .data[, user_id := sapply(user_id, function(.x) paste0(unlist(.x), collapse = ""))]
  }
  .data
}

# Get the data
get_Data <-function() {
  # cons <-   mongolite::mongo(
  #   db = "Stage_database",
  #   collection = "Checkinout",
  #   url = 'mongodb://localhost:27017'
  # )
  # data <- query_supplementary(cons,'Checkinout')
  # return(data)
  
  cons <- connect_stage_collections(url = 'mongodb://localhost:27017')
  name <- "Checkinout"
  data <- cons[[name]]$find('{}')
  
  data$status <- as.list(data$status)
  return(data)#query_checkinout(cons))
}


ui <- function(id) {
  fluidPage(uiOutput("DTedit_table"))
  
  # <div id="DT_ui_Checkinout-DTedit_table" class="shiny-html-output"></div>
}

server <- function(input, output, session) {
  
  
  # Some notes: logicals need to be factors if you want to use select input.
  # However, the only choices with factors and select input are choices that are already in the data
  # The default editing option for lists is a dropdown with the input.choices you set

  
  bike_data <- get_Data()
  # browser()
  # Insert a row. "Create"
  insert_callback <- function(data, row) {
    mydata <- rbind(data, mydata)
    return(mydata)
  }

  # Update a row
  update_callback <- function(data, olddata, row) {
    mydata[row,] <- data[1,]
    return(mydata)
  }

  # Delete a row
  delete_callback <- function(data, row) {
    mydata[row,] <- NULL
    return(mydata)
  }
  
  
  # message('DTedit is running')
  # message(colnames(data))
  # message(print(data))
  message(class(bike_data))
  output$DTedit_table <- renderUI({DTedit::dtedit(input, output,
                 name = 'DTedit_table',
                 thedata = bike_data,
                 edit.cols = c('user_id', 'status'),#, 'bikeLabel'),
                 edit.label.cols = c('user id', 'status'),#, 'bike label'),
                 input.types = c(user_id = 'textInput'),
                                 # status = 'selectInput'),#,  
                 #                 #bikeLabel = 'textInput'),
                 input.choices = list(status = c('TRUE','FALSE')),                   # list(user_id = c("Chambers, J.M.", "Becker, R.A.", "Wilks, A.R.", "Hastie, T.J."))
                 view.cols = c('user_id', 'status', 'bikeLabel'),
                 callback.update = update_callback,
                 callback.insert = insert_callback,
                 callback.delete = delete_callback)
  })
  
}

shinyApp(ui, server)

