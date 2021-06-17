#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_trips_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    dateRangeInput("dates",
                   "Select the range of dates to look at data for.", 
                   start = "2015-07-22",
                   end = "2015-09-10", #"2016-08-11",
                   min = "2015-07-22",
                   max = "2015-09-10"),
    textOutput('Documents'),
    
    actionButton(inputId = ns('reload_trips'), label = 'Reload trips data'),
    textOutput(ns('load_box')),
    textOutput(ns("last_load_datetime"))
  )
}

#' load_data Server Function
#'
#' @noRd
mod_load_trips_server <- function(input, output, session, cons) {
  ns <- session$ns
  
  max_window <- 60  # 1 month

  # window_width <- reactive({   
  #   
  #   # Find the number of days between the two selected dates
  #   width <- abs(as.numeric(difftime(input$dates[1],input$dates[2]))) + 1  # include first date
  #   message(width)
  #   return(width)
  #   # Eventually, count the number of documents between those selected dates
  # })
  # 
  # load_allowed <- reactive({
  #   allow <- (window_width() < max_window)
  #   browser()
  #   message(allow)
  #   return(allow)
  # })
  
  # disp_n_docs <- reactive({
  #   n_docs <- 500 + window_width()
  #   message(n_docs)
  #   
  #   if (load_allowed()) {
  #     out <- glue::glue('There are {n_docs} documents matching those dates')
  #     
  #   } else {
  #     out <- "The chosen date range is too wide. Please enter a new one."
  #   }
  #   return(cat(out))  # use cat to avoid the extra printed [1]
  # })
  
  # #When referring to reactives, remember to use parentheses
  #output$Documents <- renderPrint(disp_n_docs())
  
  data_geogr <- reactiveValues(data = data.frame(), name = "data")
  
  output$last_load_datetime <-
    renderText(paste0("Last loaded: ", as.character(Sys.time())))
  
  observeEvent(input$reload_trips,{
                  dates <- reactive({
                    if (!is.null(input$dates)){
                      out_dates <- input$dates
                    } else {
                      out_dates <- c("2015-07-22","2015-09-10")
                    }
                    return(out_dates)
                    }
                  )
    message("But dates is:")
    message(dates())
                  # start = "2015-07-22",
                  # end = "2015-09-10", #"2016-08-11

                 if (TRUE) {
                   browser()
                   load_display <- 'Loading data'

                   message("About to load trips")
                   data_geogr$trips <- tidy_cleaned_trips(query_cleaned_trips_by_timestamp(cons,dates()),
                                                          project_crs = get_golem_config("project_crs")
                   )
                   message("Finished loading trips")
                   
                   # message("About to load locations")
                   # data_geogr$locations <- tidy_cleaned_locations(query_cleaned_locations_by_timestamp(cons,input$dates))
                   # message("Finished loading locations")
                   # 
                   # message("About to create trajectories within trips")
                   # data_geogr$trips_with_trajectories <- generate_trajectories(data_geogr$trips,
                   #   data_geogr$locations,
                   #   project_crs = get_golem_config("project_crs")
                   # )
                   # message("Finished creating trajectories within trips")
                   
                   # output column names into R
                   # data_geogr$trips %>% colnames() %>% dput()
                   # data_geogr$participants %>% colnames() %>% dput()
                   # data_geogr$trips_with_trajectories %>% colnames() %>% dput()
                   
                   data_geogr$click <- runif(1)
                   
                 } else {
                   load_display <- 'The chosen date range is too wide to load the data.'
                 }
                 
                 output$load_box <- renderPrint(cat(load_display))
               },
               ignoreNULL = FALSE
  )
  
  message("Running: mod_load_data_server")
  return(data_geogr)
}

## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")

## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
