#' DT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DT_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(DT::dataTableOutput(ns("DTtable")))
  )
}

#' DT Server Function
#'
#' @param data a data.frame
#' @param DT_options options used by DT::renderDataTable
#'
#' @noRd
mod_DT_server <- function(input, output, session, data, DT_options) {
  ns <- session$ns
  req(data)

  if (missing(DT_options)) {
    DT_options <- default_DT_options
  } else {
    DT_options <- combine_named_lists(default_DT_options, DT_options)
  }

  output$DTtable <- DT::renderDataTable({
    DT::datatable(
      data,
      autoHideNavigation = TRUE,
      options = DT_options,
      filter = list(position = "top", clear = FALSE),
      extensions = "Buttons"
    )
  })
}

default_DT_options <- list(
  scrollX = TRUE,
  pageLength = 50,
  dom = "Bfrtip",
  bPaginate = FALSE,
  buttons = c("copy", "csv", "excel", "pdf", "print", "colvis")
)

## To be copied in the UI
# mod_DT_ui("DT_ui_1")

## To be copied in the server
# callModule(mod_DT_server, "DT_ui_1")
