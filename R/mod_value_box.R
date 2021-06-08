#' value_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_value_box_ui <- function(id, width = 3) {
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(valueBoxOutput(ns("value_box"), width = width))
  )
}

#' value_box Server Function
#'
#' @noRd
mod_value_box_server <- function(input, output, session, value, subtitle = "", color = "green", icon = NULL) {
  ns <- session$ns
  output$value_box <- renderValueBox({
    valueBox(value,
      subtitle = subtitle,
      color = color,
      icon = icon
    )
  })
}

## To be copied in the UI
# mod_value_box_ui("value_box_ui_1")

## To be copied in the server
# callModule(mod_value_box_server, "value_box_ui_1")
