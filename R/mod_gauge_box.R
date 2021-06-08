#' gauge UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gauge_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(flexdashboard::gaugeOutput(ns("gauge")), title = "gauge", width = 3)
  )
}

#' gauge Server Function
#'
#' @noRd
mod_gauge_server <- function(input, output, session, value, max_value = 100, title = "") {
  ns <- session$ns
  # output$my_title <- title
  output$gauge <- flexdashboard::renderGauge({
    flexdashboard::gauge(
      value,
      min = 0,
      max = max_value,
      symbol = "%",
      label = paste("Test Label"),
      flexdashboard::gaugeSectors(
        success = c(100, 6),
        warning = c(5, 1),
        danger = c(0, 1),
        colors = c("#CC6699")
      )
    )
  })
}

## To be copied in the UI
# mod_gauge_ui("gauge_ui_1")

## To be copied in the server
# callModule(mod_gauge_server, "gauge_ui_1")
