#' ggplotly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ggplotly_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot")))
  )
}

#' ggplotly Server Function
#' @param a_ggplot a ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_smooth geom_point theme_light
#'
#' @noRd
mod_ggplotly_server <- function(input, output, session, a_ggplot, plotly.hovermode = NULL) {
  ns <- session$ns
  output$plot <- plotly::renderPlotly({
    if (!is.null(plotly.hovermode)) {
      return(
        plotly::ggplotly(a_ggplot) %>%
          plotly::layout(hovermode = plotly.hovermode)
      )
    }
    plotly::ggplotly(a_ggplot)
  })
}

## To be copied in the UI
# mod_ggplotly_ui("ggplotly_ui_1")

## To be copied in the server
# callModule(mod_ggplotly_server, "ggplotly_ui_1")
