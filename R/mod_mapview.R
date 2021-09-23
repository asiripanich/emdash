#' mapview UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#' @param height [leaflet::leafletOutput]'s `height` argument.
#'
#' @importFrom shiny NS tagList
#' @export
mod_mapview_ui <- function(id, height) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"), height = height)
  )
}

#' mapview Server Function
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param data_sf a spatial data object from the 'sf' package.
#' @param mapview.map.types see `map.types` in the [mapview::mapview] function.
mod_mapview_server <- function(input, output, session, data_sf, mapview.map.types = "CartoDB.DarkMatter") {
  ns <- session$ns

  req(data_sf)

  output$map <- leaflet::renderLeaflet({
    if (nrow(data_sf) == 0) {
      m <- mapview::mapview(map.types = mapview.map.types)
    } else {
      m <-
        mapview::mapview(
          drop_list_columns(data_sf),
          zcol = getOption("emdash.map_trajectory_colors_variable"),
          map.types = mapview.map.types
        )
    }
    m@map
  })
}

## To be copied in the UI
# mod_mapview_ui("mapview_ui_1")

## To be copied in the server
# callModule(mod_mapview_server, "mapview_ui_1")
