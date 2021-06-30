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
#' @param tab_name table name
#'
#' @noRd
mod_DT_server <- function(input, output, session, data) {
  ns <- session$ns

  req(data)

  # button_list <- list(list(extend='colvis', columns=c()))
  output$DTtable <- DT::renderDataTable({
    DT::datatable(
      data,
      autoHideNavigation = TRUE,
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        dom = "Bfrtip",
        # columnDefs = list(list(visible=FALSE, targets=c(5:16)))
        buttons = c("copy", "csv", "excel", "pdf", "print", "colvis") # , button_list)
      ),
      filter = list(position = "top", clear = FALSE),
      extensions = "Buttons"
    )
  })
}

## To be copied in the UI
# mod_DT_ui("DT_ui_1")

## To be copied in the server
# callModule(mod_DT_server, "DT_ui_1")
