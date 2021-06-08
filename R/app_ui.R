#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      skin = "green",
      dashboardHeader(title = paste0("emdash v", packageVersion("emdash"))),

      # Sidebar -----------------------------------------------------------------
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Tables", tabName = "tables", icon = icon("table")),
          menuItem("Maps", tabName = "maps", icon = icon("globe")),
          mod_load_data_ui("load_data_ui")
        )
      ),
      dashboardBody(
        tabItems(
          # Dashboard -----------------------

          tabItem(
            tabName = "dashboard",

            # Dashboard - boxes - start -----------------
            fluidRow(
              mod_value_box_ui("value_box_ui_unique_users"),
              mod_value_box_ui("value_box_ui_active_users_today"),
              mod_value_box_ui("value_box_ui_total_trips"),
              mod_value_box_ui("value_box_ui_total_days")
            ),
            # Dashboard - boxes - end -----------------
            # Dashboard - calendar - start -----------------

            # Dashboard - calendar - end -----------------
            # Dashboard - plots - start ----------------------------------

            # Display the signup trend if that option is chosen
            if (isTRUE(getOption("emdash.disp_signup_trend"))) {
              fluidRow(
                box(
                  title = "Sign-up Trend",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  mod_ggplotly_ui("ggplotly_ui_signup_trend")
                )
              )
            },
            fluidRow(
              box(
                title = "Trip Trend",
                solidHeader = TRUE,
                collapsible = TRUE,
                mod_ggplotly_ui("ggplotly_ui_trip_trend")
              ),
              box(
                title = "Participation Period",
                solidHeader = TRUE,
                collapsible = TRUE,
                mod_ggplotly_ui("ggplotly_ui_participation_period")
              ),
              #                     box(
              #                       title = "Branch",
              #                       solidHeader = TRUE,
              #                       collapsible = TRUE,
              #                       mod_ggplotly_ui("ggplotly_ui_branch")
              #                     ),
              box(
                title = "Platform",
                solidHeader = TRUE,
                collapsible = TRUE,
                mod_ggplotly_ui("ggplotly_ui_platform")
              )
            )
            # Dashboard - plots - end ----------------------------------
          ),

          # Tables ------------------------------------------------------------------
          tabItem(
            tabName = "tables",
            box(
              title = "Interactive plot - this panel is linked with the data tab below",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              esquisserUI(
                id = "esquisse",
                header = FALSE, # dont display gadget title
                choose_data = FALSE # dont display button to change data
              ),

              # Override the default font size in esquisse with CSS
              tags$style(".container-drag-source, .box-dad {font-size: 14px;}")
            ),
            fluidRow(
              tabBox(
                id = "tabs",
                width = 12,
                tabPanel(
                  status = "primary",
                  title = "Participants",
                  value = "participants",
                  mod_DT_ui("DT_ui_participants")
                ),
                tabPanel(
                  status = "primary",
                  title = "Trips",
                  value = "trips",
                  mod_DT_ui("DT_ui_trips")
                )
              )
            )
          ),

          # Maps ----------------------
          tabItem(
            tabName = "maps",
            fluidRow(
              column(
                width = 3,
                wellPanel(
                  id = "tPanel",
                  style = "overflow-y:scroll; max-height: 800px",
                  esquisse::filterDF_UI("filtering")
                )
              ),
              column(
                width = 9,
                mod_mapview_ui("mapview_trips", height = 800)
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "emdash"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
