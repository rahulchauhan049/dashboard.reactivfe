#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard dashboardthemes shinyjs bdutilities.app
#' @import plotly DT leaflet leaflet.extras sp shinyWidgets dplyr
#' @import promises future
#' @noRds

future::plan(future::multiprocess)

app_ui <- function(request) {
  dashboardPage(
    skin = "yellow",
    dashboardHeader(title = "bddashboard Demo"),
    dashboardSidebar(
      sidebarMenu(
        id = "sideBar",
        menuItem(
          "Data Input",
          tabName = "dataInputTab",
          icon = icon("database")
        ),
        menuItem(
          "Taxonomic",
          tabName = "taxonomic_tab",
          icon = icon("eye")
        ),
        bookmarkButton()
      )
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "grey_dark"
      ),
      golem_add_external_resources(),
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "dataInputTab",
          bdutilities.app::mod_add_data_ui("bdFileInput"),
          bdutilities.app::mod_darwinize_ui("darwinize")
        ),
        tabItem(
          tabName = "taxonomic_tab",
          mod_taxonomic_ui("taxonomic_ui_1")
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'dashboard.reactivfe'
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

