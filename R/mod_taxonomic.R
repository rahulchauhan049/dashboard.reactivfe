#' taxonomic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_taxonomic_ui <- function(id){
  ns <- NS(id)

  
  fluidPage(
    fluidRow(
      mod_plotly_bubble_ui(ns("plotly_bubble_ui_1"))
    ),
    fluidRow(
      mod_plotly_bubble_ui(ns("plotly_line_ui_1"))
    ),
    fluidRow(
      mod_plotly_line_ui(ns("plotly_line_ui_2"))
    ),
    fluidRow(
      column(
        6,
        mod_plotly_bars_ui(ns("plotly_bars_ui_1"))
      ),
      column(
        6,
        mod_plotly_pie_ui(ns("plotly_pie_ui_1"))
      )
    ),
    fluidRow(
      mod_leaflet_ui(ns("leaflet_ui_1"))
    ),
    fluidRow(
      mod_DT_ui(ns("DT_ui_1"))
    )
  )
}
    
#' taxonomic Server Function
#'
#' @noRd 
mod_taxonomic_server <- function(input, output, session, data){
  ns <- session$ns
  
  # data_reactive <- reactiveValues(data = data.frame(), events = list())
  # 
  # observe({
  #   dat <- data()
  #   data_reactive$data = dat
  # })
  # 

  hyenaData <- read.csv("data/hyenaData.csv")

  data_reactive <- reactiveValues(data = hyenaData, events = list())

  


  # callModule(mod_plotly_bubble_server, "plotly_bubble_ui_1", data_reactive,  hyenaData, "species", "year")
  # callModule(mod_plotly_line_server, "plotly_line_ui_1", data_reactive,  hyenaData, "species", "year", "cumulative")
  # callModule(mod_plotly_line_server, "plotly_line_ui_2", data_reactive,  hyenaData, "species", "year", "daily")
  # callModule(mod_plotly_bars_server, "plotly_bars_ui_1", data_reactive,  hyenaData, "family", orientation ="h")
  # callModule(mod_plotly_pie_server, "plotly_pie_ui_1", data_reactive,  hyenaData, "genus")
  callModule(mod_leaflet_server, "leaflet_ui_1", data_reactive,  hyenaData)
  # callModule(mod_DT_server, "DT_ui_1", data_reactive, c(
  #   "countryCode",
  #   "locality",
  #   "decimalLatitude"
  # ))

}
    
## To be copied in the UI
# mod_taxonomic_ui("taxonomic_ui_1")
    
## To be copied in the server
# callModule(mod_taxonomic_server, "taxonomic_ui_1")
 
