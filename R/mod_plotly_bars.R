#' plotly_bars UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plotly_bars_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("back")),
    mod_field_selection_ui(ns("field_selection_ui_1")),
    plotlyOutput(ns("plot")) 
  )
}
    
#' plotly_bars Server Function
#'
#' @noRd 
mod_plotly_bars_server <- function(input, output, session, data_reactive, data_original, column_name){
  ns <- session$ns
  
  field <- callModule(mod_field_selection_server, "field_selection_ui_1", "bar", data_reactive, data_original)

  output$plot <- renderPlotly({
    if(!is.null(field$xval())){
      dat <- data_reactive$data
      dat <- as.data.frame(table("a"=dat[[field$xval()]]))
      plot_ly(dat, x = ~a, y = ~Freq, color = ~a, key = ~a, type = "bar", source = ns("tab1")) %>%
        layout(
          paper_bgcolor = 'transparent',
          plot_bgcolor = "transparent",
          showlegend = FALSE,
          xaxis = list(
            title = field$xval(),
            color = '#ffffff',
            zeroline = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            showgrid = FALSE
          ),
          yaxis = list(
            color = '#ffffff',
            showticklabels = TRUE,
            showgrid = FALSE
          )
        )
    }
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if(!is.null(data_reactive$events[[ns("tab1")]])){
      actionButton(
        ns("clear"),
        "Back/Reset",
        icon("chevron-left")
      )
    }
  })
  
  observeEvent(input$clear, {
    data_reactive$events[[ns("tab1")]] <- NULL
    temp_data <- data_original
    for(val in data_reactive$events){
      temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
    }
    data_reactive$data <- temp_data
  })
  
  
  observeEvent(event_data("plotly_click", source = ns("tab1")), ignoreNULL = FALSE, {
    event <- event_data("plotly_click", source = ns("tab1"))
    
    if(is.null(event)){
      data_reactive$events[[ns("tab1")]] <- NULL
      
      temp_data <- data_original
      for(val in data_reactive$events){
        temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
      }
      data_reactive$data <- temp_data
    }else{
      
      data_reactive$events[[ns("tab1")]] <- list(event$x, field$xval())
      temp_data <- data_original
      
      for(val in data_reactive$events){
        temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
      }
      data_reactive$data <- temp_data
    }
  })
  
  
  observeEvent(event_data("plotly_brushed", source = ns("tab1")), {
    brush <- event_data("plotly_brushed", source = ns("tab1"))
    print(brush$x)
  })


 
}
    
## To be copied in the UI
# mod_plotly_bars_ui("plotly_bars_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_bars_server, "plotly_bars_ui_1")
 
