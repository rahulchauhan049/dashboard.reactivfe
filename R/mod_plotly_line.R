#' plotly_line UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plotly_line_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("back")),
    mod_field_selection_ui(ns("field_selection_ui_1")),
    plotlyOutput(ns("plot")) 
  )
  
}
    
#' plotly_line Server Function
#'
#' @noRd 
mod_plotly_line_server <- function(input, output, session, data_reactive, data_original, column_name, column_name_y){
  ns <- session$ns
  
  field <- callModule(mod_field_selection_server, "field_selection_ui_1", "bubble", data_reactive, data_original)
  
  
  output$plot <- renderPlotly({
    if(!is.null(field$xval())){
      a <- list()
      data <- na.omit(data_reactive$data[c(field$xval(), field$yval())])
      for(i in unique(data[[field$xval()]])){
        dat <- filter(data, data[[field$xval()]]==i)
        dat <- as.data.frame(table(dat[[field$yval()]]))
        a[[i]] <- dat
      }
      
      pl <- plot_ly(type="scatter", source = ns("tab1"))
      
      for(i in names(a)){
        pl <- add_trace(pl, x=a[[i]]$Var1,y=a[[i]]$Freq, mode = "lines+markers", name = i, key=i)
      }
      
      pl %>%
        layout(paper_bgcolor = 'transparent',
               plot_bgcolor = "transparent",
               xaxis = list(
                 title = field$xval(),
                 showspikes = TRUE,
                 spikemode  = 'across',
                 spikesnap = 'cursor',
                 spikedash = "solid",
                 spikecolor = '#ffffff',
                 spikethickness = 1,
                 showline=TRUE,
                 color = "#ffffff",
                 zeroline = TRUE,
                 showline = TRUE,
                 showticklabels = TRUE,
                 showgrid = FALSE
               ),
               yaxis = list(
                 zeroline = FALSE,
                 showline = TRUE,
                 title = 'New Confirmed Cases',
                 color = '#ffffff',
                 showticklabels = TRUE,
                 showgrid = TRUE,
                 gridcolor = toRGB("gray50")
               ),
               legend = list(
                 x = 0,
                 y = 1,
                 orientation = 'h',
                 font = list(
                   color = "#ffffff"
                 )
               ),
               showlegend = TRUE,
               # hovermode  = 'x',
               spikedistance = 300,
               hoverdistance = 10
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
    
    if(!is.null(event)){
      data_reactive$events[[ns("tab1")]] <- list(event$key, field$xval())
      temp_data <- data_original

      for(val in data_reactive$events){
        temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
      }
      data_reactive$data <- temp_data
    }
  })
}
    
## To be copied in the UI
# mod_plotly_line_ui("plotly_line_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_line_server, "plotly_line_ui_1")
 
