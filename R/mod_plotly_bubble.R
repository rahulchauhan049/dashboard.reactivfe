#' plotly_bubble UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plotly_bubble_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("back")),
    mod_field_selection_ui(ns("field_selection_ui_1")),
    plotlyOutput(ns("plot"))
    
  )
}
    
#' plotly_bubble Server Function
#'
#' @noRd 
mod_plotly_bubble_server <- function(input, output, session, data_reactive, data_original, column_name, column_name_y){
  ns <- session$ns
  
  field <- callModule(mod_field_selection_server, "field_selection_ui_1", "bubble", data_reactive, data_original)
  
  output$plot <- renderPlotly({
    if(!is.null(field$xval())){
      d <- data_reactive$data[c(field$xval(), field$yval())]
      colnames(d) <- c("x", "y")
      d <- as.data.frame(table(d))
      d <- d %>% filter(Freq > 0) %>% droplevels()
      
      plot_ly(d, x = ~x, y = ~y, type = 'scatter', mode = 'markers', size = ~Freq, color = ~x, colors = 'Paired',
              sizes = c(10, 50),
              marker = list(opacity = 0.5, sizemode = 'diameter'),
              hoverinfo = 'text',
              text = ~paste(field$xval(),": ", x, '<br>',field$yval(),': ', y,
                            '<br> Freq: ', Freq),
              source = ns("tab1")) %>% 
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
                 color = "#ffffff",
                 zeroline = FALSE,
                 showline = TRUE,
                 showticklabels = TRUE,
                 showgrid = FALSE
               ),
               yaxis = list(
                 zeroline = FALSE,
                 showline = TRUE,
                 title =  field$yval(),
                 color = '#ffffff',
                 showticklabels = TRUE,
                 showgrid = TRUE,
                 gridcolor = toRGB("gray30")
               ),
               showlegend = FALSE
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
    print(event)
    
    if(!is.null(event)){
      data_reactive$events[[ns("tab1")]] <- list(event$x, field$xval())
      temp_data <- data_original

      for(val in data_reactive$events){
        temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
      }
      data_reactive$data <- temp_data
    }
  })
}
    
## To be copied in the UI
# mod_plotly_bubble_ui("plotly_bubble_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_bubble_server, "plotly_bubble_ui_1")
 
