#' field_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_field_selection_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    uiOutput(ns("choose_dataset")),
    verbatimTextOutput(ns("r"))
  )
}
    
#' field_selection Server Function
#'
#' @noRd 
mod_field_selection_server <- function(input, output, session, graph_name, data_reactive, data_original){
  ns <- session$ns
  
  fields <- list()
  
  
  if(graph_name == "bubble" || graph_name == "line"){
    output$choose_dataset <- renderUI({
      temp <- find_fields("bubble", data_reactive$data)
      lapply(names(temp), function(i) {
        fields[[i]] <- (paste0("field_", i))
        column(
          width=6,
          selectInput(
            inputId = ns(paste0("field_", i)),
            label = paste0("Column: ", i),
            choices = temp[[i]]
          )
        )
      })
    })
    
    return(
      list(
        xval = reactive(input[["field_x"]]),
        yval = reactive(input[["field_y"]])
      )
    )
    
  }else if(graph_name == "pie" || graph_name == "bar"){
    
    output$choose_dataset <- renderUI({
      temp <- find_fields("pie", data_reactive$data)
      lapply(names(temp), function(i) {
        fields[[i]] <- (paste0("field_", i))
        column(
          width=12,
          selectInput(
            inputId = ns(paste0("field_", i)),
            label = paste0("Column: ", i),
            choices = temp[[i]]
          )
        )
      })
    })
    
    return(
      list(
        xval = reactive(input[["field_x"]])
      )
    )
    
  }
}

find_fields <- function(plot, data){
  columns <- list()
  if(plot == "bubble" || plot == "line"){
    columns$x <- c("genus", "scientificName", "kingdom", "phylum", "order", "family", "species")
    # columns$x <- names(dplyr::select_if(data,is.character))
    columns$y <- c(
      "year",
      "day",
      "month"
    )
  } else if(plot == "pie" || plot == "bar"){
    columns$x <-  c("genus", "scientificName", "kingdom", "phylum", "order", "family", "species")
  }
  return(columns)
}

    
## To be copied in the UI
# mod_field_selection_ui("field_selection_ui_1")
    
## To be copied in the server
# callModule(mod_field_selection_server, "field_selection_ui_1")
 
