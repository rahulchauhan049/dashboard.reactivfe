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
    column(
      3,
      selectInput(ns("group"), "Select Group", NULL)
    ),
    column(
      9,
      uiOutput(ns("choose_dataset")),
    )
  )
}
    
#' field_selection Server Function
#'
#' @noRd 
mod_field_selection_server <- function(input, output, session, graph_name, data_reactive, data_original, default_field=NULL, default_group=NULL){
  ns <- session$ns
  
  dictionary <- read.csv("data/dictionary.csv")

  
  fields <- list()
  
  observe({
    group_list <- create_group(dictionary, data_reactive$data)
    if(is.null(default_group)){
      default_group = names(group_list)[1]
    }
    updateSelectInput(
      session,
      "group",
      choices = names(group_list),
      selected = default_group
      )
  }
  )
  
  
  if(graph_name == "bubble" || graph_name == "line"){
    output$choose_dataset <- renderUI({
      group_list <- create_group(dictionary, data_reactive$data)
      temp <- find_fields("bubble", data_reactive$data, group_list, input$group)
      
      if(is.null(default_field$x)){
        default_field$x <- temp[1]
      }
      
      if(is.null(default_field$y)){
        default_field$y <- temp[1]
      }
      
      if(!(default_field$x %in% temp$x)){
        default_field$x <- temp[1]
      }
      
      if(!(default_field$y %in% temp$y)){
        default_field$y <- temp[1]
      }
      
      
      
      lapply(names(temp), function(i) {
        fields[[i]] <- (paste0("field_", i))
        column(
          width=6,
          selectInput(
            inputId = ns(paste0("field_", i)),
            label = paste0("Column: ", i),
            choices = temp[[i]],
            selected = default_field[i]
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
      
      if(is.null(default_field$x)){
        default_field$x <- temp[1]
      }
      
      if(!(default_field$x %in% temp$x)){
        default_field$x <- temp[1]
      }
      
      lapply(names(temp), function(i) {
        fields[[i]] <- (paste0("field_", i))
        column(
          width=12,
          selectInput(
            inputId = ns(paste0("field_", i)),
            label = paste0("Column: ", i),
            choices = temp[[i]],
            selected = default_field[i]
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
    
## To be copied in the UI
# mod_field_selection_ui("field_selection_ui_1")
    
## To be copied in the server
# callModule(mod_field_selection_server, "field_selection_ui_1")
 
