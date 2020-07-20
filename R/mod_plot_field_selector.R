#' plot_field_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_field_selector_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    div(style = "display:none;",
        checkboxGroupInput(ns("checkboxgroup_spatial"), "Input checkbox 2",
                           c("Item A", "Item B", "Item C")
        )
    ),
    actionButton(ns("show"), "Show Field Selector")
  )
}
    
#' plot_field_selector Server Function
#'
#' @noRd 
mod_plot_field_selector_server <- function(input, output, session,  data_reactive, preselected, plot_type){
  ns <- session$ns

  dictionary <- read.csv("data/dictionary.csv")
  group <- reactive(create_group(dictionary, data_reactive$data))
  fields <- reactive(find_field_for_plot(data_reactive$data, plot_type, group()))
  
  

  missing <- vector()
  x <- vector()
  choices <- vector()
  a <- vector()


  columns <- vector()
  
  observe({
    columns <<- names(fields()[[1]])
  })
  

  field <- reactiveValues(selected ="Default")


  
  
  name_with_missing_number <- reactive({
    
    df <-data_reactive$data
    missing_name <- vector()
    names <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()
    
    for(i in colnames(df)){
      names <- c(names,i)
      total_records <- c(
        total_records,
        nrow(df[i])
      )
      missing_records <- c(
        missing_records,
        sum(
          is.na(
            df[i]
          )
        )
      )
      records_percentage <- c(
        records_percentage,
        round(
          (
            (
              nrow(
                df[i]
              ) - sum(
                is.na(
                  df[i]
                )
              )
            ) /
              nrow(
                df[i]
              )
          ),
          2
        ) * 100
      )
      
    }
    return (setNames(as.list(records_percentage), names))
  })
  
  

  
  
  
  observeEvent(input$show, {
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            div(
              style = "border-radius: 25px;border: 2px solid #73AD21; height: 67px;",
              column(
                2,
                selectInput(
                  ns("select_input"),
                  label = "",
                  choices = c("a","b","c"),
                  selected = 'a'
                )
              ),
              column(
                4,
                radioGroupButtons(
                  inputId =  ns("columns"),
                  label = "",
                  choices = columns,
                  checkIcon = list(
                    yes = icon("check-circle"),
                    no = icon("circle-o")
                  ),
                  selected = field$selected,
                  status = "info",
                  size = "xs",
                  direction = "horizontal",
                  individual = TRUE,
                  justified = TRUE
                )
              ),
              column(
                3,
                "Field Type",
                verbatimTextOutput(ns("field_type"))
              )
            )
          ),

          lapply(names(fields()[[1]]), function(i){
            conditionalPanel(
              sprintf("input['%s'] == '%s'", ns("columns"), i),
              
              fluidRow(
                lapply(names(fields()), function(j){
                  create_column(j, i)
                })
              )
            )
          })
          
          # conditionalPanel(
          #   sprintf("input['%s'] == 'Select_X'", ns("columns")),
          #   
          #   fluidRow(
          #     lapply(names(fields()), function(i){
          #       create_column(i, "Select_X")
          #     })
          #   )
          # )
          
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "OK")
        )
      )
    )
  })
  
  observeEvent(input$columns,{
    field$selected = input$columns
  })
  
  
  add_row <- function(id1, id2, col_name){
    selected <- FALSE
    if(col_name %in% colnames(data_reactive$data)){
      if(col_name %in% preselected$default_fields){
        selected = TRUE
      }

      fluidRow(
        column(
          6,
          progressBar(id = id1, value = name_with_missing_number()[[col_name]], display_pct = TRUE),
        ),
        column(
          6,
          prettyCheckbox(
            ns(id2),
            label = col_name,
            shape = "round", 
            outline = TRUE, 
            status = "info",
            value = selected
          )
        )
      )
    }
  }
  
  create_column <- function(group_name, field_name){
    column(
      3,
      style = "width: 25%; overflow-y:scroll; max-height: 600px; border-radius: 25px; border: 2px solid #73AD21; height: 600px;",
      fluidRow(
        column(
          12,
          h4(group_name),
        )
      ),
      lapply(fields()[[group_name]][[field_name]], function(i){
        add_row(paste0("pb_",i,field_name), paste0("cb_",i,field_name), i)
      })
    )
  }
  
  observe({
    if (!is.null(input$columns)) {
      lapply(names(data_reactive$data), function(i) {
        observeEvent(input[[paste0("cb_", i, input$columns)]], {
          if(input$columns=="Default"){
            if (i %in% preselected$default_fields) {
              updatePrettyCheckbox(session, paste0("cb_", i, input$columns), value = TRUE)
            } else{
              updatePrettyCheckbox(session, paste0("cb_", i, input$columns), value = FALSE)
            }
          }else{
            if (input[[paste0("cb_", i, input$columns)]]) {
              for(j in names(data_reactive$data)){
                if(j==i){
                  preselected$new_fields[[input$columns]] = j
                }else{
                  updatePrettyCheckbox(session, paste0("cb_", j, input$columns), value = FALSE)
                }
              }
            }
          }
        })
      })
    }
  })
  
  output$field_type <- renderPrint({
    if(plot_type == "bubble" && input$columns=="Default"){
      "X: Character, Y: Numeric"
    }else if(plot_type == "bubble" && input$columns=="Select_X"){
      "Character"
    }
    else if(plot_type == "bubble" && input$columns=="Select_Y"){
      "Numeric"
    }
  })
  
  # observe({
  # 
  #     lapply(names(data_reactive$data), function(i){
  #       observeEvent(input[[paste0("cb_",i)]],{
  #         if(input[[paste0("cb_",i)]]){
  #           for(j in names(data_reactive$data)){
  #             if(j==i){
  #             }else{
  #               updatePrettyCheckbox(session, paste0("cb_",j), value = FALSE)
  #             }
  #           }
  #         }
  #       })
  #     })
  #     
  # 
  # })
  
  

 
}
    
## To be copied in the UI
# mod_plot_field_selector_ui("plot_field_selector_ui_1")
    
## To be copied in the server
# callModule(mod_plot_field_selector_server, "plot_field_selector_ui_1")
 
