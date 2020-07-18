find_fields <- function(plot, data, group_list = NULL, group_selected = NULL){
  columns <- list()

  
  if(plot == "bubble" || plot == "line"){
    if(!is.null(group_list)){
      if(!is.null(group_selected)){
        columns$x <- group_list[[group_selected]]
        columns$y <- names(dplyr::select_if(data,is.numeric))
      }
    }else{
        columns$x <- names(dplyr::select_if(data,is.character))
        columns$y <- names(dplyr::select_if(data,is.numeric))
    }
   
  } else if(plot == "pie" || plot == "bar"){
    if(!is.null(group_list)){
      if(!is.null(group_selected)){
        columns$x <- group_list[[group_selected]]
      }
    }
    else{
      columns$x <-  columns$x <- names(dplyr::select_if(data,is.character))
    }
  }
  return(columns)
}