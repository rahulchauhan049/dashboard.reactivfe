find_field_for_plot <- function(data, plot, group){
  
  new_list <- list()
  characters <-  names(dplyr::select_if(hyenaData,is.character))
  numeric <- names(dplyr::select_if(hyenaData,is.numeric))
  
  if(plot=="bubble"){
    
    for(i in names(group)){
      if(i != "core"){
        new_list[[i]][["Default"]] <- group[[i]]
        new_list[[i]][["Select_X"]] <- Reduce(intersect, list(characters, group[[i]]))
        new_list[[i]][["Select_Y"]] <- Reduce(intersect, list(numeric, group[[i]]))
      }
      
    }
    
  }
  
  return(new_list)
  
}


 
 a <- find_field_for_plot(hyenaData,"bubble", g)
