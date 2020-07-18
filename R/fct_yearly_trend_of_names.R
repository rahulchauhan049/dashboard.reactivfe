yearly_trend_of_names <- function(data, column_1, column_2){
  a <- list()
  data <- na.omit(data[c(column_1, column_2)])
  for(i in unique(data[[column_1]])){
    dat <- filter(data, data[[column_1]]==i)
    dat <- as.data.frame(table(dat[[column_2]])) 
    dat <-  dat %>%
      mutate(cumsum = cumsum(Freq))
    a[[i]] <- dat
  }
  return(a)
}


