library(dplyr)

find_two_column_frequency <- function(data, column_1, column_2){
  d <- data[c(column_1, column_2)]
  colnames(d) <- c("x", "y")
  d <- as.data.frame(table(d))
  d <- d %>% filter(Freq > 0) %>% droplevels()
  return(d)
}
