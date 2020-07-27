## code to prepare `dictionary` dataset goes here
dictionary <- read.csv("data/dictionary.csv")

usethis::use_data(dictionary, overwrite = TRUE)
