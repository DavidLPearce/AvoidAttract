# data-raw/dataset.R

KScams <- read.csv("data-raw/KansasCamera_data.csv")

usethis::use_data(KScams, overwrite = TRUE)

