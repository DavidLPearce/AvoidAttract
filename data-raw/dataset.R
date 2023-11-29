# data-raw/dataset.R

KScams_dat <- read.csv("data-raw/KansasCamera_data.csv")

usethis::use_data(KScams_dat, overwrite = TRUE)

