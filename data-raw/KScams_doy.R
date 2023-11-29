# Day of year covariates

KScams_doy <- read.csv("data-raw/KansasCamera_doy.csv")

usethis::use_data(KScams_doy, overwrite = TRUE, compress = "xz")
