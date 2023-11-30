#library(AvoidAttract)  # Make sure to replace with your actual package name

# Set seed for reproducibility
set.seed(123)

# Create a subset with 100 random rows
subset_data <- KScams_dat[sample(nrow(KScams_dat), 1500), ]
subset_data$DateTime <- as.POSIXct(subset_data$DateTime, tryFormats = "%m/%d/%Y %H:%M:%OS")

# Save the subset_data as an RDS file
saveRDS(subset_data, "tests/testthat/Test_data/test_data.rds")
