# Load necessary libraries
# library(testthat)
# library(AvoidAttract)

# Set seed for reproducibility
set.seed(123)

# Create a subset with 1500 random rows
subset_data <- KScams_dat[sample(nrow(KScams_dat), 1500), ]
subset_data$DateTime <- as.POSIXct(subset_data$DateTime, tryFormats = "%m/%d/%Y %H:%M:%OS")

# expected result for T1 function
expected_result <- list(
  total_summary = as.difftime(114.9631, units = "hours"),
  event_count = 113
)

# Test T3 function
test_that("T3 function calculates time between detections correctly", {
  result <- T3(data = subset_data, speciesA = "White-Tailed Deer", speciesB = "Coyote",
               species_col = "Common_name", datetime_col = "DateTime", site_col = "Site")

  # Set a tolerance value
  tolerance <- 0.001

  # Check total summary with tolerance
  expect_equal(result$total_summary, expected_result$total_summary, ignore_attr = FALSE, tolerance = tolerance)

  # Check event count
  expect_equal(result$event_count, expected_result$event_count)
})
