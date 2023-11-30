# Load necessary libraries
# library(testthat)
# library(AvoidAttract)
#
# # Path to test data
# test_data_path <- system.file("testthat", "test_data.rds", package = "AvoidAttract")


# Load the subset_data for testing
subset_data <- readRDS(test_data_path)

# expected result for T1 function
expected_result <- list(
  total_summary = as.difftime(177.2397, units = "hours"),
  event_count = 13
)

# Test T1 function
test_that("T1 function calculates time between detections correctly", {
  result <- T1(data = subset_data, speciesA = "White-Tailed Deer", speciesB = "Coyote",
               species_col = "Common_name", datetime_col = "DateTime", site_col = "Site")

  # Set a tolerance value
  tolerance <- 0.001

  # Check total summary with tolerance
  expect_equal(result$total_summary, expected_result$total_summary, ignore_attr = FALSE, tolerance = tolerance)

  # Check event count
  expect_equal(result$event_count, expected_result$event_count)
})
