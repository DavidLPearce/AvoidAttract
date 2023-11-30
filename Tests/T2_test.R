# Load necessary libraries
library(testthat)
library(AvoidAttract)

# Load the subset_data for testing
subset_data <- readRDS("Tests/Test_data/test_data.rds")

# expected result for T1 function
expected_result <- list(
  total_summary = as.difftime(176.421, units = "hours"),
  event_count = 3
)

# Test T1 function
test_that("T2 function calculates time between detections correctly", {
  result <- T2(data = subset_data, speciesA = "White-Tailed Deer", speciesB = "Coyote",
               species_col = "Common_name", datetime_col = "DateTime", site_col = "Site")

  # Set a tolerance value
  tolerance <- 0.001

  # Check total summary with tolerance
  expect_equal(result$total_summary, expected_result$total_summary, check.attributes = FALSE, tol = tolerance)

  # Check event count
  expect_equal(result$event_count, expected_result$event_count)
})
