# Load necessary libraries
# library(testthat)
# library(AvoidAttract)

# Setting DateTime
KScams_dat$DateTime <- as.POSIXct(KScams_dat$DateTime, tryFormats = "%m/%d/%Y %H:%M:%OS")

# expected result for T3 function
expected_result <- list(
  total_summary = as.difftime(17.188464, units = "hours"),
  event_count = 5038
)

expected_result$total_summary <- as.numeric(expected_result$total_summary)
expected_result$event_count <- as.numeric(expected_result$event_count)


# Test T3 function
test_that("T3 function calculates time between detections correctly", {
  result <- T3(data = KScams_dat, speciesA = "White-Tailed Deer", speciesB = "Coyote",
               species_col = "Common_name", datetime_col = "DateTime", site_col = "Site")

  result$total_summary <- as.numeric(result$total_summary)
  result$event_count <- as.numeric(result$event_count)

  # Set a tolerance value
  tolerance <- 0.001

  # Check total summary with tolerance
  expect_equal(result$total_summary, expected_result$total_summary, ignore_attr = FALSE, tolerance = tolerance)

  # Check event count
  expect_equal(result$event_count, expected_result$event_count)
})
