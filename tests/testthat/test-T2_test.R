# Load necessary libraries
# library(testthat)
# library(AvoidAttract)


# Setting DateTime
KScams_dat$DateTime <- as.POSIXct(KScams_dat$DateTime, tryFormats = "%m/%d/%Y %H:%M:%OS")

# expected result for T2 function
expected_result <- list(
  total_summary = as.difftime(63.25264, units = "hours"),
  event_count = 478
)

expected_result$total_summary <- as.numeric(expected_result$total_summary)
expected_result$event_count <- as.numeric(expected_result$event_count)



# Test T2 function
test_that("T2 function calculates time between detections correctly", {
  result <- T2(data = KScams_dat, speciesA = "White-Tailed Deer", speciesB = "Coyote",
               species_col = "Common_name", datetime_col = "DateTime", site_col = "Site")

  result$total_summary <- as.numeric(result$total_summary)
  result$event_count <- as.numeric(result$event_count)

  # Set a tolerance value
  tolerance <- 0.001

  # Check total summary with tolerance
  expect_equal(result$total_summary, expected_result$total_summary, ignore_attr = FALSE , tolerance = tolerance)

  # Check event count
  expect_equal(result$event_count, expected_result$event_count)
})
