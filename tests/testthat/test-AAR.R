
# Setting DateTime
KScams_dat$DateTime <- as.POSIXct(KScams_dat$DateTime, tryFormats = "%m/%d/%Y %H:%M:%OS")

# expected result for AAR function
expected_result <- list(
  total_summary = c(58.798163, 63.252642, 17.188464, 94.676352 , 1.075759 , 5.508133),
  event_count = c(515, 478, 5038, 180))

expected_result$total_summary <- as.numeric(expected_result$total_summary)
expected_result$event_count <- as.numeric(expected_result$event_count)


# Test AAR function
test_that("AAR function calculates time between detections correctly for total summary of all T events and ratios", {
  result <- AAR(data = KScams_dat, speciesA = "White-Tailed Deer", speciesB = "Coyote",
                species_col = "Common_name", datetime_col = "DateTime", site_col = "Site")

  result$total_summary <- as.numeric(result$total_summary)
  result$event_count <- as.numeric(result$event_count)

  # Set a tolerance value
  tolerance <- 0.001

  # Check total summary with tolerance
  expect_equal(result$total_summary, expected_result$total_summary,
               ignore_attr = FALSE, tolerance = tolerance)

  # Check event count
  expect_equal(result$event_count, expected_result$event_count)
})

