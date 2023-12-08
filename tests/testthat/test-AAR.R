# Set seed for reproducibility
set.seed(123)

# Create a subset with 1500 random rows
subset_data <- KScams_dat[sample(nrow(KScams_dat), 1500), ]
subset_data$DateTime <- as.POSIXct(subset_data$DateTime, tryFormats = "%m/%d/%Y %H:%M:%OS")

# expected result for AAR function
expected_result <- list(
  total_summary = c(177.2397, 251.017817, 114.9631, 423.8421, 1.416261, 3.6867657),
  event_count = c(13, 14, 113, 3 ))
str(expected_result)

# Test AAR function
test_that("AAR function calculates time between detections correctly for T1", {
  result <- AAR(data = subset_data, speciesA = "White-Tailed Deer", speciesB = "Coyote",
                species_col = "Common_name", datetime_col = "DateTime", site_col = "Site")

  result$total_summary <- as.numeric(result$total_summary)
  result$event_count <- as.numeric(expected_result$event_count)

  # Set a tolerance value
  tolerance <- 0.001

  # Check total summary with tolerance
  expect_equal(result$total_summary, expected_result$total_summary,
               ignore_attr = FALSE, tolerance = tolerance)

  # Check event count
  expect_equal(result$event_count, expected_result$event_count)
})

