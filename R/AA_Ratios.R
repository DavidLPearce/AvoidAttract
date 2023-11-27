# AA_Ratios calculates the average T1, T2, T3, T4 times and T2/T1 T4/T3 ratios for every event, each year and the total summary
AA_Ratios <- function(data, species1, species2, species_col, datetime_col, site_col, unitTime = "hours") {

  # Check if required columns exist
  if (!(species_col %in% names(data) && datetime_col %in% names(data) && site_col %in% names(data))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }
    # Results dataframe
  detailed_result <- data.frame(Site = character(), Year = integer(), T1 = numeric(), T2 = numeric(), T3 = numeric(), T4 = numeric(), T2_over_T1 = numeric(), T4_over_T3 = numeric())

  # subsetting data by species given
  species_data <- data[data[[species_col]] == species1 | data[[species_col]] == species2, ]

  # Convert datetime to 24-hour clock
  species_data[[datetime_col]] <- as.POSIXct(species_data[[datetime_col]], format = "%Y-%m-%d %H:%M:%S")

  # Organizing data by site number and time
  species_data <- species_data[order(species_data[[site_col]], species_data[[datetime_col]]), ]

  # Iterate over all sites
  for (site in unique(species_data[[site_col]])) {

    # Temporary dataframe to collect T1, T2, T3, T4 values
    temp_result <- data.frame(T1 = numeric(), T2 = numeric(), T3 = numeric(), T4 = numeric())

    # Subsetting data by iteration
    site_data <- species_data[species_data[[site_col]] == site, ]

    # Extract years for the site
    years <- unique(as.numeric(format(site_data[[datetime_col]], "%Y")))

    for (year in years) {

      # Subset data for the current year
      year_data <- site_data[format(site_data[[datetime_col]], "%Y") == as.character(year), ]

      # Organizing data by time
      year_data <- year_data[order(year_data[[datetime_col]]), ]

      # Warning if 1 or fewer observations for a site are entered for that year
      if (nrow(year_data) <= 1) {
        # Skip that site and go to the next site
        next
      }

      # Interactions
      for (row in 1:(nrow(year_data) - 1)) {
        current_species <- year_data[[species_col]][row]
        next_species <- year_data[[species_col]][row + 1]
        third_species <- year_data[[species_col]][row + 2]


        # T1 Events
        if (isTRUE(!is.na(current_species) && !is.na(next_species) &&
            current_species == species1 && next_species == species2)) {
          # Species 1 detection followed by Species 2
          current_species_time <- year_data[[datetime_col]][row]
          next_species_time <- year_data[[datetime_col]][row + 1]

          # Calculate the time difference
          T1 <- difftime(next_species_time, current_species_time, units = unitTime)

        }

        # If T1 event condition is not met set to NA
        if (isTRUE(!is.na(current_species) && !is.na(next_species) &&
                 current_species != species1 || next_species != species2)){
           # Set to NA
           T1 <- NA
        }

        # T2 Events
        if (isTRUE(!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species == species1 && next_species == species2 && third_species == species1)) {
          # Species 1 detection followed by species 2 followed by species 1 detection
          current_species_time <- year_data[[datetime_col]][row]
          next_species_time <- year_data[[datetime_col]][row + 1]
          third_species_time <- year_data[[datetime_col]][row + 2]

          # Calculate the time difference
          T2 <- difftime(third_species_time, next_species_time, units = unitTime)
        }

        # If T2 event condition is not met set to NA
        if (isTRUE(!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species != species1 || next_species != species2 || third_species != species1)){
          # Set to NA
          T2 <- NA
        }

        # T3 Events
        if (isTRUE(!is.na(current_species) && !is.na(next_species) &&
            current_species == species1 && next_species == species1)) {
          # Species 1 detection followed by species 1 detection
          current_species_time <- year_data[[datetime_col]][row]
          next_species_time <- year_data[[datetime_col]][row + 1]

          # Calculate the time difference
          T3 <- difftime(next_species_time, current_species_time, units = unitTime)

        }

        # If T3 event condition is not met set to NA
        if (!is.na(current_species) && !is.na(next_species) &&
            current_species != species1 || next_species != species1) {
            # Set to NA
            T3 <- NA
        }

        # T4 Events
        if (isTRUE(!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species == species1 && next_species == species2 && third_species == species1)) {
          # Species 1 detection followed by species 2 followed by species 1 detection
          current_species_time <- year_data[[datetime_col]][row]
          next_species_time <- year_data[[datetime_col]][row + 1]
          third_species_time <- year_data[[datetime_col]][row + 2]

          # Calculate the time difference
          T4 <- difftime(third_species_time, current_species_time, units = unitTime)
        }

        # If T4 event condition is not met set to NA
        if (isTRUE(!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species != species1 || next_species != species2 || third_species != species1)) {
          # Set to NA
          T4 <- NA
        }

# Saving interactions
temp_result <- rbind(temp_result, c(site = site, year = year, T1 = T1, T2 = T2, T3 = T3, T4 = T4))

      }

 # Save temp_result to detailed_result
detailed_result <- rbind(detailed_result, temp_result)

    }


  }

  # # Calculate T2_over_T1 and T4_over_T3
  # T2_over_T1 <- if (any(!is.na(temp_result$T1)) && any(!is.na(temp_result$T2)))
  #   mean(temp_result$T2, na.rm = TRUE) / mean(temp_result$T1, na.rm = TRUE)
  # else
  #   NA
  #
  # T4_over_T3 <- if (any(!is.na(temp_result$T3)) && any(!is.na(temp_result$T4)))
  #   mean(temp_result$T4, na.rm = TRUE) / mean(temp_result$T3, na.rm = TRUE)
  # else
  #   NA
#
#   # Store final results for the site and year
#   detailed_result <- rbind(detailed_result, c(site, year, mean(temp_result$T1), mean(temp_result$T2), mean(temp_result$T3), mean(temp_result$T4), T2_over_T1, T4_over_T3))

  # # Rename columns for the detailed result
  # colnames(detailed_result) <- c("Site", "Year", "T1", "T2", "T3", "T4", "T2/T1", "T4/T3")

  # Total summary
  total_summary <- colMeans(detailed_result[, -c(1, 2)], na.rm = TRUE)

  # Combine results into a list
  result_list <- list(total_summary = total_summary, detailed_result = detailed_result)

  return(result_list)
}





# Example usage
test_result <- AA_Ratios(data = KScams, species1 ="White-Tailed Deer", species2 = "Coyote",
                   species_col = "Common_name", datetime_col = "DateTime", site_col ="Site")


