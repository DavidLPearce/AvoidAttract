# Function is for calculating the time from species 1 until
# species 1 with species 2 appearing between detentions of species 1.
# The function will return the average of all T4 events within that year for that site,
# every time that an interaction occurred (detailed_result), the summary by year and the total summary.
T4 <- function(data, species1, species2, species_col, datetime_col, site_col, unitTime) {

  # Check if required columns exist
  if (!(species_col %in% names(data) && datetime_col %in% names(data) && site_col %in% names(data))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }

  # subsetting data by species given
  species_data <- data[data[[species_col]] == species1 | data[[species_col]] == species2, ]

  # Convert datetime to 24-hour clock
  species_data[[datetime_col]] <- as.POSIXct(species_data[[datetime_col]], format = "%Y-%m-%d %H:%M:%S")

  # Organizing data by site number
  species_data <- species_data[order(species_data[[site_col]], species_data[[datetime_col]]), ]

  # Results dataframe
  detailed_result <- data.frame(Site = character(), Year = integer(), T2 = numeric())

  # Iterate over all sites
  for (site in unique(species_data[[site_col]]))  {

    # temporary data frame for that site
    temp_result <- data.frame(Site = character(), Year = integer(), T2 = numeric())

    # Subsetting data by iteration
    site_data <- species_data[species_data[[site_col]] == site, ]

    # Extract years for the site
    years <- unique(as.numeric(format(site_data[[datetime_col]], "%Y")))

    for (year in years) {

      # Subset data for the current year
      year_data <- site_data[format(site_data[[datetime_col]], "%Y") == as.character(year), ]
      year_data <- year_data[order(year_data[[datetime_col]]), ]

      # Warning if 1 or fewer observations for a site are entered
      if (nrow(year_data) <= 1) {
        # Skip that site and go to the next site
        next
      }

      # Interactions
      for (row in 1:(nrow(year_data) - 1)) {
        current_species <- year_data[[species_col]][row]
        next_species <- year_data[[species_col]][row + 1]
        third_species <- year_data[[species_col]][row + 2]

        if (!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species == species1 && next_species == species2 && third_species == species1) {
          # Species 1 detection followed by species 2 followed by species 1 detection
          current_species_time <- year_data[[datetime_col]][row]
          next_species_time <- year_data[[datetime_col]][row + 1]
          third_species_time <- year_data[[datetime_col]][row + 2]

          # Calculate the time difference
          time_difference <- difftime(third_species_time, current_species_time, units = unitTime)

          # Saving that interaction
          temp_result <- rbind(temp_result, data.frame(Site = site, Year = year, T4 = time_difference))
        }
      }
    }

    # Save temp_result to detailed_result
    detailed_result <- rbind(detailed_result, temp_result)

  }

  # Convert character columns to their respective types
  detailed_result$Site <- as.character(detailed_result$Site)
  detailed_result$Year <- as.integer(detailed_result$Year)

  # Summarize results by taking the mean for each year at a site
  year_result <- aggregate(T4 ~ Site + Year, data = detailed_result, FUN = mean, na.rm = TRUE)

  # Sort the year_result by ascending site number
  year_result <- year_result[order(year_result$Site), ]

  # Calculate the total summary for the entire output
  total_summary <- mean(detailed_result[, -c(1, 2)], na.rm = TRUE)

  # Combine results into a list
  result_list <- list(total_summary = total_summary, year_result = year_result, detailed_result = detailed_result)

  return(result_list)
}



# Example use
T4_test <- T4(data = KScams, species1 ="White-Tailed Deer", species2 = "Coyote",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")





