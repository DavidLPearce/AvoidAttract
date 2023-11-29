# Function is for calculating the time from species A until species B for all sites and all years within a dataframe
# The function will return the median of all T1 events within that year for that site,
# every time that an interaction occurred and the total summary
T1 <- function(data, speciesA, speciesB, species_col, datetime_col, site_col, unitTime = "hours") {

  # Check if required columns exist
  if (!(species_col %in% names(data) && datetime_col %in% names(data) && site_col %in% names(data))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }

  # Results dataframe
  detailed_result <- data.frame(Site = character(), Year = integer(), T1 = numeric())

  # subsetting data by species given
  species_data <- data[data[[species_col]] == speciesA | data[[species_col]] == speciesB, ]

  # Convert datetime to 24-hour clock
  species_data[[datetime_col]] <- as.POSIXct(species_data[[datetime_col]], format = "%Y-%m-%d %H:%M:%S")

  # Organizing data by site number
  species_data <- species_data[order(species_data[[site_col]], species_data[[datetime_col]]), ]

  # Iterate over all sites
  for (site in unique(species_data[[site_col]]))  {

    # temporary data frame for that site
    temp_result <- data.frame(Site = character(), Year = integer(), T1 = numeric())

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

        if (!is.na(current_species) && !is.na(next_species) &&
            current_species == speciesA && next_species == speciesB) {
          # Species 1 detection followed by Species 2
          current_species_time <- year_data[[datetime_col]][row]
          next_species_time <- year_data[[datetime_col]][row + 1]

          # Calculate the time difference
          time_difference <- difftime(next_species_time, current_species_time, units = unitTime)

          # Saving that interaction
          temp_result <- rbind(temp_result, data.frame(Site = site, Year = year, T1 = time_difference))
        }
      }
    }

    # Save temp_result to detailed_result
    detailed_result <- rbind(detailed_result, temp_result)

    }

  # Convert character columns to their respective types
  detailed_result$Site <- as.character(detailed_result$Site)
  detailed_result$Year <- as.integer(detailed_result$Year)

  # How many times an event occured
  event_count <- sum(!is.na(detailed_result$T1))

  # Summarize results by taking the mean for each site across all years
  site_result <- aggregate(T1 ~ Site, data = detailed_result, FUN = mean, na.rm = TRUE)

  # Convert Site to numeric and sort the result by ascending site number
  site_result$Site <- as.numeric(as.character(site_result$Site))
  site_result <- site_result[order(site_result$Site), ]

  # Renumber the row names
  row.names(site_result) <- NULL

  # Calculate the total summary for the entire output
  total_summary <- mean(detailed_result[, -c(1, 2)], na.rm = TRUE)

    # Combine results into a list
  result_list <- list(total_summary = total_summary, event_count = event_count, site_result = site_result, detailed_result = detailed_result)

  return(result_list)
}
