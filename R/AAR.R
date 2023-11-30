# AAR calculates the average T1, T2, T3, T4 times and T2/T1 T4/T3 ratios for every event, each year and the total summary
#' Avoidance-Attraction Ratios (AAR)
#'
#' @param data
#' @param speciesA
#' @param speciesB
#' @param species_col
#' @param datetime_col
#' @param site_col
#' @param unitTime
#'
#' @return
#' @export
#'
#' @examples
AAR <- function(data, speciesA, speciesB, species_col, datetime_col, site_col, unitTime = "hours") {

  # Check if required columns exist
  if (!(species_col %in% names(data) && datetime_col %in% names(data) && site_col %in% names(data))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }
    # Results dataframe
  detailed_summary <- data.frame(Site = character(), Year = integer(), T1 = numeric(), T2 = numeric(), T3 = numeric(), T4 = numeric())

  # subsetting data by species given
  species_data <- data[data[[species_col]] == speciesA | data[[species_col]] == speciesB, ]

  # Convert datetime to 24-hour clock
  species_data[[datetime_col]] <- as.POSIXct(species_data[[datetime_col]], format = "%Y-%m-%d %H:%M:%S")

  # Organizing data by site number and time
  species_data <- species_data[order(species_data[[site_col]], species_data[[datetime_col]]), ]

  # Iterate over all sites
  for (site in unique(species_data[[site_col]])) {

    # Temporary dataframe to collect T1, T2, T3, T4 values
    temp_result <- data.frame(Site = character(), Year = integer(), T1 = numeric(), T2 = numeric(), T3 = numeric(), T4 = numeric())

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
        # Getting current, next and third species
        current_species <- year_data[[species_col]][row]
        next_species <- year_data[[species_col]][row + 1]
        third_species <- year_data[[species_col]][row + 2]

        # Times of current, next and third species
        current_species_time <- year_data[[datetime_col]][row]
        next_species_time <- year_data[[datetime_col]][row + 1]
        third_species_time <- year_data[[datetime_col]][row + 2]

        # T1 Events: Species 1 detection followed by Species 2
        if (isTRUE(!is.na(current_species) && !is.na(next_species) &&
            current_species == speciesA && next_species == speciesB)) {

                    # Calculate the time difference
          T1 <- difftime(next_species_time, current_species_time, units = unitTime)

        }

        # If T1 event condition is not met set to NA
        if (isTRUE(!is.na(current_species) && !is.na(next_species) &&
                   (current_species != speciesA || next_species != speciesB))) {
          # Set to NA
          T1 <- NA
        }

        # T2 Events Species 1 detection followed by species 2 followed by species 1 detection
        if (isTRUE(!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species == speciesA && next_species == speciesB && third_species == speciesA)) {

          # Calculate the time difference
          T2 <- difftime(third_species_time, next_species_time, units = unitTime)
        }

        # If T2 event condition is not met set to NA
        if (isTRUE(!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species != speciesA || next_species != speciesB || third_species != speciesA)){
          # Set to NA
          T2 <- NA
        }

        # T3 Events Species 1 detection followed by species 1 detection
        if (isTRUE(!is.na(current_species) && !is.na(next_species) &&
            current_species == speciesA && next_species == speciesA)) {

          # Calculate the time difference
          T3 <- difftime(next_species_time, current_species_time, units = unitTime)

        }

        # If T3 event condition is not met set to NA
        if (!is.na(current_species) && !is.na(next_species) &&
            current_species != speciesA || next_species != speciesA) {
            # Set to NA
            T3 <- NA
        }

        # T4 Events Species 1 detection followed by species 2 followed by species 1 detection
        if (isTRUE(!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species == speciesA && next_species == speciesB && third_species == speciesA)) {

          # Calculate the time difference
          T4 <- difftime(third_species_time, current_species_time, units = unitTime)
        }

        # If T4 event condition is not met set to NA
        if (isTRUE(!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species != speciesA || next_species != speciesB || third_species != speciesA)) {
          # Set to NA
          T4 <- NA
        }

# Saving interactions
temp_result <- rbind(temp_result, c(site = site, year = year, T1 = T1, T2 = T2, T3 = T3, T4 = T4))

      }
    }

# Save temp_result to detailed_summary
colnames(temp_result) <- c("Site", "Year", "T1", "T2", "T3", "T4")
colnames(detailed_summary) <- c("Site", "Year", "T1", "T2", "T3", "T4")
detailed_summary <- rbind(detailed_summary, temp_result)

}

# Creating a dataframe for by site reporting
all_sites <- data.frame(Site = unique(detailed_summary$Site))

# Taking the mean of T1-T4 for each site
site_means_T1 <- aggregate(T1 ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
site_means_T2 <- aggregate(T2 ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
site_means_T3 <- aggregate(T3 ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
site_means_T4 <- aggregate(T4 ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)


# Merging all means into one summary for each site
site_summary <- merge(all_sites, site_means_T1, by = "Site", all.x = TRUE)
site_summary <- merge(site_summary, site_means_T2, by = "Site", all.x = TRUE)
site_summary <- merge(site_summary, site_means_T3, by = "Site", all.x = TRUE)
site_summary <- merge(site_summary, site_means_T4, by = "Site", all.x = TRUE)

# Calculate T2/T1 and T4/T3 ratios by site
site_summary$T2_over_T1 <- with(site_summary, T2 / T1)
site_summary$T4_over_T3 <- with(site_summary, T4 / T3)
colnames(site_summary) <- c("Site", "T1", "T2", "T3", "T4", "T2/T1", "T4/T3")


# Total summary
total_summary <- colMeans(detailed_summary[, -c(1, 2)], na.rm = TRUE)

# Calculate ratios
total_summary <- c(total_summary,
                   T2_over_T1 = total_summary[2] / total_summary[1],
                   T4_over_T3 = total_summary[4] / total_summary[3])

# Change the names of the vector elements
names(total_summary)[5] <- "T2/T1"
names(total_summary)[6] <- "T4/T3"

# Calculate the event counts
event_count_T1 <- sum(!is.na(detailed_summary$T1))
event_count_T2 <- sum(!is.na(detailed_summary$T2))
event_count_T3 <- sum(!is.na(detailed_summary$T3))
event_count_T4 <- sum(!is.na(detailed_summary$T4))

# Combine into a named vector
event_counts <- c(T1 = event_count_T1, T2 = event_count_T2, T3 = event_count_T3, T4 = event_count_T4)

# Combine results into a list
result_list <- list(total_summary = total_summary, event_count = event_counts, site_summary = site_summary)

return(result_list)
}
