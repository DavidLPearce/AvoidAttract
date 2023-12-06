#' Avoidance-Attraction Ratios (AAR)
#'
#' The AAR function analyzes camera trapping data to calculate the average time intervals for interactions involving species A and species B. It calculates the average time for T1, T2, T3, and T4 events as well as the T2/T1 and T4/T3 ratios for each site summarized across all years and provides the total summary across all sites and years.
#'
#' @param data The camera trapping dataset containing information on species, datetime, and site (dataframe).
#' @param speciesA The first species in the interaction sequence (character).
#' @param speciesB The second species in the interaction sequence (character).
#' @param species_col The column name indicating the species in the dataset (character).
#' @param datetime_col The column name indicating the datetime of each detection (character).
#' @param site_col The column name indicating the camera site (character).
#' @param unitTime The unit of time used for calculating the time differences, default is "hours".
#'   Options: "secs", "mins", "hours" (character).
#'
#' @return A list containing:
#'   \describe{
#'     \item{total_summary}{A summary of the mean values for T1, T2, T3, T4, T2/T1, and T4/T3 across all sites that recorded an event and years (named number).}
#'     \item{event_count}{The total count of T1, T2, T3, and T4 events across all sites and years (named integer).}
#'     \item{event_summary}{The min, 1st quartile, median, mean, 3rd quartile, max for all events (dataframe).}
#'     \item{site_summary}{A summary of the mean T1, T2, T3, T4, T2/T1, and T4/T3 values for each site across all years(dataframe).}
#'     \item{detailed_summary}{Detailed information on recorded time events, "including site, year and time" differences (dataframe).}
#'   }
#'
#' @references
#' Parsons, A. W., C. Bland, T. Forrester, M. C. Baker-Whatton, S. G. Schuttler, W. J. McShea, R. Costello, and R. Kays. 2016.
#' The ecological impact of humans and dogs on wildlife in protected areas in eastern North America. Biological Conservation 203:75–88.
#' URL: https://doi.org/10.1016/j.biocon.2016.09.001
#'
#' @source URL: https://doi.org/10.1016/j.biocon.2016.09.001
#'
#' @seealso
#' Niedballa, J., A. Wilting, R. Sollmann, H. Hofer, and A. Courtiol. 2019. Assessing analytical methods for detecting spatiotemporal interactions between species from camera trapping data. M. Rowcliffe and J. Ahumada, editors. Remote Sensing in Ecology and Conservation 5:272–285.
#'
#' URL: https://doi.org/10.1002/rse2.107
#'
#' @examples
#' # Function Example. Note: DateTime column must be formatted as a date time and not a character string!
#' AAR_example <- AAR(data = KScams_dat, speciesA = "White-Tailed Deer", speciesB = "Coyote",
#'                   species_col = "Common_name", datetime_col = "DateTime",
#'                   site_col = "Site", unitTime = "hours")
#'
#' @export
AAR <- function(data, speciesA, speciesB, species_col, datetime_col, site_col, unitTime = "hours") {

  # Check if required columns exist
  if (isTRUE(!(species_col %in% names(data) &&  datetime_col %in% names(data) && site_col %in% names(data)))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }

  # Check if speciesA is in the data
  if (!(speciesA %in% unique(data[[species_col]]))) {
    stop(paste("Error: Species", speciesA, "not found in the", species_col, "column of the data."))

  }
  # Check if speciesB is in the data
  if (!(speciesB %in% unique(data[[species_col]]))) {
    stop(paste("Error: Species", speciesB, "not found in the", species_col, "column of the data."))
  }

  # Ensure the datetime column is in the correct format
  if (isTRUE(!inherits(data[[datetime_col]], "POSIXct"))) {
    stop("Datetime column must be in POSIXct format.")
  }

  # Check for NAs in the datetime column
  if (any(is.na(data[[datetime_col]]))) {
    stop("Datetime column contains NA values. Please ensure all datetime values are present. Issue may be with POSIXct 'format'")
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

    # Iterate over years
    for (year in years) {

      # Subset data for the current year
      year_data <- site_data[format(site_data[[datetime_col]], "%Y") == as.character(year), ]

      # Organizing data by time
      year_data <- year_data[order(year_data[[datetime_col]]), ]

      #Check to see if either species are found in the year data subset
      if (isTRUE(!(speciesA %in% year_data[[species_col]] || speciesB %in%
                   year_data[[species_col]]))) {
        # Skip that site and go to the next site
        next

      }

      # If 1 or fewer observations for a site are entered for that year it will skip that year
      if (isTRUE(nrow(year_data) <= 1)) {
        # Skip that site and go to the next site
        next
      }

      # Interactions
      for (row in 1:(nrow(year_data) - 1)) {
        # Getting current, next and third species names
        current_species <- year_data[[species_col]][row]
        next_species <- year_data[[species_col]][row + 1]
        third_species <- year_data[[species_col]][row + 2]

        # Times of current, next and third species
        current_species_time <- year_data[[datetime_col]][row]
        next_species_time <- year_data[[datetime_col]][row + 1]
        third_species_time <- year_data[[datetime_col]][row + 2]

        # T1 Events: Species A detection followed by Species B
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

        # T2 Events Species B detection followed by species A
        if (isTRUE(!is.na(current_species) && !is.na(next_species) &&
            current_species == speciesB && next_species == speciesA)) {

          # Calculate the time difference
          T2 <- difftime(next_species_time, current_species_time, units = unitTime)
        }

        # If T2 event condition is not met set to NA
        if (isTRUE(!is.na(current_species) && !is.na(next_species)  &&
            current_species != speciesB || next_species != speciesA)){
          # Set to NA
          T2 <- NA
        }

        # T3 Events Species A detection followed by species A detection
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

        # T4 Events Species A detection followed by species B followed by species A detection
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
# Renaming temp_result columns names to ensure they match up with detailed_summary
colnames(temp_result) <- c("Site", "Year", "T1", "T2", "T3", "T4")
colnames(detailed_summary) <- c("Site", "Year", "T1", "T2", "T3", "T4")
detailed_summary <- rbind(detailed_summary, temp_result)

}

# Convert T1, T2, T3, T4 columns to numeric
detailed_summary$T1 <- as.numeric(detailed_summary$T1)
detailed_summary$T2 <- as.numeric(detailed_summary$T2)
detailed_summary$T3 <- as.numeric(detailed_summary$T3)
detailed_summary$T4 <- as.numeric(detailed_summary$T4)

# Creating a dataframe for by site reporting
all_sites <- data.frame(Site = unique(data[[site_col]]))

# Taking the mean of T1-T4 for each site
# Checking to see if there are observations to aggregate

# Check if there are non-NA numeric values in T1
if (any(!is.na(detailed_summary$T1)) && any(sapply(detailed_summary$T1, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_T1 <- aggregate(T1 ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(all_sites, site_means_T1, by = "Site", all.x = TRUE)
}
# Warning if there are NAs
if (!any(is.na(detailed_summary$T1)) && any(sapply(detailed_summary$T1, is.numeric))){
  warning("No T1 interaction events occurred. Cannot calculate a mean for this event.")
}

# Check if there are non-NA numeric values in T2
if (any(!is.na(detailed_summary$T2)) && any(sapply(detailed_summary$T2, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_T2 <- aggregate(T2 ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(site_summary, site_means_T2, by = "Site", all.x = TRUE)
}
# Warning if there are NAs
if (!any(!is.na(detailed_summary$T2)) && any(sapply(detailed_summary$T2, is.numeric)))  {
  warning("No T2 interaction events occurred. Cannot calculate a mean for this event.")
}

# Check if there are non-NA numeric values in T3
if (any(!is.na(detailed_summary$T3)) && any(sapply(detailed_summary$T3, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_T3 <- aggregate(T3 ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(site_summary, site_means_T3, by = "Site", all.x = TRUE)
}

# Warning if there are NAs
if (!any(!is.na(detailed_summary$T3)) && any(sapply(detailed_summary$T3, is.numeric))) {
  warning("No T3 interaction events occurred. Cannot calculate a mean for this event.")
}


# Check if there are non-NA numeric values in T4
if (any(!is.na(detailed_summary$T4)) && any(sapply(detailed_summary$T4, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_T4 <- aggregate(T4 ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(site_summary, site_means_T4, by = "Site", all.x = TRUE)
}

# Warning if there are NAs
if (!any(!is.na(detailed_summary$T4)) && any(sapply(detailed_summary$T4, is.numeric))){
  warning("No T4 interaction events occurred. Cannot calculate a mean for this event.")
}


# Site summary ratios
# Will only calculate ratios if there is a mean for T1 & T2 or T3 & T4
if (any(!is.na(detailed_summary$T1)) && any(sapply(detailed_summary$T1, is.numeric)) &&
    any(!is.na(detailed_summary$T2)) && any(sapply(detailed_summary$T2, is.numeric)) ||
    any(!is.na(detailed_summary$T3)) && any(sapply(detailed_summary$T3, is.numeric)) &&
    any(!is.na(detailed_summary$T4)) && any(sapply(detailed_summary$T4, is.numeric))) {

  # Calculate T2/T1 and T4/T3 ratios by site
  site_summary$T2_over_T1 <- with(site_summary, T2 / T1)
  site_summary$T4_over_T3 <- with(site_summary, T4 / T3)

  colnames(site_summary) <- c("Site", "T1", "T2", "T3", "T4", "T2/T1", "T4/T3")

}

# Error message for when there is no mean for T1 & T2 or T3 & T4
if (!any(!is.na(detailed_summary$T1)) && any(sapply(detailed_summary$T1, is.numeric)) ||
    !any(!is.na(detailed_summary$T2)) && any(sapply(detailed_summary$T2, is.numeric)) ||
    !any(!is.na(detailed_summary$T3)) && any(sapply(detailed_summary$T3, is.numeric)) ||
    !any(!is.na(detailed_summary$T4)) && any(sapply(detailed_summary$T4, is.numeric))) {

  # Warning message
  warning("Unable to calculate site summary Avoidance-Attraction Ratios due to lack of event occurances.")
}


# To not get scientific numbers in event_summery output
options(scipen = 999)

# Event summaries T1-4
summary_T1 <- as.matrix(summary(detailed_summary$T1))
summary_T2 <- as.matrix(summary(detailed_summary$T2))
summary_T3 <- as.matrix(summary(detailed_summary$T3))
summary_T4 <- as.matrix(summary(detailed_summary$T4))

# Create a data frame with columns T1, T2, T3, T4
event_summary <- data.frame(
  T1 = summary_T1[-7], # not including NA's
  T2 = summary_T2[-7],
  T3 = summary_T3[-7],
  T4 = summary_T4[-7]
)

# Replace NaN with NA in each column
event_summary[] <- lapply(event_summary, function(x) replace(x, is.nan(x), NA))

# Renaming event_summary rows
rownames(event_summary) = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")

# Calculate the event counts
event_count_T1 <- sum(!is.na(detailed_summary$T1))
event_count_T2 <- sum(!is.na(detailed_summary$T2))
event_count_T3 <- sum(!is.na(detailed_summary$T3))
event_count_T4 <- sum(!is.na(detailed_summary$T4))

# Combining all event counts into a single output
event_counts <- c(T1 = event_count_T1, T2 = event_count_T2, T3 = event_count_T3, T4 = event_count_T4)

# Total summary
total_summary <- colMeans(detailed_summary[, -c(1, 2)], na.rm = TRUE)

# Total summary ratios
# Will only calculate ratios if there is a mean for T1 & T2 or T3 & T4
if (any(!is.na(total_summary[1])) && any(sapply(total_summary[1], is.numeric)) &&
    any(!is.na(total_summary[2])) && any(sapply(total_summary[2], is.numeric)) ||
    any(!is.na(total_summary[3])) && any(sapply(total_summary[3], is.numeric)) &&
    any(!is.na(total_summary[4])) && any(sapply(total_summary[4], is.numeric))) {

  # Calculate T2/T1 and T4/T3 ratios from total means
  total_summary <- c(total_summary,
                     T2_over_T1 = total_summary[2] / total_summary[1],
                     T4_over_T3 = total_summary[4] / total_summary[3])

  # Renaming ratio columns
  names(total_summary)[5] <- "T2/T1"
  names(total_summary)[6] <- "T4/T3"

  # Replace NaN with NA
  total_summary <- replace(total_summary, is.nan(total_summary), NA)

}

# Error message for when there is no mean for T1 & T2 or T3 & T4
if (!any(!is.na(total_summary[1])) && any(sapply(total_summary[1], is.numeric)) ||
      !any(!is.na(total_summary[2])) && any(sapply(total_summary[2], is.numeric)) ||
      !any(!is.na(total_summary[3])) && any(sapply(total_summary[3], is.numeric)) ||
      !any(!is.na(total_summary[4])) && any(sapply(total_summary[4], is.numeric))) {

  # Warning message
  warning("Unable to calculate total summary Avoidance-Attraction Ratios due to lack of event occurances.")

  # Replace NaN with NA
  total_summary <- replace(total_summary, is.nan(total_summary), NA)
}


# Combine results into a list
result_list <- list(total_summary = total_summary,event_count = event_counts, event_summary = event_summary,
                    site_summary = site_summary, detailed_summary = detailed_summary)

return(result_list)
}

