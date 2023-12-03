#' Calculating the time between detections of species A to species B
#'
#' The T1 (AB) function analyzes camera trapping data to calculate the time between the detections of species A followed by species B at each camera site for all years. The function returns the average of all T1 events for all sites and across all years, the number of times the event occurred, a site summary across all years, and a detailed summary of each time the event occurred.
#'
#' @param data The camera trapping dataset containing information on species, datetime, and site.
#' @param speciesA The first species in the interaction sequence.
#' @param speciesB The second species in the interaction sequence.
#' @param species_col The column name indicating the species in the dataset.
#' @param datetime_col The column name indicating the datetime of each detection.
#' @param site_col The column name indicating the camera site.
#' @param unitTime The unit of time used for calculating the time differences, default is "hours", options = c("secs", "mins", "hours").
#'
#' @return A list containing:
#'   \describe{
#'     \item{total_summary}{A summary of the mean values for T1 across all sites that recorded an event and years.}
#'     \item{event_count}{The total count of T1 events across all sites and years.}
#'     \item{site_result}{A summary of the mean T1 values for each site that recorded an eventacross all years.}
#'     \item{detailed_summary}{Detailed information on recorded T1 events, including site, year and time differences.}
#'   }
#'
#' @references
#' Parsons, A. W., C. Bland, T. Forrester, M. C. Baker-Whatton, S. G. Schuttler, W. J. McShea, R. Costello, and R. Kays. 2016. The ecological impact of humans and dogs on wildlife in protected areas in eastern North America. Biological Conservation 203:75–88.
#'
#' @source
#' URL: \url{https://doi.org/10.1016/j.biocon.2016.09.001}
#'
#' @seealso
#' Niedballa, J., A. Wilting, R. Sollmann, H. Hofer, and A. Courtiol. 2019. Assessing analytical methods for detecting spatiotemporal interactions between species from camera trapping data. M. Rowcliffe and J. Ahumada, editors. Remote Sensing in Ecology and Conservation 5:272–285.
#'
#' URL: https://doi.org/10.1002/rse2.107
#'
#' @examples
#' # Function Example. Note: DateTime column must be formatted as a date-time and not a character string!
#' T1_example <- T1(data = KScams_dat, speciesA = "White-Tailed Deer", speciesB = "Coyote",
#'                   species_col = "Common_name", datetime_col = "DateTime",
#'                   site_col = "Site", unitTime = "hours")
#'
#' @export
T1 <- function(data, speciesA, speciesB, species_col, datetime_col, site_col, unitTime = "hours") {

  # Check if required columns exist
  if (!(species_col %in% names(data) && datetime_col %in% names(data) && site_col %in% names(data))) {
    stop("One or more specified columns do not exist in the dataframe.")
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
  detailed_summary <- data.frame(Site = character(), Year = integer(), T1 = numeric())

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

        if (isTRUE(!is.na(current_species) && !is.na(next_species) &&
            current_species == speciesA && next_species == speciesB)) {
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
    detailed_summary <- rbind(detailed_summary, temp_result)

    }

  # Convert character columns to their respective types
  detailed_summary$Site <- as.character(detailed_summary$Site)
  detailed_summary$Year <- as.integer(detailed_summary$Year)

  # How many times an event occured
  event_count <- sum(!is.na(detailed_summary$T1))

  # Summarize results by taking the mean for each site across all years
  site_result <- aggregate(T1 ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)

  # Renumber the row names
  row.names(site_result) <- NULL

  # Calculate the total summary for the entire output
  total_summary <- mean(detailed_summary[, -c(1, 2)], na.rm = TRUE)

  # Combine results into a list
  result_list <- list(total_summary = total_summary, event_count = event_count, site_result = site_result, detailed_summary = detailed_summary)

  return(result_list)
}
