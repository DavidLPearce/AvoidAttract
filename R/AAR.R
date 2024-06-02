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
  detailed_summary <- data.frame(Site = character(), Year = integer(),
                                                                       AB = numeric(), # Dominant - Subdominant
                                                                       BA = numeric(), # Subdominant - Dominant
                                                                       AA = numeric(), # Dominant - Dominant
                                                                       BB = numeric(), # Subdominant - Subdominant
                                                                       ABA = numeric(), # Dominant - Subdominant - Dominant
                                                                       BAB = numeric()) # Subdominant - Dominant - Subdominant

  # subsetting data by species given
  species_data <- data[data[[species_col]] == speciesA | data[[species_col]] == speciesB, ]

  # Convert datetime to 24-hour clock
  species_data[[datetime_col]] <- as.POSIXct(species_data[[datetime_col]], format = "%Y-%m-%d %H:%M:%S")

  # Organizing data by site number and time
  species_data <- species_data[order(species_data[[site_col]], species_data[[datetime_col]]), ]

  # Iterate over all sites
  for (site in unique(species_data[[site_col]])) {

    # Temporary dataframe to collect interaction values
    temp_result <- data.frame(Site = character(), Year = integer(),
                              AB = numeric(), # Dominant - Subdominant
                              BA = numeric(), # Subdominant - Dominant
                              AA = numeric(), # Dominant - Dominant
                              BB = numeric(), # Subdominant - Subdominant
                              ABA = numeric(), # Dominant - Subdominant - Dominant
                              BAB = numeric()) # Subdominant - Dominant - Subdominant

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

      # Check to see if either species are found in the year data subset if neither are found - next
      if (isTRUE(!(speciesA %in% year_data[[species_col]] | speciesB %in%
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

        # Initialize interaction variables to NA
        AB <- NA
        BA <- NA
        AA <- NA
        BB <- NA
        ABA <- NA
        BAB <- NA

        # Getting current and next species names
        current_species <- year_data[[species_col]][row]
        next_species <- year_data[[species_col]][row + 1]


        # Times of current and next species
        current_species_time <- year_data[[datetime_col]][row]
        next_species_time <- year_data[[datetime_col]][row + 1]


        # Initialize third_species only if it exists to avoid out of range errors
        if (row + 2 <= nrow(year_data)) {
          third_species <- year_data[[species_col]][row + 2]
          third_species_time <- year_data[[datetime_col]][row + 2]
        } else {
          third_species <- NA
          third_species_time <- NA
        }


        ### AB Event: Species A detection followed by Species B ###
        if (!is.na(current_species) && !is.na(next_species) &&
            current_species == speciesA && next_species == speciesB) {
          # Calculate the time difference
          AB <- difftime(next_species_time, current_species_time, units = unitTime)
        }


        ### BA Event: Species B detection followed by species A ###
        if (!is.na(current_species) && !is.na(next_species) &&
            current_species == speciesB && next_species == speciesA) {
          # Calculate the time difference
          BA <- difftime(next_species_time, current_species_time, units = unitTime)
        }

        ### AA Event: Species A detection followed by species A detection ###
        if (!is.na(current_species) && !is.na(next_species) &&
            current_species == speciesA && next_species == speciesA) {
          # Calculate the time difference
          AA <- difftime(next_species_time, current_species_time, units = unitTime)
        }

        ### BB Event: Species B detection followed by species B detection ###
        if (!is.na(current_species) && !is.na(next_species) &&
            current_species == speciesB && next_species == speciesB) {
          # Calculate the time difference
          BB <- difftime(next_species_time, current_species_time, units = unitTime)
        }

        ### ABA Event: Species A detection followed by species B followed by species A detection ###
        if (!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species == speciesA && next_species == speciesB && third_species == speciesA) {
          # Calculate the time difference
          ABA <- difftime(third_species_time, current_species_time, units = unitTime)
        }

        ### BAB Event: Species B detection followed by species A followed by species B detection ###
        if (!is.na(current_species) && !is.na(next_species) && !is.na(third_species) &&
            current_species == speciesB && next_species == speciesA && third_species == speciesB) {
          # Calculate the time difference
          BAB <- difftime(third_species_time, current_species_time, units = unitTime)
        }

# Saving interactions
temp_result <- rbind(temp_result, c(site = site, year = year,
                                                              AB = AB, # Dominant - Subdominant
                                                              BA = BA, # Subdominant - Dominant
                                                              AA = AA, # Dominant - Dominant
                                                              BB = BB, # Subdominant - Subdominant
                                                              ABA = ABA, # Dominant - Subdominant - Dominant
                                                              BAB = BAB)) # Subdominant - Dominant - Subdominant

      }
    }

# Save temp_result to detailed_summary
# Renaming temp_result columns names to ensure they match up with detailed_summary
colnames(temp_result) <- c("Site", "Year", "AB", "BA", "AA", "BB", "ABA", "BAB")
colnames(detailed_summary) <- c("Site", "Year", "AB", "BA", "AA", "BB", "ABA", "BAB")
detailed_summary <- rbind(detailed_summary, temp_result)

}
if (isTRUE(NROW(detailed_summary) == 0)){
  stop(paste("Error: No time interactions occured between",speciesA ,"and", speciesB))
}


# Convert AB, BA, AA, BB, ABA, BAB columns to numeric
detailed_summary$AB <- as.numeric(detailed_summary$AB)
detailed_summary$BA <- as.numeric(detailed_summary$BA)
detailed_summary$AA <- as.numeric(detailed_summary$AA)
detailed_summary$BB <- as.numeric(detailed_summary$BB)
detailed_summary$ABA <- as.numeric(detailed_summary$ABA)
detailed_summary$BAB <- as.numeric(detailed_summary$BAB)


# Creating a dataframe for by site reporting
all_sites <- data.frame(Site = unique(data[[site_col]]))

# Taking the mean of events for each site
# Checking to see if there are observations to aggregate

# Check if there are non-NA numeric values in AB
if (any(!is.na(detailed_summary$AB)) && any(sapply(detailed_summary$AB, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_AB <- aggregate(AB ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(all_sites, site_means_AB, by = "Site", all.x = TRUE)
}
# Warning if there are NAs
if (!any(is.na(detailed_summary$AB)) && any(sapply(detailed_summary$AB, is.numeric))){
  warning("No AB interaction events occurred. Cannot calculate a mean for this event.")
}

# Check if there are non-NA numeric values in BA
if (any(!is.na(detailed_summary$BA)) && any(sapply(detailed_summary$BA, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_BA <- aggregate(BA ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(site_summary, site_means_BA, by = "Site", all.x = TRUE)
}
# Warning if there are NAs
if (!any(!is.na(detailed_summary$BA)) && any(sapply(detailed_summary$BA, is.numeric)))  {
  warning("No BA interaction events occurred. Cannot calculate a mean for this event.")
}

# Check if there are non-NA numeric values in AA
if (any(!is.na(detailed_summary$AA)) && any(sapply(detailed_summary$AA, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_AA <- aggregate(AA ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(site_summary, site_means_AA, by = "Site", all.x = TRUE)
}

# Warning if there are NAs
if (!any(!is.na(detailed_summary$AA)) && any(sapply(detailed_summary$AA, is.numeric))) {
  warning("No AA interaction events occurred. Cannot calculate a mean for this event.")
}


# Check if there are non-NA numeric values in BB
if (any(!is.na(detailed_summary$BB)) && any(sapply(detailed_summary$BB, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_BB <- aggregate(BB ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(site_summary, site_means_BB, by = "Site", all.x = TRUE)
}

# Warning if there are NAs
if (!any(!is.na(detailed_summary$BB)) && any(sapply(detailed_summary$BB, is.numeric))){
  warning("No BB interaction events occurred. Cannot calculate a mean for this event.")
}

# Check if there are non-NA numeric values in ABA
if (any(!is.na(detailed_summary$ABA)) && any(sapply(detailed_summary$ABA, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_ABA <- aggregate(ABA ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(site_summary, site_means_ABA, by = "Site", all.x = TRUE)
}

# Warning if there are NAs
if (!any(!is.na(detailed_summary$ABA)) && any(sapply(detailed_summary$ABA, is.numeric))){
  warning("No ABA interaction events occurred. Cannot calculate a mean for this event.")
}

# Check if there are non-NA numeric values in BAB
if (any(!is.na(detailed_summary$BAB)) && any(sapply(detailed_summary$BAB, is.numeric))) {
  # Perform aggregation only if there are non-NA numeric values
  site_means_BAB <- aggregate(BAB ~ Site, data = detailed_summary, FUN = mean, na.rm = TRUE)
  # Adding means to site summary
  site_summary <- merge(site_summary, site_means_BAB, by = "Site", all.x = TRUE)
}

# Warning if there are NAs
if (!any(!is.na(detailed_summary$BAB)) && any(sapply(detailed_summary$BAB, is.numeric))){
  warning("No BAB interaction events occurred. Cannot calculate a mean for this event.")
}

# Site summary ratios
# Will only calculate ratios if there is a mean for AB, BA, AA, BB, ABA, BAB
if (
    any(!is.na(detailed_summary$AB)) && any(sapply(detailed_summary$AB, is.numeric)) &&
    any(!is.na(detailed_summary$BA)) && any(sapply(detailed_summary$BA, is.numeric)) ||

    any(!is.na(detailed_summary$AA)) && any(sapply(detailed_summary$AA, is.numeric)) &&
    any(!is.na(detailed_summary$BAB)) && any(sapply(detailed_summary$BAB, is.numeric)) ||

    any(!is.na(detailed_summary$BB)) && any(sapply(detailed_summary$BB, is.numeric)) &&
    any(!is.na(detailed_summary$ABA)) && any(sapply(detailed_summary$ABA, is.numeric))
    ) {

  # Calculate AB/BA, BA/AB, BAB/AA, ABA/BB ratios by site
  site_summary$AB_over_BA <- with(site_summary, AB / BA)
  site_summary$BA_over_AB <- with(site_summary, BA / AB)
  site_summary$BAB_over_AA <- with(site_summary, BAB / AA)
  site_summary$ABA_over_BB <- with(site_summary, ABA / BB)

  colnames(site_summary) <- c("Site", "AB", "BA", "AA", "BB", "ABA", "BAB",
                              "AB/BA", "BA/AB", "BAB/AA", "ABA/BB")

}

# Error message for when there is no mean for AB, BA, AA, BB, ABA, BAB
if (!any(!is.na(detailed_summary$AB)) && any(sapply(detailed_summary$AB, is.numeric)) ||
    !any(!is.na(detailed_summary$BA)) && any(sapply(detailed_summary$BA, is.numeric)) ||
    !any(!is.na(detailed_summary$AA)) && any(sapply(detailed_summary$AA, is.numeric)) ||
    !any(!is.na(detailed_summary$BB)) && any(sapply(detailed_summary$BB, is.numeric)) ||
    !any(!is.na(detailed_summary$ABA)) && any(sapply(detailed_summary$ABA, is.numeric)) ||
    !any(!is.na(detailed_summary$BAB)) && any(sapply(detailed_summary$BAB, is.numeric))) {

  # Warning message
  warning("Unable to calculate site summary for some or all Avoidance-Attraction Ratios due to lack of event occurances.")
}


# To not get scientific numbers in event_summery output
options(scipen = 999)

# Event summaries AB, BA, AA, BB, ABA, BAB
summary_AB <- as.matrix(summary(detailed_summary$AB))
summary_BA <- as.matrix(summary(detailed_summary$BA))
summary_AA <- as.matrix(summary(detailed_summary$AA))
summary_BB <- as.matrix(summary(detailed_summary$BB))
summary_ABA <- as.matrix(summary(detailed_summary$ABA))
summary_BAB <- as.matrix(summary(detailed_summary$BAB))

# Create a data frame with columns AB, BA, AA, BB, ABA, BAB
event_summary <- data.frame(
  AB = summary_AB[-7], # not including NA's
  BA = summary_BA[-7],
  AA = summary_AA[-7],
  BB = summary_BB[-7],
  ABA = summary_ABA[-7],
  BAB = summary_BAB[-7])

# Replace NaN with NA in each column
event_summary[] <- lapply(event_summary, function(x) replace(x, is.nan(x), NA))

# Renaming event_summary rows
rownames(event_summary) = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")

# Calculate the event counts
event_count_AB <- sum(!is.na(detailed_summary$AB))
event_count_BA <- sum(!is.na(detailed_summary$BA))
event_count_AA <- sum(!is.na(detailed_summary$AA))
event_count_BB <- sum(!is.na(detailed_summary$BB))
event_count_ABA <- sum(!is.na(detailed_summary$ABA))
event_count_BAB <- sum(!is.na(detailed_summary$BAB))

# Combining all event counts into a single output
event_counts <- c(AB = event_count_AB, BA = event_count_BA, AA = event_count_AA, BB = event_count_BB,
                  ABA = event_count_ABA, BAB = event_count_BAB)

# Total summary
total_summary <- colMeans(detailed_summary[, -c(1, 2)], na.rm = TRUE)

# Total summary ratios
# Will only calculate ratios if there is a mean for AB, BA, AA, BB, ABA, BAB
if (
  any(!is.na(detailed_summary$AB)) && any(sapply(detailed_summary$AB, is.numeric)) &&
  any(!is.na(detailed_summary$BA)) && any(sapply(detailed_summary$BA, is.numeric)) ||

  any(!is.na(detailed_summary$AA)) && any(sapply(detailed_summary$AA, is.numeric)) &&
  any(!is.na(detailed_summary$BAB)) && any(sapply(detailed_summary$BAB, is.numeric)) ||

  any(!is.na(detailed_summary$BB)) && any(sapply(detailed_summary$BB, is.numeric)) &&
  any(!is.na(detailed_summary$ABA)) && any(sapply(detailed_summary$ABA, is.numeric))

    ) {

  # Calculate T2/T1 and T4/T3 ratios from total means
  total_summary <- c(total_summary,
                     AB_over_BA = total_summary[1] / total_summary[2],
                     BA_over_AB = total_summary[2] / total_summary[1],
                     BAB_over_AA = total_summary[6] / total_summary[3],
                     ABA_over_BB = total_summary[5] / total_summary[4]
                     )

  # Renaming ratio columns
  names(total_summary)[7] <- "AB/BA"
  names(total_summary)[8] <- "BA/AB"
  names(total_summary)[9] <- "BAB/AA"
  names(total_summary)[10] <- "ABA/BB"

  # Replace NaN with NA
  total_summary <- replace(total_summary, is.nan(total_summary), NA)

}

# Error message for when there is no mean for AB, BA, AA, BB, ABA, BAB
if (!any(!is.na(total_summary[1])) && any(sapply(total_summary[1], is.numeric)) ||
      !any(!is.na(total_summary[2])) && any(sapply(total_summary[2], is.numeric)) ||
      !any(!is.na(total_summary[3])) && any(sapply(total_summary[3], is.numeric)) ||
      !any(!is.na(total_summary[4])) && any(sapply(total_summary[4], is.numeric))) {

  # Warning message
  warning("Unable to calculate total summary for some or all Avoidance-Attraction Ratios due to lack of event occurances.")

  # Replace NaN with NA
  total_summary <- replace(total_summary, is.nan(total_summary), NA)
}


# Combine results into a list
result_list <- list(total_summary = total_summary,event_count = event_counts, event_summary = event_summary,
                    site_summary = site_summary, detailed_summary = detailed_summary)

return(result_list)

}

