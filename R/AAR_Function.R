

# Data
data("KScams")

# Convert the numeric timestamps to a POSIXct object (timestamps in seconds since the epoch)
KScams$DateTime  <- as.POSIXct(KScams$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS")

unique(KScams$Common_name)

data = KScams
site_col = "Site"

site = 1

datetime_col = "DateTime"
species_col = "Common_name"
species1 ="White-Tailed Deer"
species2 = "Coyote"


### -------------------------------
AAR <- function(data, species1, species2, datetime_col, site_col, species_col) {

  # Results dataframe
  detailed_result <- data.frame(Site = character(), Year = integer(), T1 = numeric(), T2 = numeric(), T3 = numeric(), T4 = numeric(), T2_over_T1 = numeric(), T4_over_T3 = numeric())

  # subsetting data by species given
  species_data <- data[data[[species_col]] == species1 | data[[species_col]] == species2, ]

  # Convert datetime to 24-hour clock
  species_data[[datetime_col]] <- as.POSIXct(species_data[[datetime_col]], format = "%Y-%m-%d %H:%M:%S")

  # Organizing data by site number
  species_data <- species_data[order(species_data[[site_col]], species_data[[datetime_col]]), ]

  # Subset data for the specific site
  unique_sites <- unique(species_data[[site_col]])

  # Iterate over all sites
  for (site in unique_sites) {

    # Subsetting data by iteration
    site_data <- species_data[species_data[[site_col]] == site, ]

    # Extract years for the site
    years <- unique(as.numeric(format(site_data[[datetime_col]], "%Y")))

    for (year in years) {

      # Subset data for the current year
      year_data <- site_data[format(site_data[[datetime_col]], "%Y") == as.character(year), ]

      # Sort data by time within each site
      year_data <- year_data[order(year_data[[datetime_col]]), ]


      # Identify indices of species1 and species2 detections
      species1_indices <- which(year_data[[species_col]] == species1)
      species2_indices <- which(year_data[[species_col]] == species2)

      # Temporary dataframe to collect T1, T2, T3, T4 values
      temp_result <- data.frame(T1 = numeric(), T2 = numeric(), T3 = numeric(), T4 = numeric())

      # Calculate T1, T2, T3, T4
      if (length(species1_indices) > 0) {
        i <- species1_indices[1]
        T1 <- 0
        T3 <- diff(as.numeric(format(year_data[[datetime_col]][species1_indices], "%H:%M:%S")))

        if (length(species2_indices) > 0) {
          # T1: Time from species 1 passage until species 2 is detected
          T1 <- as.numeric(difftime(year_data[[datetime_col]][species2_indices[1]], year_data[[datetime_col]][i], units = "secs"))

          if (length(species1_indices[species1_indices > i]) > 0) {
            # T2: Time from species 2 passage until another species 1 is detected
            next_species1_index <- min(species1_indices[species1_indices > i])
            T2 <- as.numeric(difftime(year_data[[datetime_col]][next_species1_index], year_data[[datetime_col]][species2_indices[1]], units = "secs"))

            # T4: Time from a detection of species 1 until the detection of species 2
            #     and then the time from the detection of species 2 until the detection of the next species 1
            T4 <- as.numeric(difftime(year_data[[datetime_col]][next_species1_index], year_data[[datetime_col]][i], units = "secs"))
          } else {
            # If there's no next species 1 detection, set T2 and T4 to NA
            T2 <- NA
            T4 <- NA
          }
        } else {
          # If there's no species 2 detection, set T2 and T4 to NA
          T2 <- NA
          T4 <- NA
        }

        # Store results in temporary dataframe
        temp_result <- rbind(temp_result, c(T1, T2, mean(as.numeric(T3)), mean(as.numeric(T4))))
      }

      # Calculate T2/T1 and T4/T3
      T2_over_T1 <- ifelse(!is.na(temp_result$T2[1]), mean(temp_result$T2) / temp_result$T1[1], NA)
      T4_over_T3 <- ifelse(length(temp_result$T3) > 0, mean(temp_result$T4) / temp_result$T3[1], NA)

      # Store final results for the site and year
      detailed_result <- rbind(detailed_result, c(site, year, temp_result$T1[1], temp_result$T2[1], temp_result$T3[1], temp_result$T4[1], T2_over_T1, T4_over_T3))
    }
  }

  # Rename columns for the detailed result
  colnames(detailed_result) <- c("Site", "Year", "T1", "T2", "T3", "T4", "T2_over_T1", "T4_over_T3")

  # Total summary
  total_summary <- colMeans(detailed_result[, -c(1, 2)], na.rm = TRUE)

  # Combine results into a list
  result_list <- list(total_summary = total_summary, detailed_result = detailed_result)

  return(result_list)
}

# Example usage
test_result <- AAR(data = KScams, species1 ="White-Tailed Deer", species2 = "Coyote",
                   species_col = "Common_name", datetime_col = "DateTime", site_col ="Site")



-811977

