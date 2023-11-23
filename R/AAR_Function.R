
# Load required libraries
library(dplyr)
library(lubridate)


# Data
data("KScams")

# Convert the numeric timestamps to a POSIXct object (timestamps in seconds since the epoch)
KScams$DateTime  <- as.POSIXct(KScams$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS")

unique(KScams$Common_name)

# Define the two target species
species_1 <- "White-Tailed Deer"
species_2 <- "Coyote"


### -------------------------------
calculate_time_differences <- function(data, prey_species, predator_species, datetime_col, site_col, species_col) {
  # Subset data for the specific site
  unique_sites <- unique(data[[site_col]])

  result <- data.frame(site = character(), T1 = numeric(), T2 = numeric(), T3 = numeric(), T4 = numeric(), T2_over_T1 = numeric(), T4_over_T3 = numeric())

  for (site in unique_sites) {
    site_data <- data[data[[site_col]] == site, ]

    # Subset data for prey and predator species
    preys <- site_data[site_data[[species_col]] == prey_species, ]
    predators <- site_data[site_data[[species_col]] == predator_species, ]

    if (nrow(preys) <= 1 || nrow(predators) <= 1) {
      # Skip calculations if there are 1 or fewer observations
      result <- rbind(result, c(site, NA, NA, NA, NA, NA, NA))
    } else {
      # Calculate time differences
      T1 <- as.numeric(predators[[datetime_col]][1] - preys[[datetime_col]])
      T2 <- as.numeric(preys[[datetime_col]] - predators[[datetime_col]][length(predators[[datetime_col]])])
      T3 <- diff(as.numeric(preys[[datetime_col]]))
      T4 <- c(diff(as.numeric(predators[[datetime_col]])), NA)

      T1_average <- mean(T1, na.rm = TRUE)
      T2_average <- mean(T2, na.rm = TRUE)
      T3_average <- mean(T3, na.rm = TRUE)
      T4_average <- mean(T4, na.rm = TRUE)
      T2_over_T1 <- T2_average / T1_average
      T4_over_T3 <- T4_average / T3_average

      # Store results for the site
      result <- rbind(result, c(site, T1_average, T2_average, T3_average, T4_average, T2_over_T1, T4_over_T3))
    }
  }

  # Total summary
  total_summary <- colMeans(result[, -1], na.rm = TRUE)
  total_summary <- c("Total", total_summary)

  # Combine results
  result <- rbind(result, total_summary)

  return(result)
}

# Example usage
result <- calculate_time_differences(data = KScams, prey_species ="White-Tailed Deer", predator_species = "Coyote",
                                     species_col = "Common_name", datetime_col = "DateTime", site_col ="Site")

KScams
