# Function is for calculating the time from species 1 until species 2 for all sites and all years within a dataframe
# The function will return the median of all T1 events within that year for that site

# Data

data = KScams
site_col = "Site"
datetime_col = "DateTime"
species_col = "Common_name"
species1 ="White-Tailed Deer"
species2 = "Coyote"

site = 1
year = 2018
row = 1


T1 <- function(data, species1, species2, datetime_col, site_col, species_col) {

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

      # Interactions
      for (row in 1:nrow(year_data)) {
      current_species <- year_data[[species_col]][row]
      next_species <- year_data[[species_col]][row + 1]

        if (current_species == species1 && next_species == species2) {
              # Species 1 detection followed by Species 2
              current_time <- year_data[[datetime_col]][row]
              next_time <- site_data$DateTime[i + 1])

              # Calculate the time difference
              time_difference <- difftime(next_time, current_time, units = "secs")

              #
        }
      }



      temp_result <- data.frame(Site = character(), Year = integer(), T1 = numeric())



