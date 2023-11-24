
# Data
data("KScams")
head(KScams)

# Convert the numeric timestamps to a POSIXct object (timestamps in seconds since the epoch)
KScams$DateTime  <- as.POSIXct(KScams$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS")


# Sort the data frame by site name, species name, year, and date-time
KScams <- KScams[order(KScams$Site, KScams$Year, KScams$DateTime),]




#########
#########
######## Time Between Species
unique(KScams$Common_name)

# Define the two target species
species_1 <- "White-Tailed Deer"
species_2 <- "Coyote"

# Filter data for species 1 and species 2 and combine them
KScams_sub <- KScams[which(KScams$Common_name == species_1 | KScams$Common_name == species_2),]

# Sort the data frame by site name, species name, year, and date-time
KScams_sub <- KScams_sub[order(KScams_sub$Site, KScams_sub$DateTime),]

# Create a new data frame to store the results
result_data <- data.frame(Site_Name = character(),
                          Year = integer(),
                          Begin_Time_Date = character(),  # New column to store the Begin_Time_Date of the interaction
                          TimeBetweenDetections = double(),
                          stringsAsFactors = FALSE)






#### Species summary by site total dataset


# Loop through each site of the combined species data
for (site in unique(KScams_sub$Site)) {

  # Subset the data for the current site
  site_data <- KScams_sub[which(KScams_sub$Site == site),]

  # Organizing site information by site and time
  site_data <- site_data[order(site_data$Site, site_data$DateTime),]

  # Warning if 1 or fewer observations for a site are entered
  if (nrow(site_data <= 1) == TRUE){
    # Skip that site and go to the next site
    warning("At least one site only has one or fewer observations for species presented. Will not be able to report on that site.")
    next
  }
  if (nrow(site_data >= 1) == TRUE){
  # Loop through the data for each site to find consecutive detections
  for (i in 1:(nrow(site_data) - 1)) {

    current_species <- site_data$Common_name[i]
    next_species <- site_data$Common_name[i + 1]
    current_year <- site_data$Year[i]
    next_year <- site_data$Year[i + 1]

    if (current_species == species_1 && next_species == species_2  && current_year == next_year) {
      # Species 1 detection followed by Species 2 detection in the same year and same site
      current_time <- site_data$DateTime[i]
      next_time <- site_data$DateTime[i + 1]

      # Calculate the time difference
      time_difference <- difftime(next_time, current_time, units = "mins")

      # Get the year of the interaction
      year <- site_data$Year[i]

      # Store the information in the result_data data frame
      result_data <- rbind(result_data, data.frame(Site_Name = site,
                                                   Year = year,
                                                   Begin_Time_Date = current_time,
                                                   TimeBetweenDetections = time_difference))
    }
  }
}
}


# Print the result
print(result_data)
# Calculate the average time between detections
spp1_spp2 <- mean(result_data$TimeBetweenDetections)

spp1_spp2


# T1 = Time Prey > Pred





# T2 = Time Pred > Prey
# T3 = Time Prey > Prey no pred imbetween
# T4 = Time Prey > Pred > Prey (T1 and T2 )

