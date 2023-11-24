# Function is for calculating the time from species 2 until species 1 for all sites and all years within a dataframe
# The function will return the median of all T2 events within that year for that site,
# every time that an interaction occurred and the total summary


# Convert the numeric timestamps to a POSIXct object (timestamps in seconds since the epoch)
data("KScams")
KScams$DateTime  <- as.POSIXct(KScams$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS") # this is very important step

data = KScams
species1 ="White-Tailed Deer"
species2 = "Coyote"
site_col = "Site"
datetime_col = "DateTime"
species_col = "Common_name"
site = 1
year = 2018
row = 12


T2 <- function(data, species1, species2, species_col, datetime_col, site_col, unitTime) {

  # Check if required columns exist
  if (!(species_col %in% names(data) && datetime_col %in% names(data) && site_col %in% names(data))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }
