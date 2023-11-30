
system.file(package='AvoidAttract')

data("KScams_dat")
KScams_dat$DateTime  <- as.POSIXct(KScams_dat$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS") # this is very important step


data = KScams_dat
speciesA ="White-Tailed Deer"
speciesB = "Coyote"
site_col = "Site"
datetime_col = "DateTime"
species_col = "Common_name"
unitTime = "hours"

site = 0
year = 2018
row = 12


