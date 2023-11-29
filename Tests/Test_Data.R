
system.file(package='AvoidAttract')

data("KScams")
KScams$DateTime  <- as.POSIXct(KScams$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS") # this is very important step


# data = KScams
# species1 ="White-Tailed Deer"
# species2 = "Coyote"
# site_col = "Site"
# datetime_col = "DateTime"
# species_col = "Common_name"
# unitTime = "hours"
#
# site = 0
# year = 2020
# row = 1
