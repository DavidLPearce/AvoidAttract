# List all functions in the package's namespace
all_functions <- ls("package:AvoidAttract", all.names = TRUE)
all_functions

?KScams_dat
?KScams_cov
?KScams_doy


data("KScams_dat")
KScams_dat$DateTime  <- as.POSIXct(KScams_dat$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS") # this is very important step

# Example use
?T1

T1_test <- T1(data = KScams_dat, speciesA ="White-Tailed Deer", speciesB = "Coyote",
                species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
                unitTime = "hours")

# Example use
?T2

T2_test <- T2(data = KScams_dat, speciesA ="White-Tailed Deer", speciesB = "Coyote",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")

# Example use
?T3

T3_test <- T3(data = KScams_dat, speciesA ="White-Tailed Deer",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")

# Example use
?T4

T4_test <- T4(data = KScams_dat, speciesA = "White-Tailed Deer", speciesB = "Coyote",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")


# Example usage
?AAR

AAR_test <- AAR(data = KScams_dat, speciesA = "White-Tailed Deer", speciesB = "Coyote",
                   species_col = "Common_name", datetime_col = "DateTime", site_col ="Site", unitTime = "hours")

# Example use
?spp_sum

spp_sum_test <- spp_sum(KScams_dat, name_col = "Common_name")
