
data("KScams_dat")
KScams_dat$DateTime  <- as.POSIXct(KScams_dat$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS") # this is very important step

# Example use
T1_test <- T1(data = KScams_dat, speciesA ="White-Tailed Deer", speciesB = "Coyote",
                species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
                unitTime = "hours")

# Example use
T2_test <- T2(data = KScams_dat, speciesA ="White-Tailed Deer", speciesB = "Coyote",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")

# Example use
T3_test <- T3(data = KScams_dat, speciesA ="White-Tailed Deer",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")

# Example use
T4_test <- T4(data = KScams_dat, speciesA = "White-Tailed Deer", speciesB = "Coyote",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")


# Example usage
AAR_test <- AAR(data = KScams_dat, speciesA = "White-Tailed Deer", speciesB = "Coyote",
                   species_col = "Common_name", datetime_col = "DateTime", site_col ="Site", unitTime = "hours")

# Example use
spp_sum(KScams_dat, name_col = "Common_name")
