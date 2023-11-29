# Example use
T1_test <- T1(data = KScams, species1 ="White-Tailed Deer", species2 = "Coyote",
                species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
                unitTime = "hours")

# Example use
T2_test <- T2(data = KScams, species1 ="White-Tailed Deer", species2 = "Coyote",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")

# Example use
T3_test <- T3(data = KScams, species1 ="White-Tailed Deer", species2 = "Coyote",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")

# Example use
T4_test <- T4(data = KScams, species1 ="White-Tailed Deer", species2 = "Coyote",
              species_col = "Common_name", datetime_col = "DateTime", site_col ="Site",
              unitTime = "hours")


# Example usage
AAR_test <- AAR(data = KScams, species1 = "American badger"  , species2 = "Coyote",
                   species_col = "Common_name", datetime_col = "DateTime", site_col ="Site", unitTime = "hours")

# Example use
spp_sum(KScams, name_col = "Common_name")
