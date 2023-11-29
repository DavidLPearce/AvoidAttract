\name{AAR}
\alias{AAR}
\title{
Avoidance-Attraction Ratios (AAR)
}
\description{
The AAR function analyzes camera trapping data to calculate the average time intervals for interactions involving species A and species B. It calculates the average time intervals T1 (AB), T2 (BA), T3 (AA), T4 (ABA), as well as the T2/T1 (BA/AB) and T4/T3 (ABA/AA) ratios for each site summarized across all years and provides the total summary across all sites and years.
}
\usage{
AAR(data, speciesA, speciesB, species_col, datetime_col, site_col, unitTime = "hours")
}
\arguments{
  \item{data}{
The camera trapping dataset containing information on species, datetime, and site.
}
  \item{speciesA}{
The first species in the interaction sequence (preceding species).
}
  \item{speciesB}{
The second species in the interaction sequence (succeeding species).
}
  \item{species_col}{
The column name indicating the species in the dataset.
}
  \item{datetime_col}{
The column name indicating the datetime of each detection.
}
  \item{site_col}{
The column name indicating the camera site.
}
  \item{unitTime}{
The unit of time used for calculating the time differences, default is "hours", options = c("secs", "mins", "hours").
}
}
\value{
  A list containing:
  \item{total_summary}{
    A summary of the mean values for T1, T2, T3, T4, T2/T1, and T4/T3 across all sites and years.
  }
  \item{event_count}{
    The total count of T1, T2, T3, and T4 events across all sites and years.
  }
  \item{site_summary}{
    A summary of the mean T1, T2, T3, T4, T2/T1, and T4/T3 values for each site across all years.
  }
}
\references{
Parsons, A. W., C. Bland, T. Forrester, M. C. Baker-Whatton, S. G. Schuttler, W. J. McShea, R. Costello, and R. Kays. 2016. The ecological impact of humans and dogs on wildlife in protected areas in eastern North America. Biological Conservation 203:75–88.
}
\source{
URL: \url{https://doi.org/10.1016/j.biocon.2016.09.001}
}
\seealso{
Naidoo, R., and A. C. Burton. 2020. Relative effects of recreational activities on a temperate terrestrial wildlife assemblage. Conservation Science and Practice 2:e271.

URL: \url{https://doi.org/10.1111/csp2.271}

}
\examples{
# Function Example. Note: DateTime column must be formated as a date time and not a character string!
AAR_example <- AAR(data = KScams_dat, speciesA = "White-Tailed Deer", speceisB = "Coyote",
                  species_col = "Common_name", datetime_col = "DateTime",
                  site_col = "Site", unitTime = "hours")
}
