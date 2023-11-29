\name{T4}
\alias{T4}
\title{
Calculating the time between detections of species A to species A with a detection of species B inbetween
}
\description{
The T4 (ABA) function analyzes camera trapping data to calculate the time between detections of species A, with a detection of species B inbetween the detections of species A at each camera site for all years. The function returns the average of all T4 events for all sites and across all years, the number of times the event occured, a site summary across all years and a detailed summary of each time the event occured.
}
\usage{
T4(data, speciesA, speciesB, species_col, datetime_col, site_col, unitTime = "hours")
}
\arguments{
  \item{data}{
The camera trapping dataset containing information on species, datetime, and site.
}
  \item{speciesA}{
The first species in the interaction sequence (preceding and succeeding species).
}
  \item{speciesB}{
The second species in the interaction sequence (intervening species).
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
    A summary of the mean values for T4 across all sites and years.
  }
  \item{event_count}{
    The total count of T4 events across all sites and years.
  }
  \item{site_summary}{
    A summary of the mean T4 values for each site across all years.
  }
  \item{detailed_summary}{
    Detailed information on T4 events, including site, year, and time differences.
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
T4_example <- T4(data = KScams_dat, speciesA = "White-Tailed Deer", speceisB = "Coyote",
                  species_col = "Common_name", datetime_col = "DateTime",
                  site_col = "Site", unitTime = "hours")
}
