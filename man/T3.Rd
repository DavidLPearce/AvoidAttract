\name{T3}
\alias{T3}
\title{
Analyzing Temporal Patterns in Camera Trapping Data (T3 also known as AA)
}
\description{
The T3 function analyzes camera trapping data to evaluate the temporal patterns of successive detections of a specific species (speciesA) at different camera sites. The primary focus is on calculating the average time interval (T3 or AA) between consecutive detections of the target species. This metric is vital for understanding the natural behavior and activity patterns of the species, especially when no other species intervenes between successive detections.
}
\usage{
T3(data, speciesA, species_col, datetime_col, site_col, unitTime = "hours")
}
\arguments{
  \item{data}{
The camera trapping dataset containing information on species, datetime, and site.
}
  \item{speciesA}{
he target species for which T3 events are analyzed.
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
    A summary of the mean values for T3 across all sites and years.
  }
  \item{event_count}{
    The total count of T3 events across all sites and years.
  }
  \item{site_summary}{
    A summary of the mean T3 values for each site across all years.
  }
  \item{detailed_summary}{
    Detailed information on T3 events, including site, year, and time differences.
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
T3_example <- T3(data = KScams_dat, speciesA ="White-Tailed Deer",
                  species_col = "Common_name", datetime_col = "DateTime",
                  site_col = "Site", unitTime = "hours")
}
