\name{KScams_dat}
\alias{KScams_dat}
\docType{data}
\title{
KScams_dat
}
\description{
Camera trapping data for a research project in West Kansas
}
\usage{data("KScams_dat")}
\format{
  A data frame with 31226 observations on the following 17 variables.
  \describe{
    \item{\code{General_project}}{Project name (a character vector)}
    \item{\code{Class}}{Species class (a character vector)}
    \item{\code{Order}}{Species order (a character vector) }
    \item{\code{Family}}{Species family (a character vector) }
    \item{\code{Genus}}{Species genus (a character vector) }
    \item{\code{Species}}{Species specific epithet (a character vector) }
    \item{\code{Binomial}}{Species Scientific name (a character vector) }
    \item{\code{Common_name}}{Species common name (a character vector) }
    \item{\code{Year}}{Year of deployment (a numeric vector) }
    \item{\code{Site}}{Site number (a numeric vector) }
    \item{\code{DateTime}}{Date in M/D/Y and Time in H.M.S (a character vector) }
    \item{\code{Date}}{Date in M/D/Y (a character vector) }
    \item{\code{Time}}{Time in H.M.S (a character vector) }
    \item{\code{Timezone}}{Timezone study occured in (a character vector) }
    \item{\code{Latitude}}{Site latitude (a numeric vector) }
    \item{\code{Longitude}}{Site longitude (a numeric vector) }
    \item{\code{SetVisitDate}}{Camera deployment date (a character vector)}
  }
}

\source{
URL: https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.3945
}
\references{
Palomo‐Munoz, G., T. J. Werdel, C. W. Piper, M. S. Peek, A. M. Ricketts, and A. A. Ahlers. 2023. Spatiotemporal distributions of mammals occurring in an agro‐prairie ecosystem. Ecology 104:e3945
}
\examples{
data(KScams_dat)
}
\keyword{datasets}
