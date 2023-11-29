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
    \item{\code{General_project}}{a character vector Project name}
    \item{\code{Class}}{a character vector Species class}
    \item{\code{Order}}{a character vector Species order}
    \item{\code{Family}}{a character vector Species family}
    \item{\code{Genus}}{a character vector Species genus}
    \item{\code{Species}}{a character vector Species specific epithet}
    \item{\code{Binomial}}{a character vector Species Scientific name}
    \item{\code{Common_name}}{a character vector Species common name}
    \item{\code{Year}}{a numeric vector Year of deployment}
    \item{\code{Site}}{a numeric vector Site ID}
    \item{\code{DateTime}}{a character vectorDate in M/D/Y and Time in H.M.S}
    \item{\code{Date}}{a character vector Date in M/D/Y}
    \item{\code{Time}}{a character vector Time in H.M.S}
    \item{\code{Timezone}}{a character vector Timezone study occured in}
    \item{\code{Latitude}}{a numeric vector Site latitude}
    \item{\code{Longitude}}{a numeric vector Site longitude}
    \item{\code{SetVisitDate}}{a character vector Camera deployment date}
  }
}

\source{
https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.3945~
}
\references{
Palomo‐Munoz, G., T. J. Werdel, C. W. Piper, M. S. Peek, A. M. Ricketts, and A. A. Ahlers. 2023. Spatiotemporal distributions of mammals occurring in an agro‐prairie ecosystem. Ecology 104:e3945
}
\examples{
data(KScams_dat)
}
\keyword{datasets}
