\name{spp_sum}
\alias{spp_sum}
\title{
Species Summary Table (spp\_sum)
}
\description{
The `spp\_sum` function generates a frequency table summarizing the number of observations for each unique species in a dataset.
}
\usage{
spp_sum(data, name_col)
}
\arguments{
  \item{data}{
    The dataset containing information on species observations.
  }
  \item{name\_col}{
    The column name indicating the species in the dataset.
  }
}
\value{
  A data frame containing two columns:
  \item{Name}{
    The unique species names.
  }
  \item{Observations}{
    The number of observations for each unique species.
  }
}
\examples{
spp_sum_test <- spp_sum(KScams_dat, name_col = "Common_name")
