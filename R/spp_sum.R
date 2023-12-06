#' Species Summary Table
#'
#' Function generates a frequency table summarizing the number of observations for each unique species in a dataset.
#'
#' @param data The dataset containing information on species observations (dataframe).
#' @param name_col The column name indicating the species in the dataset (character).
#'
#' @return A data frame containing two columns:
#'   \describe{
#'     \item{Name}{The unique species names.}
#'     \item{Observations}{The number of observations for each unique species.}
#'   }
#'
#' @examples
#' spp_sum_ex <- spp_sum(KScams_dat, name_col = "Common_name")
#'
#' @export
spp_sum <- function(data, name_col){

  # Check if the specified column exists in the dataframe
  if (!(name_col %in% names(data))) {
    stop("The specified column does not exist in the dataframe.")
  }

  # Count the number of observations for each unique name
  observation_counts <- table(data[[name_col]])

  # Convert the result to a data frame
  result <- data.frame(Name = names(observation_counts), Observations = as.numeric(observation_counts))

  return(result)
}
