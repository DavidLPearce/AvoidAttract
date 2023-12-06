#' Species Summary Table by Year
#'
#' Function generates a list summarizing the number of observations for each unique species in a dataset by year.
#'
#' @param data The dataset containing information on species observations (dataframe).
#' @param name_col The column name indicating the species in the dataset (character).
#' @param year_col The column name indicating the year in the dataset (character).
#'
#' @return A list of dataframes where each element corresponds to a unique year and contains a data frame with three columns:
#'   \describe{
#'     \item{Name}{The unique species names.}
#'     \item{Observations}{The number of observations for each unique species in that year.}
#'   }
#'
#' @examples
#' spp_sumYear_ex <- spp_sumYear(KScams_dat, name_col = "Common_name", year_col = "Year")
#'
#' @export
spp_sumYear <- function(data, name_col, year_col){

  # Check if the specified columns exist in the dataframe
  if (!(name_col %in% names(data)) || !(year_col %in% names(data))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }

  # Get unique years in the dataset
  unique_years <- sort(unique(data[[year_col]]))

  # Initialize a list to store results for each year
  result_list <- list()

  # Loop through each year
  for (year in unique_years) {
    # Subset data for the current year
    year_data <- data[data[[year_col]] == year, ]

    # Count the number of observations for each unique name in the current year
    observation_counts <- table(year_data[[name_col]])

    # Create a data frame with species and observations for the current year
    result_df <- data.frame(Name = names(observation_counts), Observations = as.numeric(observation_counts))

    # Add the data frame to the list
    result_list[[as.character(year)]] <- result_df
  }

  return(result_list)
}
