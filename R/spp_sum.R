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

# Example use
spp_sum(KScams, name_col = "Common_name")
