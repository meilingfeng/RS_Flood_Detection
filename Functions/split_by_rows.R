# Function to split dataframe by row numbers
split_by_rows <- function(df, split_points) {
  result <- list()
  start <- 1
  for (end in split_points) {
    result[[length(result) + 1]] <- df[start:end, ]
    start <- end + 1
  }
  result
}