#' Categorize Racialized Identity
#'
#' Categorizes respondents as racialized, non-racialized, prefer not to say, or missing
#' based on responses to a set of select-all-that-apply race variables.
#'
#' @param data A data frame
#' @param race_cols Character vector of race column names
#' @param white_col Name of the column representing "White"
#' @param prefer_not_to_say_col Optional column name for "Prefer not to say"
#' @param output_col Name of the output column. Default is "demographics_racialized"
#' @param labels Named list of output labels
#' @return The original data frame with a new column
#' @export
categ_racialized <- function(data, race_cols, white_col, prefer_not_to_say_col = NULL,
                             output_col = "demographics_racialized",
                             labels = list(
                               racialized = "racialized",
                               non_racialized = "non-racialized",
                               prefer_not_to_say = "prefer not to say",
                               missing = NA)) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    if (!is.null(prefer_not_to_say_col) && isTRUE(row[[prefer_not_to_say_col]] %in% c(1, "yes", "Yes", TRUE))) {
      data[[output_col]][i] <- labels$prefer_not_to_say
    } else if (all(is.na(row[race_cols]))) {
      data[[output_col]][i] <- labels$missing
    } else if (isTRUE(row[[white_col]] %in% c(1, "yes", "Yes", TRUE)) &&
               all(row[setdiff(race_cols, white_col)] %in% c(0, "no", "No", NA))) {
      data[[output_col]][i] <- labels$non_racialized
    } else if (any(row[race_cols] %in% c(1, "yes", "Yes", TRUE), na.rm = TRUE)) {
      data[[output_col]][i] <- labels$racialized
    } else {
      data[[output_col]][i] <- labels$missing
    }
  }
  return(data)
}
