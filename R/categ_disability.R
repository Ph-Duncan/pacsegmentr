#' Categorize Disability Status
#'
#' Categorizes respondents as with or without disability based on a set of binary or textual indicators.
#'
#' @param data A data frame
#' @param disability_cols Character vector of disability indicators
#' @param no_disability_col Column representing "None of the above"
#' @param prefer_not_to_say_col Optional column
#' @param output_col Output column name
#' @param labels Named list of output labels
#' @return Data frame with new column
#' @export
categ_disability <- function(data, disability_cols, no_disability_col,
                             prefer_not_to_say_col = NULL,
                             output_col = "demographics_disability",
                             labels = list(
                               with_disability = "with disability",
                               without_disability = "without disability",
                               prefer_not_to_say = "prefer not to say",
                               missing = NA)) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    selected <- row[disability_cols]
    if (!is.null(prefer_not_to_say_col) && isTRUE(row[[prefer_not_to_say_col]] %in% c(1, "yes", "Yes", TRUE))) {
      data[[output_col]][i] <- labels$prefer_not_to_say
    } else if (isTRUE(row[[no_disability_col]] %in% c(1, "yes", "Yes", TRUE)) &&
               all(selected %in% c(0, "no", "No", NA))) {
      data[[output_col]][i] <- labels$without_disability
    } else if (any(selected %in% c(1, "yes", "Yes", TRUE), na.rm = TRUE)) {
      data[[output_col]][i] <- labels$with_disability
    } else {
      data[[output_col]][i] <- labels$missing
    }
  }
  return(data)
}
