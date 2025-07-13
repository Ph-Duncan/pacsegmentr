#' Categorize Indigenous Identity
#'
#' Categorizes respondents as Indigenous, non-Indigenous, prefer not to say, or missing
#' based on select-all Indigenous identity indicators.
#'
#' @param data A data frame
#' @param indigenous_cols Character vector of columns for Indigenous identities
#' @param not_indigenous_col Optional column name for "No"
#' @param prefer_not_to_say_col Optional column name for "Prefer not to answer"
#' @param output_col Output column name
#' @param labels Named list of output labels
#' @param prioritize_indigenous If TRUE, Indigenous overrides other responses
#' @return Data frame with new column
#' @export
categ_indigenous <- function(data, indigenous_cols,
                             not_indigenous_col = NULL,
                             prefer_not_to_say_col = NULL,
                             output_col = "demographics_indigenous",
                             labels = list(
                               indigenous = "Indigenous",
                               not_indigenous = "non-Indigenous",
                               prefer_not_to_say = "prefer not to say",
                               missing = NA),
                             prioritize_indigenous = TRUE) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    if (!is.null(prefer_not_to_say_col) && isTRUE(row[[prefer_not_to_say_col]] %in% c(1, "yes", "Yes", TRUE))) {
      data[[output_col]][i] <- labels$prefer_not_to_say
    } else if (all(is.na(row[indigenous_cols]))) {
      data[[output_col]][i] <- labels$missing
    } else if (any(row[indigenous_cols] %in% c(1, "yes", "Yes", TRUE), na.rm = TRUE)) {
      data[[output_col]][i] <- labels$indigenous
      if (!is.null(not_indigenous_col) && isTRUE(row[[not_indigenous_col]] %in% c(1, "yes", "Yes", TRUE))) {
        warning("Row ", i, " has both Indigenous and non-Indigenous responses; prioritizing Indigenous.")
      }
    } else if (!is.null(not_indigenous_col) && isTRUE(row[[not_indigenous_col]] %in% c(1, "yes", "Yes", TRUE))) {
      data[[output_col]][i] <- labels$not_indigenous
    } else {
      data[[output_col]][i] <- labels$missing
    }
  }
  return(data)
}
