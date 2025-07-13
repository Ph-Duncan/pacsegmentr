#' Categorize Sexual Orientation
#'
#' Categorizes select-all sexual orientation responses into straight, 2SLGBTQ+, or prefer not to say.
#'
#' @param data A data frame
#' @param orientation_cols Character vector of columns
#' @param straight_col Column representing "Straight"
#' @param prefer_not_to_say_col Optional column
#' @param output_col Output column name
#' @param labels Named list of labels
#' @return Data frame with a new column
#' @export
categ_sexual_orientation <- function(data, orientation_cols, straight_col,
                                     prefer_not_to_say_col = NULL,
                                     output_col = "demographics_sexual_orientation",
                                     labels = list(
                                       slgbtq = "2SLGBTQ+",
                                       non_slgbtq = "Straight",
                                       prefer_not_to_say = "prefer not to say",
                                       missing = NA)) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    if (!is.null(prefer_not_to_say_col) && isTRUE(row[[prefer_not_to_say_col]] %in% c(1, "yes", "Yes", TRUE))) {
      data[[output_col]][i] <- labels$prefer_not_to_say
    } else if (isTRUE(row[[straight_col]] %in% c(1, "yes", "Yes", TRUE)) &&
               all(row[setdiff(orientation_cols, straight_col)] %in% c(0, "no", "No", NA))) {
      data[[output_col]][i] <- labels$non_slgbtq
    } else if (any(row[orientation_cols] %in% c(1, "yes", "Yes", TRUE), na.rm = TRUE)) {
      data[[output_col]][i] <- labels$slgbtq
    } else {
      data[[output_col]][i] <- labels$missing
    }
  }
  return(data)
}
