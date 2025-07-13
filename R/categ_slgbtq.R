#' Composite 2SLGBTQ+ Categorization
#'
#' Categorizes as 2SLGBTQ+ or non-2SLGBTQ+ based on gender, trans status, and sexual orientation.
#'
#' @param data A data frame
#' @param gender_col Gender column (should already be categorized)
#' @param trans_col Trans status column
#' @param orientation_col Sexual orientation column
#' @param output_col Output column name
#' @param labels Named list of labels
#' @return Data frame with a new column
#' @export
categ_slgbtq <- function(data, gender_col, trans_col, orientation_col,
                         output_col = "demographics_slgbtq",
                         labels = list(
                           slgbtq = "2SLGBTQ+",
                           non_slgbtq = "non-2SLGBTQ+",
                           prefer_not_to_say = "prefer not to say",
                           missing = NA)) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    g <- data[[gender_col]][i]
    t <- data[[trans_col]][i]
    o <- data[[orientation_col]][i]

    if (any(is.na(c(g, t, o))) || all(c(g, t, o) == "")) {
      data[[output_col]][i] <- labels$missing
    } else if (any(c(g, t, o) == labels$prefer_not_to_say)) {
      data[[output_col]][i] <- labels$prefer_not_to_say
    } else if (
      g == "gender diverse" ||
      t == "transgender" ||
      o == "2SLGBTQ+"
    ) {
      data[[output_col]][i] <- labels$slgbtq
    } else {
      data[[output_col]][i] <- labels$non_slgbtq
    }
  }
  return(data)
}
