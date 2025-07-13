#' Categorize Numeric Age
#'
#' Categorizes respondents into children/youth, adults, or older adults using numeric input.
#'
#' @param data A data frame
#' @param age_col The column name for age
#' @param output_col Output column name
#' @param prefer_not_to_say_values Optional list of known encodings
#' @param warn If TRUE, warns about numeric values < 15
#' @param labels Named list of output labels
#' @return Data frame with new age category column
#' @export
categ_age_numeric <- function(data, age_col, output_col = "demographics_age_group",
                              prefer_not_to_say_values = NULL,
                              warn = TRUE,
                              labels = list(
                                youth = "children & youth",
                                adult = "adult",
                                older = "older adult",
                                prefer_not_to_say = "prefer not to say",
                                missing = NA)) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    val <- data[[age_col]][i]
    if (is.na(val) || val == "") {
      data[[output_col]][i] <- labels$missing
    } else if (!is.null(prefer_not_to_say_values) && val %in% prefer_not_to_say_values) {
      data[[output_col]][i] <- labels$prefer_not_to_say
    } else {
      num <- suppressWarnings(as.numeric(val))
      if (is.na(num)) {
        data[[output_col]][i] <- labels$prefer_not_to_say
      } else if (num < 0) {
        data[[output_col]][i] <- labels$missing
      } else if (num <= 17) {
        data[[output_col]][i] <- labels$youth
        if (warn && num < 15) warning("Unusually young respondent: age = ", num)
      } else if (num < 65) {
        data[[output_col]][i] <- labels$adult
      } else {
        data[[output_col]][i] <- labels$older
      }
    }
  }
  return(data)
}
