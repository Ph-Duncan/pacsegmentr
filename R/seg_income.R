#' Segmentation by Income
#'
#' Segments respondents as lower or higher income based on thresholds and optional urban/rural classification.
#'
#' @param data A data frame
#' @param income_col Name of the income column (numeric or recoded)
#' @param location_col Optional column indicating urban/rural/invalid
#' @param output_col Name of the result column
#' @param threshold Overall threshold or named list for urban/rural
#' @param labels Named list of category labels
#' @return Data frame with new income group column
#' @export
seg_income <- function(data, income_col, location_col = NULL,
                         output_col = "demographics_income",
                         threshold = list(urban = 8, rural = 7),
                         labels = list(
                           lower = "lower income",
                           higher = "higher income",
                           prefer_not_to_say = "prefer not to say",
                           missing = NA)) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    inc <- data[[income_col]][i]
    loc <- if (!is.null(location_col)) data[[location_col]][i] else NA
    if (is.na(inc) || inc == "") {
      data[[output_col]][i] <- labels$missing
    } else if (inc == "prefer not to answer") {
      data[[output_col]][i] <- labels$prefer_not_to_say
    } else {
      inc_num <- suppressWarnings(as.numeric(inc))
      if (is.na(inc_num)) {
        data[[output_col]][i] <- labels$prefer_not_to_say
      } else {
        thres <- if (!is.null(location_col)) {
          if (loc == "urban") threshold$urban else if (loc == "rural") threshold$rural else NA
        } else {
          threshold
        }
        if (is.na(thres)) {
          warning("Invalid location or threshold for row ", i)
          data[[output_col]][i] <- labels$missing
        } else {
          data[[output_col]][i] <- ifelse(inc_num >= thres, labels$higher, labels$lower)
        }
      }
    }
  }
  return(data)
}
