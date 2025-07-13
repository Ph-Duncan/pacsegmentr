#' Categorize FSA as Urban or Rural
#'
#' Uses Canadian postal code FSA to classify location as urban, rural, or invalid.
#'
#' @param data A data frame
#' @param fsa_col Column name with 3-character FSA
#' @param output_col Output column name
#' @param labels Named list of labels
#' @param warn Logical, whether to warn on invalid FSAs
#' @return Data frame with new location column
#' @export
categ_fsa_urban <- function(data, fsa_col, output_col = "demographics_location",
                            labels = list(
                              urban = "urban",
                              rural = "rural",
                              invalid = "invalid",
                              missing = NA),
                            warn = TRUE) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    fsa <- toupper(trimws(as.character(data[[fsa_col]][i])))
    if (is.na(fsa) || fsa == "") {
      data[[output_col]][i] <- labels$missing
    } else if (nchar(fsa) < 3 || !grepl("^[A-Z][0-9][A-Z]", fsa)) {
      data[[output_col]][i] <- labels$invalid
      if (warn) warning("Invalid FSA format at row ", i, ": ", fsa)
    } else {
      type <- substr(fsa, 2, 2)
      data[[output_col]][i] <- if (type == "0") labels$rural else labels$urban
    }
  }
  return(data)
}
