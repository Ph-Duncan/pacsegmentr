#' Categorize Newcomer Status
#'
#' Categorizes respondents as Canadian born, foreign born, newcomer, or prefer not to say.
#'
#' @param data A data frame
#' @param newcomer_col Column name
#' @param output_col Output column name
#' @param mapping Optional mapping table
#' @param labels Named list of output labels
#' @return Data frame with newcomer status
#' @export
categ_newcomer <- function(data, newcomer_col, output_col = "demographics_newcomer",
                           mapping = NULL,
                           labels = list(
                             canadian_born = "Canadian born",
                             foreign_born = "foreign born",
                             newcomer = "newcomer",
                             prefer_not_to_say = "prefer not to say",
                             missing = NA)) {
  if (is.null(mapping)) {
    mapping <- get_default_mapping("newcomer")
  }
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    val <- data[[newcomer_col]][i]
    if (is.na(val) || val == "") {
      data[[output_col]][i] <- labels$missing
    } else {
      match_raw <- mapping$raw == val
      match_code <- suppressWarnings(as.numeric(val)) == mapping$code
      idx <- which(match_raw | match_code)
      if (length(idx) == 1) {
        if (idx == 1) {
          data[[output_col]][i] <- labels$canadian_born
        } else if (idx == 2) {
          data[[output_col]][i] <- labels$foreign_born
        } else if (idx %in% 3:5) {
          data[[output_col]][i] <- labels$newcomer
        } else {
          data[[output_col]][i] <- labels$prefer_not_to_say
        }
        if (match_code && is.character(val)) {
          warning("Assumed numeric encoding for row ", i)
        }
      } else {
        data[[output_col]][i] <- labels$prefer_not_to_say
      }
    }
  }
  return(data)
}
