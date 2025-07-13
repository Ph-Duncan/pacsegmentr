#' Categorize Transgender Status
#'
#' Categorizes respondents as transgender, cisgender, prefer not to say, or missing.
#'
#' @param data A data frame
#' @param trans_col The name of the column with trans status
#' @param output_col Name of the output column
#' @param labels Named list of output labels
#' @return Data frame with a new column
#' @export
categ_trans_status <- function(data, trans_col, output_col = "demographics_trans_status",
                               labels = list(
                                 transgender = "transgender",
                                 cisgender = "cisgender",
                                 prefer_not_to_say = "prefer not to say",
                                 missing = NA)) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    val <- tolower(as.character(data[[trans_col]][i]))
    if (is.na(val) || val == "") {
      data[[output_col]][i] <- labels$missing
    } else if (val %in% c("yes", "y", "true", "1")) {
      data[[output_col]][i] <- labels$transgender
    } else if (val %in% c("no", "n", "false", "0")) {
      data[[output_col]][i] <- labels$cisgender
    } else if (val %in% c("prefer not to answer", "prefer not to say")) {
      data[[output_col]][i] <- labels$prefer_not_to_say
    } else {
      data[[output_col]][i] <- labels$prefer_not_to_say
    }
  }
  return(data)
}
