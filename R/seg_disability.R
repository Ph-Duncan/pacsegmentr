
#' Segment respondents by disability status
#'
#' This function classifies respondents based on responses to a set of disability-related questions,
#' using a flexible priority system across override, secondary, and tertiary columns.
#'
#' @param data A data frame.
#' @param disability_cols Character vector of columns indicating specific disabilities (used as tertiary inputs).
#' @param no_disability_cols Optional character vector for "none of the above" (used as secondary inputs).
#' @param prefer_not_to_say_cols Optional character vector for "prefer not to say" (used as override inputs).
#' @param labels A named list with output labels. Must contain:
#' \describe{
#'   \item{with_disability}{Label for respondents with at least one disability.}
#'   \item{without_disability}{Label for respondents who selected "none of the above" only.}
#'   \item{prefer_not_to_say}{Label for respondents who selected "prefer not to say".}
#'   \item{missing}{Label for blank or missing responses.}
#' }
#' @param coding A named list with `true` and `false` encodings. Defaults to common binary values.
#'
#' @return A character vector with one of the labels provided.
#' @export
seg_disability <- function(data,
                           disability_cols,
                           no_disability_cols = NULL,
                           prefer_not_to_say_cols = NULL,
                           labels = list(
                             with_disability = "with disability",
                             without_disability = "without disability",
                             prefer_not_to_say = "prefer not to say",
                             missing = NA_character_
                           ),
                           coding = list(
                             true = c("yes", "1", "true"),
                             false = c("no", "0", "false", "")
                           )) {

  sata_to_mec(
    data = data,
    override_cols = if (!is.null(prefer_not_to_say_cols)) {
      list(!!labels$prefer_not_to_say := prefer_not_to_say_cols)
    } else NULL,
    secondary_cols = if (!is.null(no_disability_cols)) {
      list(!!labels$without_disability := no_disability_cols)
    } else NULL,
    tertiary_cols = list(!!labels$with_disability := disability_cols),
    coding = coding,
    missing_label = labels$missing
  )
}
