
#' Segment respondents by racialized identity
#'
#' Classifies respondents based on racialized identity using a select-all-that-apply structure.
#' Supports prioritization via override, secondary, and tertiary columns.
#'
#' @param data A data frame.
#' @param racialized_cols Character vector of columns indicating racialized identities.
#' @param not_racialized_cols Optional character vector indicating non-racialized selections.
#' @param prefer_not_to_say_cols Optional character vector for "prefer not to say".
#' @param labels A named list with labels: `racialized`, `not_racialized`, `prefer_not_to_say`, `missing`.
#' @param coding A named list with `true` and `false` values for interpreting responses.
#'
#' @return A character vector with the segmentation result.
#' @export
seg_racialized <- function(data,
                           racialized_cols,
                           not_racialized_cols = NULL,
                           prefer_not_to_say_cols = NULL,
                           labels = list(
                             racialized = "racialized",
                             not_racialized = "not racialized",
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
    secondary_cols = if (!is.null(not_racialized_cols)) {
      list(!!labels$not_racialized := not_racialized_cols)
    } else NULL,
    tertiary_cols = list(!!labels$racialized := racialized_cols),
    coding = coding,
    missing_label = labels$missing
  )
}
