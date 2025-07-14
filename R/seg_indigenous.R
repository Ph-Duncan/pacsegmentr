
#' Segment respondents by Indigenous identity
#'
#' Classifies respondents based on Indigenous identity using a select-all-that-apply structure.
#' Uses explicit override, secondary, and tertiary column prioritization.
#'
#' @param data A data frame.
#' @param indigenous_cols Character vector of columns indicating Indigenous identities.
#' @param not_indigenous_cols Optional character vector indicating non-Indigenous selections.
#' @param prefer_not_to_say_cols Optional character vector for "prefer not to say".
#' @param labels A named list with labels: `indigenous`, `not_indigenous`, `prefer_not_to_say`, `missing`.
#' @param coding A named list with `true` and `false` values for interpreting responses.
#'
#' @return A character vector with the segmentation result.
#' @export
seg_indigenous <- function(data,
                           indigenous_cols,
                           not_indigenous_cols = NULL,
                           prefer_not_to_say_cols = NULL,
                           labels = list(
                             indigenous = "Indigenous",
                             not_indigenous = "non-Indigenous",
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
    secondary_cols = if (!is.null(not_indigenous_cols)) {
      list(!!labels$not_indigenous := not_indigenous_cols)
    } else NULL,
    tertiary_cols = list(!!labels$indigenous := indigenous_cols),
    coding = coding,
    missing_label = labels$missing
  )
}
