#' Segments Respondents as Indigenous, Not Indigenous, or Prefer Not to Answer
#'
#' Converts a set of columns representing SATA (select-all-that-apply) question data corresponding 
#' to Indigenous identities into a single variable of MECs (mutually exclusive category) using the 
#' \code{sata_to_mecp()} function. This wrapper is designed for implementing ParticipACTION's demographic 
#' segmentation strategy for classifying respondents as Indigenous vs Not Indigenous.
#'
#' @param data A data frame.
#' @param indigenous_cols Columns indicating Indigenous identity. Supports tidyselect syntax.
#' @param not_indigenous_cols Optional. Columns indicating not Indigenous.
#' @param prefer_not_cols Optional. Columns indicating "prefer not to answer".
#' @param indigenous_label Label for respondents identified as Indigenous.
#' @param not_indigenous_label Label for respondents not identified as Indigenous.
#' @param prefer_not_label Label for respondents who prefer not to answer.
#' @param ... Additional arguments passed to \code{sata_to_mecp()}, such as \code{coding} or \code{missing_label}.
#'
#' @return A character vector of mutually exclusive classifications.
#' @export
seg_indigenous <- function(data,
                           indigenous_cols,
                           not_indigenous_cols = NULL,
                           prefer_not_cols = NULL,
                           indigenous_label = "Indigenous",
                           not_indigenous_label = "Not Indigenous",
                           prefer_not_label = "Prefer Not to Answer",
                           ...) {
  # Always warn the user
  warning("Users are encouraged to consult the TCPS2.0 and expert resources on Indigenous data sovereignty & stewardship before using or reporting Indigenous data.", call. = FALSE)
  
  #use a tidyverse approach to allow users to specify columns in a variety of ways
  indigenous <- tidyselect::eval_select(rlang::enquo(indigenous_cols), data = data) |> names()
  not_indigenous <- tidyselect::eval_select(rlang::enquo(not_indigenous_cols), data = data) |> names()
  prefer_not <- tidyselect::eval_select(rlang::enquo(prefer_not_cols), data = data) |> names()
  
  #coerce inputs into a named list for sata_to_mecp
  groups <- list()
  if (length(indigenous) > 0) groups[[indigenous_label]] <- indigenous
  if (length(not_indigenous) > 0) groups[[not_indigenous_label]] <- not_indigenous
  if (length(prefer_not) > 0) groups[[prefer_not_label]] <- prefer_not
  
  #coerce inputs into a named list for sata_to_mecp
  sata_to_mecp(
    data = data,
    groups = groups,
    ...
  )
}