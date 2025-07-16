#' Segments Respondents as Racialized, Not Racialized, or Prefer Not to Answer
#'
#' Converts a set of columns representing SATA (select-all-that-apply) question data corresponding 
#' to race/ethnicity into a single variable of MECs (mutually exclusive category) using the 
#' \code{sata_to_mecp()} function. This wrapper is designed for implementing ParticipACTION's demographic 
#' segmentation strategy for classifying respondents as Racialized or Not-Racialized.
#'
#' @param data A data frame.
#' @param racialized_cols Columns indicating racialized identity. Supports tidyselect syntax.
#' @param not_racialized_cols Optional. Columns indicating not racialized.
#' @param prefer_not_cols Optional. Columns indicating "prefer not to answer".
#' @param racialized_label Label for respondents identified as racialized.
#' @param not_racialized_label Label for respondents not identified as racialized.
#' @param prefer_not_label Label for respondents who prefer not to answer.
#' @param ... Additional arguments passed to \code{sata_to_mecp()}, such as \code{coding} or \code{missing_label}.
#'
#' @return A character vector of mutually exclusive classifications.
#' @export
seg_racialized <- function(data,
                           racialized_cols,
                           not_racialized_cols = NULL,
                           prefer_not_cols = NULL,
                           racialized_label = "Racialized",
                           not_racialized_label = "Not Racialized",
                           prefer_not_label = "Prefer Not to Answer",
                           ...) {
  #use a tidyverse approach to allow users to specify columns in a variety of ways
  racialized <- tidyselect::eval_select(rlang::enquo(racialized_cols), data = data) |> names()
  not_racialized <- tidyselect::eval_select(rlang::enquo(not_racialized_cols), data = data) |> names()
  prefer_not <- tidyselect::eval_select(rlang::enquo(prefer_not_cols), data = data) |> names()
  
  #coerce inputs into a named list for sata_to_mecp
  groups <- list()
  if (length(racialized) > 0) groups[[racialized_label]] <- racialized
  if (length(not_racialized) > 0) groups[[not_racialized_label]] <- not_racialized
  if (length(prefer_not) > 0) groups[[prefer_not_label]] <- prefer_not
  
  #submit inputs to sata_to_mecp
  sata_to_mecp(
    data = data,
    groups = groups,
    ...
  )
}
