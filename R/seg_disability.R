
#' Classify Respondents as With Disability, No Disabitity, or Prefer Not to Answer
#'
#' Converts a set of columns representing SATA (select-all-that-apply) question data corresponding 
#' to disabilities/difficulties into a single variable of MECs (mutually exclusive category) using the 
#' \code{sata_to_mecp()} function. This wrapper is designed for implementing ParticipACTION's demographic 
#' segmentation strategy for classifying respondents as being a person With Disability vs a person with No Disability.
#' 
#' @param data A data frame.
#' @param disability_cols Columns indicating disabilities or difficulties. Supports tidyselect syntax.
#' @param no_disability_cols Optional. Columns indicating no disability.
#' @param prefer_not_cols Optional. Columns indicating "prefer not to answer".
#' @param disability_label Label for respondents identified as disabled.
#' @param no_disability_label Label for respondents not identified as disabled.
#' @param prefer_not_label Label for respondents who prefer not to answer.
#' @param ... Additional arguments passed to \code{sata_to_mec()}, such as \code{coding} or \code{missing_label}.
#'
#' @return A character vector of mutually exclusive classifications.
#' @export
seg_disability <- function(data,
                           disability_cols,
                           no_disability_cols = NULL,
                           prefer_not_cols = NULL,
                           disability_label = "With Disability",
                           no_disability_label = "No Disability",
                           prefer_not_label = "Prefer Not to Answer",
                           ...) {
  #use a tidyverse approach to allow users to specify columns in a variety of ways
  disability <- tidyselect::eval_select(rlang::enquo(disability_cols), data = data) |> names()
  no_disability <- tidyselect::eval_select(rlang::enquo(no_disability_cols), data = data) |> names()
  prefer_not <- tidyselect::eval_select(rlang::enquo(prefer_not_cols), data = data) |> names()
  
  #coerce inputs into a named list for sata_to_mecp
  groups <- list()
  if (length(disability) > 0) groups[[disability_label]] <- disability
  if (length(no_disability) > 0) groups[[no_disability_label]] <- no_disability
  if (length(prefer_not) > 0) groups[[prefer_not_label]] <- prefer_not
  
  #coerce inputs into a named list for sata_to_mecp
  sata_to_mecp(
    data = data,
    groups = groups,
    ...
  )
}
