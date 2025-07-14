#' Segment: Age Group from Numeric Age
#'
#' Categorizes numeric age into standard groups.
#'
#' @param age_var A vector of numeric age or character (may contain "prefer not to say" values).
#' @param prefer_not_to_say_values Optional character vector of special values to treat as prefer not to say.
#' @param labels A named list of output labels: children_youth, adults, older_adults, prefer_not_to_say, missing.
#' @return A character vector of segmentation labels.
#' @export
seg_age_numeric <- function(age_var,
                            prefer_not_to_say_values = NULL,
                            labels = list(
                              children_youth = "Children & Youth",
                              adults = "Adults",
                              older_adults = "Older Adults",
                              prefer_not_to_say = "prefer not to say",
                              missing = NA_character_
                            )) {
  raw <- suppressWarnings(as.numeric(as.character(age_var)))
  is_prefer_not_to_say <- if (!is.null(prefer_not_to_say_values)) age_var %in% prefer_not_to_say_values else FALSE
  result <- dplyr::case_when(
    is.na(age_var) | age_var == "" ~ labels$missing,
    is_prefer_not_to_say ~ labels$prefer_not_to_say,
    raw < 0 ~ labels$missing,
    raw <= 17 ~ labels$children_youth,
    raw >= 65 ~ labels$older_adults,
    raw %in% 17:65 ~ labels$adults,
    TRUE ~ labels$missing
  )
  if (any(raw < 15, na.rm = TRUE)) {
    warning("Detected respondents under age 15. Ensure youth participation is appropriate. Consider whether these responses are an error.")
  }
  return(result)
}
