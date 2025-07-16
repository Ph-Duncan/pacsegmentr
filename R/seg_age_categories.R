#' Segment: Age Group from Categorical Age Range Data
#'
#' Re-categorizes existing age data (e.g., ranges) into standard groups.
#'
#' @param age_group_var A vector of age range labels or numeric codes.
#' @param mapping Optional named mapping table for custom encoding.
#' @param labels A named list of output labels: children_youth, adults, older_adults, prefer_not_to_say, missing.
#' @return A character vector of segmentation labels.
#' @export
seg_age_groups <- function(age_group_var,
                           mapping = NULL,
                           labels = list(
                             children_youth = "Children & Youth",
                             adults = "Adults",
                             older_adults = "Older Adults",
                             prefer_not_to_say = "Prefer not to say",
                             missing = NA_character_
                           )) {
  default_map <- get_default_mapping("age_groups")
  map <- if (is.null(mapping)) default_map else mapping
  raw <- tolower(as.character(age_group_var))
  
  if (all(grepl("^[0-9]+$", raw[!is.na(raw)]))) {
    warning("Assuming numeric encoding for age groups. Using default order.")
    raw <- as.character(sapply(as.numeric(raw), function(x) names(map)[x]))
  }
  
  result <- dplyr::case_when(
    is.na(raw) | raw == "" ~ labels$missing,
    raw %in% map$children_youth ~ labels$children_youth,
    raw %in% map$adults ~ labels$adults,
    raw %in% map$older_adults ~ labels$older_adults,
    raw %in% map$prefer_not_to_say ~ labels$prefer_not_to_say,
    TRUE ~ labels$missing
  )
  return(result)
}
