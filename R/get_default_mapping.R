#' Get Default Mapping Tables
#'
#' Returns the default mapping tables used throughout the pacsegmentr package.
#'
#' @param key Optional name of a specific mapping table to retrieve.
#' @return A list of mapping tables, or a single table if a key is specified.
#' @export
get_default_mapping <- function(key = NULL) {
  maps <- list(
    income = tibble::tibble(
      raw = c("Less than $20,000", "$20,000 to $24,999", "$25,000 to $29,999",
              "$30,000 to $34,999", "$35,000 to $39,999", "$40,000 to $44,999",
              "$45,000 to $49,999", "$50,000 to $54,999", "$55,000 to $59,999",
              "$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999",
              "$90,000 to $99,999", "$100,000 to $149,999", "$150,000 or more",
              "Prefer not to answer"),
      code = c(1:15, "prefer not to answer")
    ),
    newcomer = tibble::tibble(
      raw = c("All my life", "10 years or more", "6 to 9 years", "1 to 5 years", "Less than 1 year", "Prefer not to answer"),
      code = 1:6
    ),
    age_groups = tibble::tibble(
      raw = c("18-25", "25-34", "35-44", "45-54", "55-64", "65-74", "Over 74", "Prefer not to answer"),
      code = 1:8
    )
  )
  if (!is.null(key)) {
    if (key %in% names(maps)) return(maps[[key]])
    else stop("Unknown key for default mapping.")
  }
  return(maps)
}
