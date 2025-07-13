#' Recode Income Responses
#'
#' Converts income responses to numeric codes or reverses back to labels using a mapping table.
#'
#' @param data A vector or column
#' @param mapping A tibble with 'raw' and 'code' columns
#' @param reverse Logical, if TRUE performs reverse mapping
#' @return A vector of recoded values
#' @export
recode_income <- function(data, mapping = NULL, reverse = FALSE) {
  if (is.null(mapping)) {
    mapping <- get_default_mapping("income")
  }
  if (!all(c("raw", "code") %in% names(mapping))) {
    stop("Mapping must contain 'raw' and 'code' columns.")
  }
  if (!reverse) {
    return(purrr::map_chr(data, function(x) {
      if (x %in% mapping$raw) {
        mapping$code[mapping$raw == x]
      } else {
        x
      }
    }))
  } else {
    return(purrr::map_chr(data, function(x) {
      if (x %in% as.character(mapping$code)) {
        mapping$raw[as.character(mapping$code) == x]
      } else {
        x
      }
    }))
  }
}
