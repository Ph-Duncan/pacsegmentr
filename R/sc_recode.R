
#' Recode a single column using a mapping vector or table
#'
#' @param x A vector to be recoded.
#' @param mapping A named vector OR a data frame/tibble with recoding pairs.
#' @param from Optional column name in mapping table for original values (required if mapping is a data frame).
#' @param to Optional column name in mapping table for recoded values (required if mapping is a data frame).
#' @param default Optional value to use when an input value is not in `mapping`.
#' @param missing Optional value to use for NA values in `x`. Default is to leave NA unchanged.
#'
#' @return A vector with recoded values.
#' @export
sc_recode <- function(x,
                          mapping,
                          from = NULL,
                          to = NULL,
                          default = NULL,
                          missing = NA_character_) {
  # If mapping is a data frame, convert to named vector
  if (is.data.frame(mapping)) {
    if (is.null(from) || is.null(to)) {
      stop("When using a data frame as `mapping`, you must specify `from` and `to` column names.")
    }
    mapping <- setNames(as.character(mapping[[to]]), as.character(mapping[[from]]))
  }

  # Recode using dplyr
  out <- dplyr::recode(
    as.character(x),
    !!!mapping,
    .default = default,
    .missing = missing
  )

  return(out)
}
