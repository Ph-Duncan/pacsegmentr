
#' Segment respondents by urban/rural based on FSA
#'
#' Classifies postal FSAs as "urban" or "rural" based on the second character:
#' FSA codes with '0' as the second character are rural, all others are urban.
#' Invalid or missing FSAs are handled gracefully.
#'
#' @param fsa_vector A character vector of Canadian postal codes or FSAs.
#' @param labels A named list of output labels: `urban`, `rural`, `invalid`, `missing`.
#' @param warn Logical. If TRUE, print warnings for invalid FSAs.
#'
#' @return A character vector of urban/rural/invalid/missing labels.
#' @export
seg_fsa_urban <- function(fsa_vector,
                          labels = list(
                            urban = "urban",
                            rural = "rural",
                            invalid = "invalid",
                            missing = NA_character_
                          ),
                          warn = TRUE) {
  out <- rep(labels$missing, length(fsa_vector))

  for (i in seq_along(fsa_vector)) {
    raw <- as.character(fsa_vector[i])
    fsa <- toupper(trimws(substr(raw, 1, 3)))

    if (is.na(fsa) || fsa == "") {
      out[i] <- labels$missing
    } else if (nchar(fsa) < 3 || !grepl("^[A-Z][0-9][A-Z]$", fsa)) {
      out[i] <- labels$invalid
      if (warn) warning("Invalid FSA format at position ", i, ": ", fsa)
    } else {
      type <- substr(fsa, 2, 2)
      out[i] <- if (type == "0") labels$rural else labels$urban
    }
  }

  return(out)
}
