
#' Reduce "select all that apply" fields to mutually exclusive categories
#'
#' @param data A data frame.
#' @param override_cols Optional named list of columns that take full precedence. Names are output labels.
#' @param secondary_cols Optional named list of columns that take precedence over `tertiary_cols` but are overridden by `override_cols`.
#' @param tertiary_cols Optional named list of columns that are evaluated only if no higher-priority values are selected.
#' @param coding A named list with `true` and `false` encodings.
#' @param missing_label Label to return when no inputs are selected or all are missing.
#' @return A character vector of category labels.
#' @export
sata_to_mec <- function(data,
                        override_cols = NULL,
                        secondary_cols = NULL,
                        tertiary_cols = NULL,
                        coding = list(
                          true = c("yes", "1", "true"),
                          false = c("no", "0", "false", "")
                        ),
                        missing_label = NA_character_) {

  is_selected <- function(x) {
    val <- tolower(as.character(x))
    ifelse(is.na(val), FALSE,
           ifelse(val %in% coding$true, TRUE,
                  ifelse(val %in% coding$false, FALSE, FALSE)))
  }

  n <- nrow(data)
  result <- rep(missing_label, n)

  if (!is.null(override_cols)) {
    for (label in names(override_cols)) {
      cols <- override_cols[[label]]
      selected <- rowSums(as.data.frame(lapply(data[cols], is_selected))) > 0
      result[selected] <- label
    }
  }

  if (!is.null(secondary_cols)) {
    remaining <- is.na(result)
    for (label in names(secondary_cols)) {
      cols <- secondary_cols[[label]]
      selected <- rowSums(as.data.frame(lapply(data[cols], is_selected))) > 0
      result[remaining & selected] <- label
    }
  }

  if (!is.null(tertiary_cols)) {
    remaining <- is.na(result)
    for (label in names(tertiary_cols)) {
      cols <- tertiary_cols[[label]]
      selected <- rowSums(as.data.frame(lapply(data[cols], is_selected))) > 0
      result[remaining & selected] <- label
    }
  }

  return(result)
}
