
#' Recodes "select all that apply" variables to priority based mutually exclusive categories by priority
#'
#' Processes a series of variables that correspond to select-all-that-apply (SATA) questions and recodes 
#' into a single vector of mutually exclusive categories (MEC) through a priority order. 
#' Each MEC consists of one or more columns representing SATA results.
#'
#' @param data A data frame.
#' @param groups A named list of character vectors. Each name is an output category;
#'   each value is a vector of column names to check for that category.
#' @param coding A list with `encoded_true` and `encoded_false` values to interpret the encoding of all the selected variables.
#' @param missing_label Label for rows with listwise missing selections. Defaults to NA.
#'
#' @return A character vector of mutually exclusive group labels.
#' @export
#' 
sata_to_mecp <- function(data,
                        groups,
                        coding = list(encoded_true = c("yes", "1", "true",  TRUE),
                                      encoded_false = c("no", "0", "false", FALSE)),
                        missing_label = NA_character_) {
  
  is_selected <- function(x) {
    x <- tolower(as.character(x))
    ifelse(x %in% tolower(as.character(coding$encoded_true)), TRUE,
           ifelse(x %in% tolower(as.character(coding$encoded_false)), FALSE, NA))
  }
  
  n <- nrow(data)
  result <- rep(missing_label, n)
  remaining <- rep(TRUE, n)
  
  for (group_name in names(groups)) {
    cols <- groups[[group_name]]
    if (!is.null(cols) && length(cols) > 0) {
      selected_df <- as.data.frame(lapply(data[, cols, drop = FALSE], is_selected))
      selected_any <- rowSums(selected_df, na.rm = TRUE) > 0
      result[selected_any & remaining] <- group_name
      remaining[selected_any] <- FALSE
    }
  }
  
  return(result)
}