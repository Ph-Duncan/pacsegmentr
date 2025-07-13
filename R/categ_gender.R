#' Categorize Gender
#'
#' Categorizes respondents as woman, man, gender diverse, or prefer not to say.
#'
#' @param data A data frame
#' @param gender_col Name of the gender variable
#' @param output_col Output column name
#' @param labels Named list of output labels
#' @param mapping Optional vector of gender values to recode
#' @return Data frame with gender recode
#' @export
categ_gender <- function(data, gender_col, output_col = "demographics_gender",
                         labels = list(
                           woman = "woman",
                           man = "man",
                           gender_diverse = "gender diverse",
                           prefer_not_to_say = "prefer not to say",
                           missing = NA),
                         mapping = list(
                           "Non-Binary" = "gender diverse",
                           "Two-Spirit" = "gender diverse",
                           "Agender" = "gender diverse")) {
  data[[output_col]] <- NA
  for (i in seq_len(nrow(data))) {
    val <- data[[gender_col]][i]
    if (is.na(val) || val == "") {
      data[[output_col]][i] <- labels$missing
    } else if (tolower(val) %in% tolower(names(mapping))) {
      data[[output_col]][i] <- labels$gender_diverse
    } else if (tolower(val) == "man") {
      data[[output_col]][i] <- labels$man
    } else if (tolower(val) == "woman") {
      data[[output_col]][i] <- labels$woman
    } else if (tolower(val) %in% c("prefer not to answer", "prefer not to say")) {
      data[[output_col]][i] <- labels$prefer_not_to_say
    } else {
      data[[output_col]][i] <- labels$gender_diverse
    }
  }
  return(data)
}
