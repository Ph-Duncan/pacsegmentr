
#' Segment respondents by 2SLGBTQ+ identity
#'
#' Combines gender, trans status, and sexual orientation to classify 2SLGBTQ+ identity.
#' Missing or blank values are treated as "prefer not to say" unless an SLGBTQ+ identity
#' is explicitly indicated.
#'
#' @param data A data frame.
#' @param gender_col Column name for gender.
#' @param trans_col Column name for trans status.
#' @param orientation_col Column name for sexual orientation.
#' @param labels A named list of output labels:
#'   - `two_slgbtq`: Label for respondents who are 2SLGBTQ+.
#'   - `non_slgbtq`: Label for respondents who are not.
#'   - `prefer_not_to_say`: Label if any input is missing or "prefer not to say".
#' @param match_values A named list of category triggers. Must include:
#'   - `gender_diverse`: values considered gender diverse.
#'   - `transgender`: values considered trans.
#'   - `orientation`: values considered 2SLGBTQ+.
#'
#' @return A character vector of segmentation labels.
#' @export
seg_slgbtq <- function(data,
                       gender_col,
                       trans_col,
                       orientation_col,
                       labels = list(
                         two_slgbtq = "2SLGBTQ+",
                         non_slgbtq = "non-2SLGBTQ+",
                         prefer_not_to_say = "prefer not to say"
                       ),
                       match_values = list(
                         gender_diverse = "gender diverse",
                         transgender = "transgender",
                         orientation = "2SLGBTQ+"
                       )) {
  gender <- trimws(as.character(data[[gender_col]]))
  trans <- trimws(as.character(data[[trans_col]]))
  orientation <- trimws(as.character(data[[orientation_col]]))

  result <- vector("character", length(gender))

  for (i in seq_along(gender)) {
    g <- gender[i]
    t <- trans[i]
    o <- orientation[i]

    if (
      g %in% match_values$gender_diverse ||
      t %in% match_values$transgender ||
      o %in% match_values$orientation
    ) {
      result[i] <- labels$two_slgbtq
    } else if (any(is.na(c(g, t, o))) || any(c(g, t, o) == "" | c(g, t, o) == labels$prefer_not_to_say)) {
      result[i] <- labels$prefer_not_to_say
    } else {
      result[i] <- labels$non_slgbtq
    }
  }

  return(result)
}
