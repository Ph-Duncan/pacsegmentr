% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seg_age_categories.R
\name{seg_age_groups}
\alias{seg_age_groups}
\title{Segment: Age Group from Categorical Age Range Data}
\usage{
seg_age_groups(
  age_group_var,
  mapping = NULL,
  labels = list(children_youth = "Children & Youth", adults = "Adults", older_adults =
    "Older Adults", prefer_not_to_say = "Prefer not to say", missing = NA_character_)
)
}
\arguments{
\item{age_group_var}{A vector of age range labels or numeric codes.}

\item{mapping}{Optional named mapping table for custom encoding.}

\item{labels}{A named list of output labels: children_youth, adults, older_adults, prefer_not_to_say, missing.}
}
\value{
A character vector of segmentation labels.
}
\description{
Re-categorizes existing age data (e.g., ranges) into standard groups.
}
