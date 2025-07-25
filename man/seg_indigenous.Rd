% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seg_indigenous.R
\name{seg_indigenous}
\alias{seg_indigenous}
\title{Segments Respondents as Indigenous, Not Indigenous, or Prefer Not to Answer}
\usage{
seg_indigenous(
  data,
  indigenous_cols,
  not_indigenous_cols = NULL,
  prefer_not_cols = NULL,
  indigenous_label = "Indigenous",
  not_indigenous_label = "Not Indigenous",
  prefer_not_label = "Prefer Not to Answer",
  ...
)
}
\arguments{
\item{data}{A data frame.}

\item{indigenous_cols}{Columns indicating Indigenous identity. Supports tidyselect syntax.}

\item{not_indigenous_cols}{Optional. Columns indicating not Indigenous.}

\item{prefer_not_cols}{Optional. Columns indicating "prefer not to answer".}

\item{indigenous_label}{Label for respondents identified as Indigenous.}

\item{not_indigenous_label}{Label for respondents not identified as Indigenous.}

\item{prefer_not_label}{Label for respondents who prefer not to answer.}

\item{...}{Additional arguments passed to \code{sata_to_mecp()}, such as \code{coding} or \code{missing_label}.}
}
\value{
A character vector of mutually exclusive classifications.
}
\description{
Converts a set of columns representing SATA (select-all-that-apply) question data corresponding 
to Indigenous identities into a single variable of MECs (mutually exclusive category) using the 
\code{sata_to_mecp()} function. This wrapper is designed for implementing ParticipACTION's demographic 
segmentation strategy for classifying respondents as Indigenous vs Not Indigenous.
}
