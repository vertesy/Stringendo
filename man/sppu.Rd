% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{sppu}
\alias{sppu}
\title{Simplified Paste by underscore}
\usage{
sppu(..., make.names = FALSE)
}
\arguments{
\item{...}{Multiple simple variables to parse.}

\item{make.names}{Should make.names applied to the concatenated string? Default is FALSE.}
}
\description{
Simplified Paste by underscore
}
\examples{
sppu("A", 1:2, "end", "", NULL); kppu("A", 1:2, "end", "", NULL)
}
