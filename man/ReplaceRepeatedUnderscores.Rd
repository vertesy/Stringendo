% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{ReplaceRepeatedUnderscores}
\alias{ReplaceRepeatedUnderscores}
\title{ReplaceRepeatedUnderscores}
\usage{
ReplaceRepeatedUnderscores(string)
}
\arguments{
\item{string}{The string (file path) potentially having repeated slashes.}
}
\value{
A string with repeated slashes replaced by a single slash.
}
\description{
ReplaceRepeatedUnderscores replaces multiple consecutive slashes with a single slash.
}
\examples{
ReplaceRepeatedUnderscores(string = "path//to//folder")
}
