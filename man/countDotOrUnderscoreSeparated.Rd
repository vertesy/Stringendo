% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{countDotOrUnderscoreSeparated}
\alias{countDotOrUnderscoreSeparated}
\title{Count Dots or Underscores in a String and return}
\usage{
countDotOrUnderscoreSeparated(string)
}
\arguments{
\item{string}{A character string in which the number of "." characters will be counted. Default: None.}
}
\value{
An integer representing the number of "." characters in the string.

An integer representing the number of "." characters in the string.
}
\description{
This function counts the number of "." characters in a given string.
}
\examples{
\dontrun{
countDotOrUnderscoreSeparated("Hello.World...")
countDotOrUnderscoreSeparated("add_translated_metadata")
countDotOrUnderscoreSeparated("add_translated.metadata")
countDotOrUnderscoreSeparated("add translated metadata")
countDotOrUnderscoreSeparated("addTranslatedMetadata")
}
}
