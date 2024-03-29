% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{toSentence}
\alias{toSentence}
\title{Convert CamelCase to Sentence}
\usage{
toSentence(camelCaseString)
}
\arguments{
\item{camelCaseString}{A character string in camelCase format.}
}
\value{
A character string converted to sentence format.
}
\description{
Takes a camelCase string and converts it to a sentence format: space-separated,
with the first letter capitalized and no period at the end.
}
\examples{
toSentence("mergeSmallCategories")
}
