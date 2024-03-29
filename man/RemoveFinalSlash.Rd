% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{RemoveFinalSlash}
\alias{RemoveFinalSlash}
\title{RemoveFinalSlash}
\usage{
RemoveFinalSlash(string)
}
\arguments{
\item{string}{The string (file path) potentially having a final slash.}
}
\value{
A string with the final slash removed.
}
\description{
RemoveFinalSlash removes the final slash(es) from a string (file path).
}
\examples{
RemoveFinalSlash(string = "path/to/folder/")
}
