% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{is.numeric.or.logical}
\alias{is.numeric.or.logical}
\title{Check if Input is Numeric or Logical}
\usage{
\method{is.numeric}{or.logical}(x)
}
\arguments{
\item{x}{The input to check.}
}
\value{
Returns \code{TRUE} if \code{x} is either numeric or logical, otherwise \code{FALSE}.
}
\description{
\code{is.numeric.or.logical()} checks if the provided input is either numeric or logical.
}
\examples{
is.numeric.or.logical(123)      # TRUE
is.numeric.or.logical(TRUE)     # TRUE
is.numeric.or.logical("text")   # FALSE

}
