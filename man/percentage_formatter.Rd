% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{percentage_formatter}
\alias{percentage_formatter}
\title{percentage_formatter}
\usage{
percentage_formatter(
  x,
  digitz = 3,
  keep.names = F,
  prefix = NULL,
  suffix = NULL,
  sign_sep = ""
)
}
\arguments{
\item{x}{A vector of numbers between 0-1.}

\item{digitz}{Number of digits to keep. 3 by default.}

\item{keep.names}{Keep vector names}

\item{prefix}{prefix added before the string, Default: NULL}

\item{suffix}{suffix added after the string, Default: NULL}

\item{sign_sep}{Need space before \% sign?}
}
\description{
Parse a string of 0-100\% from a number between 0 and 1.
}
\examples{
percentage_formatter(x = 4.2822212, digitz = 3)
}
