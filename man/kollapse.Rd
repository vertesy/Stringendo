% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{kollapse}
\alias{kollapse}
\title{Kollapse}
\usage{
kollapse(..., collapseby = "", print = TRUE)
}
\arguments{
\item{...}{Variables (strings, vectors) to be collapsed in consecutively.}

\item{collapseby}{collapse elements into a string separated by this character}

\item{print}{Print the results to the terminal. TRUE by default.}
}
\description{
Collapses values and strings to one string (without a white space).
It also prints the results (good for a quick check)
}
\examples{
kollapse(
  "Hello ", LETTERS[24],
  ", the winning numbers are ", c(1, 3, 5, 65, 11), " . Yay!"
)
}
