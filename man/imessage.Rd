% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{imessage}
\alias{imessage}
\title{imessage}
\usage{
imessage(..., collapse = " ")
}
\arguments{
\item{...}{Variables (strings, vectors) to be collapsed in consecutively.}
}
\description{
A variant to message() pasting with white space, sibling of iprint().
}
\examples{
iprint("Hello ", "you ", 3, ", ", 11, " year old kids.")
}
