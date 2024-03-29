% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stringendo.R
\name{parFlags}
\alias{parFlags}
\title{parFlags}
\usage{
parFlags(prefix = "", ..., pasteflg = TRUE, collapsechar = ".")
}
\arguments{
\item{prefix}{Append something before?}

\item{...}{Paramter variables}

\item{pasteflg}{Boolean: paste the parameters-flags together?}

\item{collapsechar}{Separating character between each parameters-flag}
}
\description{
Create a string from the names of the (boolean) parameters (TRUE or FALSE) of true values.
Use it for Suffixing plot names with the parameters that were used for that plot.
}
\examples{
pearson <- TRUE; filtered <- TRUE;  normalized <- FALSE
MyPlotname <- parFlags(prefix = "MyPlot", pearson, filtered, normalized); MyPlotname

}
