% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataFormatPie.R
\name{dataFormatPie}
\alias{dataFormatPie}
\title{Formats a given categorical vector for use by filterPie}
\usage{
dataFormatPie(rawVector)
}
\arguments{
\item{rawVector}{input data; should be a categorical vector.}
}
\description{
Formats a given categorical vector for use by filterPie
}
\examples{
rawVector <- sample(c("French", "German", "British", "American", "Canadian", "Dutch"),
1000, replace=TRUE, prob=c(0.2, 0.3, 0.3, .1, .05, .05))

dataFormatPie(rawVector)
}
