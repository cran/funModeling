% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information_theory.R
\name{information_gain}
\alias{information_gain}
\title{Information gain}
\usage{
information_gain(input, target)
}
\arguments{
\item{input}{numeric/character vector}

\item{target}{numeric/character vector}
}
\value{
information gain
}
\description{
Computes the information gain between an 'input' and 'target' variable (using log2). In general terms, the higher the more predictable the input is.
}
\examples{
\donttest{
information_gain(input=data_golf$outlook, target=data_golf$play_golf)
}
}
