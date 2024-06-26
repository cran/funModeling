% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information_theory.R
\name{entropy_2}
\alias{entropy_2}
\title{Computes the entropy between two variables}
\usage{
entropy_2(input, target)
}
\arguments{
\item{input}{numeric/character vector}

\item{target}{numeric/character vector}
}
\value{
Entropy measured in bits
}
\description{
It calculates the entropy between two categorical variables using log2.
This log2 is mentioned in most of the Claude Shannon bibliography.
Input/target can be numeric or character.
}
\examples{
\donttest{
# Measuring entropy between input and target variable
entropy_2(input=data_golf$outlook, target=data_golf$play_golf)
}
}
