% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outliers.R
\name{tukey_outlier}
\alias{tukey_outlier}
\title{Tukey Outlier Threshold}
\usage{
tukey_outlier(input)
}
\arguments{
\item{input}{Numeric variable vector}
}
\value{
A two-item vector, the first value represents the bottom threshold, while the second one is the top threshold
}
\description{
Retrieves the bottom and top boundaries to flag outliers or extreme values, according to the Tukey's test. More info at \url{https://en.wikipedia.org/wiki/Outlier#Tukey.27s_test}
This function is used in 'prep_outliers' function. All `NA`s values are automatically excluded. More information at: \url{https://livebook.datascienceheroes.com/data-preparation.html#how_to_deal_with_outliers_in_r}.
}
\examples{
\donttest{
tukey_outlier(heart_disease$age)
}
}
