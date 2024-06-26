% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_lib.R
\name{coord_plot}
\alias{coord_plot}
\title{Coordinate plot}
\usage{
coord_plot(data, group_var, group_func = mean, print_table = FALSE)
}
\arguments{
\item{data}{input data source}

\item{group_var}{variable to make the group by}

\item{group_func}{the data type of this parameter is a function, not an string, this is the function to be used in the group by, the default value is: mean}

\item{print_table}{False by default, if true it retrieves the mean table used to generate the plot.}
}
\value{
coordinate plot, if print_table=T it also prints a table with the average per column plus the average of the whole column
}
\description{
Calculate the means (or other function defined in 'group_func' parameter) per group to analyze how each segment behave. It scales each variable mean inti the 0 to 1 range to easily profile the groups according to its mean. It also calculate the mean regardless the grouping. This function is also useful when you want to profile cluster results in terms of its means.
}
\examples{
\donttest{
# calculating the differences based on function 'mean'
coord_plot(data=mtcars, group_var="cyl")
# printing the table used to generate the coord_plot
coord_plot(data=mtcars, group_var="cyl", print_table=TRUE)
# printing the table used to generate the coord_plot
coord_plot(data=mtcars, group_var="cyl", group_func=median, print_table=TRUE)
}
}
