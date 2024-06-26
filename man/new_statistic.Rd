% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Statistic.R
\name{new_statistic}
\alias{new_statistic}
\title{New statistic}
\usage{
new_statistic(name, value, units, proportion = NA_real_)
}
\arguments{
\item{name}{\code{character} name of statistic.}

\item{value}{\code{numeric} value or \code{NA_real_}.
This parameter describes the statistic in absolute terms (e.g. 30).}

\item{units}{\code{character} value.
This parameter contains the units for the statistic (e.g. `"km}

\item{proportion}{\code{numeric} value (optional) .
This parameter describes the statistic in relative terms (e.g. 30\%).
Note that values are expressed as a proportion (e.g. 0.3 indicates 30\%).}
}
\value{
A \link{Statistic} object.
}
\description{
Create a new \link{Statistic} object.
}
\examples{
# create a statistic
x <- new_statistic(name = "Area", value = 12, units = "ha", proportion = 0.4)

# print object
print(x)
}
