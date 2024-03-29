% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohort_methods.R
\name{plot_data}
\alias{plot_data}
\title{Plot filter related Cohort data.}
\usage{
plot_data(x, step_id, filter_id, ..., state = "post")
}
\arguments{
\item{x}{Cohort object.}

\item{step_id}{Id of step in which the filter was defined..}

\item{filter_id}{Filter id.}

\item{...}{Another parameters passed to filter plotting method.}

\item{state}{Generate plot based on data before ("pre") or after ("post") filtering.}
}
\value{
Filter-specific plot.
}
\description{
For specified filter the method calls filter-related plot method to present data.
}
\seealso{
\link{cohort-methods}
}
