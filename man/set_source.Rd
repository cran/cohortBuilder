% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/source_methods.R, R/source_tblist.R
\name{set_source}
\alias{set_source}
\alias{set_source.tblist}
\title{Create Cohort source}
\usage{
set_source(
  dtconn,
  ...,
  primary_keys = NULL,
  binding_keys = NULL,
  source_code = NULL,
  description = NULL
)

\method{set_source}{tblist}(
  dtconn,
  primary_keys = NULL,
  binding_keys = NULL,
  source_code = NULL,
  description = NULL,
  ...
)
}
\arguments{
\item{dtconn}{An object defining source data connection.}

\item{...}{Source type specific parameters. Available in `attributes` list of resulting object.}

\item{primary_keys}{Definition of primary keys describing source data (if valid).
When provided, affects the output of attrition data plot. See \link{primary_keys}.}

\item{binding_keys}{Definition of binding keys describing relations in source data (if valid).
When provided, affects post filtering data. See \link{binding-keys}.}

\item{source_code}{Expression presenting low-level code for creating source.
When provided, used as a part of reproducible code output.}

\item{description}{A named list storing the source objects description.
Can be accessed with \link{description} Cohort method.}
}
\value{
R6 object of class inherited from `dtconn`.
}
\description{
Source is an object storing information about data source such as source type,
primary keys and relations between stored data.
}
\examples{
mtcars_source <- set_source(
  tblist(mtcars = mtcars),
  source_code = quote({
    source <- list(dtconn = list(datasets = mtcars))
  })
)
mtcars_source$attributes
}
