% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter-types}
\alias{filter-types}
\alias{filter.discrete}
\alias{filter.discrete_text}
\alias{filter.range}
\alias{filter.date_range}
\alias{filter.multi_discrete}
\alias{filter.query}
\title{Filter types}
\usage{
\method{filter}{discrete}(
  type,
  id,
  name,
  ...,
  active = getOption("cb_active_filter", default = TRUE)
)

\method{filter}{discrete_text}(
  type,
  id,
  name,
  ...,
  description = NULL,
  active = getOption("cb_active_filter", default = TRUE)
)

\method{filter}{range}(
  type,
  id,
  name,
  ...,
  description = NULL,
  active = getOption("cb_active_filter", default = TRUE)
)

\method{filter}{date_range}(
  type,
  id,
  name,
  ...,
  description = NULL,
  active = getOption("cb_active_filter", default = TRUE)
)

\method{filter}{multi_discrete}(
  type,
  id,
  name,
  ...,
  description = NULL,
  active = getOption("cb_active_filter", default = TRUE)
)

\method{filter}{query}(
  type,
  id,
  name,
  ...,
  active = getOption("cb_active_filter", default = TRUE)
)
}
\arguments{
\item{type}{Character string defining filter type (having class of the same value as type).}

\item{id}{Id of the filter.}

\item{name}{Filter name.}

\item{...}{Source specific parameters passed to filter (see \link{filter-source-types}).}

\item{active}{If FALSE filter will be skipped during Cohort filtering.}

\item{description}{Filter description object. Preferable a character value.}
}
\value{
A function of class `cb_filter_constructor`.
}
\description{
Filter types
}
