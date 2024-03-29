% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{pl}
\alias{pl}
\title{Pluralize based on vector length}
\usage{
pl(x, stem = NULL)
}
\arguments{
\item{x}{A vector used ot derive a pluralization.}

\item{stem}{If present, append "s" to \code{stem} if plural.}
}
\value{
A character vector.
}
\description{
Pluralize based on vector length
}
\examples{
pl(1:2)
pl(1:2, "item")
}
