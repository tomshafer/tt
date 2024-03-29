% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caching.R
\name{with_cache}
\alias{with_cache}
\title{Cache and load an R object from disk}
\usage{
with_cache(file, object, overwrite = FALSE, ...)
}
\arguments{
\item{file}{Where to cache the R object}

\item{object}{An R object}

\item{overwrite}{Whether to overwrite the cached object}

\item{...}{Unused, will warn}
}
\value{
The fresh/cached object.
}
\description{
If the target file exists it will be loaded unless
the 'overwrite' argument is set.
}
\examples{
\dontrun{
target <- tempfile()
result <- with_cache(target, { 1 + 1 })
result <- with_cache(target, { 1 + 1 })
unlink(target)
}
}
