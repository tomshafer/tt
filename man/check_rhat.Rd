% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stan-utils.R
\name{check_rhat}
\alias{check_rhat}
\title{Check the potential scale reduction factors}
\usage{
check_rhat(fit)
}
\arguments{
\item{fit}{A \code{\link[rstan:stanfit-class]{stanfit}} object.}
}
\value{
1 if all Rhats look OK and 0 otherwise.
}
\description{
Check the potential scale reduction factors
}
\examples{
\dontrun{
check_rhat(fit)
}
}
