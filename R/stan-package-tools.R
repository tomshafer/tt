
#' Extract divergences from a variety of Stan packages
#'
#' @param x A Stan MCMC model fit.
#' @param ... Arguments passed to S3 methods if applicable.
#'
#' @return A vector containing the number of divergences per iteration.
#' @export
#' @examples
#' \dontrun{
#' fit <- brms::brm(y ~ x, data = datalist)
#' extract_divergences(fit)
#' }
extract_divergences <- function(x, ...) {
  UseMethod("extract_divergences")
}

#' @export
#' @noRd
extract_divergences.brmsfit <- function(x, ...) {
  check_for_packages("brms")
  brms::nuts_params(x, "divergent__")[, "Value"]
}

#' @export
#' @noRd
extract_divergences.stanfit <- function(x, ...) {
  check_for_packages("rstan")
  divs <- sapply(rstan::get_sampler_params(x), function(x) x[, "divergent__"])
  as.vector(divs)
}

#' @export
#' @noRd
extract_divergences.CmdStanMCMC <- function(x, ...) {
  divs <- x$sampler_diagnostics(format = "draws_array")[, , "divergent__"]
  as.vector(divs)
}

#' Extract tree depths from a variety of Stan packages
#'
#' @param x A Stan MCMC model fit.
#' @param ... Arguments passed to S3 methods if applicable.
#'
#' @return A vector containing the tree depths per iteration.
#' @export
#' @examples
#' \dontrun{
#' fit <- brms::brm(y ~ x, data = datalist)
#' extract_treedepths(fit)
#' }
extract_treedepths <- function(x, ...) {
  UseMethod("extract_treedepths")
}

#' @export
#' @noRd
extract_treedepths.brmsfit <- function(x, ...) {
  check_for_packages("brms")
  brms::nuts_params(x, "treedepth__")[, "Value"]
}

#' @export
#' @noRd
extract_treedepths.stanfit <- function(x, ...) {
  check_for_packages("rstan")
  depths <- sapply(rstan::get_sampler_params(x), function(x) x[, "treedepth__"])
  as.vector(depths)
}

#' @export
#' @noRd
extract_treedepths.CmdStanMCMC <- function(x, ...) {
  depths <- x$sampler_diagnostics(format = "draws_array")[, , "treedepth__"]
  as.vector(depths)
}


# TODO: Energy, Rhat, Neff/iter
# Check that the three packages agree on this stuff?
