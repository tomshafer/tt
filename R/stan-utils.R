# Adapted from Michael Betancourt's Stan utilities.
# <https://raw.githubusercontent.com/betanalpha/knitr_case_studies/
#   master/rstan_workflow/stan_utility.R>
#
#   The code in this case study is copyrighted by Columbia University
#   and licensed under the new BSD (3-clause) license:
#     https://opensource.org/licenses/BSD-3-Clause.


#' Check whether [rstan][rstan::rstan] is installed
test_for_rstan <- function() {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package \"rstan\" must be installed to use this function.", call. = F)
  }
}


#' Check for transitions that ended with a divergence
#'
#' @param fit A [`stanfit`][rstan::stanfit-class] object.
#'
#' @return The number of divergences in the fit.
#' @export
#'
#' @examples
#' \dontrun{
#' n_divs <- check_divergences(fit)
#' }
check_divergences <- function(fit) {
  test_for_rstan()

  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  divergent <- do.call(rbind, sampler_params)[, "divergent__"]

  n <- sum(divergent)
  nn <- length(divergent)

  cat(sprintf(
    "%s of %s iterations ended with a divergence (%s%%).\n",
    n, nn, 100 * n / nn
  ))

  if (n > 0) {
    cat("Try running with larger adapt_delta to remove the divergences.\n")
  }

  invisible(n)
}


#' Check transitions that ended prematurely due to maximum tree depth limit
#'
#' @param fit A [`stanfit`][rstan::stanfit-class] object.
#' @param max_depth The tree depth limit.
#'
#' @return The number of iterations saturating the tree depth limit.
#' @export
#'
#' @examples
#' \dontrun{
#' check_treedepth(fit, 10)
#' }
check_treedepth <- function(fit, max_depth = 10) {
  test_for_rstan()

  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  treedepths <- do.call(rbind, sampler_params)[, "treedepth__"]

  n <- length(treedepths[sapply(treedepths, function(x) x == max_depth)])
  nn <- length(treedepths)

  cat(sprintf(
    "%s of %s iterations saturated the maximum tree depth of %s (%s%%).",
    n, nn, max_depth, 100 * n / nn
  ))

  if (n > 0) {
    cat("Run again with a larger max_depth to avoid saturation.\n")
  }

  invisible(n)
}


#' Check the energy Bayesian fraction of missing information (E-BFMI)
#'
#' @param fit A [`stanfit`][rstan::stanfit-class] object.
#'
#' @return 1 if there were E-BFMI warnings and 0 otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' check_energy(fit)
#' }
check_energy <- function(fit) {
  test_for_rstan()

  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)

  no_warning <- TRUE
  for (n in seq_len(sampler_params)) {
    energies <- sampler_params[n][[1]][, "energy__"]

    numer <- sum(diff(energies) ** 2) / length(energies)
    denom <- stats::var(energies)

    if (numer / denom < 0.2) {
      cat(sprintf("Chain %s: E-BFMI = %s\n", n, numer / denom))
      no_warning <- FALSE
    }
  }

  if (no_warning) {
    cat("E-BFMI indicated no pathological behavior.\n")
  } else {
    cat("E-BFMI below 0.2: You may need to reparameterize your model.\n")
  }

  invisible(as.integer(isFALSE(no_warning)))
}


#' Checks the effective sample size per iteration
#'
#' @param fit A [`stanfit`][rstan::stanfit-class] object.
#'
#' @return 1 if n_eff / iter is low and 0 otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' check_n_eff(fit)
#' }
check_n_eff <- function(fit) {
  test_for_rstan()

  fit_summary <- summary(fit, probs = c(0.5))$summary
  nn <- dim(fit_summary)[[1]]
  iter <- dim(rstan::extract(fit)[[1]])[[1]]

  no_warning <- TRUE
  for (n in 1:nn) {
    ratio <- fit_summary[, 5][n] / iter
    if (ratio < 0.001) {
      cat(sprintf(
        "n_eff / iter for parameter %s is %s!",
        rownames(fit_summary)[n], ratio
      ))
      no_warning <- FALSE
    }
  }

  if (no_warning) {
    cat("n_eff / iter looks reasonable for all parameters.\n")
  } else {
    cat(
      "n_eff / iter below 0.001 indicates the effective sample size ",
      "has likely been overestimated.\n"
    )
  }

  invisible(as.integer(isFALSE(no_warning)))
}


#' Check the potential scale reduction factors
#'
#' @param fit A [`stanfit`][rstan::stanfit-class] object.
#'
#' @return 1 if all Rhats look OK and 0 otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' check_rhat(fit)
#' }
check_rhat <- function(fit) {
  test_for_rstan()

  fit_summary <- summary(fit, probs = c(0.5))$summary
  nn <- dim(fit_summary)[[1]]

  no_warning <- TRUE
  for (n in 1:nn) {
    rhat <- fit_summary[, 6][n]
    if (rhat > 1.1 || is.infinite(rhat) || is.nan(rhat)) {
      cat(sprintf(
        "Rhat for parameter %s is %s!",
        rownames(fit_summary)[n], rhat
      ))
      no_warning <- FALSE
    }
  }

  if (no_warning) {
    cat("Rhat looks reasonable for all parameters.\n")
  } else {
    cat("Rhat above 1.1: The chains very likely have not mixed.\n")
  }

  invisible(as.integer(isFALSE(no_warning)))
}


#' Check all Stan diagnostics
#'
#' @param fit A [`stanfit`][rstan::stanfit-class] object.
#'
#' @return A named vector of results from the various checks.
#' @export
#'
#' @examples
#' \dontrun{
#' checks <- check_all_diagnostics()
#' checks["divergences"]
#' }
check_all_diagnostics <- function(fit) {
  c(n_eff = check_n_eff(fit),
    rhat = check_rhat(fit),
    divergences = check_divergences(fit),
    treedepth = check_treedepth(fit),
    energy = check_energy(fit))
}


#' Separate parameter arrays into divergent and non-divergent transitions
#'
#' @param fit A [`stanfit`][rstan::stanfit-class] object.
#'
#' @return A list, separating divergent transitions from non-devergent.
#' @export
#'
#' @examples
#' \dontrun{
#' partition_divergences(fit)
#' }
partition_divergences <- function(fit) {
  test_for_rstan()

  nom_params <- rstan::extract(fit, permuted = FALSE)
  n_chains <- dim(nom_params)[2]

  params <- as.data.frame(do.call(
    rbind,
    lapply(1:n_chains, function(n) nom_params[, n, ])
  ))

  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  divergent <- do.call(rbind, sampler_params)[, "divergent__"]
  params$divergent <- divergent

  div_params <- params[params$divergent == 1, ]
  nondiv_params <- params[params$divergent == 0, ]

  list(divergent = div_params, nondivergent = nondiv_params)
}
