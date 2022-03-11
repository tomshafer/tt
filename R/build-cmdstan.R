
#' Build cmdstan with optimizations
#'
#' From the Stan forums ([link](https://discourse.mc-stan.org/t/speedup-by-using-external-blas-lapack-with-cmdstan-and-cmdstanr-py/25441/41)).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' build_cmdstan()
#' }
build_cmdstan <- function() {
  check_for_packages("cmdstanr")

  flags <- "-O3 -mtune=native -march=native"
  cmdstanr::cmdstan_make_local(
    cpp_options = list(
      STAN_THREADS = TRUE,
      STAN_NO_RANGE_CHECKS = TRUE,
      CXXFLAGS_OPTIM = flags,
      CXXFLAGS_OPTIM_TBB = flags,
      CXXFLAGS_OPTIM_SUNDIALS = flags
    ),
    append = FALSE
  )

  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    cmdstanr::install_cmdstan()
  } else {
    cmdstanr::rebuild_cmdstan()
  }
}
