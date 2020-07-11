
#' Three-way [setdiff()] of two collections
#'
#' @param a A collection of R objects.
#' @param b A collection of R objects.
#'
#' @return A list with objects only in `a`, only in `b`,
#'   and in both collections.
#' @export
setdiff3 <- function(a, b) {
  nm_a <- deparse(substitute(a))
  nm_b <- deparse(substitute(b))
  out <- list(setdiff(a, b), setdiff(b, a), intersect(a, b))
  names(out) <- c(nm_a, nm_b, "both")
  out
}
