
#' Check for installed packages
#'
#' @param packages A character vector of one or more package names.
#'
#' @examples
#' \dontrun{
#' check_for_namespace(c("data.table", "dplyr"))
#' }
check_for_packages <- function(packages) {
  n <- 0
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      n <- n + 1
      cat(sprintf("Package \"%s\" is required.\n", pkg))
    }
  }
  if (n > 0) {
    stop(sprintf("Please install the %d %s listed above.", n, pl(n, "package")))
  }
}


#' Pluralize based on vector length
#'
#' @param x A vector used ot derive a pluralization.
#' @param stem If present, append "s" to `stem` if plural.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' pl(1:2)
#' pl(1:2, "item")
pl <- function(x, stem = NULL) {
  sfx <- if (length(x) == 1) "" else "s"
  paste0(stem, sfx)
}
