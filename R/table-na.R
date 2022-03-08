
#' Cross tabulation with NAs included by default
#'
#' @param ... Arguments passed to [table()].
#'
#' @return A [table()] object.
#' @export
table_na <- function(...) {
  table(..., useNA = "ifany")
}
