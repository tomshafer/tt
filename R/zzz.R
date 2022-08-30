# Cf. https://r-pkgs.org/Code.html#when-you-do-need-side-effects
.onLoad <- function(libname, pkgname) {
  op <- options()

  op.tt <- list(
    tt.prefer_data_table = TRUE
  )

  toset <- !(names(op.tt) %in% names(op))
  if(any(toset)) options(op.tt[toset])

  invisible(NULL)
}
