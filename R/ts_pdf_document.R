
#' Generate a PDF document with custom options
#'
#' TODO: Create our own preprocessor to avoid setting so many options
#' in the preamble.
#'
#' @param ... Arguments passed to [bookdown::pdf_document2]
#' @export
ts_pdf_document = function(...) {
  if (!requireNamespace("bookdown", quietly = T))
    stop("The 'bookdown' package is required to compile PDFs")

  dots = list(...)
  defaults = c(toc = F, keep_md = F, keep_tex = T, number_section = F)
  for (key in names(defaults)) {
    if (!key %in% names(dots))
      dots[[key]] = defaults[[key]]
  }

  # Include our preamble file
  if (!"includes" %in% names(dots)) {
    dots[["includes"]] = list(in_header = NULL)
  }

  dots[["includes"]][["in_header"]] = c(
    dots[["includes"]][["in_header"]],
    system.file(
      "rmarkdown", "templates", "latex-article", "resources", "header.tex",
      package = "tshfr"
    )
  )

  do.call(bookdown::pdf_document2, dots)
}
