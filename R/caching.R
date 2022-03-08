#' Cache and load an R object from disk
#'
#' If the target file exists it will be loaded unless
#' the 'overwrite' argument is set.
#'
#' @param file Where to cache the R object.
#' @param object An R object.
#' @param overwrite Whether to overwrite the cached object.
#'
#' @return The fresh/cached object.
#' @export
#'
#' @examples
#' \dontrun{
#' target <- tempfile()
#' result <- with_cache(target, { 1 + 1 })
#' result <- with_cache(target, { 1 + 1 })
#' unlink(target)
#' }
with_cache <- function(file, object, overwrite = FALSE) {
  if (isFALSE(file.exists(file)) | isTRUE(overwrite)) {
    message("Caching object to ", file)
    saveRDS(object, file)
  }
  # Always load from cache as a check against failure
  message("Loading object from ", file)
  readRDS(file)
}
