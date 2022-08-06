#' Cache and load an R object from disk
#'
#' If the target file exists it will be loaded unless
#' the 'overwrite' argument is set.
#'
#' @param file Where to cache the R object.
#' @param object An R object.
#' @param overwrite Whether to overwrite the cached object.
#' @param ... Additional arguments for loading from cache.
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
with_cache <- function(file, object, overwrite = FALSE, ...) {
  cache_fns <- get_cache_fns_from_ext(file)
  if (isFALSE(file.exists(file)) || isTRUE(overwrite)) {
    message("Caching object to ", file)
    cache_fns$write(object, file)
  }
  # Always load from cache as a check against failure
  message("Loading object from ", file)
  cache_fns$read(file, ...)
}


# Determine which reading/writing functions to use based on input file
get_cache_fns_from_ext <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "fst") {
    check_for_packages("fst")
    return(list(read = fst::read_fst, write = fst::write_fst))
  }
  list(read = readRDS, write = saveRDS)
}
