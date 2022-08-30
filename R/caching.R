#' Cache and load an R object from disk
#'
#' If the target file exists it will be loaded unless
#' the 'overwrite' argument is set.
#'
#' @param file Where to cache the R object
#' @param object An R object
#' @param overwrite Whether to overwrite the cached object
#' @param ... Unused, will warn
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

  if (length(list(...)) > 0) {
    warning("with_cache() no longer uses arguments provided via `...`")
  }

  ext <- tolower(tools::file_ext(file))
  cache_fns <- switch(ext, fst = fst_cache_functions(), rds_cache_functions())

  if (isTRUE(cache_is_invalid(file)) || isTRUE(overwrite)) {
    message("Caching object to ", file)
    cache_fns$write(object, file)
  }

  # Always load from cache as a check against failure
  message("Loading object from ", file)
  cache_fns$read(file)
}


cache_is_invalid <- function(file) {
  isFALSE(file.exists(file))
}


rds_cache_functions = function() {
  list(read = readRDS, write = saveRDS)
}


fst_cache_functions = function() {
  useDT = (
    getOption("tt.cache.prefer_data_table", FALSE) &&
    requireNamespace("data.table", quietly = TRUE)
  )

  # Use the same kwargs as readRDS/saveRDS
  list(
    read = function(file) fst::read_fst(path = file, as.data.table = useDT),
    write = function(object, file) fst::write_fst(object, file)
  )
}
