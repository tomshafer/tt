
cache_files <- c(tempfile(), tempfile())
expected <- 1 + 1


test_that("writing to cache works", {
  object <- expected
  test1 <- with_cache(cache_files[1], object)
  test2 <- with_cache(cache_files[2], 1 + 1)

  expect_equal(test1, test2)
})


test_that("reading from cache works", {
  expect_equal(with_cache(cache_files[1], NULL), expected)
  expect_equal(with_cache(cache_files[2], NULL), expected)
})


test_that("overwriting = FALSE works", {
  object <- 1 + 2

  # These should still be equal to expectation
  expect_equal(with_cache(cache_files[1], 1 + 2), expected)
  expect_equal(with_cache(cache_files[2], object), expected)
})


test_that("overwriting = TRUE works", {
  object <- 1 + 2

  # These should be the new value
  expect_equal(with_cache(cache_files[1], 1 + 2, overwrite = TRUE), object)
  expect_equal(with_cache(cache_files[2], object, overwrite = TRUE), object)
})


test_that("we cache to rds files if not fst", {
  cache_file <- tempfile(fileext = ".rda")

  expected <- data.frame(i = seq_along(LETTERS), x = LETTERS)
  cache <- with_cache(cache_file, expected)

  expect_identical(cache, expected)
  expect_identical(readRDS(cache_file), expected)
})


test_that("we cache to fst if requested", {
  skip_if_not_installed("fst")
  skip_if_not_installed("withr")

  # Don't use data.table
  withr::local_options(list(tt.cache.prefer_data_table = FALSE))

  for (ext in c("fst", "FST")) {
    cache_file <- tempfile(fileext = paste0(".", ext))

    expected <- data.frame(i = seq_along(LETTERS), x = LETTERS)
    cache <- with_cache(cache_file, expected)

    expect_equal(cache, expected)
    expect_equal(fst::read_fst(cache_file), expected)
  }
})


test_that("with_cache returns data.tables if requested", {
  skip_if_not_installed("fst")
  skip_if_not_installed("data.table")

  withr::local_options(list(tt.cache.prefer_data_table = TRUE))

  cache_file <- tempfile(fileext = ".fst")

  expected <- data.table::data.table(i = seq_along(LETTERS), x = LETTERS)
  cache <- with_cache(cache_file, expected)

  expect_equal(expected, cache)
})


test_that("with_cache warns when dots are provided", {
  cache_file <- tempfile()
  expect_warning(
    with_cache(cache_file, 1, other.arg.should.warn = 1),
    "with_cache() no longer uses arguments provided via `...`",
    fixed = TRUE
  )
})
