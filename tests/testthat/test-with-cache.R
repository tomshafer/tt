
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
  expect_equal(with_cache(cache_files[1], 1 + 2, overwrite = T), object)
  expect_equal(with_cache(cache_files[2], object, overwrite = T), object)
})
