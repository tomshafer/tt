test_that("table_na mirrors table without NAs", {
  expect_equal(table(LETTERS), table_na(LETTERS))
})

test_that("table_na includes NAs", {
  v <- c(LETTERS, NA_character_)
  expect_equal(head(table_na(v), n = length(LETTERS)), table(v))

  res <- structure(1L, .Dim = 1L, .Dimnames = list(v = NA_character_), class = "table")
  expect_equal(tail(table_na(v), n = 1), res)
})
