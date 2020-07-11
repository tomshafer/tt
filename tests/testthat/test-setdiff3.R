test_that("setdiff3 behaves correctly", {

  vector1 <- list(1, 2, 3, "a", "b", "c")
  vector2 <- list(1, "a", formula(y ~ x))

  expected <- list(
    vector1 = list(2, 3, "b", "c"),
    vector2 = list(y ~ x),
    both = list(1, "a")
  )

  expect_equal(setdiff3(vector1, vector2), expected)

})
