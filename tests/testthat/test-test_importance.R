test_that("shuffle column retains other elements of dataframe", {
  j <- 1
  shuffled <- shuffle_column(mtcars, j)

  expect_equal(mtcars[, -j], shuffled[, -j])
})
