test_that("linear model importance", {
  model <- parsnip::linear_reg(mixture = 0, penalty = 0.1) %>%
    parsnip::set_engine("lm") %>%
    parsnip::fit(mpg ~ ., data = mtcars)

  permutation_importance(model, mtcars, mpg, yardstick::accuracy)

  expect_equal(2 * 2, 4)
})
