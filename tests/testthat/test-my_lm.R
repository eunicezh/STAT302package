
test_regression <- lm(formula = mpg ~ hp * wt, data = mtcars)
my_regression <- my_lm(test_regression, mtcars)
true_regression <- summary(test_regression)

test_that("my_lm works for intercept values", {
  for (i in 1:4) {
    expect_true(round(my_regression[1,i]) == round(true_regression$coefficients[1,i]))
  }
})

test_that("my_lm works for coefficient values", {
  for (i in 1:4) {
    for (i in 1:4)
    expect_true(round(my_regression[i,i]) == round(true_regression$coefficients[i,i]))
  }
})

test_that("inappropriate or missing inputs throws error", {
  expect_error(my_lm("a string", mtcars))
  expect_error(my_lm(test_regression))
})

