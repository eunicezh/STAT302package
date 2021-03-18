

test_that("my_rf_cv returns a numeric Estimate", {
  expect_type(my_rf_cv(5),"double")
})

test_that("inappropriate input throw error", {
  expect_error(my_rf_cv("a string"))
})

