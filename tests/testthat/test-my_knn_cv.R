my_penguins <- na.omit(my_penguins)

test_that("my_knn_cv returns a numeric Estimate", {
  for (i in 1:10) {
    expect_type(my_knn_cv(5, my_penguins[,3:6],my_penguins[[1]], i ),"list")
  }
})

test_that("inappropriate inputs throw error", {
  expect_error(my_knn_cv(5, my_penguins[,3:6],my_penguins[[1]], "a string" ))
})
