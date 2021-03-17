set.seed(123)
x <- rbinom(1000, size = 10, prob = 0.5)

test_that("my_t.test works with greater", {
  my_result <- my_t.test(x, "greater", 0.1)
  true_result <- t.test(x, mu = 0.1, alternative = "greater")
  expect_true(round(my_result$test_stat) == round(true_result$statistic))
  expect_true(my_result$df == true_result$parameter)
  expect_true(my_result$alternative == true_result$alternative)
  expect_true(my_result$p_val == true_result$p.value)
})

test_that("my_t.test works with less", {
  my_result <- my_t.test(x, "less", 0.1)
  true_result <- t.test(x, mu = 0.1, alternative = "less")
  expect_true(round(my_result$test_stat) == round(true_result$statistic))
  expect_true(my_result$df == true_result$parameter)
  expect_true(my_result$alternative == true_result$alternative)
  expect_true(my_result$p_val == true_result$p.value)
})

test_that("my_t.test works with two sided", {
  my_result <- my_t.test(x, "two.sided", 0.1)
  true_result <- t.test(x, mu = 0.1, alternative = "two.sided")
  expect_true(round(my_result$test_stat) == round(true_result$statistic))
  expect_true(my_result$df == true_result$parameter)
  expect_true(my_result$alternative == true_result$alternative)
  expect_true(my_result$p_val == true_result$p.value)
})

test_that("inappopriate or missing inputs throw error", {
  expect_error(my_t.test(x, "two.sided"))
  expect_error(my_t.test(x, "two.sided", "string"))
})
