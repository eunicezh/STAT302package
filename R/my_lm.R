#' my_lm function
#'
#' This function gives a summary of the lm object in R.
#'
#' @param formula Formula class object.
#' @param data Input data frame.
#'
#' @return a table with four columns, estimates, std_error, t_stat, and p_value.
#' @keywords \code{inference}
#'
#' @examples
#' my_lm(lm(formula = mpg ~ hp * wt, data = mtcars), mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  X <- model.matrix(formula, data)
  Y <- model.response(model.frame(formula, data))

  # calculate the estimates for coefficients
  estimate <- solve((t(X) %*% X)) %*% (t(X) %*% Y)

  # calculate standard errors
  df = nrow(data) - ncol(X)
  sample_var = sum((Y - X %*% estimate) ^ 2 / df)
  std_error <- sqrt(diag(sample_var * solve(t(X) %*% X)))

  # calculate t statistic
  t_stat <- estimate / std_error

  # calculate p values
  p_value <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)

  # create the matrix contained the output
  result <- matrix(NA, nrow = ncol(X), ncol = 4)
  result[, 1] <- estimate
  result[, 2] <- std_error
  result[, 3] <- t_stat
  result[, 4] <- p_value
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  for (i in 1:ncol(X)) {
    rownames(result) <- colnames(X)
  }

  return(result)
}


