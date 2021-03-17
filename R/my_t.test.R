#' T-test function
#'
#' This function performs a one sample t-test.
#'
#' @param x Numeric vector of data.
#' @param alternative Character string specifying the alternative hypothesis.
#' @param mu Number indicating the null hypothesis value of the mean
#'
#' @return A list with four elements, test_stat, df, alternative, and p_value.
#' @keywords \code{inference}
#'
#' @examples
#' my_t.test(rbinom(100, size = 7, prob = 0.1), "greater", 0.9)
#' my_t.test(rbinom(100, size = 7, prob = 0.1), "less", 0.9)
#' my_t.test(rbinom(100, size = 7, prob = 0.1), "two.sided", 0.9)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  estimate <- mean(x)
  df <- length(x) - 1
  se <- sd(x) / sqrt(length(x))
  t_obs <- (estimate - mu) / se

  if (alternative == "less") {
    p_value <- pt(t_obs, df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_value <- pt(t_obs, df, lower.tail = FALSE)
  } else {
    p_value <- 2 * pt(abs(t_obs), df, lower.tail = FALSE)
  }

  result <- list("test_stat" = t_obs,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_value)
  return(result)
}


