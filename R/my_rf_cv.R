#' my_rf_cv function
#'
#' This function predict predict the body mass based on characteristics of penguins
#'
#' @param k Number of folds.
#'
#' @return a numeric with the cross-validation error
#' @keywords \code{prediction}
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # remove the missing values from penguins data
  my_penguins <- na.omit(my_penguins)
  # randomly assign observations to the training data and test data
  folds <- sample(rep(1:k, length = nrow(my_penguins)))
  data <- data.frame("x" = my_penguins, "y" = my_penguins[[6]], "split" = folds)
  # make an empty list that stores the MSE rate for each fold
  MSE_list <- rep(NA, k)
  # split training data and test data based on i
  for (i in 1:k) {
    data_train <- data %>% filter(split != i)
    data_test <- data %>% filter(split == i)
    MODEL <- randomForest(x.body_mass_g ~ x.bill_length_mm + x.bill_depth_mm + x.flipper_length_mm, data = data_train, ntree = 100)
    PREDICTIONS <- predict(MODEL, data_test[, -1])
    # calculate MSE for each fold
    MSE_list[i] <- mean((data_test$x.body_mass_g - PREDICTIONS)^2)
  }
  # compute average MSE across all folds
  cv_err <- mean(MSE_list)
  return(cv_err)
}

