#' my_knn_cv function
#'
#' This function predict output class `species` using covariates
#' `bill_length_mm`, `bill_depth_mm`, `flipper_length_mm`, and `body_mass_g`.
#'
#' @param train Input data frame.
#' @param cl True class value of your training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#'
#' @return a list with objects: class of speciese and CV error
#' @keywords \code{prediction}
#'
#' @examples
#' my_knn_cv(5, penguins[,3:6],penguins[[1]], 1)
#'
#' @export
my_knn_cv <- function(k_cv, train, cl, k_nn) {
  # randomly assign observations to the training data and test data
  folds <- sample(rep(1:k_cv, length = nrow(train)))
  data <- data.frame("x" = train, "y" = cl, "split" = folds)
  # make an empty list that stores the misclassfication error for each fold
  error_list = rep(NA, k_cv)
  # split training data and test data based on i
  for (i in 1:k_cv) {
    data_train <- data %>% filter(split != i)
    data_test <- data %>% filter(split == i)
    cl_test <- as.numeric(data_test$y)
    pred <- knn(data_train[,1:4], data_test[,1:4], as.factor(data_train$y), k=k_nn)
    # record the misclassfication rate for each fold
    error_list[i] <- mean(data_test$y != pred)
  }
  # record the average of all misclassfication rate
  cv_err <- mean(error_list)
  # predict the class by using the full data
  class <- knn(train, train, as.factor(data$y), k=k_nn)
  return(list("class"= class,"cv_err" = cv_err))
}
