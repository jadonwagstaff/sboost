#' @useDynLib sboost
#' @importFrom Rcpp sourceCpp
NULL

#' sboost learning algorithm
#'
#' This algorithm is a machine learning algorithm using
#' adaboost on decision stumps.
#' @param feature feature set data.frame
#' @param outcomes outcomes corresponding to the features
#' @param iterations is the number of boosts
#' @return classifier
#' @keywords stump, boost
#' @export
sboost <- function(features, outcomes, iterations = 1) {
  # PREPARE INPUT
  # --------------------------------------------------------------------------------

  # test and prepare features, outcomes, and categorical
  categorical <- find_categorical(features)
  features <- process_features(features)
  outcomes <- process_outcomes(outcomes, features)
  if (is.null(outcomes) || is.null(features)) {
    return(NULL)
  }

  # DEVELOPE CLASSIFIER
  # --------------------------------------------------------------------------------
  classifier <- make_classifier(features, outcomes, categorical, iterations)


  return(classifier)

}



# make_classifiers takes an unordered set of features and outcomes,
#        orders them, and calls the appropriate functions for each iteration
# Param: features - a numerical matrix of features
# Param: outcomes - a numerical vector of outcomes
# Param: iterations to call appropriate functions
# Return: classifier consisting of a linear combination of decision stumps
make_classifier <- function(features, outcomes, categorical, iterations) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------

  # create variables
  ordered_index <- matrix(NA, nrow = nrow(features), ncol = ncol(features))
  for (i in 1:ncol(features)) {
    ordered_index[, i] <- order(features[, i]) - 1
  }
  classifier <- matrix(NA, nrow = iterations, ncol = 4)

  # CALL C++ CODE
  # --------------------------------------------------------------------------------
  classifier <- adaboost(features, ordered_index, outcomes, categorical, iterations)


  return(classifier)
}


# TODO: deal with missing values
# TODO: Categorical features
# TODO: Regression
# TODO: Visualization

