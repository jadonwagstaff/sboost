#' @useDynLib sboost
#' @importFrom Rcpp sourceCpp
NULL

#' sboost learning algorithm
#'
#' This algorithm is a machine learning algorithm using
#' adaboost on decision stumps.
#'
#' Missing values are treated as their own category for categorical,
#' and divided randomly on the best split for numeric.
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
  processed_features <- process_features(features)
  processed_outcomes <- process_outcomes(outcomes, features)
  if (is.null(processed_outcomes) || is.null(processed_features)) {
    return(NULL)
  }

  # DEVELOP MODEL
  # --------------------------------------------------------------------------------
  if (length(unique(processed_outcomes)) == 2) {
    model <- make_classifier(processed_features, processed_outcomes, categorical, iterations)
    model <- prepare_classifier(model, features, outcomes)
  } else {
    model <- make_regressor(processed_features, processed_outcomes, categorical, iterations)
    model <- prepare_regressor(model, features, outcomes)
  }

  return(model)
}



# make_classifier takes an unordered set of features and outcomes,
#        orders them, and calls the appropriate functions for each iteration
# Param: features - a numerical matrix of features
# Param: outcomes - a numerical vector of outcomes
# Param: categorical - a vector representing which features are categorical
# Param: iterations to call appropriate functions
# Return: classifier consisting of a linear combination of decision stumps
make_classifier <- function(features, outcomes, categorical, iterations) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  ordered_index <- matrix(NA, nrow = nrow(features), ncol = ncol(features))
  for (i in 1:ncol(features)) {
    ordered_index[, i] <- order(features[, i]) - 1
  }

  # CALL C++ CODE
  # --------------------------------------------------------------------------------
  classifier <- adaboost_class(features, ordered_index, outcomes, categorical, iterations)

  return(classifier)
}



# make_regressor takes an unordered set of features and outcomes,
#        orders them, and calls the appropriate functions for each iteration
# Param: features - a numerical matrix of features
# Param: outcomes - a numerical vector of outcomes
# Param: categorical - a vector representing which features are categorical
# Param: iterations to call appropriate functions
# Return: classifier consisting of a linear combination of decision stumps
make_regressor <- function(features, outcomes, categorical, iterations) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  ordered_index <- matrix(NA, nrow = nrow(features), ncol = ncol(features))
  for (i in 1:ncol(features)) {
    ordered_index[, i] <- order(features[, i]) - 1
  }

  # CALL C++ CODE
  # --------------------------------------------------------------------------------
  regressor <- adaboost_regress(features, ordered_index, outcomes, categorical, iterations)

  return(regressor)
}


# TODO: Make predictions
# TODO: Improve documentation
# TODO: Deal with missing values
# TODO: Categorical results
# TODO: Visualization

