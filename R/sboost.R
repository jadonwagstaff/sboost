#' @useDynLib sboost
#' @importFrom Rcpp sourceCpp
NULL

#' sboost Learning Algorithm
#'
#' A machine learning algorithm using adaboost on decision stumps.
#'
#' Factors and characters are treated as categorical features.
#' Missing values are treated as their own category for categorical,
#' and divided randomly on the best split for numeric.
#' @param features feature set data.frame
#' @param outcomes outcomes corresponding to the features
#' @param iterations number of boosts
#' @return Classifier where each row is a stump.
#' @keywords stump, boost, classifier, adaboost, decision stump
#' @examples
#' # malware
#' sboost(malware[-1], malware[1], iterations = 10)
#'
#' # mushrooms
#' sboost(mushrooms[-1], mushrooms[1], iterations = 10)
#' @export
sboost <- function(features, outcomes, iterations = 1) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  categorical <- find_categorical(features)
  processed_features <- process_features(features)
  processed_outcomes <- process_outcomes(outcomes, features)
  if (is.null(processed_outcomes) || is.null(processed_features)) {
    return(NULL)
  }

  # DEVELOP CLASSIFIER
  # --------------------------------------------------------------------------------
  classifier <- make_classifier(processed_features, processed_outcomes, categorical, iterations)
  classifier <- prepare_classifier(classifier, features, outcomes)

  return(classifier)
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
  classifier <- adaboost(features, ordered_index, outcomes, categorical, iterations)

  return(classifier)
}



# TODO: Categorical results
# TODO: Visualization

