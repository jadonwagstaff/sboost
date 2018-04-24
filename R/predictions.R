#` sboost Prediction Function`
#'
#' Make predictions for a feature set based on an sboost classifier.
#'
#' See sboost documentation for more details.
#' @param features feature set data.frame
#' @param outcome_possibilities possible values of outcomes in a vector
#' @param classifier classifier output from sboost
#' @return predictions in the form of a vector
#' @keywords predictions, predict
#' @examples
#' # malware
#' malware_classifier <- sboost(malware[-1], malware[1], iterations = 10)
#' predictions(malware[-1], malware_classifier)
#'
#' # mushrooms
#' mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 10)
#' predictions(mushrooms[-1], mushroom_classifier)
#' @export
predictions <- function(features, classifier) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  outcome_possibilities <- sort(strsplit(classifier$orientation[1], "\\|")[[1]])
  processed_classifier <- process_classifier_input(classifier, features)
  processed_features <- process_feature_input(features)
  if (is.null(features) || is.null(classifier)) {
    return(NULL)
  }


  # MAKE PREDICTIONS
  # --------------------------------------------------------------------------------
  predictions <- predict(processed_features, processed_classifier)
  predictions <- dplyr::if_else(predictions < 0, true = outcome_possibilities[[1]], false = outcome_possibilities[[2]])

  return(predictions)
}

