#` sboost Prediction Function`
#'
#' Make predictions for a feature set based on an sboost classifier.
#'
#' @param features feature set data.frame.
#' @param classifier \emph{sboost_classifier} S3 object output from sboost.
#' @param scores if true, raw scores generated; if false, predictions are generated.
#' @return Predictions in the form of a vector, or scores in the form of a vector.
#'   The index of the vector aligns the predictions or scores with the rows of
#'   the features. Scores represent the sum of all votes for the positive outcome
#'   minus the sum of all votes for the negative outcome.
#' @seealso \code{\link{sboost}} documentation.
#' @examples
#' # malware
#' malware_classifier <- sboost(malware[-1], malware[1], iterations = 10, positive = 1)
#' predictions(malware[-1], malware_classifier, scores = TRUE)
#' predictions(malware[-1], malware_classifier)
#'
#' # mushrooms
#' mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 10, positive = "p")
#' predictions(mushrooms[-1], mushroom_classifier, scores = TRUE)
#' predictions(mushrooms[-1], mushroom_classifier)
#' @export
predictions <- function(features, classifier, scores = FALSE) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  processed_classifier <- process_classifier_input(classifier, features)
  processed_features <- process_feature_input(features)


  # MAKE PREDICTIONS
  # --------------------------------------------------------------------------------
  predictions <- predict(processed_features, processed_classifier)
  if (scores) return(predictions)
  predictions <- dplyr::if_else(predictions > 0,
                                true = as.character(classifier$outcomes$positive),
                                false = as.character(classifier$outcomes$negative))

  return(predictions)
}


