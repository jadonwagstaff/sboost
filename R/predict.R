#` sboost Prediction Function
#'
#' Make predictions for a feature set based on an sboost classifier.
#'
#' @param object \emph{sboost_classifier} S3 object output from sboost.
#' @param features feature set data.frame.
#' @param scores if true, raw scores generated; if false, predictions are generated.
#' @param ... further arguments passed to or from other methods.
#' @return Predictions in the form of a vector, or scores in the form of a vector.
#'   The index of the vector aligns the predictions or scores with the rows of
#'   the features. Scores represent the sum of all votes for the positive outcome
#'   minus the sum of all votes for the negative outcome.
#' @seealso \code{\link{sboost}} documentation.
#' @examples
#' # malware
#' malware_classifier <- sboost(malware[-1], malware[1], iterations = 5, positive = 1)
#' predict(malware_classifier, malware[-1], scores = TRUE)
#' predict(malware_classifier, malware[-1])
#'
#' # mushrooms
#' mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 5, positive = "p")
#' predict(mushroom_classifier, mushrooms[-1], scores = TRUE)
#' predict(mushroom_classifier, mushrooms[-1])
#' @export
predict.sboost_classifier <- function(object, features, scores = FALSE, ...) {
  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  processed_classifier <- process_classifier_input(object, features)
  processed_features <- process_feature_input(features)


  # MAKE PREDICTIONS
  # --------------------------------------------------------------------------------
  predictions <- predict_cpp(processed_features, processed_classifier)
  if (scores) return(predictions)
  predictions <- dplyr::if_else(predictions > 0,
                                object$outcomes["positive"],
                                object$outcomes["negative"])

  return(unname(predictions))
}

#' @export
predict.sboost <- function(object, features, scores = FALSE, type = "median") {

  # Function for combining stumps, dependent on "type"
  combine_stumps <- switch(
    type,
    "median" = function(x, w) {
      ox <- order(x)
      sw <- cumsum(w[ox]) / sum(w)
      for (i in 1:length(sw)) {if (sw[i] >= 0.5) return((x[ox])[i])}
    },
    "mean" = stats::weighted.mean
  )

  # Function for predicting an individual observation
  predict_observation <- function(obs, object) {
    preds <- apply(object, 1, function(x) {
               ifelse(obs[x["feature"]] < x["split"],
                      x["mean_behind"],
                      x["mean_ahead"])
             })
    combine_stumps(preds, object[, "vote"])
  }

  # Predict for all observations
  apply(features, 1, function(x) predict_observation(x, object))
}

