#' predictions makes predictions for a feature set
#'
#' @param features feature set data.frame
#' @param outcome_possibilities possible values of outcomes in a vector
#' @param classifier classifier output from sboost
#' @return predictions in the form of a vector
#' @keywords stump, boost, classifier
#' @export
classifier_predictions <- function(features, outcome_possibilities, classifier) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  if(is.data.frame(outcome_possibilities)) {
    outcome_possibilities <- outcome_possibilities[[1]]
  }
  outcome_possibilities <- sort(unique(outcome_possibilities))
  if (length(outcome_possibilities) != 2) {
    message("ERROR: There must be two outcome possibilities.")
    return(NULL)
  }
  features <- process_features(features)
  classifier <- process_classifier(classifier, features, outcome_possibilities)
  if (is.null(features) || is.null(classifier)) {
    return(NULL)
  }


  # MAKE PREDICTIONS
  # --------------------------------------------------------------------------------
  predictions <- make_predictions(features, classifier)
  predictions <- dplyr::if_else(predictions < 0, true = outcome_possibilities[[1]], false = outcome_possibilities[[2]])

  return(predictions)
}

