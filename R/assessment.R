#' sboost Assessment Function
#'
#' Assesses how well a classifier fits the data.
#'
#' See sboost documentation for more details.
#' @param feature feature set data.frame
#' @param outcomes outcomes corresponding to the features
#' @param classifier must be output from sboost
#' @return Assessment after each stump in the classifier.
#' @keywords assess, assessment, f1, accuracy
#' @examples
#' # malware
#' malware_classifier <- sboost(malware[-1], malware[1], iterations = 10, positive = 1)
#' assessment(malware[-1], malware[1], malware_classifier)
#'
#' # mushrooms
#' mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 10, positive = "p")
#' assessment(mushrooms[-1], mushrooms[1], mushroom_classifier)
#' @export
assessment <- function(features, outcomes, classifier) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  if (is.data.frame(outcomes)) {
    outcomes <- outcomes[[1]]
  }
  processed_features <- process_feature_input(features)
  processed_outcomes <- process_outcome_input(outcomes, features, classifier$outcomes)
  processed_classifier <- process_classifier_input(classifier, features)
  if (is.null(processed_outcomes) || is.null(processed_features) || is.null(processed_classifier)) {
    return(NULL)
  }

  # ASSESS CLASSIFIER
  # --------------------------------------------------------------------------------
  classifier_assessment <- make_assessment(processed_features, processed_outcomes, processed_classifier)
  classifier_assessment <- process_assessment_output(classifier_assessment, classifier, match.call())


  return(classifier_assessment)
}


# classifier, features, and outcomes must already be processed
make_assessment <- function(features, outcomes, classifier) {

  classifier_assessment <- assess(features, outcomes, classifier)
  colnames(classifier_assessment) <- c("true_positive", "false_negative", "true_negative", "false_positive")
  classifier_assessment <- data.frame(classifier_assessment)

  return(classifier_assessment)
}



