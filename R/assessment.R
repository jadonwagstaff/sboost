#' sboost Assessment Function
#'
#' Assesses how well a classifier fits the data.
#'
#' See sboost documentation for more details.
#' @param feature feature set data.frame
#' @param outcomes outcomes corresponding to the features
#' @param classifier must be output from sboost
#' @param positive is the positive outcome to test for; if NULL, the first in alphebetacal order will be chosen
#' @return Assessment after each stump in the classifier.
#' @keywords assess, assessment, f1, accuracy
#' @examples
#' # malware
#' malware_classifier <- sboost(malware[-1], malware[1], iterations = 10)
#' assessment(malware[-1], malware[1], malware_classifier, positive = 1)
#'
#' # mushrooms
#' mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 10)
#' assessment(mushrooms[-1], mushrooms[1], mushroom_classifier, positive = "p")
#' @export
assessment <- function(features, outcomes, classifier, positive = NULL) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  processed_features <- process_features(features)
  processed_outcomes <- process_outcomes(outcomes, features)
  processed_classifier <- process_classifier(classifier, features)
  positive_matched <- check_positive(strsplit(classifier$orientation[1], "\\|")[[1]], positive)
  if (is.null(processed_outcomes) || is.null(processed_features) || is.null(processed_classifier) || is.null(positive_matched)) {
    return(NULL)
  }

  # ASSESS CLASSIFIER
  # --------------------------------------------------------------------------------
  classifier_assessment <- assess_classifier_internal(processed_features, processed_outcomes, processed_classifier, positive_matched)


  return(classifier_assessment)
}


# classifier, features, and outcomes must already be processed
assess_classifier_internal <- function(features, outcomes, classifier, positive_matched) {
  classifier_assessment <- assess(features, outcomes, classifier)

  if(positive_matched) {
    colnames(classifier_assessment) <- c("true_positive", "false_negative", "true_negative", "false_positive")
  } else {
    colnames(classifier_assessment) <- c("true_negative", "false_positive", "true_positive", "false_negative")
  }
  classifier_assessment <- data.frame(classifier_assessment)
  classifier_assessment <- dplyr::mutate(classifier_assessment,
                                         accuracy = (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative),
                                         recall = true_positive / (true_positive + false_negative),
                                         specificity = true_negative / (true_negative + false_positive),
                                         precision = true_positive / (true_positive + false_positive),
                                         f1 = (2 * precision * recall) / (precision + recall))

  return(classifier_assessment)
}



