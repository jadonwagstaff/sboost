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
#' malware_classifier <- sboost(malware[-1], malware[1], iterations = 10)
#' assessment(malware[-1], malware[1], malware_classifier)
#'
#' # mushrooms
#' mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 10)
#' assessment(mushrooms[-1], mushrooms[1], mushroom_classifier)
#' @export
assessment <- function(features, outcomes, classifier) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  processed_features <- process_features(features)
  processed_outcomes <- process_outcomes(outcomes, features)
  classifier <- process_classifier(classifier, features)
  if (is.null(outcomes) || is.null(features) || is.null(classifier)) {
    return(NULL)
  }

  # ASSESS CLASSIFIER
  # --------------------------------------------------------------------------------
  classifier_assessment <- assess_classifier_internal(processed_features, processed_outcomes, classifier)


  return(classifier_assessment)
}


# classifier, features, and outcomes must already be processed
assess_classifier_internal <- function(features, outcomes, classifier) {
  classifier_assessment <- assess(features, outcomes, classifier)

  colnames(classifier_assessment) <- c("true_positive", "false_negative", "true_negative", "false_positive")
  classifier_assessment <- data.frame(classifier_assessment)
  classifier_assessment <- dplyr::mutate(classifier_assessment,
                                         accuracy = (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative),
                                         recall = true_positive / (true_positive + false_negative),
                                         specificity = true_negative / (true_negative + false_positive),
                                         precision = true_positive / (true_positive + false_positive),
                                         f1 = (2 * precision * recall) / (precision + recall))

  return(classifier_assessment)
}



